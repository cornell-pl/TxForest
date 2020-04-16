open Core

(* Commonish modules *)
module Var = String
module PathSet = String.Set
module SSet = struct
  type t = String.Set.t

  let show s = String.Set.to_list s |> String.concat ~sep:"," |> fun s -> "{" ^ s ^ "}"
  let pp fmt s = Format.fprintf fmt "%S" (show s)
end

type txError =
  | TxError
  | OpError of string

(* Common Types *)
type name = string [@@deriving show]

type 'a or_fail = ('a,string) Core.result

type contents = Dir of string list | File of string

type path = string

type le =
  | Read of contents * path
  | Write_file of contents * contents * path
  | Write_directoy of contents * contents * path

type log = le list

type fetch_rep =
  | FileRep of string
  | DirRep of SSet.t
  | PathRep of name
  | PairRep of Var.t
  | CompRep of SSet.t
  | OptRep of bool
  | PredRep of bool
  | NullRep [@@deriving show]

type writeable_fetch_rep =
  | WFileRep of string
  | WDirRep of string list
  | WPathRep of name
  | WPairRep of Var.t
  | WCompRep of string list
  | WOptRep of bool
  | WPredRep of bool
  | WNullRep [@@deriving show]

(* Helper functions *)
let debug = ref false
let set_debug () = debug := true

let p = Printf.printf

let d format =
  if !debug
  then Printf.ksprintf (Core.Printf.printf "Debug: %s\n%!") format
  else Printf.ksprintf Core.ignore format

let mk_err = Result.failf

let mk_ok t = Result.Ok t

let ignore_ret x _ = Result.Ok x
let f_ret ~f z = f z; mk_ok z

let info_message ?id smsg input =
  let input = Core.String.rstrip ~drop:(Char.equal '\n') input in
  match id with
  | None -> print_endline (Printf.sprintf "%s - %s" smsg input)
  | Some id -> print_endline (Printf.sprintf "%s - %s: %s" smsg (Async.Writer.Id.to_string id) input)



let writable_of_fetch fr =
  match fr with
  | FileRep s -> WFileRep s
  | DirRep s -> WDirRep (String.Set.to_list s)
  | PathRep n -> WPathRep n
  | PairRep v -> WPairRep v
  | CompRep s -> WCompRep (String.Set.to_list s)
  | OptRep b -> WOptRep b
  | PredRep b -> WPredRep b
  | NullRep -> WNullRep

let fetch_of_writable wf =
  match wf with
  | WFileRep s -> FileRep s
  | WDirRep s -> DirRep (String.Set.of_list s)
  | WPathRep n -> PathRep n
  | WPairRep v -> PairRep v
  | WCompRep s -> CompRep (String.Set.of_list s)
  | WOptRep b -> OptRep b
  | WPredRep b -> PredRep b
  | WNullRep -> NullRep

  open Async

  let write_marshal ~flags writer to_marshal = 
    Marshal.to_string to_marshal flags 
    |> Async.Writer.write writer
    
  let write_struct writer to_marshal = 
    Marshal.to_string to_marshal [] 
    |> Async.Writer.write writer
    
  let block = Thread_safe.block_on_async_exn