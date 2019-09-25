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