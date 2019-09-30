(* TODOS:
 * - The ref for loop_txn is gross. Either don't return anything or do it better.
 * - See https://docs.google.com/document/d/1aYa2JQHo7hDt9OSYJfg2UupW4KuCSDon5n5_IeTLFdc/edit?usp=sharing
 *   for some translations
 * - Everything relating to directories should probably be sets instead of lists
 *
 *)

(* Invariants:
 * - The path and the fs passed to any function except 'sync_path' should correspond
 *)

open Core
open Result
open Result.Let_syntax
open Utils



(* ---------------   Types ---------------- *)

module StringMap = Map.Make(String)
(*note: the filesytem is the one above this path*)

type contents = Dir of string list | File of string
type fs = contents StringMap.t
type path = string list


type t = fs * path


(* ----------   Helper Functions ---------- *)

let path_to_string (p: path) : string =
  String.concat ~sep:"." (List.rev p)


(* ----------   Exposed Functions ---------- *)


let create (p': path) : t or_fail =
  mk_ok (StringMap.empty, p')

(*TODO: make this do the checking*)
let make_file ((fs, p) :t) (u: string) :t or_fail =
  let stringified_path = path_to_string p in
  let updated_map = StringMap.add stringified_path (File u) fs in
    mk_ok (updated_map, p)

(*TODO: make this do the checking*)
let make_directory ((fs, p): t) ( new_lst: string list) : t or_fail =
   let stringified_path = path_to_string p in
   let updated_map = StringMap.add stringified_path (Dir new_lst) fs in
     mk_ok (updated_map, p)

(*TODO: make this do the checking*)
let add_to_directory ((fs, p): t) (u:string) : t or_fail =
  let stringified_path = path_to_string p in
  let cur_entry = StringMap.find stringified_path fs in
    match cur_entry with
    | Dir lst -> begin
        let updated_map = StringMap.add stringified_path (Dir u::lst) fs in
          mk_ok (updated_map, p)
    end
    | _ -> failwith "unimplemted"

(*TODO: make this do the checking*)
let gotoChild (u: string) ((fs, p):t) : t or_fail =
   mk_ok (fs, u::p)

(*TODO: make this do the checking*)
let goto (p:string) ((fs,_):t) : t or_fail =
   mk_ok (fs, p)

(*TODO: make this do the checking*)
let pop ( (fs, p) :t) : t or_fail =
  match p with
  | u :: p' -> mk_ok (fs, p')
  | _ -> failwith "unimplemented"

(*TODO: make this do the checking*)
let fetch ((fs, p):t) : contents =
  let stringified_path = path_to_string p in
    StringMap.find stringified_path fs

let exists ((fs, p):t) : bool =
  let stringified_path = path_to_string p in
    StringMap.mem stringified_path fs

let is_dir (t:t) : bool = fetch t |> function
  | Dir _ -> true
  | File _ -> false


let sync_path ((fs,p) : t) : t or_fail =
   mk_ok (fs, p)

let loop_txn ~(f : fs -> 'a) () =
  let x : 'a option ref = ref None in
  let apply fs =
    x := Some(f fs);
    fs
  in
    run_txn ~f:apply ();
    Option.value_exn ~message:"loop_txn: Something went horribly wrong" !x

(*TODO: error handlling*)
let run_txn ~f () =
  let fs = create dummy_path in
    f fs

let dummy_path = []

