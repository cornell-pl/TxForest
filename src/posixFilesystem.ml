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

(*note: the filesytem is the one above this path*)
type fs = unit
type path = string
type contents = Dir of string list | File of string

type t = fs * path


(* ----------   Helper Functions ---------- *)


(* ----------   Exposed Functions ---------- *)


let create (p': path) : t or_fail =
  failwith "unimplemented"

let make_file ((fs, p) :t) (u: string) :t or_fail =
   failwith "unimplemented"


let make_directory ((fs, p): t) ( new_lst: string list) : t or_fail =
   failwith "unimplemented"

let add_to_directory ((fs, p): t) (u:string) : t or_fail =
   failwith "unimplemented"

let gotoChild (u: string) ((fs, p):t) : t or_fail =
   failwith "unimplemented"

let goto (p:string) ((fs,_):t) : t or_fail =
   failwith "unimplemented"

let pop ( (fs, p) :t) : t or_fail =
   failwith "unimplemented"

let fetch ((fs, p):t) : contents =
   failwith "unimplemented"

let exists ((fs, p):t) : bool =
   failwith "unimplemented"

let is_dir (t:t) : bool = fetch t |> function
  | Dir _ -> true
  | File _ -> false

let rec get_diff ol nl =
   failwith "unimplemented"

let sync_path ((fs,p) : t) : t or_fail =
   failwith "unimplemented"

let loop_txn ~(f : fs -> 'a) () =
   failwith "unimplemented"

let run_txn ~f () =
   failwith "unimplemented"

let dummy_path = ""

