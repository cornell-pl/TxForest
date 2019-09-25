(* TODOS:
 * - Figure out a way to not expose path type, but still have a PathSet (see Eval)
 *)

open Core
open Utils

type fs
type path = string

type contents = Dir of string list | File of string

type t = fs * path

(*[create p] creates a file system which will be currently focused on p*)
val create : path -> t or_fail

(*[make_file t u] makes a file at the current path in the file system with
 * context u*)
val make_file: t -> string -> t or_fail

(*[make_directory t lst] makes a directory at the current path in the filesystem
 * if the current path is a file this makes a new directory with empty filed for
 * each of the filenames in lst,
 * if the current path is a directory this creates an empty file for any file
 * name in lst but not currently in the directory and removes any files not
 * in lst but in the current directory*)
val make_directory: t -> string list -> t or_fail

(*[add_to_directory t u] adds a file with name u to the directory at the current
 * path, if there is a file at the current path it is replaced with a directory
 * with only a file for u in it, if it is a directory then the file is added, this
 * over writes the file if there is a file with filename u in the directory *)
val add_to_directory: t -> string -> t or_fail

(* val goto_root: t -> t or_fail *)

(*[gotoChild u t] goes to the file with name u in the directory that is at the
 * current path of the filesystem*)
val gotoChild: string -> t -> t or_fail

(* [goto p t] goes to the path p in the filesystem *)
val goto: string -> t -> t or_fail

(*[pop t] moves to the parent directoy of the current path*)
val pop: t -> t or_fail

(*[fetch t] gets the contents of the file or directory at the current path*)
val fetch: t -> contents

(*[exists t] checks if there is a node at the current path *)
val exists: t -> bool

(*[is_dir t] checks if the current path is a directory *)
val is_dir: t -> bool

val sync_path: t -> t or_fail

(*[loop_txn ~f ()] loops the transaction f until it commits *)
val loop_txn: f:(fs -> 'a) -> unit -> 'a

(*[run_txn f ()] runs the transaction f and attempts to commit it once *)
val run_txn: f:(fs -> 'a or_fail) -> unit -> ('a,txError) Core.result

val dummy_path: path