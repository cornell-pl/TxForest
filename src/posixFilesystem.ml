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

(*
 * TODO:
 * - fix the path stuff
 * - NEEEEEEDDD TO MAKE SURE THAT THE EXAMPLED GIVE GOOD STARTING PATH
 * - switch to using all core file system manipulations so that its consitant
 * - error handing everywhere
 * - fix the failwith unimplmented
 * - make sure closing files
 * - make the hiddent functions that seem like they couldbe public look like helpers
 * document preconditions
 * - dummy path seems kinda dangerous, hmmmmmmmmm
 *)

open Core
open Result
open Result.Let_syntax
open Utils
(* open Sys -> uses the Sys in Core
open Unix -> uses the unix in Core*)

(* ---------------   Types ---------------- *)

(*note: the filesytem is the one above this path*)
type fs = unit
type path = string
type contents = Dir of string list | File of string

type t = fs * path


(* ----------   Helper Functions ---------- *)

let default_perm = 0o640

let exists_h ( p : path) : bool =
  match Sys.file_exists ~follow_symlinks:false p with
  | `Yes -> true
  | _ -> false

let is_dir_h (p : path) : bool =
  match Sys.is_directory ~follow_symlinks:false p with
  | `Yes -> true
  | _ -> false

let is_file_h ( p : path) : bool =
  match Sys.is_file ~follow_symlinks:false p with
  | `Yes -> true
  | _ -> false

let remove_file_h ( p : path) : unit =
  Sys.remove p

let remove_dir_h ( p : path) : unit =
  failwith "remove_dir unimplemented"

let remove_h (p:path): unit =
  if is_file_h p then
    remove_file_h p
  else
    remove_dir_h p

let make_file_h (p: path) (u: string) : unit =
  if is_dir_h p then remove_dir_h p;
  let new_file_channel =
    Out_channel.create
      ~binary:false
      ~append:false
      ~fail_if_exists:false
      ~perm:default_perm
      p
  in
    Out_channel.output_string new_file_channel u;
    Out_channel.flush new_file_channel;
    Out_channel.close new_file_channel


(* ----------   Exposed Functions ---------- *)

(*TODO: error handling*)
let create (p: path) : t or_fail =
  let (parent_dir, _) = Filename.split p in
  if is_dir_h parent_dir then begin
    Sys.chdir parent_dir;
    mk_ok ((), p)
  end else
    mk_err "path ( %s ) does not exist" p


(*TODO: checking if is dir*)
(*TODO: double check on permision*)
let make_file ((fs, p) :t) (u: string) :t or_fail =
  make_file_h p u;
  mk_ok ((), p)


(*TODO: checking if is file or dir*)
(*TODO: double check on permision*)
let make_directory ((fs, p): t) ( new_lst: string list) : t or_fail =
  if is_file_h p then remove_file_h p;
  if not (exists_h p) then Unix.mkdir ~perm:default_perm p;
  let (parent_dir, dirname) = Filename.split p in
  let cur_lst = Sys.ls_dir p in
  let (rem_lst: string list) = List.filter cur_lst ~f:(fun u -> not (List.mem new_lst u ~equal:String.equal)) in
  let (add_lst: string list) = List.filter new_lst ~f:(fun u -> not (List.mem cur_lst u ~equal:String.equal)) in
    Unix.chdir p;
    List.iter rem_lst ~f(fun u -> remove_h (Filename.concat p u));
    List.iter add_lst ~f(fun u -> let _ = make_file_h (Filename.concat p u) "" in ());
    Unix.chdir parent_dir;
      mk_ok ((), p)


(*TODO: checking the path first*)
let add_to_directory ((fs, p): t) (u:string) : t or_fail =
  if is_file_h p then remove_file_h p;
  if not (exists (fs, p)) then Unix.mkdir ~perm:default_perm p;
    Unix.chdir p;
    let%bind _ = make_file_h (Filename.concat p u) "" in
    let (parent_dir, dirname) = Filename.split p in
      Unix.chdir parent_dir;
      mk_ok ((), p)

let goto (new_p:string) ((fs,p):t) : t or_fail =
  let (parent_dir, dirname) = Filename.split new_p in
    Unix.chdir parent_dir;
    mk_ok ((), new_p)

let gotoChild (u: string) ((fs, p):t) : t or_fail =
  Unix.chdir p;
  mk_ok ((), Filename.concat p u)

let pop ( (fs, p) :t) : t or_fail =
  let (parent_dir, dirname) = Filename.split p in
  let (parent_parent_dir, _) = Filename.split p in
      chdir parent_parent_dir;
      mk_ok ((), parent_dir)

let exists ((fs, p):t) : bool = exists_h p

let is_dir ((fs, p):t) : bool = is_dir_h p

let fetch ((fs, p):t) : contents =
  if is_dir_h p then
    let dir_contents = Sys.ls_dir p in
      Dir dir_contents
  else
    let file_contents = In_channel.read_all p in
      File file_contents

let sync_path ((fs,p) : t) : t or_fail =
  let (parent_dir, dirname) = Filename.split p in
    Unix.chdir parent_dir;
    mk_ok ((), p)

(*TODO: error handlling*)
let run_txn ~(f : fs -> 'a or_fail) () : ('a,txError) Core.result =
  match create dummy_path with
  | Ok (fs, _) -> f fs |> convert_result
  | Error _ -> Error TxError

let loop_txn ~(f : fs -> 'a) () =
  let x : 'a option ref = ref None in
  let apply fs =
    x := Some(f fs);
    mk_ok fs
  in
    run_txn ~f:apply ();
    Option.value_exn ~message:"loop_txn: Something went horribly wrong" !x

let dummy_path = "~/Documents/TxForest/example_fs_root"

