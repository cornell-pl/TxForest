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
 * - NEEEEEEDDD TO MAKE SURE THAT THE EXAMPLED GIVE GOOD STARTING PATH
 * - check the error handling especially with removing paths and goto and stuff, careful
 * - dummy path seems kinda dangerous, hmmmmmmmmm
 *        - rn forcing to use current working directoy, but im not sure what would be
 #          the best situation here
 * - make a transactional version
 *    - this should be the thing underling the fs server I think
 *)

open Core
open Result
open Result.Let_syntax
open Utils
(* open Sys -> uses the Sys in Core, decided not to open for transparence
open Unix -> uses the unix in Core, same ^*)

(* ---------------   Types ---------------- *)

(*note: the filesytem is the one above this path*)
type fs = unit

type t = fs * path


(* ----------   Helper Functions ---------- *)

let default_perm = 0o640

let convert_result res = map_error res ~f:(fun err -> (OpError err))

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

let rec remove_dir_h ( p : path) : unit =
  let (parent_dir, _) = Filename.split p in
  let dir_contents = Sys.ls_dir p in
    Unix.chdir p;
    List.iter dir_contents ~f:(fun u -> remove_h (Filename.concat p u));
    Unix.chdir parent_dir;

and remove_h (p:path): unit =
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

let check_path_preconditions ((fs, p): t) :t or_fail=
  let working_dir = Unix.getcwd () in
  let (parent_dir, _) = Filename.split p in
    if not(exists_h parent_dir) then
      mk_err "path precondition check - parent of curent path ( %s ) does not exist" p
    else if not (is_dir_h parent_dir) then
      mk_err "path precondition check - parent of curent path ( %s ) is not a drectory" p
    else if not (String.equal working_dir parent_dir) then
      mk_err "path precondition check - working directory ( %s ) does not match parent of current path ( %s )" working_dir p
    else
      mk_ok ((), p)


(* ----------   Exposed Functions ---------- *)

let get_log t = []

let clear_log (t:t) : t or_fail =
  mk_ok t

let create (p: path) : t or_fail =
  let (parent_dir, _) = Filename.split p in
  if not (exists_h parent_dir) then
    mk_err "create - parent of path ( %s ) does not exist" p
  else if not (is_dir_h parent_dir) then
    mk_err "create - parent of path ( %s ) is not a directory" p
  else begin
    Sys.chdir parent_dir;
    mk_ok ((), p)
  end

let make_file (u: string) ((fs, p) :t)  :t or_fail =
  let%bind _ = check_path_preconditions (fs, p) in
    make_file_h p u;
    mk_ok ((), p)

let make_directory ( new_lst: string list) ((fs, p): t)  : t or_fail =
  let%bind _ = check_path_preconditions (fs, p) in
    if is_file_h p then remove_file_h p;
    if not (exists_h p) then Unix.mkdir ~perm:default_perm p;
    let (parent_dir, dirname) = Filename.split p in
    let cur_lst = Sys.ls_dir p in
    let (rem_lst: string list) = List.filter cur_lst ~f:(fun u -> not (List.mem new_lst u ~equal:String.equal)) in
    let (add_lst: string list) = List.filter new_lst ~f:(fun u -> not (List.mem cur_lst u ~equal:String.equal)) in
      Unix.chdir p;
      List.iter rem_lst ~f:(fun u -> remove_h (Filename.concat p u));
      List.iter add_lst ~f:(fun u -> make_file_h (Filename.concat p u) "");
      Unix.chdir parent_dir;
      mk_ok ((), p)

let add_to_directory (u:string) ((fs, p): t)  : t or_fail =
  let%bind _ = check_path_preconditions (fs, p) in
    if is_file_h p then remove_file_h p;
    if not (exists_h p) then Unix.mkdir ~perm:default_perm p;
    let (parent_dir, dirname) = Filename.split p in
      Unix.chdir p;
      make_file_h (Filename.concat p u) "";
      Unix.chdir parent_dir;
      mk_ok ((), p)

let goto (new_p:string) ((fs,p):t) : t or_fail =
  let (parent_dir, dirname) = Filename.split new_p in
    if not (exists_h parent_dir) then
      mk_err "goto - parent of new path ( %s ) does not exist" new_p
    else if not (is_dir_h parent_dir) then
      mk_err "goto - parent of new path ( %s ) is not a directory" new_p
    else begin
      Unix.chdir parent_dir;
      mk_ok ((), new_p)
    end

let gotoChild (u: string) ((fs, p):t) : t or_fail =
  let%bind _ = check_path_preconditions (fs, p) in
    if not (exists_h p) then
      mk_err "gotochild - parent of new path ( %s ) does not exist" (Filename.concat p u)
    else if not (is_dir_h p) then
      mk_err "gotochild - parent of new path ( %s ) is not a directory" (Filename.concat p u)
    else begin
      Unix.chdir p;
      mk_ok ((), Filename.concat p u)
    end

let pop ( (fs, p) :t) : t or_fail =
  let%bind _ = check_path_preconditions (fs, p) in
  let (parent_dir, dirname) = Filename.split p in
  let (parent_parent_dir, _) = Filename.split p in
    Unix.chdir parent_parent_dir;
    mk_ok ((), parent_dir)

let exists ((fs, p):t) : bool = exists_h p

let is_dir ((fs, p):t) : bool = is_dir_h p

let fetch ((fs, p):t) : contents =
  match check_path_preconditions (fs, p) with
  | Ok _ -> begin
    if is_dir_h p then
      let dir_contents = Sys.ls_dir p in
        Dir dir_contents
    else
      let file_contents = In_channel.read_all p in
        File file_contents
  end
  | Error e -> failwith e

let sync_path ((fs,p) : t) : t or_fail =
  let (parent_dir, dirname) = Filename.split p in
    if not (exists_h parent_dir) then
      mk_err "sync_path - parent of new path ( %s ) does not exist" p
    else if not (is_dir_h parent_dir) then
      mk_err "sync_path - parent of new path ( %s ) is not a directory" p
    else begin
      Unix.chdir parent_dir;
      mk_ok ((), p)
    end

let dummy_path =
  let potential_root = Filename.concat (Unix.getcwd ()) "example_fs_root" in
    if exists_h potential_root then potential_root else Filename.root

let get_working_path ((_, p):t) : path =
  p

(*TODO: error handlling*)
let run_txn ~(f : t -> 'a or_fail) () : ('a,txError) Core.result =
  match create dummy_path with
  | Ok t -> f t |> convert_result
  | Error _ -> Error TxError

(* TODO: Why does this do what you think it does?! *)
let loop_txn ~(f : t -> 'a) () =
  let x : 'a option ref = ref None in
  let apply t =
    x := Some(f t);
    mk_ok t
  in
    let _ : (t,txError) Core.result = run_txn ~f:apply () in
    Option.value_exn ~message:"loop_txn: Something went horribly wrong" !x


