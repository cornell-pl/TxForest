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

(*Note: this is the Map inside Core, not the pervasives one*)
module FS = Hashtbl.Make(String)
(*note: the filesytem is the one above this path*)

type contents = Dir of string list | File of string | Rem
type path = string

(*
 * contents that have been read or writen,
 * paths that have been removed from the fs
 * where we currently are in the fs
 *)
type fs = contents FS.t * path ref * log

and log = le list

and le =
  Read of contents * path
  Write_file of contents * contents * path
  Write_directoy of contents * contents * path

type t = fs * path


(* ----------   Helper Functions ---------- *)

let list_path_to_string_path (p: string list) : string =
  String.concat ~sep:"/" (List.rev p)

let string_path_to_list_path (p: string ) : string list =
  let path_peices = String.split p ~on:'/' in
    List.rev path_peices


let exists_helper (fs: fs) (p: path) : bool =
  if FS.mem fs p then
    (*in the cache of the file system*)
    match FS.find_exn fs p with
    | Rem -> false
    | _ -> true
  else
    (*not in the cache of the file system, check if its in the real fs*)
    match Sys.file_exists ~follow_symlinks:false p with
    | `Yes -> true
    | _ -> false

let is_file_helper (fs: fs) (p : path) : bool =
  if FS.mem fs p then
    (*in the cache of the file system*)
    match FS.find_exn fs p with
    | File _ -> true
    | _ -> false
  else
    (*not in the cache of the file system, check if its in the real fs*)
    match Sys.is_file ~follow_symlinks:false p with
    | `Yes -> true
    | _ -> false

let is_dir_helper (fs: fs) (p : path) : bool =
  if FS.mem fs p then
    (*in the cache of the file system*)
    match FS.find_exn fs p with
    | Dir _ -> true
    | _ -> false
  else
    (*not in the cache of the file system, check if its in the real fs*)
    match Sys.is_directory ~follow_symlinks:false p with
    | `Yes -> true
    | _ -> false

let check_path_preconditions (((fs, working_path), p, _): t) :t or_fail=
  let (parent_dir, _) = Filename.split p in
    if not(exists_helper parent_dir) then
      mk_err "path precondition check - parent of curent path ( %s ) does not exist" p
    else if not (is_dir_helper parent_dir) then
      mk_err "path precondition check - parent of curent path ( %s ) is not a drectory" p
    else if (!working_path) <> parent_dir then
      mk_err "path precondition check - working directory ( %s ) does not match parent of current path ( %s )" working_dir p
    else
      mk_ok ((fs, working_path), p)

let remove_file_helper (fs: fs) ( p : path) : unit =
  FS.set fs ~key:p ~data:Rem

let rec remove_dir_helper (fs: fs) (p : path) : unit =
  begin
  if FS.mem fs p then
    (*in the cache of the file system*)
    match FS.find_exn fs p with
    | Dir dir_contents -> begin
      List.iter dir_contents ~f:(fun u -> remove_helper fs (Filename.concat p u))
    end
    | _ -> failwith "precondition of remove dir violated"
  else
    (*not in the cache of the file system, check if its in the real fs*)
    let dir_contents = Sys.ls_dir p in
      List.iter dir_contents ~f:(fun u -> remove_helper fs (Filename.concat p u));
  end;
  FS.set fs ~key:p ~data:Rem

and remove_helper (fs: fs) (p:path): unit =
  if is_file_helper fs p then
    remove_file_helper fs p
  else
    remove_dir_helper fs p

let make_file_helper (fs: fs) (p:path) (u: string) : unit =
  FS.set fs ~key:p ~data:(File u)

let fetch_helper (fs: fs) (p:path): contents =
  if FS.mem fs p then
    (*in the cache of the file system*)
    FS.find_exn fs p
  else
    (*not in the cache of the file system, check if its in the real fs*)
    if is_dir_helper fs p then
      let dir_contents = Sys.ls_dir p in
      let entry = Dir dir_contents in
        FS.set fs ~key:p ~data:entry; entry
    else
      let file_contents = In_channel.read_all p in
      let entry = File file_contents in
        FS.set fs ~key:p ~data:entry; entry

let fetch_dir_helper (fs: fs) (p:path): string list =
  match fetch_helper fs p with
  | Dir lst -> lst
  | _ -> failwith "fetch_dir_helper precondition violated"


(* ----------   Exposed Functions ---------- *)


let create (p': path) : t or_fail =
  let (parent_dir, _) = Filename.split p in
  let fs = FS.create ~growth_allowed:true () in
(*     FS.set fs ~key:"/simple" ~data:(Dir ["index.txt";"dir"]);
    FS.set fs ~key:"/simple/index.txt" ~data:(File "a\nb\nc\nd");
    FS.set fs ~key:"/simple/dir" ~data:(Dir ["a"; "b"; "c"; "d"; "e"]);
    FS.set fs ~key:"/simple/dir/a" ~data:(File "apple");
    FS.set fs ~key:"/simple/dir/b" ~data:(File "banana");
    FS.set fs ~key:"/simple/dir/c" ~data:(File "carrot");
    FS.set fs ~key:"/simple/dir/d" ~data:(File "dragon fruit");
    FS.set fs ~key:"/simple/dir/e" ~data:(File "eggplant"); *)
    mk_ok ((fs, ref parent_dir, [])  p')


(*[get_log t] gets the log out of the t*)
let get_log (((fs, working_path, l), p, l) :t) : log = l

let clear_log (((fs, working_path, l), p, l) :t) : t or_fail =
  mk_ok ((fs, working_path, []), p)

let make_file (u: string) (((fs, working_path, l), p) :t)  :t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path), p) in
  let l' = (Write_file (File "") (File u) p) in
    if is_dir_helper fs p then remove_dir_helper fs p;
    make_file_helper fs p u;
      mk_ok ((fs, working_path, l'), p)

(*TODO: make this do the checking*)
let make_directory ( new_lst: string list) (((fs, working_path), p, l): t)  : t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path), p) in
  let l' = (Write_directoy (Dir []) (Dir new_lst) p) in
    if is_file_helper p then remove_file_helper p;
    let cur_lst = fetch_dir_helper fs p in
    let (rem_lst: string list) = List.filter cur_lst ~f:(fun u -> not (List.mem new_lst u ~equal:String.equal)) in
    let (add_lst: string list) = List.filter new_lst ~f:(fun u -> not (List.mem cur_lst u ~equal:String.equal)) in
      List.iter rem_lst ~f:(fun u -> remove_helper fs (Filename.concat p u));
      List.iter add_lst ~f:(fun u -> let _ = make_file_helper fs (Filename.concat p u) "" in ());
      FS.set fs ~key:p ~data:(Dir new_lst);
      mk_ok ((fs, working_path, l'), p)

(*TODO: make this do the checking and add the file?*)
let add_to_directory (u:string) (((fs, working_path, l), p): t)  : t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path), p) in
    if is_file_helper p then remove_file_helper p;
    if not (exists_helper fs p) then FS.set fs ~key:p ~data:(Dir []);
    let lst = fetch_dir_helper fs p in
    let new_lst = List.dedup_and_sort ~compare:String.compare (u::lst) in
    let l' = (Write_directoy (Dir lst) (Dir new_lst) p) in
      FS.set fs ~key:p ~data:(Dir new_lst);
      if is_dir_helper fs (Filename.concat p u) then remove_dir_helper fs (Filename.concat p u);
      make_file_helper fs (Filename.concat p u) "";
        mk_ok ((fs, working_path, l'), p)

(*TODO: make this do the checking*)
let gotoChild (u: string) (((fs, working_path, l), p):t) : t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path), p) in
    if not (exists_helper fs p) then
      mk_err "gotochild - parent of new path ( %s ) does not exist" (Filename.concat p u)
    else if not (is_dir_helper fs p) then
      mk_err "gotochild - parent of new path ( %s ) is not a directory" (Filename.concat p u)
    else begin
      working_path := p;
      mk_ok ((fs, working_path, l), (Filename.concat p u))

(*TODO: make this do the checking*)
let goto (new_p:string) (((fs, working_path, l),p):t) : t or_fail =
  let (parent_dir, _) = Filename.split new_p in
    if not (exists_helper fs parent_dir) then
      mk_err "goto - parent of new path ( %s ) does not exist" new_p
    else if not (is_dir_helper fs parent_dir) then
      mk_err "goto - parent of new path ( %s ) is not a directory" new_p
    else begin
      working_path := parent_dir;
      mk_ok ((fs, working_path, l), new_p)

(*TODO: make this do the checking*)
let pop ( ((fs, working_path, l), p) :t) : t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path), p) in
  let (parent_dir, _) = Filename.split p in
  let (parent_parent_dir, _) = Filename.split parent_dir in
    working_path := parent_parent_dir;
    mk_ok ((fs, working_path, l), parent_dir)

(*TODO: make this do the checking*)
let fetch (((fs, working_path, l), p):t) : contents =
  fetch_helper fs p

let exists (((fs, working_path, l), p):t) : bool =
  exists_helper p

let is_dir (((fs, working_path, l), p):t) : bool =
  is_dir_helper fs p

let sync_path (((fs, working_path, l) ,p) : t) : t or_fail =
  let (parent_dir, dirname) = Filename.split p in
    if not (exists_helper fs parent_dir) then
      mk_err "sync_path - parent of new path ( %s ) does not exist" p
    else if not (is_dir_helper fs parent_dir) then
      mk_err "sync_path - parent of new path ( %s ) is not a directory" p
    else begin
      working_path := parent_dir;
      mk_ok ((fs, working_path), p)


let dummy_path = ""

let convert_result res = map_error res ~f:(fun err -> (OpError err))

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
