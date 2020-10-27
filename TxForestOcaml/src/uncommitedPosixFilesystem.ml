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

type fs_map = (contents option) FS.t

(*
 * contents that have been read or writen,
 * where we currently are in the fs
 * paths that have been removed from the fs
 *)
type fs = fs_map * path ref * log


type t = fs * path


(* ----------   Helper Functions ---------- *)

let get_full_path wp p = !wp ^/ p

let list_path_to_string_path (p: string list) : string =
  String.concat ~sep:"/" (List.rev p)

let string_path_to_list_path (p: string ) : string list =
  let path_pieces = String.split p ~on:'/' in
    List.rev path_pieces


let exists_helper (fs: fs_map) (p: path) : bool =
  if FS.mem fs p then
    (*in the cache of the file system*)
    let () = d "exists_helper: Path %s was in cache" p in
    match FS.find_exn fs p with
    | None -> false
    | _ -> true
  else
    (*not in the cache of the file system, check if its in the real fs*)
    let () = d "exists_helper: Path %s was not in cache" p in
    match Sys.file_exists ~follow_symlinks:false p with
    | `Yes -> true
    | _ -> false

let is_file_helper (fs: fs_map) (p : path) : bool =
  if FS.mem fs p then
    (*in the cache of the file system*)
    match FS.find_exn fs p with
    | Some(File _) -> true
    | _ -> false
  else
    (*not in the cache of the file system, check if its in the real fs*)
    match Sys.is_file ~follow_symlinks:false p with
    | `Yes -> true
    | _ -> false

let is_dir_helper (fs: fs_map) (p : path) : bool =
  if FS.mem fs p then
    (*in the cache of the file system*)
    match FS.find_exn fs p with
    | Some(Dir _) -> true
    | _ -> false
  else
    (*not in the cache of the file system, check if its in the real fs*)
    match Sys.is_directory ~follow_symlinks:false p with
    | `Yes -> true
    | _ -> false

let check_path_preconditions (((fs, working_path, l), p): t) :t or_fail=
  let fp = get_full_path working_path p in
  let parent_dir = Filename.dirname fp in
    if not(exists_helper fs parent_dir) then
      mk_err "path precondition check - parent of current path (%s) does not exist" fp
    else if not (is_dir_helper fs parent_dir) then
      mk_err "path precondition check - parent of current path (%s) is not a directory" fp
    (*else if not (String.equal !working_path parent_dir) then
      mk_err "path precondition check - working directory ( %s ) does not match parent ( %s ) of current path ( %s )" (!working_path) parent_dir p
    *)else
      mk_ok ((fs, working_path, l), p)

let remove_file_helper (fs: fs_map) ( p : path) : unit =
  FS.set fs ~key:p ~data:None

let rec remove_dir_helper (fs: fs_map) (p : path) : unit =
  begin
  if FS.mem fs p then
    (*in the cache of the file system*)
    match FS.find_exn fs p with
    | Some(Dir dir_contents) -> begin
      List.iter dir_contents ~f:(fun u -> remove_helper fs (Filename.concat p u))
    end
    | _ -> failwith "precondition of remove dir violated"
  else
    (*not in the cache of the file system, check if its in the real fs*)
    let dir_contents = Sys.ls_dir p in
      List.iter dir_contents ~f:(fun u -> remove_helper fs (Filename.concat p u));
  end;
  FS.set fs ~key:p ~data:None

and remove_helper (fs: fs_map) (p:path): unit =
  if is_file_helper fs p then
    remove_file_helper fs p
  else
    remove_dir_helper fs p

let make_file_helper (fs: fs_map) (p:path) (u: string) : unit =
  FS.set fs ~key:p ~data:(Some (File u))

let fetch_helper (fs: fs_map) (p:path): contents =
  if FS.mem fs p then
    (*in the cache of the file system*)
    match FS.find_exn fs p with
    | Some c -> c
    | _ -> failwith "fetch: path does not exist"
  else
    let () = d "Fetch helper found uncached path %s\n" p in
    (*not in the cache of the file system, check if its in the real fs*)
    if is_dir_helper fs p then
      let dir_contents = Sys.ls_dir p in
      let entry = Dir dir_contents in
        FS.set fs ~key:p ~data:(Some entry); entry
    else
      let file_contents = In_channel.read_all p in
      let entry = File file_contents in
        FS.set fs ~key:p ~data:(Some entry); entry

let fetch_dir_helper (fs: fs_map) (p:path): string list =
  match fetch_helper fs p with
  | Dir lst -> lst
  | _ -> failwith "fetch_dir_helper precondition violated"


(* ----------   Exposed Functions ---------- *)


let create (p': path) : t or_fail =
  let realPath = p' |> Filename.realpath in
  let fs = FS.create ~growth_allowed:true () in
(*     FS.set fs ~key:"/simple" ~data:(Dir ["index.txt";"dir"]);
    FS.set fs ~key:"/simple/index.txt" ~data:(File "a\nb\nc\nd");
    FS.set fs ~key:"/simple/dir" ~data:(Dir ["a"; "b"; "c"; "d"; "e"]);
    FS.set fs ~key:"/simple/dir/a" ~data:(File "apple");
    FS.set fs ~key:"/simple/dir/b" ~data:(File "banana");
    FS.set fs ~key:"/simple/dir/c" ~data:(File "carrot");
    FS.set fs ~key:"/simple/dir/d" ~data:(File "dragon fruit");
    FS.set fs ~key:"/simple/dir/e" ~data:(File "eggplant"); *)
    mk_ok ((fs, ref realPath, []),  ".")

(*[get_log t] gets the log out of the t*)
let get_log (((fs, working_path, l), p) :t) : log = l

let clear_log (((fs, working_path, l), p) :t) : t or_fail =
  mk_ok ((fs, working_path, []), p)

let make_file (u: string) (((fs, working_path, l), p) :t)  :t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path, l), p) in
  let fp = get_full_path working_path p in
  let l' = (Write_file (File "", File u, fp) ) :: l in
    if is_dir_helper fs fp then remove_dir_helper fs fp;
    make_file_helper fs fp u;
      mk_ok ((fs, working_path, l'), p)

(*TODO: make this do the checking*)
let make_directory ( new_lst: string list) (((fs, working_path, l), p): t)  : t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path, l), p) in
  let fp = get_full_path working_path p in
  let l' = (Write_directory (Dir [], Dir new_lst, fp)) :: l in
    if is_file_helper fs fp then remove_file_helper fs fp;
    let cur_lst = fetch_dir_helper fs fp in
    let (rem_lst: string list) = List.filter cur_lst ~f:(fun u -> not (List.mem new_lst u ~equal:String.equal)) in
    let (add_lst: string list) = List.filter new_lst ~f:(fun u -> not (List.mem cur_lst u ~equal:String.equal)) in
      List.iter rem_lst ~f:(fun u -> remove_helper fs (Filename.concat fp u));
      List.iter add_lst ~f:(fun u -> make_file_helper fs (Filename.concat fp u) "");
      FS.set fs ~key:fp ~data:(Some (Dir new_lst));
      mk_ok ((fs, working_path, l'), p)

(*TODO: make this do the checking and add the file?*)
let add_to_directory (u:string) (((fs, working_path, l), p): t)  : t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path, l), p) in
  let fp = get_full_path working_path p in
    if is_file_helper fs fp then remove_file_helper fs fp;
    if not (exists_helper fs fp) then FS.set fs ~key:fp ~data:(Some (Dir []));
    let lst = fetch_dir_helper fs fp in
    let new_lst = List.dedup_and_sort ~compare:String.compare (u::lst) in
    let l' = (Write_directory ((Dir lst), (Dir new_lst), fp)) :: l in
      FS.set fs ~key:fp ~data:(Some (Dir new_lst));
      if is_dir_helper fs (Filename.concat fp u) then remove_dir_helper fs (Filename.concat fp u);
      make_file_helper fs (Filename.concat fp u) "";
        mk_ok ((fs, working_path, l'), p)

(*TODO: make this do the checking*)
let gotoChild (u: string) (((fs, working_path, l), p):t) : t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path, l), p) in
  let parent = get_full_path working_path p in
    if not (exists_helper fs parent) then
      mk_err "gotochild - parent of new path ( %s ) does not exist" (Filename.concat p u)
    else if not (is_dir_helper fs parent) then
      mk_err "gotochild - parent of new path ( %s ) is not a directory" (Filename.concat p u)
    else begin
      (* working_path := p; *)
      mk_ok ((fs, working_path, l), p ^/ u)
    end

let split_by_prefix wp p =
  let lwp = Filename.parts wp in
  let lp = Filename.parts p in
  if List.is_prefix ~prefix:lwp ~equal:String.equal lp
  then List.drop lp (List.length lwp) |> Filename.of_parts |> mk_ok
  else mk_err "goto - working path ( %s ) is not a prefix of new path ( %s )" wp p 

(*TODO: make this do the checking*)
let goto (new_p:string) (((fs, working_path, l),p):t) : t or_fail =
  let new_p = !working_path ^/ p ^/ new_p |> Filename.realpath in
  let parent_dir = Filename.dirname new_p in
  let%bind new_p = split_by_prefix !working_path new_p in
    if not (exists_helper fs parent_dir) then
      mk_err "goto - parent of new path ( %s ) does not exist" new_p
    else if not (is_dir_helper fs parent_dir) then
      mk_err "goto - parent of new path ( %s ) is not a directory" new_p
    else begin
      (* working_path := parent_dir; *)
      mk_ok ((fs, working_path, l), new_p)
    end

(*TODO: make this do the checking*)
let pop ( ((fs, working_path, l), p) :t) : t or_fail =
  let%bind _ = check_path_preconditions ((fs, working_path, l), p) in
  if String.equal p "."
  then mk_err "pop - already at top"
  else
    let (parent_dir, _) = Filename.split p in
    (* let (parent_parent_dir, _) = Filename.split parent_dir in
      working_path := parent_parent_dir; *)
      mk_ok ((fs, working_path, l), parent_dir)

(*TODO: make this do the checking*)
let fetch (((fs, working_path, l), p):t) : contents =
  fetch_helper fs (!working_path ^/ p)

let exists (((fs, working_path, l), p):t) : bool =
  exists_helper fs (!working_path ^/ p)

let is_dir (((fs, working_path, l), p):t) : bool =
  is_dir_helper fs (!working_path ^/ p)

let sync_path (((fs, working_path, l) ,p) : t) : t or_fail =
  let (parent_dir, dirname) = Filename.split p in
    if not (exists_helper fs parent_dir) then
      mk_err "sync_path - parent of new path ( %s ) does not exist" p
    else if not (is_dir_helper fs parent_dir) then
      mk_err "sync_path - parent of new path ( %s ) is not a directory" p
    else begin
      working_path := parent_dir;
      mk_ok ((fs, working_path, l), p)
    end


let dummy_path = "."

let get_working_path (((_, working_path, _), p):t) : path =
  !working_path 

let convert_result res = map_error res ~f:(fun err -> (OpError err))

(*TODO: error handlling*)
let run_txn ~(f : t -> 'a or_fail) () : ('a,txError) Core.result =
  match create dummy_path with
  | Ok t -> f t |> convert_result
  | Error _ -> Error TxError


(* TODO: Why does this do what you think it does?! *)
let loop_txn ~(f : t -> 'a) () : 'a =
  let x : 'a option ref = ref None in
  let apply t =
    x := Some(f t);
    mk_ok t
  in
  let _ : (t,txError) Core.result = run_txn ~f:apply () in
  Option.value_exn ~message:"loop_txn: Something went horribly wrong" !x
