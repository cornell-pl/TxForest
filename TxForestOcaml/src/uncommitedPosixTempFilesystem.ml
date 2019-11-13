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
type fs = path * path (*root, the temp path*)

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

let make_dir_h (p:path) (new_lst: string list) =
  if is_file_h p then remove_file_h p;
  if not (exists_h p) then Unix.mkdir ~perm:default_perm p;
  let (parent_dir, dirname) = Filename.split p in
  let cur_lst = Sys.ls_dir p in
  let (rem_lst: string list) = List.filter cur_lst ~f:(fun u -> not (List.mem new_lst u ~equal:String.equal)) in
  let (add_lst: string list) = List.filter new_lst ~f:(fun u -> not (List.mem cur_lst u ~equal:String.equal)) in
    Unix.chdir p;
    List.iter rem_lst ~f:(fun u -> remove_h (Filename.concat p u));
    List.iter add_lst ~f:(fun u -> let _ = make_file_h (Filename.concat p u) "" in ());
    Unix.chdir parent_dir;

let move_children (real_path: path) (temp_path:path) =
  let childen = Sys.ls_dir temp_path in
    List.iter childen ~f:(fun u ->
      let temp_child_path = Filename.concat temp_path u in
      let real_child_path = Filename.concat real_path u in
      if is_dir_h temp_child_path then begin
        let childrens_children = Sys.ls_dir real_child_path in
          List.iter childrens_children ~f:(fun u ->
            if not exists_h (Filename.concat temp_child_path u) then begin
              if is_file_h (Filename.concat real_child_path u) then
                let file_contents = In_channel.read_all (Filename.concat real_child_path u) in
                  make_file_h (Filename.concat temp_child_path u) file_contents
              else
                make_dir_h (Filename.concat temp_child_path u) []
            end
          )
      end
    )


let check_path_preconditions ((fs, p): t) :t or_fail=
  let working_dir = Unix.getcwd () in
  let (parent_dir, _) = Filename.split fs in
  let (real_parent_dir, _) = Filename.split p in
    if not(exists_h parent_dir) then
      mk_err "path precondition check - parent of curent path in temp ( %s ) does not exist" fs
    else if not (is_dir_h parent_dir) then
      mk_err "path precondition check - parent of curent path in temp ( %s ) is not a drectory" fs
    else if not(exists_h real_parent_dir) then
      mk_err "path precondition check - real parent of curent path( %s ) does not exist" p
    else if not (is_dir_h real_parent_dir) then
      mk_err "path precondition check - real parent of curent path in temp ( %s ) is not a drectory" p
    else if working_dir <> parent_dir then
      mk_err "path precondition check - working directory in temp ( %s ) does not match parent of current path ( %s )" working_dir fs
    else
      mk_ok (fs, p)


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
      let temp = Filename.concat parent_dir "temp" in
      let children = Sys.ls_dir path in
        make_dir_h temp [];
        Sys.chdir parent_dir;
        List.iter children ~f:(fun u ->
          if is_file_h (Filename.concat path u) then
            let file_contents = In_channel.read_all (Filename.concat path u) in
              make_file_h (Filename.concat temp u) file_contents
          else
            make_dir_h (Filename.concat temp u) []
        );
        mk_ok ((parent_dir, temp), p)
    end

let make_file (u: string) (((root_path, temp), p) :t)  :t or_fail =
  let%bind _ = check_path_preconditions (temp, p) in
    make_file_h temp u;
    mk_ok ((root_path, temp), p)

let make_directory ( new_lst: string list) (((root_path, temp), p): t)  : t or_fail =
  let%bind _ = check_path_preconditions (temp, p) in
    make_dir_h temp new_lst;
    mk_ok ((root_path, temp), p)

let add_to_directory (u:string) (((root_path, temp), p): t)  : t or_fail =
  let%bind _ = check_path_preconditions (temp, p) in
    if is_file_h temp then remove_file_h temp;
    if not (exists_h temp) then Unix.mkdir ~perm:default_perm temp;
    let (parent_dir, _) = Filename.split temp in
    let p' = Filename.concat temp u in
      Unix.chdir temp;
      make_file_h (Filename.concat temp u) "";
      Unix.chdir parent_dir;
      mk_ok ((root_path, temp), p)

let gotoChild (u: string) (((root_path, temp), p):t) : t or_fail =
  let%bind _ = check_path_preconditions (fs, p) in
    if not (exists_h temp) then
      mk_err "gotochild - parent of new path ( %s ) does not exist" (Filename.concat temp u)
    else if not (is_dir_h temp) then
      mk_err "gotochild - parent of new path ( %s ) is not a directory" (Filename.concat temp u)
    else begin
      let new_path = Filename.concat temp u in
      let new_real_path = Filename.concat p u in
      Unix.chdir temp;
      move_children new_path new_real_path;
      mk_ok ((root_path, new_path), new_real_path)
    end


let goto (new_p:string) (((root_path, temp),p):t) : t or_fail =
  let rel_new_par = Filename.make_relative ~src:root_path new_p
  let peices = Filename.explode rel_new_par in
  let (real_root :: _) = Filename.explode (Filename.make_relative ~src:root_path p) in
      Unix.chdir root_path;
      let rec it_bind us t =
        match us with
        | [] -> mk_ok t
        | hd :: tl -> gotoChild hd t >>= it_bind tl
      in
      it_bind peices ((root_path, Filename.concat root_path "temp"), Filename.concat root_path real_root)

let pop ( ((root_path, temp), p) :t) : t or_fail =
  let%bind _ = check_path_preconditions (temp, p) in
  let (parent_dir, _) = Filename.split temp in
  let (real_parent_dir, _) = Filename.split temp in
  let (parent_parent_dir, _) = Filename.split parent_dir in
    Unix.chdir parent_parent_dir;
    mk_ok ((root_path, parent_dir), real_parent_dir)


let exists (((root_path, temp), p):t) : bool = exists_h temp

let is_dir (((root_path, temp), p):t) : bool = is_dir_h temp

let fetch (((root_path, temp), p):t) : contents =
  match check_path_preconditions (temp, p) with
  | Ok _ -> begin
    if is_dir_h temp then
      let dir_contents = Sys.ls_dir temp in
        Dir dir_contents
    else
      let file_contents = In_channel.read_all temp in
        File file_contents
  end
  | Error e -> failwith e

let sync_path (((root_path, temp),p) : t) : t or_fail =
  let (parent_dir, dirname) = Filename.split temp in
    if not (exists_h parent_dir) then
      mk_err "sync_path - parent of new path ( %s ) does not exist" p
    else if not (is_dir_h parent_dir) then
      mk_err "sync_path - parent of new path ( %s ) is not a directory" p
    else begin
      Unix.chdir parent_dir;
      mk_ok ((root_path, temp), p)
    end

let dummy_path =
  let potential_root = Filename.concat (Unix.getcwd ()) "example_fs_root" in
    if exists_h potential_root then potential_root else Filename.root

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


