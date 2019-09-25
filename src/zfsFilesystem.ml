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
open Zfs
open ZFSAPI
open Result
open Result.Let_syntax
open Utils


(* ---------------   Types ---------------- *)

(*note: the filesytem is the one above this path*)
type fs = ZFSAPI.t
type path = string
type contents = Dir of string list | File of string

type t = fs * path


(* ----------   Helper Functions ---------- *)
let convert_result =
  map_error ~f:(function
  | IllegalAction s -> s
  | CommitFail -> "Failed to commit")

(* TODO: Does not properly handle being at root (probably) *)
let getpu = Core.Filename.split

let add_to_path p u =
  let p_lst = List.rev (String.split p ~on:'/') in
  let p_lst' = u :: p_lst in
    String.concat ~sep:"/" (List.rev p_lst')


let (>>*) fs f =
  match fs with
  | Ok fs' -> f fs'
  | _ -> failwith "filesystem is not OK"



(* ----------   Exposed Functions ---------- *)


let create (p': path) : t or_fail =
  let (p, u) = getpu p' in
  let%map fs = ZFSAPI.create () |> ZFSAPI.goto p |> convert_result in
    (fs, p')

let make_file ((fs, p) :t) (u: string) :t or_fail =
  let (p', u') = getpu p in
  let fs' = if has_child u' fs then remove_child u' fs else Ok fs in
  let%map fs'' = fs' >>= mkfile u' >>= goto_child u' >>= update u >>= up |> convert_result in
    (fs'', p)


let make_directory ((fs, p): t) ( new_lst: string list) : t or_fail =
  let (p', u') = getpu p in
  if has_child u' fs then
    let fs' = goto_child u' fs in
      match fs' >>* fetch  with
      | ZFSDir cur_lst -> begin
        let mem = List.mem ~equal:(fun a b -> a = b) in
        let add_lst = List.fold new_lst ~init:[] ~f:(fun lst u -> if mem cur_lst u then lst else u::lst) in
        let rem_lst = List.fold cur_lst ~init:[] ~f:(fun lst u -> if mem new_lst u then lst else u::lst) in
        let fs'' = List.fold add_lst ~init:fs' ~f:(fun fs u -> fs >>= mkfile u) in
        let fs''' = List.fold rem_lst ~init:fs'' ~f:(fun fs u -> fs >>= remove_child u) in
        let%map fs4 = fs''' >>= up |> convert_result in
          (fs4 , p)
      end
      | _ -> begin
        let fs'' = fs' >>= up >>= remove_child u' in
        let fs''' = fs'' >>= mkdir u' >>= goto_child u' in
        let fs4 = List.fold new_lst ~init:fs''' ~f:(fun fs u -> fs >>= mkfile u) in
        let%map fs5 = fs4 >>= up |> convert_result in
          (fs5, p)
      end
  else
    let fs' = mkdir u' fs >>= goto_child u' in
    let fs'' = List.fold new_lst ~init:fs' ~f:(fun fs u -> fs >>= mkfile u) in
    let%map fs''' = fs'' >>= up |> convert_result in
      (fs''', p)

let add_to_directory ((fs, p): t) (u:string) : t or_fail =
  let (p', u') = getpu p in
  if has_child u' fs then
    let fs' = goto_child u' fs in
    let fs'' = if fs' >>* is_dir then fs' else fs' >>= up >>= remove_child u' >>= mkdir u' >>= goto_child u' in
    let fs''' = if fs'' >>* has_child u then fs'' else fs'' >>= mkfile u  in
    let%map f4 = fs''' >>= up |> convert_result in
      (f4, p)
  else
    let%map fs' = mkdir u' fs >>= goto_child u' >>= mkfile u >>= up |> convert_result in
      (fs', p)

let gotoChild (u: string) ((fs, p):t) : t or_fail =
  let (_, u') = getpu p in
  let%map fs' = goto_child u' fs |> convert_result in
    (fs', add_to_path p u)

let goto (p:string) ((fs,_):t) : t or_fail =
  let (p', u') = getpu p in
  let%map fs' = ZFSAPI.goto p' fs |> convert_result in
  let new_p = Filename.concat (ZFSAPI.get_global_path fs') u' in
    d "After initialization: Path = '%s' and Global Path = '%s'" p new_p;
    (fs', new_p)

let pop ( (fs, p) :t) : t or_fail =
  let (p', u') = getpu p in
  let%map fs' = up fs |> convert_result in
    (fs', p')

let fetch ((fs, p):t) : contents =
  let (_, u') = getpu p in
  let fs' = goto_child u' fs in
  let v =
    match fs' >>* fetch with
    | ZFSDir lst -> Dir lst
    | ZFSFile u -> File u
  in
  let _ = fs' >>= up in
    v

let exists ((fs, p):t) : bool =
  let (_, u') = getpu p in
  has_child u' fs

let is_dir (t:t) : bool = fetch t |> function
  | Dir _ -> true
  | File _ -> false

let rec get_diff ol nl =
  match ol, nl with
  | h1::t1, h2::t2 when h1 = h2 -> get_diff t1 t2
  | _ -> ol, nl

let sync_path ((fs,p) : t) : t or_fail =
  let (p', u') = getpu p in
  let old_path = ZFSAPI.get_global_path fs in
  let _ = d "syncing path -- old path: %s new path: %s" old_path p' in
  if old_path = p' then mk_ok (fs, p)
  else
    let old_list = Filename.parts old_path in
    let new_list = Filename.parts p' in
    let ol,nl = get_diff old_list new_list in
    let path = List.map ol ~f:(function | "." -> "." | _ -> "..") @ nl |> Filename.of_parts in
    let%bind fs' = ((ZFSAPI.goto path fs) |> convert_result) in
    mk_ok (fs', p)

let loop_txn ~(f : fs -> 'a) () =
  let x : 'a option ref = ref None in
  let apply fs =
    x := Some(f fs);
    fs
  in
  run_txn ~f:apply ();
  Option.value_exn ~message:"loop_txn: Something went horribly wrong" !x

let run_txn ~f () =
  let fs = ZFSAPI.create () in
  match f fs with
  | Error str ->
    destroy_no_commit fs;
    Error (OpError str)
  | Ok x -> 
    if destroy fs
    then Ok x
    else Error TxError


let dummy_path = ""

