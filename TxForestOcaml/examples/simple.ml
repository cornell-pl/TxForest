open Core
open ExUtils
open Result

open Result.Let_syntax

open Forest
open OldForest
let always _ = true

let in_lines (lines : string) (s: string) : bool =
  let lines' = (String.split lines ~on:'\n') in
    List.mem ~equal:(fun s1 s2 -> s1 = s2) lines' s

let simple_spec =
  DPair(
    "index",
    PathExp(Name "index.txt", File),
    PathExp(Name "dir", Comp (PathExp (Var "x",File),"x", VDirList ("index", in_lines )))
  )
let simple_zipper = mk_zipper simple_spec


let get_info_for_id (id: int) (zipper:t) : t_or_fail =
 down zipper >>= next >>= down >>= goto_pos_comp id >>= (fun z -> get_and_print z; mk_ok zipper)


let get_info_for_name (name:string) (zipper: t) : t_or_fail =
  down zipper >>= next >>= down >>= goto name >>= (fun z -> get_and_print z; mk_ok zipper)

let test_path z =
  (*p "%s\n" (sync_path "/a/b/c/d" "/a/b/c/d/e")*)
  let path0 = get_forest_path z in
  let path0' = get_zfs_path z in
    p "original path -- forest: %s zfs: %s \n" path0 path0';

  let%bind z' = down z in

  let path1 = get_forest_path z in
  let path1' = get_zfs_path z in
    p "original after one down -- forest: %s zfs: %s \n" path1 path1';

  let path2 = get_forest_path z' in
  let path2' = get_zfs_path z' in
    p "down path after one down -- forest: %s zfs: %s \n" path2 path2';

  let%bind z'' = down z' in

  let path3 = get_forest_path z in
  let path3' = get_zfs_path z in
    p "original after two down -- forest: %s zfs: %s \n" path3 path3';

  let path4 = get_forest_path z' in
  let path4' = get_zfs_path z' in
    p "down path after two down -- forest: %s zfs: %s \n" path4 path4';

  let path5 = get_forest_path z'' in
  let path5' = get_zfs_path z'' in
    p "down down path after second down -- forest: %s zfs: %s \n"  path5 path5';

  let%bind z''' = next z' in

  let path6 = get_forest_path z in
  let path6' = get_zfs_path z in
    p "original after two down & right -- forest: %s zfs: %s \n" path6 path6';

  let path7 = get_forest_path z' in
  let path7' = get_zfs_path z' in
    p "down path after two down right -- forest: %s zfs: %s \n" path7 path7';

  let path8 = get_forest_path z'' in
  let path8' = get_zfs_path z'' in
    p "down down path after second down -- forest: %s zfs: %s \n"  path8 path8';

  let path9 = get_forest_path z''' in
  let path9' = get_zfs_path z''' in
    p "right path after two down one right -- forest: %s zfs: %s \n" path9 path9';
    mk_ok z


let test_syncpath z =
  (*p "%s\n" (sync_path "/a/b/c/d" "/a/b/c/d/e")*)
  let path0 = get_forest_path z in
  let path0' = get_zfs_path z in
    p "original path -- forest: %s zfs: %s \n" path0 path0';

  let%bind z' = sync_path z in
  let%bind z' = down z' in

  let path1 = get_forest_path z in
  let path1' = get_zfs_path z in
    p "original after one down -- forest: %s zfs: %s \n" path1 path1';

  let path2 = get_forest_path z' in
  let path2' = get_zfs_path z' in
    p "down path after one down -- forest: %s zfs: %s \n" path2 path2';

  let%bind z'' = sync_path z' in
  let%bind z'' = down z'' in

  let path3 = get_forest_path z in
  let path3' = get_zfs_path z in
    p "original after two down -- forest: %s zfs: %s \n" path3 path3';

  let path4 = get_forest_path z' in
  let path4' = get_zfs_path z' in
    p "down path after two down -- forest: %s zfs: %s \n" path4 path4';

  let path5 = get_forest_path z'' in
  let path5' = get_zfs_path z'' in
    p "down down path after second down -- forest: %s zfs: %s \n"  path5 path5';

  let%bind z''' = sync_path z' in
  let%bind z''' = next z''' in

  let path6 = get_forest_path z in
  let path6' = get_zfs_path z in
    p "original after two down & right -- forest: %s zfs: %s \n" path6 path6';

  let path7 = get_forest_path z' in
  let path7' = get_zfs_path z' in
    p "down path after two down right -- forest: %s zfs: %s \n" path7 path7';

  let path8 = get_forest_path z'' in
  let path8' = get_zfs_path z'' in
    p "down down path after second down -- forest: %s zfs: %s \n"  path8 path8';

  let path9 = get_forest_path z''' in
  let path9' = get_zfs_path z''' in
    p "right path after two down one right -- forest: %s zfs: %s \n" path9 path9';
    mk_ok z







let count_nodes (fold) (z: t) =
  fold ~f:(fun i z ->
    match shallow_fetch z with
    | path_string, FileRep s -> begin
      p "path: %s File: %s \n" path_string s;
      i+1
    end
    | path_string, PathRep -> begin
      p "path before down: %s path\n" path_string;
      let _ = down z in
        i + 1
    end
    | path_string, PairRep -> begin
      p "path before down: %s pair\n" path_string;
      let _ = down z in
        i + 1
    end
    | path_string, CompRep _ -> begin
      p "path before down: %s comp\n" path_string;
      let _ = down z in
        i + 1
    end
  ) 0 z

let main (trans :int) (id: int option) (name:string option) (debug: bool) (zipper:t) : t =
  begin
    if debug then OldForest.set_debug () else ();
    match trans with
    | 1 -> begin
      match id with
      | Some i -> get_info_for_id i zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 2 -> begin
      match name with
      | Some n -> get_info_for_name n zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 3 -> begin
      p "%d \n" (count_nodes fold_postorder zipper);
      mk_ok zipper
    end
    | 4 -> begin
      test_path zipper
    end
    | 5 -> begin
      test_syncpath zipper
    end
    | _ -> failwith "Transaction %d is not implemented yet" trans
  end
  |> function
  | Ok zipper -> zipper
  | Error s -> p "Error: %s" s; zipper



let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs various transactions on the 'simple' filestore "
    [%map_open
      let trans = flag "trans" (required int) ~doc:"[1-2] Run this transaction"
      and id = flag "id" ( optional int )
                  ~doc:"index of the file"
      and name = flag "name" (optional string)
                  ~doc:"name of the file"
      and debug = flag "debug" (no_arg)
                  ~doc:"should print the debugging statements"
      in

        run_txn simple_spec "/simple" ~f:(main trans id name debug)
    ]
    |> Command.run