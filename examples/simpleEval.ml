open Core

open Forest
open Utils
open ForestIntf
open Result
open Result.Let_syntax

let lines = Fn.compose String.Set.of_list String.split_lines

open ForestIntf.TxForestCoreExn
[%%txforest {|

  d = directory {
    index is "index.txt" :: file;
    dir is "dir" :: [x :: file | x <- $down index |> fetch_file |> lines$ ]
  }

|}]

module TxForest = ForestIntf.TxForestCore
open TxForest
open Derived
let print_file z = fetch_file z  >>| p "File Contents: %s" >>= fun _ -> mk_ok z

(* down zipper >>= next >>= down >>= goto_pos_comp id >>= (fun z -> get_and_print z; mk_ok zipper) *)
let get_info_for_id (id: int) z =
  goto_dir_name "dir" z
  >>= down
  >>= goto_comp_pos id
  >>= down
  >>= print_file

let get_info_for_name (name:string) z =
  goto_dir_name "dir" z
  >>= down
  >>= goto_comp_name name
  >>= down
  >>= print_file


let explore z =
  into_pair z
  >>= next >>= into_pair >>= down
  >>= (fun z -> print z; mk_ok z)


(* TODO: Decide if you want it to look more like this:
      | _ -> p "Missing parameter for transaction: %d" trans; mk_ok zipper
*)
let main (trans :int) (id: int option) (name:string option) (debug: bool) zipper =
  begin
    if debug then Utils.set_debug () else ();
    match trans with
    | 0 -> begin
      explore zipper
    end
    | 1 -> begin
      match id with
      | Some i -> get_info_for_id i zipper
      | _ -> failwithf "Missing parameter for transaction: %d" trans ()
    end
    | 2 -> begin
      match name with
      | Some n -> get_info_for_name n zipper
      | _ -> failwithf "Missing parameter for transaction: %d" trans ()
    end
    | _ -> failwithf "Transaction %d is not implemented yet" trans ()
  end
  |> function
  | Ok zipper -> zipper
  | Error s -> p "Error: %s" s; zipper


let ignore_after_f (f : 'a -> 'b) = Fn.compose Core.ignore f

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs various transactions on the 'simple' filestore "
    [%map_open
      let trans = flag "trans" (required int) ~doc:"[0-2] Run this transaction"
      and id = flag "id" ( optional int )
                  ~doc:"index of the file"
      and name = flag "name" (optional string)
                  ~doc:"name of the file"
      and debug = flag "debug" (no_arg)
                  ~doc:"should print the debugging statements"
      in
      if debug then Utils.set_debug () else ();
      ignore_after_f (loop_txn_noExn d_spec "/Users/katie/Documents/TxForest/example_fs_root/simple" ~f:(main trans id name debug))
    ]
    |> Command.run

