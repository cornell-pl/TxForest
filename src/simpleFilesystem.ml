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

type fs = contents FS.t * path ref

type t = fs * path


(* ----------   Helper Functions ---------- *)

let list_path_to_string_path (p: string list) : string =
  String.concat ~sep:"/" (List.rev p)

let string_path_to_list_path (p: string ) : string list =
  let path_peices = String.split p ~on:'/' in
    List.rev path_peices

(* ----------   Exposed Functions ---------- *)

let get_log t = []

let clear_log (((fs, working_path, l), p, l) :t) : t or_fail =
  mk_ok t

let create (p': path) : t or_fail =
  let fs = FS.create ~growth_allowed:true () in
    FS.set fs ~key:"/simple" ~data:(Dir ["index.txt";"dir"]);
    FS.set fs ~key:"/simple/index.txt" ~data:(File "a\nb\nc\nd");
    FS.set fs ~key:"/simple/dir" ~data:(Dir ["a"; "b"; "c"; "d"; "e"]);
    FS.set fs ~key:"/simple/dir/a" ~data:(File "apple");
    FS.set fs ~key:"/simple/dir/b" ~data:(File "banana");
    FS.set fs ~key:"/simple/dir/c" ~data:(File "carrot");
    FS.set fs ~key:"/simple/dir/d" ~data:(File "dragon fruit");
    FS.set fs ~key:"/simple/dir/e" ~data:(File "eggplant");
    mk_ok ((fs, ref p'), p')

(*TODO: make this do the checking*)
let make_file (u: string) (((fs, working_path), _) :t)  :t or_fail =
  let stringified_path = !working_path in
  let entry = File u in
  let _ = FS.set fs ~key:stringified_path ~data:entry in
    mk_ok ((fs, working_path), stringified_path)

(*TODO: make this do the checking*)
let make_directory ( new_lst: string list)  (((fs, working_path), _): t) : t or_fail =
  let stringified_path = !working_path in
  let entry = (Dir new_lst) in
  let _ = FS.set fs ~key:stringified_path ~data:entry in
    mk_ok ((fs, working_path), stringified_path)

(*TODO: make this do the checking and add the file?*)
let add_to_directory (u:string) (((fs, working_path), _): t)  : t or_fail =
  let stringified_path = !working_path in
  let cur_entry = FS.find fs stringified_path in
    match cur_entry with
    | Some (Dir lst) -> begin
        let entry = Dir (List.dedup_and_sort ~compare:String.compare (u::lst)) in
        let _ = FS.set fs ~key:stringified_path ~data:entry in
        let _ = FS.set fs ~key:(stringified_path ^ "/" ^ u) ~data:(File "") in
          mk_ok ((fs, working_path), stringified_path)
    end
    | _ -> failwith "unimplemted"

(*TODO: make this do the checking*)
let gotoChild (u: string) (((fs, working_path), _):t) : t or_fail =
  let stringified_path = !working_path in
  let list_path = string_path_to_list_path stringified_path in
  let updated_path = list_path_to_string_path (u::list_path) in
    working_path := updated_path;
    mk_ok ((fs, working_path), updated_path)

(*TODO: make this do the checking*)
let goto (p':string) (((fs, working_path),_):t) : t or_fail =
  working_path := p';
  mk_ok ((fs, working_path), p')

(*TODO: make this do the checking*)
let pop ( ((fs, working_path), _) :t) : t or_fail =
  let stringified_path = !working_path in
  match string_path_to_list_path stringified_path with
  | u :: p' -> begin
    let updated_path = list_path_to_string_path p' in
      working_path := updated_path;
      mk_ok ((fs, working_path), updated_path)
  end
  | _ -> failwith "unimplemented"

(*TODO: make this do the checking*)
let fetch (((fs, working_path), _):t) : contents =
  let stringified_path = !working_path in
    match FS.find fs stringified_path with
    | None -> failwith stringified_path
    | Some c -> c

let exists (((fs, working_path), _):t) : bool =
  let stringified_path = !working_path in
    FS.mem fs stringified_path

let is_dir (t:t) : bool = fetch t |> function
  | Dir _ -> true
  | File _ -> false


let sync_path (((fs, working_path) ,p) : t) : t or_fail =
  working_path := p;
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




