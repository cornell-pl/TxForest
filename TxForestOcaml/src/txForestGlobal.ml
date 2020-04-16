
open Core
open Utils

type t = unit

(* timestamp
 *)
type ts = float
(* log
 * list of commited timestamped log entries, where the timestamp indicates the
 * time at which the thing was commited
 *)
type global_log = (ts * le) list

let global_log : global_log ref = ref []
let global_mutex : Error_checking_mutex.t = Error_checking_mutex.create ()

let conflict_path (p':path) (e: le) : bool =
  match e with
  | Read _ -> false
  | Write_file (_, _, p) -> String.is_prefix p' ~prefix:p
  | Write_directoy (_, _, p) -> String.is_prefix p' ~prefix:p

let rec extract_paths (ll: log) : path list =
  match ll with
  | [] -> []
  | (Read (_, p))::tl -> p::(extract_paths tl)
  | (Write_file (_, _, p))::tl -> p::(extract_paths tl)
  | (Write_directoy (_, _, p))::tl -> p::(extract_paths tl)

let check_log (ll: log) (ts: ts) : bool =
  let gl = (!global_log) in
  let paths = extract_paths ll in
  let b = List.map paths ~f:(fun p ->
      let conflicts = List.map gl ~f:(fun (ts', le) -> Float.(<) ts' ts || not (conflict_path p le) ) in
        not (List.mem conflicts false ~equal:Bool.equal)
  ) in
    not (List.mem b false ~equal:Bool.equal)


let update_global_log (ll: log) : unit =
  let ts = Unix.time () in
  let ts_local_log = List.map ll ~f:(fun le -> (ts, le)) in
    global_log := ((!global_log) @ ts_local_log)


let commit (ll: log) (t : t) : t or_fail  =
  Error_checking_mutex.lock global_mutex;
  let ts = Unix.time () in
  if check_log ll ts then begin
    (*there are no conflicts, transaction may proceed*)
    update_global_log ll;
    mk_ok t
  end
  else begin
    (*there are conflicts, transaction may NOT proceed*)
    mk_err "Conflict when trying to commit command"
  end

let finish_commit t : t or_fail =
  Error_checking_mutex.unlock global_mutex;
  mk_ok t
