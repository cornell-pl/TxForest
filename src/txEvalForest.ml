



open Core
open Result
open Result.Let_syntax
open Filesystems
open Utils


(* Types *)
type fs = TempFS.fs
type path = string

type fetch_rep =
  | FileRep of string
  | DirRep of SSet.t
  | PathRep of name
  | PairRep of Var.t
  | CompRep of SSet.t
  | OptRep of bool
  | PredRep of bool
  | NullRep [@@deriving show]

type fetch_result = fetch_rep

type forest_navigation =
  | Down
  | Up
  | Into_Pair
  | Into_Comp
  | Into_Opt
  | Out
  | Next
  | Prev

type forest_update =
  | Store_File of string fexp
  | Store_Dir of SSet.t fexp
  | Create_Path

type forest_command = Nav of forest_navigation | Update of forest_update

type specification =
  | Null
  | File
  | Dir
  | PathExp of name fexp * specification
  | DPair of Var.t * specification * specification
  | Comp of specification * Var.t * SSet.t fexp
  | Opt of specification
  | Pred of bool fexp

and 'a fexp = fs -> env -> ('a * local_log)

and direnv = ctxt Var.Map.t
and compenv = string Var.Map.t
and env = direnv * compenv

and node = env * specification

and zipper =
  { ancestor : zipper option;
    left : node list;
    current : node;
    right: node list;
  }

(*also known as context in the paper*)
and ctxt = env * path * PathSet.t * zipper


(*extra types not in the mli*)
and contents = Filesystems.contents


(* log_entry
 * Read (past tense) contents at path
 * Wrote second contents at path where there use to be first contents
 *)
and le =
  | Read of contents * path
  | Write_file of contents * contents * path
  | Write_directoy of contents * contents * path

(* timestamp
 *)
and ts = float


(* local log
 * list of things that I have done and would liek to commit
 * note: we dont have timestamps here since, none of these actions have
 * been commited yet!
 *)
and local_log = le list

(* log
 * list of commited timestamped log entries, where the timestamp indicates the
 * time at which the thing was commited
 *)
type log = (ts * le) list


type thread = ctxt * fs * forest_command

type tx_state = forest_command * ts * local_log

type transaction = tread * tx_state

type thread_pool = transaction list

type t = ctxt * fs * local_log


let global_log : log = ref []
let global_fs : (PermFS.t or_fail) ref = ref (PermFS.create PermFS.dummy_path)


let make_zipper ?ancestor ?(left=[]) ?(right=[]) current = {ancestor; left; current; right}
let add_to_dirEnv ~key ~data : env -> env = Tuple.T2.map_fst ~f:(Map.set ~key ~data)
let add_to_compEnv ~key ~data : env -> env = Tuple.T2.map_snd ~f:(Map.set ~key ~data)


let eval_forest_navigation fn (ctxt, fs, _) : t or_fail =
(*   let%bind (fs, p, ps, z) = sync_path t in *)
  let (env, p, ps, z) = cxt in
  match fn, z  with
  | Down, {current = (env_l, PathExp (e, s) ); _ } -> begin
      let (u, l) = f fs env in
        if is_dir (fs,p)
        then
          let%map (fs', p') = TempFS.gotoChild u (fs, p) in
          let l' = (Read ((Dir []),  p) ) :: l in
          let z' = make_zipper ~ancestor:z (env_l, s) in
           ((env, p',PathSet.add ps p', z'), fs',  l')
        else mk_err "Down requires the current filesystem node to be directory. %s is not" p
  end
  | Up, {ancestor = Some(z');_} when is_path z'-> begin
      let%map (fs', p') = TempFS.pop (fs, p) in
        ((env, p', ps, z'), fs', [])
  end
  | Into_Pair, {current = (env_l, DPair(x,s1,s2)); _ } -> begin
      let ctxt' = (env_l, p, ps, make_zipper (env_l, s1)) in
      let env_r = add_to_dirEnv ~key:x ~data:ctxt' env_l in
      let z' = make_zipper ~ancestor:z (env_l, s1) ~right:[(envR, s2)] in
        mk_ok ((env, p, ps, z'), fs, [])
  end
  | Into_Comp, {current = (env_l, Comp(s, x, f)); _ } -> begin
      let (us, l) = f fs env in
      match us |> Set.to_list with
      | hd :: tl ->
        let r = List.map tl ~f:(fun u -> (add_to_compEnv ~key:x ~data:u env, s)) in
        let cur = (add_to_compEnv ~key:x ~data:hd env_l, s)
        let z' = make_zipper ~ancestor:z cur ~right:r in
          mk_ok ((env, p, ps, z'), fs, l)
      | [] -> mk_err "Into_Comp requires the comprehension to be non-empty"
  end
  | Into_Opt, {current = (env, Opt s); _ } -> begin
      let z' = make_zipper ~ancestor:z (env,s) in
        mk_ok ((env, p, ps, z'), fs, [])
  end
  | Out, {ancestor = Some(z')} when not (is_path z') -> begin
      mk_ok ((env, p, ps, z'), fs, [])
  end
  | Next, {ancestor; left; current; right=(new_current::right)}-> begin
      let z' = make_zipper ~ancestor:ancestor ~left:(current::left) new_current ~right:right in
        mk_ok ((env, p, ps, z'), fs, [])
  end
  | Prev, { ancestor; left=(new_current::left); current; right} -> begin
      let z' = make_zipper ~ancestor:ancestor ~left:left new_current ~right:(current :: right) in
        mk_ok ((env, p, ps, z'), fs, [])
  end
  (* Nicer Error handling *)
  | Down, _ -> mk_err "Down can only be used at a path node"
  | Up, _ -> mk_err "Up can only be used when a path node is the ancestor"
  | Into_Pair, _ -> mk_err "Into_Pair can only be used at a pair node"
  | Into_Comp, _ -> mk_err "Into_Comp can only be used at a comprehension node"
  | Into_Opt, _ -> mk_err "Into_Opt can only be used at an option node"
  | Out, _ -> mk_err "Out can only be used when there is a non-path node ancestor"
  | Next, _ -> mk_err "Next can only be used when there is a next node"
  | Prev, _ -> mk_err "Prev can only be used when there is a previous node"


let eval_forest_update fu (ctxt, fs, _) : t or_fail =
  (*   let%bind (fs, p, ps, z) = sync_path t in *)
  let (env, p, ps, z) = cxt in
  match fu , z with
  | Store_File f, {current = (_, File); _ } -> begin
      let (u, l) = f fs env in
      let%bind (fs', p') = TempFS.make_file (fs, p) u in
      let l' = TempFS.get_log (fs', p') in
      let%map (fs'', p'') = TempFS.clear_log (fs', p') in
        ((env, p'', ps, z), fs'', l @ l')
  end
  | Store_Dir f, {current = (_, Dir); _ } ->begin
      let (s, l) = f fs env in
      let%bind (fs', p') = TempFS.make_directory (fs, p) (Set.to_list s) in
      let l' = TempFS.get_log (fs', p') in
      let%map (fs'', p'') = TempFS.clear_log (fs', p') in
        ((env, p'', ps, z), fs'', l @ l')
  end
  | Create_Path, {current = (env, PathExp (f,s)); _ } -> begin
      let (u, l) = f fs env in
      let%bind (fs', p') = TempFS.add_to_directory (fs, p) u in
      let l' = TempFS.get_log (fs', p') in
      let%map (fs'', p'') = TempFS.clear_log (fs', p') in
        ((env, p'', ps, z), fs'', (Read (File "") p)::(l @ l'))
  end
  (* Nicer Error handling *)
  | Store_File _, _ -> mk_err "Store_File can only be used at a file node"
  | Store_Dir _, _ -> mk_err "Store_Dir can only be used at a directory node"
  | Create_Path, _ -> mk_err "Create_Path can only be used at a path node"


let eval_forest_command fc (ctxt, fs, _) : t or_fail =
  match fc with
  | Nav fn -> eval_forest_navigation fn (ctxt, fs, [])
  | Update fu -> eval_forest_update fu (ctxt, fs, [])


let conflict_path (p':path) (e: le) : bool =
  match e with
  | Read _ -> false
  | Write_file (_, _, p) -> String.is_prefix p' ~prefix:p
  | Write_dir (_, _, p) -> String.is_prefix p' ~prefix:p

let rec extract_paths (ll: local_log) : path list =
  match ll with
  | [] -> []
  | (Read (_, p))::tl -> p::(extract_paths tl)
  | (Write_file (_, _, p))::tl -> p::(extract_paths tl)
  | (Write_dir (_, _, p))::tl -> p::(extract_paths tl)

let check_log (gl: log) (ll: local_log) (ts: timestamp) : bool =
  let paths = extract_paths ll in
  let b = List.map paths ~f:(fun p ->
      let conflicts = List.map gl ~f:(fun (ts', le) -> ts' < ts || not (conflict_path p le) ) in
        not (List.mem conflicts false ~equal:(fun b1 b2 -> b1 = b2))
  ) in
    not (List.mem conflicts false ~equal:(fun b1 b2 -> b1 = b2))


let update_global_fs (ll: local_log) (fs: PermFS.t)  : PermFS.t or_fail =
  match ll with
  | [] -> mk_ok fs
  | (Read _) :: tl -> update_global_fs tl fs
  | (Write_file (_ , File (u) , p)) :: tl -> begin
    PermFS.goto p fs >>= PermFS.make_file u >>= update_global_fs tl
  end
  | Write_directoy (_ , Dir (l), p) -> begin
    PermFS.goto p fs >>= PermFS.make_directory lst >>= update_global_fs tl
  end
  | (Write_file (_ , _ , _)) :: _ -> mk_err "you tryed to write somethin other than a file with Write_file"
  | (Write_dir (_ , _ , _)) :: _ -> mk_err "you tryed to write somethin other than a dir with Write_dir"


let merge (ll: local_log) : unit =
  let fs = !global_fs in
  let fs' = fs >>= (update_global_fs ll)
    global_fs := fs'

let update_global_log (ll: local_log) : unit =
  let ts = Unix.time () in
  let ts_local_log = List.map ll ~f:(fun le -> (ts, le)) in
    global_log := !global_log @ ll


let commit (fc : forest_command) ((ctxt, fs, _): t) : t or_fail =
  let new_thread = (ctxt, fs, []) in
  let ts = Unix.time () in
  let%bind (ctxt', fs', l) = eval_forest_command fc new_thread in
  if check_log (!global_log) l ts then begin
    (*there are no conflicts, transaction may proceed*)
    merge l;
    update_global_log l;
    mk_ok (ctxt', fs', [])
  end
  else begin
    (*there are conflicts, transaction may NOT proceed*)
    mk_err "Conflict when trying to commit command"
  end


let fetch t = failwith "TODO: Implement"

let verify t = failwith "TODO: Implement"

let check c t = failwith "TODO: Implement"

let loop_txn_noExn ~(f:t->'a) (s:specification) (p:string) () =
  let f_to_z (fs: fs) : 'a =
    match PermFS.goto p (fs, dummy_path) with
    | Error _ -> failwithf "loop_txn: path %s does not exist" p ()
    | Ok (fs', p') -> begin
      ((empty_env, p', PathSet.singleton p', make_zipper (empty_env, s)), fs', [])
      |> f
    end
  in
  PermFS.loop_txn ~f:f_to_z ()

let loop_txn ~f = loop_txn_noExn ~f:(Fn.compose Result.ok_or_failwith f)

let run_txn ~(f:t->'a or_fail) (s:specification) (p:string) () =
  let f_to_z (fs: fs) : 'a or_fail =
    match PermFS.goto p (fs, dummy_path) with
    | Error _ -> failwithf "run_txn: path %s does not exist" p ()
    | Ok (fs', p') ->
      ((empty_env, p', PathSet.singleton p', make_zipper (empty_env, s)), fs', [])
      |> f
  in
  PermFS.run_txn ~f:f_to_z ()



let run_command (fc : forest_command) (s:specification) (?p: path) () :unit =
  let env = empty_env in
  let p = match p with None -> TempFS.dummy_path | Some p' -> p' in
  let ps = PathSet.singleton ps in
  let z = make_zipper (empty_env, s) in
  let ctxt = (env, p, ps, z) in
  let fs = TempFS.create TempFS.dummy_path in
  let l = [] in
  let t = (ctxt, fs, l) in
    match commit fc t with
    | Ok _ -> Printf.printf "Command successfully commited!\n"
    | Error e -> Printf.printf "Command aborted because %s\n" e


let run_commands (fcs : forest_command list) (s:specification) (?p: path) () :unit =
  let env = empty_env in
  let p = match p with None -> TempFS.dummy_path | Some p' -> p' in
  let ps = PathSet.singleton ps in
  let z = make_zipper (empty_env, s) in
  let ctxt = (env, p, ps, z) in
  let fs = TempFS.create TempFS.dummy_path in
  let l = [] in
  let t = (ctxt, fs, l) in
  let rec run fcs t =
    match fcs with
    | [] -> mk_ok t
    | fc :: rest -> commit fc t >>= run rest
  in
    match run fcs t with
    | Ok _ -> Printf.printf "Commands successfully commited!\n"
    | Error e -> Printf.printf "Commands aborted because %s\n" e










