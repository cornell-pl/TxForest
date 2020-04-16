



open Core
open Result
open Result.Let_syntax
open Filesystems
open Utils


(* Types *)
type fs = TempFS.fs

type local_log = log ref


type fetch_result = Utils.fetch_rep [@@deriving show]

type forest_navigation =
  | Down
  | Up
  | Into_Pair
  | Into_Comp
  | Into_Opt
  | Out
  | Next
  | Prev [@@deriving show]

type forest_update =
  | Store_File of string fexp
  | Store_Dir of SSet.t fexp
  | Create_Path [@@deriving show]

and forest_command =
  | Nav of forest_navigation
  | Update of forest_update [@@deriving show]

and  specification =
  | Null
  | File
  | Dir
  | PathExp of name fexp * specification
  | DPair of Var.t * specification * specification
  | Comp of specification * Var.t * SSet.t fexp
  | Opt of specification
  | Pred of bool fexp [@@deriving show]

and 'a fexp = fs -> env -> 'a

and direnv = (path * zipper * local_log) Var.Map.t [@opaque]
and compenv = string Var.Map.t [@opaque]
and env = direnv * compenv

and node = env * specification

and zipper =
  { ancestor : zipper option;
    left : node list;
    current : node;
    right: node list;
  } [@@deriving show]

(*also known as context in the paper*)
type ctxt = path * PathSet.t * zipper


(*extra types not in the mli*)


type thread = ctxt * fs * forest_command

type t = ctxt * fs * local_log


let print_fetch_result = Fn.compose (Printf.printf "%s \n") show_fetch_result

let make_zipper ?ancestor ?(left=[]) ?(right=[]) current = {ancestor; left; current; right}
let add_to_dirEnv ~key ~data : env -> env = Tuple.T2.map_fst ~f:(Map.set ~key ~data)
let add_to_compEnv ~key ~data : env -> env = Tuple.T2.map_snd ~f:(Map.set ~key ~data)
let is_path = function
  | {current = (_, PathExp _);_} -> true
  | _ -> false

let empty_env : env = (Var.Map.empty, Var.Map.empty)


let eval_forest_navigation fn (ctxt, fs, l) : t or_fail =
(*   let%bind (fs, p, ps, z) = sync_path t in *)
  let p, ps, z = ctxt in
  match fn, z  with
  | Down, {current = (env_l, PathExp (f, s) ); _ } -> begin
      let u = f fs env_l in
      let old_log = !l in
        if TempFS.is_dir (fs,p)
        then
          let%map (fs', p') = TempFS.gotoChild u (fs, p) in
          let l' = (Read ((Dir []),  p) ) :: old_log in
          let z' = make_zipper ~ancestor:z (env_l, s) in
            l := l';
            ((p',PathSet.add ps p', z'), fs',  l)
        else mk_err "Down requires the current filesystem node to be directory. %s is not" p
  end
  | Up, {ancestor = Some(z');_} when is_path z'-> begin
      let%map (fs', p') = TempFS.pop (fs, p) in
        ((p', ps, z'), fs', l)
  end
  | Into_Pair, {current = (env_l, DPair(x,s1,s2)); _ } -> begin
      let env_r = add_to_dirEnv ~key:x ~data:(p, make_zipper (env_l, s1), l) env_l in
      let z' = make_zipper ~ancestor:z (env_l, s1) ~right:[(env_r, s2)] in
        mk_ok ((p, ps, z'), fs, l)
  end
  | Into_Comp, {current = (env_l, Comp(s, x, f)); _ } -> begin
      let us = f fs env_l in
      match us |> Set.to_list with
      | hd :: tl ->
        let r = List.map tl ~f:(fun u -> (add_to_compEnv ~key:x ~data:u env_l, s)) in
        let cur = (add_to_compEnv ~key:x ~data:hd env_l, s) in
        let z' = make_zipper ~ancestor:z cur ~right:r in
          mk_ok ((p, ps, z'), fs, l)
      | [] -> mk_err "Into_Comp requires the comprehension to be non-empty"
  end
  | Into_Opt, {current = (env, Opt s); _ } -> begin
      let z' = make_zipper ~ancestor:z (env,s) in
        mk_ok ((p, ps, z'), fs, l)
  end
  | Out, {ancestor = Some(z')} when not (is_path z') -> begin
      mk_ok ((p, ps, z'), fs, l)
  end
  | Next, {ancestor; left; current; right=(new_current::right)}-> begin
      let z' = make_zipper ?ancestor ~left:(current::left) new_current ~right:right in
        mk_ok ((p, ps, z'), fs, l)
  end
  | Prev, { ancestor; left=(new_current::left); current; right} -> begin
      let z' = make_zipper ?ancestor ~left:left new_current ~right:(current :: right) in
        mk_ok ((p, ps, z'), fs, l)
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


let eval_forest_update fu (ctxt, fs, l) : t or_fail =
  (*   let%bind (fs, p, ps, z) = sync_path t in *)
  let (p, ps, z) = ctxt in
  match fu , z with
  | Store_File f, {current = (env, File); _ } -> begin
      let u = f fs env in
      let old_log = !l in
      let%bind (fs', p') = TempFS.make_file u (fs, p) in
      let l' = TempFS.get_log (fs', p') in
      let%map (fs'', p'') = TempFS.clear_log (fs', p') in
        l := old_log @ l';
        ((p'', ps, z), fs'', l)
  end
  | Store_Dir f, {current = (env, Dir); _ } ->begin
      let s = f fs env in
      let old_log = !l in
      let%bind (fs', p') = TempFS.make_directory (Set.to_list s) (fs, p)  in
      let l' = TempFS.get_log (fs', p') in
      let%map (fs'', p'') = TempFS.clear_log (fs', p') in
        l := old_log @ l';
        ((p'', ps, z), fs'', l)
  end
  | Create_Path, {current = (env, PathExp (f,s)); _ } -> begin
      let u = f fs env in
      let old_log = !l in
      let%bind (fs', p') = TempFS.add_to_directory u (fs, p) in
      let l' = TempFS.get_log (fs', p') in
      let%map (fs'', p'') = TempFS.clear_log (fs', p') in
        l := (Read ((File ""), p) )::(old_log @ l');
        ((p'', ps, z), fs'', l)
  end
  (* Nicer Error handling *)
  | Store_File _, _ -> mk_err "Store_File can only be used at a file node"
  | Store_Dir _, _ -> mk_err "Store_Dir can only be used at a directory node"
  | Create_Path, _ -> mk_err "Create_Path can only be used at a path node"

let fetch ((p, ps, z), fs, l) : fetch_result or_fail =
(*   let%bind (fs, p, ps, z) = sync_path t in *)
  match z with
  | {current = (_, File); _ } ->
    begin
      match TempFS.fetch (fs, p) with
      | File u -> FileRep u |> mk_ok
      | _ -> mk_err "File expected, but node at path %s is not a file" p
    end
  | {current = (_, Dir); _ } ->
    begin
      match TempFS.fetch (fs, p) with
      | Dir l -> String.Set.of_list l |> DirRep |> mk_ok
      | _ -> mk_err "Directory expected, but node at path %s is not a directory" p
    end
  | {current = (env_l, PathExp (f,_)); _ } -> begin
    let u = f fs env_l in
      PathRep u |> mk_ok
  end
  | {current = (_, DPair (x,_,_)); _ } -> begin
    PairRep x |> mk_ok
  end
  | {current = (env_l, Comp (_,_,f)); _ } -> begin
    let u = f fs env_l in
      CompRep u |> mk_ok
  end
  | {current = (_, Opt _); _ } -> begin
    OptRep (TempFS.exists (fs,p)) |> mk_ok
  end
  | {current = (env_l, Pred f); _ } -> begin
    let u = f fs env_l in
      PredRep u |> mk_ok
  end
  | {current = (_, Null); _ } -> begin
    NullRep |> mk_ok
  end

let eval_forest_command fc (ctxt, fs, l) : t or_fail =
  match fc with
  | Nav fn -> eval_forest_navigation fn (ctxt, fs, l)
  | Update fu -> eval_forest_update fu (ctxt, fs, l)


let async_print t =
  let open Async in
  match fetch t with
    | Ok fr -> info_message "TXForest" (show_fetch_result fr)
    | Error u -> info_message "TXForest" u

let print t =
    match fetch t with
    | Ok fr -> print_fetch_result fr
    | Error u -> Printf.printf "print: %s\n" u

let print_ret t = f_ret ~f:print t

let debug_print ((_,_,z),_,_) = Printf.printf "%s\n" (show_zipper z)

let verify t = failwith "TODO: Implement"

let check c t = failwith "TODO: Implement"

let loop_txn_noExn ~(f:t->'a) (s:specification) (p:string) () =
  let f_to_z (fs: fs) : 'a =
    match TempFS.goto p (fs, TempFS.dummy_path) with
    | Error _ -> failwithf "loop_txn: path %s does not exist" p ()
    | Ok (fs', p') -> begin
      ((p', PathSet.singleton p', make_zipper (empty_env, s)), fs', ref [])
      |> f
    end
  in
  TempFS.loop_txn ~f:f_to_z ()

let loop_txn ~f = loop_txn_noExn ~f:(Fn.compose Result.ok_or_failwith f)

let run_txn ~(f:t->'a or_fail) (s:specification) (p:string) () =
  let f_to_z (fs: fs) : 'a or_fail =
    match TempFS.goto p (fs, TempFS.dummy_path) with
    | Error _ -> failwithf "run_txn: path %s does not exist" p ()
    | Ok (fs', p') -> begin
      ((p', PathSet.singleton p', make_zipper (empty_env, s)), fs', ref [])
      |> f
    end
  in
    TempFS.run_txn ~f:f_to_z ()

(* let run_command (fc : forest_command) (s:specification) ?(p: path option) () =
  let p = match p with None -> TempFS.dummy_path | Some p' -> p' in
  let ps = PathSet.singleton p in
  let z = make_zipper (empty_env, s) in
  let ctxt = (p, ps, z) in
  let fs = match TempFS.create TempFS.dummy_path with Ok (fs, p) -> fs | _ -> failwith "unable t make fs" in
  let l = ref [] in
  let t = (ctxt, fs, l) in
    match commit fc t with
    | Ok _ -> Printf.printf "Command successfully commited!\n"
    | Error e -> Printf.printf "Command aborted because %s\n" e


let run_commands (fcs : forest_command list) (s:specification) ?(p: path option) () :unit =
  let p = match p with None -> TempFS.dummy_path | Some p' -> p' in
  let ps = PathSet.singleton p in
  let z = make_zipper (empty_env, s) in
  let ctxt = (p, ps, z) in
  let fs = match TempFS.create TempFS.dummy_path with Ok (fs, p) -> fs | _ -> failwith "unable t make fs" in
  let l = ref [] in
  let t = (ctxt, fs, l) in
  let rec run fcs t =
    match fcs with
    | [] -> mk_ok t
    | fc :: rest -> commit fc t >>= run rest
  in
    match run fcs t with
    | Ok _ -> Printf.printf "Commands successfully commited!\n"
    | Error e -> Printf.printf "Commands aborted because %s\n" e *)


let create (s:specification) ?(p: path option) () : t=
  let p = match p with None -> TempFS.dummy_path | Some p' -> p' in
  let ps = PathSet.singleton p in
  let z = make_zipper (empty_env, s) in
  let ctxt = (p, ps, z) in
  let fs = match TempFS.create p with Ok (fs, p) -> fs | _ -> failwith "unable t make fs" in
  let l = ref [] in
  let t = (ctxt, fs, l) in
    async_print t;
(*     (Printf.printf "Spec: %s\n" (show_specification s));  *)
    t

let get_log (t: t) :log =
  let (_, _, log_ref)  = t in
    !log_ref

let rec update_global_fs (ll: log) (fs: PermFS.t)  : PermFS.t or_fail =
  match ll with
  | [] -> mk_ok fs
  | (Read _) :: tl -> update_global_fs tl fs
  | (Write_file (_ , File (u) , p)) :: tl -> begin
    PermFS.goto p fs >>= PermFS.make_file u >>= update_global_fs tl
  end
  | (Write_directoy (_ , Dir (l), p)) :: tl -> begin
    PermFS.goto p fs >>= PermFS.make_directory l >>= update_global_fs tl
  end
  | (Write_file (_ , _ , _)) :: _ -> mk_err "you tryed to write somethin other than a file with Write_file"
  | (Write_directoy (_ , _ , _)) :: _ -> mk_err "you tryed to write somethin other than a dir with Write_dir"


let merge (ll: log) (p:path) : unit =
  let fs = PermFS.create p in
  let _ : PermFS.t or_fail = fs >>= (update_global_fs ll) in
    ()


(*given the gohead to update the global fs*)
let commit ((p, ps, z), fs, l) : t or_fail =
  merge (!l) p;
  TempFS.create p
  >>= (fun (fs', p') ->
    mk_ok ((p, ps, z), fs', ref [])
  )







