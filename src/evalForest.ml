(* TODOS:
 * - Make loop_txn return an 'or_fail'
 * - Get rid of loop_txn_noExn
 * - Combine FS and path into one construct (since you always use them together anyway)
 * - Maybe make a 'next_or' command/function for comprehensions
 * - Check FS implementations
 * - Implement Verify
 * - Implement Check
 *)


open Core
open Result
open Result.Let_syntax
open Filesystem
open Utils

(* Types *)
type fs = Filesystem.fs
type path = Filesystem.path

type fetch_rep =
  | FileRep of string
  | DirRep of SSet.t
  | PathRep of name
  | PairRep of Var.t
  | CompRep of SSet.t
  | OptRep of bool
  | PredRep of bool
  | NullRep  [@@deriving show]

type fetch_result = fetch_rep  [@@deriving show]

type forest_navigation =
  | Down
  | Up
  | Into_Pair
  | Into_Comp
  | Into_Opt
  | Out
  | Next
  | Prev  [@@deriving show]

type forest_update =
  | Store_File of string
  | Store_Dir of SSet.t
  | Create_Path  [@@deriving show]

type forest_command =
  | Nav of forest_navigation
  | Update of forest_update  [@@deriving show]

type specification =
  | Null
  | File
  | Dir
  | PathExp of name fexp * specification
  | DPair of Var.t * specification * specification
  | Comp of specification * Var.t * SSet.t fexp
  | Opt of specification
  | Pred of bool fexp  [@@deriving show]

and 'a fexp = fs -> env -> 'a

and direnv = (path * zipper) Var.Map.t [@opaque]
and compenv = string Var.Map.t [@opaque]
and env = direnv * compenv

and node = env * specification

and zipper =
  { ancestor : zipper option;
    left : node list;
    current : node;
    right: node list;
  }  [@@deriving show]


let make_zipper ?ancestor ?(left=[]) ?(right=[]) current = {ancestor; left; current; right}

let empty_env : env = (Var.Map.empty, Var.Map.empty)

type t = fs * path * PathSet.t * zipper


let print_fetch_result = Fn.compose (Printf.printf "%s \n") show_fetch_result
let debug_print (_,_,_,z) = Printf.printf "%s\n" (show_zipper z)


(* Helper functions *)
let add_to_dirEnv ~key ~data : env -> env = Tuple.T2.map_fst ~f:(Map.set ~key ~data)
let add_to_compEnv ~key ~data : env -> env = Tuple.T2.map_snd ~f:(Map.set ~key ~data)

let is_path = function
  | {current = (_, PathExp _);_} -> true
  | _ -> false

let sync_path ((fs, p, ps, z): t) : t or_fail =
  let%map (fs',p') = sync_path (fs,p) in
  (fs',p',ps,z)

(* Main functions *)

let eval_forest_navigation c t  : t or_fail =
  let%bind (fs, p, ps, z) = sync_path t in
  match (c,z) with
  | Down, {current = (env, PathExp (f, s) ); _ } ->
      let u = f fs env in
      if is_dir (fs,p)
      then
        let%map (fs', p') = gotoChild u (fs, p) in
        let z' = make_zipper ~ancestor:z (env, s) in
        (fs', p',PathSet.add ps p', z')
      else mk_err "Down requires the current filesystem node to be directory. %s is not" p
  | Up, {ancestor = Some(z');_} when is_path z'->
      let%map (fs', p') = pop (fs, p) in
      (fs', p', ps, z')
  | Into_Pair, {current = (env, DPair(x,s1,s2)); _ } ->
      let ctxt = p,make_zipper (env, s1) in
      let envR = add_to_dirEnv ~key:x ~data:ctxt env in
      let z' = make_zipper ~ancestor:z (env, s1) ~right:[(envR, s2)] in
        mk_ok (fs, p, ps, z')
  | Into_Comp, {current = (env, Comp(s, x, f)); _ } ->
      begin
        match f fs env |> Set.to_list with
        | hd :: tl ->
          let z' =
            make_zipper ~ancestor:z
            (add_to_compEnv ~key:x ~data:hd env, s)
            ~right:(List.map tl ~f:(fun u -> (add_to_compEnv ~key:x ~data:u env, s)))
          in
          mk_ok (fs, p, ps, z')
        | [] -> mk_err "Into_Comp requires the comprehension to be non-empty"
      end
  | Into_Opt, {current = (env, Opt s); _ } ->
      let z' = make_zipper ~ancestor:z (env,s) in
      mk_ok (fs, p, ps, z')
  | Out, {ancestor = Some(z')} when not (is_path z') -> mk_ok (fs, p, ps, z')
  | Next, {ancestor; left; current; right=(new_current::right)}->
      let z' = make_zipper ?ancestor ~left:(current::left) new_current ~right in
      mk_ok (fs, p, ps, z')
  | Prev, { ancestor; left=(new_current::left); current; right} ->
      let z' = make_zipper ?ancestor ~left new_current ~right:(current :: right) in
      mk_ok (fs, p, ps, z')

  (* Nicer Error handling *)
  | Down, _ -> mk_err "Down can only be used at a path node"
  | Up, _ -> mk_err "Up can only be used when a path node is the ancestor"
  | Into_Pair, _ -> mk_err "Into_Pair can only be used at a pair node"
  | Into_Comp, _ -> mk_err "Into_Comp can only be used at a comprehension node"
  | Into_Opt, _ -> mk_err "Into_Opt can only be used at an option node"
  | Out, _ -> mk_err "Out can only be used when there is a non-path node ancestor"
  | Next, _ -> mk_err "Next can only be used when there is a next node"
  | Prev, _ -> mk_err "Prev can only be used when there is a previous node"

let eval_forest_update c t : t or_fail =
  let%bind (fs, p, ps, z) = sync_path t in
  match c,z with
  | Store_File u, {current = (_, File); _ } ->
      let%map (fs', p') = make_file u (fs, p) in
      (fs', p', ps, z)
  | Store_Dir s, {current = (_, Dir); _ } ->
      let%map (fs', p') = make_directory (Set.to_list s) (fs, p) in
      (fs', p', ps, z)
  | Create_Path, {current = (env, PathExp (f,s)); _ } ->
      let%map (fs', p') = add_to_directory (f fs env) (fs, p)  in
      (fs', p', ps, z)

  (* Nicer Error handling *)
  | Store_File _, _ -> mk_err "Store_File can only be used at a file node"
  | Store_Dir _, _ -> mk_err "Store_Dir can only be used at a directory node"
  | Create_Path, _ -> mk_err "Create_Path can only be used at a path node"

let eval_forest_command = function
  | Nav fn -> eval_forest_navigation fn
  | Update fu -> eval_forest_update fu

(* Exp Functions *)

let fetch t : fetch_result or_fail =
  let%bind (fs, p, ps, z) = sync_path t in
  match z with
  | {current = (_, File); _ } ->
    begin
      match Filesystem.fetch (fs, p) with
      | File u -> FileRep u |> mk_ok
      | _ -> mk_err "File expected, but node at path %s is not a file" p
    end
  | {current = (_, Dir); _ } ->
    begin
      match Filesystem.fetch (fs, p) with
      | Dir l -> String.Set.of_list l |> DirRep |> mk_ok
      | _ -> mk_err "Directory expected, but node at path %s is not a directory" p
    end
  | {current = (env, PathExp (f,_)); _ } -> PathRep (f fs env) |> mk_ok
  | {current = (_, DPair (x,_,_)); _ } -> PairRep x |> mk_ok
  | {current = (env, Comp (_,_,f)); _ } -> CompRep (f fs env) |> mk_ok
  | {current = (_, Opt _); _ } -> OptRep (exists (fs,p)) |> mk_ok
  | {current = (env, Pred f); _ } ->  PredRep (f fs env) |> mk_ok
  | {current = (_, Null); _ } -> NullRep |> mk_ok

let print t =
    match fetch t with
    | Ok fr -> print_fetch_result fr
    | Error u -> Printf.printf "print: %s\n" u

let print_ret = f_ret ~f:print

let verify t = failwith "TODO: Implement"

(* TODO:
 *  If we keep 'Check' for users, then we should consider advantages/disadvantages
 *  of just trying to run it and seeing if we get an error
 *)
let check c t = failwith "TODO: Implement"

let loop_txn_noExn ~(f:t->'a) (s:specification) (p:string) () =
  let f_to_z (fs: fs) : 'a =
    match Filesystem.goto p (fs, dummy_path) with
    | Error _ -> failwithf "loop_txn: path %s does not exist" p ()
    | Ok (fs', p') -> begin
      (fs', p', PathSet.singleton p', make_zipper (empty_env, s))
      |> f
    end
  in
  Filesystem.loop_txn ~f:f_to_z ()

let loop_txn ~f = loop_txn_noExn ~f:(Fn.compose Result.ok_or_failwith f)

let run_txn ~(f:t->'a or_fail) (s:specification) (p:string) () =
  let f_to_z (fs: fs) : 'a or_fail =
    match Filesystem.goto p (fs, dummy_path) with
    | Error _ -> failwithf "run_txn: path %s does not exist" p ()
    | Ok (fs', p') ->
      (fs', p', PathSet.singleton p', make_zipper (empty_env, s))
      |> f
  in
  run_txn ~f:f_to_z ()
