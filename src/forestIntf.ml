(* TODO:
 * - Add Check and Verify and decide what needs to be exposed to make that useful
 * - make a transactional wrapper
 *)



(* Notes on Surface to Core translation:
(* Surface to Core translation:
 * - Var x -> 'x_spec'
 * - File -> File
 * - Option ast -> Opt [[ast]]
 * - Directory asts -> <x1 : s1, <... <xn-1: sn-1, sn>
 * -- We should perhaps have a 'dummy' spec at the end instead, and that way everything
      can more correctly be referred to by name. Could also be done with metadata.
 * -- 'goto x' should work
 * -- 'next' should go to next dir entry (i.e. down, then next) and symmetric
      for prev
 * -- Some fetch that gets name of var?
 * -- Up needs to go to above directory (or out?)
 * -- No 'store' available
 * - Comprehension (typ,ast,qual) ->
      < SOME_UNIQ_ID : Dir, Comp ([[Ast]], Var(s?)from qual, fun from quals) >
 * -- Down goes to first Comp child (if Comp or Dir)
 * -- Skip 'Goto x'
 * -- Temporarily a fetch that gives both full 'dir' and comp filter
 * -- Only allow 'store_dir' and then only at comprehensions that use FS
 * - PathExp (ocaml,ast) -> PathExp ([[ocaml]],[[ast]])
 * - Predicate (ast,ocaml) -> <this : [[ast]], Pred [[ocaml]]>
 * -- Fetch gives bool
 * -- 'Next' to a pred fails
 * -- Up goes 2 up, etc?
 *)
*)

open Core
open EvalForest
open Utils

module Var = Utils.Var

type name = Utils.name

type fetch_rep = EvalForest.fetch_rep =
  | FileRep of string
  | DirRep of SSet.t
  | PathRep of name
  | PairRep of Var.t
  | CompRep of SSet.t
  | OptRep of bool
  | PredRep of bool
  | NullRep [@@deriving show]

type specification = EvalForest.specification =
  | Null
  | File
  | Dir
  | PathExp of name fexp * specification
  | DPair of Var.t * specification * specification
  | Comp of specification * Var.t * SSet.t fexp
  | Opt of specification
  | Pred of bool fexp


type t = EvalForest.t

(* Additional *)

let loop_txn_noExn = loop_txn_noExn
let loop_txn = loop_txn
let run_txn = run_txn


let print_fetch_result = EvalForest.print_fetch_result
let print = EvalForest.print
let print_ret = EvalForest.print_ret
let debug_print = EvalForest.debug_print

(* TODO: Figure out how to properly expose this module type
module type ForestRaw = sig
  (** Basically as seen in TxForest paper *)

  type 'a out

  (* Standard Navigations *)
  val down : t -> t out
  val up : t -> t out

  val into_opt : t -> t out
  val into_pair : t -> t out
  val into_comp : t -> t out
  val out : t -> t out

  val next : t -> t out
  val prev : t -> t out

  (* Updates *)
  val store_file : string ->  t -> t out
  val store_dir : name list -> t -> t out
  val create_path : t -> t out

  (* Fetches *)
  val fetch : t -> fetch_result out

  val fetch_file : t -> string out

  val fetch_dir : t -> string list out

  val fetch_path : t -> name out

  val fetch_pair : t -> Var.t out

  val fetch_comp : t -> name list out

  val fetch_opt : t -> bool out

  val fetch_pred: t -> bool out


  (* Other *)

  val verify : t -> bool * bool
  val check : forest_command -> t -> bool
  module type Derived = sig
    val goto_pos : int -> t -> t out
    val goto_name : name -> t -> t out
  end
  include Derived
end
*)

module TxForestCore = struct
  type 'a out = 'a or_fail

  (* Standard Navigations *)
  let down = eval_forest_command (Nav Down)
  let up = eval_forest_command (Nav Up)

  let into_opt =  eval_forest_command (Nav Into_Opt)
  let into_pair = eval_forest_command (Nav Into_Pair)
  let into_comp = eval_forest_command (Nav Into_Comp)
  let out = eval_forest_command (Nav Out)

  let next = eval_forest_command (Nav Next)
  let prev = eval_forest_command (Nav Prev)

  (* Updates *)
  let store_file u = eval_forest_command (Update (Store_File u))
  let store_dir l = eval_forest_command (Update (Store_Dir l))
  let create_path = eval_forest_command (Update Create_Path)

  (* Fetches *)
  let fetch = fetch

  open Core.Result
  let fetch_file t = fetch t >>= function
  | FileRep u -> mk_ok u
  | _ -> mk_err  "Fetch_file can only be used at a file node"

  let fetch_dir t = fetch t >>= function
  | DirRep l -> mk_ok l
  | _ -> mk_err  "Fetch_dir can only be used at a dir node"

  let fetch_path t = fetch t >>= function
  | PathRep u -> mk_ok u
  | _ -> mk_err  "Fetch_path can only be used at a path node"

  let fetch_pair t = fetch t >>= function
  | PairRep x -> mk_ok x
  | _ -> mk_err  "Fetch_pair can only be used at a pair node"

  let fetch_comp t = fetch t >>= function
  | CompRep l -> mk_ok l
  | _ -> mk_err  "Fetch_comp can only be used at a comp node"

  let fetch_opt t = fetch t >>= function
  | OptRep b -> mk_ok b
  | _ -> mk_err  "Fetch_opt can only be used at a opt node"

  let fetch_pred t = fetch t >>= function
  | PredRep b -> mk_ok b
  | _ -> mk_err  "Fetch_pred can only be used at a pred node"


  let is_null t = fetch t >>| function
  | NullRep -> true
  | _ -> false

  (* Other *)

  let verify = verify
  let check = check

  module Derived  = struct
    open Result.Let_syntax
    (* Derived Navigations *)
    let goto_comp_pos pos t =
      let rec keep_going pos t =
        if pos > 0
        then next t >>= keep_going (pos-1)
        else mk_ok t
      in
        into_comp t >>= keep_going pos

    let goto_comp_name u t =
      let%bind l = fetch_comp t >>| Set.to_list in
      let o = Core.List.findi ~f:(fun _ -> String.equal u) l in
      let%bind (i,_) = Result.of_option o ~error:(Printf.sprintf "%s was not in comprehension" u) in
      goto_comp_pos i t


    let goto_dir_pos pos t =
      let rec keep_going pos t =
        if pos > 0
        then next t >>= into_pair >>= keep_going (pos-1)
        else mk_ok t
      in
        into_pair t >>= keep_going pos

    let rec goto_dir_name u t =
      match%bind fetch t with
      | PairRep x when x = u -> into_pair t
      | PairRep _ -> into_pair t >>= next >>= goto_dir_name u
      | NullRep -> mk_err "%s is not in this directory specification" u
      | _ -> mk_err "Goto_dir_name can only be used in a directory"

  end
  include Derived
end


module TxForestCoreExn = struct
  type 'a out = 'a

  open Core
  open TxForestCore
  (* Standard Navigations *)
  let down = Fn.compose Result.ok_or_failwith down
  let up = Fn.compose Result.ok_or_failwith up
  let into_opt = Fn.compose Result.ok_or_failwith into_opt
  let into_pair = Fn.compose Result.ok_or_failwith into_pair
  let into_comp = Fn.compose Result.ok_or_failwith into_comp
  let out = Fn.compose Result.ok_or_failwith out
  let next = Fn.compose Result.ok_or_failwith next
  let prev = Fn.compose Result.ok_or_failwith prev

  (* Updates *)
  let store_file u = Fn.compose Result.ok_or_failwith (store_file u)
  let store_dir l = Fn.compose Result.ok_or_failwith (store_dir l)
  let create_path = Fn.compose Result.ok_or_failwith create_path

  (* Fetches *)
  let fetch = Fn.compose Result.ok_or_failwith fetch

  let fetch_file = Fn.compose Result.ok_or_failwith fetch_file
  let fetch_dir = Fn.compose Result.ok_or_failwith fetch_dir
  let fetch_path = Fn.compose Result.ok_or_failwith fetch_path
  let fetch_pair = Fn.compose Result.ok_or_failwith fetch_pair
  let fetch_comp = Fn.compose Result.ok_or_failwith fetch_comp
  let fetch_opt = Fn.compose Result.ok_or_failwith fetch_opt
  let fetch_pred = Fn.compose Result.ok_or_failwith fetch_pred

  let is_null = Fn.compose Result.ok_or_failwith is_null

  (* Other *)
  let verify = verify
  let check = check

  module Derived  = struct
    open Result.Let_syntax
    (* Derived Navigations *)
    let goto_comp_pos pos t =
      let rec keep_going pos t =
        if pos > 0
        then next t |> keep_going (pos-1)
        else t
      in
        into_comp t |> keep_going pos

    let goto_comp_name u t =
      let l = fetch_comp t |> Set.to_list in
      Core.List.findi ~f:(fun _ -> String.equal u) l
      |> Option.value_exn ~message:(Printf.sprintf "%s was not in comprehension" u)
      |> fun (i,_) -> goto_comp_pos i t

    let goto_dir_pos pos t =
      let rec keep_going pos t =
        if pos > 0
        then next t |> into_pair |> keep_going (pos-1)
        else t
      in
        into_pair t |> keep_going pos

    let rec goto_dir_name u t =
      match fetch t with
      | PairRep x when x = u -> into_pair t
      | PairRep _ -> into_pair t |> next |> goto_dir_name u
      | NullRep -> failwithf "%s is not in this directory specification" u ()
      | _ -> failwith "Goto_dir_name can only be used in a directory"

  end
  include Derived
end

(* TODO: Implement a version where we walk through paths using is_path *)
module TxForestRawWalkThrough = struct
end


module TxForestS = struct
  type 'a out = 'a or_fail

  open Result.Let_syntax

  type fetch_result =
  | SFileRep of string
  | SDirRep of SSet.t
  | SPathRep of name
  | SCompRep of SSet.t
  | SOptRep of bool
  | SPredRep of bool [@@deriving show]

  (* Helpers *)
  type situation =
    | SDirComp
    | SPred
    | SDir
    | SPath
    | SOpt
    | SComp
    | SFile
    | SDirS
    | SPredS
    | SNull [@@deriving show]

  (* TODO: Do this in evalForest and avoid computation *)
  let sitch t =
    let open TxForestCore in
    let%map s =
    match%map fetch t with
    | PairRep "dir'" -> SDirComp
    | PairRep "this" -> SPred
    | PairRep _ -> SDir
    | PathRep _ -> SPath
    | OptRep _ -> SOpt
    | CompRep _ -> SComp
    | FileRep _ -> SFile
    | DirRep _ -> SDirS
    | PredRep _ -> SPredS
    | NullRep -> SNull
    in
    d "Got sitch %s from:" (show_situation s);
    s

  let do_err ~fok ~ferr t =
    if is_ok t then t >>= fok else ferr t

  let out_or_up t =
    let open TxForestCore in
    up t |> do_err  ~ferr:(fun _ -> out t) ~fok:mk_ok

  (* Standard Navigations *)
  let down t =
    let open TxForestCore in
    match%bind sitch t with
    | SDirComp -> into_pair t >>= next >>= into_comp
    | SPred
    | SDir -> into_pair t
    | SPath -> TxForestCore.down t
    | SOpt -> into_opt t
    | SComp -> into_comp t
    | _ -> mk_err "Down is illegal at File, Dir, or Pred nodes"

  (* TODO: Use check or some other aux function when implemented *)
  let up t =
    let rec walk_up_dir t =
      TxForestCore.out t
      |> do_err ~ferr:(ignore_ret t)
        ~fok:(fun t' ->
          match%bind sitch t' with
          | SDir
          | SDirComp -> walk_up_dir t'
          | _ -> down t
        )
    in
    let open TxForestCore in
    let%bind t' = out_or_up t in
    match%bind sitch t' with
    | SDir -> walk_up_dir t'
    | SComp ->
      let t'' = out t' in
      do_err t'' ~ferr:(ignore_ret t')
        ~fok:(fun t' -> match%bind sitch t' with | SDirComp -> mk_ok t' | _ -> t'')
    | SDirComp
    | SPred
    | SPath
    | SOpt -> mk_ok t'
    | _ -> mk_err "It should be impossible to still be in File, Dir, or Pred nodes"

  let next t =
    let%bind t' = out_or_up t in
    match%bind sitch t' with
    | SDir ->
        let%bind t'' = TxForestCore.next t >>= down in
        let%bind b = TxForestCore.is_null t in
        if b then mk_err "Walked next to a Null node" else mk_ok t''
    | SPred -> mk_err "Predicates do not have a next node"
    | _ -> TxForestCore.next t


  let prev t =
    let%bind t' = out_or_up t in
    match%bind sitch t' with
    | SDir -> TxForestCore.prev t'
    | _ -> TxForestCore.prev t


  (* Updates *)
  let store_file u = eval_forest_command (Update (Store_File u))
  let store_dir s t =
    let open TxForestCore in
    match%bind sitch t with
    | SDirComp ->
        let%bind t' = into_pair t in
        let dir = fetch_dir t' in
        do_err dir ~ferr:(fun _ -> TxForestCore.store_dir s t' >>= out)
        ~fok:(fun dir ->
          let%bind t' = next t' in
          let%bind comp = fetch_comp t' in
          let s' =
            Set.filter dir ~f:(fun u -> not (Set.exists ~f:(String.equal u) comp))
          in
          prev t' >>= store_dir (Set.union s s') >>= out
        )
    | _ -> mk_err "Store_dir is only allowed at a directory-based comprehension"

  let create_path = eval_forest_command (Update Create_Path)

  (* Fetches *)
  let convert = function
    | FileRep u -> SFileRep u
    | DirRep s -> SDirRep s
    | PathRep u -> SPathRep u
    | CompRep s -> SCompRep s
    | OptRep b -> SOptRep b
    | PredRep b -> SPredRep b
    | _ -> failwith "Convert: Something deeply unexpected happened..."

  let fetch t =
    let rec get_names acc t =
      let open TxForestCore in
      let%bind b = is_null t in
      if b then mk_ok acc
      else
        let%bind u = fetch_pair t in
        d "%s" u;
        do_err TxForestCore.(into_pair t >>= next) ~ferr:(ignore_ret acc)
          ~fok:(get_names (String.Set.add acc u))
    in
    match%bind sitch t with
    | SDirComp
    | SPred ->
        TxForestCore.into_pair t >>= TxForestCore.next
        >>= TxForestCore.fetch >>| convert
    | SDir -> get_names String.Set.empty t >>| fun s -> SDirRep s
    | SPath
    | SOpt
    | SComp
    | SFile -> TxForestCore.fetch t >>| convert
    | SDirS
    | SPredS
    | SNull -> mk_err "Fetch: It should be impossible to get to a Pred, Null, or Dir node"

  let print_res f = Printf.printf "%s \n"  (show_fetch_result f)
  let fetch_n_print t = fetch t |> Core.Result.iter ~f:print_res

  let fetch_file t = fetch t >>= function
  | SFileRep u -> mk_ok u
  | _ -> mk_err  "Fetch_file can only be used at a file node"

  let fetch_dir t = fetch t >>= function
  | SDirRep l -> mk_ok l
  | _ -> mk_err  "Fetch_dir can only be used at a directory node"

  let fetch_path t = fetch t >>= function
  | SPathRep u -> mk_ok u
  | _ -> mk_err  "Fetch_path can only be used at a path node"

  let fetch_comp t = fetch t >>= function
  | SCompRep l -> mk_ok l
  | _ -> mk_err  "Fetch_comp can only be used at a comprehension node"

  let fetch_opt t = fetch t >>= function
  | SOptRep b -> mk_ok b
  | _ -> mk_err  "Fetch_opt can only be used at a opt node"

  let fetch_pred t = fetch t >>= function
  | SPredRep b -> mk_ok b
  | _ -> mk_err  "Fetch_pred can only be used at a predicate node"

  (* Other *)

  let verify = verify
  let check = check

  module Derived  = struct
    (* Derived Navigations *)
    let goto_pos pos t =
      let rec keep_going pos t =
        if pos > 0
        then next t >>= keep_going (pos-1)
        else mk_ok t
      in
        down t >>= keep_going pos

    let goto_comp_name name t =
      let%bind l = fetch_comp t >>| Set.to_list in
      let o = Core.List.findi ~f:(fun _ -> String.equal name) l in
      let%bind (i,_) = Result.of_option o ~error:(Printf.sprintf "%s was not in comprehension" name) in
      goto_pos i t

    let rec goto u t =
      let open TxForestCore in
      match%bind fetch t with
      | PairRep x when x = u -> into_pair t
      | PairRep _ -> into_pair t >>= next >>= goto u
      | NullRep -> mk_err "%s is not in this directory specification" u
      | _ -> mk_err "Goto can only be used in a directory"

    let goto_name name t =
      match%bind fetch t with
      | SDirRep _ -> goto name t
      | SCompRep _ -> goto_comp_name name t
      | _ -> mk_err "Goto_name is only defined on directories and comprehensions"



    let fold ~init ~f t =
      let rec fold ~init ~f t =
        d "Folding";
        let%bind init = f init t in
        do_err (next t) ~ferr:(ignore_ret init) ~fok:(fold ~init ~f)
      in
      match%bind fetch t with
      | SDirRep _ | SCompRep _ -> down t >>= fold ~init ~f
      | _ -> mk_err "Can only fold in comprehensions or directories"

    (* TODO: Technically, there's nothing users can do when traversing the
    zipper that will change the underlying tree, so just throwing away the
    result of the function instead of using it would be fine and perhaps less
    error prone... but it somehow feels wrong? *)
    let map ~(f : t -> t or_fail) (t : t) : t or_fail=
      let rec map ~f t =
        d "Mapping";
        let%bind t = f t in
        do_err (next t) ~ferr:(fun _ -> up t) ~fok:(map ~f)
      in
      match%bind fetch t with
      | SDirRep _ | SCompRep _ -> down t >>= map ~f
      | _ -> mk_err "Can only fold in comprehensions or directories"

  end
  include Derived

  let print_dir z =
    match fetch_dir z with
    | Ok s -> Printf.printf "Directory Entries:\n"; Set.iter ~f:print_endline s
    | Error u -> Printf.printf "print_dir: %s\n" u

  let print_comp z =
    match fetch_comp z with
    | Ok s -> Printf.printf "Comprehension Entries:\n"; Set.iter ~f:print_endline s
    | Error u -> Printf.printf "print_comp: %s\n" u
end

(* PPX Helpers *)
type varType = ZipVar | StrVar

let empty_ps = PathSet.empty

let fetch_dir = TxForestCoreExn.fetch_dir

let get_var map var =
  let open Core in
  Map.find map var
  |> Option.value_exn ~message:(Printf.sprintf "'%s' is expected, but unbound" var)

let regexp_match_from_string reg str =
  try
    let _ = Str.search_forward (Str.regexp reg) str 0 in
    true
  with Not_found | Not_found_s _ -> false [@@warning "-3"]

let glob_match_from_string reg str =
  try
    let _ = Re.exec (Re.compile (Re.Glob.glob reg)) str in
    true
  with Not_found | Not_found_s _ -> false [@@warning "-3"]
