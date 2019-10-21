open Core
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
  | Store_File of string
  | Store_Dir of SSet.t
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

and 'a fexp = fs -> env -> 'a

and direnv = (path * zipper) Var.Map.t
and compenv = string Var.Map.t
and env = direnv * compenv

and node = env * specification

and zipper =
  { ancestor : zipper option;
    left : node list;
    current : node;
    right: node list;
  }

type t = fs * path * PathSet.t * zipper

val print_fetch_result : fetch_result -> unit

(* [print t] fetches the current node and prints the result *)
val print : t -> unit

(* [print_ret t] acts as print, but can be threaded through in a monadic computation *)
val print_ret : t -> t or_fail


(* [debug_print t] prints the entire zipper structure *)
val debug_print : t -> unit

(*[eval_forest_navigation fn t] evaluates the forest navigation command, a command which
 * only moves around in the zipper, using the state and context
 * in t -- see formal semantics*)
val eval_forest_navigation: forest_navigation -> t -> t or_fail

(*[eval_forest_update fu t] evaluates the forest forest update, a command which updates the
 * underlying file store, using the state and context
 * in t -- see formal semantics*)
val eval_forest_update: forest_update -> t -> t or_fail

(*[eval_forest_command fc t] evaluates forest command fc, which is either a forest navigation
 * or forest update using the state and context
 * in t -- see formal semantics*)
val eval_forest_command: forest_command ->  t -> t or_fail

val fetch : t -> fetch_result or_fail
val verify : t -> bool * bool
val check : forest_command -> t -> bool

(*[loop_txn f s p ()] runs the "transaction" f on a state and context where the
 * zipper in the state has specification s, and the path in the context is p,
 * as well as the file system in state is at path p
 * [loop_txn] will throw an exception and not commit if the result is an Error
 * [loop_txn_noExn] will always attempt to commit if 'f' returns
 *)
val loop_txn_noExn : f:( t -> 'a) -> specification -> string -> unit -> 'a
val loop_txn : f:( t -> 'a or_fail) -> specification -> string -> unit -> 'a

val run_txn : f:( t -> 'a or_fail) -> specification -> string -> unit -> ('a,txError) Core.result




