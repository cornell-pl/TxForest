open Core

module Var = Core.String
module PathSet = Core.String.Set
module SSet : sig
  type t = String.Set.t

  val show : t -> string
  val pp : Format.formatter -> t -> unit
end

type txError =
  | TxError
  | OpError of string

type name = string [@@deriving show]

type contents = Dir of string list | File of string

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

type writeable_fetch_rep =
  | WFileRep of string
  | WDirRep of string list
  | WPathRep of name
  | WPairRep of Var.t
  | WCompRep of string list
  | WOptRep of bool
  | WPredRep of bool
  | WNullRep [@@deriving show]

(* log_entry
 * Read (past tense) contents at path
 * Wrote second contents at path where there use to be first contents
 *)
type le =
  | Read of contents * path
  | Write_file of contents * contents * path
  | Write_directory of contents * contents * path

(* local log
 * list of things that I have done and would liek to commit
 * note: we dont have timestamps here since, none of these actions have
 * been commited yet!
 *)
type log = le list

type 'a or_fail = ('a, string) Core.result


val debug : bool ref
val set_debug : unit -> unit
val p : ('a, Stdio.Out_channel.t, unit) Base.format -> 'a
val d : ('a, unit, string, unit) format4 -> 'a

val mk_err : ('a, unit, string, 'b or_fail) format4 -> 'a
val mk_ok : 'a -> 'a or_fail
val ignore_ret : 'a -> 'b -> 'a or_fail
val f_ret : f:('a -> 'b) -> 'a -> 'a or_fail
val info_message : ?id:Async.Writer.Id.t -> string  -> string -> unit
val writable_of_fetch: fetch_rep -> writeable_fetch_rep
val fetch_of_writable: writeable_fetch_rep -> fetch_rep


open Async

val write_marshal : flags:Marshal.extern_flags list -> Writer.t -> 'a -> unit
  
val write_struct : Writer.t -> 'a -> unit
val block : (unit -> 'a Deferred.t) -> 'a