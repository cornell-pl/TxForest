
open Utils

type t = unit

val commit : log -> t -> t or_fail

val finish_commit:  t -> t or_fail
