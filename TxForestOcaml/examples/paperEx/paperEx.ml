open Core
open TxForest
open Rawforest
open Utils
open ForestIntf
open Result
open Result.Let_syntax

[%%txforest {|
  hws = directory {
      max is "max" :: file;
      students is [student :: file | student <- matches RE "[a-z]+[0-9]+"];
    }

  grades = [hw :: hws | hw <- matches RE "hw[0-9]+"]

|}]

module Forest = ForestIntf.ForestS
open Forest
open Derived

type hw = int
and student = string
and score = int

open Result
open Let_syntax

(* Helpers *)

let base_dir = "examples/paperEx/grades"

let ignore_after_f (f : 'a -> 'b) = Fn.compose Core.ignore f

let parse_score u =
  int_of_string_opt u
  |> Option.value_exn ~message:"parse_score: File was improperly formatted"

let goto_name_p name z = goto_name name z >>= down

let set_score = Fn.compose store_file string_of_int

let get_score z = fetch_file z >>| parse_score

let goto_hw hw z =
  let hwDir = string_of_int hw |> (^) "hw" in
  goto_name hwDir z >>= down

let goto_student student hw z =
  goto_hw hw z >>= goto "students" >>= goto_name student >>= down


(* TODO: It is very annoying that fetching a comprehension can cause a fatal
error because it uses 'fetch_dir' which is opened with 'TxForestCoreExn' *)
let add_to_comp u z =
  try
    let s = fetch_comp z |> ok_or_failwith in
    store_dir (String.Set.add s u) z
  with _ -> store_dir (String.Set.singleton u) z

let add_and_goto u z = add_to_comp u z >>= goto_name u

let create_student student hw z =
  let hwDir = string_of_int hw |> (^) "hw" in
  add_and_goto hwDir z >>= down
  >>= goto "students"
  >>= add_and_goto student >>= down

(* Get Score *)

let get_score_input ~hw ?student  =
  let get_score_trans hw student z =
    let%bind z' = goto_student student hw z in
    let%map score = get_score z' in
    p "%s's grade for hw%d: %d\n" student hw score
  in
  let student = Option.value_exn ~message:"get_score: Supply a student please" student in
  TxForestCore.loop_txn grades_spec base_dir ~f:(get_score_trans hw student)

(* Set Score *)
let set_score_input ~hw ?student ?score =
  let set_score_trans hw student score z =
    let%bind z' = create_student student hw z in
    let%map z' = set_score score z' in
    p "Set %s's grade for hw%d to %d\n" student hw score
  in
  let student = Option.value_exn ~message:"set_score: Supply a student please" student in
  let score = Option.value_exn ~message:"set_score: Supply a score please" score in
  TxForestCore.loop_txn grades_spec base_dir ~f:(set_score_trans hw student score)

(* PaperEx Opt 3:
   - T1: Renormalize
   - T2: Renormalize
 *)
let renorm featmin featmax goalmin goalmax score =
  d "Renormalizing with Xmin = %d, Xmax = %d, goalmin = %d, goalmax = %d\n"
  featmin featmax goalmin goalmax;
  if featmax = featmin
  then score
  else
    let denom = float_of_int ((score - featmin)*(goalmax - goalmin)) in
    let div = float_of_int (featmax - featmin) in
    denom /. div |> Float.round_nearest |> int_of_float |> (+) goalmin

 let map_scores ~rn =
  Forest.map ~f:(fun z ->
    let%bind z' = down z in
    let%bind score = get_score z' in
    let score_new = rn score in
    d "Score before/after = %d / %d\n" score score_new;
    set_score score_new z' >>= up
  )

let get_min_max =
  fold ~init:(Int.max_value,Int.min_value) ~f:(fun (l,h) z ->
    down z >>= get_score >>| fun i -> (min i l, max i h)
  )

let renormalize_trans hw goalmin ?max z =
  let%bind z' = goto_hw hw z in
  let%bind goalmax =
    match max with
    | None -> goto "max" z' >>= down >>= get_score
    | Some max -> return max
  in
  let%bind z' = goto "students" z' in
  let%bind (featmin, featmax) = get_min_max z' in
  map_scores ~rn:(renorm featmin featmax goalmin goalmax) z'

let renormalize ~hw ?min ?max =
  let min = Option.value_exn ~message:"renormalize: Supply a minimum score please" min in
  TxForestCore.loop_txn grades_spec base_dir ~f:(renormalize_trans hw min ?max)

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs various operations on the 'grades' filestore.

    Operation 0: Print grade of a given student and hw
      Req. args: hw, student
    Operation 1: Change grade of a given student and hw
      Req. args: hw, student, score
    Operation 2: Precise example from paper for renormalizing
      Req. args: hw, min, max
    "
    [%map_open
      let op = flag "op" (required int)
              ~doc:"[0-2] Run this operation"
      and hw = flag "hw" (optional_with_default 1 int)
                  ~doc:"N Homework number (default 1)"
      and student = flag "student" (optional string)
                  ~doc:"N Student Id"
      and score = flag "score" (optional int)
                  ~doc:"N Score"
      and max = flag "max" (optional int)
                  ~doc:"N Max score after normalization"
      and min = flag "min" (optional int)
                  ~doc:"N Minimum score after normalization"
      and debug = flag "debug" (no_arg)
                  ~doc:"Print debug statements"
      in
      if debug then Utils.set_debug ();
      match op with
      | 0 -> get_score_input ~hw ?student
      | 1 -> set_score_input ~hw ?student ?score
      | 2 -> ignore_after_f (renormalize ~hw ?min ?max)
      | _ -> failwithf "%d is not a valid operation" op
    ]
    |> Command.run