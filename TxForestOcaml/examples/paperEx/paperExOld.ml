open Core
open TxForest
open ForestIntf
open Result
open Result.Let_syntax

[%%txforest {|

  students = directory {
    total is "total" :: file option;
    problems is [problem :: file | problem <- matches RE "[0-9]+"];
  }

  hws = directory {
      rubric is "rubric" :: file;
      students is [student :: students | student <- matches GL "*", $not (String.equal student "rubric")$];
    }

  grades = directory {
      hws is [hw :: hws | hw <- matches RE "hw[0-9]+"];
      queue is "gradeQueue" :: file;
    }

|}]

module Forest = ForestIntf.ForestS
open Forest
open Derived

(* Example directory from main repo
let baseDir = "forest/examples/paperEx/grades"

let get_hwDir hw = string_of_int hw |> (^) "hw" |> (Filename.concat baseDir)
let get_studentDir hw student = Filename.concat (get_hwDir hw) student
let get_problemFile hw student problem =
  string_of_int problem |> Filename.concat (get_studentDir hw student)
*)

type hw = int
and student = string
and problem = int
and score = int
and grader = string
and toGrade = hw * student * problem
and queue =
  { assigned : (grader * toGrade) list;
    unassigned : toGrade list;
   } [@@deriving yojson, eq]


open Result
open Let_syntax

(* Helpers *)

let ignore_after_f (f : 'a -> 'b) = Fn.compose Core.ignore f

let parse_score u =
  int_of_string_opt u
  |> Option.value_exn ~message:"parse_score: File was improperly formatted"

let parse_rubric file =
  String.split_lines file
  |> List.map ~f:(fun u ->
      String.split ~on:':' u
      |> function
        | k :: v :: [] -> (int_of_string k,int_of_string v)
        | _ -> failwith "parse_rubric: File was improperly formatted"
    )

let deparse_rubric lines =
  List.map ~f:(Tuple2.uncurry (sprintf "%d:%d")) lines
  |> String.concat ~sep:"\n"

let goto_name_p name z = goto_name name z >>= down

let set_score = Fn.compose store_file string_of_int

let get_score z = fetch_file z >>| parse_score

let student_get_sum z =
  let%bind z' = goto_name_p "total" z in
  let%bind b = fetch_opt z' in
  if b
  then down z' >>= get_score
  else goto "problems" z >>= fold ~init:0 ~f:(fun acc z -> down z >>= get_score >>| (+) acc)

let goto_hw hw z =
  let hwDir = string_of_int hw |> (^) "hw" in
  goto "hws" z >>= goto_name hwDir >>= down

let goto_student student hw z =
  goto_hw hw z >>= goto "students" >>= goto_name student >>= down

let goto_problem problem student hw z =
  goto_student student hw z >>= goto "problems" >>= goto_name problem >>= down

(* PaperEx Opt 1 (no concurrency):
   - get_score
 *)

let get_score_full hw student problem z =
  goto "hws" z
  >>= goto_name_p (sprintf "hw%d" hw)
  >>= goto "students"
  >>= goto_name_p student
  >>= goto_name_p (string_of_int problem)
  >>= fetch_file
  >>| parse_score

let get_print_score ~hw ?student ?problem =
  let student = Option.value_exn ~message:"get_print_score: Supply a student please" student in
  let problem = Option.value_exn ~message:"get_print_score: Supply a problem please" problem in
  TxForestCore.loop_txn grades_spec "/grades" ~f:(fun z ->
    get_score_full hw student problem z >>|  p "Score was %d\n"
  )

(* PaperEx Opt 2:
   - T1: Update rubric and grades
   - T2: Check rubric and grade single problem
 *)

(* Expects to be in HW *)
let update_rubric problem max_score z : ForestIntf.t or_fail =
  let%bind z' = goto_name_p "rubric" z in
  let%bind rubric = fetch_file z' >>| parse_rubric in
  let u =
    List.Assoc.add ~equal:Int.equal rubric problem max_score
    |> deparse_rubric
  in
  store_file u z'
  >>= up >>= up


(* Expects to be in HW *)
let assert_rubric problem max_score z =
  let%bind z' = goto_name_p "rubric" z in
  let%bind rubric = fetch_file z' >>| parse_rubric in
  List.Assoc.find ~equal:Int.equal rubric problem
  |> Option.value_exn ~message:(sprintf "assert_rubric: Problem %d was not in rubric" problem)
  |> fun i -> if not (Int.equal max_score i)
      then mk_err "Assert failure: Max score was %d, but %d was expected" i max_score
      else mk_ok ()


let update_rubric_and_grades_trans hw problem max_score ~f z =
  let%bind z' = goto_hw hw z in
  let%bind z' = update_rubric problem max_score z' in
  let problem = string_of_int problem in
  goto "students" z'
  >>= Forest.map ~f:(fun z ->
      let%bind z' = down z >>= goto "problems" >>= goto_name problem >>= create_path >>= down in
      let score = get_score z' in
      if Result.is_error score
      then set_score (-1) z'
      else score >>| f >>= fun i -> set_score i z'
      >>= up >>= up >>= up >>= up
    )



let update_rubric_and_grades ~hw ?problem ?max_score =
  let problem = Option.value_exn ~message:"update_rubric_and_grades: Supply a problem please" problem in
  let max_score = Option.value_exn ~message:"update_rubric_and_grades: Supply a max-score please" max_score in
  TxForestCore.loop_txn grades_spec "/grades" ~f:(update_rubric_and_grades_trans hw problem max_score ~f:succ)


let check_rubric_and_grade_trans hw student problem max_score score z =
  let%bind z' = goto_hw hw z in
  let%bind _ = assert_rubric problem max_score z' in
  goto_problem (string_of_int problem) student hw z
  >>= set_score score


let check_rubric_and_grade ~hw ?student ?problem ?max_score ?score =
  let student = Option.value_exn ~message:"check_rubric_and_grade: Supply a student please" student in
  let problem = Option.value_exn ~message:"check_rubric_and_grade: Supply a problem please" problem in
  let max_score = Option.value_exn ~message:"check_rubric_and_grade: Supply a max-score please" max_score in
  let score = Option.value_exn ~message:"check_rubric_and_grade: Supply a score please" score in
  TxForestCore.loop_txn grades_spec "/grades" ~f:(check_rubric_and_grade_trans hw student problem max_score score)

(* PaperEx Opt 3:
   - T1: Renormalize
   - T2: Renormalize
 *)
let set_student_total score z =
  let open ForestCore in
  goto_name_p "total" z >>= into_opt >>= set_score score
  >>= out >>= up >>= out

let hw_map_totals ~f =
  Forest.map ~f:(fun z ->
    let%bind z' = down z in
    let%bind score = student_get_sum z' in
    let score_new = f score in
    d "Score before/after = %d / %d\n" score score_new;
    set_student_total score_new z' >>= up
  )

let renorm featmin featmax goalmin goalmax score =
  d "Renormalizing with Xmin = %d, Xmax = %d, goalmin = %d, goalmax = %d\n"
  featmin featmax goalmin goalmax;
  if featmax = featmin
  then score
  else
    let denom = float_of_int ((score - featmin)*(goalmax - goalmin)) in
    let div = float_of_int (featmax - featmin) in
    denom /. div |> Float.round_nearest |> int_of_float |> (+) goalmin

let tup_min_max (min1,max1) (min2,max2) = (min min1 min2, max max1 max2)

(* TODO: You want to get the sum here instead if it exists *)
let hw_get_min_max =
  fold ~init:(Int.max_value,Int.min_value) ~f:(fun (l,h) z ->
    down z >>= student_get_sum >>| fun i -> (min i l, max i h)
  )

let renormalize_trans hw goalmin goalmax z =
  let%bind z' = goto_hw hw z >>= goto "students" in
  let%bind (featmin, featmax) = hw_get_min_max z' in
  hw_map_totals ~f:(renorm featmin featmax goalmin goalmax) z'

let renormalize ~hw ?min ?max =
  let min = Option.value_exn ~message:"renormalize: Supply a minimum score please" min in
  let max = Option.value_exn ~message:"renormalize: Supply a maximum score please" max in
  TxForestCore.loop_txn grades_spec "/grades" ~f:(renormalize_trans hw min max)



(* Normal stuff that isn't as exactly like the paper *)

let student_get_avg z =
  let%bind sum = student_get_sum z in
  let%map amt = goto "problems" z >>= fetch_comp >>| Set.length in
  (float_of_int sum) /. (float_of_int amt)

let hw_get_sum = fold ~init:0 ~f:(fun acc z -> down z >>= student_get_sum >>| (+) acc)

let hw_get_avg z =
  let%bind amt = fetch_comp z >>| Set.length in
  let%map sum = hw_get_sum z in
  (float_of_int sum) /. (float_of_int amt)

(* TODO: It is very annoying that fetching a comprehension can cause a fatal
error because it uses 'fetch_dir' which is opened with 'TxForestCoreExn' *)
let add_to_comp u z =
  try
    let s = fetch_comp z |> ok_or_failwith in
    store_dir (String.Set.add s u) z
  with _ -> store_dir (String.Set.singleton u) z

let add_and_goto u z = add_to_comp u z >>= goto_name u

let create_problem problem student hw z =
  let hwDir = string_of_int hw |> (^) "hw" in
  goto "hws" z
  >>= add_and_goto hwDir >>= down
  >>= goto "students"
  >>= add_and_goto student >>= down
  >>= goto "problems"
  >>= add_and_goto problem >>= down


let get_students z =
  goto "hws" z >>=
  fold ~init:String.Set.empty ~f:(fun acc z ->
    match%map down z >>= fetch with
    | SCompRep names -> String.Set.union acc names
    | _ -> acc
  )

let mk_grades hw students problems =
  List.cartesian_product students problems
  |> List.map ~f:(fun (s,p) -> (hw,s,p))

let add_to_grade_queue ~hw ?student ~problem z =
  (* TODO: Make idempotent. I.e. if something is already on queue,
    it should not be added again2.
  *)
  let%bind students =
    match student with
    | Some id -> mk_ok [id]
    | None -> get_students z >>| Set.to_list
  in
  let problems =
    if problem >= 0
    then [problem]
    else List.init (-problem) ~f:succ
  in
  let%bind z' = goto "queue" z >>= create_path >>= down in
  let%bind queue =
    fetch_file z' >>= fun v ->
    if String.equal "" v then mk_ok {assigned = []; unassigned = []}
    else Yojson.Safe.from_string v |> queue_of_yojson
  in
  let unassigned =
    mk_grades hw students problems
    |> List.filter ~f:(fun toG -> not
        (List.exists ~f:(equal_toGrade toG) queue.unassigned ||
        List.exists ~f:(fun (_,toG2) -> equal_toGrade toG toG2) queue.assigned)
    ) |> (@) queue.unassigned
  in
  let u = queue_to_yojson {queue with unassigned} |> Yojson.Safe.to_string in
  store_file u z'

(*TODO: Use max_score and rubric in some way *)
let main ~(op : int) ~(hw : hw) ~(debug : bool) ?(student : student option)
    ?(problem : problem option) ?(score : score option)
    (z : ForestIntf.t) : ForestIntf.t or_fail =
  if debug then set_debug ();
  match op with
  | 0 ->
    let%bind z' = goto_hw hw z >>= goto "students" in
    let%map avg = hw_get_avg z' in
    p "Hw%d Average: %g\n" hw avg;
    z'
  | 1 ->
    let%bind z' = goto_hw hw z >>= goto "students" in
    let%map sum = hw_get_sum z' in
    p "Hw%d Sum: %d\n" hw sum;
    z'
  | 2 ->
    Option.value_exn student ~message:"Student id is required for operation 2"
    |> fun student ->
      let%bind z' = goto_student student hw z in
      let%map avg = student_get_avg z' in
      p "%s's average for hw%d: %g\n" student hw avg;
      z'

  | 3 ->
    Option.value_exn student ~message:"Student id is required for operation 3"
    |> fun student ->
      let%bind z' = goto_student student hw z in
      let%map sum = student_get_sum z' in
      p "%s's sum for hw%d: %d\n" student hw sum;
      z'
  | 4 ->
      Option.value_exn student ~message:"Student id is required for operation 4"
      |> fun student ->
        Option.value_exn problem ~message:"Problem name is required for operation 4"
        |> fun problem ->
          let problem = string_of_int problem in
          let%bind z' = goto_problem problem student hw z in
          let%map score = get_score z' in
          p "%s's grade for problem %s of hw%d: %d\n" student problem hw score;
          z'
  | 5 ->
      Option.value_exn student ~message:"Student id is required for operation 5"
      |> fun student ->
        Option.value_exn problem ~message:"Problem name is required for operation 5"
        |> fun problem ->
          Option.value_exn score ~message:"Score is required for operation 5"
          |> fun score ->
            let problem = string_of_int problem in
            let%bind z' = create_problem problem student hw z in
            let%map z' = set_score score z' in
            p "Set %s's grade for problem %s of hw%d to %d\n" student problem hw score;
            z'
  | 6 ->
        Option.value_exn problem ~message:"Problem name is required for operation 6"
        |> fun problem ->
            let%map z' = add_to_grade_queue ~hw ~problem ?student z in
            begin
              match student with
              | Some id when problem >= 0 -> p "Added hw %d, student '%s' and problem %d to grading queue\n" hw id problem;
              | None when problem >= 0-> p "Added hw %d and problem %d to grading queue for all students\n" hw problem;
              | Some id -> p "Added %d problems for hw %d and student '%s' to grading queue\n" (-problem) hw id;
              | None -> p "Added %d problems for every student in hw %d to grading queue\n" (-problem) hw;
            end;
            z'
  | 7 -> failwith "It should be impossible to get here"
  | _ -> failwithf "%d is not a valid operation" op ()

(* Operation 7: Gradescope example *)
let get_next grader z =
  let%bind z' = goto "queue" z >>= down in
  let%bind queue =
    fetch_file z' >>| Yojson.Safe.from_string >>=  queue_of_yojson
  in
  match queue.unassigned with
  | hd :: tl ->
    let u =
      {unassigned = tl; assigned = (grader,hd):: queue.assigned}
      |> queue_to_yojson |> Yojson.Safe.to_string
    in
    store_file u z'
    
  | [] -> mk_err "Queue is empty"

let inputRef : score option ref = ref None

let read_and_prompt ~msg () =
  match !inputRef with
  | Some input -> input
  | None ->
    msg ();
    let input =
      In_channel.input_line ~fix_win_eol:true In_channel.stdin
      |> Option.value_exn ~message:"read: Failed to receive input"
      |> int_of_string
    in
    inputRef := Some input;
    input

let assign_grade grader z =
  let get_mine z =
    let%bind z' = goto "queue" z >>= down in
    let%map queue =
      fetch_file z' >>| Yojson.Safe.from_string >>=  queue_of_yojson
    in
    List.Assoc.find_exn ~equal:equal_grader queue.assigned grader
  in
  let%bind (hw,student,problem) = get_mine z in
  let msg () =
    p "You are grading problem %d of student %s in hw %d.\n" problem student hw;
    p "Please enter a score for this problem> %!"
  in
  let score = read_and_prompt ~msg () in
  main ~op:5 ~hw ~student ~problem ~score ~debug:false z

let tx_grade ?grader () =
  let grader = Option.value_exn ~message:"tx_grade: Grader is required for this operation" grader in
  let _ : ForestIntf.t = TxForestCore.loop_txn grades_spec "/grades" ~f:(get_next grader) () in
  TxForestCore.loop_txn grades_spec "/grades" ~f:(assign_grade grader) ()

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs various operations on the 'grades' filestore.

    Operation 0: Compute average grade of a hw
      Req. args: hw
    Operation 1: Compute total grade of a hw
      Req. args: hw
    Operation 2: Compute average grade of a hw for a particular student
      Req. args: hw, student
    Operation 3: Compute total grade of a hw for a particular student
      Req. args: hw, student
    Operation 4: Print grade of a given problem, student, and hw
      Req. args: hw, student, problem
    Operation 5: Change grade of a given problem, student, and hw
      Req. args: hw, student, problem, score
    Operation 6: Add a hw, [student], and problem to grading queue
      Req. args: hw, problem
      Opt. args: student
      Note: If problem is negative, it adds that many problems instead
    Operation 7: Get a problem from the grading queue and assign a grade
      Req. args: grader
    Operation 8: Precise example from paper for printing grade
      Req. args: hw, student, problem
    Operation 9: Precise example from paper for updating rubric and grades
      Req. args: hw, problem, max-score
    Operation 10: Precise example from paper for checking rubric and setting a grade
      Req. args: hw, student, problem, max-score, score
    Operation 11: Precise example from paper for renormalizing
      Req. args: hw, min, max
    "
    [%map_open
      let op = flag "op" (required int)
              ~doc:"[0-7] Run this operation"
      and hw = flag "hw" (optional_with_default 1 int)
                  ~doc:"N Homework number (default 1)"
      and student = flag "student" (optional string)
                  ~doc:"N Student Id"
      and problem = flag "problem" (optional int)
                  ~doc:"N Problem Id"
      and score = flag "score" (optional int)
                  ~doc:"N Score"
      and debug = flag "debug" (no_arg)
                  ~doc:"Print debug statements"
      and grader = flag "grader" (optional string)
                  ~doc:"N Grader Id"
      and max_score = flag "max-score" (optional int)
                  ~doc:"N Max Score for changing or checking rubric"
      and max = flag "max" (optional int)
                  ~doc:"N Max score after normalization"
      and min = flag "min" (optional int)
                  ~doc:"N Minimum score after normalization"
      in
      match op with
      | 7 -> ignore_after_f (tx_grade ?grader)
      | 8 -> get_print_score ~hw ?student ?problem
      | 9 -> ignore_after_f (update_rubric_and_grades ~hw ?problem ?max_score)
      | 10 -> ignore_after_f (check_rubric_and_grade ~hw ?student ?problem ?max_score ?score)
      | 11 -> ignore_after_f (renormalize ~hw ?min ?max)
      | _ -> ignore_after_f (TxForestCore.loop_txn grades_spec "/grades" ~f:(main ~op ~hw ~debug ?student ?problem ?score))
    ]
    |> Command.run
