
(*
[%%forest {|

student1 = [pnum :: file | pnum <- matches GL "(0-9)+"]

student2 = file

hwdir_spec = [x :: student1 | x <- matches GL "*"]

(* Explicit enumeration so we have pairs,
 * but should probably be a comprehension *)
grades = directory {
  hw1 is "hw1" :: hwdir_spec;
  hw2 is "hw2" :: hwdir_spec;
  hw3 is "hw3" :: hwdir_spec;
  hw4 is "hw4" :: hwdir_spec;
  hw5 is "hw5" :: hwdir_spec;
  }

|}]

Differences in core syntax:

grades =
   <"hw1" :: hwdir_spec,
     <"hw2" :: hwdir_spec,
       <"hw3" :: hwdir_spec,
         <"hw4" :: hwdir_spec,
          "hw5" :: hwdir_spec>>>>

*)


(* TODOS:
 * - See inline!
 * - When you add goto_child, make most name based
 * - Additional possible transactions:

  T3a:
  - Directory also has parternship mapping (partners.txt)
  - Read score of one partner, write score of other partner

  T4: (read N values, by-homework)
  - Compute summary stats for one homework

*)

open Core
open ExUtils

open Result
open Result.Let_syntax
open Forest
open OldForest
(* GENERATED *)


let numCheck s = Option.is_some (int_of_string_opt s)
let always _ = true

let student1_spec = Comp (PathExp (Var "pnum",File),"pnum",DirList numCheck)
let student1_zipper = mk_zipper student1_spec

let student2_spec = File
let student2_zipper = mk_zipper student2_spec

let hwdir_spec = Comp (PathExp (Var "x",student1_spec),"x",DirList always)
let hwdir_zipper = mk_zipper hwdir_spec

let grades_spec =
  IPair(PathExp(Name "hw1", hwdir_spec),
    IPair(PathExp(Name "hw2", hwdir_spec),
      IPair(PathExp(Name "hw3", hwdir_spec),
        IPair(PathExp(Name "hw4", hwdir_spec),
              PathExp(Name "hw5", hwdir_spec)
     ))))

let grades_zipper = mk_zipper grades_spec

(* REAL CODE *)

(* Helper functions *)

let fold_gen_form_stud ~f exPos lst z =
  let%bind z = down z in
  let%bind z = goto_pos_comp exPos z in
  let%bind (lst,z) = f z lst in
  let%map z = up z >>= up >>= up in
  (lst,z)

let apply_gen_form_stud ~f exPos z =
  let%bind z = down z in
  let%bind z = goto_pos_comp exPos z in
  let%bind z = f z in
  let%map z = up z >>= up >>= up in
  z

(* Transactions *)
(*
  T1 (write one value):
  - Update the score for one homework, student, exercise
    at hwPos, studPos, and exPos respectively
    I.e. update score for Homework (hwPos + 1), (studPos + 1)th student,
    Exercise (exPos+1)
*)

let t1 hwPos studPos exPos score z =
  goto_pos_dir hwPos z
  >>= goto_pos_comp studPos
  >>= goto_pos_comp exPos
  >>= write_num score

(*
  T2 (write N values):
  - Update the scores for one homework, N students, one exercise
    I.e. update score for every student's Homework (hwPos + 1),
    Exercise (exPos+1)
*)
let t2 hwPos exPos score z =
  let%bind z = goto_pos_dir hwPos z in
  match snd (fetch z) with
  | CompRep l ->
    let i = List.length l in
    let _ = d "Comp num %d" i in
    let score_student = apply_gen_form_stud ~f:(write_num score) exPos in
    down z >>= apply_n_comp ~f:score_student i >>= up
  | _ -> failwith "t2: homework did not contain a comprehension"

let memory_table = Int.Table.create ()

let read_and_prompt ~msg n =
  match Int.Table.find memory_table n with
  | Some input -> input
  | None ->
    msg ();
    let input =
      In_channel.input_line ~fix_win_eol:true In_channel.stdin
      |> Option.value_exn ~message:"read: Failed to receive input"
    in
    Int.Table.set ~key:n ~data:input memory_table;
    input

let t2_stream hwPos exPos z =
  let%bind z = goto_pos_dir hwPos z in
  match snd (fetch z) with
  | CompRep l ->
    let i = List.length l in
    let rec interactive_score z n =
      let n = i-n in
      let msg () =
        p "Enter a new score for student %d> %!" n
      in
      let input = read_and_prompt ~msg n in
      try
        write_num (int_of_string input) z
      with _ ->
        p "Please enter a number!\n %!";
        Int.Table.remove memory_table n;
        interactive_score z n
    in
    let f z c =
      let%bind z = down z in
      let%bind z = goto_pos_comp exPos z in
      let%bind z = interactive_score z c in
      let%map z = up z >>= up >>= up in
      z
    in
    down z >>= apply_n_comp_i ~f i >>= up
  | _ -> failwith "t2: homework did not contain a comprehension"



(*
  T3 (read and write the same N values):
  - Normalize the scores for one homework, one exercise i.e., read all
    the scores, do something outside of Forest, then write all the
    scores. Another scenario that fits this is changing the rubric.
*)
let t3 hwPos exPos z =
  let apply_f ~(f: score list -> score list) lst = f lst in
  let some_computation lst =
    List.min_elt lst ~compare:Int.compare
    |> Option.value_map ~default:[]
      ~f:(fun i -> List.map ~f:(fun _ -> i) lst)
  in
  let%bind z = goto_pos_dir hwPos z in
  match snd (fetch z) with
  | CompRep l ->
    let i = List.length l in
    let _ = d "Comp num %d" i in
    let collect z lst =
      mk_ok (get_num z :: lst, z)
    in
    let disseminate z = function
      | hd :: tl ->
        let%map z = write_num hd z in
        (tl,z)
      | [] -> mk_ok ([],z)
    in
    let collect = fold_gen_form_stud ~f:collect exPos in
    let disseminate = fold_gen_form_stud ~f:disseminate exPos in
    let%bind z = down z in
    (* TODO: Check why precisely this should be i-1, or fix your primitives *)
    let%bind (lst,z) = fold_n_comp ~f:collect i ([],z) in
    let lst = apply_f ~f:some_computation lst in
    let%bind z = up z >>= down in
    let%bind (_,z) = fold_n_comp ~f:disseminate i (lst,z) in
    up z
  | _ -> failwith "t3: homework did not contain a comprehension"

(*
    T5: (read N values, by-student)
    - Compute score for all homeworks, one student
    TODO: Right now just prints a value for each HW
*)
let t5 studPos z =
  let get_exercise score z =
    let%bind z = down z in
    let score = get_num z + score in
    let%map z = up z in
    (score, z)
  in
  let get_hw z =
    let%bind z = down z >>= goto_pos_comp studPos in
    match snd (fetch z) with
    | CompRep l ->
      let i = List.length l in
      let%bind z = down z in
      let%bind (v,z) = fold_n_comp ~f:get_exercise i (0,z) in
      Printf.printf "t5: Student %d got %d for this hw" studPos v;
      up z
    | _ -> failwith "t5: student did not contain a comprehension"
    >>= up >>= up
    >>= up
  in
  down z >>= apply_n_dir ~f:get_hw 1 >>= up

(*  T6: (add a new homework assignment, new problem, new student) *)
let t6 _ = failwith "t6: This is not currently possible through Forest"

(*
  T7 (read one value):
  - Read score for one homework, student, exercise
    at hwPos, studPos, and exPos respectively
    I.e. update score for Homework (hwPos + 1), (studPos + 1)th student,
    Exercise (exPos+1)
*)
let t7 hwPos studPos exPos z =
  goto_pos_dir hwPos z
  >>= goto_pos_comp studPos
  >>= goto_pos_comp exPos
  >>| fun z -> get_and_print z; z


let main hwPos studPos exPos score trans stream zipper : t =
  begin
    if stream
    then t2_stream hwPos exPos zipper
    else
      match trans with
      | 1 -> t1 hwPos studPos exPos score zipper
      | 2 -> t2 hwPos exPos score zipper
      | 3 -> t3 hwPos exPos zipper
      | 4 -> failwith "Transaction %d is not implemented yet" trans
      | 5 -> t5 studPos zipper
      | 6 -> t6 zipper
      | 7 -> t7 hwPos studPos exPos zipper
      | _ -> failwith "No transaction %d exists" trans
  end
  |> function
  | Ok zipper -> zipper
  | Error s -> p "Error: %s" s; zipper

(* TODO: Add in streaming option *)
let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs various transactions on the 'grades' filestore"
    [%map_open
      let t = flag "trans" (required int) ~doc:"[1-7] Run this transaction"
      and hwPos = flag "hw" (optional_with_default 0 int)
                  ~doc:"N Position of hw to affect if any (0 indexed)"
      and studPos = flag "student" (optional_with_default 0 int)
                  ~doc:"N Position of student to affect if any (0 indexed)"
      and exPos = flag "problem" (optional_with_default 0 int)
                  ~doc:"N Position of problem to affect if any (0 indexed)"
      and score = flag "score" (optional_with_default 0 int)
                  ~doc:"N Score to set if any"
      and stream = flag "stream" (no_arg)
                  ~doc:"Streams transaction 2 (if chosen)"
      in
      run_txn grades_spec "/grades"
        ~f:(main hwPos studPos exPos score t stream)


    ]
    |> Command.run