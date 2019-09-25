open Core
open Forest

module CostMon = Forest.CostNameMon
[%%forest {|
  hws = directory {
      max is "max" :: file;
      students is [student :: file | student <- matches RE "[a-z]+[0-9]+"]
    }

  grades = [hw :: hws | hw <- matches RE "hw[0-9]+"]

|}]
let baseDir =
"/home/dilorenzo/everything/research/project/ozfs/forest/examples/paperEx"

(* Helpers *)
let get_path md = 
  Option.value_exn ~message:"get_path: Info was empty" md.info
  |> fun info -> info.full_path

let find u (compr,compmd) =
  List.zip_exn compr compmd.data
  |> List.find_exn ~f:(fun (r,md) -> String.equal (Filename.basename (get_path md)) u)

let parse_score u = 
  let opt = int_of_string_opt u in
  Option.value_exn ~message:"parse_score: File was improperly formatted" opt

let get_score = parse_score

let set_score score (_,md) = store_file (string_of_int score,md) (get_path md)

let get_hwDir hw = string_of_int hw |> (^) "hw" |> (Filename.concat baseDir)

let get_hw hw = 
  let (gr,gmd) = grades_load (baseDir ^/ "grades") in
  if gmd.num_errors = 0
  then 
    find (sprintf "hw%d" hw) (gr,gmd)
  else
    let error = String.concat ~sep:"\n" gmd.error_msg in
    failwith error

let get_student hw student = 
  let (hwr, hwmd) = get_hw hw in
  find student (hwr.students,hwmd.data.students_md)
  

(* Get Score *)
let get_score_input ~hw ?student () = 
  let get_score_simple hw student = get_student hw student |> fst |> get_score in
  let student = Option.value_exn ~message:"get_score: Supply a student please" student in
  let score = get_score_simple hw student in
  Printf.printf "%s's grade for hw%d: %d\n" student hw score

(* Set Score *)
let set_score_simple hw student score = get_student hw student |> set_score score


let set_score_input ~hw ?student ?score () =
  let student = Option.value_exn ~message:"set_score: Supply a student please" student in
  let score = Option.value_exn ~message:"set_score: Supply a score please" score in
  set_score_simple hw student score;
  Printf.printf "Set %s's grade for hw%d to %d\n" student hw score
  
(* PaperEx Opt 3: 
   - T1: Renormalize
   - T2: Renormalize
 *)
let renorm featmin featmax goalmin goalmax score =
  Printf.printf "Renormalizing with Xmin = %d, Xmax = %d, goalmin = %d, goalmax = %d\n"
  featmin featmax goalmin goalmax;
  if featmax = featmin
  then score
  else 
    let denom = float_of_int ((score - featmin)*(goalmax - goalmin)) in
    let div = float_of_int (featmax - featmin) in
    denom /. div |> Float.round_nearest |> int_of_float |> (+) goalmin

 let map_scores ~rn (hwr,hwmd) = 
  List.zip_exn hwr.students hwmd.data.students_md.data
  |> List.iter ~f:(fun (rep,md) -> 
      let score = get_score rep in
      let score_new = rn score in
      let _ = Printf.printf "Score before/after = %d / %d\n" score score_new in
      set_score score_new (rep,md)
    )

let get_min_max hwr =
  List.fold hwr.students ~init:(Int.max_value,Int.min_value) 
    ~f:(fun (l,h) rep -> 
      let score = get_score rep in
      (min score l, max score h)
    )

let renormalize goalmin goalmax (hwr,hwmd) =
  let (featmin, featmax) = get_min_max hwr in
  map_scores ~rn:(renorm featmin featmax goalmin goalmax) (hwr,hwmd)

let renormalize_input ~hw ?min ?max () = 
  let min = Option.value_exn ~message:"renormalize: Supply a minimum score please" min in
  let (hwr,hwmd) = get_hw hw in
  let max = 
    match max with
    | None -> get_score hwr.max
    | Some max -> max
  in
  renormalize min max (hwr,hwmd)

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
      in
      match op with
      | 0 -> get_score_input ~hw ?student
      | 1 -> set_score_input ~hw ?student ?score
      | 2 -> renormalize_input ~hw ?min ?max
      | _ -> failwithf "%d is not a valid operation" op
    ]
    |> Command.run

