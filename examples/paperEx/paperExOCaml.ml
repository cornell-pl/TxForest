open Core

type hw = int
and student = string
and score = int

(* Example directory from main repo *)
let baseDir = "forest/examples/paperEx/grades"

(* Helpers *)
let parse_score u = 
  int_of_string_opt u
  |> Option.value_exn ~message:"parse_score: File was improperly formatted"

let set_score score = Out_channel.write_all ~data:(string_of_int score) 

let get_score file = In_channel.read_all file |> parse_score

let get_hwDir hw = string_of_int hw |> (^) "hw" |> (Filename.concat baseDir)
let get_studentFile hw student = Filename.concat (get_hwDir hw) student
  

(* Get Score *)

let get_score_input ~hw ?student () = 
  let get_score_simple hw student = 
    let score = get_studentFile hw student |> get_score in
    Printf.printf "%s's grade for hw%d: %d\n" student hw score
  in
  let student = Option.value_exn ~message:"get_score: Supply a student please" student in
  get_score_simple hw student

(* Set Score *)
let set_score_input ~hw ?student ?score () =
  let set_score_simple hw student score = 
    get_studentFile hw student |> set_score score;
    Printf.printf "Set %s's grade for hw%d to %d\n" student hw score
  in
  let student = Option.value_exn ~message:"set_score: Supply a student please" student in
  let score = Option.value_exn ~message:"set_score: Supply a score please" score in
  set_score_simple hw student score
  
(* PaperEx Opt 3: 
   - T1: Renormalize
   - T2: Renormalize
 *)

 let map_scores ~f hwDir = 
  Sys.ls_dir hwDir
  |> List.iter ~f:(fun name -> 
      if name <> "max"
      then 
        let file = hwDir ^/ name in
        let score = get_score file in
        let score_new = f score in
        let _ = Printf.printf "Score before/after = %d / %d\n" score score_new in
        set_score score_new file
    )

let renorm featmin featmax goalmin goalmax score =
  Printf.printf "Renormalizing with Xmin = %d, Xmax = %d, goalmin = %d, goalmax = %d\n"
  featmin featmax goalmin goalmax;
  if featmax = featmin
  then score
  else 
    let denom = float_of_int ((score - featmin)*(goalmax - goalmin)) in
    let div = float_of_int (featmax - featmin) in
    denom /. div |> Float.round_nearest |> int_of_float |> (+) goalmin

let get_min_max hwDir =
  Sys.fold_dir ~init:(Int.max_value,Int.min_value) ~f:(fun (l,h) name -> 
    if name = "max"
    then (l,h)
    else 
      let score = get_score (hwDir ^/ name) in
      (min score l, max score h)
  ) hwDir

let renormalize hw goalmin goalmax =
  let hwDir = get_hwDir hw in
  let (featmin, featmax) = get_min_max hwDir in
  map_scores ~f:(renorm featmin featmax goalmin goalmax) hwDir

let renormalize_input ~hw ?min ?max () = 
  let min = Option.value_exn ~message:"renormalize: Supply a minimum score please" min in
  match max with
  | None -> (get_hwDir hw) ^/ "max" |> get_score |> renormalize hw min
  | Some max -> renormalize hw min max

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