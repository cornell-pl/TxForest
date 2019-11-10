open Core


(* Example directory from main repo *)
let baseDir = "forest/examples/paperEx/grades"

let get_hwDir hw = string_of_int hw |> (^) "hw" |> (Filename.concat baseDir)
let get_studentDir hw student = Filename.concat (get_hwDir hw) student
let get_problemFile hw student problem = 
  string_of_int problem |> Filename.concat (get_studentDir hw student) 
  
let get_score_from_file file =
  In_channel.read_all file |> String.split ~on:'/' |> List.hd_exn |>
  int_of_string

let set_score_to_file score = Out_channel.write_all ~data:((string_of_int score) ^ "/10")
  
let get_score_unsafe hw student problem =
  let problemPath = sprintf "grades/hw%d/%s/%d" hw student problem in
  let file = In_channel.read_all problemPath in
  String.split ~on:'/' file |> List.hd_exn |> int_of_string

let get_score_safe hw student problem =
  let hwPath = sprintf "%s/hw%d" baseDir hw in
  match Sys.is_directory hwPath with
  | `Yes ->
    let studentPath = sprintf "%s/%s" hwPath student in
    begin
      match Sys.is_directory studentPath with
      | `Yes ->
        let problemPath = sprintf "%s/%d" studentPath problem in
        begin
          match Sys.is_file problemPath with
          | `Yes ->
            let file = In_channel.read_all problemPath in
            String.split ~on:'/' file |> List.hd_exn |> int_of_string
          | _ -> failwithf "get_score: %s is not a file" problemPath ()
        end
      | _ -> failwithf "get_score: %s is not a directory" studentPath ()
    end
  | _ -> failwithf "get_score: %s is not a directory" hwPath ()

let set_score hw student problem score =
  let studentDir = get_studentDir hw student in
  match Sys.is_directory studentDir with
  | `Yes -> set_score_to_file score (get_problemFile hw student problem)
  | _ -> failwithf "set_score: %s is not a directory" studentDir ()


let get_score hw student problem =
  let problemFile = get_problemFile hw student problem in
  match Sys.is_file problemFile with
  | `Yes -> get_score_from_file problemFile
  | _ -> failwithf "get_score: %s is not a file" problemFile ()

let student_get_sum hw student =
  let studentDir = get_studentDir hw student in
  match Sys.is_directory studentDir with
  | `Yes ->
    Sys.fold_dir studentDir ~init:0 
      ~f:(fun sum problem -> get_score_from_file (Filename.concat studentDir problem) + sum)
  | _ -> failwithf "student_get_sum: %s is not a directory" studentDir ()

let student_get_avg hw student =
  let studentDir = get_studentDir hw student in
  match Sys.is_directory studentDir with
  | `Yes ->
    let (sum,amt) = 
      Sys.fold_dir studentDir ~init:(0,0) 
        ~f:(fun (sum,amt) problem -> 
            let sum = get_score_from_file (Filename.concat studentDir problem) + sum in
            sum, amt+1
          )
    in
    (float_of_int sum) /. (float_of_int amt)
  | _ -> failwithf "student_get_avg: %s is not a directory" studentDir ()

let hw_get_sum hw =
  let hwDir = get_hwDir hw in
  match Sys.is_directory hwDir with
  | `Yes ->
    Sys.fold_dir hwDir ~init:0 
      ~f:(fun sum student -> student_get_sum hw student + sum)
  | _ -> failwithf "hw_get_sum: %s is not a directory" hwDir ()
    
let hw_get_avg hw =
  let hwDir = get_hwDir hw in
  match Sys.is_directory hwDir with
  | `Yes ->
    let (sum,amt) = 
      Sys.fold_dir hwDir ~init:(0,0) 
        ~f:(fun (sum,amt) student -> 
            let sum = student_get_sum hw student + sum in
            sum, amt+1
          )
    in
    (float_of_int sum) /. (float_of_int amt)
  | _ -> failwithf "hw_get_avg: %s is not a directory" hwDir ()

let main ~op ~hw ?student ?problem ?score () = 
  let open Printf in
  match op with
  | 0 -> 
    let avg = hw_get_avg hw in
    printf "Hw%d Average: %g\n" hw avg
  | 1 -> 
    let sum = hw_get_sum hw in
    printf "Hw%d Sum: %d\n" hw sum
  | 2 -> 
    Option.value_exn student ~message:"Student id is required for operation 2" 
    |> fun student -> 
      let avg = student_get_avg hw student in
      printf "%s's average for hw%d: %g\n" student hw avg
    
  | 3 -> 
    Option.value_exn student ~message:"Student id is required for operation 3" 
    |> fun student -> 
      let sum = student_get_sum hw student in
      printf "%s's sum for hw%d: %d\n" student hw sum
  | 4 -> 
      Option.value_exn student ~message:"Student id is required for operation 4" 
      |> fun student ->
        Option.value_exn problem ~message:"Problem number is required for operation 4" 
        |> fun problem -> 
          let score = get_score_safe hw student problem in
          printf "%s's grade for problem %d of hw%d: %d\n" student problem hw score
  | 5 -> 
      Option.value_exn student ~message:"Student id is required for operation 5" 
      |> fun student ->
        Option.value_exn problem ~message:"Problem number is required for operation 5" 
        |> fun problem -> 
          Option.value_exn score ~message:"Score is required for operation 5" 
          |> fun score -> 
            set_score hw student problem score;
            printf "Set %s's grade for problem %d of hw%d to %d\n" student problem hw score
  | _ -> failwithf "%d is not a valid operation" op ()

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs various operations on the 'grades' filestore.
    
    Operation 0: Compute average grade of a hw (req. args: hw)
    Operation 1: Compute total grade of a hw (req. args: hw)
    Operation 2: Compute average grade of a hw for a particular student (req. args: hw, student)
    Operation 3: Compute total grade of a hw for a particular student (req. args: hw, student)
    Operation 4: Print grade of a given problem, student, and hw (req. args: hw, student, problem)
    Operation 5: Change grade of a given problem, student, and hw (req. args: hw, student, problem, score)
    "
    [%map_open
      let op = flag "op" (required int) 
              ~doc:"[0-5] Run this operation"
      and hw = flag "hw" (optional_with_default 1 int)
                  ~doc:"N Homework number (default 1)"
      and student = flag "student" (optional string)
                  ~doc:"N Student Id"
      and problem = flag "problem" (optional int)
                  ~doc:"N Problem number"
      and score = flag "score" (optional int)
                  ~doc:"N Score"
      in
      main ~op ~hw ?student ?problem ?score
    ]
    |> Command.run
