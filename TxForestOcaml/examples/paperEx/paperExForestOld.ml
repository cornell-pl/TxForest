open Core
open Forest

module CostMon = Forest.CostNameMon
[%%forest {|
  students = map [problem :: file | problem <- matches RE "[0-9]+"]
  hws = map [student :: students | student <- matches GL "*"]
  grades = map [hw :: hws | hw <- matches RE "hw[0-9]+"]
|}]

let parse_score u = 
  let score_opt = String.split ~on:'/' u |> List.hd in
  Option.value_exn ~message:"get_score: File was improperly formatted" score_opt
  |>  int_of_string

let baseDir = "/home/dilorenzo/everything/research/project/ozfs/forest/examples/paperEx"

let get_path md = 
  Option.value_exn ~message:"get_path: Info was empty" md.info
  |> fun info -> info.full_path

let find name map = 
  PathMap.filter (fun key _ -> String.equal (Filename.basename key) name) map
  |> PathMap.bindings |> List.hd_exn |> snd

let get_score hw student problem =
  let (gr,gmd) = grades_load (baseDir ^/ "grades") in
  if gmd.num_errors = 0
  then 
    find (sprintf "hw%d" hw) gr
    |> find student
    |> find (string_of_int problem)
    |> parse_score
  else
    let error = String.concat ~sep:"\n" gmd.error_msg in
    failwith error
  

let () =
  Printf.printf "Score is %d\n" (get_score 1 "aaa17" 1)