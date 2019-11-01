
open Core
open ForestIntf
open ForestCoreExn



[%%txforest {|
    univ = [x :: univ option | x <- matches GL "*"]
|}]



let name_to_spec name =
  match name with
  | "universal" -> univ_spec
  | _ -> failwith "that spec has not been specified"
