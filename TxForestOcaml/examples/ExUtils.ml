open Core
open Result.Let_syntax
open TxForest
open OldForest
type score = int

(* Helper functions *)
let failwith format = Printf.ksprintf failwith format

let p = Printf.printf

let get_and_print z =
  match snd (shallow_fetch z) with
  | FileRep s -> p "File: %s\n" s
  | CompRep i -> p "Comprehension: %d\n" (List.length i)
  | PathRep -> p "Path\n"
  | PairRep -> p "Pair\n"

(* Final down comes from walking through path *)
let goto_pos_comp pos z =
  let rec goto_pos pos z =
    if pos > 0
    then next z >>= goto_pos (pos-1)
    else return z
  in
  down z >>= goto_pos pos >>= down

(* Intermediate downs to enter new pairs *)
(* TODO: I think these break for the last entry *)
let goto_pos_dir pos z =
  let rec goto_pos pos z =
    (*i think this makes it one indexed*)
    if pos > 1
    then next z >>= down >>= goto_pos (pos-1)
    else return z
  in
  down z >>= goto_pos (pos) >>= down

let goto_pos_dir_alt pos z =
  let rec goto_pos pos z =
    if pos > 0
    then next z >>= (fun z ->
      match snd(shallow_fetch z) with
      | PairRep -> down z
      | _ -> mk_ok z)
    >>= goto_pos (pos-1)

    else return z
  in
  down z >>= goto_pos (pos) >>= down

let rec apply_n_comp ~f n z =
  if n > 1
  then f z >>= next >>= apply_n_comp ~f (n-1)
  else f z

let rec apply_n_comp_i ~f n z =
  if n > 1
  then f z n >>= next >>= apply_n_comp_i ~f (n-1)
  else f z n

(*this prolly breaks on the last one too?*)
let rec apply_n_dir ~f n z =
  if n > 1
  then f z >>= next >>= down >>= apply_n_dir ~f (n-1)
  else f z

let rec apply_n_dir_alt ~f n z =
  if n > 1
  then f z >>= next >>= (fun z ->
    match snd(shallow_fetch z) with
      | PairRep -> down z
      | _ -> mk_ok z)
    >>= apply_n_dir_alt ~f (n-1)
  else f z

let rec apply_dir ~f z =
  match snd(shallow_fetch z) with
  | PairRep -> down z >>= f >>= next >>= apply_dir ~f:f >>= prev >>= up
  | _ -> f z


let rec fold_n_comp ~f n (init, z)=
  if n > 1
  then
    let%bind (init,z) = f init z in
    let%bind z = next z in
    fold_n_comp ~f (n-1) (init,z)
  else f init z

let get_num z =
  shallow_fetch z
  |> snd
  |> function
  | FileRep s -> int_of_string s
  | _ -> failwith "get_num: Current node did not contain only a number"

let write_num n = put (FileRep (string_of_int n))
let write_string s = put (FileRep s)

