open Core
open ExUtils
open Result
open Result.Let_syntax

open Forest
open OldForest




let always _ = true

(*
let cat_spec = Comp (PathExp (Var "x",File),"x",DirList always)
let cat_zipper = mk_zipper cat_spec

let cats_spec =
  IPair(PathExp(Name "munchkin", cat_spec),
          PathExp(Name "other", cat_spec))
let cats_zipper = mk_zipper cats_spec

let dog_spec = Comp (PathExp (Var "x",File),"x",DirList always)
let dog_zipper = mk_zipper dog_spec

let dogs_spec =
  IPair(PathExp(Name "large", dog_spec),
        PathExp(Name "small", dog_spec))
let dogs_zipper = mk_zipper dogs_spec

let shelter_spec =
  IPair(PathExp(Name "cats", cats_spec),
    PathExp(Name "dogs", dogs_spec)

  )
let shelter_zipper = mk_zipper shelter_spec
*)


[%%txforest {|
    cat_spec = [ x :: file | x <- $always$ ]

    cats_spec = directory {
        munchkin is "munchkin" :: cat_spec ;
        other is "other" :: cat_spec
    }

    dog_spec = [ x :: file | x <- $always$ ]

    dogs_spec = directory {
        large is "large" :: dog_spec ;
        other is "small" :: dog_spec
    }

    shelter_spec = directory {
        cats is "cats" :: cats_spec ;
        other is "dogs" :: dogs_spec
    }

|}]

let get_fiona (zipper:t)=
  (get_and_print zipper; down zipper) (*in shelter pair*)
  >>= (fun z -> get_and_print z; down z) (*into cats path*)
  >>= (fun z -> get_and_print z; down z) (*into cats pair cats*)
  >>= (fun z -> get_and_print z; next z) (*right to other*)
  >>= (fun z -> get_and_print z; down z) (*into other path*)
  >>= (fun z -> get_and_print z; down z) (*into other comp*)
  >>= (fun z -> get_and_print z; next z) (*into second item*)
  >>= (fun z -> get_and_print z; down z) (*into second file*)
  >>| fun z -> get_and_print z; z

let traverse_to (animal_type:int) (sub_type: int) (name:int) (zipper: t) =
  goto_pos_dir_alt animal_type zipper
  >>= goto_pos_dir_alt sub_type
  >>= goto_pos_comp name

let get_info (animal_type:int) (sub_type: int) (name:int) (zipper: t) =
  traverse_to animal_type sub_type name zipper
  >>| fun z -> get_and_print z; z

let get_info_for_name (animal_type:int) (sub_type: int) (name:string) (zipper: t) =
  goto_pos_dir_alt animal_type zipper
  >>= goto_pos_dir_alt sub_type
  >>= goto name
  >>= down
  >>| fun z -> get_and_print z; z

let read_and_prompt ~msg =
  msg ();
  let input =
    In_channel.input_line ~fix_win_eol:true In_channel.stdin
    |> Option.value_exn ~message:"read: Failed to receive input"
  in
  input

let goto_animal (animal_type:int) (sub_type: int) (name:string) (zipper: t) =
  goto_pos_dir_alt animal_type zipper
  >>= goto_pos_dir_alt sub_type
  >>= goto name
  >>= down


let stream_set_age (animal_type:int) (sub_type: int) (name:string) (zipper: t) =
  let%bind z = goto_pos_dir_alt animal_type zipper
               >>= goto_pos_dir_alt sub_type in
  let msg () = p "Enter a new age for the animal %s> %!" name in
  let new_age = read_and_prompt ~msg:msg in
    let%bind z' = goto name z >>= down in
    match fetch z' with
    | _, FileRep s -> begin
      match (String.split s ~on:',') with
      | avail::breed::imunized::_::[] -> begin
        let new_info = avail::breed::imunized::new_age::[] in
        let stringified = String.concat ~sep:"," new_info in
          write_string stringified z'
      end
      | _ -> failwith "wrong file format"
    end
    | _ -> mk_ok z'




let on_availability a n zipper =
  match snd (fetch zipper) with
  | FileRep s -> begin
    match (String.split s ~on:',') with
    | avail::_ ->
      if (int_of_string avail) = 1
      then a s
      else n s
    | _ -> failwith "wrong file format"
  end
  | _ -> failwith "can not get avaiability of a non file"


let get_avaialitity =
  on_availability (fun _ -> p "Available") (fun _ -> p "Unavailable")


let is_avaiable (animal_type:int) (sub_type: int) (name:int) (zipper: t) =
  traverse_to animal_type sub_type name zipper
  >>| fun z -> get_avaialitity z; z

let set_availablity zipper =
  match snd (fetch zipper) with
  | FileRep s -> begin
    match (String.split s ~on:',') with
    | avail::t ->
      if (int_of_string avail) = 0
      then failwith "can not adopt an unavaiable animal"
      else begin
        let new_info = "0" :: t in
        let stringified = String.concat ~sep:"," new_info in
          write_string stringified zipper
      end
    | _ -> failwith "wrong file format"
  end
  | _ -> failwith "can not set avaiability of a non file"


let adopt (animal_type:int) (sub_type: int) (name:int) (zipper: t) =
  traverse_to animal_type sub_type name zipper
  >>= set_availablity


let fold_over_sub_type (f:t -> t) (zipper: t) =
  match snd (fetch zipper) with
  | CompRep l -> begin
    let i = List.length l in
      down zipper
      >>= (apply_n_comp ~f:(fun z ->
        down z
        >>| f
        >>= up
      ) i)
      >>= up
  end
  | _ -> failwith "list of animals is not a comprehension"

let get_all_of_sub_type =
  fold_over_sub_type (fun z' -> get_and_print z'; p "\n"; z')

let read_all_of_sub_type (animal_type:int) (sub_type:int) (zipper:t) =
  goto_pos_dir_alt animal_type zipper
  >>= goto_pos_dir_alt sub_type
  >>= get_all_of_sub_type

let fold_over_type f =
  apply_dir ~f: (fun z ->
    down z
    >>= f
    >>= up
  )

let read_all_of_type (animal_type:int) (zipper:t) =
  goto_pos_dir_alt animal_type zipper
  >>= fold_over_type get_all_of_sub_type


let read_all_avail_of_type (animal_type:int) (zipper:t) =
  goto_pos_dir_alt animal_type zipper
  >>= fold_over_type (fun z ->
    fold_over_sub_type (fun z' ->
      (on_availability (fun s -> p "%s\n" s) (fun _ -> ()) z' ); z'
    ) z
  )

let fold_over_all g =
  apply_dir ~f: (fun z ->
    down z
    >>= (fold_over_type (fun z' -> fold_over_sub_type g z' ))
    >>= up
  )

let read_all = fold_over_all (fun z' -> get_and_print z'; p "\n"; z')

let get_path_of (animal_type:int) (sub_type: int) (name:int) (zipper: t) =
  traverse_to animal_type sub_type name zipper
  >>| fun z -> p "my path is: %s \n" (fst (fetch z)); z

let count_total (fold) (z: t) =
  fold ~f:(fun i z ->
    match shallow_fetch z with
    | _, FileRep _ -> i+1
    | _ ->i
  ) 0 z

let count_available (fold) (z: t) =
  fold ~f:(fun i z ->
    match shallow_fetch z with
    | _, FileRep _ -> on_availability (fun _ -> i+1) (fun _ -> i) z
    | _ -> i
  ) 0 z

let incr_age (map) (z:t) =
  map ~m:(fun z ->
    match shallow_fetch z with
    | _, FileRep s -> begin
      match (String.split s ~on:',') with
      | avail::breed::imunized::age::[] -> begin
        let new_age = string_of_int ((int_of_string age) + 1) in
        let new_info = avail::breed::imunized::new_age::[] in
        let stringified = String.concat ~sep:"," new_info in
          write_string stringified z
      end
      | _ -> failwith "wrong file format"
    end
    | _ -> mk_ok z
  ) z

let main (trans :int) (animal_type: int option) (subtype: int option) (id: int option) (name:string option) (order: bool) (debug: bool) (zipper:t) : t =
  begin
    if debug then OldForest.set_debug () else ();
    match trans with
    | 1 -> begin
      match animal_type, subtype, id with
      | Some a, Some s, Some n -> get_info a s n zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 2 -> begin
      match animal_type, subtype, id with
      | Some a, Some s, Some n -> is_avaiable a s n zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 3 -> begin
      match animal_type, subtype, id with
      | Some a, Some s, Some n -> adopt a s n zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 4 -> begin
      match animal_type, subtype with
      | Some a, Some s -> read_all_of_sub_type a s zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 5 -> begin
      match animal_type with
      | Some a -> read_all_of_type a zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 6 -> begin
      match animal_type with
      | Some a -> read_all_avail_of_type a zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 7 -> begin
        read_all zipper
      end
    | 8 -> begin
      match animal_type, subtype, id with
      | Some a, Some s, Some n -> get_path_of a s n zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 9 -> begin
      match animal_type, subtype, name with
      | Some a, Some s, Some n -> get_info_for_name a s n zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | 10 -> begin
      let fold = if order then fold_preorder else fold_postorder in
      p "the number of nodes in the tree is: %d \n" (fold ~f:(fun i _ -> i + 1) 0 zipper);
      mk_ok zipper
    end
    | 11 -> begin
      let fold = if order then fold_preorder else fold_postorder in
      p "the number of animals in the shelter is: %d \n" (count_total fold zipper);
      mk_ok zipper
    end
    | 12 -> begin
      let fold = if order then fold_preorder else fold_postorder in
      p "the number of avaiable animals in the shelter is: %d \n"
        (count_available fold zipper);
      mk_ok zipper
    end
    | 13 -> begin
      let map = if order then map_preorder else map_postorder in
      incr_age map zipper
    end
    | 14 -> begin
      match animal_type, subtype, name with
      | Some a, Some s, Some n -> stream_set_age a s n zipper
      | _ -> failwith "Missing parameter for transaction: %d" trans
    end
    | _ -> failwith "Transaction %d is not implemented yet" trans
  end
  |> function
  | Ok zipper -> zipper
  | Error s -> p "Error: %s" s; zipper



let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Runs various transactions on the 'shelther' filestore"
    [%map_open
      let trans = flag "trans" (required int) ~doc:"[1-14] Run this transaction"
      and animal_type = flag "typ" ( optional int )
                  ~doc:"name of the type of animal"
      and sub_type = flag "subtyp" ( optional int )
                  ~doc:"subtype of the animal"
      and id = flag "id" ( optional int )
                  ~doc:"index of the animal in its sub type"
      and name = flag "name" (optional string)
                  ~doc:"name of the animal"
      and order = flag "pre" (no_arg)
                  ~doc:"any folds should use preorder traveral rather than postorder traversal "
      and debug = flag "debug" (no_arg)
                  ~doc:"should print the debugging statements"
      in

        run_txn shelter_spec "/shelter" ~f:(main trans animal_type sub_type id name order debug)
    ]
    |> Command.run

