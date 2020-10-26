open Core
open TxForest
open OldForest
open Result.Let_syntax


let specification : specification =
  DPair("x", PathExp(Name "a", File),
   DPair("y", PathExp(Name "a", File),
    PathExp(Var "x", File)))

let go () =
  set_debug ();
  let f z =
    Printf.printf "(0) "; print_with_newline z;
    let%bind z = down z in (* x *)
    Printf.printf "(1) "; print_with_newline z;
    let%bind z = next z in (* <y: "a"::File, x::File> *)
    Printf.printf "(2) "; print_with_newline z;
    let%bind z = down z in (* "a"::File *)
    Printf.printf "(3) "; print_with_newline z;
    let%bind z = down z in (* File *)
    Printf.printf "(4) "; print_with_newline z;
    let%bind z = put (FileRep "b") z in
    Printf.printf "(*) "; print_with_newline z;
    let%bind z = up z in (* "a"::File *)
    Printf.printf "(5) "; print_with_newline z;
    let%bind z = next z in (* x::File *)
    Printf.printf "(6) "; print_with_newline z;
    let%bind z = down z in (* x::File *)
    Printf.printf "(7) "; print_with_newline z;
    return z in
  run_txn_exn ~f specification "quux" ()

let () = go ()
