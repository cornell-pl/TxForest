(* TODOS:
 * - Implement cp and mv
 *)

open Core
open Result
open Forest
open ForestIntf
open TxForestCoreExn

[%%txforest {|
    univ = directory {
      files is [x :: file | x <- matches GL "*"];
      dirs is [x :: univ | x <- matches GL "*"];
    }
|}]


let () = Printexc.record_backtrace true

let norm_write s =
  let open Out_channel in
  output_string stdout s;
  flush stdout

let write = ref norm_write

let write_endline s = !write (s ^ "\n")
let write_space s = !write (s ^ "  ")

let write_format format = Core.Printf.ksprintf write_endline format



let write_fetch fr =
  match fr with
    | Ok fr -> write_endline (show_fetch_rep fr)
    | Error u -> write_endline u


(* Helper functions *)



let memory_table = Int.Table.create ()


let make_prompt t = "> "

let read_and_prompt n t =
  match Int.Table.find memory_table n with
  | Some input -> input
  | None ->
    !write (make_prompt t);
    let input =
      In_channel.input_line ~fix_win_eol:true In_channel.stdin
      |> Option.value_exn ~message:"read: Failed to receive input"
    in
    Int.Table.set ~key:n ~data:input memory_table;
    input

let mk_error format = Core.Printf.ksprintf (fun s -> Error s) format

(* Main logic *)

let rec fscommands =
  let open Result in
  let open Result.Let_syntax in
  let mal_exp = mk_error "Malformed expression" in
  (*TODO: this obviously needs to actually do the thing*)
  let goto p t = Ok t in
  let up t =
    match TxForestCore.up t with
    | Error _ -> TxForestCore.out t
    | t -> t
  in
  let up_dir' t =
    match TxForestCore.fetch t with
      | Ok(PairRep "dir'") -> up t
      | _ -> Ok t
  in
  let down_dir' t =
    match TxForestCore.fetch t with
      | Ok(PairRep "dir'") -> TxForestCore.into_pair t >>= TxForestCore.next
      | _ -> Ok t
  in
  let down t =
    let%bind temp_res =
      match TxForestCore.fetch t with
      | Ok(PathRep _) -> TxForestCore.down t
      | Ok(PairRep _) -> TxForestCore.into_pair t
      | Ok(CompRep _) -> TxForestCore.into_comp t
      | Ok(OptRep _) -> TxForestCore.into_opt t
      | Error e -> Error e
      | _ -> Ok t
    in
      down_dir' temp_res
  in
  let next t =
    match TxForestCore.next t with
    | Ok temp_res -> down_dir' temp_res
    | Error _ -> TxForestCore.out t >>= TxForestCore.next >>= down_dir'
  in
  let prev t =TxForestCore.prev t >>= down_dir' in
  let up t = up t >>= up_dir' in
  let rec pair_lst (t,acc) =
    match TxForestCore.fetch t with
    | Ok(PairRep s) -> begin
      let%bind t' = TxForestCore.into_pair t >>= TxForestCore.next in
      let acc' = s :: acc in
      let%bind (t'', acc'') = pair_lst (t', acc') in
      let%bind t''' = TxForestCore.prev t'' >>= TxForestCore.out in
        Ok (t''', acc'')
    end
    | _ -> Ok (t, acc)
  in
  let touch (u: string) t =
    let cur_dir = match TxForestCore.fetch t with
    | Ok (DirRep s)
    | Ok (CompRep s) -> s
    | Ok (PairRep _) -> begin
      match pair_lst (t, []) with
      | Ok (_, s) -> String.Set.of_list s
      | _ -> failwith "not in a directory"
    end
    | _ -> failwith "not in a directory"
    in
    let new_dir = String.Set.add cur_dir u in
      TxForestCore.store_dir new_dir t
  in
  let arg0 ~f (t, reader, writer) = function
    | [] -> f t >>= (fun t -> TxForestCore.commit (t, reader, writer) )
    | _ -> mal_exp
  in
  let arg1 ~f (t, reader, writer) = function
    | hd :: [] -> f hd t >>= (fun t -> TxForestCore.commit (t, reader, writer) )
    | _ -> mal_exp
  in
  let argE ~f (t, reader, writer) = function
    | [] -> f t >>= (fun t -> TxForestCore.commit (t, reader, writer))
    | hd :: [] -> goto hd t >>= f >>= (fun t -> TxForestCore.commit (t, reader, writer))
    | _ -> mal_exp
  in
  let fetch = argE ~f:(fun t -> TxForestCore.fetch t |> write_fetch; return t) in
  let lst = argE ~f:(fun t ->
    (match TxForestCore.fetch t with
    | Ok (DirRep s)
    | Ok (CompRep s) -> String.Set.iter s ~f:(fun u -> write_space u); write_endline ""
    | Ok (PairRep _) -> begin
      match pair_lst (t, []) with
      | Ok (_, s) -> List.iter (List.rev s) ~f:(fun u -> write_space u); write_endline ""
      | _ -> ()
    end
    | _ -> ()
    ); return t) in
  let cat = argE ~f:(fun t ->
    (match TxForestCore.fetch t with
    | Ok(FileRep s) -> write_endline s
    | _ -> ()
    ); return t) in
  [
    "cd", arg1 ~f:TxForestCore.goto_name;
    "ls", lst;
    "cat", cat;
    "fetch", fetch;
    "prev", arg0 ~f:prev;
    "next", arg0 ~f:next;
    "up", arg0 ~f:up;
    "down", arg0 ~f:down;
    "update", arg1 ~f:TxForestCore.store_file;
    "touch", arg1 ~f:touch;
    "mkdir", arg0 ~f:(TxForestCore.store_dir String.Set.empty);
(*     "rm", arg1 ~f:(remove_child); *)
    "quit", arg0 ~f:return;
    "help", help;
    "into_pair", arg0 ~f:TxForestCore.into_pair;
    "into_comp", arg0 ~f:TxForestCore.into_comp;
    "into_opt", arg0 ~f:TxForestCore.into_opt;
    "out", arg0 ~f:TxForestCore.out;
    ]

and help t _ =
  "Commands: " ^ (String.concat ~sep:", " (List.map ~f:fst fscommands))
  |> write_endline;
  Result.return t


and perform_command input t =
  match String.split ~on:' ' input with
  | [] -> mk_error "Got empty input: %s" input
  | command :: rest ->
    match List.Assoc.find ~equal:String.equal fscommands command with
    | None -> mk_error "Command `%s` does not exist" command
    | Some(f) -> f t rest

let rec shell_loop n t =
  let input = read_and_prompt n t in
  if String.is_prefix ~prefix:"quit" input
  then (write := Core.ignore; Ok t)
  else
    match perform_command input t with
    | Error s ->
      write_format "Error: %s" s;
      shell_loop (n+1) t
    | Ok t -> shell_loop (n+1) t

let start_client p ~port ~host () =
  (* block (
    fun () ->
      let open Async in
      Tcp.connect
        (Core.Host_and_port.create ~host ~port
        |> Tcp.Where_to_connect.of_host_and_port)
      >>= fun (_,reader,writer)
      ->
        write_struct writer s_spec;
        Reader.read_marshal reader
        >>| function
        | `Eof -> failwith "create: No response from server"
        | `Ok (Error e) -> failwith "create: Returned an error: %s" e
        | `Ok (Ok _) -> shell_loop 0 (reader,writer)
  ) *)
  write_endline "Forest Client";
  TxForestCore.create univ_spec p ~port ~host ()

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Start filesystem client (MAKE SURE TO START SERVER FIRST!)"
    [%map_open
      let path =
      flag "-path" (required string)
      ~doc:"path for the client"
     and port =
      flag "-port" (optional_with_default 8765 int)
      ~doc:"Port to listen on or connect to (if shard) (default 8765)"
     and host =
       flag "-host" (optional_with_default "localhost" string)
       ~doc:"Host for shard to connect to (default 'localhost')"
     in
     fun () -> (start_client path ~port ~host () |> shell_loop 0); ()
    ] |> Command.run