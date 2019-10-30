(* TODOS:
 * - Implement cp and mv
 *)

open Core
open Result
open Forest
open ForestIntf



open ForestCoreExn
(* Helper functions *)

[%%txforest {|

  s = [x :: file | x <- matches GL "*.txt"]

|}]

open TxForestCore

let norm_write s =
  let open Out_channel in
  output_string stdout s;
  flush stdout

let memory_table = Int.Table.create ()
let write = ref norm_write

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

let write_endline s = !write (s ^ "\n")
let write_format format = Core.Printf.ksprintf write_endline format

let mk_error format = Core.Printf.ksprintf (fun s -> Error s) format

(* Main logic *)

let rec fscommands =
  let open Result in
  let mal_exp = mk_error "Malformed expression" in
  let print_node n = show_fetch_rep n |> write_endline
  in
  (*TODO: this obviously needs to actually do the thing*)
  let goto p t = Ok t in
  let arg0 ~f t = function
    | [] -> f t
    | _ -> mal_exp
  in
  let arg1 ~f t = function
    | hd :: [] -> f hd t
    | _ -> mal_exp
  in
  let argE ~f t = function
    | [] -> f t
    | hd :: [] -> goto hd t >>= f
    | _ -> mal_exp
  in
  let fetch = argE ~f:(fun t -> fetch t |> print_node; return t) in
  [
    "cd", arg1 ~f:goto;
    "ls", fetch;
    "cat", fetch;
    "fetch", fetch;
    "update", arg1 ~f:store_file;
    "prev", arg0 ~f:prev;
    "next", arg0 ~f:next;
    "up", arg0 ~f:up;
    "down", arg0 ~f:down;
    "touch", arg0 ~f:(store_file "");
    "mkdir", arg0 ~f:(store_dir String.Set.empty);
(*     "rm", arg1 ~f:(remove_child); *)
    "quit", arg0 ~f:return;
    "help", help;
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

let start_client ~port ~host () =
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
  create s_spec ~port ~host

let () =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Start filesystem client (MAKE SURE TO START SERVER FIRST!)"
    [%map_open
     let port =
      flag "-port" (optional_with_default 8765 int)
      ~doc:"Port to listen on or connect to (if shard) (default 8765)"
     and host =
       flag "-host" (optional_with_default "localhost" string)
       ~doc:"Host for shard to connect to (default 'localhost')"
     in
     fun () -> (start_client ~port ~host () |> shell_loop 0); ()
    ] |> Command.run