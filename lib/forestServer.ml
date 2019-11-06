open Async
open Rawforest
open EvalForest
open Utils



let write_struct = Writer.write_marshal ~flags:[]

type command =
  | Forest of forest_command
  | Fetch
  | Commit

module Server = struct

  let print_fetch fr =
    match fr with
      | Ok fr -> info_message "S" (show_fetch_rep fr)
      | Error u -> info_message "S" u




  let eval_command c context writer =
    match c with
    | Forest fc -> begin
      match eval_forest_command fc context with
      | Ok context' -> write_struct writer (Ok ()); context'
      | Error e -> write_struct writer (Error e); context
    end
    | Fetch -> begin
      let fr = fetch context in
        print_fetch fr;
        write_struct writer (let open Core.Result in fr >>| writable_of_fetch);
        context
    end
    | Commit -> begin
      match commit_log context with
      | Ok context' -> write_struct writer (Ok ()); context'
      | Error e -> write_struct writer (Error e); context
    end

  let read_and_run reader ~init ~f =
    Reader.read_marshal reader
    >>= function
    | `Eof -> return init
    | `Ok command -> f command

  let rec read_spec id reader =
    let debug = info_message ~id:id "C" in
    debug "Reading spec";
    Reader.read_marshal reader
    >>= function
    | `Eof -> read_spec id reader
    | `Ok s -> begin
      debug "Spec read";
      return (Forest.Spec.name_to_spec s)
    end


  let rec run_loop reader writer context =
    let run_loop = run_loop reader writer in
    read_and_run ~init:context reader
    ~f:(fun command ->
        Async.Deferred.Result.return (eval_command command context writer)
        >>= fun nContext ->
          match nContext with
          | Ok context -> run_loop context
          | _          -> run_loop context
    )

  let start_server ~port path =
    set_debug ();
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun _ reader writer ->
        let debug = info_message ~id:(Writer.id writer) "C" in
        debug "Connected to server";
        let spec = read_spec (Writer.id writer) reader in
        let context = spec >>| (fun s -> EvalForest.create s ~p:path ()) in
        context >>| (fun c -> write_struct writer (Ok ()));
        context >>= run_loop reader writer
        >>| fun context -> commit_log context; debug "Disconnected from server"
        )
    >>= fun _ -> Deferred.never ()

end

let () =
  Command.async_spec
    ~summary:"Start filesystem server at PATH"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 8765 int)
        ~doc:"Port to listen on or connect to (if shard) (default 8765)"
      +> anon ("path" %: string)
    )
    (fun port path () ->
      info_message "S" "Starting server";
      Server.start_server ~port path
    )
  |> Command.run