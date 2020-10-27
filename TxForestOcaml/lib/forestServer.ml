open Async
open Rawforest
open Utils

type command =
  | Commit of log
  | CommitFinished [@@deriving show]

module Server = struct

  let print_fetch fr =
    match fr with
      | Ok fr -> info_message "S" (show_fetch_rep fr)
      | Error u -> info_message "S" u


  let eval_command c writer =
    match c with
    | Commit l -> begin
      let result = TxForestGlobal.commit l () in
        write_struct writer result;
        result
    end
    | CommitFinished -> begin
      let result = TxForestGlobal.finish_commit () in
        write_struct writer result;
        result
    end

  let read_and_run reader ~f =
    Reader.read_marshal reader
    >>= function
    | `Eof -> return ()
    | `Ok command -> f command


  let rec run_loop reader writer =
    read_and_run reader
    ~f:(fun command ->
        d "got command %s" (show_command command);
        Async.Deferred.Result.return (eval_command command writer)
        >>= fun result -> run_loop reader writer
    )

  let start_server ~port =
    set_debug ();
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun _ reader writer ->
        let debug = info_message ~id:(Writer.id writer) "C" in
        debug "Connected to server";
        write_struct writer (mk_ok ());
        run_loop reader writer
        >>| fun _ -> debug "Disconnected from server"
      )
    >>= fun _ -> Deferred.never ()
  end

let () =
  let open Command.Let_syntax in
  Command.async
    ~summary:"Start filesystem server"
    [%map_open
      let port =
        flag "-port" (optional_with_default 8765 int)
        ~doc:"Port to listen on or connect to (if shard) (default 8765)"
      in
      (fun () ->
        info_message "S" "Starting server";
        Server.start_server ~port
      )
    ] |> Command.run