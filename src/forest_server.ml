open Async
(* open Zfs.ZFSUtils *)
(* open Filesystem *)


(*TODO: make this inter act with the filesystem  rather than zfs version*)
(*TODO: make a shell to interat with this and test it*)

module Server (Filesystem) = struct
  open ZFS

  let msg_of_context = msg_of_context ~get_zipper

  let rec run_loop reader writer context =
    let run_loop = run_loop reader writer in
    read_marshal_and_run ~init:context reader
    ~f:(fun command ->
        add_command command context |> apply_command
        >>= fun nContext ->
          Writer.write_marshal ~flags:[] writer (msg_of_context nContext);
          match nContext with
          | Ok context -> run_loop context
          | _          -> run_loop context
    )

  let start_server ~port path =
    set_debug ();
    FS.init path;
    Tcp.Server.create
      ~on_handler_error:`Raise
      (Tcp.Where_to_listen.of_port port)
      (fun _ reader writer ->
        let debug = info_message ~id:(Writer.id writer) "C" in
        debug "Connected to server";
        let context = init_context () in
        let msg = msg_of_context (mk_ok context) in
        write_struct writer msg;
        run_loop reader writer context
        >>| fun _ -> debug "Disconnected from server"
        )
    >>= fun _ -> Deferred.never ()

end

module TxServer = Server(Zfs.ZFS.TxZFS)
module NotTxServer = Server(Zfs.ZFS.ZFS)

let () =
  Command.async_spec
    ~summary:"Start filesystem server at PATH"
    Command.Spec.(
      empty
      +> flag "-port" (optional_with_default 8765 int)
        ~doc:"Port to listen on or connect to (if shard) (default 8765)"
      +> flag "-notx" (no_arg) ~doc:"Runs the non-transactional version of the
      server"
      +> anon ("path" %: string)
    )
    (fun port notx path () ->
      info_message "S" "Starting server";
      if notx
      then NotTxServer.start_server ~port path
      else TxServer.start_server ~port path)
  |> Command.run