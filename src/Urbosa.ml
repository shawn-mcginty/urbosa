let connection_handler handler_fn client_socket file_descr =
  let () = print_endline "----------\n-> Incoming socket event" in
  let () = Debug_utils.inspect_sockaddr client_socket in
  let () = Debug_utils.inspect_file_descr file_descr in
  let read_ch = Lwt_io.of_fd file_descr ~mode:Lwt_io.input in
  let _write_ch = Lwt_io.of_fd file_descr ~mode:Lwt_io.output in
  let read_st = Lwt_io.read_chars read_ch in
  let req = new Req.t read_st in
  let%lwt _ = req#init () in
  let () = print_endline (req#to_string ()) in
  let () = print_endline "----------" in
  handler_fn req

let listen ?(inet_addr = Unix.inet_addr_any) port handler_fn =
  let listen_address = Unix.ADDR_INET (inet_addr, port) in
  let req_handler = connection_handler handler_fn in

  let _ =
    Lwt.async (fun () ->
        let%lwt _server =
          Lwt_io.establish_server_with_client_socket listen_address req_handler
        in
        Lwt.return_unit)
  in

  Lwt.wait ()

let listen_and_wait_forever ?(inet_addr = Unix.inet_addr_any) port handler_fn =
  let forever, _ = listen ~inet_addr port handler_fn in
  forever
