let listen ?(inet_addr = Unix.inet_addr_any) port =
  let listen_address = Unix.ADDR_INET (inet_addr, port) in

  let _ =
    Lwt.async (fun () ->
        let%lwt _server =
          Lwt_io.establish_server_with_client_socket listen_address
            (fun _ _ -> Lwt.return_unit)
        in
        Lwt.return_unit)
  in

  Lwt.wait ()

let listen_and_wait_forever ?(inet_addr = Unix.inet_addr_any) port =
  let forever, _ = listen ~inet_addr port in
  forever