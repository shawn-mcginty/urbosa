let inspect_file_descr fdesc =
  let state_msg = match Lwt_unix.state fdesc with
  | Opened -> "Opened"
  | Closed -> "Closed"
  | Aborted exn -> "Aborted -> " ^ Printexc.to_string exn in
  let () = print_endline "Inspecting file_desc:" in
  let () = print_endline ("  State: " ^ state_msg) in
  let () = print_endline ("  Readable: " ^ (string_of_bool (Lwt_unix.readable fdesc))) in
  let () = print_endline ("  Writable: " ^ (string_of_bool (Lwt_unix.writable fdesc))) in
  let () = print_endline "\n" in
  ()

let inspect_sockaddr sockaddr =
  let () = print_endline "Inspecting sockaddr:" in
  let () = match sockaddr with
  | Lwt_unix.ADDR_UNIX addr -> print_endline ("  ADDR_UNIX: " ^ addr)
  | Lwt_unix.ADDR_INET (inet_addr, port) ->
    let () = print_endline ("  INET_ADDR: " ^ (Unix.string_of_inet_addr inet_addr)) in
    print_endline ("  PORT: " ^ (string_of_int port)) in
  let () = print_endline "\n" in
  ()