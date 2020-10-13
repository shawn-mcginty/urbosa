type state = | Ready | Parsing | Bad of exn

type meth = | GET | POST | DELETE | Other of string

let meth_of_str str =
	match str with
	| "GET" -> GET
	| "POST" -> POST
	| "DELETE" -> DELETE
	| other -> Other other

let str_of_meth meth =
	match meth with
	| GET -> "GET"
	| POST -> "POST"
	| DELETE -> "DELETE"
	| Other s -> s

let read_until ch stream =
	let () = print_endline ("read until '" ^ (Char.escaped ch) ^ "'") in
	let%lwt chars = Lwt_stream.get_while (fun c -> c <> ch) stream in
	let () = print_endline "done reading." in
	let%lwt () = Lwt_stream.junk stream in
	match List.length chars with
	| 0 -> Lwt.return ""
	| len ->
		let new_str = String.init len (fun i -> List.nth chars i) in
		Lwt.return new_str

(* non-tail recursive, there's probably a better way to do this *)
let rec parse_headers ?headers:(hds = []) stream =
	let () = print_endline "parse_headers" in
	if Lwt_stream.is_closed stream then
		let () = print_endline "is_closed" in
		Lwt.return hds
	else
		let%lwt next = Lwt_stream.peek stream in
		match next with
		| None | Some '\n' | Some '\r' ->
			let () = print_endline "found none or newline" in
			Lwt.return hds
		| Some x ->
			let () = print_endline ("I just round found->'" ^ (Char.escaped x) ^ "'") in
			let%lwt next_hd_key = read_until ':' stream in
			let%lwt next_hd_val = read_until '\n' stream in
			parse_headers ~headers:(hds @ [(next_hd_key, next_hd_val)]) stream

class t ch_stream = object
	val stream : char Lwt_stream.t = ch_stream
	val mutable state = Parsing
	val mutable meth = GET
	val mutable http_version = ""
	val mutable target = "/"
	val mutable headers = []

	method init () =
		let%lwt meth_str = read_until ' ' stream in
		meth <- meth_of_str meth_str;
		let%lwt raw_target = read_until ' ' stream in
		target <- raw_target;
		let%lwt http_ver_str = read_until '\n' stream in
		http_version <- http_ver_str;
		state <- Ready;
		let%lwt hds = parse_headers stream in
		let () = print_endline "finished parsing those headers" in
		headers <- hds;
		Lwt.return state
	
	method to_string () =
		(str_of_meth meth) ^ " " ^ target ^ " " ^ http_version  ^ (List.fold_left (fun s (k, v) -> s ^ "\n" ^ k ^ ": " ^ v) "" headers)
end
