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

class t ch_stream = object
	val stream : char Lwt_stream.t = ch_stream
	val mutable state = Parsing
	val mutable meth = GET
	val mutable http_version = ""

	method init () =
		let read_until ch =
			let%lwt chars = Lwt_stream.get_while (fun c -> c <> ch) stream in
			match List.length chars with
			| 0 -> Lwt.return ""
			| len ->
				let new_str = String.init len (fun i -> List.nth chars i) in
				Lwt.return new_str in
		
		let%lwt meth_str = read_until ' ' in
		meth <- meth_of_str meth_str;
		let%lwt () = Lwt_stream.njunk 3 stream in
		let%lwt http_ver_str = read_until '\n' in
		http_version <- http_ver_str;
		state <- Ready;
		Lwt.return state
	
	method to_string () =
		(str_of_meth meth) ^ " / " ^ http_version
end
