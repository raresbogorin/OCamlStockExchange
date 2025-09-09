open Domain

let thread f = Domain.spawn f

let run n =
	let f = (fun () -> n + n) in
	let t = thread f in
	let result = Domain.join t in
	print_endline (string_of_int result)