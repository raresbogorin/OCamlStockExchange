module FIFO = struct
	type 'a t = {
		head : 'a list;
		tail : 'a list
	}

	let empty = {head = [] ; tail = []}

	let push e q = {head = (e :: q.head) ; tail = q.tail}

	let rec pop q =
		match q.tail with
		| [] -> 
			if q.head = [] 
				then 
					(None, {head = [] ; tail = List.rev q.head}) 
				else 
					pop {head = [] ; tail = List.rev q.head}
		| x :: xs ->
			(Some x, {head = q.head ; tail = xs})

	let rec peek q = 
		match q.tail with
		| [] -> 
			if q.head = []
				then
					(None, q)
				else
					(peek {head = []; tail = List.rev q.head})
		| x :: xs -> 
			(Some x, q)
end