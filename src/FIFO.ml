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

	let compareFirst (a : 'a) (q : 'a t) (f : 'a -> 'a -> int) = 
		match (peek q) with
		| None, _ -> failwith "empty queue; cannot compare"
		| Some x, _ -> f a x

	let compareQueues (a : 'a t) (b : 'a t) (f : 'a -> 'a -> int) =
		match (peek a) , (peek b) with
		| (Some x , _) , (Some y, _) -> f x y
		| _ , _ -> failwith "cannot compare; queues might be empty"

	let isEmpty q =
		match q.head, q.tail with
		| [], [] -> true
		| _, _ -> false
end