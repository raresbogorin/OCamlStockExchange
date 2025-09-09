open Queue
open Mutex
open Condition

module ThreadSafeQueue = struct
	type 'a t = {
		q : 'a Queue.t ;
		m : Mutex.t ; 
		c : Condition.t
	}

	let create () = { q = Queue.create () ; m = Mutex.create () ; c = Condition.create()}

	let push e q = 
		Mutex.lock q.m;
		Queue.push e q.q;
		Mutex.unlock q.m;
		Condition.signal q.c


	let pop q =
		Mutex.lock q.m;
		while Queue.is_empty q.q do
			Condition.wait q.c q.m
		done;
		let e = Queue.pop q.q in
		Mutex.unlock q.m;
		e
end