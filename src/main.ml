open FIFO

type ticker = string

type stock = {
	price : float;
	ticker : ticker;
}

type order = Buy of (stock) | Sell of (stock)

type orderBook = {
	buys : stock FIFO.t list;
	sells : stock FIFO.t list;
}


let stockComparator a b = 
	if a.price < b.price 
		then -1
		else
			if a.price = b.price
				then 
					0
				else
					+1

let rec insertIntoBuys (stock : stock) (orderBook : stock FIFO.t list) (result : stock FIFO.t list) = 
	match orderBook with
	| [] -> 
		List.rev ((FIFO.push stock FIFO.empty) :: result) 
	| x::xs -> 
		if FIFO.compareFirst stock x stockComparator > 0 
			then 
				List.append (List.rev result) ((FIFO.push stock FIFO.empty) :: orderBook)
			else
				if FIFO.compareFirst stock x stockComparator = 0
					then
						List.append (List.rev result) ((FIFO.push stock x) :: xs)
					else
						insertIntoBuys stock xs (x::result)



let rec insertIntoSells (stock : stock) (orderBook : stock FIFO.t list) (result : stock FIFO.t list) = 
	match orderBook with
	| [] -> 
		List.rev ((FIFO.push stock FIFO.empty) :: result) 
	| x::xs -> 
		if FIFO.compareFirst stock x stockComparator < 0 
			then 
				List.append (List.rev result) ((FIFO.push stock FIFO.empty) :: orderBook)
			else
				if FIFO.compareFirst stock x stockComparator = 0
					then
						List.append (List.rev result) ((FIFO.push stock x) :: xs)
					else
						insertIntoSells stock xs (x::result)

let addOrder order ((orderBook) : orderBook) = 
	match order with
	| Buy (stock) -> 
		{buys = insertIntoBuys stock orderBook.buys []; sells = orderBook.sells}
	| Sell (stock) -> 
		{buys = orderBook.buys; sells = insertIntoSells stock orderBook.sells []}

let rec matchOrders buys sells =
	match FIFO.isEmpty buys, FIFO.isEmpty sells with
	| true, _ | _, true -> buys, sells
	| _, _ -> 
		let (buyOrder, bq) = (match FIFO.pop buys with | Some x, q -> x, q) in
		let (sellOrder, sq) = (match FIFO.pop sells with | Some x, q -> x, q) in
		print_endline ("Executed order @ " ^ string_of_float sellOrder.price);
		matchOrders bq sq




let rec executeOrders (orderBook : orderBook) = 
	match orderBook.buys, orderBook.sells with
	| _, [] | [], _ -> orderBook
	| buySide :: restBuys, sellSide :: restSells ->
		let res = FIFO.compareQueues buySide sellSide stockComparator in
		if res >= 0
			then 
				let (bq, sq) = matchOrders buySide sellSide in
				match FIFO.isEmpty bq, FIFO.isEmpty sq with
				| false, false -> {buys = (bq :: restBuys); sells = (sq :: restSells)}
				| true,  false -> executeOrders {buys = restBuys; sells = (sq :: restSells)}
				| false,  true -> executeOrders {buys = (bq :: restBuys); sells = restSells}
				| true, true -> executeOrders {buys = restBuys; sells = restSells}
			else 
				orderBook


let rec insertAll orders (orderBook : orderBook) = 
	match orders with
	| [] -> orderBook
	| x :: xs -> insertAll xs (addOrder x orderBook)

let toStock ticker price =
	try
		let p = Float.of_string price in
		{price = p ; ticker = ticker}	
	with
	| _ -> failwith "error at toStock"

let toOrder t stock =
	match t with
	| "buy" -> Buy (stock)
	| "sell" -> Sell (stock)
	| _ -> failwith "error at toOrder"

let check line =
	let tokenized = String.split_on_char ';' line in
	match tokenized with
	| t :: ticker :: price :: _ :: [] ->
		let stock = toStock ticker price in
		toOrder t stock
	| _ -> raise Parsing.Parse_error

let rec parseBook stream result =
	try
		let line = input_line stream in
		let order = check line in
		parseBook stream (order :: result)
	with
	| End_of_file -> close_in stream; result
	| Parsing.Parse_error -> close_in stream; failwith "Encountered invalid line while reading file"


let readBook path =
	let stream = open_in path in
	let orders = parseBook stream [] in
	insertAll orders {buys = [] ; sells = [] } 	

let main = 
	let book = readBook "../orders.txt"  in
	executeOrders book