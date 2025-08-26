open FIFO

type ticker = string

type stock = (float * ticker)

type order = Buy of (stock) | Sell of (stock)

type orderBook = (stock list * stock list)


let rec insertIntoBookBuys ((p, t) : stock) orderBook result = 
	match orderBook with
	| [] -> 
		List.rev ((p, t) :: result) 
	| (price, name)::xs -> 
		if (price > p) 
			then 
				insertIntoBookBuys (p, t) xs ((price, name)::result) 
			else 
				List.append (List.rev ((p, t) :: (price, name) :: result)) xs

let rec insertIntoBookSells ((p, t) : stock) (orderBook : stock list) (result : stock list) = 
	match orderBook with
	| [] -> 
		List.rev ((p, t) :: result) 
	| (price, name)::xs -> 
		if (price < p) 
			then 
				insertIntoBookSells (p, t) xs ((price, name)::result) 
			else 
				List.append (List.rev ((p, t) :: (price, name) :: result)) xs

let addOrder order ((buys, sells) : orderBook) = 
	match order with
	| Buy (stock) -> 
		insertIntoBookBuys stock buys []
	| Sell (stock) -> 
		insertIntoBookSells stock sells []



let rec executeOrders ((buys, sells) : orderBook) = 
	match buys, sells with
	| [], [] -> 
		(buys, sells)
	| (p, n) :: xs, [] -> 
		(buys, sells)
	| [], (p, n) :: xs -> 
		(buys, sells)
	| (buyPrice, _) :: restBuys, (sellPrice, _) :: restSells -> 
		if buyPrice = sellPrice 
			then 
				(print_endline ("Executed order @ " ^ string_of_float buyPrice);
				executeOrders (restBuys, restSells))
			else 
				((restBuys, restSells) : orderBook)

let getEmpty = FIFO.empty