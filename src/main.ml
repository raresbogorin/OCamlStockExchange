open FIFO

type ticker = string

type stock = (float * ticker)

type order = Buy of (stock) | Sell of (stock)

type orderBook = {
	buys : stock FIFO.t list;
	sells : stock FIFO.t list;
}


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

let addOrder order ((orderBook) : orderBook) = 
	match order with
	| Buy (stock) -> 
		insertIntoBookBuys stock orderBook.buys []
	| Sell (stock) -> 
		insertIntoBookSells stock orderBook.sells []



let rec executeOrders (orderBook : orderBook) = 
	match orderBook.buys, orderBook.sells with
	| [], [] -> 
		(buys, sells)
	| o :: xs, [] -> 
		(buys, sells)
	| [], o :: xs -> 
		(buys, sells)
	| buySide :: restBuys, sellSide :: restSells -> 
		if buyPrice = sellPrice 
			then 
				(print_endline ("Executed order @ " ^ string_of_float buyPrice);
				executeOrders (restBuys, restSells))
			else 
				((restBuys, restSells) : orderBook)

let getEmpty = FIFO.empty