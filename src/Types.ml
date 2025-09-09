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