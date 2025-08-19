type tickr = string


type order = (char * int * tickr)



let rec execute_trades trades = match trades with
	| [] -> ()
	| trade :: xs -> print_endline trade; execute_trades xs


