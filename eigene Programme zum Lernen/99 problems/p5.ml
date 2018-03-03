let rec rev = function
	| [] -> []
	| hd::tl -> (rev tl) @ [hd]
;;	
rev ["a" ; "b" ; "c"];;