let rec rev = function
	| [] -> []
	| hd::tl -> (rev tl) @ [hd]
;;	

let rec getHD = function
	| [] -> ()
	| hd::tl -> hd
;;

let rec getTL = function
	| [] -> ()
	| hd::tl -> tl
;;

let rec is_palindrome ls = 
	match ls with
	| [] -> true
	| [_] -> true
	| hd::tl -> hd==getHD (rev ls) && is_palindrome (rev (getTL tl))
;;


is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;