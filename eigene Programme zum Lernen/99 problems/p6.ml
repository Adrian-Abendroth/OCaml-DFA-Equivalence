let rec rev = function
	| [] -> []
	| hd::tl -> (rev tl) @ [hd]
;;	

let rec is_palindrome ls = 
	match ls with
	| [] -> true
	| [_] -> true
	(* | hd::tl -> (hd=getHD (rev ls)) && (is_palindrome (rev (getTL tl))) *)
	| hd::tl -> 
		let _::tltl = (rev tl) in
		let rshd::_ = (rev ls) in
		(hd=rshd) && (is_palindrome tltl)
;;

let print_boolean bedingung =
	if bedingung
		then print_string "true"
		else print_string "false";;

print_boolean( is_palindrome []);;

