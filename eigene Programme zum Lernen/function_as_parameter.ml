let f x = print_int x;;
let f2 x = print_string x;;

let func i f f2 =
	match i with
	| 1 -> f 1
	| 2 -> f2 "zwei"
	| _ -> print_string "wrong"
;;

func 2 f f2;;
	