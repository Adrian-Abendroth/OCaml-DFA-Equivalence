let f x= 
    print_string "Start ";
    (match x with
	    | 1 -> print_string "1"
    	| 2 -> print_int 2
    	| _ -> ());
    print_string " End\n\n"
;;

f 1;;
f 2;;
f 3;;