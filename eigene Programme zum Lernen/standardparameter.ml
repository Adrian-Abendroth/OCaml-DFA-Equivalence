let func p1 ?(p2 = "Standard") () = 
	print_string p1; print_string " "; print_string p2;;


func "String 1 with" ~p2:"String 2" ();;
print_string "\n";;
func "String 1 only" ();;