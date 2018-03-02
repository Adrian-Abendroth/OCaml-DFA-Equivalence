let odp a ?(b = "Standard") () = print_string a; print_string " "; print_string b;;


odp "String 1 with" ~b:"String 2" ();;
print_string "\n";;
odp "String 1 only" ();;