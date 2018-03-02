let rec len ls =
	match ls with
		| [] -> 0
		| [_] -> 1
		| hd::tl -> 1+ len tl
;;
print_int (len [ "a" ; "b" ; "c"]);;