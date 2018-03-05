let x =
	let rec recursion i =
		if i<4
			then recursion (i+1)
			else i
	in recursion 0
;;
print_int x;;