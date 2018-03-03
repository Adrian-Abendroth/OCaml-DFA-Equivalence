let rec merge = function
	| [] -> []       (*(a,1);(b,1);(b,1)*)
	| [x] -> [x]
	| (a1, i1)::(a2, i2)::tl -> 
	if i1 = i2 
		then merge ((a1+a2, i1)::tl)
	else (a1, i1)::(merge ((a2, i2)::tl))
;;	
let rec encode = function
	| [] -> []
	| hd::tl -> merge((1, hd)::(encode tl))
;;

encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;