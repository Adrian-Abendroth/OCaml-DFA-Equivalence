type 'a node =
    | One of 'a 
    | Many of 'a node list;;

let rec flatten ls =
	match ls with
	| [] -> []
	| One hd :: tl-> hd::(flatten tl)
	| Many hd :: tl -> (flatten hd)@(flatten tl)
;;
flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;