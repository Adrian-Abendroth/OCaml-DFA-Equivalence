let rec print_list x = 
match x with
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;

(*let rec makes liste counts value= 
	match counts with
		| 0 -> liste
		(* | _ -> liste @ [value] ; makes liste (counts-1) value	 *)
		| _ -> makes (value :: liste) (Counts-1) value		
in makes liste counts value;;*)

let rec makes x n = function 
    | [] -> if n = 0 then [] else x :: (makes x (n-1) [])
    | hd :: tl -> hd :: (makes x n tl)
    (* | _ -> failwith "egatives n!" *)


let a=[1;2];;
(* let b = makes a 3 5;;     (* => [1;2;5;5;5] *) *)
let b = makes 5 3 a;;     (* => [1;2;5;5;5] *)
print_list (b);;
