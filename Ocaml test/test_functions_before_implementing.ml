(*
let dostuff tab condition func =
	match tab with (ls, x, y) ->
		let stuff ls condition func count =
			if count=x*Y -> tab else stuff 
		in stuff ls condition func 1
;;

x = dostuff [(1,1,1);(2,2,2)];;


if x*y= 
*)

let ( ! ) a = not a;;
let xor x y = (x && !y)||(!x && y);;
let ( % ) dividend divisor = dividend mod divisor;;

let columnHight, rowWidth= 5,5;;
let encode2D row column = ((column * columnHight) + row);;

let decode2D value = (value / columnHight, value % columnHight);;




let rec getNElement ls element =
	if element < 0 then failwith "Out of bounds";
	match ls with
	| [] -> failwith "Out of bounds"
	| hd::tl -> if element=0 then hd else getNElement tl (element-1)
;;

let rec setNElement ls element value=
	if element < 0 then failwith "Out of bounds";
	match ls with
	| [] -> failwith "Out of bounds"
	| hd::tl -> if element=0 then value::tl else hd::(setNElement tl (element-1) value)
;;

(* let forl func condition endvalue ls = *)
   let forl     condition  endvalue ls =
	let rec recursion ls i =
		if i <= endvalue 
			(* then (recursion (setNElement ls i true) (i+1))  *)
			then (
				if condition (ls, i, (1111111111,22222222) )
					then recursion (setNElement ls i true) (i+1)
					else recursion ls (i+1)
				)
			else ls
	in recursion ls 0
;;

type state_type = S | F | N | SF;; (* S = Startzustand; F = Finalzustand; N = Normaler Zustand *)
type dfa_transition = state_type * int * int * int;; 
type dfa_transition_table = dfa_transition list;; (* [dfa_transition,dfa_transition] *)
let candidateList  = [(S,1,1,2);(N,2,3,4);(F,3,3,2);(F,4,3,2)] ;; (* DEA 1 *)

let isFinal zustand = 
	match zustand with s -> s = F || s = SF
;;
let areDifferentState tpl =
	(* t1 und t2 sind tab1/2 *)
	let (ls, where, (t1, t2)) = tpl in
	let (x, y) = (decode2D where) in
	let (sx, _, _, _), (sy, _, _, _) = getNElement candidateList x, getNElement candidateList y in
	if (xor (isFinal sx) (isFinal sy)) then true else false
;;




(* let dostuff tab condition func = *)
let dostuff tab =
	match tab with (ls, x, y) -> 
	(* ((forl func condition (x*y) ls), x, y) *)
	((forl areDifferentState ((x*y)-1) ls), x, y)
	(* (ls, x, y) *)
;;

(* print_string "\n";; *)
(* print_int (getNElement (setNElement [1;2] 1 15) 1);; *)





let tabelle = ([false; false; false; false], 2, 2);;
let tabelle = dostuff tabelle;;

(* if ([1;1;1] = forl 2 [1;2;3]) then print_string "True" else print_string "False";; *)