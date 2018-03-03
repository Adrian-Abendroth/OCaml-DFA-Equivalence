(* ~~~~~~~~~~~~~~~~~~~~~~~~ Typ-Definition ~~~~~~~~~~~~~~~~~~~~~~~~ *)

type state_type = S | F | N | SF;; (* S = Startzustand; F = Finalzustand; N = Normaler Zustand *)

type dfa_transition = state_type * int * int * int;; 
type dfa_transition_table = dfa_transition list;; (* [dfa_transition,dfa_transition] *)
type candidates = dfa_transition_table * dfa_transition_table;;
type min_dfa_transition = state_type * int list * int list * int list;;
type min_dfa_transition_table = min_dfa_transition list;;
type equivalence_result = bool * min_dfa_transition_table;;

	type nfa_transition = state_type * int * int list * int list;;
	type nfa_transition_table = nfa_transition list;;



(* ~~~~~~~~~~~~~~~~~~ Allgemeine Funktionen ~~~~~~~~~~~~~~~~~~~ *)
let rec print_int_list ls = 
	match ls with
	| [] -> ()
	| hd::tl -> print_int hd; print_string ", "; print_int_list tl
;;

let rec print_dfa_transition (state_type, name, partner0, partner1) =
	(match state_type with
		| S ->  print_string " >"
		| F ->  print_string " *"
		| N ->  print_string "  "
		| SF -> print_string ">*" );
	print_int name; print_string " | ";
	print_int partner0; print_string " | ";
	print_int partner1; print_string "\n"

;;

let rec print_dfa_transition_table dfa_transition_table = 
	match dfa_transition_table with
	| [] -> ()
	| hd::tl -> print_dfa_transition hd; print_dfa_transition_table tl
;; 

let print_candidates (a, b) =
	print_dfa_transition_table a ; print_string "\n"; print_dfa_transition_table b
;;

let rec lenght ls =
	match ls with
	| [] -> 0  | [_] -> 1
	| hd::tl -> 1+(lenght tl)
;;

let square x = x*x;;

let rec contains element ls = 
	match ls with
	| hd::tl -> if (hd = element) then true else (contains element tl)
	| []   -> false
;;
(* ~~~~~~~~~~~~~~~~~~~~~~~~ Funktionen ~~~~~~~~~~~~~~~~~~~~~~~~ *)
let rec make ls count element = 
	match ls with
    | [] -> if count = 0 then [] else element :: (make [] (count-1) element)
    | hd::tl -> hd :: (make tl count element)
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Variabeln ~~~~~~~~~~~~~~~~~~~~~~~~ *)
let tabelle1  = [(S,1,1,2);(N,2,3,4);(F,3,3,2);(F,4,3,2)] ;; (* DEA 1 *)
let tabelle2  = [(S,5,6,7);(N,6,5,8);(N,7,9,9);(N,8,9,9);(F,9,9,8);(F,10,10,9)] ;; (* DEA 2 *)
let tablecan  = (tabelle1, tabelle2);;

(* ~~~~~~~~~~~~~~~~~~~~~~~~ Main-Programm ~~~~~~~~~~~~~~~~~~~~~~~~ *)

print_candidates tablecan;;
let (tab1, tab2) = tablecan;;
let filling_table = make [] (square ((lenght tab1) + (lenght tab2))) false;; (* true -> angekreuzt *)
