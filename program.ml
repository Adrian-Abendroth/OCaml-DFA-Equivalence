(* ~~~~~~~~~~~~~~~~~~~~~~~~ Typ-Definition ~~~~~~~~~~~~~~~~~~~~~~~~ *)

type state_type = S | F | N | SF;; (* S = Startzustand; F = Finalzustand K = Kein Start- oder Endzustand *)

type nfa_transition = state_type * int * int list * int list;;
type nfa_transition_table = nfa_transition list;;

type dfa_transition = state_type * int * int * int;; 
type dfa_transition_table = nfa_transition list;;
type candidates = dfa_transition_table * dfa_transition_table;;
type min_dfa_transition = state_type * int list * int list * int list;;
type min_dfa_transition_table = min_dfa_transition list;;
type equivalence_result = bool * min_dfa_transition_table;;



(* ~~~~~~~~~~~~~~~~~~~~~~~~ Funktionen ~~~~~~~~~~~~~~~~~~~~~~~~ *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Variabeln ~~~~~~~~~~~~~~~~~~~~~~~~ *)
let dfa_transition_table tabelle_1 = [(S,1,1,2);(K,2,3,4);(F,3,3,2);(F,4,3,2)] ;; (* DEA 1 *)
let dfa_transition_table tabelle_2 = [(S,5,6,7);(K,6,5,8);(K,7,9,9);(K,8,9,9);(F,9,9,8);(F,10,10,9)] ;; (* DEA 2 *)


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Main-Programm ~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* Variabeln *)


let main  =


;;
