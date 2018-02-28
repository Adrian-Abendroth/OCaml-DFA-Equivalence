(* ~~~~~~~~~~~~~~~~~~~~~~~~ Typ-Definition ~~~~~~~~~~~~~~~~~~~~~~~~ *)
type state_type = S | F | K;; (* S = Startzustand; F = Finalzustand K = Kein Start- oder Endzustand *)
type dfa_transition = state_type * int * int * int ;;

type nfa_transition_list = state_type * int list * int list * int list ;;
type dfa_transition_table = nfa_transition_list ;; 

type candidates = dfa_transition_table * dfa_transition_table ;;

type min_dfa_transition = state_type * int list * int list * int list ;;
type min_dfa_transition_table = min_dfa_transition list ;;

type equivalence_result = bool * min_dfa_transition_table ;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Funktionen ~~~~~~~~~~~~~~~~~~~~~~~~ *)

(** Vereinigung der Automaten *)
let joinTable table_one table_two =
    let finished_table = table_two @ table_one in
    finished_table
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Variabeln ~~~~~~~~~~~~~~~~~~~~~~~~ *)
let tabelle_1 = [(S,1,1,2);(K,2,3,4);(F,3,3,2);(F,4,3,2)] ;; (* DEA 1 *)
let tabelle_2 = [(S,5,6,7);(K,6,5,8);(K,7,9,9);(K,8,9,9);(F,9,9,8);(F,10,10,9)] ;; (* DEA 2 *)

let candidates =  (tabelle_1, tabelle_2) ;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Main-Programm ~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* Variabeln *)


let main  =

    joinTable tabelle_1 tabelle_2 ;;

    print_string "Willkommen zu diesem super Programm"; (* newline *)
    print_string "\n"; (* newline *)




;;
