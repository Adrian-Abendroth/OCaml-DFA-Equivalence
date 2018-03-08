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
let ( ! ) a = not a;;
let xor x y = (x && !y)||(!x && y);;
let ( % ) dividend divisor = dividend mod divisor;;
let ( ^* ) str b =
   match b with
   | true -> str
   | false -> ""
;;


let rec getNElement (columnHeight, rowWidth) ls element =
   if element < 0 then failwith "Out of bounds";
   match ls with
   | [] -> failwith "Out of bounds"
   | hd::tl -> if element=0 then hd else getNElement (columnHeight, rowWidth) tl (element-1)
;;

let rec setNElement ls element value=
   if element < 0 then failwith "Out of bounds";
   match ls with
   | [] -> failwith "Out of bounds"
   | hd::tl -> if element=0 
      then value::tl 
      else hd::(setNElement tl (element-1) value)
;;

let print_boolInt expression  =
   match expression with
   | true -> print_int 1  
   | false -> print_int 0
;;

let print_bool expression  =
   match expression with
   | true -> print_string "true"  
   | false -> print_string "false"  
;;

let encode2D (columnHeight, rowWidth) row column = ((column * columnHeight) + row);;

let decode2D (columnHeight, rowWidth) value = (value / columnHeight, value % columnHeight);;

let rec print_boolean_table (columnHeight, rowWidth) ls row column =
   let rec recursion i =
      if i < (row * column) 
         then( print_boolInt (getNElement (columnHeight, rowWidth) ls i);
         print_string ("|" ^* (let (_, y) = decode2D (columnHeight, rowWidth) i in !(column-1 = y)));
         (if ((i+1) % column = 0)
            then print_string "\n"
         ); recursion (i+1));
   in recursion 0; print_string "\n"
;;

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
   | [] -> 0  
   | [_] -> 1
   | hd::tl -> 1+(lenght tl)
;;

let rec contains element ls = 
   match ls with
   | hd::tl -> if (hd = element) then true else (contains element tl)
   | [] -> false
;;

let rec containsAmount element ls =
   match ls with
   | hd::tl -> if (hd = element) 
      then (containsAmount element tl) +1
      else (containsAmount element tl)
   | []   -> 0
;;   






(* ~~~~~~~~~~~~~~~~~~~~~~~~ Funktionen ~~~~~~~~~~~~~~~~~~~~~~~~ *)
let rec make ls count element = 
   match ls with
    | [] -> if count = 0 then [] else element :: (make [] (count-1) element)
    | hd::tl -> hd :: (make tl count element)
;;

let isStart zustand = 
   match zustand with s -> s = S || s = SF
;;

let isFinal zustand = 
   match zustand with s -> s = F || s = SF
;;


let getTransitionByPoint candidateList knot =
   let rec recursion ls = 
      match ls with
      | [] -> failwith "Unknown knot"
      | hd::tl -> (
         let (_, k, t0, t1) = hd in
         if (knot = k) then ((t0, t1)) else (recursion tl)
      );
   in recursion candidateList
;;
   
let getPositioninTable candidateList knot = 
   let rec recursion ls i = 
      match ls with
      | [] -> failwith ("Unknown knot: " ^ (string_of_int knot))
      | hd::tl -> (
         let (_, k, _, _) = hd in
         if (knot = k) then i else (recursion tl (i+1))
      );
   in recursion candidateList 0
;;
   







let forl candidateList (columnHeight, rowWidth) func condition reversecondition endvalue ls =
   let rec recursion ls i =
      if i <= endvalue 
         then (
            if (if reversecondition then (not (condition candidateList (columnHeight, rowWidth) ls i)) else (condition candidateList (columnHeight, rowWidth) ls i))
               then recursion (setNElement ls i (func candidateList (columnHeight, rowWidth) ls i)) (i+1)
               else recursion ls (i+1)
            )
         else ls
   in recursion ls 0
;;

let beTrue _ _ _ _ = 
   true
;;

let streiche candidateList (columnHeight, rowWidth) ls element =
   let (x,y) = decode2D (columnHeight, rowWidth) element in
   let (_, a, a0, a1) = (getNElement (columnHeight, rowWidth) candidateList y) in
   let (_, b, b0, b1) = (getNElement (columnHeight, rowWidth) candidateList x) in
      (getNElement (columnHeight, rowWidth) ls (encode2D (columnHeight, rowWidth) (getPositioninTable candidateList a0) (getPositioninTable candidateList b0))) 
   ||   (getNElement (columnHeight, rowWidth) ls (encode2D (columnHeight, rowWidth) (getPositioninTable candidateList a1) (getPositioninTable candidateList b1)))
;;

let areDifferentState candidateList (columnHeight, rowWidth) ls where =
   let (x, y) = (decode2D (columnHeight, rowWidth) where) in
   let (sx, _, _, _), (sy, _, _, _) = getNElement (columnHeight, rowWidth) candidateList x, getNElement (columnHeight, rowWidth) candidateList y in
   (xor (isFinal sx) (isFinal sy))
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Variabeln ~~~~~~~~~~~~~~~~~~~~~~~~ *)
let tabelle1  = [(S,1,1,2);(N,2,3,4);(F,3,3,2);(F,4,3,2)] ;; (* DEA 1 *)
let tabelle2  = [(S,5,6,7);(N,6,5,8);(N,7,9,9);(N,8,9,9);(F,9,9,8);(F,10,10,9)] ;; (* DEA 2 *)
let tabelle1min  = [(S,1,1,2);(N,2,3,3);(F,3,3,2)] ;; (* DEA 1 *)
let tabelle2min  = [(S,5,6,7);(N,6,5,8);(N,7,9,9);(N,8,9,9);(F,9,9,8)] ;; (* DEA 2 *)
let candidates  = (tabelle1min, tabelle2min);;

let (tab1, tab2) = candidates;;
let candidateList = tab1 @ tab2;;
let rowWidth, columnHeight = (lenght tab1)+(lenght tab2), (lenght tab1)+(lenght tab2);;

(* ~~~~~~~~~~~~~~~~~~~~~~~~ Main-Programm ~~~~~~~~~~~~~~~~~~~~~~~~ *)

print_candidates candidates;;
print_string "\n";;

let filling_table = make [] (rowWidth * columnHeight) false;; (* true -> angekreuzt *)
(* let tabelle = (filling_table, rowWidth, columnHeight);; *)

print_boolean_table (columnHeight, rowWidth) filling_table rowWidth columnHeight;; 

let filling_table = forl candidateList (columnHeight, rowWidth) beTrue areDifferentState false ((rowWidth * columnHeight)-1) filling_table;;
print_boolean_table (columnHeight, rowWidth) filling_table rowWidth columnHeight;;

(* let filling_table = forl streiche getNElement true ((rowWidth * columnHeight)-1) filling_table;; *)
(* print_boolean_table (columnHeight, rowWidth) filling_table rowWidth columnHeight;; *)

let forl candidateList (columnHeight, rowWidth) func condition reversecondition endvalue ls =
   let rec recursion ls i =
      if i <= endvalue 
         then (
            if (if reversecondition then (not (condition candidateList (columnHeight, rowWidth) ls i)) else (condition candidateList (columnHeight, rowWidth) ls i))
               then recursion (setNElement ls i (func candidateList (columnHeight, rowWidth) ls i)) (i+1)
               else recursion ls (i+1)
            )
         else ls
   in recursion ls 0
;;

let streichen candidateList (columnHeight, rowWidth) endvalue ls =
   let rec recursion ls i =
      if i <= endvalue 
         then (
            if (not (getNElement (columnHeight, rowWidth) ls i))
            
               then recursion (setNElement ls i (streiche candidateList (columnHeight, rowWidth) ls i)) (i+1)
               else recursion ls (i+1)
            )
         else ls
   in recursion ls 0
;;

let filling_table =
   let rec recursion ft = (
      (* print_boolean_table (columnHeight, rowWidth) ft rowWidth columnHeight; *)
      let new_filling_table = (streichen candidateList (columnHeight, rowWidth) ((rowWidth * columnHeight)-1) ft) in
      
      if (ft = new_filling_table)
         then (ft)
         else (print_boolean_table (columnHeight, rowWidth) new_filling_table rowWidth columnHeight; recursion new_filling_table)
         (* else (recursion new_filling_table) *)
   );
   in recursion filling_table
;;
(*
let allaequivalenzklassen =
   let rec recursion =
      
   in recursion
;; *)
(* print_boolean_table (columnHeight, rowWidth) filling_table rowWidth columnHeight;; *)


let ausgabae =
   match (containsAmount false filling_table) with
   | i when i = rowWidth -> (false, [])
   | i when i > rowWidth -> ( true,
[]




)


   | i -> failwith ("Less falses then expected (" ^ string_of_int i ^ " instead of at least " ^ string_of_int rowWidth ^ ")")
;;