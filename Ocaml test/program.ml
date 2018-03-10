(* ~~~~~~~~~~~~~~~~~~~~~~~~ Typ-Definition ~~~~~~~~~~~~~~~~~~~~~~~~ *)


type state_type = S | F | N | SF;; (* S = Startzustand; F = Finalzustand; N = Normaler Zustand *)

type dfa_transition = state_type * int * int * int;; 
type dfa_transition_table = dfa_transition list;; 
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
         then( print_boolInt (getNElement ls i);
         print_string ("|" ^* (let (_, y) = decode2D (columnHeight, rowWidth) i in !(column-1 = y)));
         (if ((i+1) % column = 0)
            then print_string "\n"
         ); recursion (i+1));
   in recursion 0; print_string "\n"
;;

let rec print_int_list ls = 
   match ls with
   | [] -> ()
   | [x] -> print_int x
   | hd::tl -> print_int hd; print_string ", "; print_int_list tl
;;

let rec print_int_list_list ls = 
   match ls with
   | [] -> ()
   | [x] -> print_string "["; print_int_list x; print_string "]\n"
   | hd::tl -> print_string "["; print_int_list hd; print_string "];\n"; print_int_list_list tl;
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

let rec print_min_dfa_transition (state_type, name, partner0, partner1) =
   (match state_type with
      | S ->  print_string " >"
      | F ->  print_string " *"
      | N ->  print_string "  "
      | SF -> print_string ">*" );
   print_int_list name; print_string " | ";
   print_int_list partner0; print_string " | ";
   print_int_list partner1; print_string "\n"
;;

let rec print_dfa_transition_table dfa_transition_table = 
   match dfa_transition_table with
   | [] -> ()
   | hd::tl -> print_dfa_transition hd; print_dfa_transition_table tl
;; 

let rec print_min_dfa_transition_table min_dfa_transition_table = 
   match min_dfa_transition_table with
   | [] -> ()
   | hd::tl -> print_min_dfa_transition hd; print_min_dfa_transition_table tl
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
    | ls -> ls @ (make [] count element)
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
         if (knot = k) then (t0, t1) else (recursion tl)
      );
   in recursion candidateList
;;

let getStateByPoint candidateList knot =
   let rec recursion ls = 
      match ls with
      | [] -> failwith "Unknown knot"
      | hd::tl -> (
         let (s, k, _, _) = hd in
         if (knot = k) then s else (recursion tl)
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
   





let strike_out_element candidateList (columnHeight, rowWidth) ls element =
   let (x,y) = decode2D (columnHeight, rowWidth) element in
   let (_, _, a0, a1) = (getNElement candidateList y) in
   let (_, _, b0, b1) = (getNElement candidateList x) in
      (getNElement ls (encode2D (columnHeight, rowWidth) (getPositioninTable candidateList a0) (getPositioninTable candidateList b0))) 
   ||   (getNElement ls (encode2D (columnHeight, rowWidth) (getPositioninTable candidateList a1) (getPositioninTable candidateList b1)))
;;

let areDifferentState candidateList (columnHeight, rowWidth) where =
   let (x, y) = (decode2D (columnHeight, rowWidth) where) in
   let (sx, _, _, _) = getNElement candidateList x in
   let (sy, _, _, _) = getNElement candidateList y in
   (xor (isFinal sx) (isFinal sy))
;;


let strike_finals candidateList (columnHeight, rowWidth) endvalue ls =
   let rec recursion ls i =
      if i <= endvalue 
         then (
            if (areDifferentState candidateList (columnHeight, rowWidth) i)
               then recursion (setNElement ls i true) (i+1)
               else recursion ls (i+1)
            )
         else ls
   in recursion ls 0
;;

let aequivalenz_klasse candidateList (columnHeight, rowWidth) endvalue ls =
   let rec recursion i =
      if i <= endvalue 
         then(
            let (x,y) = decode2D (columnHeight, rowWidth) i in
            if (not (x=y))
               then (
                  if not (getNElement ls i) 
                  (* then (x,y)::(recursion (i+1)) *)
                     then (
                        let (_, a, _, _) = (getNElement candidateList x) in
                        let (_, b, _, _) = (getNElement candidateList y) in
                        (a, b)::(recursion (i+1))
                     )
                     else recursion (i+1)
               )
               else recursion (i+1)
         )
         else []
   in recursion 0
;;

let print_int_2_tuple a =
   let (x,y) = a in
   print_int x; print_string ", "; print_int y
;;

let rec print_aquivalenzklasse a =
   match a with
   | [] -> ()
   | hd::tl -> print_string "["; print_int_2_tuple hd; print_string "]\n"; print_aquivalenzklasse tl
;;

let rec get_aequivalenztuple element ls =
   match ls with
   | [] -> []
   | hd::tl -> 
      let (x,y) = hd in
      if x = element 
         then (
            y::(get_aequivalenztuple element tl)
         )
         else (get_aequivalenztuple element tl)
;;

let rec streiche_aequi ls aequi =
   match ls with
   | [] -> []
   | hd::tl -> 
      let (x, y) = hd in
      if (contains x aequi)
         then streiche_aequi tl aequi
         else hd::streiche_aequi tl aequi
;;

let rec aequivalenz_klasse_bilden ls =
   match ls with
   |[] -> []
   | hd::tl -> 
      let (a, _) = hd in 
      let aequi = a::(get_aequivalenztuple a ls) in (* [1;2;3]*) (*0::[3;4]*) 
      aequi :: (aequivalenz_klasse_bilden (streiche_aequi ls aequi))
;;

let areStartSame candidateList (columnHeight, rowWidth) where =
   let (x, y) = (decode2D (columnHeight, rowWidth) where) in
   let (sx, _, _, _) = getNElement candidateList x in
   let (sy, _, _, _) = getNElement candidateList y in
   (xor (isFinal sx) (isFinal sy))
;;

let rec string_of_int_list ls =
   match ls with 
   | [] -> ""
   | [x] -> (string_of_int x)
   | hd::tl -> ((string_of_int hd) ^ ", " ^ (string_of_int_list tl))
;;

let rec string_of_int_list_list ls =
   match ls with
   | [] -> ""
   | [x] -> string_of_int_list x
   | hd::tl -> "[" ^ ((string_of_int_list hd) ^ ", " ^ (string_of_int_list_list tl)) ^ "]"
;;

let rec getListbyElement element ls = 
   match ls with
   | [] ->  failwith ("Element not in List " ^ (string_of_int element) ^ "[" ^ (string_of_int_list_list ls) ^ "]")
   | hd::tl -> 
      if (contains element hd) 
         then hd 
         else getListbyElement element tl
;;

let rec determineStart candidateList ls =
   match ls with
   | [] -> false
   | hd::tl -> (isStart (getStateByPoint candidateList hd)) || (determineStart candidateList tl)
;;

let rec determineFinal candidateList ls =
   match ls with
   | [] -> false
   | hd::tl -> (isFinal (getStateByPoint candidateList hd)) || (determineFinal candidateList tl)
;;

let determineState candidateList ls =
   let start = determineStart candidateList ls in 
   let final = determineFinal candidateList ls in 
   
   if final 
      then 
         if start
            then SF
            else F
      else
         if start
            then S
            else N
;;

let rec dostuff candidateList ls =
   let rec recursion lss = 
      match lss with 
      | [] -> []
      | hd::tl -> (
         match hd with
         | [] -> failwith "Empty List inside of Aequivalentliste"
         | hdhd::_ -> (
            let (hd0, hd1) = getTransitionByPoint candidateList hdhd in
            (* print_min_dfa_transition (((determineState candidateList hd), hd, (getListbyElement hd0 ls), (getListbyElement hd1 ls))); *)
            (* print_int_list (getListbyElement hd0 ls); print_string " | "; print_int_list (getListbyElement hd1 ls); print_string " | "; *)
            (* print_int_list_list ls; *)
            ((determineState candidateList hd), hd, (getListbyElement hd0 ls), (getListbyElement hd0 ls))::(recursion tl)
            )
            (* (F,[3;9],[3;9],[2;7;8]) *)
         )
   in recursion ls
;;

let rec getStartList candidateList =
   match candidateList with
   | [] -> []
   | hd::tl -> 
      let (s, k, _, _) = hd in
      if isStart s 
         then k::(getStartList tl)
         else (getStartList tl)
;;

let rec containsMultiple ls =
   match ls with
   | [] -> false 
   | hd::tl -> (contains hd tl) || (containsMultiple tl)
;;

let strike_out candidateList (columnHeight, rowWidth) endvalue ls =
   let rec recursion ls i =
      if i <= endvalue 
         then (
            if (not (getNElement ls i))
               then recursion (setNElement ls i (strike_out_element candidateList (columnHeight, rowWidth) ls i)) (i+1)
               else recursion ls (i+1)
            )
         else ls
   in recursion ls 0
;;

let rec getListOfPointersNames ls = 
   match ls with
   | [] -> []
   | hd::tl -> (
      let (_, k, _, _) = (getNElement ls 0) in
      k::(getListOfPointersNames tl)
   )
;;

let minimize dfa_transition_table = (*TODO: nicht erreichbare Zustände und gleiche rausschmeißen *)
   (* checks if transition table is empty: *)
   if (dfa_transition_table = []) 
      then failwith "One of the given dfa transition table's was empty"
      else (
         let zustaende=getListOfPointersNames dfa_transition_table in
         (* checks if there are multiple points with the same name: *)
         if (containsMultiple zustaende)
            then failwith "Pointer name assignend multiple times"
            else (
               let rec doTransitionPointsExist ls = (
                  match ls with
                  | [] -> true
                  | hd::tl -> (
                     let (_, _, a0, a1) = (getNElement ls 0) in
                     (contains a0 zustaende && contains a1 zustaende) && (doTransitionPointsExist tl)
                  )
                  (* checks if there is a invalid transition (meaning transition to a point that does not exist) *)
               ) in if not (doTransitionPointsExist dfa_transition_table) 
                  then failwith "Pointed to invalid Point" 
                  (* checks if there is exactly one starting point: *)
                  else (if (lenght(getStartList dfa_transition_table) != 1) 
                     then failwith "Wrong Amount of Start Points"
                     else 
                        dfa_transition_table
                  )
            )
      )
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Variabeln ~~~~~~~~~~~~~~~~~~~~~~~~ *)
let tabelle1  = [(S,1,1,2);(N,2,3,4);(F,3,3,2);(F,4,3,2)] ;; (* DEA 1 *)
let tabelle2  = [(S,5,6,7);(N,6,5,8);(N,7,9,9);(N,8,9,9);(F,9,9,8);(F,10,10,9)] ;; (* DEA 2 *)

let tabelle1min  = [(S,1,1,2);(N,2,3,3);(F,3,3,2)] ;; (* DEA 1 selbst minimiert*)
let tabelle2min  = [(S,5,6,7);(N,6,5,8);(N,7,9,9);(N,8,9,9);(F,9,9,8)] ;; (* DEA 2 selbst minimiert*)

let tabelle3  = [(S,1,1,2);(N,2,3,4);(F,3,3,2);(F,4,3,2)] ;; (* DEA 1 *)
let tabelle4  = [(S,5,6,7);(N,6,5,8);(F,7,5,9);(N,8,9,9);(F,9,6,8);(F,10,10,9);(N,1,5,6)] ;; (* DEA 2 *)

let candidates  = (minimize tabelle1, minimize tabelle2);; (*checks for errors in Inputs (like: empty, multipletimes same name, points to invalid point) and deletes unreachable and duplicated points*)

let (tab1, tab2) = candidates;;
let candidateList = tab1 @ tab2;;

if (containsMultiple (getListOfPointersNames candidateList)) then failwith "Pointer name assignend multiple times";; (*checks if there are points in tab1 that have the same name as in tab2*)

let rowWidth, columnHeight = (lenght tab1)+(lenght tab2), (lenght tab1)+(lenght tab2);;

(* ~~~~~~~~~~~~~~~~~~~~~~~~ Main-Programm ~~~~~~~~~~~~~~~~~~~~~~~~ *)

print_candidates candidates;;
print_string "\n";;

let filling_table = make [] (rowWidth * columnHeight) false;; (* true -> angekreuzt *)
(* let tabelle = (filling_table, rowWidth, columnHeight);; *)

print_boolean_table (columnHeight, rowWidth) filling_table rowWidth columnHeight;; 


let filling_table = strike_finals candidateList (columnHeight, rowWidth) ((rowWidth * columnHeight)-1) filling_table;;
print_boolean_table (columnHeight, rowWidth) filling_table rowWidth columnHeight;;


let filling_table =
   let rec recursion ft = (
      let new_filling_table = (strike_out candidateList (columnHeight, rowWidth) ((rowWidth * columnHeight)-1) ft) in
      
      if (ft = new_filling_table)
         then (ft)
         else (print_boolean_table (columnHeight, rowWidth) new_filling_table rowWidth columnHeight; recursion new_filling_table)
   );
   in recursion filling_table
;;

let aequivalenz_tuple = (aequivalenz_klasse candidateList (columnHeight, rowWidth) ((rowWidth * columnHeight)-1) filling_table);;
print_aquivalenzklasse aequivalenz_tuple;;

print_string "\n\n";;
let aequivalenzklasse = (aequivalenz_klasse_bilden aequivalenz_tuple);;
(* print_int_list_list aequivalenzklasse;; *)


print_min_dfa_transition_table (dostuff candidateList aequivalenzklasse);;
(* print_min_dfa_transition_table (dostuff candidateList [[1; 5; 6];[3; 9];[2; 7; 8]]);; *)
(*
(S/SF, k, _, _)


ROW = getPositioninTable LISTe1 Knoten 
Collumn = getPositioninTable LISTe2 Knoten 
if getNElement filling_table (encode2D (columnHeight, rowWidth) ROW COLUMN)


*)