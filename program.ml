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
(** Not operator *)
let ( ! ) a = not a;;

(** Xor operator *)
let xor x y = (x && !y)||(!x && y);;

(** Modulo operator *)
let ( % ) dividend divisor = dividend mod divisor;;
let ( ^* ) str b =
   match b with
   | true -> str
   | false -> ""
;;

(** Gets position from a list
    Input:
        list,
        element (position)
    Output:
        element in position

*)
let rec getNElement ls element =
   if element < 0 then failwith "Out of bounds";
   match ls with
   | [] -> failwith "Out of bounds"
   | hd::tl -> if element=0 then hd else getNElement tl (element-1)
;;

(** Sets value for an element in list from its position
    Input:
        list,
        element (position),
        value
    Output:
        list with new value in position element

*)
let rec setNElement ls element value=
   if element < 0 then failwith "Out of bounds";
   match ls with
   | [] -> failwith "Out of bounds"
   | hd::tl -> if element=0
      then value::tl
      else hd::(setNElement tl (element-1) value)
;;

(** Prints boolean as int 0 or 1
    Input:
        expression
    Output:
        string
*)
let print_boolInt expression  =
   match expression with
   | true -> print_int 1
   | false -> print_int 0
;;

(** Prints boolean as string
    Input:
        expression
    Output:
        string
*)
let print_bool expression  =
   match expression with
   | true -> print_string "true"
   | false -> print_string "false"
;;

(** Encodes Platzhalter a to a 2d List
    Input:
        tuple of (columnHeight, rowWidth),
        row,
        column
    Output:
        List
*)
let encode2D (columnHeight, rowWidth) row column = ((column * columnHeight) + row);;

(** Decodes a 2d List to a Platzhalter
    Input:
        tuple of (columnHeight, rowWidth),
        row,
        column
    Output:
        string
*)
let decode2D (columnHeight, rowWidth) value = (value / columnHeight, value % columnHeight);;

(** Prints table with boolean values to see which states are aquivalent
    Input:
        tuple of (columnHeight, rowWidth),
        list,
        row,
        column
    Output:
        table of booleans
*)
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

(** Prints integer list
    Input:
        list
    Output:
        integerlist as string
*)
let rec print_int_list ls =
   match ls with
   | [] -> ()
   | [x] -> print_int x
   | hd::tl -> print_int hd; print_string ", "; print_int_list tl
;;

(** Prints integer list list
    Input:
        list
    Output:
        integer list list as string
*)
let rec print_int_list_list ls =
   match ls with
   | [] -> ()
   | [x] -> print_string "["; print_int_list x; print_string "]\n"
   | hd::tl -> print_string "["; print_int_list hd; print_string "];\n"; print_int_list_list tl;
;;

(** Prints dfa_transitions
    Input:
        quadtuple (state_type, name, partner0, partner1)
    Output:
        dfa_transitions as string table
*)
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

(** Prints min_dfa_transition
    Input:
        quadtuple (state_type, name, partner0, partner1)
    Output:
        min_dfa_transition as string
*)
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

(** Prints dfa_transition as table
    Input:
        quadtuple (state_type, name, partner0, partner1)
    Output:
        min_dfa_transition as string table
*)
let rec print_dfa_transition_table dfa_transition_table =
   match dfa_transition_table with
   | [] -> ()
   | hd::tl -> print_dfa_transition hd; print_dfa_transition_table tl
;;

(** Prints min_dfa_transition_table
    Input:
        min_dfa_transition_table
    Output:
        min_dfa_transition  as string table
*)
let rec print_min_dfa_transition_table min_dfa_transition_table =
   match min_dfa_transition_table with
   | [] -> ()
   | hd::tl -> print_min_dfa_transition hd; print_min_dfa_transition_table tl
;;

(** Prints candidates
    Input:
        candidates as tuple (a,b)
    Output:
        canditates as string table
*)
let print_candidates (a, b) =
   print_dfa_transition_table a ; print_string "\n"; print_dfa_transition_table b
;;

(** Determine length of list
    Input:
        list
    Output:
        length of list
*)
let rec lenght ls =
   match ls with
   | [] -> 0
   | [_] -> 1
   | hd::tl -> 1+(lenght tl)
;;

(** Determine if list contains element
    Input:
        element,
        list
    Output:
        boolean
*)
let rec contains element ls =
   match ls with
   | hd::tl -> if (hd = element) then true else (contains element tl)
   | [] -> false
;;

(** Determine how often a element is in a list
    Input:
        element,
        list
    Output:
        integer
*)
let rec containsAmount element ls =
   match ls with
   | hd::tl -> if (hd = element)
      then (containsAmount element tl) +1
      else (containsAmount element tl)
   | []   -> 0
;;


(* ~~~~~~~~~~~~~~~~~~~~~~~~ Funktionen ~~~~~~~~~~~~~~~~~~~~~~~~ *)
(** Determine how often a element is in a list
    Input:
        list,
        count,
        element
    Output:
        list of booleans
*)
(** Makes a boolean false table for table-filling algorithm
    Input:
        ls,
        count,
        element
    Output:
        boolean
*)
let rec make ls count element =
   match ls with
    | [] -> if count = 0 then [] else element :: (make [] (count-1) element)
    | ls -> ls @ (make [] count element)
;;

(** Determine if a state is a start-state
    Input:
        state
    Output:
        boolean
*)
let isStart zustand =
   match zustand with s -> s = S || s = SF
;;

(** Determine if a state is a final-state
    Input:
        state
    Output:
        boolean
*)
let isFinal zustand =
   match zustand with s -> s = F || s = SF
;;

(** Shows where a state is going to transitions
    Input:
        canditateList,
        knot
    Output:
        gets transitions of knot
*)
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

(** Get state of a knot
    Input:
        canditateList,
        knot
    Output:
        state of a knot
*)
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

(** Get Position in table of a knot
    Input:
        canditateList,
        knot
    Output:
        Position of knot
*)
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


(** Function for table-filling-algorithm. Determines if the table don't have an aquivalence class
    Input:
        candidateList (columnHeight, rowWidth),
        ls,
        element
    Output:
        table of booleans
*)
let strike_out_element candidateList (columnHeight, rowWidth) ls element =
   let (x,y) = decode2D (columnHeight, rowWidth) element in
   let (_, _, a0, a1) = (getNElement candidateList y) in
   let (_, _, b0, b1) = (getNElement candidateList x) in
      (getNElement ls (encode2D (columnHeight, rowWidth) (getPositioninTable candidateList a0) (getPositioninTable candidateList b0)))
   ||   (getNElement ls (encode2D (columnHeight, rowWidth) (getPositioninTable candidateList a1) (getPositioninTable candidateList b1)))
;;

(** Checks if a states of boolean-table are different
    Input:
        candidateList (columnHeight, rowWidth),
        where
    Output:
        boolean
*)
let areDifferentState candidateList (columnHeight, rowWidth) where =
   let (x, y) = (decode2D (columnHeight, rowWidth) where) in
   let (sx, _, _, _) = getNElement candidateList x in
   let (sy, _, _, _) = getNElement candidateList y in
   (xor (isFinal sx) (isFinal sy))
;;

(** Marks state in table of booleans when they are final and start
    Input:
        candidateList (columnHeight, rowWidth)
        endvalue.
        ls

    Output:
    table of booleans

*)
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

(** Computes aequivalence classes
    Input:
        candidateList (columnHeight, rowWidth)
        endvalue.
        ls

    Output:
    aequivalenz classes as a table of string

*)
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

(** Prints tuple as string
    Input:
        tuple

    Output:
        String of tuple

*)
let print_int_2_tuple a =
   let (x,y) = a in
   print_int x; print_string ", "; print_int y
;;

(** Prints aquivalence classes
    Input:
        aquivalence-class

    Output:
        String of table

*)
let rec print_aquivalenzklasse a =
   match a with
   | [] -> ()
   | hd::tl -> print_string "["; print_int_2_tuple hd; print_string "]\n"; print_aquivalenzklasse tl
;;

(** Prints aquivalence classes
    Input:
        aquivalence-class

    Output:
        String of table

*)
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

(** Function to combine tuple of aquivalence-classes to aquivalence-class
    Input:
        ls,
        aquivalence-classes

    Output:
        table of booleans

*)
let rec streiche_aequi ls aequi =
   match ls with
   | [] -> []
   | hd::tl ->
      let (x, y) = hd in
      if (contains x aequi)
         then streiche_aequi tl aequi
         else hd::streiche_aequi tl aequi
;;

(** Builds aquivalence-classes
    Input:
        ls

    Output:
        aquivalence-classes

*)
let rec aequivalenz_klasse_bilden ls =
   match ls with
   |[] -> []
   | hd::tl ->
      let (a, _) = hd in
      let aequi = a::(get_aequivalenztuple a ls) in (* [1;2;3]*) (*0::[3;4]*)
      aequi :: (aequivalenz_klasse_bilden (streiche_aequi ls aequi))
;;

(** Checks if states are same
    Input:
        candidateList (columnHeight, rowWidth),
        where

    Output:
        boolean

*)
let areStartSame candidateList (columnHeight, rowWidth) where =
   let (x, y) = (decode2D (columnHeight, rowWidth) where) in
   let (sx, _, _, _) = getNElement candidateList x in
   let (sy, _, _, _) = getNElement candidateList y in
   (xor (isFinal sx) (isFinal sy))
;;

(** Prints integer list as string
    Input:
        ls

    Output:
        string of  int list

*)
let rec string_of_int_list ls =
   match ls with
   | [] -> ""
   | [x] -> (string_of_int x)
   | hd::tl -> ((string_of_int hd) ^ ", " ^ (string_of_int_list tl))
;;
(** Prints integer list list as string
    Input:
        ls

    Output:
        integer list list as string

*)
let rec string_of_int_list_list ls =
   match ls with
   | [] -> ""
   | [x] -> string_of_int_list x
   | hd::tl -> "[" ^ ((string_of_int_list hd) ^ ", " ^ (string_of_int_list_list tl)) ^ "]"
;;

(** Gets List by Element
    Input:
        element,
        ls

    Output:
        list in position (element)

*)
let rec getListbyElement element ls =
   match ls with
   | [] ->  failwith ("Element not in List " ^ (string_of_int element) ^ "[" ^ (string_of_int_list_list ls) ^ "]")
   | hd::tl ->
      if (contains element hd)
         then hd
         else getListbyElement element tl
;;

(** Determine if a state is a start-type (SF || S)
    Input:
        canditateList
        ls

    Output:
        boolean

*)
let rec determineStart candidateList ls =
   match ls with
   | [] -> false
   | hd::tl -> (isStart (getStateByPoint candidateList hd)) || (determineStart candidateList tl)
;;

(** Determine if a state is a final-type (SF || F)
    Input:
        canditateList
        ls

    Output:
        boolean

*)
let rec determineFinal candidateList ls =
   match ls with
   | [] -> false
   | hd::tl -> (isFinal (getStateByPoint candidateList hd)) || (determineFinal candidateList tl)
;;

(** Determine state-type of canditate list and list
    Input:
        canditateList
        ls

    Output:
        state-type (SF || S || F || N)

*)
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

(** Gets startList
    Input:
        canditateList

    Output:


*)
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
