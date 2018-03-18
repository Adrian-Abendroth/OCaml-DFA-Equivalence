(* ~~~~~~~~~~~~~~~~~~~~~~~~ Typ-Definitions ~~~~~~~~~~~~~~~~~~~~~~~~ *)

type state_type = S | F | N | SF;; (* S = Startzustand; F = Finalzustand; N = Normaler Zustand *)

type dfa_transition = state_type * int * int * int;;
type dfa_transition_table = dfa_transition list;;
type candidates = dfa_transition_table * dfa_transition_table;;
type min_dfa_transition = state_type * int list * int list * int list;;
type min_dfa_transition_table = min_dfa_transition list;;
type equivalence_result = bool * min_dfa_transition_table;;




(* ~~~~~~~~~~~~~~~~~~ Generell Functions ~~~~~~~~~~~~~~~~~~~ *)

(** Not operator *)
let ( ! ) a = not a;;

(** Xor operator *)
let xor x y = (x && !y)||(!x && y);;

(** Modulo operator *)
let ( % ) dividend divisor = dividend mod divisor;;

(** String Operator *)
let ( ^* ) str b =
   match b with
   | true -> str
   | false -> ""
;;

(** Gets position from a list

    Input:
        list,
        position

    Output:
        element in position

*)
let rec getNElement ls position =
   if position < 0 then failwith "Out of bounds";
   match ls with
   | [] -> failwith "Out of bounds"
   | hd::tl -> if position = 0 then hd else getNElement tl (position - 1)
;;

(** Sets value for an element in list from its position

    Input:
        list,
        position,
        value

    Output:
        list with new value in position element

*)
let rec setNElement ls position value=
   if position < 0 then failwith "Out of bounds";
   match ls with
   | [] -> failwith "Out of bounds"
   | hd::tl -> if position = 0
      then value::tl
      else hd::(setNElement tl (position - 1) value)
;;

(** Encodes an Array Position to a List position

    Input:
        columnHeight,
        rowWidth
        row (Array x position), 
        column (Array y position)

    Output:
        Integer (list position)
*)
let encode2D (columnHeight, rowWidth) row column = 
   ((column * columnHeight) + row)
;;

(** Decodes a List Position to an Array position

    Input:
        columnHeight,
        rowWidth,
        value (list position)

    Output:
        Tuple for row (Array x position) and column (Array y position)
*)
let decode2D (columnHeight, rowWidth) value =
   (value / columnHeight, value % columnHeight)
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
   | hd::tl -> 1 + (lenght tl)
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
   | hd::tl -> if (hd = element) 
      then true 
      else (contains element tl)
   | [] -> false
;;

(** Determine how often an element is in a list

    Input:
        element,
        list
    Output:
        integer
*)
let rec containsAmount element ls =
   match ls with
   | hd::tl -> if (hd = element)
      then (containsAmount element tl) + 1
      else (containsAmount element tl)
   | [] -> 0
;;




(* ~~~~~~~~~~~~~~~~~~ Print Functions ~~~~~~~~~~~~~~~~~~~ *)

(** Prints boolean as int 0 or 1

    Input:
        boolean
    Output:
        unit (print)
*)
let print_boolInt expression =
   match expression with
   | true -> print_int 1
   | false -> print_int 0
;;

(** Prints boolean as string

    Input:
        boolean
    Output:
        unit (print)
*)
let print_bool expression =
   match expression with
   | true -> print_string "true"
   | false -> print_string "false"
;;

(** Prints table with boolean values to see which states are equivalent

    Input:
        tuple of (columnHeight, rowWidth),
        list,
        row,
        column

    Output:
        unit (print table of booleans)
*)
let rec print_boolean_table (columnHeight, rowWidth) ls row column =
   let rec recursion i =
      if i < (row * column)
         then( 
            print_boolInt (getNElement ls i);
            print_string ("|" ^* (let (_, y) = decode2D (columnHeight, rowWidth) i in !(column-1 = y)));
            if ((i+1) % column = 0)
               then print_string "\n";
            recursion (i+1)
         );
   in recursion 0;
   print_string "\n"
;;

(** Prints integer list

    Input:
        list

    Output:
        unit (integerlist as string=
*)
let rec print_int_list ls =
   match ls with
   | [] -> ()
   | [x] -> print_int x
   | hd::tl -> print_int hd; print_string ", "; print_int_list tl
;;

(** Prints integer list list

    Input:
        list of int list

    Output:
        unit (integer list list as string)
*)
let rec print_int_list_list ls =
   match ls with
   | [] -> ()
   | [x] -> 
      print_string "[";
      print_int_list x;
      print_string "]\n"
   | hd::tl -> 
      print_string "[";
      print_int_list hd;
      print_string "];\n";
      print_int_list_list tl;
;;

(** Prints dfa_transitions

    Input:
        quadtuple (state_type, name, partner0, partner1)

    Output:
        unit (dfa_transitions as string table)
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
        unit (min_dfa_transition as string)
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
        unit (min_dfa_transition as string table)
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
        unit (min_dfa_transition  as string table)
*)
let rec print_min_dfa_transition_table min_dfa_transition_table =
   match min_dfa_transition_table with
   | [] -> ()
   | hd::tl -> print_min_dfa_transition hd; print_min_dfa_transition_table tl
;;

(** Prints equivalence_result

    Input:
        equivalence_result

    Output:
        unit (equivalence_result as tuple of boolean and min_dfa_transition_table)
*)
let print_equivalence_result equivalence_result =
   let (booleanvalue, min_dfa_transition_table) = equivalence_result in
      print_bool booleanvalue; print_string "\n\n";
      print_min_dfa_transition_table min_dfa_transition_table
;;

(** Prints candidates

    Input:
        candidates as tuple (a,b)

    Output:
        unit (canditates as string table)
*)
let print_candidates (a, b) =
   print_dfa_transition_table a; print_string "\n"; print_dfa_transition_table b
;;


(** Prints tuple

    Input:
        tuple
     
    Output:
        unit (print)

*)
let print_int_2_tuple a =
   let (x,y) = a in
      print_int x; print_string ", "; print_int y
;;


(** Prints equivalence classes

    Input:
        equivalence-class

    Output:
        unit(String of table)

*)
let rec print_aquivalenzklasse a =
   match a with
   | [] -> ()
   | hd::tl -> 
      print_string "[";
      print_int_2_tuple hd;
      print_string "]\n";
      print_aquivalenzklasse tl
;;

(** returns string of integer list

    Input:
        ls

    Output:
        string of int list

*)
let rec string_of_int_list ls =
   match ls with
   | [] -> ""
   | [x] -> (string_of_int x)
   | hd::tl -> ((string_of_int hd) ^ ", " ^ (string_of_int_list tl))
;;

(** returns string of integer list list
    Input:
        ls

    Output:
        string of integer list list

*)
let rec string_of_int_list_list ls =
   match ls with
   | [] -> ""
   | [x] -> string_of_int_list x
   | hd::tl -> "[" ^ ((string_of_int_list hd) ^ ", " ^ (string_of_int_list_list tl)) ^ "]"
;;




(* ~~~~~~~~~~~~~~~~~~~~~~~~ Functions ~~~~~~~~~~~~~~~~~~~~~~~~ *)

(** Creates a table (concatenation with given table) of n times a specific element

    Input:
        ls,
        count,
        element

    Output:
        list
    
    Example:
        [1] 5 false
        ->
        [1; false; false; false; false; false]
*)
let rec make ls count element =
   match ls with
    | [] -> 
      if count = 0 
         then []
         else element :: (make [] (count-1) element)
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

(** Shows where a state is going to transitions to

    Input:
        canditateList,
        knot

    Output:
        gets transitions of knot
        
    Example:
        [(S, 0, 1, 2);...] 0
        ->
        (1, 2)
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

(** Shows point which has that exact transition

    Input:
        candidateList,
        transitiontuple

    Output:
        tuple of boolean and knot

    Example1:
        [(S, 0, 1, 2);...] (1, 2)
        ->
        (true, 0)
        
    Example2:
        [(S, 0, 1, 2);...] (7,1)
        ->
        (false, 0)

*)
let rec getPointByTransitions candidateList transitiontuple =
      match candidateList with
      | [] -> (false, 0) (* returns 0, als 'false' Wert, Programm fängt das 'false' ab und ignoriert die 0 dann *)
      | hd::tl ->
         let (_, t, t0, t1) = hd in
         if transitiontuple = (t0,t1)
            then (true, t)
            else getPointByTransitions tl transitiontuple
;;

(** 

    Input:
       List of knots

    Output:
       List of tuples of transitions of (points in list in Input)
    
    Example:
        [(S, 0, 1, 2);...]
        ->
        [(1, 2);...]
*)
let getTransitionByListOfPoints knotls =
   let rec recursion ls =
      match ls with
      | [] -> []
      | hd::tl -> (
         let (_, _, t0, t1) = hd in
         (t0, t1) :: (recursion tl)
      );
   in recursion knotls
;;

(** Get state of a knot

    Input:
        canditateList,
        knot

    Output:
        state of a knot
        
    Example:
        [(S, 0, 1, 2);...] 0
        ->
        S
*)
let getStateByPoint candidateList knot =
   let rec recursion ls =
      match ls with
      | [] -> failwith "Unknown knot"
      | hd::tl -> (
         let (s, k, _, _) = hd in
         if (knot = k)
            then s
            else (recursion tl)
      );
   in recursion candidateList
;;

(** Get Position in table of a knot

    Input:
        canditateList,
        knot

    Output:
        Position of knot in table
        
    Example:
        [(S, 0, 4, 99); (N, 4, 4, 99); (F, 99, 0, 4)] 4
        ->
        1
*)
let getPositioninTable candidateList knot =
   let rec recursion ls i =
      match ls with
      | [] -> failwith ("Unknown knot: " ^ (string_of_int knot))
      | hd::tl -> (
         let (_, k, _, _) = hd in
         if (knot = k)
            then i
            else (recursion tl (i+1))
      );
   in recursion candidateList 0
;;

(** Function for table-filling-algorithm. returns true if one of the following is marked with true: the state tuple of both 0 or 1 transitions of 2 points.  

    Input:
        candidateList (columnHeight, rowWidth),
        ls,
        element

    Output:
        boolean
        
    Example:
        [(S, 0, 1, 0); (F, 1, 1, 0)] (2, 2) [false; false; false; true] 0
        ->
        (true) || (false) -> true
*)
let strike_out_element candidateList (columnHeight, rowWidth) ls element =
   let (x,y) = decode2D (columnHeight, rowWidth) element in
   let (_, _, a0, a1) = (getNElement candidateList y) in
   let (_, _, b0, b1) = (getNElement candidateList x) in
      (getNElement ls (encode2D (columnHeight, rowWidth) (getPositioninTable candidateList a0) (getPositioninTable candidateList b0)))
   ||   (getNElement ls (encode2D (columnHeight, rowWidth) (getPositioninTable candidateList a1) (getPositioninTable candidateList b1)))
;;

(** Checks if a states tuple contains 2 different states (like final - start)

    Input:
        candidateList (columnHeight, rowWidth),
        where

    Output:
        boolean
        
    Example:
        [(S, 0, 1, 0); (F, 1, 1, 0)] (2, 2) 1
        ->
        xor (isFinal S) (isFinal F) -> true
*)
let areDifferentState candidateList (columnHeight, rowWidth) where =
   let (x, y) = (decode2D (columnHeight, rowWidth) where) in
   let (sx, _, _, _) = getNElement candidateList x in
   let (sy, _, _, _) = getNElement candidateList y in
   (xor (isFinal sx) (isFinal sy))
;;

(** Marks state in table of booleans when they are final and not final

    Input:
        candidateList (columnHeight, rowWidth)
        endvalue.
        ls

    Output:
    table of booleans
    
    Example:
        [(S, 0, 1, 0); (F, 1, 1, 0)] (2, 2) 3 [false; false; false; false]
        ->
        [false; true; true; false]

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

(** Computes aequivalence tuples

    Input:
        candidateList (columnHeight, rowWidth)
        endvalue.
        ls


    Output:
        aequivalenz classes as a table of string
        
    Example:
        [...] (7, 7) 48 [false; ...; false]
        ->
        [(0,1);(0,2);(1,2);(2,3); (4,5);(4,6);(5,6)]
        
        

*)
let aequivalenz_klasse candidateList (columnHeight, rowWidth) endvalue ls =
   let rec recursion i =
      if i <= endvalue
         then(
            let (x,y) = decode2D (columnHeight, rowWidth) i in
            if (not (x=y)) (* Herausfilter der Diagonalen als Äquivalenztupel *)
               then (
                  if not (getNElement ls i) (* falls noch nicht markiert *)
                     then (
                        let (_, a, _, _) = (getNElement candidateList x) in
                        let (_, b, _, _) = (getNElement candidateList y) in
                        (a, b)::(recursion (i + 1))
                     )
                     else recursion (i + 1)
               )
               else recursion (i + 1)
         )
         else []
   in recursion 0
;;

(** returns pre equivalence class

    Input:
        int, tuple list

    Output:
        int list
        
    Example:
        0 [(0,1);(0,2);(1,2);(2,3); (4,5);(4,6);(5,6)]
        ->
        [1;2;3]

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

(** Deletes equivalence tuples which are contained in a given equivalence class

    Input:
        list of equivalence tuple,
        aquivalence-class

    Output:
        list of remaining equivalence tuple

   Example:
      [(0,1);(0,2);(0,3);(1,2);(1,3);(2,3); (4,5);(4,6);(5,6)] [0;1;2;3]
      ->
      [(4,5);(4,6);(5,6)]

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

(** creates aquivalence-classes

   Input:
      list of equivalence tuples

   Output:
      list of aquivalence-classes

   Example:
      [(0,1);(0,2);(0,3);(1,2);(1,3);(2,3); (4,5);(4,6);(5,6)]
      ->
      [[0; 1; 2; 3]; [4; 5; 6]]

*)
let rec aequivalenz_klasse_bilden ls =
   match ls with
   | [] -> []
   | hd::tl ->
      let (a, _) = hd in
      let aequi = a::(get_aequivalenztuple a ls) in
      aequi :: (aequivalenz_klasse_bilden (streiche_aequi ls aequi))
;;

(** Returns first List which contains element 

    Input:
        element,
        ls (int list list)

    Output:
        list
        
    Example1:
       2 [[1;2;3];[4;5;6]]
       ->
       [1;2;3]
       
    Example2:
       6 [[1;2;3];[4;5;6]]
       ->
       [4;5;6]

*)
let rec getListbyElement element ls =
   match ls with
   | [] ->  failwith ("Element not in List " ^ (string_of_int element) ^ "[" ^ (string_of_int_list_list ls) ^ "]")
   | hd::tl ->
      if (contains element hd)
         then hd
         else getListbyElement element tl
;;

(** Determine if a given knot list contains any start-type (SF || S)

    Input:
        canditateList
        ls

    Output:
        boolean
        
    Example1:
        [(S, 0, 0, 0); (N, 1, 0, 0); (F, 2, 0, 0)] [(S, 0, 0, 0); (N, 1, 0, 0)]
        -> 
        true
       
    Example2:
        [(S, 0, 0, 0); (N, 1, 0, 0); (F, 2, 0, 0)] [(N, 1, 0, 0); (F, 2, 0, 0)]
        -> 
        false

*)
let rec determineStart candidateList ls =
   match ls with
   | [] -> false
   | hd::tl -> (isStart (getStateByPoint candidateList hd)) || (determineStart candidateList tl)
;;

(** Determine if a given knot list contains any final-type (SF || F)

    Input:
        canditateList
        ls

    Output:
        boolean
        
    Example:
      see determineStart

*)
let rec determineFinal candidateList ls =
   match ls with
   | [] -> false
   | hd::tl -> (isFinal (getStateByPoint candidateList hd)) || (determineFinal candidateList tl)
;;

(** Determine state-type of knot list

    Input:
        canditateList
        ls

    Output:
        state-type (SF || S || F || N)
    
    Example:
       [(S, 0, 0, 0); (N, 1, 0, 0); (F, 2, 0, 0)] [(S, 0, 0, 0); (N, 1, 0, 0)]
       ->
       S

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

(** Creates min_dfa_transition_table

    Input:
        canditateList
        list of equivalence-classes

    Output:
        min_dfa_transition_table
    
    Example:
       [(S, 2, 2, 4); (F, 5, 0, 5); ...] [[0; 1; 2; 3]; [4; 5; 6]]
       ->
       [(S, [0; 1; 2; 3], [0; 1; 2; 3], [4; 5; 6]);
        (F, [4; 5; 6], [0; 1; 2; 3], [4; 5; 6])]

*)
let create_min_dfa_transition_table candidateList aequiLS =
   let rec recursion ls =
      match ls with
      | [] -> []
      | hd::tl -> (
         match hd with
         | [] -> failwith "Aequivalentliste is empty"
         | hdhd::_ -> (
            let (hd0, hd1) = getTransitionByPoint candidateList hdhd in
            ((determineState candidateList hd), hd, (getListbyElement hd0 aequiLS), (getListbyElement hd1 aequiLS))::(recursion tl)
            )
         )
   in recursion aequiLS
;;

(** returns List of all knots that are start points

    Input:
        canditateList

    Output:
        list of points

    Example:
       [(S, 2, 2, 4); (F, 5, 0, 5)]
       ->
       [2]
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

(** checks if list contains something multiple times

    Input:
        list

    Output:
        boolean
*)
let rec containsMultiple ls =
   match ls with
   | [] -> false
   | hd::tl -> (contains hd tl) || (containsMultiple tl)
;;

(** Function for table-filling-algorithm. returns filling table with applied strike_out_element

    Input:
        candidateList (columnHeight, rowWidth),
        endvalue ls

    Output:
        ls
*)
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

(** returns List of all knots

    Input:
        canditateList

    Output:
        list of points

    Example:
       [(S, 2, 2, 4); (F, 5, 0, 5)]
       ->
       [2; 5]
*)
let rec getListOfPointerNames ls =
   match ls with
   | [] -> []
   | hd::tl -> (
      let (_, k, _, _) = (getNElement ls 0) in
      k::(getListOfPointerNames tl)
   )
;;

(** checks for various human made input erros

    Input:
        dfa_transition_table

    Output:
        dfa_transition_table
*)
let checkforInputErrors dfa_transition_table =
   (** checks if transition table is empty: *)
   if (dfa_transition_table = [])
      then failwith "One of the given dfa transition tables was empty"
      else (
         let zustaende=getListOfPointerNames dfa_transition_table in
         (** checks if there are multiple points with the same name: *)
         if (containsMultiple zustaende)
            then failwith "In one of the given dfa transition tables pointer names were assignend multiple times"
            else (
               let rec doTransitionPointsExist ls = (
                  match ls with
                  | [] -> true
                  | hd::tl -> (
                     let (_, _, a0, a1) = (getNElement ls 0) in
                     (contains a0 zustaende && contains a1 zustaende) && (doTransitionPointsExist tl)
                  )
                  (** checks if there is a invalid transition (meaning transition to a point that does not exist) *)
               ) in if not (doTransitionPointsExist dfa_transition_table)
                  then failwith "Pointed to invalid Point"
                  (** checks if there is exactly one starting point: *)
                  else (if (lenght(getStartList dfa_transition_table) != 1)
                     then failwith "Wrong Amount of Start Points"
                     else dfa_transition_table
                  )
            )
      )
;;

(** renames Transition (used in minimize)

    Input:
        searchvalue replacevalue,
        ls

    Output:
        ls
        
    Example:
       2 1 [(S, 0, 1, 2); (N, 1, 2, 4)]
       ->
       [(S, 0, 1, 1); (N, 1, 1, 4)]
*)
let rec renameTransitions searchvalue replacevalue ls =
   match ls with
   | [] -> []
   | hd::tl ->
      let (y, z, a, b) = hd in
      (y, z, (if a=searchvalue then replacevalue else a), (if b=searchvalue then replacevalue else b))::(renameTransitions searchvalue replacevalue tl)
;;

(** checks for points with same transitions

    Input:
        dfa_transition_table

    Output:
        dfa_transition_table
        
    Example:
       [(S, 0, 1, 2); (N, 1, 2, 4); (N, 2, 2, 4); ...]
       ->
       [(S, 0, 1, 1); (N, 1, 1, 4); ...]
*)
let minimize dfa_transition_table =
   let rec recursion ls recls=
      match ls with
      | [] -> recls
      | hd::tl ->
         let (_, a, a0, a1) = hd in
         let (booleanvalue, point) = getPointByTransitions recls (a0, a1) in
         if booleanvalue
            then recursion (renameTransitions a point tl) (renameTransitions a point recls)
            else recursion tl (recls @ [hd])

   in recursion dfa_transition_table []
;;




(* ~~~~~~~~~~~~~~~~~~~~~~~~ Variables ~~~~~~~~~~~~~~~~~~~~~~~~ *)

let tabelle1 = [(S,1,1,2); (N,2,3,4); (F,3,3,2); (F,4,3,2)];; (* DEA 1 *)
let tabelle2 = [(S,5,6,7); (N,6,5,8); (N,7,9,9); (N,8,9,9); (F,9,9,8); (F,10,10,9)];; (* DEA 2 *)

let tabelle3 = [];; (* triggers  Error 1*)
let tabelle4 = [(S,1,1,2);(S,2,3,4);(F,3,3,2);(F,4,3,2)];; (* triggers  Error 2*)
let tabelle5 = [(S,1,1,1);(N,1,3,4);(F,3,3,1);(F,4,3,1)];; (* triggers  Error 3*)
let tabelle6 = [(S,1,1,2);(N,2,3,4);(F,3,3,2);(F,4,3,2)];; (* triggers  Error 4*)

let tabelle7 = [(S,5,5,2); (N,2,3,4); (F,3,3,2); (F,4,3,2)];; (* triggers Error 5 *)

let tabelle1n = [(S,40,40,50); (N,50,60,70); (F,60,60,50); (F,70,60,50)];; (* DEA 1 with different names *)
let tabelle2n = [(S,80,90,100); (N,90,80,110); (N,100,120,120); (N,110,120,120); (F,120,120,110); (F,130,130,120)];; (* DEA 2 with different names *)


(* Step 0: minimizing functions dfa_transition_table *)
let candidates = (minimize (checkforInputErrors tabelle1), minimize (checkforInputErrors tabelle2));; (* checks for errors in Inputs (like: empty, multipletimes same name, points to invalid point) and deletes unreachable and duplicated points *)




let (tab1, tab2) = candidates;;
let candidateList = tab1 @ tab2;;

if (containsMultiple (getListOfPointerNames candidateList)) then failwith "Pointer name assignend multiple times across both tables";; (* checks if there are points in tab1 that have the same name as in tab2*)

let rowWidth = (lenght tab1)+(lenght tab2);;
let columnHeight = (lenght tab1)+(lenght tab2);;




(* ~~~~~~~~~~~~~~~~~~~~~~~~ Main-Programm ~~~~~~~~~~~~~~~~~~~~~~~~ *)

print_candidates candidates;;
print_string "\n";;

(* Step 1: Creates table with both DFA's and marks complete table as false with function make
   set filling_table to:
      table of booleans with 'false'´s
*)
let filling_table = make [] (rowWidth * columnHeight) false;; (* true -> angekreuzt *)

(* Prints table of boolean false of both DFA's for step 1 *)
print_boolean_table (columnHeight, rowWidth) filling_table rowWidth columnHeight;;

(* Step 2: Mark state, which are not start-states (N, F)
   set filling_table to:
      table of booleans
*)
let filling_table = strike_finals candidateList (columnHeight, rowWidth) ((rowWidth * columnHeight)-1) filling_table;;

(* Print table of booleans for step 2 *)
print_boolean_table (columnHeight, rowWidth) filling_table rowWidth columnHeight;;

(* Step 3: Mark state, which are different

    set filling_table to:
      updated filling_table with strike_out algorithm
*)
let filling_table =
   let rec recursion ft = (
      let new_filling_table = (strike_out candidateList (columnHeight, rowWidth) ((rowWidth * columnHeight)-1) ft) in

      if (ft = new_filling_table)
         then (ft)
         else (print_boolean_table (columnHeight, rowWidth) new_filling_table rowWidth columnHeight; recursion new_filling_table)
   );
   in recursion filling_table
;;


(* Step 4+5 *)
let result =
   match getStartList tab1 with
   | [startOfTab1] -> (
      match getStartList tab2 with
      | [startOfTab2] -> (
         let row = getPositioninTable candidateList startOfTab1 in
         let column = getPositioninTable candidateList startOfTab2 in
         if not (getNElement filling_table (encode2D (columnHeight, rowWidth) row column)) (* checkt, ob Startzustände unterscheidbar *)
            then (
               (* Step 4: Create Aquivalence-Classes *)
               (* Step 4.1: Create Aquivalence-Tuples
                   set aequivalenz_tuple to:
                       list of aquivalence-tuples
               *)
               let aequivalenz_tuple = (aequivalenz_klasse candidateList (columnHeight, rowWidth) ((rowWidth * columnHeight)-1) filling_table) in
               
               print_aquivalenzklasse aequivalenz_tuple;
               print_string "\n\n";

               (* Step 4.2: Create Aquivalence-Classes
                   set aequivalenzklasse to:
                       list of aquivalence-classes
               *)
               let aequivalenzklasse = (aequivalenz_klasse_bilden aequivalenz_tuple) in

               (* Step 5: If both DFA's are aquivalent, minimize DFA's. Else
                   set min_dfa_transition_table to:
                       minimized dfa_transition_table
                   
                   returnning minimized min_dfa_transition_table and boolean representing if DFA were equivalent
               *)
               let min_dfa_transition_table = create_min_dfa_transition_table candidateList aequivalenzklasse in

               (true, min_dfa_transition_table)
            )
            else (
               (false, [])
            )
         )

      | _ -> failwith "This will never trigger - just so there is no warning"
   )
   | _ -> failwith "This will never trigger - just so there is no warning"
;;
print_equivalence_result result;;