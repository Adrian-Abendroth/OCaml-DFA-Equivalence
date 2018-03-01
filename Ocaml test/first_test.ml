let mylist table_one table_two =
    let finished_table = table_two @ table_one in
    finished_table
;;

let rec print_list x = 
match x with
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l;;

let x = [1;2;3];;
(* print_list(x);; *)
print_list([1;2] @ [3;4]);;


let list a = [1;2;3];;
let list b = [4;5;6];;

(* let mylist a b;;*)

(* print_list(mylist);; *)