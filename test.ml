let square x = x*x;;

let zustaende1 = [|(S,1,1,2)|;|(K,2,3,4)|;|(F,3,3,2)|;|(F,4,3,2)|] ;;
let zustaende2 = [|(S,1,1,2)|;|(K,2,3,4)|;|(F,3,3,2)|;|(F,4,3,2)|] ;;


let lenzustaende1 = Array.length zustaende1 ;;
let lenzustaende2 = Array.length zustaende2 ;;

let a = Array.make (square (lenzustaende1+lenzustaende2)) false;;
print_int (Array.length a);;
