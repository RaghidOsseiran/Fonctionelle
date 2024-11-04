(*

fold left: operation with accumulator and first element then result will be the operation with
newly calculated accumulator then next element.  (new_acc (acc elt1) elt2)

fold right: operation with accumulator and last element ... 
  (elt n-1 (eltn acc) new_acc)

*)

let l = [5; 10; 4];;


let x = List.fold_left (fun x y -> x + y) 0 l;;
let y = List.fold_right (fun x y -> x + y) l 0;;

let rec right_fold f l acc =
  match l with 
  | [] -> acc 
  | h :: tail -> let res = right_fold f tail acc in 
  f h res;;

let rec left_fold f acc l =
  match l with 
  | [] -> acc 
  | h :: tail -> left_fold f (f acc h) tail;;


let length l = List.fold_left (fun x y -> x + 1) 0 l;;
let length_right l = List.fold_right (fun x y -> y + 1) l 0;;

let reverse l = List.fold_left (fun x y -> y::x) [] l;;
let reverse_right l = List.fold_right (fun x y -> y @ [x]) l [];;

let maximum l = List.fold_left (fun x y -> if (x > y) then x else y) min_int l;;

let filter p l = List.fold_left (fun x y -> if (p y) then y::x else x) [] l;;


let rec in_list x l =
  match l with 
  | [] -> false 
  | h :: tail -> if (x = h) then true else in_list x tail;;

let remove_duplicates l = List.fold_left (fun x y -> filter in_list)