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


let remove_duplicates l = List.fold_left (fun acc x -> filter (fun y -> y <> x) acc @ [x]) [] l;;

let append l1 l2 = List.fold_right ( fun x acc -> x :: acc) l1 l2;;


let map f l = List.rev(List.fold_left (fun acc x -> (f x) :: acc) [] l);;


let seq_max_len l e = 
  let rec aux_max_len l e curr_acc max_acc = 
    match l with 
    | [] -> max curr_acc max_acc
    | h :: tail -> if h = e then aux_max_len tail e (curr_acc + 1) max_acc 
                   else aux_max_len tail e 0 (max curr_acc max_acc)
  in aux_max_len l e 0 0;;