type ('k, 'v) dict = ('k * 'v) list;;
type 'a option = Some of 'a | None;;
(* type ('a, 'b) btree = Node of ('a * 'b) * ('a, 'b) btree | Empty;; *)


let empty_dict : ('k, 'v) dict = [];;

let get_value d = let (_, v) = d in v;;
let get_key d = let (k, _) = d in k;;


(* O(n) *)
let rec find key (dict : ('k, 'v) dict) = 
  match dict with 
  | [] -> None
  | value :: tail -> let k = get_key value in if (k = key) then Some (get_value value) else find key tail;;


let (test : ('k, 'v) dict) = [("one", 2); ("two", 3); ("three", 5); ("five", 8)];;

let reverse_dict d = 
  let rec rev_aux d acc = 
    match d with 
    | [] -> acc 
    | v :: tail -> rev_aux tail (v :: acc)
  in rev_aux d [];;


let add key value (dict : ('k, 'v)dict) : ('k, 'v) dict = 
  let rec add_aux key value dict acc found= 
    match dict with
    | [] -> if (not found) then (key, value) :: acc else acc
    | v :: tail -> if (get_key v = key) 
                        then add_aux key value tail ((key,  value) :: acc) true
                        else add_aux key value tail (v :: acc) false
  in reverse_dict(add_aux key value dict [] false);;



let remove k (dict: ('k, 'v) dict) : ('k,'v) dict = 
  let rec remove_aux k dict acc = 
    match dict with 
    | [] -> acc 
    | v :: tail -> if (get_key v = k) then remove_aux k tail acc else remove_aux k tail (v :: acc)
  in reverse_dict(remove_aux k dict []);;