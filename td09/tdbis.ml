(* type 'a option = Some of 'a | None;;
type ('k , 'v) dict = 'k -> 'v option;;

let empty_dict = fun k -> None;;

let find_key key (d: ('k, 'v) dict) = d key;;

let add_or_update key value (d: ('k, 'v) dict) : ('k, 'v) dict  = 
  fun k -> if k = key then Some value else d k;;

let remove key (dict: ('k, 'v) dict) : ('k, 'v) dict = 
  fun k -> if k = key then None else dict k;;

let test_dict = fun x -> if (x = 3) then Some 3 else None;; *)
(* let test1_dict = add_or_update 3 2 test_dict;; *)


(* type ('k, 'v) dict = ('k * 'v option) list;;
let empty_dict = [];;

let rec find key dict = 
  match dict with 
  | [] -> None 
  | couple :: tail -> let (k, v) = couple in if (k = key) then v else find key tail;;

let add_or_update key value dict = (key, value) :: dict;;

let remove key dict = (key, None) :: dict;; *)



type ('k, 'v) dict = ('k * 'v) list;;

let test = [(1, "Lundi"); (2, "Mardi"); (3, "Jeudi")];;

let remove key dict = List.rev(List.fold_left (fun x y -> let (k, v) = y in if (k = key) then x else y::x) [] dict);;

let add_or_update key value dict = 
  let new_dict = remove key dict in (key, value) :: new_dict;;


let l1 = function
  | 0 -> Some "Hello"
  | 1 -> Some "World!"
  | _ -> None;;


let l2 x = if (x < 5) then Some (2 * x) else None;;


type 'a fun_list = int -> 'a option;;


let fun_nil = fun _ -> None;;

let fun_is_nil list = list 0 = None;;


let fun_nth index (list : 'a fun_list) : 'a option = list index;;

let fun_cons value (list: 'a fun_list) : 'a fun_list = fun x -> if (x = 0) then Some value else list (x-1);;

let fun_tail list : 'a fun_list = fun x -> list (x+1);;

let fun_length (list : 'a fun_list) = 
  let rec aux_length list acc = 
    if (list acc = None) then acc else aux_length list (acc+1)
  in aux_length list 0;;

let fun_map f (list: 'a fun_list) : 'b fun_list = 
  fun x -> 
    match list x with 
      | Some v -> Some (f v)
      | None -> None;;


let list_of_fun_list (list : 'a fun_list) : 'a option list = 
  let rec aux_lol acc index = 
    match list index with
      | Some y as s_y -> aux_lol (s_y :: acc) (index+1)
      | None -> List.rev(acc)
  in aux_lol [] 0;;


let fun_list_of_list (list : 'a list) : ('a fun_list) = 
  let rec flof_aux list x index = 
    match list with 
    | [] -> None 
    | h :: tail -> if (x = index) then Some h else flof_aux tail x (index+1) 
  in fun x -> flof_aux list x 0;;
  

let truncate (list : 'a fun_list) n = 
  fun x -> if (x >= n) then None else list x;;


<<<<<<< HEAD
type ('k, 'v) abr = Empty | Node of ('k,'v) abr * 'k * 'v * ('k,'v) abr;;

type comparaison = Lt | Eq | Gt;;

type ('k, 'v) dict = ('k, 'v) abr;;
=======
type ('k,'v) abr = Empty | Node of ('k,'v) abr * 'k * 'v * ('k,'v) abr;;
type comparison = Lt | Eq | Gt;;

type ('k, 'v) dict = {
  cmp: 'k -> 'k -> comparison; 
  tree: ('k, 'v) abr;
};;

let empty_dict cmp_func = {cmp = cmp_func ; tree = Empty};;

let find k dict = 
  let rec find_aux cmp curr_tree = 
    match curr_tree with 
    | Empty -> None
    | Node (left, key, value, right) -> 
        match (cmp k key) with
        | Lt -> find_aux cmp left
        | Eq -> Some value
        | Gt -> find_aux cmp right
  in find_aux dict.cmp dict.tree;;


let add k v dict = 
  let rec add_aux cmp curr_tree = 
    match curr_tree with
    | Empty -> Node(Empty, k, v, Empty)
    | Node(left, key, value, right) -> 
        match (cmp k key) with
        | Lt -> Node(add_aux cmp left, key, value, right)
        | Eq -> Node(left, key, v, right)
        | Gt -> Node(left, key, value, add_aux cmp right)
  in let new_tree = add_aux dict.cmp dict.tree in {cmp = dict.cmp ; tree = new_tree };;

type comparison = Lt | Eq | Gt

type ('k, 'v) abr =
  | Empty
  | Node of ('k, 'v) abr * 'k * 'v * ('k, 'v) abr

type ('k, 'v) dict = {
  cmp: 'k -> 'k -> comparison;
  tree: ('k, 'v) abr;
}

(* Helper function to find the minimum valued node and its parent *)
let rec min_value_node_and_parent parent = function
  | Empty -> failwith "Should not happen"  (* This should not be called with Empty *)
  | Node (Empty, k, v, _) -> (parent, k, v)
  | Node (left, k, _, _) -> min_value_node_and_parent (Some k) left

(* Recursive function to remove a node *)
let rec remove_aux cmp k = function
  | Empty -> failwith "Should not happen"  (* Key not found *)
  | Node (left, key, value, right) as node ->
      match cmp k key with
      | Eq -> (match (left, right) with
          | (Empty, Empty) -> Empty  (* No children *)
          | (left, Empty) -> left    (* One child, on the left *)
          | (Empty, right) -> right  (* One child, on the right *)
          | (left, right) ->         (* Two children *)
              let _, s_key, s_value = min_value_node_and_parent None right in
              Node (left, s_key, s_value, remove_aux cmp s_key right))
      | Lt -> Node (remove_aux cmp k left, key, value, right)
      | Gt -> Node (left, key, value, remove_aux cmp k right)

(* Function to remove a key from the dictionary *)
let remove k dict =
  { dict with tree = remove_aux dict.cmp k dict.tree }
>>>>>>> 02ee6b3b734632224a289619982a58ac661b8dd0
