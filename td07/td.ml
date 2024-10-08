type 'a mylist = Nil | C of 'a * 'a mylist;;


let reverse_list l = 
  let rec aux_rev l acc = 
    match l with 
    | []-> acc 
    | h :: tail -> aux_rev tail (h :: acc)
  in aux_rev l [];;


let rec interval_list n p = 
  let rec aux_interval n p acc =
    if (n > p) then acc 
    else aux_interval (n+1) p (n::acc)
  in reverse_list(aux_interval n p []);; 





let rec map f l =
  let rec aux_map f l acc = 
    match l with 
    | [] -> acc 
    | h :: tail -> aux_map f tail (f h :: acc)
  in reverse_list(aux_map f l []);;


let test_loml = C(1, C(2, C(3, Nil)));;
let test_mlol = [1; 2; 3];;

let list_of_mylist ml = 
  let rec aux_loml ml acc =
    match ml with  
    | Nil -> acc 
    | C(h, tail) -> aux_loml tail (h :: acc)
  in reverse_list(aux_loml ml []);;

let rec mylist_of_list l = 
  match l with 
  | [] -> Nil
  | h :: tail -> C(h, mylist_of_list tail);;


(*type: int -> int -> int list*)

(*Non recursive terminal*)
let rec replicate x k = 
  if (k <= 0) then [] 
  else 
    let res = replicate x (k-1) in 
    x :: res;;


(*recursive terminal*)
let replicate_terminal x k = 
  let rec aux_replicate x k acc = 
    if (k <= 0) then acc 
    else aux_replicate x (k-1) (x :: acc)
  in aux_replicate x k [];;

(* complexiter O(n²) car on utilise O(len(l)) fois une l'operateur @ qui est de complexiter linear par rapport a la taille de la liste *)
let rec reverse_non_terminal l = 
  match l with 
  | [] -> [] 
  | h :: tail -> 
    let res = reverse_non_terminal tail in res @ [h] ;;


(* complexiter O(n) *)
let reverse_terminal l = 
  let rec aux_reverse l acc = 
    match l with 
    | [] -> acc 
    | h :: tail -> aux_reverse tail (h::acc)
  in aux_reverse l [];;


(*interval_list est de complexiter O(n) + O(n) car on reverse la liste a la fin, et comme on reverse encore une fois c'est O(3n) = O(n) *)
let iota_r n = reverse_terminal(interval_list 1 n) 

(* meme raisonemment O(2n) = O(n) *)
let iota n = interval_list 1 n;;


(*

La solution montrer dans l'exo 7.4.6 est mauvais car elle est de complexiter quadratique car on applique n fois l'operateur @ qui est lui meme
lineaire par rapport a la taille de la liste, donc on aura une complexiter de O(n x n) = O(n²).

*)


(* fonction pour comparer des fonction *)

let time f = 
  let start = Sys.time() in 
  let _ = f() in 
  Sys.time() -. start;;



let rec member x l = 
  match l with 
  | [] -> false 
  | h :: tail -> if (h = x) then (true || member x tail) else (false || member x tail);;

let time_member = time (fun () -> member 2 [1;5;4;6;2]);;
let time_Lmem = time (fun () -> List.mem 2 [1;5;4;6;2]);;


let compare_membs x l = 
  let res1 = member x l and res2 = List.mem x l in (res1 = res2);;


(* both function have the exac same time *)



let count x l =
  let rec aux_count x l acc =
    match l with 
    | []-> acc 
    | h :: tail -> if (h = x) then aux_count x tail (acc+1) else aux_count x tail acc
  in aux_count x l 0;;


let member' x l = ((count x l) > 0);;

let time_member' = time (fun () -> member' 2 [1;5;4;6;2])


let rec list_subset l l' = 
  match l with 
  | [] -> true 
  | h :: tail -> 
    let res = List.mem h l' in (res && list_subset tail l');;

let test_subset ()= 
  begin 
    assert(list_subset [1;2;3;4;5] [5;4;3;2;1] = true);
    assert(list_subset [1;2;3] [1;2;3;4;5] = true);
    assert(not (list_subset [1;2;6] [1;2;3;4;5]) = true);
    assert((list_subset [1;2;6] [1;2;3;4;5]) = false);
  end


let permutation l l' = 
  let rec aux_permutation org_l l l' = 
    match l with 
    | [] -> true
    | h :: tail -> let res = ((count h org_l) = (count h l')) in (res && aux_permutation org_l tail l')
  in aux_permutation l l l';;


let test_permuation ()= 
  begin 
    assert(permutation [1;2;3;4;5] [5;4;3;2;1] = true);
    assert(permutation [3;1;2;1] [1;3;2;1] = true);
    assert(not (permutation [1;2;6] [1;2;3;4;5]) = true);
    assert((permutation [1;2;6] [1;2;3;4;5]) = false);
  end


let list_length l = 
  let rec aux_length l acc = 
    match l with 
    | [] -> acc 
    | h :: tail -> aux_length tail (acc+1)
  in aux_length l 0;;

let rec prefix l l' =
  if (list_length l > list_length l') then false else 
  let rec aux_prefix l l' =
    match l, l' with
    | [], _ -> true 
    | _, [] -> false  
    | h :: tail, h'::tail' -> let res = (h = h') in (res && aux_prefix tail tail')
  in aux_prefix l l';; 
    
    
let test_prefix () = 
  begin 
    assert(prefix [1;2;3] [3;2;1] = false);
    assert(prefix [1;2;3] [1;2] = false);
    assert(prefix [1;2;3;4] [1;2;3;4;5] = true);
  end

let rec squares l =
  match l with 
  | [] -> [] 
  | h :: tail -> let res = squares tail in (h*h) :: res;; 
  
let squares' l = map (fun x -> x*x) l;;

let sum l = 
  let rec aux_sum l acc = 
    match l with 
    | [] -> acc 
    | h :: tail -> aux_sum tail (acc+h)
  in aux_sum l 0;;


let prod l = 
  let rec aux_sum l acc = 
    match l with 
    | [] -> acc 
    | h :: tail -> if (h = 0) then 0 else aux_sum tail (acc*h)
  in aux_sum l 1;;


let test_prod = prod (0 :: (iota 10000))



(*Exercice 7.13.1

let type de break est ('a -> bool)->'a list-> 'a list * 'a list;;

la valeur en general de break p l c'est un couple contenant une liste qui contient tout les elements de l qui verifie un certain predicat p, et
la deuxieme liste qui contient le reste des element de l qui ne verifie pas ce predicat.

*)

let rec break p l =
  match l with 
  | [] -> ([], [])
  | a :: r -> let (l1, l2) = break p r in if p a then (a :: l1, l2) else (l1, a:: l2);;


let breakfast p l =
  let rec aux_break p l acc1 acc2 = 
    match l with 
    | [] -> (reverse_terminal(acc1), reverse_terminal(acc2))
    | h :: tail -> if p h then aux_break p tail (h::acc1) acc2 else aux_break p tail acc1 (h::acc2)
  in aux_break p l [] [];;



let rec rotate_left l =
  match l with 
  | [] -> []
  | h :: tail -> tail @ [h];;






let list_to_pairs l =
  let rec aux_to_pairs l acc = 
    match l with 
    | [] -> acc 
    | h :: tail ->
      let new_list = List.filter(fun x -> x <> h) l in aux_to_pairs new_list ((h, count h l) :: acc) 
  in aux_to_pairs l [];;



let rec mapcan f l =
  match l with 
  | [] -> []
  | h :: tail -> let res = mapcan f tail in 
  let to_add = f h in to_add @ res;;


let prefix l n = 
  let rec aux_prefix l n acc = 
    if (n <= 0) then acc 
    else 
      match l with 
      | [] -> acc 
      | h :: tail -> aux_prefix tail (n-1) (h :: acc)
    in reverse_terminal(aux_prefix l n []);;


let suffix l n = reverse_terminal(prefix (reverse_terminal l) n);;