let rec rev_append l acc =
  match l with
  | [] -> acc
  | h :: t -> rev_append t (h :: acc);;

let reverse_efficace l = rev_append l [];;


type e_b_c = E of int | B of bool | C of string;;


let sum_ebc l = 
  let rec aux_sum l acc =
    match l with 
    | [] -> acc
    | h :: tail -> match h with
      | E(x) -> aux_sum tail acc+x
      | _ -> aux_sum tail acc
  in aux_sum l 0;;

let rec filter_int_nonterm l = 
  match l with 
  | [] -> [] 
  | h :: tail -> match h with 
    | E(x) -> let prev_list = filter_int_nonterm tail in (x :: prev_list)
    | _ -> filter_int_nonterm tail 

let filter_int l = 
  let rec aux_filter l acc = 
    match l with 
    | [] -> List.rev(acc)
    | h :: tail -> match h with
      | E(x) -> aux_filter tail (x::acc)
      | _ -> aux_filter tail acc 
  in aux_filter l [];;

let test = [C("t"); C("e"); C("s");C("t"); E(1)];;

let rec concat_non_term l = 
  match l with 
  | [] -> "" 
  | h :: tail -> match h with 
    | C(c) -> let prev_string = concat_non_term tail in (c^prev_string)
    | _ -> concat_non_term tail

let concat_term l = 
  let rec aux_concat l acc = 
    match l with 
    | [] -> acc 
    | h :: tail -> match h with 
      | C(c) -> aux_concat tail (acc ^ c)
      | _ -> aux_concat tail acc
  in aux_concat l "";;


let decomp10 n = 
  let rec decomp_aux n acc  = 
  match n with 
  | x -> if (x < 10) then List.rev(x :: acc) else decomp_aux (n / 10) (n mod 10 :: acc)
  in decomp_aux n [];;


let uncomp_temp n = let (x, nb_occ) = n in 
  let rec uncomp_aux x n acc = 
    if (n = 0) then acc else uncomp_aux x (n-1) (x::acc)
  in uncomp_aux x nb_occ [];;

let uncompress l = List.fold_right (fun x acc -> (uncomp_temp x) @ acc ) l [];;


let compress l = if (l = []) then [] else
  let rec aux_comp l acc nb_occ curr_indx prev_elt= 
    match l with 
    | [] -> List.rev((prev_elt, nb_occ) :: acc) 
    | h :: tail -> if (curr_indx == 0) 
      then aux_comp tail acc nb_occ (curr_indx+1) h 
      else if (prev_elt = h) then aux_comp tail acc (nb_occ+1) (curr_indx+1) h 
      else aux_comp tail ((prev_elt, nb_occ) :: acc) 1 (curr_indx+1) h 
    in aux_comp l [] 1 0 0;;