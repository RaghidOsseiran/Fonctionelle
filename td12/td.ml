let partition p l = 
  let rec aux_partition acc1 acc2 l = 
    match l with 
    | [] -> (acc1, acc2)
    | h::tail -> if (h < p) then aux_partition (h::acc1) acc2 tail else aux_partition acc1 (h::acc2) tail
  in aux_partition [] [] l;;


let quick_sort l = 
  let rec aux_quick l = 
    match l with 
    | [] -> []
    | h::tail -> 
      let (l1,l2) = partition h tail in 
      aux_quick l1 @ (h :: aux_quick l2)
    in aux_quick l;;

(* O(n log n) best/common case *)
(* O(n^2) wost case where pivot is always biggest or smallest element *)


let split l = let half_index = if (l != []) then List.length l / 2 else 0 in 
    let rec aux_split cur_index acc l = 
      match l with 
      | [] -> (List.rev acc, l) 
      | h::tail as cur_list -> if (cur_index < half_index) then aux_split (cur_index+1) (h::acc) tail 
      else (List.rev acc, cur_list)
    in aux_split 0 [] l;;


let merge l1 l2 = 
  let rec aux_merge l1 l2 acc = 
    match l1, l2 with 
    | [], [] -> List.rev acc 
    | h1::tail1 , [] -> aux_merge tail1 l2 (h1::acc)
    | [], h2::tail2 -> aux_merge l1 tail2 (h2::acc) 
    | h1::tail1, h2::tail2 -> if (h1 < h2) then aux_merge tail1 l2 (h1::acc) else
      aux_merge l1 tail2 (h2::acc)
  in aux_merge l1 l2 [];;



let rec merge_sort l = 
  match l with 
  | [] -> [] 
  | [x] -> l
  | h :: tail -> 
    let (left_half, right_half) = split l in 
    merge (merge_sort left_half) (merge_sort right_half);;