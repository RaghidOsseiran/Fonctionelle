let make_list n e = 
  let rec aux_make_list acc n =
    match n with 
    | 0 -> acc 
    | _ -> aux_make_list (e::acc) (n-1)
  in aux_make_list [] n;;

let make_list_aux n e = List.init n (fun _ -> e);;

let make_zeros n = make_list n 0;;

let list = [2;4;6;8];;

let get_nth l i = 
  let rec aux_get_nth l inc = 
    match l with
    | [] -> failwith "hd"
    | h::tail -> if (inc = i) then h else aux_get_nth tail (inc+1)
  in aux_get_nth l 0;;


let set_nth l i e = 
  let rec aux_set_nth l inc acc =
    match l with 
    | [] -> failwith "tl"
    | h::tail -> if (inc = i) then (List.rev acc) @ (e::tail) else aux_set_nth tail (inc+1) (h::acc)
  in aux_set_nth l 0 [];; 


let get_nth_opt l i = 
  let rec aux_get_opt l inc = 
    match l with 
    | [] -> None 
    | h::tail -> if (inc = i) then Some h else aux_get_opt tail (inc+1)
  in aux_get_opt l 0;;


type grid = int list list;;


let make_empty_grid_size size = 
  if (size = 0) then [] else 
  let rec aux_make_empty acc inc = 
    if (inc = size) then (make_zeros size::acc) else aux_make_empty (make_zeros size ::acc) (inc+1)
  in aux_make_empty [] 1;;

let grid_size grid = 
  match grid with 
  | [] -> 0
  | h::tail -> List.length h;;


let correct_z_p grid z = (z < grid_size grid);;

let correct_coor_p grid li co = correct_z_p grid li && correct_z_p grid co;;

let test_grid = make_empty_grid_size 3;;

let get_square grid line column = 
  if (correct_coor_p grid line column) then 
    let rec aux_get_square grid inc = 
      match grid with 
      | [] -> failwith "invalid coord"
      | h::tail -> if (inc = line) then get_nth h column else aux_get_square tail (inc+1)
    in
    aux_get_square grid 0
  else failwith "invalid coordinates";;


let set_square grid line column n = 
  if (correct_coor_p grid line column) then 
    let rec aux_set_square grid inc acc = 
      match grid with 
      | [] -> failwith "invalid coord"
      | h::tail -> if (inc = line) then List.rev(acc) @ (set_nth h column n :: tail) else aux_set_square tail (inc+1) (h::acc)
    in
    aux_set_square grid 0 []
  else failwith "invalid coordinates";;

let test_grid = set_square test_grid 1 2 5;;
let test_grid = set_square test_grid 2 1 8;;


let pred_square_p pred grid line column = pred (get_square grid line column);;

let test_pred = pred_square_p (fun x -> x mod 2 =0) test_grid 1 2;;
let test_pred2 = pred_square_p (fun x-> x > 4) test_grid 1 2;;

let zero_square_p grid line column = pred_square_p (fun x -> x = 0) grid line column;;


let grid_map f grid = 
  let rec aux_grid grid acc =
    match grid with 
    | [] -> acc 
    | h::tail -> aux_grid tail ((List.map f h)::acc) 
  in List.rev(aux_grid grid []);;