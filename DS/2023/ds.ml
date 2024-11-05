(*

Exo 1)

1) Incorrect car int + float. 
2) 9 <int>
3) 15 <int>
4) 13 <int>
5) int -> int -> int * int <func>
6) int -> float -> int
7) Erreur premier parameter attendue est de type int. 

Exo 2)

8) fun x y -> float_of_int x +. y;;
9) fun c -> let (v1, v2) = c in (v1 = v2);;
10) fun x f -> if (f x) then 1 else 0;;


*)


(* Exo 3*)

let rec pgcd a b = 
  if (a = b && a = 0) 
    then failwith "pgcd (0,0)" 
    else 
      if (b = 0)
        then a 
        else pgcd b (a mod b);;


(* Exo 4 *)

type ratio = Ratio of int * int;;

let make_ratio n d = 
  if (d = 0) then failwith "div by zero"
  else let p = pgcd n d in 
  Ratio (n/p, d/p);;


let numerateur = function
   | Ratio(v, _) -> v;;

let denominateur = function 
  | Ratio(_, v) -> v;;

let float_of_ratio r = (float_of_int (numerateur r) /. float_of_int(denominateur r));;

let ratio_infeg r1 r2 = (float_of_ratio r1 <= float_of_ratio r2);;

let ratio_sum r1 r2 = make_ratio(numerateur r1*denominateur r2 + numerateur r2*denominateur r1)(denominateur r1 * denominateur r2);;


(* Exo 5 *)

type rset = ratio -> bool;;

let rset_empty : rset = fun r -> false;;

let rset_member r rset = rset r;;

let rset_full : rset = fun r -> true;;

let rset_make_interval (r1: ratio) (r2:ratio) : rset = 
  fun r -> (not(ratio_infeg r r1) && ratio_infeg r r2);;

let rset_make_singleton r = 
  fun x -> (float_of_ratio x = float_of_ratio r);;


let rset_complement (rset:rset) : rset = 
  fun x -> not (rset_member x rset);;

let rset_intersectio (rset1:rset) (rset2:rset) :rset =
  fun r -> (rset_member r rset1 && rset_member r rset2);;

let rset_remove_if_pred pred rset = 
  fun r -> if (rset_member r rset) 
            then not(pred r)
            else false;;

let priv_entier_rset = fun r -> (denominateur r != 1);;

let rset_find_smaller_int rset n = 
  let rec aux_find i = 
    match i with 
    | 0 -> n 
    | _ -> if (rset_member (make_ratio i 1) rset) then i else aux_find (i-1) 
  in aux_find (n-1);;