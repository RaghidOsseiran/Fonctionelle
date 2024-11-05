(*

Exo 1)

1) 7 <int>
2) 7. <float>
3) int-> int <func>
4) int -> int * int
5) int -> int -> int
6) val f: int -> int -> int
7) int -> int
8) 13 <int>


*)

(* Exo 2 *)

let rec pgcd x y = 
  if (x = y) then x
  else if (x > y) then pgcd (x-y) y 
  else pgcd x (y-x);;

(* Exo 3 *)

type 'a mylist = Nil | C of 'a * 'a mylist;;

let rec sequence_from_i pred i n = 
  if (n = 0) then Nil 
  else if (pred i) then C(i, sequence_from_i pred (i+1) (n-1))
  else sequence_from_i pred (i+1) (n);;


let sequence pred n = sequence_from_i pred 0 n;;

(* Exo 4, eniter pos ou neg *)

let i_member i set = set i;;
let i_empty = fun i -> false;;
let i_evens = fun i -> i mod 2 = 0;; (* type: int-> bool *)

let i_odds = fun i -> not(i_member i i_evens);;

let i_union s1 s2 = fun i -> i_member i s1 || i_member i s2;;

(*

a) retourn false. 
b) i_member 2 i_evens retourn true. 

*)

let i_integers = fun (i: int) -> true;;

let multiple_of n = fun i -> if (pgcd i n = n) then true else false;;

let i_complement set = 
  fun i -> not(i_member i set);;

let i_intersection set1 set2 = 
  fun i -> (i_member i set1 && i_member i set2);;

let i_sequence set n = 
  sequence set n;;


