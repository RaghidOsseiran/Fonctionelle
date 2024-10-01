type couple = C of int * int 

let make_couple x y = C(x, y)

let couple_fst (c: couple) = 
  match c with
  | C(x, _) -> x



let couple_snd (c: couple) = 
  match c with
  | C(_, y) -> y


let couple_add c1 c2 = make_couple (couple_fst c1 + couple_fst c2) (couple_snd c1 + couple_snd c2)


let couple_sum c1 = couple_fst c1 + couple_snd c1


let pair_of_couple c = (couple_fst c, couple_snd c)



(* let test = assert((couple_add (make_couple 1 2) (make_couple 1 2)) = make_couple 2 4) *)

type couple = bool -> int

let make_couple x y = fun b -> if b then x else y

(* Basically calling make_couple x y return a function that take a boolean and returns an integer based on the value of the bool*)

let couple_fst c = c true

let couple_snd c = c false 

(*Oui, les fonction couple_sum, couple_add, et pair_of_couple reste valables avec le nouveau type*)



let couple_add c1 c2 = make_couple (couple_fst c1 + couple_fst c2) (couple_snd c1 + couple_snd c2)


let couple_sum c1 = couple_fst c1 + couple_snd c1


let pair_of_couple c = (couple_fst c, couple_snd c)

let compare_couple c1 c2 = (couple_fst c1 = couple_fst c2 && couple_snd c1 = couple_snd c2);;
let test = assert(compare_couple((couple_add (make_couple 1 2) (make_couple 1 2))) (make_couple 2 4))


(* let test = assert((couple_add (make_couple 1 2) (make_couple 1 2)) = make_couple 2 4) *)