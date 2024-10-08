type complex = {real: float; imag: float;}


let make_complex x y : complex= 
  {real= x; imag= y}


let c_sum c1 c2 = 
  {real = c1.real +. c2.real; imag = c1.imag +. c2.imag}

let c_dif c1 c2 = 
  {real = c1.real -. c2.real; imag = c1.imag -. c2.imag}

let c_abs c = 
  sqrt((c.real**2.)+.(c.imag**2.))


let c_exp c = 
  let r = c_abs c in 
  let theta = atan2 c.imag c.real in 
  r *. exp theta;;



type date = {
  day: int;
  month: int; 
  year: int;
}

let today = {day = 24; month = 9; year= 2024}

let date_infeg d1 d2 = 
  if (d1.year < d2.year) then true 
  else if (d1.year = d2.year && d1.month < d2.month) then true 
  else if (d1.year = d2.year && d1.month = d2.month && d1.day < d2.day) then true 
  else false;;

type 'a mylist = Nil | C of 'a * 'a mylist 

let mylist_length l =
  let rec aux_length l n =  
    match l with 
    | Nil -> n 
    | C(_, t) -> aux_length t (n+1) 
  in aux_length l 0;;

let interval_list n p =
  let rec aux_interval n p = 
    if (n > p) then Nil 
    else C(n, aux_interval (n+1) p)
  in aux_interval n p


let test_reverse = C(1, C(2, C(3, Nil)));;

let rec reverse_list l  =
  let rec aux_rev l acc = 
    match l with 
    | Nil -> acc 
    | C(x, t) -> aux_rev t (C(x, acc))
  in aux_rev l Nil


let rec mylist_length_term l = 
  let rec aux_term l acc = 
    match l with 
    | Nil -> acc 
    | C(_, t) -> aux_term t (acc + 1)
  in aux_term l 0;;  
  
(*

difference between terminal and non terminal recursive calls:

Terminal : last operation is the return of the recursive call (so non contruction or operation after receiving
a value). This is done using an auxaliary function and a accumulator.

Non-terminal : we can do operation with the result received from a previous recursive call 



*)



let interval_list_terminal n p = 
  let rec aux_interval n p acc = 
    if (n > p) then acc 
    else aux_interval (n+1) p (C(n, acc)) 
  in reverse_list(aux_interval n p Nil);; 


let rec map f l = 
  match l with 
  | Nil -> Nil 
  | C(x, t) -> C(f x, map f t);;


let rec filter pred l =
  match l with 
  | Nil -> Nil 
  | C(x, t) -> if (pred x) then C(x, filter pred t) else filter pred t;;


let fact n =
  let rec aux_fact n acc =
    if n = 0 then acc
    else aux_fact (n - 1) (n * acc)
  in
  aux_fact n 1;;

let sum n p = 
  let rec aux_sum n p acc = 
    if (n > p) then acc 
    else aux_sum (n+1) p (acc+n)
  in aux_sum n p 0;;


type peano = ZERO | SUCC of peano;;


let rec add m n = 
  match n with 
  | ZERO -> m
  | SUCC(x) -> SUCC(add m x)

let rec mult m n = 
  match n with 
  | ZERO -> ZERO
  | SUCC(x) -> add m (mult m x)


let int_of_peano m= 
  let rec aux_peano m acc = 
    match m with 
    | ZERO -> acc 
    | SUCC(x) -> aux_peano x (acc+1)
  in aux_peano m 0;;

let peano_of_int n =
  let rec aux_int n acc = 
    if (n <= 0) then acc 
    else aux_int (n-1) (SUCC(acc))
  in aux_int n ZERO;;