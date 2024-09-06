(* EXO 2 *)



(* 1 *)

let test x y z = let somme = x + y in (z = somme);;

(* 2 *)


let valeur_p4 x = let p_4 y = x * x * x * x in ((p_4 x) * 3) + 7 * x - 1;; 

(* 3 *)

let polynome3 a b c = fun x -> ((a * (x*x)) + b*x + c);;

(* 4 Sur cahier *)

(* 5 *)

let rec pgcd n p = if (p = 0) then n else pgcd p (n mod p);;

(* 6 *)

let rec ack m n = 
  if (m = 0) 
    then n+1
  else if (m > 0 && n = 0)
    then ack (m-1) 1
  else
    ack (m-1) (ack m (n-1));; 

(* 7 *)

let rec fibo n = 
  if (n <= 1) 
    then 1
  else
    (fibo (n-1)) + (fibo (n-2));;

(* avec cette version de fibonacci, il y'a S(n) = S(n-1)+S(n-2)+1 avec S(n) le nb de somme effectuer lors de l'apelle a f(n) *)

let rec aux n = 
  if (n = 1)
    then (0, 1)
  else
    let (u_n, u_n1) = aux (n-1) in 
    (u_n1, u_n + u_n1);;

let fib n = 
  fst (aux n);;


(* 8 *)

let rec stairs n = 
  if (n <= 1)
    then 1 
  else (stairs (n-1)) + (stairs (n-2)) + (stairs (n-3));;


let trip_fst (x, _, _) = x;;


let rec stairs_improved n = 
  let rec aux n =
    if (n <= 1)
      then (1, 1, 0)
    else
      let (un, un_1, un_2) = aux (n-1) in 
      (un + un_1 + un_2, un, un_1)
  in (trip_fst(aux n));;  


(* 9 *)

let rec power b n = 
  if (n <= 0)
    then 1 
  else
    b * (power b (n-1));; 

(* Dans cette version on effectue O(n) multiplication *)

(* On peut faire mieux, on peut faire une fonction qui calcule la puissance d'un nombre avec une complexiter logarithmique *)

let rec power_log b n =
  if (n <= 0) 
    then 1 
  else
    let y = (power_log b (n/2)) in if (n mod 2 = 0) then y * y else b * y * y;; 


(* 10 *)

let rec iterate f k =
  let rec aux x init_k = 
    if (x = 0) 
      then init_k 
    else 
      aux(x-1)(f init_k)
  in aux k k;;

(* 11 *)

let multiplicateur n = fun i -> n * i;;


(* 12 *)




let rec seq_aux n = 
  match n with 
  | 0 -> (0, 3)
  | 1 -> let (u0, u1) = (0, 3) in 
         let u2 = -u1 + 2 * u0 in (u1, u2)
  | _ -> let (un_1, un) = seq_aux (n-1) in
         let un_1_2 = (-un) + (2 * un_1) in 
         (un, un_1_2);;


let seq n = fst(seq_aux n);;

(* La complexiter de cette fonction est lineaire, 0(n) *)

(* 13 *)

let derivee f h = let aux x = ((f(x+.h) -. f(x-.h)) /. 2.0 *.h) in aux;;



(* exo moodle *)

let rec sum n p = 
  if (n > p) then 0
  else n + sum (n+1) p;;


let rec op_prod n p f op neutral = 
  if (n > p)
    then neutral
  else
    (op) (f n) (op_prod (n+1) p f op neutral);;   




let convolution f g = 
  let rec aux_conv k n =
    if (n = 0) 
      then 0
    else
      (f k) * (g n) + (aux_conv (k+1)(n-1))
    in aux_conv 0;;  

    