(* Exo 3.3 *)


let rec fibo_aux n = 
  if (n = 0) 
    then (1, 1) 
  else 
    let (u_n, u_n1) = fibo_aux (n-1) in 
    (u_n1, u_n + u_n1);;


let fibo n = fst (fibo_aux n);;

(* Exo 3.6 *)

(* type couleur = Pique | Coeur | Carreau | Trefle;; *)

(* type carte =
As of couleur
| Roi of couleur
| Dame of couleur
| Valet of couleur
| Numero of int * couleur;;


let couleur_carte crt = 
  match crt with
  | As(c) | Roi(c) | Dame(c) | Valet(c) | Numero(_, c) -> c

let est_de_couleur crt col = (couleur_carte crt == col);;

let est_figure crt = 
  match crt with 
  | Dame(_) | Roi(_) | Valet(_) -> true 
  | _ -> false *)


(* EXO 3.7 *)

type carburant = Diesel | Essence | Electrique;;
type vehicle = int * carburant;;

let make_vehicle (carb : carburant) (value : int) : vehicle = (value, carb);;

let carburant_of (v : vehicle) = 
  match v with
  | (_, c) -> c;;

let nb_wheels_of (v : vehicle) = 
  match v with
  | (w, _) -> w;;

let can_run (v : vehicle) = 
  match v with 
  | (w, Diesel) -> (w < 4)
  | _ -> true;;



let calc_consumption (v: vehicle) n = 
  let n = float_of_int n in 
  match v with 
  | (_, Electrique) -> (n *. 10.0) /. 100.0 
  | (_, Diesel) -> (n *. 6.0) /. 100.0
  | (_, Essence) -> (n *. 8.0) /. 100.0;;
 
let consomation (v : vehicle) n = 
  let calculated_c = calc_consumption v n in 
  match v with 
  | (_, Electrique) -> calculated_c *. 0.25
  | (_, Diesel) | (_, Essence) -> calculated_c *. 1.5;;


(* EXO 3.8 *)

type expr = 
  Var of string 
  | Number of float 
  | Plus of expr * expr 
  | Minus of expr * expr 
  | Mult of expr * expr 
  | Exp of expr;;


let vx = Var("x");;
let vy = Var("y");;

let e1 = Plus(Mult(Number(2.0), vx), Number(1.0));;
let e2 = Plus(Mult(Number(3.0), Mult(vx, vx)), Plus(Mult(Number(2.0), vx), Number(1.0)));;

let get_val v = 
  match v with 
 | Var(s) -> s;;

let rec derive (var : expr) expr =
  match expr with
  | Var x -> if x = get_val vx then Number 1.0 else Number 0.0
  | Number _ -> Number 0.0
  | Plus (e1, e2) -> Plus (derive var e1, derive var e2)
  | Mult (e1, e2) -> Plus (Mult (derive var e1, e2), Mult (e1, derive var e2))
  | Exp e -> Mult (derive var e, Exp e);;


let rec derivee_n var expr n =
  if n = 0 then expr
  else derivee_n var (derive var expr) (n - 1)



(* Moodle *)

type region = Medoc | Graves | Alsace | Beaujolais | Touraine | Bourgogne 
type couleur = Blanc | Rouge | Rose
type vin = region * couleur * int


let region_of (vine :vin) = 
  match vine with 
  | (r, _, _) -> r;;

let color_of (vine :vin) = 
  match vine with 
  | (_, c, _) -> c;;

let year_of (vine :vin) = 
  match vine with 
  | (_, _, y) -> y;;
  

let bordeaux_p vine = 
  let region_v = region_of vine in (region_v = Medoc || region_v = Graves);;

let has_color_p vine color =
  let color_v = color_of vine in (color_v = color);;

let after_year_p vine year = 
  let year_v = year_of vine in (year_v >= year);;