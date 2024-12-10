(* Nom: *)
(* Prénom: *)
(* Groupe: INF501Ax *)

(*** Section 1: Préléminaires ***)
(** 1- remove_duplicates récursive terminale **)
(* let remove_duplicates l = *)
(* 'a list -> 'a list *)

(** 2- pairs_flatten récursive terminale **)
(* let pairs_flatten pairs = *)
(* ('a * 'a) list -> 'a list *)

(** 3- exists **)
(* let exists predicate l = *)
(* ('a -> bool) -> 'a list -> bool *)

(*** Section 2: Arcs ***)
type arc = A of int * int

(** 4- constructeur/accesseurs pour type arc **)
(* let make_arc origin extremity = *)
(* int -> int -> arc *)

(* let arc_origin arc = *)
(* arc -> int *)

(* let arc_extremity arc = *)
(* arc -> int *)

(** 5- reverse_arc **)
(* let reverse_arc arc = *)
(* arc -> arc *)

(** 6- pair_to_arc **)
(* let pair_to_arc pair = *)
(* int * int -> arc *)

(** 7- arc_to_pair **)
(* let arc_to_pair arc = *)
(* arc -> int * int *)  

(** 8- pairs_to_arcs **)
(* let pairs_to_arcs pairs = *)
(* (int * int) list -> arc list *)

(** 9- arcs_to_pairs arcs **)
(* let arcs_to_pairs arcs = *)
(* arc list -> (int * int) list *)

(*** Section 3: Graphes ***)
type graph = { arcs : arc list; isolated_nodes : int list }
(** 10- Constructeur/accesseurs pour le type graph **)

(* let make_graph arcs isolated_nodes = *)
(* arc list -> int list -> graph *)
  
(* let graph_arcs graph = *)
(* graph -> arc list *)
    
(* let graph_isolated_nodes graph = *)
(* graph -> int list *)

(** 11- **)
(* let mygraph = *)

(** 12- Visualisation - facultatif **)
(* #use "dot.ml" *)
(* graph_view mygraph *)

(** 13- nodes_from_arcs **)
(* let nodes_from_arcs arcs = *)
(* arc list -> int list *)

(** 14- graph_nodes **)
(* let graph_nodes graph = *)
(* graph -> int list *)

(** 15- node_neighbours **)
(* let node_neighbours node arcs = *)
(* int -> arc list -> int list *)

(** 16- mark_from_nodes *)
(* let mark_from_nodes nodes arcs marked = *)
(*  int list -> arc list -> int list -> int list *)

(** 17- mark_from_node **)
(* let mark_from_node node arcs = *)
(* int -> arc list -> int list *)

(** 18- unorient_arcs arcs **)
(* let unorient_arcs arcs = *)
(* arc list -> arc list *)

(** 19- graph_connected_p **)
(* graph_connected_p graph = *)
(* graph -> bool *)

(** 20- graph_has_circuit **)
(* let graph_has_circuit_p graph = *)
(* graph -> bool *)