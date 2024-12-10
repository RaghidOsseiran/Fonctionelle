# Exercice 1

l'operateur :: est utiliser O(len(l1)) fois.

soit a(n) le nombre d'apelle recursive:

1) 

-> a(0) = 0.
-> a(n) = 1 + a(n-1).

3) 

reverse [1; 2; 3]

1) append [3; 2] [1] => 3 :: append [2] [1] => 2 :: append [] [1] => 2 :: [1] : [2; 1] => 3 :: [2; 1] => [3; 2; 1].
2) append [3] [2] => 3 :: append [] [2] => [3; 2] 
3) append [] [3] => [3]
4) -> []

c(0) = 0. 
c(n) = c(n-1) + a(n-1).


# Exercice 2 

La fonction suivant ajoute les les n elements en tete de la liste 1 par 1 commencant par le premiere element a gauche de la liste gauche.