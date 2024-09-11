=> EXO 3.1:

fun x y -> (x, y)

-> type: 'a -> 'b -> 'a * 'b 

=> EXO 3.2:

let compose f g = fun x -> f (g x)

-> type: ('a -> 'b)->('c -> 'a)->'c->'b

=> EXO 3.4:

1) 'x', 2.1, (true, 0) = ('x', 2.1, (true, 0))

-> type: char * float * (bool * int)

2) 3 + 2, false || 2 = 3, "bonjour" = (5, false)

-> type: int * bool

3) let p = 1, 2 in snd p, fst p = (2, 1)

-> type: int * int

=> EXO 3.5:

1) int * bool * string

let x = 3, 2 != 2, "hello";;
-> x = (3, false, "hello");

2) (int * bool) * string

let x =  (3, false == true) , "hello"
-> x = ((3, false), "hello")

3) int * (bool * string)

let x = 5, (true, "hello")
x = (5, (true, "hello"))