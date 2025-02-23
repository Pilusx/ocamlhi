#use "assert.ml"

#use "list.ml"

let plus3 = ( + ) 3;;

Assert.eq (plus3 39) 42

(* Test complex partial functions *)
let f : 'a * 'a -> ('a -> 'a) -> 'a = fun (x, y) z -> z x + z y
let pf = f (2, 3);;

Assert.eq (pf id) 5;;
Assert.eq (pf (fun x -> 2 * x)) 10

let f = ( * )
let row f m i = List.init m (f i)
let col f n = List.init n (row f n);;

Assert.eq (row f 4 1) [ 0; 1; 2; 3 ];;
Assert.eq (col f 4) [ [ 0; 0; 0; 0 ]; [ 0; 1; 2; 3 ]; [ 0; 2; 4; 6 ]; [ 0; 3; 6; 9 ] ]
