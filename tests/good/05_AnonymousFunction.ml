#use "assert.ml"

let f = fun x -> 42
let g = fun x y t _ -> 13
let i = fun x -> x;;

Assert.eq (f ()) 42;;
Assert.eq (g "kot" "ma" "ale" ()) 13;;
Assert.eq (i 715) 715

(* Matching in the anonymous functions. *)
let f = fun x (a, b) -> x + (a * b);;

Assert.eq (f 1 (2, 3)) 7
