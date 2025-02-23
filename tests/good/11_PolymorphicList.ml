#use "assert.ml"

#use "list.ml"

(* Lists of lambdas. *)
let f = fun x -> 42
let g = fun x -> 13
let i = fun x -> x
let z1 = [ f; g ]
let z2 = [ f; g; i ]
let d = [ 2; 4; 7; 11 ]

let rec mapl fs l =
  match fs with
  | [] -> []
  | f :: xs -> List.map f l :: mapl xs l
;;

Assert.eq (List.map f d) (List.init 4 f);;
Assert.eq (mapl z1 d) [ [ 42; 42; 42; 42 ]; [ 13; 13; 13; 13 ] ];;
Assert.eq (mapl z2 d) [ [ 42; 42; 42; 42 ]; [ 13; 13; 13; 13 ]; d ]
