#use "../stdlib/list.ml";;

-- Lists of lambdas.
let f = fun x -> 42;;
let g = fun x -> 13;;
let i = fun x -> x;;

let z1 = List rec(f, g; Nil);;
let z2 = List rec(f, g, i; Nil);;
let d = List rec(2, 4, 7, 11; Nil);;

let rec mapl fs l = match fs with(
      Nil -> Nil
    | List(f, xs) -> List(MList.map(f, l), mapl(xs, l))
);;

MList.map(f, d);;
mapl(z1, d);; 
mapl(z2, d);;
