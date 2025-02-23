#use "assert.ml"

#use "list.ml"

let x = List.init 11 id
let x2 = List.map (fun x -> x * x) x
let sum = List.fold_left ( + ) 0 x
let sum2 = List.fold_left ( + ) 0 x2;;

Assert.eq sum 55;;
Assert.eq sum2 385
