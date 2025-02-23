(* Field `a` is not marked as mutable *)
type record = { a : int }

let x = { a = 3 };;

x.a <- 4
