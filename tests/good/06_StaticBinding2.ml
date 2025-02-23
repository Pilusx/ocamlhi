(* Stress test. *)
#use "assert.ml"

let x = 3
let x = 4
let y = 3 + x
let y = x
let y = 4
let f x = y
let y = 5
let f1 = f
let f g x n = g n x
let g x n = f1 x + n
let h x y = f g x y
let x = 31
let y = 14;;

Assert.eq (h x y) 35
