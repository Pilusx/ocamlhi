#use "assert.ml"

let x = 3;;

(* Simple matching with blank. *)
Assert.eq
  (match x with
   | 0 -> 0
   | 1 -> 1
   | _ -> 2)
  2

(* Function call inside of the matching. *)
let f x = x
let n = 5;;

Assert.eq
  (match n with
   | 0 -> 0
   | 1 -> 1
   | k -> f (k - 1) + f (n - 2))
  7
