(* Test some of the functions from the math module. *)
#use "assert.ml"

#use "math.ml"

let relu (x : 'a) : 'a = max 0 x
let x = 13;;

Assert.eq (relu x) 13

let rec f2 (x : 'a) : 'a = if x < 0 then f2 (x + 1) + 1 else relu x;;

Assert.eq (f2 (-x)) 13

let gcd = Math.gcd_rec;;

Assert.eq (gcd 17 4) 1;;
Assert.eq (gcd 16 4) 4

let fib = Math.fib_rec;;

Assert.eq (fib 0) 0;;
Assert.eq (fib 1) 1;;
Assert.eq (fib 2) 1;;
Assert.eq (fib 5) 5;;
Assert.eq (fib 9) 34
