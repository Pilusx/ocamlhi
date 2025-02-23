#use "assert.ml"

(* Simple division test. *)
let f : int = 3
let z : int = 1;;

Assert.eq (f / z) 3

let ( <*> ) (x : 'a) (y : 'a) = x + y;;

Assert.eq (3 <*> 4) 7

let ( >>> ) = ( - );;

Assert.eq (1 >>> -41) 42

let z1 = 13 < 15;;

Assert.eq z1 true

let z2 = false <> true;;

Assert.eq z2 true

(* Test prefix and infix operators inside functions. *)
let f1 (x : 'a) : 'a = not x
let f2 (x : 'a) : 'a = x *. x
let f3 (f : int) (z : int) : int = f / z;;

Assert.eq (id 1) 1;;
Assert.eq (id 3.14) 3.14;;
Assert.eq (f1 false) true;;
Assert.eq (f1 true) false;;
f2 3.14;;
Assert.eq (f3 3 2) 1

(* Test complex multiparameter function. *)
let f x1 x2 x3 x4 x5 x6 = ((x6 * x5) - x4 + x3) / x2 % x1;;

Assert.eq (f 3 4 5 6 7 8) 1;;
Assert.eq (__typeof__ f) "int -> int -> int -> int -> int -> int -> int"

(* Here the function should not be evaluated. *)
let r = if true then 1 else f3 3 0;;

Assert.eq r 1
