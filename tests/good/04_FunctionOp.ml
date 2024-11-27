#use "../stdlib/assert.ml";;

-- Test prefix and infix operators inside functions.
val id of `a -> `a;;
val f1 of `a -> `a;; 
val f2 of `a -> `a;;
val f3 of int -> int -> int;;

let id x = x ;;
let f1 x = not(x) ;;
let f2 x = (x *. x) ;;
let f3 f z = (f / z);;

id(1);;
id(3.14);;
f1(false);;
f1(true);;
f2(3.14);;
Assert.eq(f3(3, 2), 1);;

-- Test complex multiparameter function.
val f of `a -> `a -> `a -> `a -> `a -> `a -> `a ;;
let f x1 x2 x3 x4 x5 x6 = (((((x6 * x5) - x4) + x3) / x2) mod x1) ;;
f(3,4,5,6,7,8);;
Assert.eq(__typeof__ f, "int -> int -> int -> int -> int -> int -> int");;

-- Here the function should not be evaluated.
let r = if true then {1} else {f3(3, 0)} ;;
Assert.eq(r, 1);;