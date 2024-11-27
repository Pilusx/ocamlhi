#use "../stdlib/assert.ml";;

-- Test constant functions.
val f of int ;;
let f = 3;;
f;;

-- Test 1arg functions.
val f of `a -> int ;;
let f x = 3 ;;

let z = f(0) in Assert.eq(z, 3);;
let z = f(0) in Assert.eq((z == 4), false);;
let z = f(0) in Assert.eq((z == 3), true);;

-- Test blanks.
val f of `a -> int ;;
let f _ = 3 ;;

Assert.eq(f(3), 3);;
Assert.eq(f(3.14), 3);;
Assert.eq(f(false), 3);;

-- Test partial functions
val f of `a -> `a -> `a;;
let f x y = (x + y);;

val pf of int -> int;;
let pf = f(1);;

Assert.eq(pf(2), 3);;
Assert.eq(f(1, 2), 3);;

-- Test complex partial functions
val f of `a * `a -> (`a -> `a) -> `a;;
let f = fun T(x, y), z -> (z(x) + z(y));;

let pf = f(T(2, 3));;
Assert.eq(pf(fun x -> x), 5);;
Assert.eq(pf(fun x -> (2 * x)), 10);;

-- [ISSUE] Add pattern matching to ValueDefinition...
-- let f T(x, y) z = ...

-- [ISSUE] Void functions are not implemented.
-- Test void functions.
--val f of `a -> ();;
--let f _ = T() ;;
--T(f, function _ -> T());;
