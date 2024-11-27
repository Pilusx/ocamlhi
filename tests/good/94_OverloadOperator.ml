-- Additional user-defined operators.
#use "../stdlib/assert.ml";;
#use "../stdlib/list.ml";;

val (<*>) of `a -> `a -> `a;;
let (<*>) x y = (x + y);;

Assert.eq((3 <*> 4), 7);;

val (>>>) of `a -> `a -> `a;;
let (>>>) x y = (x - y);;

Assert.eq((1 >>> -41), 42);;

-- Overloading operators for complex types
val (<*>) of `a list -> `a list -> `a list;;
let rec (<*>) s1 s2 = match s1 with(
      Nil -> s2
    | List(x, Nil) -> List(x, s2)
    | List(x, xs) -> List(x, (xs <*> s2))
);;

Assert.eq((List rec(3, 4, 5;Nil) <*> List rec (6;Nil)), List rec (3,4,5,6;Nil));;

-- Mathematics is wrong.
-- Overloading default operators.

let add = (+);; -- Remember default operator.
let (+) x y = ((x + y) + 1);;
Assert.eq((2 + 2), 5);;
Assert.eq(add(2,2), 4);;

-- 3 argument operator.
let (+) x y z = (x + 3);;
let f = (3 + 4) in Assert.eq(f(5), 7);;
Assert.eq((+)(3,4,5), 7);;
