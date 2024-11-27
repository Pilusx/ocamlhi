#use "../stdlib/assert.ml";;

-- Simple types.
val z1 of int;;
let z1 = 3;;
Assert.eq(__typeof__ z1, "int");;

-- Multiple brackets.
val z2 of ((int));;
let z2 = 4;;
Assert.eq(__typeof__ z2, "int");;

-- Tuples.
val z3 of int * int * int;;
let z3 = T(4, 5, 6);;
Assert.eq(__typeof__ z3, "int * int * int");;

-- Functions.
val z4 of int * int -> int;;
let z4 x = match x with (
    T(x, y) -> x
);;
z4;;
Assert.eq(__typeof__ z4, "int * int -> int");;

-- Higher order functions.
val z5 of (int * int -> int) -> int;;
let z5 f = f(T(4,5));;
z5;;
Assert.eq(z5 (z4), 4);;
Assert.eq(__typeof__ z5, "(int * int -> int) -> int");;

-- Functions with multiple parameters.
val z6 of int -> int -> int;;
let z6 x y = (x + y);;
z6;;
Assert.eq(__typeof__ z6, "int -> int -> int");;

-- Dereference.
let z7 x = !x;;
z7;;
Assert.eq(__typeof__ z7, "`a ref -> `a");;

-- Application of operator.
let z8 = (+)(3, 4);;
z8;;
Assert.eq(__typeof__ z8, "int");;

-- Operators in brackets.
(+);;
Assert.eq(__typeof__ (+), "int -> int -> int");;
