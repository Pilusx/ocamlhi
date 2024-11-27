-- [ISSUE] It should print the mismatched pattern. 
#use "../stdlib/assert.ml";;

-- Complex tuples.
type `a bigtuple = Tuple of (`a * `a) * (`a * `a);;
val x of int bigtuple;;
let x = Tuple(T(1, 2), T(3, 4));;
let f x = match x with(
  Tuple(a, b, c, d) -> (((a + b) + c) + d)
) in Assert.eq(f(x), 10);;