#use "../stdlib/list.ml";;

-- This test should throw a nice error.
-- [ISSUE] Unparametrized constructors should not allow brackets.
let z = Nil();;
let z2 = List (2, Nil);;