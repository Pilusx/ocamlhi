#use "../stdlib/assert.ml";;

-- Algebraic type definitions.
type i = I of int ;;
type `a e = E of `a ;;
type `a may = None | Just of `a;;
type (`a, `b) eth = Left of `a | Right of `b;;

-- Inline types.
None :: `a may ;;
Left(4) :: (int, `a) eth ;;
Right(5) :: (`a, int) eth ;;
Just(5) :: int may ;;

-- Complex pattern matching for user defined types.
let z = T(E(Left(Just(3))), E(Right(None)));;
z;;
Assert.eq(match z with(
      None, None -> 0
    | E(_), None -> 1
    | E(Left(Just(x))), E(Right(None)) -> (2 + x)
    | _ -> 3
), 5);;

-- Recursive types.
type `a list = List of `a * `a list | Nil;;
T(List rec (2; Nil), List(2, Nil));;
List rec(2, 3, 4; Nil);;

-- [ISSUE] Local type definitions override global type definitions.
-- Polymorphic zero-parameter constructors
val x1 of `a list;;
let x1 = Nil;;

val x2 of `a list list;;
let x2 = x1;;

val x3 of int list;;
let x3 = x1;;

Assert.eq(__typeof__ Nil, "`a list");;
Assert.eq(__typeof__ x1, "`a list");;

