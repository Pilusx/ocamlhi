-- Test the stdlib implemenation of maybe.
#use "../stdlib/assert.ml";;
#use "../stdlib/maybe.ml";;

let j = Maybe.just(13);;
Assert.eq(j, Just(13));;
let no = Maybe.nothing;;
Assert.eq(no, Nothing);;

let plus4 = fun x -> (x + 4);;
let one = fun _ -> 1;;

Maybe.either;;
let f1 x = Maybe.either(42, plus4, x);;
Assert.eq(f1(j), 17);;
Assert.eq(f1(no), 42);;

let f y m = Maybe.either(-42, y, m);;
Assert.eq(f(plus4, j), 17);;
Assert.eq(f(plus4, no), -42);;

let g m = f (one, m);;
Assert.eq(g(j), 1);;
Assert.eq(g(no), -42);;

-- Type deduction
Assert.eq(__typeof__ plus4, "int -> int");;
Assert.eq(__typeof__ j, "int may");;
Assert.eq(__typeof__ f, "(`a -> int) -> `a may -> int");;
