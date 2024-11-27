-- Test the stdlib implementation of pair.
#use "../stdlib/assert.ml";;
#use "../stdlib/pair.ml";;

val fst of `a -> `b -> `a ;;
val snd of `a -> `b -> `b ;;
let fst x y = x ;;
let snd x y = y ;;

Assert.eq(fst((17 - 3), 4), 14);;
Assert.eq(snd(3, (18 + 24)), 42);;

-- Check that signatures have not changed...
Assert.eq(__typeof__ fst, "`a -> `b -> `a");;
Assert.eq(__typeof__ snd, "`a -> `b -> `b");;

let z = Pair(3.14, 4);;
z;;
Assert.eq(MPair.first(z), 3.14);;
Assert.eq(MPair.second(z), 4);;
Assert.eq(MPair.pair(1, 2), Pair(1, 2))