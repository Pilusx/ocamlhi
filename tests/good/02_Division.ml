#use "../stdlib/assert.ml";;

-- Simple division test.
val f of int;;
let f = 3;;
val z of int;;
let z = 1;;
Assert.eq((f / z), 3) ;;

-- Branches of the if statement should be evaluated lazily.
let r = if true then {1} else {(f / 0)} ;;
Assert.eq(r, 1);;
