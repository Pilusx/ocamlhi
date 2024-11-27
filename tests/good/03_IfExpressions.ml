#use "../stdlib/assert.ml";;

-- Test comparisons in the if statement
(13 < 15);;
let f b = if b then {13} else {15};;
Assert.eq(T(f(false), if (13 > 15) then {13} else {15}), T(15,15));;
Assert.eq(T(f(true), if (13 <= 15) then {13} else {15}), T(13,13));;

Assert.eq(if (13 > 15) then {14} else {17}, 17);;
let z = if (13 < 15) then {17} else {13} ;; 
Assert.eq(z, 17);;

-- Test static bindings in the if statements.
let zy = if (z > 17) then {(z * z)} else {(z - 3)};; 
Assert.eq(zy, 14);;

-- Test function calls.
let f1 x = if (x == 0) then {0} else {x};;
let y = 13;;
Assert.eq(f1(42), 42) ;;
Assert.eq(f1(y), 13);;
Assert.eq(f1((y + 29)), 42);;