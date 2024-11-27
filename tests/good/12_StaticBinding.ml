#use "../stdlib/assert.ml";;

-- Binding of global variables.
let x = 1;;
let z1 = T(x, (x + 1));;
let x = 3 in Assert.eq(z1, T(1, 2));;

-- Definition time evaluation.
let f _ = x;;
f;;

Assert.eq(f(3), 1);;

-- Binding functions.
let id x = x;;
let f2 = id;;

let x = 100 in Assert.eq(f(9999), 1);;
let x = 100 in Assert.eq(f(x), 1);;
let x = 100 in Assert.eq(f2(x), 100);;

-- Binding recursive functions.
let y = 1;;
let rec f3 x = if (x > 0) then {f3((x-1))} else {y};;
Assert.eq(f3(13), 1);;

-- Binding operators and functions.
let f4 x y = (x + y);;
let g f x = f(x, 37);;
Assert.eq(g(f4, 5), 42);;