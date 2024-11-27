-- Test some of the functions from the math module.
#use "../stdlib/assert.ml";;
#use "../stdlib/math.ml";;

val relu of `a -> `a;;
let relu x = Math.max(0, x);;

let x = 13;;
Assert.eq(relu(x), 13);;

val f2 of `a -> `a;;
let rec f2 x = if (x < 0) then {(f2((x + 1)) + 1)} else {relu(x)} ;;

Assert.eq(f2(-x), 13);;

let gcd = Math.gcd;;
Assert.eq(gcd(100030, 300), 10);;
Assert.eq(gcd(17, 4), 1);;
Assert.eq(gcd(16, 4), 4);;

let fib = Math.fib;;
let z = fib(0) in Assert.eq(z, 0);;
let z = fib(1) in Assert.eq(z, 1);;
let z = fib(2) in Assert.eq(z, 1);;
let z = fib(5) in Assert.eq(z, 5);;
let z = fib(19) in Assert.eq(z, 4181);;
let z = fib(36) in Assert.eq(z, 14930352);;
let z = fib(299) in Assert.eq(z, 137347080577163115432025771710279131845700275212767467264610201);;

-- Recursive functions
let gcd = Math.gcd_rec;;
let z = gcd(17, 4) in Assert.eq(z, 1);;
let z = gcd(16, 4) in Assert.eq(z, 4);;

let fib = Math.fib_rec;;
let z = fib(0) in Assert.eq(z, 0);;
let z = fib(1) in Assert.eq(z, 1);;
let z = fib(2) in Assert.eq(z, 1);;
let z = fib(5) in Assert.eq(z, 5);;
let z = fib(9) in Assert.eq(z, 34);;