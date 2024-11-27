#use "../stdlib/assert.ml";;

-- Test most common infix operators.
let n = 3;;
Assert.eq((n-1), 2);;
let k = 4;;
let z = T(n, k);;
let pi = T(3, 14);;
let k3 = T(n, n, n);;
let k5 = T(n, (n - 1), (n + 2), (n / 3), (n * 4));;
let c4 = T(3, (3 < 4), (3 == 4), (3 > 4));;

Assert.eq(n, 3);;
Assert.eq(k, 4);;
Assert.eq(z, T(3, 4));;
Assert.eq(pi, T(3, 14));;
Assert.eq(k3, T(3, 3, 3));;
Assert.eq(k5, T(3, 2, 5, 1, 12));;
Assert.eq(c4, T(3, true, false, false));;
