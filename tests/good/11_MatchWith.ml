#use "../stdlib/assert.ml";;

let x = 3 ;;

-- Simple matching with blank.
Assert.eq(match x with (
      0 -> 0
    |  1 -> 1
    |  _ -> 2
), 2);;

-- Function call inside of the matching.
let f x = x;;
let n = 5;;

Assert.eq(match n with (
      0 -> 0
    | 1 -> 1
    | k -> (f((k-1)) + f((n-2)))
), 7);;

-- Multiple parameters.
let k = 13 ;;

Assert.eq(match n, k with (
      3, 0 -> 0
    | 3, 1 -> 1
    | 5, _ -> 13
    | _ -> -1
), 13);;

-- Tuple matching.
let v = T(n, k, 7) in Assert.eq(match v with(
      T(3, 0, 7) -> 0
    | T(k, n, 7) -> 13
    | T(n, k, 7) -> 14
    | _ -> -1
), 13);;

-- Matching in the anonymous functions.
let f = fun x, T(a, b) -> (x + (a*b));;
Assert.eq(f(1, T(2, 3)), 7);;