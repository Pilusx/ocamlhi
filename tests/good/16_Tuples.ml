#use "assert.ml"

(* Test most common infix operators. *)
let n = 3;;

Assert.eq (n - 1) 2

let k = 4
let z = n, k
let pi = 3, 14
let k3 = n, n, n
let k5 = n, n - 1, n + 2, n / 3, n * 4
let c4 = 3, 3 < 4, 3 == 4, 3 > 4;;

Assert.eq n 3;;
Assert.eq k 4;;
Assert.eq z (3, 4);;
Assert.eq pi (3, 14);;
Assert.eq k3 (3, 3, 3);;
Assert.eq k5 (3, 2, 5, 1, 12);;
Assert.eq c4 (3, true, false, false)

(* Multiple parameters. *)
let n = 5
let k = 13;;

Assert.eq
  (match n, k with
   | 3, 0 -> 0
   | 3, 1 -> 1
   | 5, _ -> 13
   | _ -> -1)
  13
;;

(* Tuple matching. *)
let v = n, k, 7 in
Assert.eq
  (match v with
   | 3, 0, 7 -> 0
   | k, n, 7 -> 13
   | n, k, 7 -> 14
   | _ -> -1)
  13

(* Pattern matching in definition *)
let f (x, y) = x + y;;

Assert.eq (f (2, 3)) 5
