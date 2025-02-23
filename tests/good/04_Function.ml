(* Test some of the functions from the math module. *)
#use "assert.ml"

#use "math.ml"

(* Test void functions. *)
let f _ : unit = ();;

Assert.eq (f 3) ();;
Assert.eq (f id) ()

(* Test 1arg functions. *)
let f (x : 'a) : int = 3;;

let z = f 0 in
Assert.eq z 3
;;

let z = f 0 in
Assert.eq (z == 4) false
;;

let z = f 0 in
Assert.eq (z == 3) true

(* Test blanks. *)
let f (_ : 'a) : int = 3;;

Assert.eq (f 3) 3;;
Assert.eq (f 3.14) 3;;
Assert.eq (f false) 3

let gcd = Math.gcd;;

Assert.eq (gcd 100030 300) 10;;
Assert.eq (gcd 17 4) 1;;
Assert.eq (gcd 16 4) 4

let fib = Math.fib;;

Assert.eq (fib 0) 0;;
Assert.eq (fib 1) 1;;
Assert.eq (fib 2) 1;;
Assert.eq (fib 5) 5;;
Assert.eq (fib 19) 4181;;
Assert.eq (fib 36) 14930352

(* Mathematics is wrong. *)
(* Overloading default operators. *)

let add = ( + )

(* Remember default operator. *)
let ( + ) x y = x + y + 1;;

Assert.eq (2 + 2) 5;;
Assert.eq (add 2 2) 4

(* 3 argument operator. *)
let ( + ) x y z = x + 3;;

let f = 3 + 4 in
Assert.eq (f 5) 7
;;

Assert.eq (( + ) 3 4 5) 7
