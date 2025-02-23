#use "assert.ml"

(* Simple types. *)
let z1 : int = 3;;

Assert.eq (__typeof__ z1) "int"

(* Multiple brackets. *)
let z2 : int = 4;;

Assert.eq (__typeof__ z2) "int"

(* Tuples. *)
let z3 : int * int * int = 4, 5, 6;;

Assert.eq (__typeof__ z3) "int * int * int"

(* Functions. *)
let z4 (x : int * int) : int =
  match x with
  | x, y -> x
;;

Assert.eq (__typeof__ z4) "int * int -> int"

(* Higher order functions. *)
let z5 (f : int * int -> int) : int = f (4, 5);;

Assert.eq (z5 z4) 4;;
Assert.eq (__typeof__ z5) "(int * int -> int) -> int"

(* Functions with multiple parameters. *)
let z6 (x : int) (y : int) : int = x + y;;

Assert.eq (__typeof__ z6) "int -> int -> int"

(* Dereference. *)
let z7 x = !x;;

Assert.eq (__typeof__ z7) "'a ref -> 'a"

(* Application of operator. *)
let z8 = 3 + 4;;

Assert.eq (__typeof__ z8) "int";;

