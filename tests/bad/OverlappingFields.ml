(* [ISSUE] (Out of scope) Fix me. *)
#use "assert.ml"

(* Simple one-level record. *)
(* Constructors *)
type t1 =
  { a : int
  ; b : int
  ; d : int
  }

type t2 =
  { a : int
  ; b : int
  ; c : int
  }

let x1 = { a = 3; b = 4; d = 5 }
let x2 = { a = 1; b = 2; c = 3 };;

Assert.eq (__typeof__ x1) "t1";;
Assert.eq (__typeof__ x2) "t2"

(* Getters *)
let f1 x = x.a + x.b
let f2 x = x.a + x.b + x.c
let f3 x = x.d + x.a + x.b;;

Assert.eq (__typeof__ f1) "t2 -> int";;
Assert.eq (__typeof__ f2) "t2 -> int";;
Assert.eq (__typeof__ f3) "t1 -> int";;
Assert.eq x1.a 3;;
Assert.eq x2.a 1;;
Assert.eq (f1 x2) 3;;
Assert.eq (f2 x2) 6;;
Assert.eq (f3 x1) 12
