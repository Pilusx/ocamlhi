#use "assert.ml"

(* Simple one-level record. *)
(* Constructors *)
type t =
  { a : int
  ; b : int
  ; c : int
  }

let x = { a = 3; b = 4; c = 5 };;

Assert.eq (__typeof__ x) "t"

(* Pattern matching. *)
let f x =
  match x with
  | { a = a_; b = b_; c = _ } -> a_ + b_
;;

Assert.eq (__typeof__ f) "t -> int"

(* Setters *)
type r = { mutable d : int }

let x = { d = 4 };;

Assert.eq x.d 4;;
x.d <- 5;;
Assert.eq x.d 5

(* Reference to mutable record. *)
type s =
  { mutable e : int
  ; f : int
  }

let x : s ref = ref { e = 0; f = 1 };;

x := { e = 3; f = 4 };;
x.contents.e <- 6;;

(* [ISSUE] !x.a means (!x).a not !(x.a). *)
Assert.eq
  (let y = !x in
   y.e)
  6
;;

Assert.eq x.contents.e 6;;
Assert.eq x.contents.f 4

(* Local references. *)
type u = { mutable g : int }

let f a =
  a.contents.g <- a.contents.g + 1;
  a
;;

Assert.eq (__typeof__ f) "u ref -> u ref"

let a = ref { g = 6 };;

Assert.eq
  (let a = f a in
   a.contents.g)
  7
