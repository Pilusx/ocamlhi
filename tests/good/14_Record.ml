-- [ISSUE] Fix variable signatures.
#use "../stdlib/assert.ml";;

-- Simple one-level record.
-- Constructors
type t1 = {a: int; b:int; d:int};;
type t2 = {a: int; b:int; c:int};;

let x1 = {a = 3; b = 4; d = 5};;
let x2 = {a = 1; b = 2; c = 3};;

Assert.eq(__typeof__ x1, "t1");;
Assert.eq(__typeof__ x2, "t2");;

-- Getters
let f1 x = (x.a + x.b);;
let f2 x = ((x.a + x.b) + x.c);;
let f3 x = ((x.d + x.a) + x.b);;

Assert.eq(__typeof__ f1, "t2 -> int");;
Assert.eq(__typeof__ f2, "t2 -> int");;
Assert.eq(__typeof__ f3, "t1 -> int");;

Assert.eq(x1.a, 3);;
Assert.eq(x2.a, 1);;
Assert.eq(f1(x2), 3);;
Assert.eq(f2(x2), 6);;
Assert.eq(f3(x1), 12);;

-- Pattern matching.
let f1 x = match x with(
  {a = a_; b = b_; d = _} -> (a_ + b_)
);;
Assert.eq(__typeof__ f1, "t1 -> int");;

let f2 x = match x with(
  {a = a_; b = b_; c = c_} -> (a_ + b_)
);;
Assert.eq(__typeof__ f2, "t2 -> int");;

-- Setters
type t3 = {mutable a: int};;
let x3 = {a = 4};;
Assert.eq(x3.a, 4);;
-- [ISSUE] It should work without begin .. end.
begin x3.a <- 5 end;;
Assert.eq(x3.a, 5);;

-- Reference to mutable record.
type hehe = {mutable a : int; b : int};;
-- [ISSUE] Temporary. It should be let x : hehe ...
val x of hehe ref;; 
let x = ref ({a = 0; b = 1});;
(x := {a=3; b = 4});;
-- [ISSUE] It is parsed as EOp2.
-- (x.contents.a <- 6);;
begin x.contents.a <- 6 end;;
!x;;
-- [ISSUE] !x.a means (!x).a not !(x.a).
-- Assert.eq(!x.a, 6);;
Assert.eq(x.contents.a, 6);;
Assert.eq(x.contents.b, 4);;

-- Local references.
type t4 = {mutable value : int};;
let a = {value = 6};;
Assert.eq(a.value, 6);;

let f a = begin a.contents.value <- 7; a end;;
Assert.eq(__typeof__ f, "t4 ref -> t4 ref");;

f(ref(a));;
-- [ISSUE] f(a).contents.value should work.
-- a := !f(a) ?? Does it work?
Assert.eq(a.value, 7);;

-- Multiobject, polimorphic record.
type message = {mutable value : int; value2 : float; value3 : string};;
type `a status = {hehe : float; mutable msg : `a};;
type `a wrapper = {mutable status: `a};;

-- Recursive constructor
let x  = {
  status = {
    hehe = 3.14;
    msg = {
      value = 3;
      value2 = 4.15;
      value3 = "msg"
    }
  }
};;
Assert.eq(__typeof__ x, "message status wrapper");;

-- Recursive getter
val z of string;;
let z = x.status.msg.value3;;
Assert.eq(z, x.status.msg.value3);;

-- Recursive setter
Assert.eq(x.status.msg.value, 3);;
begin x.status.msg.value <- 4 end;;
Assert.eq(x.status.msg.value, 4);;
x;;

-- [ISSUE] Mixing trees with records...
