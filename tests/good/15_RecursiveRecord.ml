#use "assert.ml"

(* Multiobject, polimorphic record. *)
type message =
  { mutable value : int
  ; value2 : float
  ; value3 : string
  }

type 'a status =
  { hehe : float
  ; mutable msg : 'a
  }

type 'a wrapper = { mutable status : 'a }

(* Recursive constructor *)
let x = { status = { hehe = 3.14; msg = { value = 3; value2 = 4.15; value3 = "msg" } } };;

Assert.eq (__typeof__ x) "message status wrapper"

(* Recursive getter *)
let z : string = x.status.msg.value3;;

Assert.eq z x.status.msg.value3;;

(* Recursive setter *)
Assert.eq x.status.msg.value 3;;
x.status.msg.value <- 4;;
Assert.eq x.status.msg.value 4
