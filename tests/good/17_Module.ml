(* Tests the implemenation of modules and module signatures. *)
#use "assert.ml"

module type SALG = sig
  val x : int ref
  val f : int -> int
end

(* In-module variable binding. *)
module ALG : SALG = struct
  let x = ref 5
  let y = 3

  (* Additional private definition. *)
  let f m = m + !x + y
end
;;

(* Calling functions from modules. *)
Assert.eq (ALG.f 1) 9;;

(* Modifing variables in modules. *)
ALG.x := 7;;
Assert.eq (ALG.f 1) 11;;

(* Check signatures *)
Assert.eq (__typeof__ ALG.x) "int ref";;
Assert.eq (__typeof__ ALG.f) "int -> int"

(* Test module signatures. *)
module type SInt = sig
  val compare : int -> int -> bool
end

module MInt : SInt = struct
  let compare = ( == ) (* It has type 'a -> 'a -> bool. *)
end
;;

Assert.eq (__typeof__ MInt.compare) "int -> int -> bool"

(* Modules in modules *)
let x = 3

module A = struct
  let x = 4

  module B = struct
    let x = "test"
    let z = x
  end

  let x = B.x
end
;;

Assert.eq A.B.z "test";;
Assert.eq A.B.x "test";;
Assert.eq A.x "test";;
Assert.eq x 3;;

Assert.eq
  (let x = 2 in
   x)
  2
