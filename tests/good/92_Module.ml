-- Tests the implemenation of modules and module signatures.
#use "../stdlib/assert.ml";;

module type SALG = sig
    val x of int ref
    val f of int -> int
end;;

-- In-module variable binding.
module ALG : SALG = struct
    let x = ref(5);;
    let y = 3;; -- Additional private definition.
    let f m = ((m + !x) + y);;
end;;

-- Calling functions from modules.
Assert.eq(ALG.f(1), 9);;

-- Modifing variables in modules.
(ALG.x := 7);;
Assert.eq(ALG.f(1), 11);;

-- Print module information...
ALG;;

-- Check signatures
Assert.eq(__typeof__ ALG.x, "int ref");;
Assert.eq(__typeof__ ALG.f, "int -> int");;

-- Test module signatures.
module type SInt = sig
    val compare of int -> int -> bool
end;;

module MInt : SInt = struct
    let compare = (==) -- It has type `a -> `a -> bool.
end;;

Assert.eq(__typeof__ MInt.compare, "int -> int -> bool");;

-- https://ocaml.org/learn/tutorials/modules.html
-- [ISSUE] Functors. Original stdlib uses functors to bind the comparison function.

-- [ISSUE] Module inclusion.
