-- [ISSUE] Compatibility with ocaml.
-- [ISSUE] Brackets.

#use "../stdlib/assert.ml";;

type point0 = {mutable x : int; y : int};;
type point2 = {mutable x : int; y : int; h : int};;
type point3 = {mutable x : int; y : int; z : int};;
type point4 = {x : int; y : int};;

-- Those should pass.
-- [ISSUE] Useless begin ... end
let g x = let y = (x.h + x.y) in begin x.x <- (14 + y) end;;
Assert.eq(__typeof__ g, "point2 -> unit");;

val g of point2 -> unit;;
-- [ISSUE] (x: point2) local type signature.
let g x = let y = (x.y + x.h) in begin x.x <- (14 + y) end;;
Assert.eq(__typeof__ g, "point2 -> unit");;

let z = {x = 3; y = 4};;
Assert.eq(__typeof__ z, "point4");;

-- Those should break.
let g x = let y = (x.y + x.h) in begin x.x <- (14 + y) end;; -- (point4 : It does not have field 'h')
