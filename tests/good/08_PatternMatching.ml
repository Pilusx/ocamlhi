#use "assert.ml"

(* Algebraic type definitions. *)
type i = I of int
type 'a e = E of 'a

type 'a may =
  | None
  | Just of 'a

type ('a, 'b) eth =
  | Left of 'a
  | Right of 'b
;;

(* Inline types. *)
(None : 'a may);;
(Left 4 : (int, 'a) eth);;
(Right 5 : ('a, int) eth);;
(Just 5 : int may)

(* Complex pattern matching for user defined types. *)
let z = E (Left (Just 3)), E (Right None);;

Assert.eq
  (match z with
   | E (Left (Just x)), E (Right _) -> 2 + x
   | _ -> 3)
  5
