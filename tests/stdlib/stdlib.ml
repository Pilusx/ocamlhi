(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**************************************************************************)
(*   Original, up-to-date source can be found here:                       *)
(*     https://github.com/ocaml/ocaml/blob/trunk/stdlib/stdlib.ml         *)
(*   License file of OCaml (LGPLv2.1) can be found here:                  *)
(*     https://github.com/ocaml/ocaml/blob/trunk/LICENSE                  *)
(**************************************************************************)

-- Prefix operators
val (~+) of int -> int;;
let (~+) x = x;;
external (~-) : int -> int = "ocaml_neg_int";;

-- Infix operators
external (+) : int -> int -> int = "ocaml_add_int";;
external (-) : int -> int -> int = "ocaml_sub_int";;
external (*) : int -> int -> int = "ocaml_mul_int";;
external (%) : int -> int -> int = "ocaml_mod_int";;
external (/) : int -> int -> int = "ocaml_div_int";;
external (mod) : int -> int -> int = "ocaml_mod_int";;

external (*.) : float -> float -> float = "ocaml_mul_float";;

external (<)  : `a -> `a -> bool = "ocaml_lt";;
external (<=) : `a -> `a -> bool = "ocaml_lte";;
external (>)  : `a -> `a -> bool = "ocaml_gt";;
external (>=) : `a -> `a -> bool = "ocaml_gte";;
external (==) : `a -> `a -> bool = "ocaml_eq";;

val not of bool -> bool;;
let not x = if x then {false} else {true};;

val (<>) of `a -> `a -> bool;;
let (<>) x y = not((x == y));;

type `a ref = {mutable contents : `a};;

val ref of `a -> `a ref;;
let ref x = {contents=x};;

val (!) of `a ref -> `a;;
let (!) x = x.contents;;

val (:=) of `a ref -> `a -> unit;;
let (:=) x e = begin x.contents <- e end;;
