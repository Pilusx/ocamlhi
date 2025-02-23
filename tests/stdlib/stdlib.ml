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

external raise : exn -> 'a = "%raise"

(* Prefix operators *)
external ( ~+ ) : int -> int = "%identity"
external ( ~- ) : int -> int = "%negint"
external ( ~+. ) : float -> float = "%identity"
external ( ~-. ) : float -> float = "%negfloat"
external not : bool -> bool = "%boolnot"

(* Infix operators *)
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( % ) : int -> int -> int = "%modint"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( <> ) : 'a -> 'a -> bool = "%notequal"

let id x = x
let max a b = if a < b then b else a
let min a b = if a < b then a else b

type 'a ref = { mutable contents : 'a }

let ref (x : 'a) : 'a ref = { contents = x }
let ( ! ) (x : 'a ref) : 'a = x.contents
let ( := ) (x : 'a ref) (e : 'a) : unit = x.contents <- e

external __tostring__ : 'a -> string = "to_string"
external __debug__ : unit -> bool = "debug"

let __DEBUG__ = __debug__ ()

external __typeof__ : 'a -> string = "type_of"

exception Exception of string
