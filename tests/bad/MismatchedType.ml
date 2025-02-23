(* Complex tuples. *)
type 'a bigtuple = Tuple of ('a * 'a) * ('a * 'a)

let x = Tuple ((1, 2), (3, 4))

let f x =
  match x with
  | Tuple (a, b, c, d) -> a + b + c + d
