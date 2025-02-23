type ('a, 'b) pair = Pair of 'a * 'b

module type SPair = sig
  val pair : 'a -> 'b -> ('a, 'b) pair
  val first : ('a, 'b) pair -> 'a
  val second : ('a, 'b) pair -> 'b
end

module MPair : SPair = struct
  let pair f s = Pair (f, s)

  let first x =
    match x with
    | Pair (f, _) -> f

  let second x =
    match x with
    | Pair (_, s) -> s
end
