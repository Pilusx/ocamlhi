type ('a, 'b) eth =
  | Left of 'a
  | Right of 'b

module type SEither = sig
  val left : 'a -> ('a, 'b) eth
  val right : 'b -> ('a, 'b) eth
  val either : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) eth -> 'c
end

module Either : SEither = struct
  let left a = Left a
  let right b = Right b

  let either fa fb v =
    match v with
    | Left a -> fa a
    | Right b -> fb b
end
