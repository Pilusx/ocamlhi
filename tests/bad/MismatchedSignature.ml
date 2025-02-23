module type Sig = sig
  val f : 'a -> 'a -> bool
end

module MSig : Sig = struct
  let f y z = y + 3 == z
end
(* Fail. Expected "'a -> 'a -> bool" got "int -> int -> bool" *)
