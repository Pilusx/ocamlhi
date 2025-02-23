(* [ISSUE] (Out of scope) Fix private module members. *)

module type Sig = sig
  val x : int
end

module MSig : Sig = struct
  let y = 3
  let x = y
end
;;

MSig.y
