#use "../stdlib/assert.ml";;

module type Sig = sig
  val x of int 
end;;

module MSig : Sig = struct
  let y = 3;;
  let x = y;; 
end;;

Assert.eq(MSig.x, 3);; -- OK
MSig.y;; -- Fail. y is a private module member.
