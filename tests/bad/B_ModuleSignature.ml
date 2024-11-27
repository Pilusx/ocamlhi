#use "../stdlib/stdlib.ml";;

-- [ISSUE] Replace 'val ... of' with ' val ... :' 
module type Sig = sig
  val f of `a -> `a -> bool 
end;;

module MSig : Sig = struct
  let f y z = ((y + 3) == z) 
end
-- Fail. Assert.eq("`a -> `a -> bool", "int -> int -> bool");
