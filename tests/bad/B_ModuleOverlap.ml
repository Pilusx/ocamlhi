
module type Sig = sig
  val f of int
end;;

module MSig : Sig = struct 
  let f = 3;; 
end;;

let f = MSig.f;;

module MSig = struct
  let x = ref (5);; 
end;;

MSig.f;; -- Not found.