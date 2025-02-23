(* Test the stdlib implementation of pair. *)
#use "assert.ml"

#use "pair.ml"

let z = Pair (3.14, 4);;

Assert.eq (MPair.first z) 3.14;;
Assert.eq (MPair.second z) 4;;
Assert.eq (MPair.pair 1 2) (Pair (1, 2));;

(* Check that signatures have not changed... *)
Assert.eq (__typeof__ z) "(float, int) pair";;
Assert.eq (__typeof__ MPair.first) "('a, 'b) pair -> 'a";;
Assert.eq (__typeof__ MPair.second) "('a, 'b) pair -> 'b"
