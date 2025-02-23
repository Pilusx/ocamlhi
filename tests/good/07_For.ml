(* https://ocaml.org/learn/tutorials/if_statements_loops_and_recursion.html *)
#use "assert.ml"

(* Simple for loop *)

let f n =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> 2

(* placeholders *)
let n = ref 3
let k = ref 0;;

for i = 1 to 3 do
  n := !n - 1;
  k := f !n
done
;;

Assert.eq !n 0;;
Assert.eq !k 0

(* Downto loop *)

let n = ref 0;;

for i = 13 downto 1 do
  n := !n + 1
done
;;

Assert.eq !n 13
