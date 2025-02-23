#use "assert.ml"

#use "either.ml"

let d =
  let x = 3
  and y = 4
  and z = 5 in
  (x * x) + (y * y) == z * z
;;

Assert.eq d true

let x = 3
and z a = 3
and d = Right (fun x -> x)
;;

Assert.eq (__typeof__ x) "int";;
Assert.eq (__typeof__ z) "'a -> int";;
Assert.eq (__typeof__ d) "('a, ('b -> 'b)) eth"
