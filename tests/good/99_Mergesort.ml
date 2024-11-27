#use "../stdlib/assert.ml";;
#use "../stdlib/list.ml";;

val id of `a -> `a;;
let id x = x;;

-- It is dumb, but it tests complex types....
val group of `a list * `a list -> `a list list;;
let group = fun T(l1, l2) -> List rec(l1, l2; Nil);;

val uncurry of (`a -> `b -> `c) -> `a * `b -> `c;;
let uncurry = fun f, T(x, y) -> f(x, y);;

val split of `a list -> `a list * `a list;;
let split x =
  let n = MList.length(x) in
  let p = (n / 2) in
  let x1 = MList.combine(MList.init(n, id), x) in
  let x2 = MList.partition(fun T(i, _) -> (i < p), x1) in
  let list_snd xs = MList.map(fun T(x, y) -> y, xs) in 
    match x2 with(
      T(l1, l2) -> T(list_snd(l1), list_snd(l2))
    );;

-- [ISSUE] Implement syntactic sugar for lists... (::), (++), []
val merge of `a list -> `a list -> `a list;;
let merge x y = 
  let rec helper l1 l2 acc = match l1, l2 with(
      Nil, _ -> MList.concat(group(T(MList.rev(l2), acc)))
    | _, Nil -> MList.concat(group(T(MList.rev(l1), acc)))
    | List(x, xs), List(y, ys) -> if (x > y) 
        then {helper(xs, l2, List(x, acc))} 
        else {helper(l1, ys, List(y, acc))} 
  ) in helper(MList.rev(x), MList.rev(y), Nil);;

-- Sanity check
val x of int list;;
let x = List rec(1,2,3,4,5; Nil);;
val y of int list * int list;;
let y = T(List rec(1,2; Nil), List rec(3,4,5; Nil));;
Assert.eq(split(x), y);;
Assert.eq(MList.concat(group(y)), x);;
Assert.eq(uncurry(merge, y), x);;

val mergesort of `a list -> `a list;;
let rec mergesort x = match x with(
    Nil -> Nil
  | List(h, Nil) -> List(h, Nil)
  | _ -> let z = split(x) in match z with(
      T(p1, p2) -> merge(mergesort(p1), mergesort(p2))
    )
  );;

-- Small test
val x of int list;;
let x = List rec(0, 3, 2, 1; Nil);;
val y of int list;;
let y = MList.init(MList.length(x), id);;
Assert.eq(mergesort(x), y);;

-- Second small test
val x of `a;;
let x = List rec(0, 9, 7, 5, 3, 1, 2, 8, 6, 4; Nil);;
val y of `a list;;
let y = MList.init(MList.length(x), id);;
Assert.eq(mergesort(x), y);;

-- Check type signatures.
Assert.eq(__typeof__ x,         "int list");;
Assert.eq(__typeof__ y,         "int list");;
Assert.eq(__typeof__ id,        "`a -> `a");;
Assert.eq(__typeof__ group,     "`a list * `a list -> `a list list");;
Assert.eq(__typeof__ uncurry,   "(`a -> `b -> `c) -> `a * `b -> `c");;
Assert.eq(__typeof__ split,     "`a list -> `a list * `a list");;
Assert.eq(__typeof__ merge,     "`a list -> `a list -> `a list");;
Assert.eq(__typeof__ mergesort, "`a list -> `a list");;
