-- Test the stdlib implementation of list.
#use "../stdlib/assert.ml";;
#use "../stdlib/list.ml";;

-- This should print the definitions on the stdout.
MList.hd;;
MList.length;;
MList.fold_left;;
MList.init;;

-- Nil tests
let n = Nil;;
let n2 = List (2, Nil);;
let n3 = List rec (2, 3; Nil);;

-- Simple and recursive constructors.
let t = List rec(3, 5, 7, 12; Nil);;
t;;
let x = List(2, t);;
x;;
Assert.eq(MList.hd(x), 2);;
Assert.eq(MList.tl(x), t);;
Assert.eq(MList.hd(MList.tl(x)), 3);;
Assert.eq(MList.hd(MList.rev(x)), 12);;

-- Higher order functions.
let plus x y = (x + y);;
Assert.eq(MList.fold_left(plus, 0, x), 29);;
Assert.eq(MList.fold_left((+), 0, x), 29);;

Assert.eq(MList.length(x), 5);;
Assert.eq(MList.fold_right(fun _, acc -> (acc + 1), x, 0), 5);;

let i4 = MList.init(4, fun x -> x);;
let qi4 = MList.init(4, fun x -> (x*x));;
let mqi4 = MList.map(fun x -> (x*x), i4);;
let res = List rec(0, 1, 4, 9; Nil);;
Assert.eq(T(qi4, mqi4), T(res, res));;

let f = fun x -> T(x, (x*x));;
MList.map(f, i4);;

MList.combine(i4, qi4);;
