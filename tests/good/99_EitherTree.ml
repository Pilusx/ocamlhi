-- Tests 'either lists' and 'either trees'.

#use "../stdlib/assert.ml";;
#use "../stdlib/either.ml";;
#use "../stdlib/list.ml";;
#use "../stdlib/tree.ml";;

-- Data construction 'either list'
val l1 of (int, char list) eth list ;;
let l1 = List rec (Left(4), Right(List rec('a', 'l', 'a'; Nil)), Left(13), Right(List rec('m', 'a'; Nil)); Nil) ;;

-- Merging lists
val l2 of (int, char list) eth list ;;
let l2 = List rec (Left(5), Right(List rec('h', 'e', 'h', 'e'; Nil)); l1) ;;
MList.hd(l2);;
Assert.eq(MList.length(l2), 6);;
MList.hd(MList.rev(l2));;

-- Data construction 'either tree';
let x = Either.left(13);;
let y = Either.right(Tree.init(2, fun x -> x));;

-- Test mapping either
let f x = Either.either(fun x -> (x + 29), fun x -> Tree.size(x), x);;
Assert.eq(f(x), 42);;
Assert.eq(f(y), 2);;