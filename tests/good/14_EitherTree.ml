(* Tests 'either lists' and 'either trees'. *)
#use "assert.ml"

#use "either.ml"

#use "list.ml"

#use "tree.ml"

(* Data construction 'either list' *)
let l1 : (int, char list) eth list =
  [ Left 4; Right [ 'a'; 'l'; 'a' ]; Left 13; Right [ 'm'; 'a' ] ]

(* Merging lists *)
let ( ++ ) = List.( ++ )
let l2 : (int, char list) eth list = [ Left 5; Right [ 'h'; 'e'; 'h'; 'e' ] ] ++ l1;;

Assert.eq (List.hd l2) (Left 5);;
Assert.eq (List.length l2) 6;;
Assert.eq (List.hd (List.rev l2)) (Right [ 'm'; 'a' ])

(* Data construction 'either tree'; *)
let x = Either.left 13
let y = Either.right (Tree.init 2 id)

(* Test mapping either *)
let f x = Either.either (fun x -> x + 29) (fun x -> Tree.size x) x;;

Assert.eq (f x) 42;;
Assert.eq (f y) 2
