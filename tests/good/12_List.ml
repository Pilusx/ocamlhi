#use "assert.ml"

(* Recursive types. *)
type 'a mylist =
  | List of 'a * 'a mylist
  | Nil
;;

List (2, Nil);;
Assert.eq (__typeof__ Nil) "'a mylist";;
List (2, List (3, List (4, Nil)))

(* Polymorphic zero-parameter constructors *)
let x1 : 'a mylist = Nil
let x2 : 'a mylist mylist = x1
let x3 : int mylist = x1;;

Assert.eq (__typeof__ []) "'a list";;
Assert.eq (__typeof__ x1) "'a mylist"
