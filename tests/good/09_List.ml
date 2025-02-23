(* Test the stdlib implementation of list. *)
#use "assert.ml"

#use "list.ml"

(* Nil tests *)
let n = []
let n2 = 2 :: n
let ( ++ ) = List.( ++ )
let n3 = [ 2; 3 ] ++ n;;

Assert.eq ([ 3; 4; 5 ] ++ [ 6 ]) [ 3; 4; 5; 6 ]

(* Simple and recursive constructors. *)
let t = [ 3; 5; 7; 12 ]
let x = 2 :: t;;

Assert.eq (List.hd x) 2;;
Assert.eq (List.tl x) t;;
Assert.eq (List.hd (List.tl x)) 3;;
Assert.eq (List.hd (List.rev x)) 12;;

(* Higher order functions. *)

Assert.eq (List.fold_left ( + ) 0 x) 29;;
Assert.eq (List.length x) 5;;
Assert.eq (List.fold_right (fun _ acc -> acc + 1) x 0) 5

let i4 = List.init 4 id
let qi4 = List.init 4 (fun x -> x * x)
let mqi4 = List.map (fun x -> x * x) i4
let res = [ 0; 1; 4; 9 ];;

Assert.eq (qi4, mqi4) (res, res)

let f = fun x -> x, x * x;;

Assert.eq (List.map f i4) (List.combine i4 qi4)
