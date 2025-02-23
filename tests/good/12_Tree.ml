(* Test the stdlib implementation of tree. *)
#use "assert.ml"

#use "tree.ml"

let nt = Node (Node (None, 2, None), 3, Node (None, 4, None));;

Assert.eq (Tree.size nt) 3;;
Assert.eq (Tree.depth nt) 2;;
Assert.eq (Tree.empty nt) false;;
Assert.eq (Tree.size None) 0;;
Assert.eq (Tree.depth None) 0;;
Assert.eq (Tree.empty None) true

let t = Tree.init 13 id;;

Tree.map (fun x -> x, x * x) t;;
Assert.eq (Tree.size t) 13;;
Tree.depth t

let sum t = Tree.fold_left ( + ) 0 t;;

Assert.eq (sum t == 13 * 7) true

let t2 = Tree.map ( ~- ) t;;

Assert.eq (sum t2 == -13 * 7) true
