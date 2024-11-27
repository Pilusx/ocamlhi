#use "../stdlib/include.ml";;

-- Test printing information about all modules.

Assert;;
Either;;
Maybe;;
MList;;
MPair;;
ANN;;
Tree;;

-- Test function signatures.

Assert.eq(__typeof__ (~+), "int -> int");;
Assert.eq(__typeof__ (+),  "int -> int -> int");;
Assert.eq(__typeof__ (==), "`a -> `a -> bool");;

Assert.eq(__typeof__ (!),  "`a ref -> `a");;
Assert.eq(__typeof__ (:=), "`a ref -> `a -> unit");;
Assert.eq(__typeof__ ref,  "`a -> `a ref");;

Assert.eq(__typeof__ Assert.eq, "`a -> `a -> `a assertion");;

Assert.eq(__typeof__ Either.left,   "`a -> (`a, `b) eth");;
Assert.eq(__typeof__ Either.right,  "`a -> (`b, `a) eth");;
Assert.eq(__typeof__ Either.either, "(`a -> `b) -> (`c -> `b) -> (`a, `c) eth -> `b");;

Assert.eq(__typeof__ Maybe.nothing, "`a may");;
Assert.eq(__typeof__ Maybe.just,    "`a -> `a may");;
Assert.eq(__typeof__ Maybe.either,  "`a -> (`b -> `a) -> `b may -> `a");;

Assert.eq(__typeof__ MList.fold_left,  "(`a -> `b -> `a) -> `a -> `b list -> `a");;
Assert.eq(__typeof__ MList.fold_right, "(`a -> `b -> `b) -> `a list -> `b -> `b");;
Assert.eq(__typeof__ MList.length,     "`a list -> int");;
Assert.eq(__typeof__ MList.hd,         "`a list -> `a");;
Assert.eq(__typeof__ MList.tl,         "`a list -> `a list");;
Assert.eq(__typeof__ MList.rev,        "`a list -> `a list");;
Assert.eq(__typeof__ MList.init,       "int -> (int -> `a) -> `a list");;
Assert.eq(__typeof__ MList.concat,     "`a list list -> `a list");;
Assert.eq(__typeof__ MList.map,        "(`a -> `b) -> `a list -> `b list");;
Assert.eq(__typeof__ MList.empty,      "`a list -> bool");;
Assert.eq(__typeof__ MList.combine,    "`a list -> `b list -> (`a * `b) list");;
Assert.eq(__typeof__ MList.partition,  "(`a -> bool) -> `a list -> `a list * `a list");;
Assert.eq(__typeof__ MList.empty_list_exception,   "string");;
Assert.eq(__typeof__ MList.equal_length_exception, "string");;

Assert.eq(__typeof__ MPair.pair,   "`a -> `b -> (`a, `b) pair");;
Assert.eq(__typeof__ MPair.first,  "(`a, `b) pair -> `a");;
Assert.eq(__typeof__ MPair.second, "(`a, `b) pair -> `b");;

Assert.eq(__typeof__ ANN.init,      "(int -> int -> `a) -> int -> int -> `a mat");;
Assert.eq(__typeof__ ANN.vecvecmul, "int list -> int list -> int");;
Assert.eq(__typeof__ ANN.matvecmul, "int mat -> int list -> int list");;

Assert.eq(__typeof__ Tree.fold_left, "(`a -> `b -> `a) -> `a -> `b tree -> `a");;
Assert.eq(__typeof__ Tree.fold_up,   "(`a -> `b -> `a -> `a) -> `a -> `b tree -> `a");;
Assert.eq(__typeof__ Tree.size,      "`a tree -> int");;
Assert.eq(__typeof__ Tree.depth,     "`a tree -> int");;
Assert.eq(__typeof__ Tree.root,      "`a tree -> `a");;
Assert.eq(__typeof__ Tree.init,      "int -> (int -> `a) -> `a tree");;
Assert.eq(__typeof__ Tree.map,       "(`a -> `b) -> `a tree -> `b tree");;
Assert.eq(__typeof__ Tree.empty,     "`a tree -> bool");;
