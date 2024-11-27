#use "math.ml";;

type `a tree = Node of `a tree * `a * `a tree | None;;

module type STree = sig
    val fold_left of (`a -> `b -> `a) -> `a -> `b tree -> `a
    val fold_up of (`a -> `b -> `a -> `a) -> `a -> `b tree -> `a 
    val size of `a tree -> int
    val depth of `a tree -> int
    val root of `a tree -> `a
    val init of int -> (int -> `a) -> `a tree
    val map of (`a -> `b) -> `a tree -> `b tree
    val empty of `a tree -> bool
end;;

module Tree : STree = struct 
    exception Exception of cstring;;

    let rec fold_left f acc x = match x with(
          None -> acc
        | Node (ls, v, rs) -> 
            let lacc = Tree.fold_left(f, acc, ls) in
            let vacc = f(lacc, v) in Tree.fold_left(f, vacc, rs)
    );;

    let rec fold_up f lv x = match x with(
          None -> lv
        | Node (ls, v, rs) -> f(Tree.fold_up(f, lv, ls), v, Tree.fold_up(f, lv, rs))
    );;

    let size x = Tree.fold_left(fun acc, _ -> (acc + 1), 0, x);;

    let depth x = Tree.fold_up(fun lp, _, rp -> (Math.max(lp, rp) + 1), 0, x);;

    let root x = match x with(
          None -> raise (Exception "Empty Tree")
        | Node (ls, v, rs) -> v
    );;

    let rec initHelper id n f = if (id > n) 
        then {None}
        else {Node(Tree.initHelper((2*id), n, f), f(id), Tree.initHelper(((2*id)+1), n, f))};;

    let init n f = Tree.initHelper(1, n, f);;
    
    let rec map f x = match x with(
          None -> None
        | Node (ls, x, rs) -> Node(Tree.map(f, ls), f(x), Tree.map(f, rs))
    );;

    let empty x = match x with(
          None -> true
        | _ -> false
    );;
end;;
