#use "stdlib.ml";;

type `a list = Nil | List of `a * `a list;;

module type SList = sig
    val fold_left of (`a -> `b -> `a) -> `a -> `b list -> `a
    val fold_right of (`a -> `b -> `b) -> `a list -> `b -> `b 
    val length of `a list -> int
    val hd of `a list -> `a
    val tl of `a list -> `a list
    val rev of `a list -> `a list
    val init of int -> (int -> `a) -> `a list
    val concat of `a list list -> `a list
    val map of (`a -> `b) -> `a list -> `b list
    val empty of `a list -> bool
    val combine of `a list -> `b list -> (`a * `b) list
    val partition of (`a -> bool) -> `a list -> `a list * `a list
    val empty_list_exception of string
    val equal_length_exception of string
end;;

module MList : SList = struct 
-- [ISSUE] Exception types are not checked.
    exception Exception of string;;

    let empty_list_exception = "Empty List" ;;
    let equal_length_exception = "Lists should have same lengths";;

    let rec fold_left f acc x = match x with(
          Nil -> acc
        | List(h, xs) -> MList.fold_left(f, f(acc, h), xs)
    );;

    let rec fold_right f x acc = match x with(
          Nil -> acc
        | List(h, xs) -> f(h, MList.fold_right(f, xs, acc))
    );;

    let length x = MList.fold_left(fun acc, _ -> (acc + 1), 0, x);;

    let hd x = match x with(
          Nil -> raise (Exception MList.empty_list_exception)
        | List(h, xs) -> h
    );;

    let tl x = match x with(
          Nil -> raise (Exception MList.empty_list_exception)
        | List(h, Nil) -> Nil
        | List(h, xs) -> xs
    );;

    let rev x = MList.fold_left(fun acc, x -> List(x, acc), Nil, x);;

    let rec initHelper c n f = if (c >= n) 
        then {Nil} 
        else {List(f(c), initHelper((c + 1), n, f))};;

    let init n f = initHelper(0, n, f);;
    
    let concat x =
        let rec concat2 l1 l2 = match l1 with(
          Nil -> l2
        | List(h, xs) -> List(h, concat2(xs, l2))
        ) in fold_right(concat2, x, Nil);;

    let rec map f x = match x with(
          Nil -> Nil
        | List(h, xs) -> List(f(h), MList.map(f, xs))
    );;

    let empty x = match x with(
          Nil -> true
        | _ -> false
    );;

    let rec combine x y = match x, y with(
          List(h1, xs), List(h2, ys) -> List(T(h1, h2), MList.combine(xs, ys))
        | Nil, Nil -> Nil
        | _, _ -> raise (Exception T(MList.equal_length_exception, x, y))
    );;

    let partition f x = 
        let helper = fun el, T(x, y) -> if f(el)
            then {T(List(el, x), y)} 
            else {T(x, List(el, y))}
        in fold_right(helper, x, T(Nil, Nil));;
end;;
