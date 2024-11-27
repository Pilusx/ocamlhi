type (`a, `b) pair = Pair of `a * `b;;

module type SPair = sig
    val pair of `a -> `b -> (`a, `b) pair
    val first of (`a, `b) pair -> `a
    val second of (`a, `b) pair -> `b
end;;

module MPair : SPair = struct
    let pair f s = Pair(f, s);;
    
    let first x = match x with (
        Pair(f, _) -> f
    ) ;;

    let second x = match x with (
        Pair(_, s) -> s   
    );;
end;;