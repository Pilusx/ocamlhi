type `a may = Just of `a | Nothing;;

module type SMaybe = sig
    val nothing of `a may
    val just of `a -> `a may
    val either of `c -> (`a -> `c) -> `a may -> `c
end;;

module Maybe : SMaybe = struct
    let nothing = Nothing;;
    
    let just a = Just(a);;

    let either v f x = match x with(
          Nothing -> v
        | Just(a) -> f(a)
    );;
end;;
