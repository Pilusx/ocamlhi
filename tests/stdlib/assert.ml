#use "stdlib.ml";;

type `a assertion = Assertion of string * `a;;
module type SAssert = sig
    val eq of `a -> `a -> `a assertion 
end;;

module Assert : SAssert = struct
    let eq a b = if (a == b) then {
        Assertion("OK", b) 
    } else {
        raise (AssertEqualFailed T(a, b))
    };;
end;;
