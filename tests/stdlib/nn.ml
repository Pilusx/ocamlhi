#use "list.ml";;

type `a mat = Matrix of `a list list ;;

module type SANN = sig
    val init of (int -> int -> `a) -> int -> int -> `a mat
    val vecvecmul of int list -> int list -> int
    val matvecmul of int mat -> int list -> int list
end;;

module ANN : SANN = struct
    let init f n m = Matrix(
        MList.init(n, fun i -> 
            MList.init(m, fun j -> f(i, j))
        )
    );;

    let vecvecmul x y = MList.fold_left(fun acc, T(x, y) -> (acc + (x * y)), 0, MList.combine(x, y));;

    let matvecmul m x = match m with (
        Matrix(m) -> MList.map(fun v -> vecvecmul(x, v), m)
    );;
end;;

