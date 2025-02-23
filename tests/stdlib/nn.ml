#use "list.ml"

type 'a mat = Matrix of 'a list list

module type SANN = sig
  val init : (int -> int -> 'a) -> int -> int -> 'a mat
  val vecvecmul : int list -> int list -> int
  val matvecmul : int mat -> int list -> int list
end

module ANN : SANN = struct
  let init f n m = Matrix (List.init n (fun i -> List.init m (fun j -> f i j)))

  let vecvecmul x y =
    List.fold_left (fun acc (x, y) -> acc + (x * y)) 0 (List.combine x y)

  let matvecmul m x =
    match m with
    | Matrix m -> List.map (fun v -> vecvecmul x v) m
end
