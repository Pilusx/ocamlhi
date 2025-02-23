#use "assert.ml"

#use "nn.ml"

(* Test vector-vector operations. *)
let v = List.init 10 (fun _ -> 1);;

(* Vector of 1's *)
Assert.eq (ANN.vecvecmul v v) 10

(* Test 2D matrices. *)
let a = ANN.init (fun i j -> i % (j + 1)) 10 10

let matrix_dim1 m =
  match m with
  | Matrix m -> List.length m
;;

Assert.eq (matrix_dim1 a, List.length v) (10, 10)

(* Test matrix-vector operations. *)
let av = ANN.matvecmul a v
let vTav = ANN.vecvecmul v av;;

(* v^Tav = sum of a_i,j *)
Assert.eq
  vTav
  (let list_sum x = List.fold_right ( + ) x 0 in
   let list_sum2d m = List.fold_right (fun v acc -> acc + list_sum v) m 0 in
   let matrix_unwrap m =
     match m with
     | Matrix m -> m
   in
   list_sum2d (matrix_unwrap a))
