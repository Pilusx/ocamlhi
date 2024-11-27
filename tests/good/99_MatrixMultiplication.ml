#use "../stdlib/assert.ml";;
#use "../stdlib/nn.ml";;

MList.init;;
MList.fold_left;;
ANN.init;;

-- Test vector-vector operations.
let v = MList.init(10, fun _ -> 1);; -- Vector of 1's
Assert.eq(ANN.vecvecmul(v, v), 10);;

-- Test 2D matrices.
let A = ANN.init(fun i, j -> (i mod (j + 1)), 10, 10);;

let matrix_dim1 m = match m with (Matrix(m) -> MList.length(m));;
Assert.eq(T(matrix_dim1(A), MList.length(v)), T(10, 10));;

-- Test matrix-vector operations.
let Av = ANN.matvecmul(A, v);;
let vTAv = ANN.vecvecmul(v, Av);;

-- v^TAv = sum of A_i,j
Assert.eq(vTAv,
    let list_sum x = MList.fold_right((+), x, 0) in
        let list_sum2d m = MList.fold_right(fun v, acc -> (acc + list_sum(v)), m, 0) in
            let matrix_unwrap m = match m with (Matrix(m) -> m) in
                list_sum2d(matrix_unwrap(A))
);;