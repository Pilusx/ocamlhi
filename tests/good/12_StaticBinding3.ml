#use "../stdlib/list.ml";;

let f i j = (i * j);;

let row f m i = MList.init(m, fun j -> f(i, j));; -- f was badly captured
let col f n = MList.init(n, fun i -> row(f, n, i));;

row(f, 4, 1);;
col(f, 4);;
