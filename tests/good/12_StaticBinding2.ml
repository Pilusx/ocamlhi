-- Stress test.

#use "../stdlib/assert.ml";;
#enable "trace-input-tree";;
#enable "trace-input-version";;
#enable "trace-output-tree";;

let x = 3;;
let x = 4;;
let y = (3 + x);;
let x = ref(4);;
Assert.eq(!x, 4);;
let y = x;;
let y = 4;;
let f x = y;;
let y = 5;;
let f1 = f;;
let f g x n = g(n, x);;
let g x n = (f1(x) + n);;
let h x y = f(g, x, y);;
let x = 31;;
let y = 14;;
Assert.eq(h(x, y), 35);;
let x = ref(4);;
let f x = let n = ref(1) in begin
        while (!n > 1) do (x := (!x + 1)); (n := 0) done;
        !x
    end;;
let g = f(x);;
let g x y z = ((x + y) + z);;
let f x y = g(y, x, 3);;
Assert.eq(f(2, 3), 8);;
let rec h x y z  = if (x <= 0) then {(y + z)} else {h((x - 1), (z + 1), y)};;
let rec fib n = if (n < 0) then {1} else {(fib ((n - 1)) + fib ((n - 2)))};;
let x = ref(0);;
let f y = let n = ref(1) in begin
        while (!n > 1) do (x := (!x - 1)); (n := 0) done;
        !x
    end;;
type record1 = {code : int; mutable msg : string};;
let x = {code=0; msg=" "};;
Assert.eq(f(x), 0);;
let f x = let n = ref(2) in begin 
        while (!n > 1) do x.msg <- "once"; (n := 0) done;
        x
    end;;
f(x);;
Assert.eq(x.msg, "once");;
