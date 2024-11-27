#use "../stdlib/assert.ml";;

let x = ref(0);;
Assert.eq(!x, 0);;
(x := 15);;
Assert.eq(!x, 15);;

-- Passing reference by reference...
let x = ref(3);;
let increment x = let once = ref(1) in begin
        while (!once > 0) do
            (x := (!x + 1)); 
            (once := 0)
        done;
        !x 
    end;;
Assert.eq(increment(x), 4);;
Assert.eq(!x, 4);;
Assert.eq(increment(x), 5);;
Assert.eq(!x, 5);;

-- Lazy reference evaluation...
let gen m = ref(!x);;
Assert.eq(__typeof__ gen, "`a -> int ref");;
(x := 15);;
let y = gen(3);;
Assert.eq(!y, 15);;
(x := 16);;
Assert.eq(!y, 15);;

-- (f d).hehe <- 3;;