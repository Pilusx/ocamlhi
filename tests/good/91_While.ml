#use "../stdlib/assert.ml";;

let x = ref (1) in let y = ref(2) in -- Local mutable objects.
Assert.eq(
    begin
        while (!y > 0) do (x := (!x + !y)); (y := (!y - 1)) done;
        !x
    end, 4);;

let x = ref(1);; -- Global mutable object
let y = ref(2) in let z = 3 in
Assert.eq(
    begin
        while (!y > 0) do (x := (!x + !y)); (y := (!y - 1)) done;
        !x
    end, 4);;

Assert.eq(!x, 4);;

-- While loop in function.
let x = ref(0);;
let f _ = let once = ref(1) in 
    begin
        while (!once > 0) do (x := (!x+1)); (once := 0) done;
        !x
    end;;

let z = ref(f(9999));;
Assert.eq(!z,1);;
Assert.eq(!z,1);;

(z := f(9999));;
Assert.eq(!z,2);;
Assert.eq(!z,2);;
