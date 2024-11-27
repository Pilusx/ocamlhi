#use "stdlib.ml";;

module type SMath = sig
    val max of `a -> `a -> `a
    val min of `a -> `a -> `a

    val fib_rec of int -> int 
    val fib of int -> int

    val gcd_rec of int -> int -> int
    val gcd of int -> int -> int
end;;

module Math : SMath = struct
    let max a b = if (a<b) then {b} else {a};;
    let min a b = if (a<b) then {a} else {b};;
    
    let rec fib_rec n = match n with (
          0 -> 0
        | 1 -> 1
        | _ -> (Math.fib_rec((n-1)) + Math.fib_rec((n-2)))
    );;
    let fib n = match n with (
          0 -> 0
        | 1 -> 1
        | _ ->
            let a = ref(0) in
            let b = ref(1) in
            let c = ref(1) in
            let i = ref(1) in -- placeholder
            begin
                for i = 1 to n do 
                    (c := (!a + !b));
                    (a := !b);
                    (b := !c)
                done;
                !a
            end);;


    let rec gcd_rec a b = if (b == 0) then {a} else {Math.gcd_rec(b, (a mod b))};;
    let gcd m n  =
        let a = ref(m) in
        let b = ref(n) in
        let c = ref(0) in
        begin
            while (!b <> 0) do
                (c := (!a mod !b));
                (a := !b);
                (b := !c)
            done;
            !a
        end
end;;
