module type SMath = sig
  val fib_rec : int -> int
  val fib : int -> int
  val gcd_rec : int -> int -> int
  val gcd : int -> int -> int
end

module Math : SMath = struct
  let rec fib_rec n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fib_rec (n - 1) + fib_rec (n - 2)

  let fib n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> begin
      let a = ref 0 in
      let b = ref 1 in
      let c = ref 1 in
      for i = 1 to n do
        c := !a + !b;
        a := !b;
        b := !c
      done;
      !a
    end

  let rec gcd_rec a b = if b == 0 then a else gcd_rec b (a % b)

  let gcd m n =
    let a = ref m in
    let b = ref n in
    let c = ref 0 in
    while !b <> 0 do
      c := !a % !b;
      a := !b;
      b := !c
    done;
    !a
end
