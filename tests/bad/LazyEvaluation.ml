(* Arguments in OCaml are not lazily evaluated.. *)
let f _ = 3;;

f (1 / 0)
