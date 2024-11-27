type record = {a: int; b : int};;
let x = {a=3; b=4};;
begin x.a <- 4 end;; -- Field `a` is not marked as mutable
