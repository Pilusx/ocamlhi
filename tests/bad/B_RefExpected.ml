#use "../stdlib/stdlib.ml";;

let x = 2;;
T(x, 2);;
-- [ISSUE] Useless brackets.
(x := 3);;
T(x, 3);;