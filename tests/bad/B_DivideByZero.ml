#use "../stdlib/stdlib.ml";;

-- [ISSUE] It should fail as arguments in ocaml are not lazily evaluated..
let f _ = 3;;
T(f((1 / 0)), 3);;
