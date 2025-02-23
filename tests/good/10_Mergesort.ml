#use "assert.ml"

#use "list.ml"

let split (x : 'a list) : 'a list * 'a list =
  let n = List.length x in
  let p = n / 2 in
  let x1 = List.combine (List.init n id) x in
  let x2 = List.partition (fun (i, _) -> i < p) x1 in
  let list_snd xs = List.map (fun (x, y) -> y) xs in
  match x2 with
  | l1, l2 -> list_snd l1, list_snd l2

let rec merge x y =
  match x, y with
  | [], _ -> y
  | _, [] -> x
  | xh :: xs, yh :: ys -> if x < y then xh :: merge xs y else yh :: merge x ys

let rec mergesort (x : 'a list) : 'a list =
  match x with
  | [] -> []
  | [ h ] -> [ h ]
  | _ ->
    (let p1, p2 = split x in
    merge (mergesort p1) (mergesort p2))

(* Sanity check *)
let x : int list = [ 1; 2; 3; 4; 5 ]
let y : int list * int list = [ 1; 2 ], [ 3; 4; 5 ];;

Assert.eq (split x) y;;
Assert.eq (mergesort x) x

(* Small test *)
let x : int list = [ 0; 3; 2; 1 ]
let y : int list = List.init (List.length x) id
let p1, p2 = split x
let p21, p22 = split p2;;

Assert.eq p1 [ 0; 3 ];;
Assert.eq p2 [ 2; 1 ];;
Assert.eq (mergesort p2) [ 1; 2 ];;
Assert.eq (mergesort x) y

(* Second small test *)
let x : 'a = [ 0; 9; 7; 5; 3; 1; 2; 8; 6; 4 ]
let y : 'a list = List.init (List.length x) id;;

Assert.eq (mergesort x) y;;

(* Check type signatures. *)
Assert.eq (__typeof__ x) "int list";;
Assert.eq (__typeof__ y) "int list";;
Assert.eq (__typeof__ id) "'a -> 'a";;
Assert.eq (__typeof__ split) "'a list -> 'a list * 'a list";;
Assert.eq (__typeof__ merge) "'a list -> 'a list -> 'a list";;
Assert.eq (__typeof__ mergesort) "'a list -> 'a list"
