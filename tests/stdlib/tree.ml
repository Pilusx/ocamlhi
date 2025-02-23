type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | None

module type STree = sig
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b tree -> 'a
  val fold_up : ('a -> 'b -> 'a -> 'a) -> 'a -> 'b tree -> 'a
  val size : 'a tree -> int
  val depth : 'a tree -> int
  val root : 'a tree -> 'a
  val init : int -> (int -> 'a) -> 'a tree
  val map : ('a -> 'b) -> 'a tree -> 'b tree
  val empty : 'a tree -> bool
end

module Tree : STree = struct
  let rec fold_left f acc x =
    match x with
    | None -> acc
    | Node (ls, v, rs) -> begin
      let lacc = fold_left f acc ls in
      let vacc = f lacc v in
      fold_left f vacc rs
    end

  let rec fold_up f lv x =
    match x with
    | None -> lv
    | Node (ls, v, rs) -> f (fold_up f lv ls) v (fold_up f lv rs)

  let size x = fold_left (fun acc _ -> acc + 1) 0 x
  let depth x = fold_up (fun lp _ rp -> max lp rp + 1) 0 x

  let root x =
    match x with
    | None -> raise (Exception "Empty Tree")
    | Node (ls, v, rs) -> v

  let rec initHelper (id : int) (n : int) (f : int -> 'a) : 'a tree =
    if id > n
    then None
    else Node (initHelper (2 * id) n f, f id, initHelper ((2 * id) + 1) n f)

  let init n f = initHelper 1 n f

  let rec map f x =
    match x with
    | None -> None
    | Node (ls, x, rs) -> Node (map f ls, f x, map f rs)

  let empty x =
    match x with
    | None -> true
    | _ -> false
end
