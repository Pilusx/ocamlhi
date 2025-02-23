module type SList = sig
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val length : 'a list -> int
  val hd : 'a list -> 'a
  val tl : 'a list -> 'a list
  val rev : 'a list -> 'a list
  val init : int -> (int -> 'a) -> 'a list
  val ( ++ ) : 'a list -> 'a list -> 'a list
  val map : ('a -> 'b) -> 'a list -> 'b list
  val empty : 'a list -> bool
  val combine : 'a list -> 'b list -> ('a * 'b) list
  val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
  val empty_list_exception : string
  val equal_length_exception : string
end

module List : SList = struct
  exception Exception of string

  let empty_list_exception = "Empty List"
  let equal_length_exception = "Lists should have same lengths"

  let rec fold_left f acc x =
    match x with
    | [] -> acc
    | h :: xs -> fold_left f (f acc h) xs

  let rec fold_right f x acc =
    match x with
    | [] -> acc
    | h :: xs -> f h (fold_right f xs acc)

  let length x = fold_left (fun acc _ -> acc + 1) 0 x

  let hd x =
    match x with
    | [] -> raise (Exception empty_list_exception)
    | h :: _ -> h

  let tl x =
    match x with
    | [] -> raise (Exception empty_list_exception)
    | [ h ] -> []
    | _ :: xs -> xs

  let rev x = fold_left (fun acc x -> x :: acc) [] x
  let rec initHelper c n f = if c >= n then [] else f c :: initHelper (c + 1) n f
  let init n f = initHelper 0 n f

  let rec ( ++ ) l1 l2 =
    match l1 with
    | [] -> l2
    | h :: xs -> h :: (xs ++ l2)

  let rec map f x =
    match x with
    | [] -> []
    | h :: xs -> f h :: map f xs

  let empty x =
    match x with
    | [] -> true
    | _ -> false

  let rec combine x y =
    match x, y with
    | h1 :: xs, h2 :: ys -> (h1, h2) :: combine xs ys
    | [], [] -> []
    | _, _ -> raise (Exception equal_length_exception)

  let partition f x =
    let helper = fun el (x, y) -> if f el then el :: x, y else x, el :: y in
    fold_right helper x ([], [])
end
