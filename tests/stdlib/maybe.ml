type 'a may =
  | Just of 'a
  | Nothing

module type SMaybe = sig
  val nothing : 'a may
  val just : 'a -> 'a may
  val either : 'c -> ('a -> 'c) -> 'a may -> 'c
end

module Maybe : SMaybe = struct
  let nothing = Nothing
  let just a = Just a

  let either v f x =
    match x with
    | Nothing -> v
    | Just a -> f a
end
