type 'a assertion = Assertion of string * 'a

exception AssertEqualFailed of string * string

module type SAssert = sig
  val eq : 'a -> 'a -> 'a assertion
end

module Assert : SAssert = struct
  let eq a b =
    if __DEBUG__
    then begin
      if a == b
      then Assertion ("OK", b)
      else raise (AssertEqualFailed (__tostring__ a, __tostring__ b))
    end
    else Assertion ("Skipped", a)
end
