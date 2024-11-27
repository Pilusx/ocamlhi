#use "../stdlib/assert.ml";;

-- Let in statements should operate in the local environment.

let f = 3;;
let f = 4 in Assert.eq(f, 4);;
Assert.eq(f, 3);;