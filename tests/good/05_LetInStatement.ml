#use "../stdlib/assert.ml";;

-- Test for recursion.
let z1 = (13 < 15);; T(z1, true);;
let z2 = (false <> true);; T(z2, true);;
let d = let z = 3 in z ;; T(d, 3);;
let d = 
    let x = 3 in
        let y = 4 in
            let z = 5 in (((x * x) + (y * y)) == (z * z));; 
Assert.eq(d, true);;

-- Test for static binding, and declaration time evaluation.
let x = ref(3);;
let y = ref(4);;
let z = ref(5);;
let isTriple = (((!x * !x) + (!y * !y)) == (!z * !z));;
Assert.eq(isTriple, true);;