# Records
Records are passed by reference to function calls in Ocaml.

### Type declarations for records with mutable semantics
```
type a1 = {mutable a: int; b: int};;
```
### Constructors
```
let x = {a=3; b=4};;
```
### Getters of record fields
```
x.a;; -- Prints 3
```
### Setters of record fields (<-)
```
x.a <- 5;;
x.a;; -- Prints 5
```
### Deep copying? (2 ways)
* passing all parameters to the constructor [TODO]
* 'with' statement [TODO]
### Recursive records
```
type a2 = {a : a1; b : string};;
let y = {a=x; b="message"};;
y.a.a;; -- Prints 3
y.b;; -- Prints "message"
```

### Passing records as parameters
Records are always passed by reference.
```
val f : a1 -> unit;;
let f x = begin x.a <- 3 end;;
f(x) -- Modifies the mutable value.
```

# References
### Declarations with type reconstruction
```
let x = ref 3;;
=> val x : int ref;;
```
### Assignment operator (:=)
```
x := 4;;
```
### Dereferencing operator (!)
```
!x;; -- Prints 4
```

### Implementation
References are implemented as mutable records.

```
type `a ref = {mutable contents: `a};;

let ref x = {contents=x};;
let (!) x = x.contents;;
let (:=) x e = x.contents <- e;;

```

### Virtual memory
Each record stores pointers to subrecords.  
Does it store types?  
Types are stored in a seperate data structure. Given a CanonicalName we can traverse the path using the root node and field idents.

## References
[Pointers in OCaml](https://ocaml.org/learn/tutorials/pointers.html)
