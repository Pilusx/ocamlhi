# Static binding

This file describes, how static binding should work...  
It contains examples and implementation details.

Linux : 'Everything is a file.'  
Static binding : 'Every expression is a part of the filesystem tree.'

## Examples:
let x = 3
> ls x.* | next -> x.0  
> echo 3 > x.0  
> chmod r-- x.0
```
x.0 : `a, 3 : Int
=> x.0 :: Int
```

let x = 4
> ls x.* | next -> x.1  
> echo 4 > x.1  
> chmod r-- x.1
```
x.1 : `a, 4 : Int
=> x.1 :: Int
```

let y = 3 + x
> ls x.* | last -> x.1  
> cat x.1 -> 4  
> ls y.* -> None  
> evalOp (3 + 4) > y.0  
```
x.1 : Int, y : `a, 3 : Int, (+) : `a -> `a -> `a
=> y.0 :: Int
```


let x = ref 4
> ls x.* | next -> x.2  
> mkdir x.2  
> echo 4 > x.2/contents  
> chmod r-x x.2  
> chmod rw- x.2/contents
```
4 : Int, x : `a ref
=> x.2 :: Int ref
```

!x
> ls x.* | last -> x.2  
> cat x.2/contents -> 4
```
x.2 : Int ref
=> Int
```

let y = x
> ls x.* | last -> x.2  
> ls y.* | next -> y.1  
> ?  
> ln -s y.1 x.2 ?
```
x.2 : `a ref, y : `a
=> y :: Int ref
```

let y = 4
> ls y.* | next -> y.2  
> echo 4 > y.2  
> chmod r-- y.2
```
y : `a, 4 : Int
=> y :: Int
```

let f x = y
> ls f.* | next -> f.0  
> ls y.* | last -> y.2  
> echo \x -> y > f.0  
> chmod r-x f.0
```
f : `a -> `b, x : `a, y : Int
=> f :: `a -> Int
```

let y = 5
> ls y.* | next -> y.3  
> echo 5 > y.3  
> chmod r-- y.3
```
y : `a, 5 : Int
=> y :: Int
```

let f1 = f
> ls f1.* | next -> f1.0  
> ls f.* | last -> f.0  
> cp f.0 -t f1.0 ? ln -s ?
```
f1 : `a -> `b, f : `c -> Int,
=> f1 :: `c -> Int (Simplify)
=> f1 :: `a -> Int
```

let f g x n = g(n, x)
> ls f.* | next -> f.1  
> echo \g x n -> g(n, x) > f.1  
> chmod r-x f.1
```
f : `a -> `b -> `c -> `d, g : `a, x : `b, n : `c, 
g : `c -> `b -> `e, `e ~ `d
=> ( `c -> `b -> `e ) -> `b -> `c -> `d (Substitute?)
=> ( `c -> `b -> `d ) -> `b -> `c -> `d (Simplify)
=> ( `a -> `b -> `c ) -> `b -> `a -> `c
```

let g x n = f1(x) + n
> ls g.* | next -> g.0  
> ls f1.* | last -> f1.0  
> echo \x n -> f1.0(x) + n > g.0  
> chmod r-x g.0
```
g : `a -> `b -> `c, x : `a, n : `b
f1 :: `d -> Int, `a (parameter x) ~ `d
+ : `e -> `e -> `e, `e ~ Int (result of f1) ~ `b (parameter n) ~ `c (result of call)
=> `a -> Int -> Int
```

let h x y = f(g, x, y)
> ls h.* | next -> h.0  
> ls f.* | last -> f.1  
> ls g.* | last -> g.0  
> echo \x y -> f.1(g.0, x, y)  
> chmod r-x h.0
```
h :  `a -> `b -> `c, x : `a, y : `b
f : ( `d -> `e -> `f ) -> `e -> `d -> `f (renamed from d)
g: `g -> Int -> Int (renamed from g)
( `d -> `e -> `f) ~ (`g -> Int -> Int) (parameter f(g..))
`e ~ `a (parameter f(..x..))
`d ~ `b (parameter f(..y))
`f ~ `c (result of f, result of h)
=> (Substitute `a ~ `e ~ Int)
=> Int -> `b -> `c (Subsitute `c ~ `f ~ Int)
=> Int -> `b -> Int (Simplify)
=> h :: Int -> `a -> Int
```

let x = 31
> ls x.* | next -> x.3  
> echo 31 > x.3  
> chmod r-- x.3
```
31 : Int, x : `a
=> x :: Int
```

let y = 14
> ls y.* | next -> y.4  
> echo 14 > y.4  
> chmod r-- y.4
```
14 : Int, y : `a
=> y :: Int
```

h(x, y)
> ls h.* | last -> h.0  
> ls x.* | last -> x.3  
> ls y.* | last -> y.4  
> exec h.0(x.3, y.4) -> 35
```
h : Int -> `a -> Int, x : Int, y : Int
Int ~ `a (h(x..))
=> Int
```

let x = ref 4
> ls x.* | next -> x.4  
> mkdir x.4  
> echo 4 > x.4/contents  
> chmod r-x x.4  
> chmod rw- x.4/contents
```
x : `a ref, 4 : Int
=> x :: Int ref
```

let f x = do x := !x + 1; end !x
> ls f.* | next -> f.1  
> echo /x -> do x := !x + 1; end !x > f.1  
> chmod r-x f.1
```
f : `a -> `b, x ~ `a ~ `c ref, 1 : Int
(+) : `d -> `d -> `d, 
`d ~ Int (constant 1), 
`d ~ deref(x) ~ `c (param x)
`b ~ deref(x) ~ `c (result of f)
(Solve)
=> `d ref -> `d (`d ~ Int)
=> f :: Int ref -> Int
```

let g = f x
> ls g.* | next -> g.0  
> ls f.* | last -> f.1  
> ls x.* | last -> x.4  
> exec f.1(x.4) > g.0 # 5?  
> chmod r-- g.0
```
g : `a, f :: Int ref -> Int, x : Int ref
(result of f)
=> g :: Int
```

let g(x, y, z) = x + y + z
> ls g.* | next -> g.1  
> echo /x y z -> x + y + z > g.1  
> chmod r-x g.1
```
[TODO]
```

let f(x, y) = g(y, x, 3)
> ls f.* | next -> f.2  
> ls g.* | last -> g.1  
> echo /x y -> g.1(y, x, 3) > f.2  
> chmod r-x f.2

f(2, 3)
> ls f.* | last -> f.2  
> exec f.2(2, 3) -> 8

let rec h(x, y, z) = if x <= 0 then y + z else h(x - 1, z + 1, y)
> ls h.* | next -> h.1  
> echo /x y z -> if x <= 0 then y + z else h.1(x - 1, z + 1, y) > h.1  
> chmod r-x h.1

let rec fib n = if n < 0 then 1 else fib (n - 1) + fib (n - 2);;
> ls fib.* | next -> fib.0  
> echo /n -> if n < 0 then 1 else fib.0(n + 1) + fib.0(n - 2) > fib.0  
> chmod r-x fib.0

let x = ref 0;;
> ls x.* | next -> x.5  
> echo 0 > x/contents  
> chmod r-x x.5  
> chmod rwx x.5/contents

let f x = let n = ref 1 in while (!n > 1) do {x := !x - 1;; n := 0;;} done x;;
> ls f.* | next -> f.3  
> echo /x -> let n = ref 1 in while (!n > 1) do {x := !x - 1;; n := 0;; } done x;; > f.3  
> chmod r-x f.3

let x = {code = 0; msg = " " };;
> ls x.* | next -> x.6  
> echo 0 > x.6/code  
> echo " " > x.6/msg
> chmod r-x x.6/code
> chmod r-x x.6/msg

let f x = let n = ref 1 in while (!n > 1) do {x.msg := "once";; n := 0 } done x;; (n is not static here)
> ls f.* | next -> f.4  
> echo /x -> let n = ref 1 in while (!n > 1) do {x.msg := "once";; n:= 0} done x;; > f.4

f x;;
> ls f.* | last -> f.4  
> ls x.* | last -> x.6  
> exec f.4(x.6) ...

# Filesystem details

### Local variables
* How to handle local variables?  
Same as local parameters. In the local environment. They have to be enumerated
They can be seen as default parameters...

* How to handle recursive lets? They are handled only in recursive calls.  
var.0.d3? <- depth 3 suffix  
Variable versions are not very human readable.  
Maybe var/v0/d0?  
While loops may be translated to recursive functions?  
For loops may be translated to recursive functions?  
Looks hard to implement as variables in the for loop are modified...
Maybe callstack?

* How to handle function parameters? By copy? Pointers by copy?  
All parameters by copy. Records are pointers..

* How to copy local variables to global variables?

### Records
* How to store a simple record?  
env = { "field1" => 1, "field2" => 2 }  
.global/env/v.0/field1 -> 1  
.global/env/v.0/field2 -> 2  
Passing records by copy by default.

* Are records consistent with this system? Polymorphic records?  
.global/var/v.0/d0 as integer  
.global/var/v.0/d1 as float  
.global/var/v.0/d1
!x.a.b instead of !x  
x.contents.a.b

* Are GADTs supported? Ex. Recursive trees? Can they be emulated like records?  
let z = Tree x1 (Tree none none)  
echo x1 > z/Tree/1  
echo none > z/Tree/2/Tree/1  
echo none > z/Tree/2/Tree/2

Currently, they are always copied.

### Operators
* How to handle := and ! operator.  
Each ref is just a record with a field named contents.  
Thus !x should be an alias for x.contents  
Thus all records should be readable and writable.

* Operators are generally functions that can be invoked in the prefix/infix context.  
There are specific rules that describe the grammar of operators.  
For example a prefix operator cannot start with "!", but can be any combination of some characters (such as "<>+-").
* Default operators are external functions.

Useful links about operators:
* [(+) is an external function.](https://github.com/ocaml/ocaml/blob/trunk/stdlib/stdlib.ml)  
* [Operators in Ocaml](https://caml.inria.fr/pub/docs/manual-ocaml/expr.html#ss%3Aexpr-operators)
* [Grammar of operators in Ocaml](https://caml.inria.fr/pub/docs/manual-ocaml/lex.html#infix-symbol)

### Modules
* Are modules consistent with this system?  
Modules are just path prefixes.  
.local/f/v.1....
.global/f/v.1/...

* Multiple module instances
List.1/f/v.1/...
List.2/f/v.1/...

* References to private fields are banned outside of the module.
This is checked manually. It does not change the tree structure.

* Signatures and modules are indexed in the same way. (context ^. hmodules . lastindex)

### Pointers optimization TODO(Validate)
* How to handle very long paths? Can they be converted to pointers and enumerated?

* How should we keep the pointers????  
Lemma. Only one of the available local pointers will be used for calculations, at each given time.
Proof. To update the read-only value we would have to traverse the tree from the function call node to that node, thus we are in the middle of recursive subcall.  
Each recursive call should in some way overlay the local variables.
Those overlays are always local, thus can we use MonadReader here?
Each local pointer should be invalidated at the end of local operation.

### Other

* How to simplify filesystem with lenses?

* How does exec work?

* How to handle polymorphic functions? When parameters are records?  
a -> b

* How does evalOp work?

# API
It looks similar to a simple filesystem with symlinks.
Basic call stack is supported by Haskell.


How should names be represented?  
CanonicalName {
> * modulename :: Ident,
> * name :: Ident,
> * version :: Int,
> * recordidents :: [Ident]

}

DebugInfo {
> * filename :: Path [?]
> * line :: Int [?]

}

## External
* (exec) translate :: Expression -> AResult ()  
Executes function calls.

* (exec) eval :: Expression -> AResult Expression  
Executes built-in operators like +,-,*...

* (write) pushToEnvironment :: LabeledStatement -> Environment -> Environment  
Stores the expression in the filesystem.

* (read) peekFromEnvironment :: CanonicalName -> AResult Expression  
Reads the expression from the environment.

* fvBind :: Expression -> Expression  
Resolves static bindings using the free variables method. Converts all variables to CanonicalName pointers.

## Internal
* fvNext :: (Modulename as Ident, NName) -> CanonicalName

* fvLast :: (Modulename as Ident, NName) -> CanonicalName
