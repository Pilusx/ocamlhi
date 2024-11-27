# Type inference

This file describes, how inference should work...  
It contains implementation details.  
Examples can be found in [StaticBinding.md](StaticBinding.md).  
It is based on the [Control.Unification](https://hackage.haskell.org/package/unification-fd-0.10.0.1/docs/Control-Unification.html).

## Requirements
* All paths in records should have defined types.
* All functions should have assignable types.
* All let in assignments define constraints.
* All assignments define constraints.
* All function calls define constraints.
* All match with statements define constraints.
* All if/while <...> define bool (TBool) constraints.
* All for and while statements produce unit (TUnit) constraints.
* All constructors should check signatures.
* Sometimes there are manually specified types (typedef statements).
```
type `a Node = Nil | Node `a
- Nil => type = `a Node
- `a Node => type = `a Node

Example:
type (`a,`b) eth = Left `a | Right `b
- Left 3 => Left (`a = int) => type = (int, `b) eth
- Left Left 3
=> Left (`a = (`a1 = int, `b1) eth)
=> type = ((int, `b1) eth, `b) eth

```

### Typing records
This one is not trivial. There are currently 3 most important ways of handling records. Each has a seperate way of type inference.

If the variable has a assigned type signature then the signature defines the type.
* Constructors  
  Otherwise, the type is the last type that matches all fields (names) of the record.
* Getters  
  Otherwise, the type is defined by the last type signature that matches the field.
* Setters  
  Otherwise, the type is defined by the last type signature that matches the field.

What should happen in case of paths?  
What should happen in case of mutable fields?

```
type `a record = {field: `a};;
type `a record2 = {mutable field2: `a};;

let x : int record record2 = {field2 = {field = 3}};;
let y : int record2 record = {field = {field2 = 3}};;

x.field2 <- {field = 4};;
let f y = y.field;;
```
More examples can be found in [B_RecordType.ml](../../tests/bad/B_RecordType.ml).


## Algorithm
* Given all former global variables' types...
* Find constraints in statement.
* Solve using unification.
* Push the result to the environment.

# API
[TODO] It seems out of date...

## External
* typeResolveStatement :: Statement -> Result

## Internal

TypeHandler {
> * nextfresh - counter for used variables
> * constraints - Keeps all gathered type relations of type: TSubsumes(t1,t2) or TEquals(t1, t2)
> * assignments :: Map.Map Variable Int - Keeps all assignments of types.
> * substitutions :: Map.Map Int Type - Maps fresh types into substitutions

}

### Fresh variables
* typeFresh :: AResult Type  
Returns a new fresh type

* typeFreshCached :: Variable -> AResult Type  
Assigns a new fresh type, or returns cached type.

### Handling constraints
* typeAssign :: Variable -> Type -> Result  
Pushes type constraint to the environment.

* typePatternMatch :: Typeable a => a -> AResult Type  
It is used for finding constraints in nested patterns in pattern matching.

* typeFindConstraints :: Typeable a => a -> AResult (Maybe Type)
It is used for finding constraints in expressions.
* typeJustFindConstraints :: Typeable a => a -> AResult Type

* typeUnify :: TypeConstraint -> Result  
Updates current substitutions with a new constraint. 
Can it be done faster?

* typeResolve :: [TypeConstraint] -> Result  
[TODO] ?

* typeOf :: Typeable a => a -> AResult Type  
[TODO] ?

### Type conversion
* typeUnknown :: Int -> Type  
Returns 0-indexed, n-th type. 
```
typeUnknown(3) = 'd
typeUnknown(26) = 'a1
```

* typeIsUnknown :: Type -> Maybe Int  
It is an inverse function to typeUnknown.

* typeIdent :: String -> Type  
Converts strings to types...
```
typeIdent "dummy_type" = `dummy_type
```

### Relabelling
* typeSimplify :: Type -> Type  
Makes the type more canonical i.e fresh variables are assigned in the printing order.
```
typeSimplify(( `c -> `b -> `d ) -> `b -> `c -> `d) 
= ( `a -> `b -> `c ) -> `b -> `a -> `c)
```

* typeRelabelingMap :: Type -> Map.Map Type Type  
Provides a relabelling mapping, that minimizes number of fresh variables. It simplifies types so that they look as in Ocaml.
```
typeRelabelingMap('c -> 'b -> 'd) = [(`c, `a), ('b, 'b), ('d, 'c)]
```

* typeRelabel :: IsType a => Map.Map Type Type -> a -> a  
Maps all fresh variables in the given type.
```
typeRelabel([(`c, `a), ('b, 'b), ('d, 'c)], 'c -> 'b -> 'd)
= 'a -> 'b -> 'c
```

* typeUnknowns :: IsType a => a -> [Int]  
Returns list of all 'fresh' types used in the given type.
```
typeUnknowns('d -> `b -> (`a -> `c) -> `a) = [3, 1, 0, 2]
```
