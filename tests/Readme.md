# Tests' description
01 - Types  
02 - Arithmetic, Comparisons  
03 - Conditional expressions (if)  
04 - Functions with multiple parameters, Recursion  
05 - Anonymous functions, Higher-order functions, Partial application  
06 - Error handling (exceptions), throw [+], catch [TODO]    
10 - Lists with pattern matching  
10 - Operations on lists (empty, head, tail)  
10 - Syntactic sugar (List rec(x, y; tl))  
10 - List('a), List('a -> 'a), List(List('a))  
11 - Simple algebraic data types (Maybe('a), T()), Pattern matching  
12 - Static binding  
13 - Polymorphic data types[+], Recursive data types ('a tree)[+], type aliases [TODO]  
13 - Recursive pattern matching [+], Exhaustiveness checks [TODO]  
13 - Static typing  
14 - Lazy data types [TODO]  
14 - Records  

91 - Imperative programming (for loop, while loop, assignment operator)  
92 - Modules (#use directive), Module documentation  
93 - Records with recursive modification  
94 - Operators as function parameters f((+)), user-defined operators  
95 - Classes, Object-oriented programming [TODO]  
96 - Multithreading, Synchronization [TODO]  

98 - Stdlib (Either, Maybe, List, Tree, Pair, Math)  
99 - Stdlib (Combinations of modules, EitherList, NeuralNetwork...)

```
If (all tests use assertions) && (always some of them fail) && (suddenly all tests pass)
Then (bug in assertions) || (new feature) ?
```
```
It would be better, not to remove 'done' tasks...
```
# TODO [Next=89]
## Active
[43] Fix static typing. (aliases)  
[69] Reduce number of usages of pushConstraint operation.  
Cache types from last statements, do not add constraints for one variable multiple times...  
After each typeResolveConstraints call, all unused fresh variables can be removed.  
This can be used to decrease the number of variables in memory.  
Print execution stats at the end of the program.[+]  

## Low priority
### Grammar errors
[83] Rewrite some of the trace functions to JSON (maybe AESON?). (traceEnvironment)  

### Code quality
[28] Compile with -Wall  
[46] Add two lines of comments to each test..  
[60] Refactor unfoldRecursivePair   
[62] File level lexical scoping. Hints about ambiguity.  
[66] Add ./ocamlhi --debug flag that enables all #enable statements.  
[69] Fix unnecessary outputs, when loading modules with #use statement..  
[70] Try (?) to rewrite StaticBinding to StaticBindingT, check if ReaderT can be refactored.  
[74] Integrate BNFC preprocessor into Setup.hs
[78] Return exit codes instead of printing "Interpreter successful"....  
[81] Use Haddock.  

### Dependencies upgrades
[21][22] Line numbers in errors and stack traces. File:Line:Function  
Line errors are (?) implemented in BNFC 2.9.1 using the --functor flag.   
This would probably complicate generating TermTypes...  
Upgrading bnfc would also add 'internal' rules (preety printing for 'SLabeledBound').  
[29] Add deps to make all  
make deps should install non-cabal dependencies only...  
[67] Including TRef in bnfc grammar may be a good idea.  
Include TypeConstraint as an unreachable node for parser.    
[75] Remove external/bindings.dsl.h during upgrade to a new version of Cabal.  

### Future
[72] Garbage collector ? (FFI)  
[73] Compiler?  
[76] N-N transpiler? :O  
It would make the usage of libraries from other languages possible.  
[84] Most of the ocaml features (especially object oriented programming) can be ported to a nice, compact compiler of C++.  
```
g++h is not a joke. XDDD
```
Easier to read template errors...  
C++ interpreter for proof of concepts, C++ compiler for production usage.

### Other
[64] Add disable statement. Example: #disable trace-input-tree.  
Should it work as #define and #undef in C?  
[65] Rewrite loops to fixpoints  
[85] Check license issues.  
- tests/stdlib/* : LGPLv2.1
- ocaml.fc  

[86] Self hosting.  
[88] Experimental plugins. Weighted average minimization of use cases.  

## Compatibility with ocaml
[16] Print function types instead of definitions in records  
[37] Make bad and good tests interpretable by ocaml again.  
Merge with old tests from change abb87374924a7abcba36a088aac7f46ed2952564  
[42] Maybe(a) is 'a option in Ocaml.  
[48] Fix brackets around operators, example: (x+3) should be x+3. Add operator precedence.
Fix boolean expressions (brackets).  
[50] Add private module members  
    https://ocaml.org/learn/tutorials/modules.html  
[51] Start adding tests that are interpretable by both ocaml and ocamlhi.  
Add a script that compares both outputs.  
[57] Operator , :)  
[58] Performance test with the ocaml interpreter  
[71] Fix ocaml comments. "(* *)"  (Added to bnfc, rewrite tests?)  
[82] Integer emulation... Is 'int' bignum in ocaml?  
[87] Be 'compatible' with at least Ocaml API version 4.12.  
https://ocaml.org/api/Stdlib.html#top

# Configuration Errors
1. If recompilation fails with error "missing module Data.C2Hsc", try to "cabal install c2hsc" globally?
2. Sometimes doing `cabal install happy` globally helps..
