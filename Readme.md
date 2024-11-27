# Ocamlhi - Caml-like functional language interpreter written in Haskell
## Useful files 
[Modified language grammar](ocaml.fc)  
[Tests' description and known bugs](tests/Readme.md)

## Language modifications
1. Function calls should be Ocaml-like.
    [Work in progress.]
2. Expressions with infix operators always have to be enclosed in brackets.
3. Static-typing with type signatures.
4. Static bindings.
5. #enable statements which modify the behaviour of the interpreter.
6. C++ style garbage collector based on shared pointers.

## Naming Convention (Source)
1. eval* functions evaluate expressions.
2. apply* functions apply functions to elements and returns the result.
3. trans* functions modify the environment.
4. to\*From\* functions perform casting.
5. type* functions are connected with type evaluation.
6. trace* functions print to stdout.
7. make* functions generate code.
8. fv* functions are connected with static binding.
9. set* functions modify the scope properties.
10. mem* functions are responsible for memory management.

## Build docker image
```
    make build_docker
```

## Dealing with containers
If you want to start the container and you have not already created it use:
```
    make run_docker
```
Otherwise attach to the container:
```
    make start_docker
```
If you want to delete the container run:
```
    docker container rm ocamlhi
```

## Build
```
    make build
```

## Test
```
    make test
    python3 unittests.py [--print_failed]
    ./ocamlhi tests/good/01_Constants.ml
    ./ocamlhi -s tests/good/99_Tree.ml # TODO(?)
```

## Style
```
    ./test_style.sh
    make test_style
```
## Debugging
Use printing functions inside the code:
``` 
    pushToOstream "Checkpoint"
    traceEnvironment;
```
Alternatively, you can change `defaultFlags` in `Flags.hs`.

## Grammar changes
If you need to update the grammar you can:
1. Modify the ocaml.fc file.
2. Run `make bnfc`
3. Resolve changes manually.

## Issues
If the output is "Killed" you probably have exceeded timeout (tests, timeout = 1s).  
You may try to run the test manually.

## References
* [OCaml language description](https://caml.inria.fr/pub/docs/manual-ocaml/language.html)  
* [OCaml BNF syntax](http://www.willforge.fr/wikiosp/index.php/OCaml/BNF_syntax)  
* [OCamli - Ocaml interpreter in Ocaml (BSD)](https://github.com/johnwhitington/ocamli)  
* [BNF converter (GPL)](https://bnfc.digitalgrammars.com/)  
* [Happy - Parser generator (BSD)](https://www.haskell.org/happy/)  
* [Alex - Lexical analyser generator (BSD)](https://www.haskell.org/alex/)  
* [Haskell documentation - Profiling](https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/profiling.html)
* [Stack Overflow - Why is impredicative polymorphism allowed only for functions in Haskell?](https://stackoverflow.com/questions/56448814/why-is-impredicative-polymorphism-allowed-only-for-functions-in-haskell)
* [TH - Template Haskell tutorial by Mark Karpov](https://markkarpov.com/tutorial/th.html)
* [PathDB](https://github.com/maxsumrall/pathdb)
* [ruslanspivak.com - Algorithm for nesting scopes in Python](https://ruslanspivak.com/lsbasi-part14/)
* [unification-fd example](https://gist.github.com/gelisam/11068221)
* [Monadic Compilers for Programming Languages](https://wiki.haskell.org/Modular_Monadic_Compilers_for_Programming_Languages)
* [Polymorphism in Ocaml - Ad-hoc, parametric, inclusion/subtyping](https://stackoverflow.com/questions/33227667/polymorphism-in-ocaml-ad-hoc-parametric-inclusion-subtyping)

### Referenced papers
* [Dezani, Mariangiola & Giannini, Paola & Zucca, Elena. (2009). The essence of static and dynamic bindings. 99-106.](https://www.researchgate.net/publication/220829330_The_essence_of_static_and_dynamic_bindings)
* [Erkok (2002). Value Recursion in Monadic Computations](https://leventerkok.github.io/papers/erkok-thesis.pdf)
* Sunil Kothari and James L. Caldwell (2007). Type Reconstruction Algorithms: A Survey
* [Vytiniotis, Peyton Jones, Schrijvers, Sulzmann (2010). OutsideIn(X) Modular type inference with local assumptions](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp-outsidein.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2F%7Esimonpj%2Fpapers%2Fconstraints%2Fjfp-outsidein.pdf)
* [Mark P. Jones (2000). Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/TypingHaskellInHaskell.html)
* [Xavier Leroy (1992). Polymorphic typing of an algorithmic language](https://xavierleroy.org/publi/phd-thesis.pdf)
* [David J. King, John Launchbury (1998). Lazy Depth-First Search and Linear Graph Algorithms in Haskell.](https://www.researchgate.net/publication/2295532_Lazy_Depth-First_Search_and_Linear_Graph_Algorithms_in_Haskell)
