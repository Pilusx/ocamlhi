![Logo](./assets/ocamlhi.jpeg)

# ocamlhi - Caml interpreter written in Haskell

> [!NOTE]  
> [![Last stable release](https://img.shields.io/github/v/tag/pilusx/ocamlhi?label=Release)](https://github.com/pilusx/ocamlhi/releases/latest) [![License](https://img.shields.io/badge/License-BSD_3--Clause-red.svg)](LICENSE)  
> If you want to test the interpreter, use the latest released version.

This package contains a simple interpreter of Caml with various algorithms such as static binding and static typing.

> [!NOTE]  
> Check [DeepAI](https://deepai.org/machine-learning-model/text2img). The image was generated using Generative AI with the following prompt.
```
Camel with a tshirt containing a haskell logo, sitting in front of the computers.
```

## Working with containers
Firstly, build the docker image:
```
    make build_docker
```
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

## Build the interpreter
```
    make build
```

## Tests
![GIF - Usage](./assets/demo.gif)

| Index | Features      |
| :---: | :--------------- |
| 01 | Simple types: char, double, int, string|
| 02 | Arithmetic, Comparisons |
| 03 | Conditional expressions (if then else) |
| 04 | Functions with multiple parameters, Recursion | 
| 05 | Anonymous functions, Higher-order functions, Partial application |
| 06 | Static binding |
| 07 | Imperative programming (for loop, while loop)
| 08 | Pattern matching |
| 09 | Operations on lists (head, map, tail) | 
| 10 | Syntactic sugar for lists (x:y:tl) |
| 11 | Polymorphic lists: ('a -> 'a) list, 'a list list | 
| 12 | Recursive data types ('a tree) |
| 13 | Static typing |
| 14 | Generalized algebraic data types: Either, Maybe, Pair |
| 15 | Records, References (assignments) |
| 16 | Tuples |
| 17 | Modules |

To run the tests, use one of the following:
```
    make test
    python3 unittests.py [--print_failed]
    ./ocamlhi -I tests/stdlib -I tests/good 01_Constants.ml
```

> [!TIP]  
> If the output is "Killed" you probably have exceeded timeout (timeout = 1s). You may try to run the test manually (without using Python).

You can also compare the results with the original OCaml interpreter (v.4.05.0). Unfortunately, opam cannot be used during building of the image. Thus you have to install it before testing.
```
    make install_ocaml
    make test_ocaml
```

You can test the style of the source code by using:
```
    make install_haskell
    make test_style
```

## Grammar changes
If you need to update the grammar you can:
1. Modify the ocaml.fc file.
2. Run `make bnfc`
3. Resolve changes manually.

## Out of scope / Possible extensions
* Be 'compatible' with Ocaml API.   
* Fix static typing. (aliases)  
* Add private module members. 
* Add a garbage collector.  
* Integer emulation... Is 'int' bignum in ocaml?  
* Extend modules. Implement 'include' and 'open'.  
* Implement classes (object-oriented programming).  
* Implement multithreading and synchronization.  
* Implement lazy variables.  
* Implement catching exceptions. (raise / catch)  
* Implement exhaustiveness checks for pattern matching.   
* Implement recursive module signatures.  
* Implement a command line interpreter (REPL).  

## References
* [OCaml programming book](https://cs3110.github.io/textbook/chapters/interp/inference.html)
* [OCaml language description](https://caml.inria.fr/pub/docs/manual-ocaml/language.html)  
* [OCaml BNF syntax](http://www.willforge.fr/wikiosp/index.php/OCaml/BNF_syntax)  
* [Menhir Manual](http://gallium.inria.fr/~fpottier/menhir/manual.pdf)    
* [Modules in Ocaml](https://ocaml.org/learn/tutorials/modules.html)
* [Operators in Ocaml](https://caml.inria.fr/pub/docs/manual-ocaml/expr.html#ss%3Aexpr-operators)
* [Grammar of operators in Ocaml](https://caml.inria.fr/pub/docs/manual-ocaml/lex.html#infix-symbol)
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
* [Vytiniotis, Peyton Jones, Schrijvers, Sulzmann (2010). OutsideIn(X) Modular type inference with local assumptions](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp-outsidein.pdf?from=http%3A%2F%2Fresearch.microsoft.com%2F%7Esimonpj%2Fpapers%2Fconstraints%2Fjfp-outsidein.pdf)
* [Mark P. Jones (2000). Typing Haskell in Haskell](https://web.cecs.pdx.edu/~mpj/thih/TypingHaskellInHaskell.html)
* [Xavier Leroy (1992). Polymorphic typing of an algorithmic language](https://xavierleroy.org/publi/phd-thesis.pdf)
* [David J. King, John Launchbury (1998). Lazy Depth-First Search and Linear Graph Algorithms in Haskell.](https://www.researchgate.net/publication/2295532_Lazy_Depth-First_Search_and_Linear_Graph_Algorithms_in_Haskell)
