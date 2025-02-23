# Revision history for ocamlhi

## 0.1.0.0  -- 2020-05-04

* First version. Released on an unsuspecting world.

## 1.0.0.0  -- 2024-11-27

* Implemented basic functionality of the interpreter.
It includes static typing, static binding and even 
a simple standard library.
* Added 55 test cases, out of which 51 pass.
* Added examples of advanced algorithms such as mergesort.

## 1.1.0.0  -- 2025-02-23

* Made tests interpretable by OCaml again.
* Added a formatter and reformatted all tests.
* Added a nice, AI generated image of a camel.
* Cleaned up shell scripts.
* Extended the grammar just a bit. Now we use optimistic parsing. 
Valid programs should be parsable, but invalid programs can too. :D
* Fixed usage of brackets and added syntactic sugar for lists.
* Refactored the interpreter. Especially, I have changed the
memory management.