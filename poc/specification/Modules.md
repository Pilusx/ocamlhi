# Modules
What can you do with modules?

* Define module signatures.  
```
module type X = sig ... end
```
* Define module implementation.  
```
module type X_impl : X = struct ... end
```
* Use definitions from modules. (variables / function call)  
```
X_impl.f 3
```
* Modify mutable records from modules.  
```
X_Impl.x <- 3
```
* Print information about module. (Module documentation)  
```
#show X_Impl
```
* Define functors.  
```
[TODO] ?
```
* Extend modules with module inclusion.  
```
module X2_impl = struct 
include X_impl 
... 
end
```  

Specific usage can be found in [92_Module.ml](../../tests/good/92_Module.ml)

## References
[Modules in Ocaml](https://ocaml.org/learn/tutorials/modules.html)
