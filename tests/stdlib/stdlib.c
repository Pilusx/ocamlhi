#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

// https://caml.inria.fr/pub/docs/oreilly-book/html/book-ora115.html may be useful...

CAMLprim value to_string(value x) {
    CAMLparam1(x);
    value v = caml_copy_string("TODO to_string");
    CAMLreturn(v);
}

CAMLprim value debug() {
    CAMLparam0();
    CAMLreturn(Val_false);
}

CAMLprim value type_of(value x) {
    CAMLparam1(x);
    value v = caml_copy_string("TODO type_of");
    CAMLreturn(v);
}
