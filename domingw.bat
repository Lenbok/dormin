ocamlc -ccopt "-O2 -fno-strict-aliasing" -c skin.c skinvp.c swizzle.c
ocamlc -cclib "-L . -L %root%\mingw\lib" -o dormin -custom -I +lablGL unix.cma lablgl.cma lablglut.cma skin.o skinvp.o swizzle.o slice.ml vec.ml qtr.ml xff.ml skin.ml rend.ml anb.ml skb.ml nto.ml nmo.ml
