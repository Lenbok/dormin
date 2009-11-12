ocamlc -ccopt -O -o dormin -custom -I +lablGL unix.cma lablgl.cma lablglut.cma \
 skin.c skinvp.c swizzle.c \
 slice.ml vec.ml qtr.ml xff.ml skin.ml rend.ml anb.ml skb.ml nto.ml nmo.ml

ocamlc -g -ccopt -g -o imgv -custom -I +lablGL unix.cma lablgl.cma lablglut.cma \
 swizzle.c \
 slice.ml xff.ml nto.ml imgv.ml

#mv skin.o skin1.o
#mv swizzle.o swizzle1.o
#ocamlopt.opt -o dormin.opt -I +lablGL unix.cmxa lablgl.cmxa lablglut.cmxa \
# slice.ml vec.ml qtr.ml xff.ml rend.ml skin.ml anb.ml skb.ml nto.ml nmo.ml \
# skin1.o swizzle1.o skinvp.o
