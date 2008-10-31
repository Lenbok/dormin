ocamlc -ccopt -O -o dormin -custom -I +lablGL unix.cma lablgl.cma lablglut.cma \
 skin.c swizzle.c \
 slice.ml vec.ml qtr.ml xff.ml rend.ml skin.ml anb.ml skb.ml nto.ml nmo.ml

#mv skin.o skin1.o
#mv swizzle.o swizzle1.o
#ocamlopt.opt -o dormin.opt -I +lablGL unix.cmxa lablgl.cmxa lablglut.cmxa \
# slice.ml vec.ml qtr.ml xff.ml rend.ml skin.ml anb.ml skb.ml nto.ml nmo.ml \
# skin1.o swizzle1.o
