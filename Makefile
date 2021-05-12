SRC=opal.ml ast.ml parser.ml graph.ml evaluator.ml def_use_gen.ml blade.ml utils.ml pipeline.ml
LLSRC=$(SRC) lleval.ml llpipe.ml
BLADE=opal.ml ast.ml parser.ml graph.ml def_use_gen.ml blade.ml run_blade.ml
TEST_SRC=test/test_gen.ml test/test_graph.ml test/test0.ml test/test1.ml
OBJ=test1 test2 test3 test4 test5 test6 test7 test8 test9

all: pipe run_blade

pipe: $(SRC)
	ocamlopt -o $@ $(SRC)

run_blade: $(BLADE)
	ocamlopt -o $@ $(BLADE)

llpipe: $(LLSRC)
	ocamlbuild -pkgs "llvm" $@.native

%.native: $(LLSRC)
	ocamlbuild -pkgs "llvm" $@

llfuncs.bc: llfuncs.c
	clang -emit-llvm -O0 -c -S $< -o $@

test%.bc: llfuncs.bc
	llvm-as test$*.ll -o test$*-ul.bc
	llvm-link test$*-ul.bc llfuncs.bc -o $@
	rm -f test$*-ul.bc

test%: test%.bc
	llc -filetype=obj $< -o $@.o
	clang -O0 $@.o -o $@
	rm *.o

cleantest:
	rm -f test*.bc test*.ll ${OBJ} test*nb test*bs test*bf test*v

clean:
	rm -f test/*.cmi test/*.cmx test/*.o *.cmi *.cmx *.o *.test pipe run_blade *.trace *.out *.native *.ll *.bc ${OBJ} test*nb test*bf test*bs test*v
	rm -rf _build

