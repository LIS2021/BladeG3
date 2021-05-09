SRC=opal.ml ast.ml parser.ml graph.ml evaluator.ml def_use_gen.ml blade.ml utils.ml pipeline.ml lleval.ml llpipe.ml
BLADE=opal.ml ast.ml parser.ml graph.ml def_use_gen.ml blade.ml run_blade.ml
TEST_SRC=test/test_gen.ml test/test_graph.ml test/test0.ml test/test1.ml
OBJ=test1 test2 test3 test4

%.test: $(SRC) $(TEST_SRC)
	ocamlopt -o $@ $(SRC) test/$*.ml

all: pipe run_blade

pipe: $(SRC)
	ocamlopt -o $@ $(SRC)

run_blade: $(BLADE)
	ocamlopt -o $@ $(BLADE)

%.native: $(SRC)
	ocamlbuild -pkgs "llvm llvm.bitwriter" $@

test%: 
	clang -emit-llvm -c -S llfuncs.c -o llfuncs.bc
	ocamlbuild -pkgs "llvm llvm.bitwriter" llpipe.native
	./llpipe.native --fancy --blade test/$@.txt > $@.ll
	llvm-as $@.ll
	llvm-link $@.bc llfuncs.bc -o $@-lnk.bc
	llc -filetype=obj $@-lnk.bc -o $@.o
	clang $@.o -o $@
	rm -f *.o *.native *.bc
	rm -rf _build

clean:
	rm -f test/*.cmi test/*.cmx test/*.o *.cmi *.cmx *.o *.test pipe run_blade *.trace *.out *.native *.ll *.bc ${OBJ}
	rm -rf _build

