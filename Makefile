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

llpipe: $(SRC)
	ocamlbuild -pkgs "llvm llvm.bitwriter" $@.native

%.native: $(SRC)
	ocamlbuild -pkgs "llvm llvm.bitwriter" $@

llfuncs.bc: llfuncs.c
	clang -emit-llvm -O0 -c -S $< -o $@

%.bc: llfuncs.bc
	llvm-as $*.ll -o $*-ul.bc
	llvm-link $*-ul.bc llfuncs.bc -o $@
	rm -f $*-ul.bc llfuncs.bc

clean:
	rm -f test/*.cmi test/*.cmx test/*.o *.cmi *.cmx *.o *.test pipe run_blade *.trace *.out *.native *.ll *.bc ${OBJ}
	rm -rf _build

