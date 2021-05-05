SRC=opal.ml ast.ml parser.ml graph.ml evaluator.ml def_use_gen.ml blade.ml utils.ml pipeline.ml
BLADE=opal.ml ast.ml parser.ml graph.ml def_use_gen.ml blade.ml run_blade.ml
TEST_SRC=test/test_gen.ml test/test_graph.ml test/test0.ml test/test1.ml

%.test: $(SRC) $(TEST_SRC)
	ocamlopt -o $@ $(SRC) test/$*.ml

all: pipe run_blade

pipe: $(SRC)
	ocamlopt -o $@ $(SRC)

run_blade: $(BLADE)
	ocamlopt -o $@ $(BLADE)

clean:
	rm -f test/*.cmi test/*.cmx test/*.o *.cmi *.cmx *.o *.test pipe run_blade *.trace *.out
