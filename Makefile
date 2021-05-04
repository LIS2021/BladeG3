SRC=opal.ml ast.ml parser.ml graph.ml evaluator.ml def_use_gen.ml blade.ml utils.ml pipeline.ml
TEST_SRC=test/test_gen.ml test/test_graph.ml test/test0.ml test/test1.ml
EXEC=test_gen test_graph test0 test1

%.test: $(SRC) $(TEST_SRC)
	ocamlopt -o $@ $(SRC) test/$*.ml

clean:
	rm test/*.cmi test/*.cmx test/*.o *.cmi *.cmx *.o $(EXEC) &2> /dev/null
