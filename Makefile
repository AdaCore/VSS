
all:
	gprbuild -p -P gnat/gnatcoll_text.gpr -cargs $(ADAFLAGS)

build_tests:
	gprbuild -p -P gnat/gnatcoll_text_tests.gpr

check: build_tests
	.objs/tests/test_conversions
	.objs/tests/test_character_iterators
	.objs/tests/test_json_writer
	for f in testsuite/JSONTestSuite/test_parsing/*.json testsuite/JSON_checker/test/*.json; \
		do echo -n "`basename $$f`: "; \
		testsuite/run_json_reader_test $$f; \
	done

coverage:
	gcov --verbose .objs/*
