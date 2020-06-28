# Build mode (dev or prod)
BUILD_MODE=dev

all:
	gprbuild -p -P gnat/gnatcoll_text.gpr -XBUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)

build_tests:
	gprbuild -p -P gnat/gnatcoll_text_tests.gpr

check: build_tests
	.objs/tests/test_conversions
	.objs/tests/test_character_iterators
	.objs/tests/test_stream_element_buffer
	.objs/tests/test_string_equal
	.objs/tests/test_json_writer
	rm -f .objs/tests/.fails
	for f in testsuite/JSONTestSuite/test_parsing/*.json testsuite/JSON_checker/test/*.json; \
		do echo -n "`basename $$f`: "; \
		testsuite/run_json_reader_test $$f || touch .objs/tests/.fails; \
	done
	test ! -e .objs/tests/.fails

coverage:
	gcov --verbose .objs/*
