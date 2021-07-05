# Build mode (dev or prod)
BUILD_MODE=dev

all:
	gprbuild -p -P gnat/vss_text.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild -p -P gnat/vss_json.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild -p -P gnat/vss_json.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild -p -P gnat/vss_regexp.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild -p -P gnat/vss_markdown.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)

build_tests:
	gprbuild -p -P gnat/tests/vss_text_tests.gpr
	gprbuild -p -P gnat/tests/vss_json_tests.gpr
	gprbuild -p -P gnat/tests/vss_stream_tests.gpr
	gprbuild -p -P gnat/tests/vss_regexp_tests.gpr
	gprbuild -p -P gnat/tests/vss_markdown_tests.gpr

check: build_tests check_text check_json check_regexp check_markdown

check_text:
	.objs/tests/test_character_iterators
	.objs/tests/test_character_markers
	.objs/tests/test_converters
	.objs/tests/test_line_iterators
	.objs/tests/test_stream_element_buffer
	.objs/tests/test_text_streams
	.objs/tests/test_string_append
	.objs/tests/test_string_compare
	.objs/tests/test_string_conversions
	.objs/tests/test_string_delete
	.objs/tests/test_string_hash
	.objs/tests/test_string_insert
	.objs/tests/test_string_buffer
	.objs/tests/test_string_replace
	.objs/tests/test_string_slice
	.objs/tests/test_string_split_lines
	.objs/tests/test_string_vector

check_json:
	.objs/tests/test_json_content_handler
	rm -f .objs/tests/.fails
	for f in testsuite/json/JSONTestSuite/test_parsing/*.json testsuite/json/JSON_checker/test/*.json; \
		do echo -n "`basename $$f`: "; \
		testsuite/run_json_reader_test $$f || touch .objs/tests/.fails; \
	done
	test ! -e .objs/tests/.fails
	.objs/tests/test_json_writer testsuite/json/test_json_writer.expected

check_regexp: re_tests
	.objs/tests/test_regexp_re_tests < re_tests

re_tests:
	curl -o $@ https://raw.githubusercontent.com/Perl/perl5/blead/t/re/re_tests

check_markdown: commonmark-spec
	cd commonmark-spec; python3 test/spec_tests.py --program ../.objs/tests/test_markdown_tests |\
	  grep -E "^Example|^[0-9]+.passed" |\
	  tee markdown_tests_result
	diff -u testsuite/markdown/xfails.txt commonmark-spec/markdown_tests_result

commonmark-spec:
	@echo Checkout commonmark repo with:
	@echo git clone --depth=1 https://github.com/commonmark/commonmark-spec
	@false

coverage:
	gcov --verbose .objs/*

docs: all
	make -C docs

clean:
	rm -rf .objs re_tests
