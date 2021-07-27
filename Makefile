# Build mode (dev or prod)
BUILD_MODE=dev

GPRBUILD_FLAGS = -p -j0
PREFIX                 ?= /usr
GPRDIR                 ?= $(PREFIX)/share/gpr
LIBDIR                 ?= $(PREFIX)/lib
BINDIR                 ?= $(PREFIX)/bin
INSTALL_PROJECT_DIR    ?= $(DESTDIR)$(GPRDIR)
INSTALL_INCLUDE_DIR    ?= $(DESTDIR)$(PREFIX)/include/vss
INSTALL_EXEC_DIR       ?= $(DESTDIR)$(BINDIR)
INSTALL_LIBRARY_DIR    ?= $(DESTDIR)$(LIBDIR)
INSTALL_ALI_DIR        ?= $(INSTALL_LIBRARY_DIR)/vss

GPRINSTALL_FLAGS = --prefix=$(PREFIX) --exec-subdir=$(INSTALL_EXEC_DIR)\
 --lib-subdir=$(INSTALL_ALI_DIR) --project-subdir=$(INSTALL_PROJECT_DIR)\
 --link-lib-subdir=$(INSTALL_LIBRARY_DIR) --sources-subdir=$(INSTALL_INCLUDE_DIR)


all:
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_text.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_json.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)

generate:
	gprbuild $(GPRBUILD_FLAGS) gnat/tools/gen_ucd.gpr
	.objs/tools/gen_ucd data/ucd .objs/ucd.ada
	rm -f source/text/ucd/*.ad[sb]
	gnatchop .objs/ucd.ada source/text/ucd

build_tests:
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_text_tests.gpr
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_json_tests.gpr
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_stream_tests.gpr
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_regexp_tests.gpr

check: build_tests check_text check_json check_regexp

check_text:
	.objs/tests/test_characters data/ucd
	.objs/tests/test_character_iterators
	.objs/tests/test_character_markers
	.objs/tests/test_converters
	.objs/tests/test_grapheme_cluster_iterators data/ucd
	.objs/tests/test_line_iterators
	.objs/tests/test_stream_element_buffer
	.objs/tests/test_text_streams
	.objs/tests/test_string_append
	.objs/tests/test_string_casing
	.objs/tests/test_string_compare
	.objs/tests/test_string_conversions
	.objs/tests/test_string_delete
	.objs/tests/test_string_hash
	.objs/tests/test_string_insert
	.objs/tests/test_string_buffer
	.objs/tests/test_string_normalization data/ucd
	.objs/tests/test_string_replace
	.objs/tests/test_string_slice
	.objs/tests/test_string_split_lines
	.objs/tests/test_string_vector
	for f in testsuite/text/w3c-i18n-tests-casing/*.txt; do \
		echo "   $$f"; .objs/tests/test_string_casing_w3c_i18n $$f || return 1; \
	done
	.objs/tests/test_word_iterators data/ucd

check_json:
	.objs/tests/test_json_content_handler
	.objs/tests/test_json_document
	rm -f .objs/tests/.fails
	for f in testsuite/json/JSONTestSuite/test_parsing/*.json testsuite/json/JSON_checker/test/*.json; \
		do echo -n "`basename $$f`: "; \
		testsuite/run_json_reader_test $$f || touch .objs/tests/.fails; \
	done
	test ! -e .objs/tests/.fails
	.objs/tests/test_json_writer testsuite/json/test_json_writer.expected

check_regexp: re_tests
	.objs/tests/test_regexp_re_tests < re_tests

check_install:
	echo 'with "vss_text.gpr";'             >  example.gpr
	echo 'with "vss_json.gpr";'             >> example.gpr
	echo 'project Example is'               >> example.gpr
	echo '   for Main use ("example.adb");' >> example.gpr
	echo 'end Example;'                     >> example.gpr
	echo 'with VSS.Strings;'    >  example.adb
	echo 'with VSS.JSON;'       >> example.adb
	echo 'procedure Example is' >> example.adb
	echo 'begin null; end;'     >> example.adb
	gprbuild -aP $(INSTALL_PROJECT_DIR) -P example.gpr
	gprclean -aP $(INSTALL_PROJECT_DIR) -P example.gpr
	rm -f example.*

re_tests:
	curl -o $@ https://raw.githubusercontent.com/Perl/perl5/blead/t/re/re_tests

coverage:
	gcov --verbose .objs/*

docs: all
	make -C docs

clean:
	rm -rf .objs re_tests

install:
	gprinstall $(GPRINSTALL_FLAGS)/gnat -p -P gnat/vss_gnat.gpr
	gprinstall $(GPRINSTALL_FLAGS)/text -p -P gnat/vss_text.gpr
	gprinstall $(GPRINSTALL_FLAGS)/json -p -P gnat/vss_json.gpr
