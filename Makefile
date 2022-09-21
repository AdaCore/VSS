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

OK_RE_TESTS := 449 # Number of re_tests to be passed

ifeq ($(OS),Windows_NT)
	VSS_PS=;
else
	VSS_PS=:
endif

.PHONY: docs

all:
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_gnat.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_text.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_json.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_regexp.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_xml.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_xml_templates.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)
	gprbuild $(GPRBUILD_FLAGS) gnat/vss_xml_xmlada.gpr -XVSS_BUILD_MODE=$(BUILD_MODE) -cargs $(ADAFLAGS)

generate:
	gprbuild $(GPRBUILD_FLAGS) gnat/tools/gen_ucd.gpr
	.objs/tools/gen_ucd data/ucd .objs/ucd.ada
	rm -f source/text/ucd/*.ad[sb]
	gnatchop .objs/ucd.ada source/text/ucd

build_tests:
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_text_tests.gpr
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_os_tests.gpr
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_json_tests.gpr
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_stream_tests.gpr
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_regexp_tests.gpr
	gprbuild $(GPRBUILD_FLAGS) gnat/tests/vss_html_tests.gpr

check: build_tests check_text check_json check_regexp check_html

check_text:
	.objs/tests/test_characters data/ucd
	.objs/tests/test_character_iterators
	.objs/tests/test_character_markers
	.objs/tests/test_converters
	.objs/tests/test_grapheme_cluster_iterators data/ucd
	.objs/tests/test_line_iterators
	.objs/tests/test_stream_element_vector
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
	.objs/tests/test_string_slice
	.objs/tests/test_string_split
	.objs/tests/test_string_split_lines
	.objs/tests/test_string
	.objs/tests/test_string_vector
	for f in testsuite/text/w3c-i18n-tests-casing/*.txt; do \
		echo "   $$f"; .objs/tests/test_string_casing_w3c_i18n $$f || return 1; \
	done
	.objs/tests/test_word_iterators data/ucd
	.objs/tests/test_standard_paths
ifeq ($(OS),Windows_NT)
	cmd.exe /c "testsuite\\run_test_application_arguments.cmd"
else
	.objs/tests/test_application_arguments hello привет გამარჯობა 👋
endif
	VSS_ENV1="A$(VSS_PS)B$(VSS_PS)C" .objs/tests/test_environment
	.objs/tests/test_command_line_parser

check_json:
	.objs/tests/test_json_content_handler
	.objs/tests/test_json_decimal_to_number /dev/null data/parse-number-fxx-test-data/data/*.txt
	rm -f .objs/tests/.fails
	for f in testsuite/json/JSONTestSuite/test_parsing/*.json testsuite/json/JSON_checker/test/*.json; \
		do echo -n "`basename $$f`: "; \
		testsuite/run_json_reader_test $$f || touch .objs/tests/.fails; \
	done
	test ! -e .objs/tests/.fails
	.objs/tests/test_json_writer testsuite/json/test_json_writer.expected

check_regexp: re_tests
	.objs/tests/test_regexp
	.objs/tests/test_regexp_re_tests $(OK_RE_TESTS) < re_tests

check_html:
	rm -f .objs/tests/.fails
	for f in testsuite/html/test_data/*.xhtml; do \
	  echo -n "$$f: "; if .objs/tests/test_html_writer $$f 1>.objs/out 2>.objs/err; (cat .objs/out; sed 's/.*[\/\\]\(.*:\)/\1/' .objs/err) | diff --strip-trailing-cr -u -- $${f%xhtml}out - ; then echo "PASS"; else echo "FAIL"; touch .objs/tests/.fails; fi ; \
	done
	test ! -e .objs/tests/.fails

check_install:
	echo 'with "vss_text.gpr";'             >  example.gpr
	echo 'with "vss_json.gpr";'             >> example.gpr
	echo 'with "vss_regexp.gpr";'           >> example.gpr
	echo 'project Example is'               >> example.gpr
	echo '   for Main use ("example.adb");' >> example.gpr
	echo 'end Example;'                     >> example.gpr
	echo 'with VSS.Strings;'                >  example.adb
	echo 'with VSS.JSON;'                   >> example.adb
	echo 'with VSS.Regular_Expressions;'    >> example.adb
	echo 'procedure Example is'             >> example.adb
	echo 'begin null; end;'                 >> example.adb
	gprbuild -aP $(INSTALL_PROJECT_DIR) -P example.gpr
	gprclean -aP $(INSTALL_PROJECT_DIR) -P example.gpr
	rm -f example.*

re_tests:
	curl -o $@ https://raw.githubusercontent.com/Perl/perl5/blead/t/re/re_tests

coverage:
	find .objs/ -name *.o | xargs -s 512 gcov || true

docs:
	make -C docs

clean:
	rm -rf .objs .libs re_tests

install:
	gprinstall $(GPRINSTALL_FLAGS)/gnat -f -p -P gnat/vss_gnat.gpr
	gprinstall $(GPRINSTALL_FLAGS)/text -f -p -P gnat/vss_text.gpr
	gprinstall $(GPRINSTALL_FLAGS)/json -f -p -P gnat/vss_json.gpr
	gprinstall $(GPRINSTALL_FLAGS)/regexp -f -p -P gnat/vss_regexp.gpr
	gprinstall $(GPRINSTALL_FLAGS)/xml -f -p -P gnat/vss_xml.gpr
	gprinstall $(GPRINSTALL_FLAGS)/xml_templates -f -p -P gnat/vss_xml_templates.gpr
	gprinstall $(GPRINSTALL_FLAGS)/xml_xmlada -f -p -P gnat/vss_xml_xmlada.gpr

misc: # Check compilation of other projects
	gprbuild $(GPRBUILD_FLAGS) -aPgnat gnat/tools/json_schema.gpr
	gprbuild $(GPRBUILD_FLAGS) -aPgnat examples/regexp/grep.gpr
	gprbuild $(GPRBUILD_FLAGS) examples/blogs/json_1/blog_1.gpr

spellcheck:
	@STATUS=0; \
	for J in source/*/*.ads; do \
	  sed -e 's/#[^#]*#//g' -e "s/'\([A-Z]\)/ \1/g" $$J |   \
	  aspell list --lang=en --home-dir=./data/ --ignore-case > /tmp/spell.txt; \
	  if [ -s /tmp/spell.txt ] ; then \
	    echo "\n$$J:"; sort -u -f /tmp/spell.txt; STATUS=1; \
	  fi  \
	done; \
	if [ $$STATUS != 0 ] ; then \
	   echo "\n\nFIX SPELLING or append exceptions to data/.aspell.en.pws !!!" ; \
	   exit 1 ; \
	fi
