
all:
	gprbuild -p -P gnat/gnatcoll_text.gpr -cargs $(ADAFLAGS)

check:
	gprbuild -p -P gnat/gnatcoll_text_tests.gpr
	.objs/tests/test_conversions
	.objs/tests/test_character_iterators
	.objs/tests/test_json_writer
	for f in testsuite/JSONTestSuite/test_parsing/*.json; \
		do echo -n "`basename $$f`:"; \
		.objs/tests/test_json_reader s $$f > /tmp/`basename $$f`-s.log || (echo " (s) FAIL"; false) && \
		(.objs/tests/test_json_reader i $$f > /tmp/`basename $$f`-i.log || (echo " (i) FAIL"; false)) && \
		(diff -u /tmp/`basename $$f`-s.log /tmp/`basename $$f`-i.log || (echo " (d) FAIL"; false)) && \
		echo " PASS"; \
	done

coverage:
	gcov --verbose .objs/*
