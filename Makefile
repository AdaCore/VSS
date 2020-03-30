
all:
	gprbuild -p -P gnat/gnatcoll_text.gpr -cargs $(ADAFLAGS)

check:
	gprbuild -p -P gnat/gnatcoll_text_tests.gpr
	.objs/tests/test_conversions
	.objs/tests/test_character_iterators
	.objs/tests/test_json_writer && (cat json.json); rm json.json
