DIRECTORIES := dap lsp sarif
JSON_GEN := $(abspath ../../.objs/tools/gen_json)
GOT_FILE := $(shell mktemp -t got-XXXXXX.txt)

all: $(foreach dir,$(DIRECTORIES),$(dir)-run)

%-run:
	cd $*; $(JSON_GEN) `cat cmd.txt` > $(GOT_FILE)
	diff -u $*/expected.txt $(GOT_FILE)
	rm -f $(GOT_FILE)