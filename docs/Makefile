SKIP_TEXT=$(shell find ../source/text -regex '.*\(impl\|internal\).*.ads' -printf %f,)
SKIP_JSON=$(shell find ../source/json -regex '.*\(impl\|internal\).*.ads' -printf %f,)

docs:
	gnatdoc -P ../gnat/vss_json.gpr --ignore-files=$(SKIP_TEXT),$(SKIP_JSON)
	cp -fv gnatdoc.css ../.objs/gnatdoc/
	pandoc README.md > ../.objs/index.html
