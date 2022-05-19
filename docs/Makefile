docs:
	mkdir -p ../.objs/gnatdoc/
	pandoc README.md > ../.objs/index.html
	GNATDOC_BIN=`dirname $$(which gnatdoc4)`;\
	  cp -fv $$GNATDOC_BIN/../share/gnatdoc/html/static/gnatdoc.css ../.objs/gnatdoc/
	cd ../.objs/gnatdoc/; gnatdoc4 ../../gnat/vss_text.gpr
