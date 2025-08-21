
all:
	gprbuild -p -P test.gpr -XBUILD_PROFILE=release -XLIBRARY_TYPE=static

run: all
	valgrind --tool=callgrind .objs/main

clean:
	rm -rf .objs ../../../../.objs
