
all:
	gprbuild -p -P test.gpr -XSUPERPROJECT= -XBUILD_PROFILE=release

run: all
	valgrind --tool=callgrind .objs/main

clean:
	rm -rf .objs ../../../../.objs
