
all:
	gprbuild -p -P test.gpr -XSUPERPROJECT= -XBUILD_MODE=prod 

run: all
	valgrind --tool=callgrind .objs/main

clean:
	rm -rf .objs ../../../../.objs
