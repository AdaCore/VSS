
all:
	gprbuild -p -P gnat/gnatcoll_text.gpr -cargs $(ADAFLAGS)
