gprbuild -q -P test.gpr
gdb --batch --command=gdbinit --directory=$VSS_GDBPP .objs/main
