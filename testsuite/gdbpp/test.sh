gprbuild -q -P test.gpr
if [ "$OS" = "Windows_NT" ]; then
  gdb --batch --command=gdbinit.windows --directory=$VSS_GDBPP .objs/main
else
  gdb --batch --command=gdbinit.posix --directory=$VSS_GDBPP .objs/main
fi
