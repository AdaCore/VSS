::  Run test driver executable and pass necessary arguments
::  It is workaround of failure to process command line arguments with Unicode
::  characters by Makefile/bash on GitHub Actions.
chcp 65001
.objs\tests\test_application_arguments.exe hello привет გამარჯობა 👋
