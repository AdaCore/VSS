This directory contains casing conversion tests extracted from W3C i18n-tests, see

https://github.com/w3c/i18n-tests

To rebuild data files clone repository into <VSS>/data/i18n-tests directory

    cd <VSS>/data
    git clone https://github.com/w3c/i18n-tests.git

When run extract-casing.sh script from this directory

    cd <VSS>/testsuite/text/w3c-i18n-tests-casing
    ./extract-casing.sh

Some produced files was removed, because:
 1. They checks locale specific case conversions
 1. Expected results are different from defined by Unicode 13.0
