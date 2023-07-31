# External data for the testsuite

To download extra data and create the package run:

```sh
rm -rf data
mkdir data
cd data
curl -o UCD-15.0.0.zip https://www.unicode.org/Public/15.0.0/ucd/UCD.zip
mkdir ucd
unzip UCD-15.0.0.zip -d ucd
rm -f UCD-15.0.0.zip
git clone https://github.com/nigeltao/parse-number-fxx-test-data
rm -rf parse-number-fxx-test-data/.git
curl -O https://raw.githubusercontent.com/Perl/perl5/blead/t/re/re_tests
tar caf ../vss-tests-data.tar.bz2 .
```
