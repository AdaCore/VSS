# External data for the testsuite

To download extra data and create the package run:

```sh
rm -rf data
mkdir data
cd data
curl -o UCD-15.1.0.zip https://www.unicode.org/Public/15.1.0/ucd/UCD.zip
mkdir ucd
unzip UCD-15.1.0.zip -d ucd
rm -f UCD-15.1.0.zip
mkdir emoji
curl -o emoji/ReadMe.txt https://www.unicode.org/Public/emoji/15.1/ReadMe.txt
curl -o emoji/emoji-sequences.txt https://www.unicode.org/Public/emoji/15.1/emoji-sequences.txt
curl -o emoji/emoji-test.txt https://www.unicode.org/Public/emoji/15.1/emoji-test.txt
curl -o emoji/emoji-zwj-sequences.txt https://www.unicode.org/Public/emoji/15.1/emoji-zwj-sequences.txt
git clone https://github.com/nigeltao/parse-number-fxx-test-data
rm -rf parse-number-fxx-test-data/.git
git clone https://github.com/json5/json5-tests.git
rm -rf json5-tests/.git
curl -O https://raw.githubusercontent.com/Perl/perl5/blead/t/re/re_tests
cd ..
tar caf vss-tests-data-`date +%Y%m%d`.tar.bz2 data
```
