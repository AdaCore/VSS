#!/bin/bash

for f in `find ../../../data/i18n-tests/css-text/text-transform -name '*.html'`; do
    src=$f
    dst=`basename ${f%.html}.txt`
    echo $src "=>" $dst
    xsltproc --html --output "$dst" extract-casing.xslt $src || rm -f "$dst"
done
