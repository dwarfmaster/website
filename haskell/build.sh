#!/usr/bin/env bash

for f in articles/*/*.m4.hamlet; do
    name="$(basename `dirname $f`)"
    echo "define(ARTICLE, \"$name\")dnl" > tmp.build
    cat tmp.build articles/template.m4 $f | m4 > "${f%.m4.hamlet}.hamlet"
done
rm tmp.build

cabal build

