#!/bin/sh
lein doc
(cd doc; make)
cp -vr doc/index.html /tmp/index.html;
cp -vr doc/static /tmp/static
cp -vr doc/codox /tmp/codox
cp -vr logo /tmp/static
git checkout gh-pages;

rm -rf static index.html codox
mv -fv /tmp/static .
mv -fv /tmp/index.html .
mv -fv /tmp/codox .

git add --all index.html
git add --all static
git add --all codox

git commit -a -m "Update doc"
