#!/bin/sh
lein doc
(cd doc; make)
cp -vr doc/index.html /tmp/index.html;
cp -vr doc/codox /tmp/codox
cp -vr logo/logo.png /tmp/cats-logo.png
git checkout gh-pages;

rm -rf index.html codox
mv -fv /tmp/cats-logo.png logo.png
mv -fv /tmp/index.html ./index.html
mv -fv /tmp/codox ./codox

git add --all index.html
git add --all codox
git add --all logo.png

git commit -a -m "Update dev doc"
