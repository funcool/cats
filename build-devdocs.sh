#!/bin/sh
lein doc
(cd doc; make)
cp -vr doc/index.html /tmp/index.html;
cp -vr doc/codox /tmp/codox
cp -vr logo/logo.png /tmp/cats-logo.png
git checkout gh-pages;

rm -rf index-dev.html codox-dev
mv -fv /tmp/cats-logo.png logo.png
mv -fv /tmp/index.html ./index-dev.html
mv -fv /tmp/codox ./codox-dev

git add --all index-dev.html
git add --all codox-dev
git add --all logo.png

git commit -a -m "Update dev doc"
