#!/bin/sh
lein doc
(cd doc; make)
cp -vr doc/index.html /tmp/index.html;
cp -vr doc/static /tmp/static
cp -vr doc/codox /tmp/codox
cp -vr logo /tmp/static
git checkout gh-pages;

rm -rf static index-dev.html codox-dev
mv -fv /tmp/static .
mv -fv /tmp/index.html ./index-dev.html
mv -fv /tmp/codox ./codox-dev

git add --all index-dev.html
git add --all static
git add --all codox-dev

git commit -a -m "Update dev doc"
