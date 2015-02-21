#!/bin/sh
VERSION="devel"

lein doc
(cd doc; make)

rm -rf /tmp/cats-doc/
mkdir -p /tmp/cats-doc/
mv doc/index.html /tmp/cats-doc/
mv doc/codox /tmp/cats-doc/codox
cp logo/logo.png /tmp/cats-doc/logo.png

git checkout gh-pages;

rm -rf ./$VERSION
mv /tmp/cats-doc/ ./$VERSION

git add --all ./$VERSION
git commit -a -m "Update ${VERSION} doc"
