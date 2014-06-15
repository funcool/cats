#!/bin/sh
lein doc
(cd doc; make)

if [ $1 == "devel" ]; then
    echo "Building devel docs"
    sleep 3
    cp -vr doc/index.html /tmp/index.html;
    git checkout gh-pages;
    mv -fv /tmp/index.html index-dev.html
    git add --all index-dev.html
    git commit -a -m "Update devel doc"
else
    echo "Build stable docs"
    sleep 3
    cp -vr doc/index.html /tmp/index.html;
    cp -vr doc/static /tmp/static
    cp -vr doc/api /tmp/api
    git checkout gh-pages;

    rm -rf *
    mv -fv /tmp/static .
    mv -fv /tmp/index.html .
    mv -fv /tmp/api .

    git add --all index.html
    git add --all api
    git add --all static

    git commit -a -m "Update doc"
fi
