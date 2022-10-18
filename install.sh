#!/bin/bash

if [ $# -lt 3 ];
then
    echo "Usage: $0 HOME DOXYGEN-PATH INSTALL-PATH"
    exit
fi

# root=$(pwd)

# pushd $1 > /dev/null
# ln -s $root/.emacs .emacs
# ln -s $root/.latex.el .latex.el
# ln -s $root/.auto-jump.el .auto-jump.el
# ln -s $root/.handout.el .handout.el
# popd > /dev/null

pushd $2 > /dev/null

tar zxf doxymacs-1.8.0.tar.gz
cd doxymacs-1.8.0

DOXYMACS=http://ftp.netbsd.org/pub/pkgsrc/current/pkgsrc/devel/doxymacs

wget ${DOXYMACS}/patches/patch-c_Makefile.in
wget ${DOXYMACS}/patches/patch-c_doxymacs__parser.c

patch -p0 < patch-c_Makefile.in
patch -p0 < patch-c_doxymacs__parser.c

mkdir build

BUILDPATH=$(pwd)/build

./configure --prefix=${BUILDPATH}
make all install

cp build/bin/doxymacs_parser ~/.local/bin/
cp build/share/emacs/site-lisp/* ~/$3/

popd > /dev/null
