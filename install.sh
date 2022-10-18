#!/bin/bash

if [ $# -lt 2 ];
then
    echo "Usage: $0 HOME DOXYGEN-PATH"
    exit
fi

root=$(pwd)

pushd $1 > /dev/null
ln -s $root/.emacs .emacs
ln -s $root/.latex.el .latex.el
ln -s $root/.auto-jump.el .auto-jump.el
ln -s $root/.handout.el .handout.el
popd > /dev/null

# https://www.hiroom2.com/2016/10/31/emacs-doxymacs-package/#sec-1

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

mkdir -p ~/.emacs.d/elpa/doxymacs

cp build/bin/doxymacs_parser ~/.local/bin/
cp build/share/emacs/site-lisp/* ~/.emacs.d/elpa/doxymacs/

echo "Add the following line to .emacs:"
echo "(add-to-list 'load-path \"~/.emacs.d/elpa/doxymacs\")"

popd > /dev/null
