#!/bin/bash

if [ $# -lt 1 ];
then
    echo "Usage: $0 HOME"
    exit
fi

root=$(pwd)

pushd $1 > /dev/null
ln -s $root/.emacs .emacs
ln -s $root/.latex.el .latex.el
ln -s $root/.auto-jump.el .auto-jump.el
ln -s $root/.handout.el .handout.el
popd > /dev/null
