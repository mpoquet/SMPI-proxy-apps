#! /bin/sh

export SIMGRID_PATH=/builds/simgrid_install

set -exu
mkdir -p bin
emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "main.org")'
chmod +x bin/*.sh
./bin/MDTest.sh
