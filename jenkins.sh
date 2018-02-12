#! /bin/sh

export SIMGRID_PATH=/builds/workspace/SimGrid-Multi/build_mode/Debug/node/simgrid-fedora-rawhide-64/build/SimGrid-3.19

set -exu
mkdir -p bin
emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "main.org")'
chmod +x bin/*.sh
./bin/MDTest.sh
