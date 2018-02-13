#! /bin/sh

export SIMGRID_PATH=/builds/simgrid_install

set -exu
mkdir -p bin
emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "main.org")'
chmod +x bin/*.sh

cmake .

ctest -T test --output-on-failure --no-compress-output || true
if [ -f Testing/TAG ] ; then
   xsltproc $WORKSPACE/src/ctest2junit.xsl Testing/$( head -n 1 < Testing/TAG )/Test.xml > $WORKSPACE/CTestResults.xml
fi


