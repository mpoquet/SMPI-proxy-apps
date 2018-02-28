#! /bin/sh

export SIMGRID_PATH=/builds/simgrid_install

set -exu
rm -rf bin
mkdir -p bin
for org in CodeVault.org  Coral.org  Mantevo.org  Trinity-Nersc.org ; do 
  emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "'${org}'")'
done
chmod +x bin/*.sh

cmake .

ctest -T test --output-on-failure --no-compress-output || true
if [ -f Testing/TAG ] ; then
   xsltproc $WORKSPACE/src/ctest2junit.xsl Testing/$( head -n 1 < Testing/TAG )/Test.xml > $WORKSPACE/CTestResults.xml
fi


