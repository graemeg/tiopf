#!/bin/sh
# Full daily builds script for tiOPF.
# created 2009-04-30
# Created by Graeme Geldenhuys <graemeg@gmail.com>

#export USERNAME=graemeg

TIOPF="/opt/dailybuilds/tiopf"              # tiOPF root directory
SCRIPTS="$TIOPF/InstallScripts/linux"       # linux scripts directory

if [ -f $TIOPF/halt.tests ]; then
  echo "Remove the file 'halt.tests' if you want the tests to continue."
  exit 0
fi  

# clean out old files and recompile
cd $SCRIPTS
./cleanup.sh

# compile tiOPF library
cd $TIOPF/Compilers/FPC
$SCRIPTS/opf_package-32.run

# compile Text Test Runner application
/bin/rm -f $TIOPF/UnitTests/Text/textrunner
$SCRIPTS/textrunner-32.run

# run the tests
#./fpcUnitTIOPFText.exe -a > results.xml
#./fpcUnitTIOPFText.exe -a --file=results.xml
#rm ./results.xml
cd $TIOPF/UnitTests/Text/
./textrunner -a --format=xml

# Do we have test results?
if ! [ -f ./results.xml ]; then
  exit 0
fi

# generate the result in text and html format
cp results.xml /opt/dailybuilds/results/results.xml
cd /opt/dailybuilds/results/
/usr/bin/xsltproc -o index.html $SCRIPTS/fpcunit2.xsl results.xml
/usr/bin/xsltproc -o msg.txt $SCRIPTS/summarypost.xsl results.xml

# inject the SVN revision
REV=`svnversion -n $TIOPF/`
FPCVER=`/opt/fpc/bin/fpc -iV`
FPCCPU=`/opt/fpc/bin/fpc -iTP`
FPCHOST=`/opt/fpc/bin/fpc -iTO`
sed "s/#REV/$REV/g" msg.txt > msg1.txt
sed "s/#FPCVER/$FPCVER/g" msg1.txt > msg2.txt
sed "s/#FPCCPU/$FPCCPU-$FPCHOST/g" msg2.txt > msg3.txt

# post text result to tiopf.dailybuilds newsgroup
/usr/bin/rpost 192.168.0.54 < msg3.txt
# copy html results to web server
scp -q -i /home/graemeg/.ssh/id_dsa_github_mirroring index.html graemeg@opensoft:/var/www/opensoft.homeip.net/tiopf/fpcunit/index.html

