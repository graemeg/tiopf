#!/bin/sh
# Full daily builds script for tiOPF.
# created 2009-04-30
# Created by Graeme Geldenhuys <graemeg@gmail.com>

#export USERNAME=graemeg

TIOPF="/opt/dailybuilds/tiopf"              # tiOPF root directory
SCRIPTS="$TIOPF/InstallScripts/linux"       # linux scripts directory
FPC="/opt/fpc-2.4.3/x86_64-linux/bin/ppcx64"

if [ -f $TIOPF/halt.tests ]; then
  echo "Remove the file 'halt.tests' if you want the tests to continue."
  exit 0
fi  

# clean out old files and recompile
cd $SCRIPTS
./cleanup.sh
rm /tmp/DUnitReportShort2.4.3.txt

# compile tiOPF library
cd $TIOPF/Compilers/FPC
$SCRIPTS/opf_package-64.run

# compile Text Test Runner application
/bin/rm -f $TIOPF/UnitTests/Text/textrunner
$SCRIPTS/textrunner_dunit2-64.run
#$SCRIPTS/textrunner-64.run

# Restore the Firebird database to make sure we have a clean/empty one every time
/usr/bin/gbak -r -REPLACE_DATABASE -USER SYSDBA -PASSWORD masterkey /opt/dailybuilds/data/sqldb_ib_dunit2.fbk /opt/dailybuilds/data/sqldb_ib_dunit2.fdb
/usr/bin/gbak -r -REPLACE_DATABASE -USER SYSDBA -PASSWORD masterkey /opt/dailybuilds/data/sqldb_ib_dunit2.fbk /opt/dailybuilds/data/fblib_dunit2.fdb

# run the tests
#./fpcUnitTIOPFText.exe -a > results.xml
#./fpcUnitTIOPFText.exe -a --file=results.xml
#rm ./results.xml
cd $TIOPF/UnitTests/Text/
./textrunner64 -xml

# Do we have test results?
if ! [ -f ./textrunner64.xml ]; then
  exit 0
fi

# generate the result in text and html format
cp textrunner64.xml /opt/dailybuilds/results/results64.xml
cd /opt/dailybuilds/results/
/usr/bin/xsltproc -o index.html $SCRIPTS/fpcunit2.xsl results64.xml
/usr/bin/xsltproc -o msg.txt $SCRIPTS/summarypost.xsl results64.xml

# inject the SVN revision
REV=`svnversion -n $TIOPF/`
FPCVER=`$FPC -iV`
FPCCPU=`$FPC -iTP`
FPCHOST=`$FPC -iTO`
sed "s/#REV/$REV/g" msg.txt > msg1.txt
sed "s/#FPCVER/$FPCVER/g" msg1.txt > msg2.txt
sed "s/#FPCCPU/$FPCCPU-$FPCHOST/g" msg2.txt > msg3.txt
cat msg3.txt divider.txt /tmp/DUnitReportShort2.4.3.txt > msg4.txt

# post text result to tiopf.dailybuilds newsgroup
/usr/bin/rpost 192.168.0.54 < msg4.txt
# copy html results to web server
scp -q -i /home/graemeg/.ssh/id_dsa_github_mirroring index.html graemeg@opensoft:/var/www/opensoft.homeip.net/tiopf/fpcunit/index64.html

