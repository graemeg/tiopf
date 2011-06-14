#!/bin/sh
# Full daily builds script for tiOPF.
# created 2010-10-01
# Created by Graeme Geldenhuys <graemeg@gmail.com>

#export USERNAME=graemeg

TIOPF="/opt/dailybuilds/tiopf"              # tiOPF root directory
SCRIPTS="$TIOPF/InstallScripts/linux"       # linux scripts directory
FPC="/opt/fpc-2.4.5/i386-linux/bin/ppc386"
REV=`svnversion -n $TIOPF/`
FPCVER=`$FPC -iV`
FPCCPU=`$FPC -iTP`
FPCHOST=`$FPC -iTO`


if [ -f $TIOPF/halt.tests ]; then
  echo "Remove the file 'halt.tests' if you want the tests to continue."
  exit 0
fi  

# clean out old files and recompile
cd $SCRIPTS
./cleanup.sh
rm /tmp/DUnitReportShort${FPCVER}.txt

# compile tiOPF library
cd $TIOPF/Compilers/FPC
$SCRIPTS/opf_package-32.run

# compile Text Test Runner application
/bin/rm -f $TIOPF/UnitTests/Text/textrunner32
$SCRIPTS/textrunner_dunit2-32.run


# Restore the Firebird database to make sure we have a clean/empty one every time
#/usr/bin/gbak -REP -USER SYSDBA -PASSWORD masterkey /opt/data/tiopf/sqldb_ib_dunit2.fbk /opt/data/tiopf/sqldb_ib_dunit2.fdb
#/usr/bin/gbak -REP -USER SYSDBA -PASSWORD masterkey /opt/data/tiopf/sqldb_ib_dunit2.fbk /opt/data/tiopf/fblib_dunit2.fdb

# run the tests
#./fpcUnitTIOPFText.exe -a > results.xml
#./fpcUnitTIOPFText.exe -a --file=results.xml
#rm ./results.xml
cd $TIOPF/UnitTests/Text/
./textrunner32 -xml

# Do we have test results?
if ! [ -f ./textrunner32.xml ]; then
  exit 0
fi

# generate the result in text and html format
cp textrunner32.xml /opt/dailybuilds/results/results32.xml
cd /opt/dailybuilds/results/
/usr/bin/xsltproc -o index.html $SCRIPTS/fpcunit2.xsl results32.xml
/usr/bin/xsltproc -o msg.txt $SCRIPTS/summarypost-32.xsl results32.xml

# inject the SVN revision
sed "s/#REV/$REV/g" msg.txt > msg1.txt
sed "s/#FPCVER/$FPCVER/g" msg1.txt > msg2.txt
sed "s/#FPCCPU/$FPCCPU-$FPCHOST/g" msg2.txt > msg3.txt
cat msg3.txt divider.txt /tmp/DUnitReportShort${FPCVER}.txt > msg4.txt

# post text result to tiopf.dailybuilds newsgroup
/usr/bin/rpost 192.168.0.54 < msg4.txt
# copy html results to web server
scp -q -i /home/graemeg/.ssh/id_dsa_github_mirroring index.html graemeg@opensoft:/var/www/opensoft.homeip.net/tiopf/fpcunit/index32.html

