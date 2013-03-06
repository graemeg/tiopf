#!/bin/sh
# Full daily builds script for tiOPF.
# created 2009-04-30
# Created by Graeme Geldenhuys <graemeg@gmail.com>

#export USERNAME=graemeg
BASEDIR="/data/devel"
TIOPF="$BASEDIR/tiopf_dailybuilds/tiopf"              # tiOPF root directory
SCRIPTS="$TIOPF/InstallScripts/freebsd"       # scripts directory
FPC="$BASEDIR/fpc-2.6.0/x86_64-freebsd/bin/ppcx64"
#REV=`svnversion -n $TIOPF/`
REV=`git log --pretty="%h" --abbrev-commit -1`
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
$SCRIPTS/opf_package-64.run

# compile Console Test Runner application
/bin/rm -f $TIOPF/Compilers/FPC/tiOPFUnitTestsConsole
$SCRIPTS/textrunner_dunit2-64.run


# Restore the Firebird database to make sure we have a clean/empty one every time
#cp $BASEDIR/data/tiopf/dunit2.fdb /home/graemeg/devel/data/tiopf/sqldb_ib.fdb
#cp $BASEDIR/data/tiopf/dunit2.fdb /home/graemeg/devel/data/tiopf/fblib.fdb
#cp $BASEDIR/data/tiopf/dunit2.fdb /home/graemeg/devel/data/tiopf/zeos_fb.fdb

#/usr/bin/gbak -REP -USER SYSDBA -PASSWORD masterkey /opt/data/tiopf/dunit2.fbk /opt/data/tiopf/sqldb_ib_dunit2.fdb
#/bin/chown graemeg:firebird /opt/data/tiopf/sqldb_ib_dunit2.fdb
#/usr/bin/gbak -REP -USER SYSDBA -PASSWORD masterkey /opt/data/tiopf/dunit2.fbk /opt/data/tiopf/fblib_dunit2.fdb
#/bin/chown graemeg:firebird /opt/data/tiopf/fblib_dunit2.fdb

# run the tests
#./fpcUnitTIOPFText.exe -a > results.xml
#./fpcUnitTIOPFText.exe -a --file=results.xml
#rm ./results.xml
cd $TIOPF/Compilers/FPC
./tiOPFUnitTestsConsole -xml

# Do we have test results?
if ! [ -f ./tiOPFUnitTestsConsole.xml ]; then
  exit 0
fi

# generate the result in text and html format
cp tiOPFUnitTestsConsole.xml $BASEDIR/tiopf_dailybuilds/results/results64.xml
cd $BASEDIR/tiopf_dailybuilds/results/
/usr/local/bin/xsltproc -o index.html $SCRIPTS/fpcunit2.xsl results64.xml
/usr/local/bin/xsltproc -o msg.txt $SCRIPTS/summarypost-64.xsl results64.xml

# inject the SVN revision
sed "s/#REV/$REV/g" msg.txt > msg1.txt
sed "s/#FPCVER/$FPCVER/g" msg1.txt > msg2.txt
sed "s/#FPCCPU/$FPCCPU-$FPCHOST/g" msg2.txt > msg3.txt
cat msg3.txt divider.txt /tmp/DUnitReportShort${FPCVER}.txt > msg4.txt

# post text result to tiopf.dailybuilds newsgroup
/usr/local/bin/rpost opensoft.homeip.net < msg4.txt
# copy html results to web server
scp -q -i /home/graemeg/.ssh/id_rsa index.html graemeg@192.168.0.5:/usr/local/www/opensoft.homeip.net/tiopf/unittests/freebsd64.html
