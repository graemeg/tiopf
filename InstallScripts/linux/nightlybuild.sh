#!/bin/sh

if [ -f /home/graemeg/programming/tiOPF2/Source/halt.tests ]; then
  echo "Remove the file 'halt.tests' if you want the tests to continue."
  exit 0
fi  

# clean out old files and recompile
cd /home/graemeg/programming/tiOPF2/Source/
./cleanup.sh
#cd lib/i386-linux
#/bin/rm -f *.o *.ppu *.rst *.s *.a *.so *.ppl
#cd ../..
cd Compilers/FPC
./opf_package.run
cd ../..
cd UnitTests/Text
#cd _Dcu
#/bin/rm -f *.o *.ppu *.rst *.s *.a *.so *.ppl
#cd ..
#./fpcUnitTIOPFText.run
/bin/rm -f *.exe
./testrunner.run

# run the tests
#./fpcUnitTIOPFText.exe -a > results.xml
#./fpcUnitTIOPFText.exe -a --file=results.xml
./testrunner.exe -a

# Do we have test results?
if ! [ -f ./results.xml ]; then
  exit 0
fi

# generate the result in text and html format
cp results.xml /var/www/html/tiopf/fpcunit/results.xml
cd /var/www/html/tiopf/fpcunit
/usr/bin/xsltproc -o index.html fpcunit2.xsl results.xml
/usr/bin/xsltproc -o msg.txt summarypost.xsl results.xml

# inject the SVN revision
REV=`svnversion -n /home/graemeg/programming/tiOPF2/Source`
sed "s/####/$REV/g" msg.txt > msg2.txt

# post text result to tiopf.dailybuilds newsgroup
/usr/bin/rpost localhost < msg2.txt
