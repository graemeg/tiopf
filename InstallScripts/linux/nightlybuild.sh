#!/bin/sh

cd /home/graemeg/programming/tiOPF2/Source/Compilers/FPC/
./opf_package.run
cd ../..
cd UnitTests/Text
./fpcUnitTIOPFText.run

# run the tests
./fpcUnitTIOPFText.exe -a > results.xml
#./fixup_xml.sh

cp results.xml /var/www/html/tiopf/fpcunit/results.xml
cd /var/www/html/tiopf/fpcunit
/usr/bin/xsltproc -o index.html fpcunit.xsl results.xml
/usr/bin/xsltproc -o msg.txt summarypost.xsl results.xml
/usr/bin/rpost localhost < msg.txt
