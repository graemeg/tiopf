@echo off

: You may have to change PathToISQL depending on the installed location of
: Firebird on your machine
set PathToISQL="C:\Program Files\Firebird\Firebird_2_0\bin\isql.exe"
set PathToUnitTest=..\UnitTests\_bin
set PathToBin=..\_bin

if exist Adrs.fdb erase Adrs.fdb
%PathToISQL% -i CreateFirebirdDB.sql
%PathToISQL% -i InsertSeedData.sql
copy Adrs.fdb %PathToUnitTest%\Adrs.fdb
copy Adrs.fdb %PathToBin%\Adrs.fdb
copy Adrs.XMLLight %PathToUnitTest%\Adrs.XMLLight
copy Adrs.XMLLight %PathToBin%\Adrs.XMLLight

pause
