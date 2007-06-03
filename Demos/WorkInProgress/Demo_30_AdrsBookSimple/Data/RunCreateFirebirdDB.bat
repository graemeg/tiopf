@echo off

set PathToISQL="C:\Program Files\Firebird\Firebird_1_5\bin\isql.exe"
set ScriptName=CreateFirebirdDB.sql

erase AdrsBookSimple.fdb
%PathToISQL% -i %ScriptName%

pause