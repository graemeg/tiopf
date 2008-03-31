@echo off

set PathToISQL="C:\Program Files\Firebird\Firebird_2_0\bin\isql.exe"
set ScriptName=CreateFirebirdDB.sql

if exist Adrs.fdb erase Adrs.fdb
%PathToISQL% -i %ScriptName%

pause
