#!/bin/sh

BASEDIR='/data/devel/tiopf_dailybuilds'

rm -f $BASEDIR/tiopf/3rdParty/FBLib/lib/i386-freebsd/*
rm -f $BASEDIR/tiopf/Compilers/FPC/lib/i386-freebsd/*
rm -f $BASEDIR/tiopf/3rdParty/FBLib/lib/x86_64-freebsd/*
rm -f $BASEDIR/tiopf/Compilers/FPC/lib/x86_64-freebsd/*
rm -f $BASEDIR/tiopf/Compilers/FPC/DataAccessTimingResults.txt
rm -f $BASEDIR/tiopf/Compilers/FPC/tiOPFUnitTestsConsole
rm -f $BASEDIR/tiopf/Compilers/FPC/tiOPFUnitTestsConsole.xml

