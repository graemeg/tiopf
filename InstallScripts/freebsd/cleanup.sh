#!/bin/sh

BASEDIR='/data/devel/tiopf_dailybuilds'

rm -f $BASEDIR/tiopf/3rdParty/FBLib/lib/i386-freebsd/*
rm -f $BASEDIR/tiopf/Compilers/FPC/lib/i386-freebsd/*
rm -f $BASEDIR/tiopf/3rdParty/FBLib/lib/x86_64-freebsd/*
rm -f $BASEDIR/tiopf/Compilers/FPC/lib/x86_64-freebsd/*
rm -f $BASEDIR/tiopf/UnitTests/Text/_Dcu/*
rm -f $BASEDIR/tiopf/UnitTests/Text/results.xml
rm -f $BASEDIR/tiopf/UnitTests/Text/textrunner32
rm -f $BASEDIR/tiopf/UnitTests/Text/textrunner64
rm -f $BASEDIR/tiopf/UnitTests/Text/textrunner32.xml
rm -f $BASEDIR/tiopf/UnitTests/Text/textrunner64.xml

