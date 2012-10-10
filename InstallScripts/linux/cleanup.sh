#!/bin/sh

BASEDIR='/home/graemeg/devel/tiOPF2'

rm -f $BASEDIR/dailybuilds/tiopf/3rdParty/FBLib/lib/i386-linux/*
rm -f $BASEDIR/dailybuilds/tiopf/3rdParty/FBLib/lib/x86_64-linux/*
rm -f $BASEDIR/dailybuilds/tiopf/Compilers/FPC/lib/i386-linux/*
rm -f $BASEDIR/dailybuilds/tiopf/Compilers/FPC/lib/x86_64-linux/*
rm -f $BASEDIR/dailybuilds/tiopf/UnitTests/Text/_Dcu/*
rm -f $BASEDIR/dailybuilds/tiopf/UnitTests/Text/results.xml
rm -f $BASEDIR/dailybuilds/tiopf/UnitTests/Text/textrunner32
rm -f $BASEDIR/dailybuilds/tiopf/UnitTests/Text/textrunner64
rm -f $BASEDIR/dailybuilds/tiopf/UnitTests/Text/textrunner32.xml
rm -f $BASEDIR/dailybuilds/tiopf/UnitTests/Text/textrunner64.xml

