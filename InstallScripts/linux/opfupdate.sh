#!/bin/sh

BASEDIR="/data/devel"
TIOPF="$BASEDIR/tiopf_dailybuilds/tiopf"
FBLIB="$BASEDIR/tiopf_dailybuilds/fblib"
GIT="/usr/bin/git"

cd $TIOPF
$GIT stash
$GIT pull
$GIT stash pop

cd $FBLIB
$GIT pull
