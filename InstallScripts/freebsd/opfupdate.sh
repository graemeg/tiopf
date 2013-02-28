#!/bin/sh

BASEDIR="/data/devel"
TIOPF="$BASEDIR/tiopf_dailybuilds/tiopf"
GIT="/usr/local/bin/git"

cd $TIOPF
$GIT stash
$GIT pull
$GIT stash pop



