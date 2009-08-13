#!/bin/sh
for f in `find . \( ! -regex '.*/\..*' \) -type f -name '*.pas'`; do
  svn ps svn:eol-style native $f
  svn ps svn:mime-type text/plain $f
done
