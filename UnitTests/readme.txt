
 Automated Testing
 -----------------

The code in this directory requires DUnit2, which is available from
SourceForge (for Delphi), or from GitHub (for Free Pascal). At this
time the DUnit2 for Free Pascal only supports the Text Test Runner,
the GUI Runner has not been ported yet.

Included in this directory is a sample DUnitTIOPF.ini file as well.
This file allows you to control at runtime what persistence layers
you want to test, some expected answers based on your machine's setup
etc.

This INI file must be place in your profile's application data directory.
eg: under Linux that would be:   ~/.config/dunit2_fpc_textrunner/


DUnit2 for Delphi:
   http://sourceforge.net/projects/dunit2/
  
DUnit2 for Free Pascal:
   http://github.com/graemeg/dunit2/


   ----- END -----
