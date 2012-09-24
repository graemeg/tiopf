
 Unit Testing tiOPF
 ==================

The code in this directory requires DUnit2 (for Delphi), which is available
from SourceForge, or FPTest (for Free Pascal), which is available on GitHub.
FPTest is a fork of DUnit2, and is modified to be specific to Free Pascal
and user interface toolkits that work with Free Pascal (eg: fpGUI or LCL).
I changed the name of my fork on GitHub, so as not to confuse Delphi
developers about which testing framework to use.

At the current time (Septemer 2012), both DUnit2 and FPTest are pretty
much equal in features. FPTest is just a version of the testing
framework without VCL or Delphi.NET code, thus specific to what the
Free Pascal Compiler supports.

Included in this directory is also a sample DUnitTIOPF.ini file.
This file allows you to control at runtime what persistence layers
you want to test, some expected answers based on your machine's setup, and
database connection information.

This INI file must be place in your profile's application data directory.
eg: under Linux that would be:   ~/.config/<application_name>/
The location is whatever the following code returns on your system:

  tiRemoveTrailingSlash(tiGetAppDataDirPrivate) + PathDelim + 'DUnitTIOPF.ini'

If you haven't setup this file and you run the unit testing project, some
unit tests will fail and hint at the location where you need to place the
configured DUnitTIOPF.ini file on your system.
  
When you compile the testing framework, make sure you pass to the compiler
the correct Compiler Defines for the persistence layers you would like to
test. The compiler defines are the ones that look like:

   LINK_xxx

eg:
   LINK_IBX
   LINK_SQLDB_IB
   etc.

It's the ones you can see in the <tiopf>/Core/tiOPFManager.pas unit, around
line 243.


Delphi 7
---------
1) tiOPF includes the DUnit2 framework - a slightly older version which
   should still work - in the SVN checkout '3rdParty' directory. Otherwise
   use the latest version from SourceForge (project like is below).
   
2) Open the <tiopf>\Compilers\Delphi7\tiOPFUnitTestsGUI.dpr project.

3) Double check that the Compiler Defines are correctly setup for the
   persistence layers you want to compile in and test. This is done in
   "Project - Options - Directories/Conditionals". Then click on the '...'
   button next to the "Confitional defines" edit combobox.
   
4) Now compile the tiOPFUnitTestsGUI project.

5) You should now be able to run the unit testing project. You will probably
   have some tests that fail due to unknown user/password combinations
   for database servers etc. This is configured in the DUnitTIOPF.ini
   file I mentioned earlier.

   
FPC + Lazarus IDE + fpGUI Toolkit
---------------------------------
1) You need the latest FPTest from GitHub. See the link below. If you
   don't have Git installed, GitHub allows you to download the latest
   code as a ZIP archive.
   
2) Open the <tiopf>/UnitTests/GUI/tiOPFUnitTestsFPGui.lpi project.

3) Make sure you have the LINK_xxx compiler defines setup for the
   persistence frameworks you want to test. You need to set this up
   in two place.
   
   3.1) Open the <tiOPF>/Compilers/FPC/tiOPF.lpk package, then
        click Options - Compiler Options - Other.
        
   3.2) You also need to set the same LINK_xxx compiler defines for
        the tiOPFUnitTestsFPGui project. "Project - Project Options -
        Compiler Options - Other"
        
4) Now compile the tiOPFUnitTestsFPGui project.

5) You should now be able to run the unit testing project. You will probably
   have some tests that fail due to unknown user/password combinations
   for database servers etc. This is configured in the DUnitTIOPF.ini
   file I mentioned earlier.

   
Where to find the latest DUnit2 or FPTest?
------------------------------------------

DUnit2 for Delphi:
   http://sourceforge.net/projects/dunit2/

FPTest for Free Pascal:
   http://github.com/graemeg/fptest/


   
                       ----- END -----

