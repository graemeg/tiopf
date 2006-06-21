 -- Readme.txt file for tiOPF NexusDb V1.0 Persistence layer --
    10 March 2004 - Steven Mitchell (srmitch_at tpg dot_com dot au)

Disclaimer
----------
This source code is provided 'as is' with no guarantees or
warranties intended or implied. Use at your own risk. The
author(s) accept(s) no liability or responsibility for any loss
or damages arising from the use of this source code or any
of its subsequent revisions or versions.

Setup Instructions 
-------------------
(Assumes default tiOPF folder installation).

1.  Add the following line to the list of constants in
ctiPersist.pas in the ..\Techinsite\tiPersist folder:

cTIPersistNx1 = 'Nx1';

(See the example ctiPersist_Nx1.pas file in this folder.)

2.  Open tiPerFramework_D7.bpg in Delphi 7. Add the
tiPersistNx1.dpk in the ..\TechInsite\tiOPFExtras\tiPersistNx1
folder to the program goup. Adjust the Options|Directories
settings as appropriate for your environment.

3.  Rebuild the tiPersistCore package. This adds the new
cTIPersistNx1 const.

4.  Build the tiPersistNx1 package.

DUnit Testing
-------------
1.  Copy the tiPerFrameworkNx1_TST.pas file in the
..\TechInsite\tiOPFExtras\tiPersistNx1\DUnit_Nx1 folder into
the ..\TechInsite\DUnitTests folder.

2.  Edit the tiDUnitDependencies.pas file in the
..\TechInsite\DUnitTests folder to include testing for the
NexusDb persistence layer. (See the 
Example_tiDUnitDependencies.pas file in the
..\TechInsite\tiOPFExtras\tiPersistNx1\DUnit_Nx1 folder).

3.  Open tiPerFramework_D7.bpg in Delphi 7 and build
DUnitTIOPFGui.exe.

4.  Run DUnitTIOPFGui.exe. If all is well "Nx1" should be
available for testing and all its related tests will pass.

Usage Instructions
------------------
Create an <name>.ini file in the same folder as the executable
and use the "-config <name>.ini" command line option.

There are some settings specific to this NexusDb layer. Please
see the header comments in the accompanying Example_Nx1.ini file.

