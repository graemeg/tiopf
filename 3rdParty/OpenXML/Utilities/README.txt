OPEN XML UTILITIES LIBRARY README
=================================

For installation instructions see the INSTALL.txt file.


Folders
-------
doc:            Documentation
packages:       Delphi for Win32 (Delphi 4+) and for .NET 
                (Delphi 2005+) packages
sources:        Delphi for Win32 and for .NET sources 
Win32-examples: Sample projects for Delphi for Win32


Compiler Conditionals for .NET development
------------------------------------------

The dkCodecUtilsRTL unit declares two platform specific class 
functions: TEncodingRepository.CreateCodecForSystemEncoding and 
TEncodingRepository.SystemEncodingName.  This functions work 
only on Windows operating systems.  If you are planning to use 
the Utilities Library on other operating systems (for example 
with Mono under Linux or Mac OS) you should compile it without 
this functions.  This can be accomplished by defining an ALL_OS
compiler conditional symbol using one of the following methods:

1. When compiling from within the Delphi IDE, then add the ALL_OS
   symbol to the "Conditional defines" box on the "Directories/
   Conditionals" page of the "Project Options" dialog.
2. When using the command-line compiler you can define the ALL_OS
   symbol by using the -D switch.
3. A more radical solution is to add the command {$DEFINE ALL_OS} 
   to the beginning of the dkCodecUtilsRTL unit.
