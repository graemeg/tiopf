The XML Listener code in DUnit2 can use either XDOM or ADOM. Delphi 2010 and
later include both. Earlier versions of Delphi include XDOM only. There are
issues using XDOM in Delphi 2009 and later so ADOM is chosen in these versions.
See DUnit.inc for the code that determines which is used.

This directory contains the source code of both the core Open XML ADOM and
the Open XML Utility Library, both required to use ADOM. The Delphi 2010
version with "dk" prefix is used to avoid a unit name clash with the
pre-installed ADOM source in Delphi 2010.

To use ADOM with XML Listener in your DUnit projects add the \externals\adom
directory to your projects search path.

The original source can be found here:

http://www.philo.de/xml/

Versions Used:
- Alternative Document Object Model v.4.3.3 (current stable version as at
  02-Feb-2011)
- Utility Library v.3.0.1 (released 2010-05-11) (current version as at
  02-Feb-2011)
