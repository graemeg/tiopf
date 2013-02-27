Delphi-Headers for libxml2
==========================
libxml2 is a low-level library for the work with xml-documents.
libxslt is a low-level library implementing the XSL Transformations standard
libxml2-pas is translation of their header files into Pascal language.

Contents:
	AUTHORS
	VERSION COMPLIANCE
	WHAT YOU FIND IN THIS PACKAGE
	USAGE
	H2PAS options
	LINKS
	HOW TO HELP

CONDITIONAL DEFINES
-------------------
USE_PASCAL_MM
	activates the compiler's native memory manager, instead of making use of the MM already active in libxml2.
	Use it only if you encounter MM-related problems. In that case, please report the circumstances to our mailing list.

AUTHORS
-------
Petr Kozelka <pkozelka@email.cz>
	-  original header translations

Following people contributed:

Uwe Fechner <ufechner@4commerce.de>
	- testing, demo application(s), initiated the libxslt translation
Martijn Brinkers <martijn_brinkers@yahoo.co.uk>
Mikhail Soukhanov <m.soukhanov@geosys.ru>
Eric Zurchner <Eric.Zurcher@csiro.au>

VERSION COMPLIANCE
------------------
libxml2: look into file 'libxml_xmlwin32version.inc' for value of constant LIBXML_DOTTED_VERSION.
libxslt: look into file 'libxml_xsltconfig.inc' for value of constant LIBXSLT_DOTTED_VERSION
libexslt: look into file 'libxml_exsltconfig.inc' for value of constant LIBEXSLT_DOTTED_VERSION

WHAT YOU FIND IN THIS PACKAGE
-----------------------------
src/			- pascal translations of the libxml2 headers
demos/libxml2/		- demo applications
demos/libxml2/demo1/	- demo application for xpath testing (by Uwe Fechner)


USAGE
-----
Note that you should *NEVER* need to include any of the *.inc files in the translation directory.
To use functions implemented in libxml2, you just put "libxml2" into your _uses_ list:

program MyPrg;
uses libxml2;
begin
  ... your code here ...
end.

The following dlls are necessary and must be placed in this directory or in a 
directory listed in the PATH environment variable:

	iconv.dll
	libxml2.dll
(plus, for libxslt):
	libxslt.dll
	libexslt.dll

On Linux system, you need the corresponding ".so" shared object files.

KNOWN PROBLEMS
--------------
- extending the xslt-processor with own functions doesn't work yet
- no demo-program yet for xslt

H2PAS options
-------------
The utility h2pas was used with the following options:
h2pas -d -e -c -i <filename>.h -o libxml_<filename>.inc


LINKS
-----
http://sourceforge.net/projects/libxml2-pas			- project's web site
mailto:libxml2-pas-devel@lists.sourceforge.net			- mailing list
http://xmlsoft.org/						- the libxml2 libraries web site
http://www.zlatkovic.com/projects/libxml/index.html		- Windows build of libxml2 by Igor Zlatkovic


HOW TO HELP
-----------
You can help the project in various ways: the best thing to do first is to subscribe 
to the abovementioned mailing-list, have a look at the archives; then you can: 
* provide patches when you find problems 
* provide new documentation pieces (translations, examples, etc ...) 
* implement new features 
* help with the packaging 
* test, give feedback, send bugreports... 
