{
    ------------------------------------------------------------------------------
    Translated into pascal with help of h2pas utility from the FreePascal project.
    Copyright for the header translation:
        4commerce technologies AG
        Kamerbalken 10-14
        22525 Hamburg, Germany
    Published under a double license:
    a) the GNU Library General Public License: 
       http://www.gnu.org/copyleft/lgpl.html
    b) the Mozilla Public License:
       http://www.mozilla.org/MPL/MPL-1.1.html
    Please send corrections to: ufechner@4commerce.de
    ------------------------------------------------------------------------------
}
{
     xsltwin32config.h: compile-time version informations for the XSLT engine
                        when compiled on windows
    
     See Copyright for the status of this software.
    
     daniel@veillard.com
    }

  { 
     LIBXSLT_DOTTED_VERSION:
    
     the version string like "1.2.3"
    }

  const
     LIBXSLT_DOTTED_VERSION = '1.0.7';
  { 
     LIBXSLT_VERSION:
    
     the version number: 1.2.3 value is 1002003
    }
     LIBXSLT_VERSION = 10007;
  { 
     LIBXSLT_VERSION_STRING:
    
     the version number string, 1.2.3 value is "1002003"
    }
     LIBXSLT_VERSION_STRING = '10007';
  { 
     WITH_XSLT_DEBUG:
    
     Activate the compilation of the debug reporting. Speed penalty
     is insignifiant and being able to run xsltpoc -v is useful. On
     by default
    }
{$if true}
{$define WITH_XSLT_DEBUG}
{$ifend}
{$if false}
  {
     DEBUG_MEMORY:
    
     should be activated only when debugging libxslt. It replaces the
     allocator with a collect and debug shell to the libc allocator.
     Use configure --with-mem-debug to activate it on both library
    }
{$define DEBUG_MEMORY}
  { 
     DEBUG_MEMORY_LOCATION:
    
     should be activated only when debugging libxslt.
     DEBUG_MEMORY_LOCATION should be activated only when libxml has
     been configured with --with-debug-mem too
    }
{$define DEBUG_MEMORY_LOCATION}
{$ifend}
  { 
     ATTRIBUTE_UNUSED:
    
     This macro is used to flag unused function parameters to GCC, useless here
    }
{$ifndef ATTRIBUTE_UNUSED}
{$define ATTRIBUTE_UNUSED}
{$endif}
  { 
     LIBXSLT_PUBLIC:
    
     This macro is needed on Win32 when using MSVC. It enables the client code
     to access exported variables. It should expand to nothing when compiling
     this library itself, but must expand to __declspec(dllimport) when a
     client includes the library header and that only if it links dynamically
     against this library.
    }
{$if !defined LIBXSLT_PUBLIC}
{$if defined _MSC_VER && !defined IN_LIBXSLT && !defined LIBXSLT_STATIC}
    { was #define dname def_expr }
    function LIBXSLT_PUBLIC : longint;
      { return type might be wrong }

{$else}
{$define LIBXSLT_PUBLIC}
{$endif}
{$endif}
{ C++ end of extern C conditionnal removed }
{$endif}
  { __XML_XSLTWIN32CONFIG_H__  }
    { was #define dname def_expr }
    function LIBXSLT_PUBLIC : longint;
      { return type might be wrong }
      begin
         LIBXSLT_PUBLIC:=__declspec(dllimport);
      end;


