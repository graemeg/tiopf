// CVS-REV:gnome-xml/include/libxml/xmlwin32version.h:1.40:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

(*
 * xmlwin32version.h : compile-time version informations for the XML parser
 *                     when compiled on the Windows platform
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

(*
 * use those to be sure nothing nasty will happen if
 * your library and includes mismatch
 *)
{$IFNDEF LIBXML2_COMPILING_MSCCDEF}

procedure xmlCheckVersion(version: Longint); cdecl; external LIBXML2_SO;
{$ENDIF}
{ LIBXML2_COMPILING_MSCCDEF  }
const
  (**
   * LIBXML_DOTTED_VERSION:
   *
   * the version string like "1.2.3"
   *)
  LIBXML_DOTTED_VERSION = '2.5.2';

  (**
   * LIBXML_VERSION:
   *
   * the version number: 1.2.3 value is 1002003
   *)
  LIBXML_VERSION = 20502;

  (**
   * LIBXML_VERSION_STRING:
   *
   * the version number string, 1.2.3 value is "1002003"
   *)
  LIBXML_VERSION_STRING = '20502';

  (**
   * LIBXML_TEST_VERSION:
   *
   * Macro to check that the libxml version in use is compatible with
   * the version the software has been compiled against
   *)

//TODO: #define LIBXML_TEST_VERSION xmlCheckVersion(20502);

{$IFDEF ALWAYS_FALSE}

(**
 * WITH_TRIO:
 *
 * defined if the trio support need to be configured in
 *)
{$DEFINE WITH_TRIO}
{$ELSE}

(**
 * WITHOUT_TRIO:
 *
 * defined if the trio support should not be configured in
 *)
{$DEFINE WITHOUT_TRIO}
{$ENDIF}

(**
 * LIBXML_THREAD_ENABLED:
 *
 * Whether the thread support is configured in
 *)
{$IFDEF ALWAYS_FALSE}
{$DEFINE LIBXML_THREAD_ENABLED}
{$ENDIF}

(**
 * LIBXML_FTP_ENABLED:
 *
 * Whether the FTP support is configured in
 *)
{$IFDEF ALWAYS_TRUE}
{$DEFINE LIBXML_FTP_ENABLED}
{$ENDIF}

(**
 * LIBXML_HTTP_ENABLED:
 *
 * Whether the HTTP support is configured in
 *)
{$IFDEF ALWAYS_TRUE}
{$DEFINE LIBXML_HTTP_ENABLED}
{$ENDIF}

(**
 * LIBXML_HTML_ENABLED:
 *
 * Whether the HTML support is configured in
 *)
{$IFDEF ALWAYS_TRUE}
{$DEFINE LIBXML_HTML_ENABLED}
{$ENDIF}

(**
 * LIBXML_CATALOG_ENABLED:
 *
 * Whether the Catalog support is configured in
 *)
{$IFDEF ALWAYS_TRUE}
{$DEFINE LIBXML_CATALOG_ENABLED}
{$ENDIF}

(**
 * LIBXML_DOCB_ENABLED:
 *
 * Whether the SGML Docbook support is configured in
 *)
{$IFDEF ALWAYS_TRUE}
{$DEFINE LIBXML_DOCB_ENABLED}
{$ENDIF}

(**
 * LIBXML_XPATH_ENABLED:
 *
 * Whether XPath is configured in
 *)
{$IFDEF ALWAYS_TRUE}
{$DEFINE LIBXML_XPATH_ENABLED}
{$ENDIF}

(**
 * LIBXML_XPTR_ENABLED:
 *
 * Whether XPointer is configured in
 *)
{$IFDEF ALWAYS_TRUE}
{$DEFINE LIBXML_XPTR_ENABLED}
{$ENDIF}

(**
 * LIBXML_C14N_ENABLED:
 *
 * Whether the Canonicalization support is configured in
 *)
{$IFDEF ALWAYS_FALSE}
{$DEFINE LIBXML_C14N_ENABLED}
{$ENDIF}

(**
 * LIBXML_XINCLUDE_ENABLED:
 *
 * Whether XInclude is configured in
 *)
{$IFDEF ALWAYS_TRUE}
{$DEFINE LIBXML_XINCLUDE_ENABLED}
{$ENDIF}

(**
 * LIBXML_ICONV_ENABLED:
 *
 * Whether iconv support is available
 *)
{$IFDEF ALWAYS_FALSE}
{$DEFINE LIBXML_ICONV_ENABLED}
{$ENDIF}

(**
 * LIBXML_DEBUG_ENABLED:
 *
 * Whether Debugging module is configured in
 *)
{$IFDEF ALWAYS_FALSE}
{$DEFINE LIBXML_DEBUG_ENABLED}
{$ENDIF}

(**
 * DEBUG_MEMORY_LOCATION:
 *
 * Whether the memory debugging is configured in
 *)
{$IFDEF ALWAYS_FALSE}
{$DEFINE DEBUG_MEMORY_LOCATION}
{$ENDIF}

(**
 * LIBXML_DLL_IMPORT:
 *
 * Used on Windows (MS C compiler only) to declare a variable as
 * imported from the library. This macro should be empty when compiling
 * libxml itself. It should expand to __declspec(dllimport)
 * when the client code includes this header, and that only if the client
 * links dynamically against libxml.
 * For this to work, we need three macros. One tells us which compiler is
 * being used and luckily the compiler defines such a thing: _MSC_VER. The
 * second macro tells us if we are compiling libxml or the client code and
 * we define the macro IN_LIBXML on the compiler's command line for this
 * purpose. The third macro, LIBXML_STATIC, must be defined by any client
 * code which links against libxml statically.
 *)
{$IFNDEF LIBXML_DLL_IMPORT}
{$ENDIF}

(**
 * ATTRIBUTE_UNUSED:
 *
 * Macro used to signal to GCC unused function parameters
 *)
{$IFDEF __GNUC__}
{$IFDEF HAVE_ANSIDECL_H}
{$INCLUDE <ansidecl.h>}
{$ENDIF}
{$IFNDEF ATTRIBUTE_UNUSED}
{$DEFINE ATTRIBUTE_UNUSED}
{$ENDIF}
{$ELSE}
{$DEFINE ATTRIBUTE_UNUSED}
{$ENDIF}

(**
 * #pragma comment(lib, "iconv.lib")
 *
 * pragma understood my MS compiler which enables a conditional link with
 * iconv.
 *)
(*
#ifdef _MSC_VER
#if defined LIBXML_ICONV_ENABLED && !defined LIBXML2_COMPILING_MSCCDEF
#pragma comment(lib, "iconv.lib")
#endif
#endif
*)

