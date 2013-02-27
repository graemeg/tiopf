// CVS-REV:libxslt/libxslt/xslt.h:1.15:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * xslt.h: Interfaces, constants and types related to the XSLT engine
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

const
  (**
   * XSLT_DEFAULT_VERSION:
   *
   * The default version of XSLT supported.
   *)
  XSLT_DEFAULT_VERSION = '1.0';

  (**
   * XSLT_DEFAULT_VENDOR:
   *
   * The XSLT "vendor" string for this processor.
   *)
  XSLT_DEFAULT_VENDOR = 'libxslt';

  (**
   * XSLT_DEFAULT_URL:
   *
   * The XSLT "vendor" URL for this processor.
   *)
  XSLT_DEFAULT_URL = 'http://xmlsoft.org/XSLT/';

  (**
   * XSLT_NAMESPACE:
   *
   * The XSLT specification namespace.
   *)
  XSLT_NAMESPACE = 'http://www.w3.org/1999/XSL/Transform';

  (**
   * LIBXSLT_PUBLIC:
   *
   * Macro used on Windows to tag public identifiers from shared libraries.
   *)
{-macro-
  #if !defined LIBXSLT_PUBLIC
  #if (defined (__CYGWIN__) || defined _MSC_VER) && !defined IN_LIBXSLT && !defined LIBXSLT_STATIC
  #define LIBXSLT_PUBLIC __declspec(dllimport)
  #else
  #define LIBXSLT_PUBLIC
  #endif
  #endif
}
  (**
   * xsltMaxDepth:
   *
   * This value is used to detect templates loops.
   *)
//  LIBXSLT_PUBLIC extern int xsltMaxDepth;

  (**
   * xsltEngineVersion:
   *
   * The version string for libxslt.
   *)
//  LIBXSLT_PUBLIC extern const char *xsltEngineVersion;

  (**
   * xsltLibxsltVersion:
   *
   * The version of libxslt compiled.
   *)
//  LIBXSLT_PUBLIC extern const int xsltLibxsltVersion;

  (**
   * xsltLibxmlVersion:
   *
   * The version of libxml libxslt was compiled against.
   *)
//  LIBXSLT_PUBLIC extern const int xsltLibxmlVersion;

(*
 * Global cleanup function.
 *)
procedure xsltCleanupGlobals; cdecl; external LIBXSLT_SO;

