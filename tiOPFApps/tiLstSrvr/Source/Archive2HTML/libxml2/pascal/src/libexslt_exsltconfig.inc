// CVS-REV:libxslt/libexslt/exsltconfig.h.in:1.4:
{
  ------------------------------------------------------------------------------
  Translated into pascal with some help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * exsltconfig.h: compile-time version informations for the EXSLT library
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

const

  (**
   * LIBEXSLT_DOTTED_VERSION:
   *
   * the version string like "1.2.3"
   *)
  LIBEXSLT_DOTTED_VERSION = '0.7.15';

  (**
   * LIBEXSLT_VERSION:
   *
   * the version number: 1.2.3 value is 1002003
   *)
  LIBEXSLT_VERSION = 00715;

  (**
   * LIBEXSLT_VERSION_STRING:
   *
   * the version number string, 1.2.3 value is "1002003"
   *)
  LIBEXSLT_VERSION_STRING = '00715';

(**
 * ATTRIBUTE_UNUSED:
 *
 * This macro is used to flag unused function parameters to GCC
 *)
{
#ifdef __GNUC__
#ifdef HAVE_ANSIDECL_H
#include <ansidecl.h>
#endif
#ifndef ATTRIBUTE_UNUSED
#define ATTRIBUTE_UNUSED
#endif
#else
#define ATTRIBUTE_UNUSED
#endif
}

(**
 * LIBEXSLT_PUBLIC:
 *
 * This macro is needed on Win32 when using MSVC. It enables the client code
 * to access exported variables. It should expand to nothing when compiling
 * this library itself, but must expand to __declspec(dllimport) when a
 * client includes the library header and that only if it links dynamically
 * against this library.
 *)
{
#if !defined LIBEXSLT_PUBLIC
#if (defined (__CYGWIN__) || defined _MSC_VER) && !defined IN_LIBEXSLT && !defined LIBEXSLT_STATIC
#define LIBEXSLT_PUBLIC __declspec(dllimport)
#else
#define LIBEXSLT_PUBLIC
#endif
#endif
}

