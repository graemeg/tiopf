// CVS-REV:libxslt/libxslt/security.h:1.1:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * security.h: interface for the XSLT security framework
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  (**
   * xsltSecurityPref:
   *
   * structure to indicate the preferences for security in the XSLT
   * transformation.
   *)
  xsltSecurityPrefs = record end;
  xsltSecurityPrefsPtr = ^xsltSecurityPrefs;

  (**
   * xsltSecurityOption:
   *
   * the set of option that can be configured
   *)
  xsltSecurityOption = (
    DUMMY__XSLT_SECPREF_READ_0,
    XSLT_SECPREF_READ_FILE,   // = 1,
    XSLT_SECPREF_WRITE_FILE,
    XSLT_SECPREF_CREATE_DIRECTORY,
    XSLT_SECPREF_READ_NETWORK,
    XSLT_SECPREF_WRITE_NETWORK
  );

  (**
   * xsltSecurityCheck:
   *
   * User provided function to check the value of a string like a file
   * path or an URL ...
   *)
  xsltSecurityCheck = function (sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; value: Pchar): Longint; cdecl;

(*
 * Module interfaces
 *)
function  xsltNewSecurityPrefs: xsltSecurityPrefsPtr; cdecl; external LIBXSLT_SO;
procedure xsltFreeSecurityPrefs(sec: xsltSecurityPrefsPtr); cdecl; external LIBXSLT_SO;
function  xsltSetSecurityPrefs(sec: xsltSecurityPrefsPtr; option: xsltSecurityOption; func: xsltSecurityCheck): Longint; cdecl; external LIBXSLT_SO;
function  xsltGetSecurityPrefs(sec: xsltSecurityPrefsPtr; option: xsltSecurityOption): xsltSecurityCheck; cdecl; external LIBXSLT_SO;
procedure xsltSetDefaultSecurityPrefs(sec: xsltSecurityPrefsPtr); cdecl; external LIBXSLT_SO;
function  xsltGetDefaultSecurityPrefs: xsltSecurityPrefsPtr; cdecl; external LIBXSLT_SO;
function  xsltSetCtxtSecurityPrefs(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltSecurityAllow(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; value: Pchar): Longint; cdecl; external LIBXSLT_SO;
function  xsltSecurityForbid(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; value: Pchar): Longint; cdecl; external LIBXSLT_SO;

(*
 * internal interfaces
 *)
function  xsltCheckWrite(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; URL: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
function  xsltCheckRead(sec: xsltSecurityPrefsPtr; ctxt: xsltTransformContextPtr; URL: PxmlChar): Longint; cdecl; external LIBXSLT_SO;

