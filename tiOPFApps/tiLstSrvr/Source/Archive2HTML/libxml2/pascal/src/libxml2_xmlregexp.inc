// CVS-REV:gnome-xml/include/libxml/xmlregexp.h:1.4:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * regexp.h: describes the basic API for libxml regular expressions handling
 *
 * See Copyright for the status of this software.
 *
 * Daniel Veillard <veillard@redhat.com>
 *)


type
  (**
   * xmlRegexpPtr:
   *
   * A libxml regular expression, they can actually be far more complex
   * thank the POSIX regex expressions.
   *)
  xmlRegexp = record end;
  xmlRegexpPtr = ^xmlRegexp;

  (**
   * xmlRegExecCtxtPtr:
   *
   * A libxml progressive regular expression evaluation context
   *)
  xmlRegExecCtxt = record end;
  xmlRegExecCtxtPtr = ^xmlRegExecCtxt;

(*
 * The POSIX like API
 *)
function xmlRegexpCompile(regexp: PxmlChar): xmlRegexpPtr; cdecl; external LIBXML2_SO;
procedure xmlRegFreeRegexp(regexp: xmlRegexpPtr); cdecl; external LIBXML2_SO;
function xmlRegexpExec(comp: xmlRegexpPtr; value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
procedure xmlRegexpPrint(output: PLibXml2File; regexp: xmlRegexpPtr); cdecl; external LIBXML2_SO;
function xmlRegexpIsDeterminist(comp: xmlRegexpPtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Callback function when doing a transition in the automata
 *)
type
  xmlRegExecCallbacks = procedure (exec: xmlRegExecCtxtPtr; token: PxmlChar; transdata: Pointer; inputdata: Pointer);cdecl;

(*
 * The progressive API
 *)
function xmlRegNewExecCtxt(comp: xmlRegexpPtr; callback: xmlRegExecCallbacks; data: Pointer): xmlRegExecCtxtPtr; cdecl; external LIBXML2_SO;
procedure xmlRegFreeExecCtxt(exec: xmlRegExecCtxtPtr); cdecl; external LIBXML2_SO;
function xmlRegExecPushString(exec: xmlRegExecCtxtPtr; value: PxmlChar; data: Pointer): Longint; cdecl; external LIBXML2_SO;
