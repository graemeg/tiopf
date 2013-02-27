// CVS-REV:libxslt/libxslt/pattern.h:1.16:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * pattern.h: interface for the pattern matching used in template matches.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

(**
 * xsltCompMatch:
 *
 * Data structure used for the implementation of patterns.
 * It is kept private (in pattern.c).
 *)
type
  xsltCompMatch = record end;
  xsltCompMatchPtr = ^xsltCompMatch;

(*
 * Pattern related interfaces.
 *)
function  xsltCompilePattern(pattern: PxmlChar; doc: xmlDocPtr; node: xmlNodePtr): xsltCompMatchPtr; cdecl; external LIBXSLT_SO;
procedure xsltFreeCompMatchList(comp: xsltCompMatchPtr); cdecl; external LIBXSLT_SO;
function  xsltTestCompMatchList(ctxt: xsltTransformContextPtr; node: xmlNodePtr; comp: xsltCompMatchPtr): Longint; cdecl; external LIBXSLT_SO;

(*
 * Template related interfaces.
 *)
function  xsltAddTemplate(style: xsltStylesheetPtr; cur: xsltTemplatePtr; mode: PxmlChar; modeURI: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
function  xsltGetTemplate(ctxt: xsltTransformContextPtr; node: xmlNodePtr; style: xsltStylesheetPtr): xsltTemplatePtr; cdecl; external LIBXSLT_SO;
procedure xsltFreeTemplateHashes(style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;
procedure xsltCleanupTemplates(style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;

{$IFDEF ALWAYS_FALSE}
function  xsltMatchPattern(ctxt: xsltTransformContextPtr; node: xmlNodePtr; pattern: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
{$ENDIF}

