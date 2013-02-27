// CVS-REV:libxslt/libxslt/attributes.h:1.4:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * attributes.h: interface for the XSLT attribute handling
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

procedure xsltParseStylesheetAttributeSet(style: xsltStylesheetPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO;
procedure xsltFreeAttributeSetsHashes(style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;
procedure xsltApplyAttributeSet(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; attributes: PxmlChar); cdecl; external LIBXSLT_SO;
procedure xsltResolveStylesheetAttributeSet(style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;

