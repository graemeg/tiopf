// CVS-REV:libxslt/libxslt/namespaces.h:1.5:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * namespaces.h: interface for the XSLT namespace handling
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

procedure xsltNamespaceAlias(style: xsltStylesheetPtr; node: xmlNodePtr); cdecl; external LIBXSLT_SO;
function  xsltGetNamespace(ctxt: xsltTransformContextPtr; cur: xmlNodePtr; ns: xmlNsPtr; outnode: xmlNodePtr): xmlNsPtr; cdecl; external LIBXSLT_SO;
function  xsltGetSpecialNamespace(ctxt: xsltTransformContextPtr; cur: xmlNodePtr; URI: PxmlChar; prefix: PxmlChar; outnode: xmlNodePtr): xmlNsPtr; cdecl; external LIBXSLT_SO;
function  xsltCopyNamespace(ctxt: xsltTransformContextPtr; node: xmlNodePtr; cur: xmlNsPtr): xmlNsPtr; cdecl; external LIBXSLT_SO;
function  xsltCopyNamespaceList(ctxt: xsltTransformContextPtr; node: xmlNodePtr; cur: xmlNsPtr): xmlNsPtr; cdecl; external LIBXSLT_SO;
procedure xsltFreeNamespaceAliasHashes(style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;

