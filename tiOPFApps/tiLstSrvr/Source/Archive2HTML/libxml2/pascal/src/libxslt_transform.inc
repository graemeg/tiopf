// CVS-REV:libxslt/libxslt/transform.h:1.23:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * transform.h: Interfaces, constants and types related to the XSLT engine
 *            transform part.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

(**
 * XInclude default processing.
 *)
procedure xsltSetXIncludeDefault(xinclude: Longint); cdecl; external LIBXSLT_SO;
function  xsltGetXIncludeDefault: Longint; cdecl; external LIBXSLT_SO;

(**
 * Export context to users.
 *)
function  xsltNewTransformContext(style: xsltStylesheetPtr; doc: xmlDocPtr): xsltTransformContextPtr; cdecl; external LIBXSLT_SO;
procedure xsltFreeTransformContext(ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO;
function  xsltApplyStylesheetUser(style: xsltStylesheetPtr; doc: xmlDocPtr; params: PPchar; output: Pchar; profile: PLibXml2File; userCtxt: xsltTransformContextPtr): xmlDocPtr; cdecl; external LIBXSLT_SO;

(**
 * Private Interfaces.
 *)
procedure xsltApplyStripSpaces(ctxt: xsltTransformContextPtr; node: xmlNodePtr); cdecl; external LIBXSLT_SO;
function  xsltApplyStylesheet(style: xsltStylesheetPtr; doc: xmlDocPtr; params: PPchar): xmlDocPtr; cdecl; external LIBXSLT_SO;
function  xsltProfileStylesheet(style: xsltStylesheetPtr; doc: xmlDocPtr; params: PPchar; output: PLibXml2File): xmlDocPtr; cdecl; external LIBXSLT_SO;
function  xsltRunStylesheet(style: xsltStylesheetPtr; doc: xmlDocPtr; params: PPchar; output: Pchar; SAX: xmlSAXHandlerPtr; IObuf: xmlOutputBufferPtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltRunStylesheetUser(style: xsltStylesheetPtr; doc: xmlDocPtr; params: PPchar; output: Pchar; SAX: xmlSAXHandlerPtr; IObuf: xmlOutputBufferPtr; profile: PLibXml2File; userCtxt: xsltTransformContextPtr): Longint; cdecl; external LIBXSLT_SO;
procedure xsltApplyOneTemplate(ctxt: xsltTransformContextPtr; node: xmlNodePtr; list: xmlNodePtr; templ: xsltTemplatePtr; params: xsltStackElemPtr); cdecl; external LIBXSLT_SO;
procedure xsltDocumentElem(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltSort(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltCopy(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltText(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltElement(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltComment(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltAttribute(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltProcessingInstruction(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltCopyOf(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltValueOf(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltNumber(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltApplyImports(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltCallTemplate(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltApplyTemplates(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltChoose(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltIf(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltForEach(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr; comp: xsltStylePreCompPtr); cdecl; external LIBXSLT_SO;
procedure xsltRegisterAllElement(ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO;

(*
 * Hook for the debugger if activated.
 *)
procedure xslHandleDebugger(cur: xmlNodePtr; node: xmlNodePtr; templ: xsltTemplatePtr; ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO;

