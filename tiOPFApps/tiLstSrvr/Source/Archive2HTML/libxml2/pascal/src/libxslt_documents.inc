// CVS-REV:libxslt/libxslt/documents.h:1.4:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

(*
 * documents.h: interface for the document handling
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

function  xsltNewDocument(ctxt: xsltTransformContextPtr; doc:xmlDocPtr): xsltDocumentPtr; cdecl; external LIBXSLT_SO;
function  xsltLoadDocument(ctxt: xsltTransformContextPtr; URI:PxmlChar): xsltDocumentPtr; cdecl; external LIBXSLT_SO;
function  xsltFindDocument(ctxt: xsltTransformContextPtr; doc:xmlDocPtr): xsltDocumentPtr; cdecl; external LIBXSLT_SO;
procedure xsltFreeDocuments(ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO;
function  xsltLoadStyleDocument(style: xsltStylesheetPtr; URI: PxmlChar): xsltDocumentPtr; cdecl; external LIBXSLT_SO;
function  xsltNewStyleDocument(style: xsltStylesheetPtr; doc: xmlDocPtr): xsltDocumentPtr; cdecl; external LIBXSLT_SO;
procedure xsltFreeStyleDocuments(style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;

