// CVS-REV:libxslt/libxslt/xsltutils.h:1.35:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * xsltutils.h: interfaces for the utilities module of the XSLT engine.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

(**
 * XSLT_TODO:
 *
 * Macro to flag unimplemented blocks.
 *)
{-macro-
#define XSLT_TODO 							\
    xsltGenericError(xsltGenericErrorContext,				\
      "Unimplemented block at %s: %d\n",				\
            __FILE__, __LINE__);
}
(**
 * XSLT_STRANGE:
 *
 * Macro to flag that a problem was detected internally.
 *)
{-macro-
#define XSLT_STRANGE 							\
    xsltGenericError(xsltGenericErrorContext,				\
      "Internal error at %s: %d\n",				\
            __FILE__, __LINE__);
}
(**
 * IS_XSLT_ELEM:
 *
 * Checks that the element pertains to XSLT namespace.
 *)
{-macro-todo
#define IS_XSLT_ELEM(n)							\
    (((n) != NULL) && ((n)->ns != NULL) &&				\
     (xmlStrEqual((n)->ns->href, XSLT_NAMESPACE)))
}
(**
 * IS_XSLT_NAME:
 *
 * Checks the value of an element in XSLT namespace.
 *)
{-macro-todo
#define IS_XSLT_NAME(n, val)						\
    (xmlStrEqual((n)->name, (const xmlChar *) (val)))
}
(**
 * IS_XSLT_REAL_NODE:
 *
 * Check that a node is a 'real' one: document, element, text or attribute.
 *)
{$IFDEF LIBXML_DOCB_ENABLED}
{-macro-todo
#define IS_XSLT_REAL_NODE(n)						\
    (((n) != NULL) &&							\
     (((n)->type == XML_ELEMENT_NODE) ||				\
      ((n)->type == XML_TEXT_NODE) ||					\
      ((n)->type == XML_ATTRIBUTE_NODE) ||				\
      ((n)->type == XML_DOCUMENT_NODE) ||				\
      ((n)->type == XML_HTML_DOCUMENT_NODE) ||				\
      ((n)->type == XML_DOCB_DOCUMENT_NODE)))
}
{$ELSE}
{-macro-todo
#define IS_XSLT_REAL_NODE(n)						\
    (((n) != NULL) &&							\
     (((n)->type == XML_ELEMENT_NODE) ||				\
      ((n)->type == XML_TEXT_NODE) ||					\
      ((n)->type == XML_ATTRIBUTE_NODE) ||				\
      ((n)->type == XML_DOCUMENT_NODE) ||				\
      ((n)->type == XML_HTML_DOCUMENT_NODE)))
}
{$ENDIF}
(*
 * Our own version of namespaced atributes lookup.
 *)
function xsltGetNsProp(node: xmlNodePtr; const name, namespace: PxmlChar): PxmlChar; cdecl; external LIBXSLT_SO;
function xsltGetUTF8Char(utf: PChar; len: PLongint): PxmlChar; cdecl; external LIBXSLT_SO;

(*
 * XSLT specific error and debug reporting functions.
 *)
{todo
LIBXSLT_PUBLIC extern xmlGenericErrorFunc xsltGenericError;
LIBXSLT_PUBLIC extern void *xsltGenericErrorContext;
LIBXSLT_PUBLIC extern xmlGenericErrorFunc xsltGenericDebug;
LIBXSLT_PUBLIC extern void *xsltGenericDebugContext;
}
procedure xsltPrintErrorContext(ctxt: xsltTransformContextPtr; style: xsltStylesheetPtr; node: xmlNodePtr); cdecl; external LIBXSLT_SO;
procedure xsltMessage(ctxt: xsltTransformContextPtr; node: xmlNodePtr; inst: xmlNodePtr); cdecl; external LIBXSLT_SO;
procedure xsltSetGenericErrorFunc(ctx: Pointer; handler: xmlGenericErrorFunc); cdecl; external LIBXSLT_SO;
procedure xsltSetGenericDebugFunc(ctx: Pointer; handler: xmlGenericErrorFunc); cdecl; external LIBXSLT_SO;
procedure xsltSetTransformErrorFunc(ctxt: xsltTransformContextPtr; ctx: Pointer; handler: xmlGenericErrorFunc); cdecl; external LIBXSLT_SO;
procedure xsltTransformError(ctxt: xsltTransformContextPtr; style: xsltStylesheetPtr; node: xmlNodePtr; msg: PChar); cdecl; {$IFDEF HAS_VA} varargs; {$ENDIF} external LIBXML2_SO;

(*
 * Sorting.
 *)
procedure xsltDocumentSortFunction(list: xmlNodeSetPtr); cdecl; external LIBXSLT_SO;
//procedure xsltDoSortFunction(ctxt: xsltTransformContextPtr; sorts: PxmlNodePtr; nbsorts: Longint); cdecl; external LIBXSLT_SO;

procedure xsltSetSortFunc(handler: xsltSortFunc); cdecl; external LIBXSLT_SO;
procedure xsltSetCtxtSortFunc(ctxt: xsltTransformContextPtr; handler: xsltSortFunc); cdecl; external LIBXSLT_SO;
procedure xsltDefaultSortFunction(ctxt: xsltTransformContextPtr; sorts: PxmlNodePtr; nbsorts: Longint); cdecl; external LIBXSLT_SO;
procedure xsltDoSortFunction(ctxt: xsltTransformContextPtr; sorts: PxmlNodePtr; nbsorts: Longint); cdecl; external LIBXSLT_SO;
function  xsltComputeSortResult(ctxt: xsltTransformContextPtr; sort: xmlNodePtr): PxmlXPathObjectPtr; cdecl; external LIBXSLT_SO;

(*
 * QNames handling.
 *)
function xsltGetQNameURI(node: xmlNodePtr; name: PPxmlChar): PxmlChar; cdecl; external LIBXSLT_SO;

(*
 * Output, reuse libxml I/O buffers.
 *)
function  xsltSaveResultTo(buf: xmlOutputBufferPtr; result: xmlDocPtr; style: xsltStylesheetPtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltSaveResultToFilename(URI: Pchar; result: xmlDocPtr; style: xsltStylesheetPtr; compression: Longint): Longint; cdecl; external LIBXSLT_SO;
function  xsltSaveResultToFile(var file_: TextFile; result: xmlDocPtr; style: xsltStylesheetPtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltSaveResultToFd(fd: Longint; result: xmlDocPtr; style: xsltStylesheetPtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltSaveResultToString(doc_txt_ptr: PPxmlChar; doc_txt_len: PLongint; result: xmlDocPtr; style: xsltStylesheetPtr): Longint; cdecl; external LIBXSLT_SO;

(*
 * Profiling.
 *)
procedure xsltSaveProfiling(ctxt: xsltTransformContextPtr; output: PLibXml2File); cdecl; external LIBXSLT_SO;
function  xsltGetProfileInformation(ctxt: xsltTransformContextPtr): xmlDocPtr; cdecl; external LIBXSLT_SO;

function  xsltTimestamp: Longint; cdecl; external LIBXSLT_SO;
procedure xsltCalibrateAdjust(delta: Longint); cdecl; external LIBXSLT_SO;

const
  (**
   * XSLT_TIMESTAMP_TICS_PER_SEC:
   *
   * Sampling precision for profiling
   *)
  XSLT_TIMESTAMP_TICS_PER_SEC = 100000;

type
  (*
   * Hooks for the debugger.
   *)
  xsltDebugStatusCodes = Longint;
const
  XSLT_DEBUG_NONE         = 0; (* no debugging allowed *)
  XSLT_DEBUG_INIT         = 1;
  XSLT_DEBUG_STEP         = 2;
  XSLT_DEBUG_STEPOUT      = 3;
  XSLT_DEBUG_NEXT         = 4;
  XSLT_DEBUG_STOP         = 5;
  XSLT_DEBUG_CONT         = 6;
  XSLT_DEBUG_RUN          = 7;
  XSLT_DEBUG_RUN_RESTART  = 8;
  XSLT_DEBUG_QUIT         = 9;

//[pk] LIBXSLT_PUBLIC extern int xslDebugStatus;
type
  xsltHandleDebuggerCallback = procedure(cur, node: xmlNodePtr; templ: xsltTemplatePtr; ctxt: xsltTransformContextPtr); cdecl;
  xsltAddCallCallback = function(templ: xsltTemplatePtr; source: xmlNodePtr): Longint; cdecl;
  xsltDropCallCallback = procedure;

function  xsltSetDebuggerCallbacks(no: Longint; block: Pointer): Longint; cdecl; external LIBXSLT_SO;
function  xslAddCall(templ: xsltTemplatePtr; source: xmlNodePtr): Longint; cdecl; external LIBXSLT_SO;
procedure xslDropCall; cdecl; external LIBXSLT_SO;

