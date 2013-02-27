// CVS-REV:libxslt/libxslt/variables.h:1.16:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(**
 * XSLT_REGISTER_VARIABLE_LOOKUP:
 *
 * Registering macro, not general purpose at all but used in different modules.
 *)

{-macro-todo
#define XSLT_REGISTER_VARIABLE_LOOKUP(ctxt)			\
    xmlXPathRegisterVariableLookup((ctxt)->xpathCtxt,		\
	       xsltXPathVariableLookup,	(void *)(ctxt));	\
    xsltRegisterAllFunctions((ctxt)->xpathCtxt);		\
    xsltRegisterAllElement(ctxt);				\
    (ctxt)->xpathCtxt->extra = ctxt
}
(*
 * Interfaces for the variable module.
 *)
function  xsltEvalGlobalVariables(ctxt: xsltTransformContextPtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltEvalUserParams(ctxt: xsltTransformContextPtr; params: PPchar): Longint; cdecl; external LIBXSLT_SO;
function  xsltQuoteUserParams(ctxt: xsltTransformContextPtr; params: PPchar): Longint; cdecl; external LIBXSLT_SO;
function  xsltEvalOneUserParam(ctxt: xsltTransformContextPtr; name: PxmlChar; value: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
function  xsltQuoteOneUserParam(ctxt: xsltTransformContextPtr; name: PxmlChar; value: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
procedure xsltParseGlobalVariable(style: xsltStylesheetPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO;
procedure xsltParseGlobalParam(style: xsltStylesheetPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO;
procedure xsltParseStylesheetVariable(ctxt: xsltTransformContextPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO;
procedure xsltParseStylesheetParam(ctxt: xsltTransformContextPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO;
function  xsltParseStylesheetCallerParam(ctxt: xsltTransformContextPtr; cur: xmlNodePtr): xsltStackElemPtr; cdecl; external LIBXSLT_SO;
function  xsltAddStackElemList(ctxt: xsltTransformContextPtr; elems: xsltStackElemPtr): Longint; cdecl; external LIBXSLT_SO;
procedure xsltFreeGlobalVariables(ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO;
function  xsltVariableLookup(ctxt: xsltTransformContextPtr; name: PxmlChar; ns_uri: PxmlChar): xmlXPathObjectPtr; cdecl; external LIBXSLT_SO;
function  xsltXPathVariableLookup(ctxt: Pointer; name: PxmlChar; ns_uri: PxmlChar): xmlXPathObjectPtr; cdecl; external LIBXSLT_SO;

