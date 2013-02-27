// CVS-REV:libxslt/libxslt/functions.h:1.13:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * functions.h: interface for the XSLT extra functions
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 * Bjorn Reese <breese@users.sourceforge.net> for number formatting
 *)

(**
 * XSLT_REGISTER_FUNCTION_LOOKUP:
 *
 * Registering macro, not general purpose at all but used in different modules.
 *)
{-macro-todo
#define XSLT_REGISTER_FUNCTION_LOOKUP(ctxt)			\
    xmlXPathRegisterFuncLookup((ctxt)->xpathCtxt,		\
	(xmlXPathFuncLookupFunc) xsltXPathFunctionLookup,	\
	(void *)(ctxt->xpathCtxt));
}

function xsltXPathFunctionLookup(ctxt: xmlXPathContextPtr; name: PxmlChar; ns_uri: PxmlChar): xmlXPathFunction; cdecl; external LIBXSLT_SO;

(*
 * Interfaces for the functions implementations.
 *)
procedure xsltDocumentFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO;
procedure xsltKeyFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO;
procedure xsltUnparsedEntityURIFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO;
procedure xsltFormatNumberFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO;
procedure xsltGenerateIdFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO;
procedure xsltSystemPropertyFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO;
procedure xsltElementAvailableFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO;
procedure xsltFunctionAvailableFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXSLT_SO;

(*
 * And the registration
 *)
procedure xsltRegisterAllFunctions(ctxt: xmlXPathContextPtr); cdecl; external LIBXSLT_SO;

