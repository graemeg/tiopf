// CVS-REV:libxslt/libxslt/imports.h:1.7:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * imports.h: interface for the XSLT import support
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

(**
 * XSLT_GET_IMPORT_PTR:
 *
 * A macro to import Pointers from the stylesheet cascading order.
 *)
(*{-macro-todo
#define XSLT_GET_IMPORT_PTR(res, style, name) {			\
    xsltStylesheetPtr st = style;				\
    res = NULL;							\
    while (st != NULL) {					\
	if (st->name != NULL) { res = st->name; break; }	\
	st = xsltNextImport(st);				\
    }}
}*)
(**
 * XSLT_GET_IMPORT_INT:
 *
 * A macro to import intergers from the stylesheet cascading order.
 *)
(*{-macro-todo
#define XSLT_GET_IMPORT_INT(res, style, name) {			\
    xsltStylesheetPtr st = style;				\
    res = -1;							\
    while (st != NULL) {					\
	if (st->name != -1) { res = st->name; break; }	\
	st = xsltNextImport(st);				\
    }}
}*)
(*
 * Module interfaces
 *)
function  xsltParseStylesheetImport(style: xsltStylesheetPtr; cur: xmlNodePtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltParseStylesheetInclude(style: xsltStylesheetPtr; cur: xmlNodePtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltNextImport(style: xsltStylesheetPtr): xsltStylesheetPtr; cdecl; external LIBXSLT_SO;
function  xsltNeedElemSpaceHandling(ctxt: xsltTransformContextPtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltFindElemSpaceHandling(ctxt: xsltTransformContextPtr; node: xmlNodePtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltFindTemplate(ctxt: xsltTransformContextPtr; name: PxmlChar; nameURI: PxmlChar): xsltTemplatePtr; cdecl; external LIBXSLT_SO;

