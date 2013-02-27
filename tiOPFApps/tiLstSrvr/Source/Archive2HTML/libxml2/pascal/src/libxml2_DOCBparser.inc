// CVS-REV:gnome-xml/include/libxml/DOCBparser.h:1.6:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * DOCBparser.h : interface for a DocBook SGML non-verifying parser
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  (*
   * Most of the back-end structures from XML and SGML are shared.
   *)
  docbParserCtxt = xmlParserCtxt;
  docbParserCtxtPtr = xmlParserCtxtPtr;
  docbParserNodeInfo = xmlParserNodeInfo;
  docbSAXHandler = xmlSAXHandler;
  docbSAXHandlerPtr = xmlSAXHandlerPtr;
  docbParserInput = xmlParserInput;
  docbParserInputPtr = xmlParserInputPtr;
  docbDocPtr = xmlDocPtr;
  docbNodePtr = xmlNodePtr;

(*
 * There is only few public functions.
 *)
function  docbEncodeEntities(aOut: PByte; outlen: PLongint; aIn: PByte; inlen: PLongint; quoteChar: Longint): Longint; cdecl; external LIBXML2_SO;
function  docbSAXParseDoc(cur: PxmlChar; encoding: PChar; sax: docbSAXHandlerPtr; userData: Pointer): docbDocPtr; cdecl; external LIBXML2_SO;
function  docbParseDoc(cur: PxmlChar; encoding: PChar): docbDocPtr; cdecl; external LIBXML2_SO;
function  docbSAXParseFile(filename: PChar; encoding: PChar; sax: docbSAXHandlerPtr; userData: Pointer): docbDocPtr; cdecl; external LIBXML2_SO;
function  docbParseFile(filename: PChar; encoding: PChar): docbDocPtr; cdecl; external LIBXML2_SO;

(**
 * Interfaces for the Push mode.
 *)
procedure docbFreeParserCtxt(ctxt: docbParserCtxtPtr); cdecl; external LIBXML2_SO;
function  docbCreatePushParserCtxt(sax: docbSAXHandlerPtr; user_data: Pointer; chunk: PChar; size: Longint; filename: PChar; enc: xmlCharEncoding): docbParserCtxtPtr; cdecl; external LIBXML2_SO;
function  docbParseChunk(ctxt: docbParserCtxtPtr; chunk: PChar; size: Longint; terminate: Longint): Longint; cdecl; external LIBXML2_SO;
function  docbCreateFileParserCtxt(filename: PChar; encoding: PChar): docbParserCtxtPtr; cdecl; external LIBXML2_SO;
function  docbParseDocument(ctxt: docbParserCtxtPtr): Longint; cdecl; external LIBXML2_SO;

