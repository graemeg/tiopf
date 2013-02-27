// CVS-REV:gnome-xml/include/libxml/relaxng.h:1.1:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * relaxng.h: header for the implementation of the Relax-NG validation
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)
type
  xmlRelaxNG = record end;
  xmlRelaxNGPtr = ^xmlRelaxNG;

  (*
   * A schemas validation context
   *)
  xmlRelaxNGValidityErrorFunc = procedure (ctx: Pointer; msg: Pchar); cdecl;  {$IFDEF HAS_TVA} varargs; {$ENDIF}
  xmlRelaxNGValidityWarningFunc = procedure (ctx: Pointer; msg: Pchar); cdecl;  {$IFDEF HAS_TVA} varargs; {$ENDIF}

  xmlRelaxNGParserCtxt = record end;
  xmlRelaxNGParserCtxtPtr = ^xmlRelaxNGParserCtxt;
  xmlRelaxNGValidCtxt = record end;
  xmlRelaxNGValidCtxtPtr = ^xmlRelaxNGValidCtxt;

(*
 * Interfaces for parsing.
 *)
function xmlRelaxNGNewParserCtxt(URL: Pchar): xmlRelaxNGParserCtxtPtr; cdecl; external LIBXML2_SO;
function xmlRelaxNGNewMemParserCtxt(buffer: Pchar; size: Longint): xmlRelaxNGParserCtxtPtr; cdecl; external LIBXML2_SO;
procedure xmlRelaxNGFreeParserCtxt(ctxt: xmlRelaxNGParserCtxtPtr); cdecl; external LIBXML2_SO;
procedure xmlRelaxNGSetParserErrors(ctxt: xmlRelaxNGParserCtxtPtr; err: xmlRelaxNGValidityErrorFunc; warn: xmlRelaxNGValidityWarningFunc; ctx: Pointer); cdecl; external LIBXML2_SO;
function xmlRelaxNGParse(ctxt: xmlRelaxNGParserCtxtPtr): xmlRelaxNGPtr; cdecl; external LIBXML2_SO;
procedure xmlRelaxNGFree(schema: xmlRelaxNGPtr); cdecl; external LIBXML2_SO;
procedure xmlRelaxNGDump(output: PLibXml2File; schema: xmlRelaxNGPtr); cdecl; external LIBXML2_SO;
(*
 * Interfaces for validating
 *)
procedure xmlRelaxNGSetValidErrors(ctxt: xmlRelaxNGValidCtxtPtr; err: xmlRelaxNGValidityErrorFunc; warn: xmlRelaxNGValidityWarningFunc; ctx: Pointer); cdecl; external LIBXML2_SO;
function xmlRelaxNGNewValidCtxt(schema: xmlRelaxNGPtr): xmlRelaxNGValidCtxtPtr; cdecl; external LIBXML2_SO;
procedure xmlRelaxNGFreeValidCtxt(ctxt: xmlRelaxNGValidCtxtPtr); cdecl; external LIBXML2_SO;
function xmlRelaxNGValidateDoc(ctxt: xmlRelaxNGValidCtxtPtr; instance: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
function xmlRelaxNGValidateStream(ctxt: xmlRelaxNGValidCtxtPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding; sax: xmlSAXHandlerPtr; user_data: Pointer): Longint; cdecl; external LIBXML2_SO;
procedure xmlRelaxNGCleanupTypes; cdecl; external LIBXML2_SO;

