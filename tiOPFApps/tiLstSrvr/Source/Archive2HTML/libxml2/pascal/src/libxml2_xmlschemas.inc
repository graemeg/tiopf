// CVS-REV:gnome-xml/include/libxml/xmlschemas.h:1.5:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * schemas.h: interface to the XML Schemas handling and schema validity
 *             checking
 *
 * See Copyright for the status of this software.
 *
 * Daniel.Veillard@w3.org
 *)
type
  xmlSchemaValidError = (
    XML_SCHEMAS_ERR_OK,              // = 0,
    XML_SCHEMAS_ERR_NOROOT,          // = 1,
    XML_SCHEMAS_ERR_UNDECLAREDELEM,
    XML_SCHEMAS_ERR_NOTTOPLEVEL,
    XML_SCHEMAS_ERR_MISSING,
    XML_SCHEMAS_ERR_WRONGELEM,
    XML_SCHEMAS_ERR_NOTYPE,
    XML_SCHEMAS_ERR_NOROLLBACK,
    XML_SCHEMAS_ERR_ISABSTRACT,
    XML_SCHEMAS_ERR_NOTEMPTY,
    XML_SCHEMAS_ERR_ELEMCONT,
    XML_SCHEMAS_ERR_HAVEDEFAULT,
    XML_SCHEMAS_ERR_NOTNILLABLE,
    XML_SCHEMAS_ERR_EXTRACONTENT,
    XML_SCHEMAS_ERR_INVALIDATTR,
    XML_SCHEMAS_ERR_INVALIDELEM,
    XML_SCHEMAS_ERR_NOTDETERMINIST,
    XML_SCHEMAS_ERR_CONSTRUCT,
    XML_SCHEMAS_ERR_INTERNAL,
    XML_SCHEMAS_ERR_NOTSIMPLE,
    XML_SCHEMAS_ERR_ATTRUNKNOWN,
    XML_SCHEMAS_ERR_ATTRINVALID,
    XML_SCHEMAS_ERR_,
    XML_SCHEMAS_ERR_XXX
  );
type
  (*
   *  The schemas related types are kept internal
   *)
  xmlSchema = record end;
  xmlSchemaPtr = ^xmlSchema;

  (*
   *  A schemas validation context
   *)
  xmlSchemaValidityErrorFunc = procedure (ctx: Pointer; msg: Pchar); cdecl; {$IFDEF HAS_TVA} varargs; {$ENDIF}
  xmlSchemaValidityWarningFunc = procedure (ctx: Pointer; msg: Pchar); cdecl; {$IFDEF HAS_TVA} varargs; {$ENDIF}

  xmlSchemaParserCtxt = record end;
  xmlSchemaParserCtxtPtr = ^xmlSchemaParserCtxt;

  xmlSchemaValidCtxt = record end;
  xmlSchemaValidCtxtPtr = ^xmlSchemaValidCtxt;

(*
 * Interfaces for parsing.
 *)
function  xmlSchemaNewParserCtxt(URL: PChar): xmlSchemaParserCtxtPtr; cdecl; external LIBXML2_SO;
function  xmlSchemaNewMemParserCtxt(buffer: PChar; size: Longint): xmlSchemaParserCtxtPtr; cdecl; external LIBXML2_SO;
procedure xmlSchemaFreeParserCtxt(ctxt: xmlSchemaParserCtxtPtr); cdecl; external LIBXML2_SO;
procedure xmlSchemaSetParserErrors(ctxt: xmlSchemaParserCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: Pointer); cdecl; external LIBXML2_SO;
function  xmlSchemaParse(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr; cdecl; external LIBXML2_SO;
procedure xmlSchemaFree(schema: xmlSchemaPtr); cdecl; external LIBXML2_SO;
procedure xmlSchemaDump(output: PLibXml2File; schema: xmlSchemaPtr); cdecl; external LIBXML2_SO;

(*
 * Interfaces for validating
 *)
procedure xmlSchemaSetValidErrors(ctxt: xmlSchemaValidCtxtPtr; err: xmlSchemaValidityErrorFunc; warn: xmlSchemaValidityWarningFunc; ctx: Pointer); cdecl; external LIBXML2_SO;
function  xmlSchemaNewValidCtxt(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr; cdecl; external LIBXML2_SO;
procedure xmlSchemaFreeValidCtxt(ctxt: xmlSchemaValidCtxtPtr); cdecl; external LIBXML2_SO;
function  xmlSchemaValidateDoc(ctxt: xmlSchemaValidCtxtPtr; instance: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlSchemaValidateStream(ctxt: xmlSchemaValidCtxtPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding; sax: xmlSAXHandlerPtr; user_data: Pointer): Longint; cdecl; external LIBXML2_SO;

