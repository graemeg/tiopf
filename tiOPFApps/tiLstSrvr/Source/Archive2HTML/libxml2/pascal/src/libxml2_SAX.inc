// CVS-REV:gnome-xml/include/libxml/SAX.h:1.16:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * SAX.h : Default SAX handler interfaces.
 *
 * See Copyright for the status of this software.
 *
 * Daniel Veillard <daniel@veillard.com>
 *)

function  getPublicId(ctx: Pointer): PxmlChar; cdecl; external LIBXML2_SO;
function  getSystemId(ctx: Pointer): PxmlChar; cdecl; external LIBXML2_SO;
procedure setDocumentLocator(ctx: Pointer; loc: xmlSAXLocatorPtr); cdecl; external LIBXML2_SO;
function  getLineNumber(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  getColumnNumber(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  isStandalone(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  hasInternalSubset(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  hasExternalSubset(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
procedure internalSubset(ctx: Pointer; name: PxmlChar; ExternalID: PxmlChar; SystemID: PxmlChar); cdecl; external LIBXML2_SO;
procedure externalSubset(ctx: Pointer; name: PxmlChar; ExternalID: PxmlChar; SystemID: PxmlChar); cdecl; external LIBXML2_SO;
function  getEntity(ctx: Pointer; name: PxmlChar): xmlEntityPtr; cdecl; external LIBXML2_SO;
function  getParameterEntity(ctx: Pointer; name: PxmlChar): xmlEntityPtr; cdecl; external LIBXML2_SO;
function  resolveEntity(ctx: Pointer; publicId: PxmlChar; systemId: PxmlChar): xmlParserInputPtr; cdecl; external LIBXML2_SO;
procedure entityDecl(ctx: Pointer; name: PxmlChar; aType: Longint; publicId: PxmlChar; systemId: PxmlChar; content: PxmlChar); cdecl; external LIBXML2_SO;
procedure attributeDecl(ctx: Pointer; elem: PxmlChar; fullname: PxmlChar; aType: Longint; def: Longint; defaultValue: PxmlChar; tree: xmlEnumerationPtr); cdecl; external LIBXML2_SO;
procedure elementDecl(ctx: Pointer; name: PxmlChar; aType: Longint; content: xmlElementContentPtr); cdecl; external LIBXML2_SO;
procedure notationDecl(ctx: Pointer; name: PxmlChar; publicId: PxmlChar; systemId: PxmlChar); cdecl; external LIBXML2_SO;
procedure unparsedEntityDecl(ctx: Pointer; name: PxmlChar; publicId: PxmlChar; systemId: PxmlChar; notationName: PxmlChar); cdecl; external LIBXML2_SO;
procedure startDocument(ctx: Pointer); cdecl; external LIBXML2_SO;
procedure endDocument(ctx: Pointer); cdecl; external LIBXML2_SO;
procedure attribute(ctx: Pointer; fullname: PxmlChar; value: PxmlChar); cdecl; external LIBXML2_SO;
procedure startElement(ctx: Pointer; fullname: PxmlChar; atts: PPxmlChar); cdecl; external LIBXML2_SO;
procedure endElement(ctx: Pointer; name: PxmlChar); cdecl; external LIBXML2_SO;
procedure reference(ctx: Pointer; name: PxmlChar); cdecl; external LIBXML2_SO;
procedure characters(ctx: Pointer; ch: PxmlChar; len: Longint); cdecl; external LIBXML2_SO;
procedure ignorableWhitespace(ctx: Pointer; ch: PxmlChar; len: Longint); cdecl; external LIBXML2_SO;
procedure processingInstruction(ctx: Pointer; target: PxmlChar; data: PxmlChar); cdecl; external LIBXML2_SO;
procedure globalNamespace(ctx: Pointer; href: PxmlChar; prefix: PxmlChar); cdecl; external LIBXML2_SO;
procedure setNamespace(ctx: Pointer; name: PxmlChar); cdecl; external LIBXML2_SO;
function  getNamespace(ctx: Pointer): xmlNsPtr; cdecl; external LIBXML2_SO;
function  checkNamespace(ctx: Pointer; nameSpace: PxmlChar): Longint; cdecl; external LIBXML2_SO;
procedure namespaceDecl(ctx: Pointer; href: PxmlChar; prefix: PxmlChar); cdecl; external LIBXML2_SO;
procedure comment(ctx: Pointer; value: PxmlChar); cdecl; external LIBXML2_SO;
procedure cdataBlock(ctx: Pointer; value: PxmlChar; len: Longint); cdecl; external LIBXML2_SO;
{$IFDEF LIBXML_HTML_ENABLED}
procedure inithtmlDefaultSAXHandler(hdlr: xmlSAXHandlerPtr); cdecl; external LIBXML2_SO;
{$ENDIF}
{$IFDEF LIBXML_DOCB_ENABLED}
procedure initdocbDefaultSAXHandler(hdlr: xmlSAXHandlerPtr); cdecl; external LIBXML2_SO;
{$ENDIF}
procedure xmlDefaultSAXHandlerInit; cdecl; external LIBXML2_SO;
procedure htmlDefaultSAXHandlerInit; cdecl; external LIBXML2_SO;
procedure docbDefaultSAXHandlerInit; cdecl; external LIBXML2_SO;

