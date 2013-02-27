// CVS-REV:gnome-xml/include/libxml/xmlreader.h:1.9:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * xmlreader.h : Interfaces, constants and types of the XML streaming API.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  xmlParserProperties = (
    DUMMY__XML_PARSER_LOADDTD_0,
    XML_PARSER_LOADDTD,       // = 1
    XML_PARSER_DEFAULTATTRS,  // = 2
    XML_PARSER_VALIDATE,      // = 3
    XML_PARSER_SUBST_ENTITIES // = 4
  );

  xmlParserSeverities = (
    DUMMY__XML_PARSER_SEVERITY_0,
    XML_PARSER_SEVERITY_VALIDITY_WARNING, // 1
    XML_PARSER_SEVERITY_VALIDITY_ERROR,   // 2
    XML_PARSER_SEVERITY_WARNING,          // 3
    XML_PARSER_SEVERITY_ERROR             // 4
  );

  xmlTextReader = record end;
  xmlTextReaderPtr = ^xmlTextReader;

(*
 * Constructors & Destructor
 *)
function xmlNewTextReader(input: xmlParserInputBufferPtr; URI: PChar): xmlTextReaderPtr; cdecl; external LIBXML2_SO;
function xmlNewTextReaderFilename(URI: Pchar): xmlTextReaderPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeTextReader(reader: xmlTextReaderPtr); cdecl; external LIBXML2_SO;

(*
 * Iterators
 *)
function xmlTextReaderRead(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderReadInnerXml(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderReadOuterXml(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderReadString(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderReadAttributeValue(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Attributes of the node
 *)
function xmlTextReaderAttributeCount(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderBaseUri(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderDepth(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderHasAttributes(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderHasValue(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderIsDefault(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderIsEmptyElement(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderLocalName(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderName(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderNamespaceUri(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderNodeType(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderPrefix(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderQuoteChar(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderValue(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderXmlLang(reader: xmlTextReaderPtr): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderReadState(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Methods of the XmlTextReader
 *)
function xmlTextReaderClose(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderGetAttributeNo(reader: xmlTextReaderPtr; no: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderGetAttribute(reader: xmlTextReaderPtr; name: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderGetAttributeNs(reader: xmlTextReaderPtr; localName: PxmlChar; namespaceURI: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderGetRemainder(reader: xmlTextReaderPtr): xmlParserInputBufferPtr; cdecl; external LIBXML2_SO;
function xmlTextReaderLookupNamespace(reader: xmlTextReaderPtr; prefix: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function xmlTextReaderMoveToAttributeNo(reader: xmlTextReaderPtr; no: Longint): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderMoveToAttribute(reader: xmlTextReaderPtr; name: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderMoveToAttributeNs(reader: xmlTextReaderPtr; localName: PxmlChar; namespaceURI: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderMoveToFirstAttribute(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderMoveToNextAttribute(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderMoveToElement(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderNormalization(reader: xmlTextReaderPtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Extensions
 *)
function xmlTextReaderSetParserProp(reader: xmlTextReaderPtr; prop: Longint; value: Longint): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderGetParserProp(reader: xmlTextReaderPtr; prop: Longint): Longint; cdecl; external LIBXML2_SO;
function xmlTextReaderCurrentNode(reader: xmlTextReaderPtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function xmlTextReaderCurrentDoc(reader: xmlTextReaderPtr): xmlDocPtr; cdecl; external LIBXML2_SO;

(*
 * Error handling extensions
 *)
type
  xmlTextReaderLocatorPtr = Pointer;
  xmlTextReaderErrorFunc = procedure(arg: Pointer; msg: Pchar; severity: xmlParserSeverities; locator: xmlTextReaderLocatorPtr); cdecl;

function xmlTextReaderLocatorLineNumber(locator: xmlTextReaderLocatorPtr): Longint; cdecl; external LIBXML2_SO;
{ int xmlTextReaderLocatorLinePosition(xmlTextReaderLocatorPtr locator); }
function xmlTextReaderLocatorBaseURI(locator: xmlTextReaderLocatorPtr): PxmlChar; cdecl; external LIBXML2_SO;
procedure xmlTextReaderSetErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; var arg: Pointer); cdecl; external LIBXML2_SO;
procedure xmlTextReaderGetErrorHandler(reader: xmlTextReaderPtr; f: xmlTextReaderErrorFunc; var arg: Pointer); cdecl; external LIBXML2_SO;

