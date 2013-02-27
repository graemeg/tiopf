// CVS-REV:gnome-xml/include/libxml/parser.h:1.82:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * parser.h : Interfaces, constants and types related to the XML parser.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

const
  (**
   * XML_DEFAULT_VERSION:
   *
   * The default version of XML used: 1.0
   *)
  XML_DEFAULT_VERSION = '1.0';

type
  (**
   * xmlParserInputDeallocate:
   * @str:  the string to deallocate
   *
   * Callback for freeing some parser input allocations.
   *)
  xmlParserInputDeallocate = procedure(p1: PxmlChar); cdecl;

  (**
   * xmlParserInput:
   *
   * An xmlParserInput is an input flow for the XML processor.
   * Each entity parsed is associated an xmlParserInput (except the
   * few predefined ones). This is the case both for internal entities
   * - in which case the flow is already completely in memory - or
   * external entities - in which case we use the buf structure for
   * progressive reading and I18N conversions to the internal UTF-8 format.
   *)
  xmlParserInputPtr = ^xmlParserInput;
  xmlParserInput = record
    (* Input buffer *)
    buf: xmlParserInputBufferPtr;   (* UTF-8 encoded buffer  *)
    filename: PChar;                (* The file analyzed, if any  *)
    directory: PChar;               (* the directory/base of the file  *)
    base: PxmlChar;                 (* Base of the array to parse  *)
    cur: PxmlChar;                  (* Current char being parsed  *)
    _end: PxmlChar;                 (* end of the array to parse  *)
    length: Longint;                (* length if known  *)
    line: Longint;                  (* Current line  *)
    col: Longint;                   (* Current column  *)
    consumed: Longint;              (* How many xmlChars already consumed  *)
    free: xmlParserInputDeallocate; (* function to deallocate the base  *)
    encoding: PxmlChar;             (* the encoding string for entity  *)
    version: PxmlChar;              (* the version string for entity  *)
    standalone: Longint;            (* Was that entity marked standalone  *)
  end;

  (**
   * xmlParserInputState:
   *
   * The parser is now working also as a state based parser.
   * The recursive one use the state info for entities processing.
   *)
type
  xmlParserInputState = Longint;
const
  XML_PARSER_EOF             = -1; (* nothing is to be parsed *)
  XML_PARSER_START           =  0; (* nothing has been parsed *)
  XML_PARSER_MISC            =  1; (* Misc  before int subset *)
  XML_PARSER_PI              =  2; (* Within a processing instruction *)
  XML_PARSER_DTD             =  3; (* within some DTD content *)
  XML_PARSER_PROLOG          =  4; (* Misc  after internal subset *)
  XML_PARSER_COMMENT         =  5; (* within a comment *)
  XML_PARSER_START_TAG       =  6; (* within a start tag *)
  XML_PARSER_CONTENT         =  7; (* within the content *)
  XML_PARSER_CDATA_SECTION   =  8; (* within a CDATA section *)
  XML_PARSER_END_TAG         =  9; (* within a closing tag *)
  XML_PARSER_ENTITY_DECL     = 10; (* within an entity declaration *)
  XML_PARSER_ENTITY_VALUE    = 11; (* within an entity value in a decl *)
  XML_PARSER_ATTRIBUTE_VALUE = 12; (* within an attribute value *)
  XML_PARSER_SYSTEM_LITERAL  = 13; (* within a SYSTEM value *)
  XML_PARSER_EPILOG          = 14; (* the Misc  after the last end tag *)
  XML_PARSER_IGNORE          = 15; (* within an IGNORED section *)
  XML_PARSER_PUBLIC_LITERAL  = 16; (* within a PUBLIC value *)

  (**
   * XML_DETECT_IDS:
   *
   * Bit in the loadsubset context field to tell to do ID/REFs lookups.
   * Use it to initialize xmlLoadExtDtdDefaultValue.
   *)
  XML_DETECT_IDS = 2;

  (**
   * XML_COMPLETE_ATTRS:
   *
   * Bit in the loadsubset context field to tell to do complete the
   * elements attributes lists with the ones defaulted from the DTDs.
   * Use it to initialize xmlLoadExtDtdDefaultValue.
   *)
  XML_COMPLETE_ATTRS = 4;

type
  xmlSAXHandlerPtr = ^xmlSAXHandler;
  (**
   * xmlParserNodeInfo:
   *
   * The parser can be asked to collect Node informations, i.e. at what
   * place in the file they were detected.
   * NOTE: This is off by default and not very well tested.
   *)
  xmlParserNodeInfoPtr = ^xmlParserNodeInfo;
  xmlParserNodeInfo = record
    node: xmlNodePtr;
    { Position & line # that text that created the node begins & ends on  }
    begin_pos: DWORD;
    begin_line: DWORD;
    end_pos: DWORD;
    end_line: DWORD;
  end;

  xmlParserNodeInfoSeqPtr = ^xmlParserNodeInfoSeq;
  xmlParserNodeInfoSeq = record
    maximum: DWORD;
    length: DWORD;
    buffer: ^xmlParserNodeInfo;
  end;

  (**
   * xmlParserCtxt:
   *
   * The parser context.
   * NOTE This doesn't completely define the parser state, the (current ?)
   *      design of the parser uses recursive function calls since this allow
   *      and easy mapping from the production rules of the specification
   *      to the actual code. The drawback is that the actual function call
   *      also reflect the parser state. However most of the parsing routines
   *      takes as the only argument the parser context Pointer, so migrating
   *      to a state based parser for progressive parsing shouldn't be too hard.
   *)
  xmlParserCtxtPtr = ^xmlParserCtxt;
  xmlParserCtxt = record
    sax: xmlSAXHandlerPtr;                (* The SAX handler  *)
    userData: Pointer;                    (* For SAX interface only, used by DOM build  *)
    myDoc: xmlDocPtr;                     (* the document being built  *)
    wellFormed: Longint;                  (* is the document well formed  *)
    replaceEntities: Longint;             (* shall we replace entities ?  *)
    version: PxmlChar;                    (* the XML version string  *)
    encoding: PxmlChar;                   (* the declared encoding, if any  *)
    standalone: Longint;                  (* standalone document  *)
    html: Longint;                        (* an HTML(1)/Docbook(2) document  *)
    (* Input stream stack *)
    input: xmlParserInputPtr;             (* Current input stream  *)
    inputNr: Longint;                     (* Number of current input streams  *)
    inputMax: Longint;                    (* Max number of input streams  *)
    (* stack of inputs *)
    inputTab: ^xmlParserInputPtr;         (* Node analysis stack only used for DOM building  *)
    node: xmlNodePtr;                     (* Current parsed Node  *)
    nodeNr: Longint;                      (* Depth of the parsing stack  *)
    nodeMax: Longint;                     (* Max depth of the parsing stack  *)
    nodeTab: ^xmlNodePtr;                 (* array of nodes  *)
    record_info: Longint;                 (* Whether node info should be kept  *)
    node_seq: xmlParserNodeInfoSeq;       (* info about each node parsed  *)
    errNo: Longint;                       (* error code  *)
    hasExternalSubset: Longint;           (* reference and external subset  *)
    hasPErefs: Longint;                   (* the internal subset has PE refs  *)
    external_: Longint;                   (* are we parsing an external entity  *)
    valid: Longint;                       (* is the document valid  *)
    validate: Longint;                    (* shall we try to validate ?  *)
    vctxt: xmlValidCtxt;                  (* The validity context  *)
    instate: xmlParserInputState;         (* current type of input  *)
    token: Longint;                       (* next char look-ahead  *)
    directory: PChar;                     (* the data directory  *)
    (* Node name stack *)
    name: PxmlChar;                       (* Current parsed Node  *)
    nameNr: Longint;                      (* Depth of the parsing stack  *)
    nameMax: Longint;                     (* Max depth of the parsing stack  *)
    nameTab: ^PxmlChar;                   (* array of nodes  *)
    nbChars: Longint;                     (* number of xmlChar processed  *)
    checkIndex: Longint;                  (* used by progressive parsing lookup  *)
    keepBlanks: Longint;                  (* ugly but ...  *)
    disableSAX: Longint;                  (* SAX callbacks are disabled  *)
    inSubset: Longint;                    (* Parsing is in int 1/ext 2 subset  *)
    intSubName: PxmlChar;                 (* name of subset  *)
    extSubURI: PxmlChar;                  (* URI of external subset  *)
    extSubSystem: PxmlChar;               (* SYSTEM ID of external subset  *)
    (* xml:space values *)
    space: PLongint;                      (* Should the parser preserve spaces  *)
    spaceNr: Longint;                     (* Depth of the parsing stack  *)
    spaceMax: Longint;                    (* Max depth of the parsing stack  *)
    spaceTab: PLongint;                   (* array of space infos  *)
    depth: Longint;                       (* to prevent entity substitution loops  *)
    entity: xmlParserInputPtr;            (* used to check entities boundaries  *)
    charset: Longint;                     (* encoding of the in-memory content - actually an xmlCharEncoding  *)
    nodelen: Longint;                     (* Those two fields are there to  *)
    nodemem: Longint;                     (* Speed up large node parsing  *)
    pedantic: Longint;                    (* signal pedantic warnings  *)
    _private: Pointer;                    (* For user data, libxml won't touch it  *)
    loadsubset: Longint;                  (* should the external subset be loaded  *)
    linenumbers: Longint;                 (* set line number in element content *)
    catalogs: Pointer;                    (* document's own catalog *)
    recovery: Longint;                    (* run in recovery mode *)
  end;

  (**
   * xmlSAXLocator:
   *
   * A SAX Locator.
   *)
  xmlSAXLocatorPtr = ^xmlSAXLocator;
  xmlSAXLocator = record
    getPublicId: function(ctx: Pointer): PxmlChar; cdecl;
    getSystemId: function(ctx: Pointer): PxmlChar; cdecl;
    getLineNumber: function(ctx: Pointer): Longint; cdecl;
    getColumnNumber: function(ctx: Pointer): Longint; cdecl;
  end;

  (**
   * resolveEntitySAXFunc:
   * @ctx:  the user data (XML parser context)
   * @publicId: The public ID of the entity
   * @systemId: The system ID of the entity
   *
   * Callback:
   * The entity loader, to control the loading of external entities,
   * the application can either:
   *    - override this resolveEntity() callback in the SAX block
   *    - or better use the xmlSetExternalEntityLoader() function to
   *      set up it's own entity resolution routine
   *
   * Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
   *)
  resolveEntitySAXFunc = function(ctx: Pointer; publicId, systemId: PxmlChar): xmlParserInputPtr; cdecl;

  (**
   * internalSubsetSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name:  the root element name
   * @ExternalID:  the external ID
   * @SystemID:  the SYSTEM ID (e.g. filename or URL)
   *
   * Callback on internal subset declaration.
   *)

  internalSubsetSAXFunc = procedure(ctx: Pointer; name, ExternalID, SystemID: PxmlChar); cdecl;
  (**
   * externalSubsetSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name:  the root element name
   * @ExternalID:  the external ID
   * @SystemID:  the SYSTEM ID (e.g. filename or URL)
   *
   * Callback on external subset declaration.
   *)
  externalSubsetSAXFunc = procedure(ctx: Pointer; name, ExternalID, SystemID: PxmlChar); cdecl;

  (**
   * getEntitySAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name: The entity name
   *
   * Get an entity by name.
   *
   * Returns the xmlEntityPtr if found.
   *)
  getEntitySAXFunc = function(ctx: Pointer; name: PxmlChar): xmlEntityPtr; cdecl;

  (**
   * getParameterEntitySAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name: The entity name
   *
   * Get a parameter entity by name.
   *
   * Returns the xmlEntityPtr if found.
   *)
  getParameterEntitySAXFunc = function(ctx: Pointer; name: PxmlChar): xmlEntityPtr; cdecl;

  (**
   * entityDeclSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name:  the entity name
   * @type:  the entity type
   * @publicId: The public ID of the entity
   * @systemId: The system ID of the entity
   * @content: the entity value (without processing).
   *
   * An entity definition has been parsed.
   *)
  entityDeclSAXFunc = procedure(ctx: Pointer; name: PxmlChar; aEntityType: Integer; publicID, systemID, content: PxmlChar); cdecl;

  (**
   * notationDeclSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name: The name of the notation
   * @publicId: The public ID of the entity
   * @systemId: The system ID of the entity
   *
   * What to do when a notation declaration has been parsed.
   *)
  notationDeclSAXFunc = procedure(ctx: Pointer; name: PxmlChar; publicID, systemID: PxmlChar); cdecl;

  (**
   * attributeDeclSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @elem:  the name of the element
   * @fullname:  the attribute name
   * @type:  the attribute type
   * @def:  the type of default value
   * @defaultValue: the attribute default value
   * @tree:  the tree of enumerated value set
   *
   * An attribute definition has been parsed.
   *)
  attributeDeclSAXFunc = procedure(ctx: Pointer; elem, name: PxmlChar; aType, def: Integer; defaultValue: PxmlChar; tree: xmlEnumerationPtr); cdecl;

  (**
   * elementDeclSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name:  the element name
   * @type:  the element type
   * @content: the element value tree
   *
   * An element definition has been parsed.
   *)
  elementDeclSAXFunc = procedure(ctx: Pointer; name: PxmlChar; aType: Integer; content: xmlElementContentPtr); cdecl;

  (**
   * unparsedEntityDeclSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name: The name of the entity
   * @publicId: The public ID of the entity
   * @systemId: The system ID of the entity
   * @notationName: the name of the notation
   *
   * What to do when an unparsed entity declaration is parsed.
   *)
  unparsedEntityDeclSAXFunc = procedure(ctx: Pointer; name: PxmlChar; publicID, systemID, notationName: PxmlChar); cdecl;

  (**
   * setDocumentLocatorSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @loc: A SAX Locator
   *
   * Receive the document locator at startup, actually xmlDefaultSAXLocator.
   * Everything is available on the context, so this is useless in our case.
   *)
  setDocumentLocatorSAXFunc = procedure(ctx: Pointer; loc: xmlSAXLocator); cdecl;

  (**
   * startDocumentSAXFunc:
   * @ctx:  the user data (XML parser context)
   *
   * Called when the document start being processed.
   *)
  startDocumentSAXFunc = procedure(ctx: Pointer); cdecl;

  (**
   * endDocumentSAXFunc:
   * @ctx:  the user data (XML parser context)
   *
   * Called when the document end has been detected.
   *)
  endDocumentSAXFunc = procedure(ctx: Pointer); cdecl;

  (**
   * startElementSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name:  The element name, including namespace prefix
   * @atts:  An array of name/value attributes pairs, NULL terminated
   *
   * Called when an opening tag has been processed.
   *)
  startElementSAXFunc = procedure(ctx: Pointer; name: PxmlChar; atts: PPxmlChar); cdecl;

  (**
   * endElementSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name:  The element name
   *
   * Called when the end of an element has been detected.
   *)
  endElementSAXFunc = procedure(ctx: Pointer; name: PxmlChar); cdecl;

  (**
   * attributeSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name:  The attribute name, including namespace prefix
   * @value:  The attribute value
   *
   * Handle an attribute that has been read by the parser.
   * The default handling is to convert the attribute into an
   * DOM subtree and past it in a new xmlAttr element added to
   * the element.
   *)
  attributeSAXFunc = procedure(ctx: Pointer; name, value: PxmlChar); cdecl;

  (**
   * referenceSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @name:  The entity name
   *
   * Called when an entity reference is detected.
   *)
  referenceSAXFunc = procedure(ctx: Pointer; name: PxmlChar); cdecl;

  (**
   * charactersSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @ch:  a xmlChar string
   * @len: the number of xmlChar
   *
   * Receiving some chars from the parser.
   *)
  charactersSAXFunc = procedure(ctx: Pointer; ch: PxmlChar; len: Integer); cdecl;

  (**
   * ignorableWhitespaceSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @ch:  a xmlChar string
   * @len: the number of xmlChar
   *
   * Receiving some ignorable whitespaces from the parser.
   * UNUSED: by default the DOM building will use characters.
   *)
  ignorableWhitespaceSAXFunc = procedure(ctx: Pointer; ch: PxmlChar; len: Integer); cdecl;

  (**
   * processingInstructionSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @target:  the target name
   * @data: the PI data's
   *
   * A processing instruction has been parsed.
   *)
  processingInstructionSAXFunc = procedure(ctx: Pointer; target, data: PxmlChar); cdecl;

  (**
   * commentSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @value:  the comment content
   *
   * A comment has been parsed.
   *)
  commentSAXFunc = procedure(ctx: Pointer; value: PxmlChar); cdecl;

  (**
   * cdataBlockSAXFunc:
   * @ctx:  the user data (XML parser context)
   * @value:  The pcdata content
   * @len:  the block length
   *
   * Called when a pcdata block has been parsed.
   *)
  cdataBlockSAXFunc = procedure(ctx: Pointer; value: PxmlChar; len: Integer); cdecl;

  (**
   * warningSAXFunc:
   * @ctx:  an XML parser context
   * @msg:  the message to display/transmit
   * @...:  extra parameters for the message display
   *
   * Display and format a warning messages, callback.
   *)
  warningSAXFunc = procedure(ctx: Pointer; msg: PxmlChar); cdecl; //varargs;

  (**
   * errorSAXFunc:
   * @ctx:  an XML parser context
   * @msg:  the message to display/transmit
   * @...:  extra parameters for the message display
   *
   * Display and format an error messages, callback.
   *)
  errorSAXFunc = procedure(ctx: Pointer; msg: PxmlChar); cdecl; //varargs;

  (**
   * fatalErrorSAXFunc:
   * @ctx:  an XML parser context
   * @msg:  the message to display/transmit
   * @...:  extra parameters for the message display
   *
   * Display and format fatal error messages, callback.
   * Note: so far fatalError() SAX callbacks are not used, error()
   *       get all the callbacks for errors.
   *)
  fatalErrorSAXFunc = procedure(ctx: Pointer; msg: PxmlChar); cdecl; //varargs;

  (**
   * isStandaloneSAXFunc:
   * @ctx:  the user data (XML parser context)
   *
   * Is this document tagged standalone?
   *
   * Returns 1 if true
   *)
  isStandaloneSAXFunc = function(ctx: Pointer): Integer; cdecl;

  (**
   * hasInternalSubsetSAXFunc:
   * @ctx:  the user data (XML parser context)
   *
   * Does this document has an internal subset?
   *
   * Returns 1 if true
   *)
  hasInternalSubsetSAXFunc = function(ctx: Pointer): Integer; cdecl;

  (**
   * hasExternalSubsetSAXFunc:
   * @ctx:  the user data (XML parser context)
   *
   * Does this document has an external subset?
   *
   * Returns 1 if true
   *)
  hasExternalSubsetSAXFunc = function(ctx: Pointer): Integer; cdecl;

  (**
   * xmlSAXHandler:
   *
   * A SAX handler is bunch of callbacks called by the parser when processing
   * of the input generate data or structure informations.
   *)
  xmlSAXHandler = record
    internalSubset: internalSubsetSAXFunc;
    isStandalone: isStandaloneSAXFunc;
    hasInternalSubset: hasInternalSubsetSAXFunc;
    hasExternalSubset: hasExternalSubsetSAXFunc;
    resolveEntity: resolveEntitySAXFunc;
    getEntity: getEntitySAXFunc;
    entityDecl: entityDeclSAXFunc;
    notationDecl: notationDeclSAXFunc;
    attributeDecl: attributeDeclSAXFunc;
    elementDecl: elementDeclSAXFunc;
    unparsedEntityDecl: unparsedEntityDeclSAXFunc;
    setDocumentLocator: setDocumentLocatorSAXFunc;
    startDocument: startDocumentSAXFunc;
    endDocument: endDocumentSAXFunc;
    startElement: startElementSAXFunc;
    endElement: endElementSAXFunc;
    reference: referenceSAXFunc;
    characters: charactersSAXFunc;
    ignorableWhitespace: ignorableWhitespaceSAXFunc;
    processingInstruction: processingInstructionSAXFunc;
    comment: commentSAXFunc;
    warning: warningSAXFunc;
    error: errorSAXFunc;
    fatalError: fatalErrorSAXFunc;      (* unused error() get all the errors *)
    getParameterEntity: getParameterEntitySAXFunc;
    cdataBlock: cdataBlockSAXFunc;
    externalSubset: externalSubsetSAXFunc;
    initialized: Integer;
  end;

  (**
   * xmlExternalEntityLoader:
   * @URL: The System ID of the resource requested
   * @ID: The Public ID of the resource requested
   * @context: the XML parser context
   *
   * External entity loaders types.
   *
   * Returns the entity input parser.
   *)
  xmlExternalEntityLoader = function(URL, ID: PChar; context: xmlParserCtxtPtr): xmlParserInputPtr; cdecl;

(*
 * Global variables: just the default SAX interface tables and XML
 * version infos.
 *)
(* TODO somehow
{$if false}
LIBXML_DLL_IMPORT extern const char *xmlParserVersion;
{$ifend}
LIBXML_DLL_IMPORT extern xmlSAXLocator xmlDefaultSAXLocator;
LIBXML_DLL_IMPORT extern xmlSAXHandler xmlDefaultSAXHandler;
LIBXML_DLL_IMPORT extern xmlSAXHandler htmlDefaultSAXHandler;
LIBXML_DLL_IMPORT extern xmlSAXHandler docbDefaultSAXHandler;
*)

(*
 * Entity substitution default behavior.
 *)

{$IFDEF ALWAYS_FALSE}
//TODO: LIBXML_DLL_IMPORT extern int xmlSubstituteEntitiesDefaultValue;
//TODO: LIBXML_DLL_IMPORT extern int xmlGetWarningsDefaultValue;
{$ENDIF}

(*
 * Init/Cleanup.
 *)
procedure xmlInitParser; cdecl; external LIBXML2_SO;
procedure xmlCleanupParser; cdecl; external LIBXML2_SO;

(*
 * Input functions.
 *)
function  xmlParserInputRead(aIn: xmlParserInputPtr; len: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlParserInputGrow(aIn: xmlParserInputPtr; len: Longint): Longint; cdecl; external LIBXML2_SO;

(*
 * xmlChar handling.
 *)
function  xmlStrdup(cur: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlStrndup(cur: PxmlChar; len: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlCharStrndup(cur: PChar; len: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlCharStrdup(cur: PChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlStrsub(str: PxmlChar; start: Longint; len: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlStrchr(str: PxmlChar; val: xmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlStrstr(str: PxmlChar; val: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlStrcasestr(str: PxmlChar; val: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlStrcmp(str1: PxmlChar; str2: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlStrncmp(str1: PxmlChar; str2: PxmlChar; len: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlStrcasecmp(str1: PxmlChar; str2: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlStrncasecmp(str1: PxmlChar; str2: PxmlChar; len: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlStrEqual(str1: PxmlChar; str2: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlStrlen(str: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlStrcat(cur: PxmlChar; add: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlStrncat(cur: PxmlChar; add: PxmlChar; len: Longint): PxmlChar; cdecl; external LIBXML2_SO;

(*
 * Basic parsing Interfaces.
 *)
function  xmlParseDoc(cur: PxmlChar): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlParseMemory(buffer: PChar; size: Longint): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlParseFile(filename: PChar): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlSubstituteEntitiesDefault(val: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlKeepBlanksDefault(val: Longint): Longint; cdecl; external LIBXML2_SO;
procedure xmlStopParser(ctxt: xmlParserCtxtPtr); cdecl; external LIBXML2_SO;
function  xmlPedanticParserDefault(val: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlLineNumbersDefault(val: Longint): Longint; cdecl; external LIBXML2_SO;

(*
 * Recovery mode.
 *)
function  xmlRecoverDoc(cur: PxmlChar): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlRecoverMemory(buffer: PChar; size: Longint): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlRecoverFile(filename: PChar): xmlDocPtr; cdecl; external LIBXML2_SO;

(*
 * Less common routines and SAX interfaces.
 *)
function  xmlParseDocument(ctxt: xmlParserCtxtPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlParseExtParsedEnt(ctxt: xmlParserCtxtPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlSAXParseDoc(sax: xmlSAXHandlerPtr; cur: PxmlChar; recovery: Longint): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlSAXUserParseFile(sax: xmlSAXHandlerPtr; user_data: Pointer; filename: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlSAXUserParseMemory(sax: xmlSAXHandlerPtr; user_data: Pointer; buffer: PChar; size: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlSAXParseMemory(sax: xmlSAXHandlerPtr; buffer: PChar; size: Longint; recovery: Longint): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlSAXParseMemoryWithData(sax: xmlSAXHandlerPtr; buffer: PChar; size: Longint; recovery: Longint; data: Pointer): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlSAXParseFile(sax: xmlSAXHandlerPtr; filename: PChar; recovery: Longint): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlSAXParseFileWithData(sax: xmlSAXHandlerPtr; filename: PChar; recovery: Longint; data: Pointer): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlSAXParseEntity(sax: xmlSAXHandlerPtr; filename: PChar): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlParseEntity(filename: PChar): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlParseDTD(ExternalID: PxmlChar; SystemID: PxmlChar): xmlDtdPtr; cdecl; external LIBXML2_SO;
function  xmlSAXParseDTD(sax: xmlSAXHandlerPtr; ExternalID: PxmlChar; SystemID: PxmlChar): xmlDtdPtr; cdecl; external LIBXML2_SO;
function  xmlIOParseDTD(sax: xmlSAXHandlerPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding): xmlDtdPtr; cdecl; external LIBXML2_SO;
function  xmlParseBalancedChunkMemory(doc: xmlDocPtr; sax: xmlSAXHandlerPtr; user_data: Pointer; depth: Longint; aString: PxmlChar; lst: PxmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlParseBalancedChunkMemoryRecover(doc: xmlDocPtr; sax: xmlSAXHandlerPtr; user_data: Pointer; depth: Longint; aString: PxmlChar; lst: PxmlNodePtr; aRecover: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlParseExternalEntity(doc: xmlDocPtr; sax: xmlSAXHandlerPtr; user_data: Pointer; depth: Longint; URL: PxmlChar; ID: PxmlChar; lst: PxmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlParseCtxtExternalEntity(ctx: xmlParserCtxtPtr; URL: PxmlChar; ID: PxmlChar; lst: PxmlNodePtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Parser contexts handling.
 *)
procedure xmlInitParserCtxt(ctxt: xmlParserCtxtPtr); cdecl; external LIBXML2_SO;
procedure xmlClearParserCtxt(ctxt: xmlParserCtxtPtr); cdecl; external LIBXML2_SO;
procedure xmlFreeParserCtxt(ctxt: xmlParserCtxtPtr); cdecl; external LIBXML2_SO;
procedure xmlSetupParserForBuffer(ctxt: xmlParserCtxtPtr; buffer: PxmlChar; filename: PChar); cdecl; external LIBXML2_SO;
function  xmlCreateDocParserCtxt(cur: PxmlChar): xmlParserCtxtPtr; cdecl; external LIBXML2_SO;

(*
 * Reading/setting optional parsing features.
 *)
function  xmlGetFeaturesList(len: PLongint; result: PPChar): Longint; cdecl; external LIBXML2_SO;
function  xmlGetFeature(ctxt: xmlParserCtxtPtr; name: PChar; result: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlSetFeature(ctxt: xmlParserCtxtPtr; name: PChar; value: Pointer): Longint; cdecl; external LIBXML2_SO;

(*
 * Interfaces for the Push mode.
 *)
function  xmlCreatePushParserCtxt(sax: xmlSAXHandlerPtr; user_data: Pointer; chunk: PChar; size: Longint; filename: PChar): xmlParserCtxtPtr; cdecl; external LIBXML2_SO;
function  xmlParseChunk(ctxt: xmlParserCtxtPtr; chunk: PChar; size: Longint; terminate: Longint): Longint; cdecl; external LIBXML2_SO;

(*
 * Special I/O mode.
 *)
function  xmlCreateIOParserCtxt(sax: xmlSAXHandlerPtr; user_data: Pointer; ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: Pointer; enc: xmlCharEncoding): xmlParserCtxtPtr; cdecl; external LIBXML2_SO;
function  xmlNewIOInputStream(ctxt: xmlParserCtxtPtr; input: xmlParserInputBufferPtr; enc: xmlCharEncoding): xmlParserInputPtr; cdecl; external LIBXML2_SO;

(*
 * Node infos.
 *)
function  xmlParserFindNodeInfo(ctxt: xmlParserCtxtPtr; node: xmlNodePtr): xmlParserNodeInfoPtr; cdecl; external LIBXML2_SO;
procedure xmlInitNodeInfoSeq(seq: xmlParserNodeInfoSeqPtr); cdecl; external LIBXML2_SO;
procedure xmlClearNodeInfoSeq(seq: xmlParserNodeInfoSeqPtr); cdecl; external LIBXML2_SO;
function  xmlParserFindNodeInfoIndex(seq: xmlParserNodeInfoSeqPtr; node: xmlNodePtr): DWORD; cdecl; external LIBXML2_SO;
procedure xmlParserAddNodeInfo(ctxt: xmlParserCtxtPtr; info: xmlParserNodeInfoPtr); cdecl; external LIBXML2_SO;

(*
 * External entities handling actually implemented in xmlIO.
 *)
procedure xmlSetExternalEntityLoader(f: xmlExternalEntityLoader); cdecl; external LIBXML2_SO;
function  xmlGetExternalEntityLoader: xmlExternalEntityLoader; cdecl; external LIBXML2_SO;
function  xmlLoadExternalEntity(URL: PChar; ID: PChar; context: xmlParserCtxtPtr): xmlParserInputPtr; cdecl; external LIBXML2_SO;

