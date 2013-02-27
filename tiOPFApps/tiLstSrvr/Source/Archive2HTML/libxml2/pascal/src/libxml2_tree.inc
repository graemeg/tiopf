// CVS-REV:gnome-xml/include/libxml/tree.h:1.119:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

(*
 * tree.h : describes the structures found in an tree resulting
 *          from an XML parsing.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *
 *)

{[pk] following declarations are in xmlIO.inc:
xmlParserInputBuffer
xmlParserInputBufferPtr
xmlOutputBuffer
xmlOutputBufferPtr
}

{[pk] following declarations are in parser.inc:
xmlParserInput
xmlParserInputPtr
xmlParserCtxt
xmlParserCtxtPtr
xmlSAXLocatorPtr
xmlSAXHandlerPtr
}

{[pk] following declarations are in entities.inc:
xmlEntityPtr
}

const
  (**
   * BASE_BUFFER_SIZE:
   *
   * default buffer size 4000.
   *)
  BASE_BUFFER_SIZE = 4000;
  (**
   * XML_XML_NAMESPACE:
   *
   * This is the namespace for the special xml: prefix predefined in the
   * XML Namespace specification.
   *)
  XML_XML_NAMESPACE = 'http://www.w3.org/XML/1998/namespace';

  (*
   * The different element types carried by an XML tree.
   *
   * NOTE: This is synchronized with DOM Level1 values
   *       See http://www.w3.org/TR/REC-DOM-Level-1/
   *
   * Actually this had diverged a bit, and now XML_DOCUMENT_TYPE_NODE should
   * be deprecated to use an XML_DTD_NODE.
   *)
  XML_ELEMENT_NODE       = 1;
  XML_ATTRIBUTE_NODE     = 2;
  XML_TEXT_NODE          = 3;
  XML_CDATA_SECTION_NODE = 4;
  XML_ENTITY_REF_NODE    = 5;
  XML_ENTITY_NODE        = 6;
  XML_PI_NODE            = 7;
  XML_COMMENT_NODE       = 8;
  XML_DOCUMENT_NODE      = 9;
  XML_DOCUMENT_TYPE_NODE = 10;
  XML_DOCUMENT_FRAG_NODE = 11;
  XML_NOTATION_NODE      = 12;
  XML_HTML_DOCUMENT_NODE = 13;
  XML_DTD_NODE           = 14;
  XML_ELEMENT_DECL       = 15;
  XML_ATTRIBUTE_DECL     = 16;
  XML_ENTITY_DECL        = 17;
  XML_NAMESPACE_DECL     = 18;
  XML_XINCLUDE_START     = 19;
  XML_XINCLUDE_END       = 20;
  XML_DOCB_DOCUMENT_NODE = 21;

type
  xmlElementType = Longint;

  (**
   * xmlChar:
   *
   * This is a basic byte in an UTF-8 encoded string.
   * It's unsigned allowing to pinpoint case where char * are assigned
   * to xmlChar * (possibly making serialization back impossible).
   *)
{[pk] moved to libxml2.pas
  xmlChar = Char;
  PxmlChar = PChar;
  PPxmlChar = ^PxmlChar;
}
// BAD_CAST: not necessary in Pascal

const
  (**
   * XML_LOCAL_NAMESPACE:
   *
   * A namespace declaration node
   *)
  XML_LOCAL_NAMESPACE = XML_NAMESPACE_DECL;

type
  xmlDocPtr = ^xmlDoc;
  xmlDtdPtr = ^xmlDtd;
  xmlNodePtr = ^xmlNode;

  PxmlNodePtr = ^xmlNodePtr;

  (**
   * xmlNotation:
   *
   * A DTD Notation definition.
   *)
  xmlNotationPtr = ^xmlNotation;
  xmlNotation = record
    name: PxmlChar;                 (* Notation name  *)
    PublicID: PxmlChar;             (* Public identifier, if any  *)
    SystemID: PxmlChar;             (* System identifier, if any  *)
  end;

  (**
   * xmlAttributeType:
   *
   * A DTD Attribute type definition.
   *)

  xmlAttributeType = (
    DUMMY_XML_ATTRIBUTE_TYPE_0, // = 0 // this is for D5 compatibility
    XML_ATTRIBUTE_CDATA,        // = 1,
    XML_ATTRIBUTE_ID,
    XML_ATTRIBUTE_IDREF,
    XML_ATTRIBUTE_IDREFS,
    XML_ATTRIBUTE_ENTITY,
    XML_ATTRIBUTE_ENTITIES,
    XML_ATTRIBUTE_NMTOKEN,
    XML_ATTRIBUTE_NMTOKENS,
    XML_ATTRIBUTE_ENUMERATION,
    XML_ATTRIBUTE_NOTATION
  );

  (**
   * xmlAttributeDefault:
   *
   * A DTD Attribute default definition.
   *)
  xmlAttributeDefault = (
    DUMMY_XML_ATTRIBUTE_DEFAULT_0, // = 0 // this is for D5 compatibility
    XML_ATTRIBUTE_NONE,            // = 1,
    XML_ATTRIBUTE_REQUIRED,
    XML_ATTRIBUTE_IMPLIED,
    XML_ATTRIBUTE_FIXED
  );

  (**
   * xmlEnumeration:
   *
   * list structure used when there is an enumeration in DTDs.
   *)
  xmlEnumerationPtr = ^xmlEnumeration;
  xmlEnumeration = record
    next: xmlEnumerationPtr;        (* next one  *)
    name: PxmlChar;                 (* Enumeration name  *)
  end;

  (**
   * xmlAttribute:
   *
   * An Attribute declaration in a DTD.
   *)
  xmlAttributePtr = ^xmlAttribute;
  xmlAttribute = record
    _private: Pointer;              (* application data *)
    type_: xmlElementType;          (* XML_ATTRIBUTE_DECL, must be second !  *)
    name: PxmlChar;                 (* Attribute name  *)
    children: xmlNodePtr;           (* NULL  *)
    last: xmlNodePtr;               (* NULL  *)
    parent: xmlDtdPtr;              (* -> DTD  *)
    next: xmlNodePtr;               (* next sibling link   *)
    prev: xmlNodePtr;               (* previous sibling link   *)
    doc: xmlDocPtr;                 (* the containing document  *)
    nexth: xmlAttributePtr;         (* next in hash table  *)
    atype: xmlAttributeType;        (* The attribute type  *)
    def: xmlAttributeDefault;       (* the default  *)
    defaultValue: PxmlChar;         (* or the default value  *)
    tree: xmlEnumerationPtr;        (* or the enumeration tree if any  *)
    prefix: PxmlChar;               (* the namespace prefix if any  *)
    elem: PxmlChar;                 (* Element holding the attribute  *)
  end;

  (**
   * xmlElementContentType:
   *
   * Possible definitions of element content types.
   *)
  xmlElementContentType = (
    DUMMY_XML_ELEMENT_CONTENT_TYPE_0, // = 0 // this is for D5 compatibility
    XML_ELEMENT_CONTENT_PCDATA,       // = 1,
    XML_ELEMENT_CONTENT_ELEMENT,
    XML_ELEMENT_CONTENT_SEQ,
    XML_ELEMENT_CONTENT_OR
  );

  (**
   * xmlElementContentOccur:
   *
   * Possible definitions of element content occurences.
   *)
  xmlElementContentOccur = (
    DUMMY_XML_ELEMENT_CONTENT_OCCUR_0, // = 0 // this is for D5 compatibility
    XML_ELEMENT_CONTENT_ONCE,          // = 1,
    XML_ELEMENT_CONTENT_OPT,
    XML_ELEMENT_CONTENT_MULT,
    XML_ELEMENT_CONTENT_PLUS
  );

  (**
   * xmlElementContent:
   *
   * An XML Element content as stored after parsing an element definition
   * in a DTD.
   *)
  xmlElementContentPtr = ^xmlElementContent;
  xmlElementContent = record
    type_: xmlElementContentType;   (* PCDATA, ELEMENT, SEQ or OR *)
    ocur: xmlElementContentOccur;   (* ONCE, OPT, MULT or PLUS *)
    name: PxmlChar;                 (* Element name *)
    c1: xmlElementContentPtr;       (* first child *)
    c2: xmlElementContentPtr;       (* second child *)
    parent: xmlElementContentPtr;   (* parent *)
    prefix: PxmlChar;               (* Namespace prefix *)
  end;

  (**
   * xmlElementTypeVal:
   *
   * The differnt possibility for an element content type.
   *)
  xmlElementTypeVal = (
    XML_ELEMENT_TYPE_UNDEFINED, // = 0,
    XML_ELEMENT_TYPE_EMPTY,
    XML_ELEMENT_TYPE_ANY,
    XML_ELEMENT_TYPE_MIXED,
    XML_ELEMENT_TYPE_ELEMENT
  );

  (**
   * xmlElement:
   *
   * An XML Element declaration from a DTD.
   *)
  xmlElementPtr = ^xmlElement;
  xmlElement = record
    _private: Pointer;              (* application data *)
    type_: xmlElementType;          (* XML_ELEMENT_DECL, must be second !  *)
    name: PxmlChar;                 (* Element name  *)
    children: xmlNodePtr;           (* NULL  *)
    last: xmlNodePtr;               (* NULL  *)
    parent: xmlDtdPtr;              (* -> DTD  *)
    next: xmlNodePtr;               (* next sibling link   *)
    prev: xmlNodePtr;               (* previous sibling link   *)
    doc: xmlDocPtr;                 (* the containing document  *)
    etype: xmlElementTypeVal;       (* The type  *)
    content: xmlElementContentPtr;  (* the allowed element content  *)
    attributes: xmlAttributePtr;    (* List of the declared attributes  *)
    prefix: PxmlChar;               (* the namespace prefix if any  *)
{$IFDEF LIBXML_REGEXP_ENABLED}
    contModel: xmlRegexpPtr;        (* the validating regexp *)
{$ELSE}
    contModel: Pointer;
{$ENDIF}
  end;

  xmlNsType = xmlElementType;
  (**
   * xmlNs:
   *
   * An XML namespace.
   * Note that prefix == NULL is valid, it defines the default namespace
   * within the subtree (until overriden).
   *
   * xmlNsType is unified with xmlElementType.
   *)
  xmlNsPtr = ^xmlNs;
  PxmlNsPtr = ^xmlNsPtr;
  xmlNs = record
    next: xmlNsPtr;                 (* next Ns link for this node   *)
    nodetype: xmlNsType;            (* global or local  *)
    href: PxmlChar;                 (* URL for the namespace  *)
    prefix: PxmlChar;               (* prefix for the namespace  *)
    _private: Pointer;              (* application data *)
  end;

  (**
   * xmlDtd:
   *
   * An XML DtD, as defined by <!DOCTYPE ... There is actually one for
   * the internal subset and for the external subset.
   *)
  xmlDtd = record
    _private: Pointer;              (* application data *)
    type_: xmlElementType;          (* XML_DTD_NODE, must be second !  *)
    name: PxmlChar;                 (* Name of the DTD  *)
    children: xmlNodePtr;           (* the value of the property link  *)
    last: xmlNodePtr;               (* last child link  *)
    parent: xmlDocPtr;              (* child->parent link  *)
    next: xmlNodePtr;               (* next sibling link   *)
    prev: xmlNodePtr;               (* previous sibling link   *)
    doc: xmlDocPtr;                 (* the containing document  *)
    (* End of common part  *)
    notations: Pointer;             (* Hash table for notations if any  *)
    elements: Pointer;              (* Hash table for elements if any  *)
    attributes: Pointer;            (* Hash table for attributes if any  *)
    entities: Pointer;              (* Hash table for entities if any  *)
    ExternalID: PxmlChar;           (* External identifier for PUBLIC DTD  *)
    SystemID: PxmlChar;             (* URI for a SYSTEM or PUBLIC DTD  *)
    pentities: Pointer;             (* Hash table for param entities if any  *)
  end;

  (**
   * xmlAttr:
   *
   * A attribute on an XML node.
   *)

  xmlAttrPtr = ^xmlAttr;
  xmlAttr = record
    _private: Pointer;              (* application data *)
    type_: xmlElementType;          (* XML_ATTRIBUTE_NODE, must be second !  *)
    name: PxmlChar;                 (* the name of the property  *)
    children: xmlNodePtr;           (* the value of the property  *)
    last: xmlNodePtr;               (* NULL  *)
    parent: xmlNodePtr;             (* child->parent link  *)
    next: xmlAttrPtr;               (* next sibling link   *)
    prev: xmlAttrPtr;               (* previous sibling link   *)
    doc: xmlDocPtr;                 (* the containing document  *)
    ns: xmlNsPtr;                   (* Pointer to the associated namespace  *)
    atype: xmlAttributeType;        (* the attribute type if validating  *)
  end;

  (**
   * xmlID:
   * An XML ID instance.
   *)
  xmlIDPtr = ^xmlID;
  xmlID = record
    next: xmlIDPtr;                 (* next ID *)
    value: PxmlChar;                (* The ID name *)
    attr: xmlAttrPtr;               (* The attribut holding it *)
    name: PxmlChar;                 (* The attribute if attr is not available *)
    lineno: Longint;                (* The line number if attr is not available *)
  end;

  (**
   * xmlRef:
   * An XML IDREF instance.
   *)
  xmlRefPtr = ^xmlRef;
  xmlRef = record
    next: xmlRefPtr;                (* next Ref  *)
    value: PxmlChar;                (* The Ref name  *)
    attr: xmlAttrPtr;               (* The attribut holding it  *)
    name: PxmlChar;                 (* The attribute if attr is not available *)
    lineno: Longint;                (* The line number if attr is not available *)
  end;

  (**
   * xmlBufferAllocationScheme:
   *
   * A buffer allocation scheme can be defined to either match exactly the
   * need or double it's allocated size each time it is found too small.
   *)
  xmlBufferAllocationScheme = (
    XML_BUFFER_ALLOC_DOUBLEIT, // = 0,
    XML_BUFFER_ALLOC_EXACT
  );

  (**
   * xmlBuffer:
   *
   * A buffer structure.
   *)
  xmlBufferPtr = ^xmlBuffer;
  xmlBuffer = record
    content: PxmlChar; (* The buffer content UTF8  *)
    use: DWORD; (* The buffer size used  *)
    size: DWORD; (* The buffer size  *)
    alloc: xmlBufferAllocationScheme; (* The realloc method  *)
  end;

  (**
   * xmlNode:
   *
   * A node in an XML tree.
   *)
  xmlNode = record
    _private: Pointer;              (* application data *)
    type_: xmlElementType;          (* type number, must be second !  *)
    name: PxmlChar;                 (* the name of the node, or the entity  *)
    children: xmlNodePtr;           (* parent->childs link  *)
    last: xmlNodePtr;               (* last child link  *)
    parent: xmlNodePtr;             (* child->parent link  *)
    next: xmlNodePtr;               (* next sibling link   *)
    prev: xmlNodePtr;               (* previous sibling link   *)
    doc: xmlDocPtr;                 (* the containing document  *)
    (* End of common part  *)
    ns: xmlNsPtr;                   (* Pointer to the associated namespace  *)
    content: PxmlChar;              (* the content  *)
    properties: xmlAttrPtr;         (* properties list  *)
    nsDef: xmlNsPtr; (* namespace definitions on this node  *)
  end;

  (**
   * xmlDoc:
   *
   * An XML document.
   *)
  xmlDoc = record
    _private: Pointer;              (* application data *)
    type_: xmlElementType;          (* XML_DOCUMENT_NODE, must be second !  *)
    name: PChar;                    (* name/filename/URI of the document  *)
    children: xmlNodePtr;           (* the document tree  *)
    last: xmlNodePtr;               (* last child link  *)
    parent: xmlNodePtr;             (* child->parent link  *)
    next: xmlNodePtr;               (* next sibling link   *)
    prev: xmlNodePtr;               (* previous sibling link   *)
    doc: xmlDocPtr;                 (* autoreference to itself  *)
    (* End of common part  *)
    compression: Longint;           (* level of zlib compression  *)
    standalone: Longint;            (* standalone document (no external refs)  *)
    intSubset: xmlDtdPtr;           (* the document internal subset  *)
    extSubset: xmlDtdPtr;           (* the document external subset  *)
    oldNs: xmlNsPtr;                (* Global namespace, the old way  *)
    version: PxmlChar;              (* the XML version string  *)
    encoding: PxmlChar;             (* external initial encoding, if any  *)
    ids: Pointer;                   (* Hash table for ID attributes if any  *)
    refs: Pointer;                  (* Hash table for IDREFs attributes if any  *)
    URL: PxmlChar;                  (* The URI for that document  *)
    charset: Longint;               (* encoding of the in-memory content - actually an xmlCharEncoding  *)
  end;

(**
 * Variables.
 *)
{$IFDEF HAS_SO_VARS}
LIBXML_DLL_IMPORT extern int oldXMLWDcompatibility; (* maintain compatibility with old WD *)
LIBXML_DLL_IMPORT extern int xmlIndentTreeOutput;  (* try to indent the tree dumps *)
LIBXML_DLL_IMPORT extern xmlBufferAllocationScheme xmlBufferAllocScheme; (* alloc scheme to use *)
LIBXML_DLL_IMPORT extern int xmlSaveNoEmptyTags; (* save empty tags as <empty></empty> *)
LIBXML_DLL_IMPORT extern int xmlDefaultBufferSize; (* default buffer size *)
{$ENDIF}

(*
 * Handling Buffers.
 *)
procedure xmlSetBufferAllocationScheme(scheme: xmlBufferAllocationScheme); cdecl; external LIBXML2_SO;
function  xmlGetBufferAllocationScheme: xmlBufferAllocationScheme; cdecl; external LIBXML2_SO;
function  xmlBufferCreate: xmlBufferPtr; cdecl; external LIBXML2_SO;
function  xmlBufferCreateSize(size: Integer): xmlBufferPtr; cdecl; external LIBXML2_SO;
function  xmlBufferResize(buf: xmlBufferPtr; size: DWORD): Longint; cdecl; external LIBXML2_SO;
procedure xmlBufferFree(buf: xmlBufferPtr); cdecl; external LIBXML2_SO;
function  xmlBufferDump(aFile: PLibXml2File; buf: xmlBufferPtr): Longint; cdecl; external LIBXML2_SO;
procedure xmlBufferAdd(buf: xmlBufferPtr; str: PxmlChar; len: Longint); cdecl; external LIBXML2_SO;
procedure xmlBufferAddHead(buf: xmlBufferPtr; str: PxmlChar; len: Longint); cdecl; external LIBXML2_SO;
procedure xmlBufferCat(buf: xmlBufferPtr; str: PxmlChar); cdecl; external LIBXML2_SO;
procedure xmlBufferCCat(buf: xmlBufferPtr; str: PChar); cdecl; external LIBXML2_SO;
function  xmlBufferShrink(buf: xmlBufferPtr; len: DWORD): Longint; cdecl; external LIBXML2_SO;
function  xmlBufferGrow(buf: xmlBufferPtr; len: DWORD): Longint; cdecl; external LIBXML2_SO;
procedure xmlBufferEmpty(buf: xmlBufferPtr); cdecl; external LIBXML2_SO;
function  xmlBufferContent(buf: xmlBufferPtr): PxmlChar; cdecl; external LIBXML2_SO;
procedure xmlBufferSetAllocationScheme(buf: xmlBufferPtr; scheme: xmlBufferAllocationScheme); cdecl; external LIBXML2_SO;
function  xmlBufferLength(buf: xmlBufferPtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Creating/freeing new structures.
 *)
function  xmlCreateIntSubset(doc: xmlDocPtr; name: PxmlChar; ExternalID: PxmlChar; SystemID: PxmlChar): xmlDtdPtr; cdecl; external LIBXML2_SO;
function  xmlNewDtd(doc: xmlDocPtr; name: PxmlChar; ExternalID: PxmlChar; SystemID: PxmlChar): xmlDtdPtr; cdecl; external LIBXML2_SO;
function  xmlGetIntSubset(doc: xmlDocPtr): xmlDtdPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeDtd(cur: xmlDtdPtr); cdecl; external LIBXML2_SO;
function  xmlNewGlobalNs(doc: xmlDocPtr; href: PxmlChar; prefix: PxmlChar): xmlNsPtr; cdecl; external LIBXML2_SO; {$IFDEF HAS_DEPR} deprecated; {$ENDIF}
function  xmlNewNs(node: xmlNodePtr; href: PxmlChar; prefix: PxmlChar): xmlNsPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeNs(cur: xmlNsPtr); cdecl; external LIBXML2_SO;
procedure xmlFreeNsList(cur: xmlNsPtr); cdecl; external LIBXML2_SO;
function  xmlNewDoc(version: PxmlChar): xmlDocPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeDoc(cur: xmlDocPtr); cdecl; external LIBXML2_SO;
function  xmlNewDocProp(doc: xmlDocPtr; name: PxmlChar; value: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlNewProp(node: xmlNodePtr; name: PxmlChar; value: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlNewNsProp(node: xmlNodePtr; ns: xmlNsPtr; name: PxmlChar; value: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlNewNsPropEatName(node: xmlNodePtr; ns: xmlNsPtr; name: PxmlChar; value: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
procedure xmlFreePropList(cur: xmlAttrPtr); cdecl; external LIBXML2_SO;
procedure xmlFreeProp(cur: xmlAttrPtr); cdecl; external LIBXML2_SO;
function  xmlCopyProp(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlCopyPropList(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlCopyDtd(dtd: xmlDtdPtr): xmlDtdPtr; cdecl; external LIBXML2_SO;
function  xmlCopyDoc(doc: xmlDocPtr; recursive: Longint): xmlDocPtr; cdecl; external LIBXML2_SO;

(*
 * Creating new nodes.
 *)
function  xmlNewDocNode(doc: xmlDocPtr; ns: xmlNsPtr; name: PxmlChar; content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewDocNodeEatName(doc: xmlDocPtr; ns: xmlNsPtr; name: PxmlChar; content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewDocRawNode(doc: xmlDocPtr; ns: xmlNsPtr; name: PxmlChar; content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewNode(ns: xmlNsPtr; name: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewNodeEatName(ns: xmlNsPtr; name: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO; 
function  xmlNewChild(parent: xmlNodePtr; ns: xmlNsPtr; name: PxmlChar; content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewTextChild(parent: xmlNodePtr; ns: xmlNsPtr; name: PxmlChar; content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewDocText(doc: xmlDocPtr; content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewText(content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewPI(name: PxmlChar; content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewDocTextLen(doc: xmlDocPtr; content: PxmlChar; len: Longint): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewTextLen(content: PxmlChar; len: Longint): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewDocComment(doc: xmlDocPtr; content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewComment(content: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewCDataBlock(doc: xmlDocPtr; content: PxmlChar; len: Longint): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewCharRef(doc: xmlDocPtr; name: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewReference(doc: xmlDocPtr; name: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlCopyNode(node: xmlNodePtr; recursive: Longint): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlDocCopyNode(const node: xmlNodePtr; doc: xmlDocPtr; recursive: Longint): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlCopyNodeList(const node: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNewDocFragment(doc: xmlDocPtr): xmlNodePtr; cdecl; external LIBXML2_SO;

(*
 * Navigating.
 *)
function  xmlGetLineNo(node: xmlNodePtr): int64; cdecl; external LIBXML2_SO;
function  xmlGetNodePath(node: xmlNodePtr): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlDocGetRootElement(doc: xmlDocPtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlGetLastChild(parent: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNodeIsText(node: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlIsBlankNode(node: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Changing the structure.
 *)
function  xmlDocSetRootElement(doc: xmlDocPtr; root: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
procedure xmlNodeSetName(cur: xmlNodePtr; name: PxmlChar); cdecl; external LIBXML2_SO;
function  xmlAddChild(parent: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlAddChildList(parent: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlReplaceNode(old: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlAddSibling(cur: xmlNodePtr; elem: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlAddPrevSibling(cur: xmlNodePtr; elem: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlAddNextSibling(cur: xmlNodePtr; elem: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
procedure xmlUnlinkNode(cur: xmlNodePtr); cdecl; external LIBXML2_SO;
function  xmlTextMerge(first: xmlNodePtr; second: xmlNodePtr): xmlNodePtr; cdecl; external LIBXML2_SO;
procedure xmlTextConcat(node: xmlNodePtr; content: PxmlChar; len: Longint); cdecl; external LIBXML2_SO;
procedure xmlFreeNodeList(cur: xmlNodePtr); cdecl; external LIBXML2_SO;
procedure xmlFreeNode(cur: xmlNodePtr); cdecl; external LIBXML2_SO;
procedure xmlSetTreeDoc(tree: xmlNodePtr; doc: xmlDocPtr); cdecl; external LIBXML2_SO;
procedure xmlSetListDoc(list: xmlNodePtr; doc: xmlDocPtr); cdecl; external LIBXML2_SO;

(*
 * Namespaces.
 *)
function  xmlSearchNs(doc: xmlDocPtr; node: xmlNodePtr; nameSpace: PxmlChar): xmlNsPtr; cdecl; external LIBXML2_SO;
function  xmlSearchNsByHref(doc: xmlDocPtr; node: xmlNodePtr; href: PxmlChar): xmlNsPtr; cdecl; external LIBXML2_SO;
function  xmlGetNsList(doc: xmlDocPtr; node: xmlNodePtr): PxmlNsPtr; cdecl; external LIBXML2_SO;
procedure xmlSetNs(node: xmlNodePtr; ns: xmlNsPtr); cdecl; external LIBXML2_SO;
function  xmlCopyNamespace(cur: xmlNsPtr): xmlNsPtr; cdecl; external LIBXML2_SO;
function  xmlCopyNamespaceList(cur: xmlNsPtr): xmlNsPtr; cdecl; external LIBXML2_SO;

(*
 * Changing the content.
 *)
function  xmlSetProp(node: xmlNodePtr; name: PxmlChar; value: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlGetProp(node: xmlNodePtr; name: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlGetNoNsProp(node: xmlNodePtr; name: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlHasProp(node: xmlNodePtr; name: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlHasNsProp(node: xmlNodePtr; name: PxmlChar; nameSpace: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlSetNsProp(node: xmlNodePtr; ns: xmlNsPtr; name: PxmlChar; value: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlGetNsProp(node: xmlNodePtr; name: PxmlChar; nameSpace: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlStringGetNodeList(doc: xmlDocPtr; value: PxmlChar): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlStringLenGetNodeList(doc: xmlDocPtr; value: PxmlChar; len: Longint): xmlNodePtr; cdecl; external LIBXML2_SO;
function  xmlNodeListGetString(doc: xmlDocPtr; list: xmlNodePtr; ainLine: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlNodeListGetRawString(doc: xmlDocPtr; list: xmlNodePtr; ainLine: Longint): PxmlChar; cdecl; external LIBXML2_SO;
procedure xmlNodeSetContent(cur: xmlNodePtr; content: PxmlChar); cdecl; external LIBXML2_SO;
procedure xmlNodeSetContentLen(cur: xmlNodePtr; content: PxmlChar; len: Longint); cdecl; external LIBXML2_SO;
procedure xmlNodeAddContent(cur: xmlNodePtr; content: PxmlChar); cdecl; external LIBXML2_SO;
procedure xmlNodeAddContentLen(cur: xmlNodePtr; content: PxmlChar; len: Longint); cdecl; external LIBXML2_SO;
function  xmlNodeGetContent(cur: xmlNodePtr): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlNodeGetLang(cur: xmlNodePtr): PxmlChar; cdecl; external LIBXML2_SO;
procedure xmlNodeSetLang(cur: xmlNodePtr; lang: PxmlChar); cdecl; external LIBXML2_SO;
function  xmlNodeGetSpacePreserve(cur: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
procedure xmlNodeSetSpacePreserve(cur: xmlNodePtr; val: Longint); cdecl; external LIBXML2_SO;
function  xmlNodeGetBase(doc: xmlDocPtr; cur: xmlNodePtr): PxmlChar; cdecl; external LIBXML2_SO;
procedure xmlNodeSetBase(cur: xmlNodePtr; uri: PxmlChar); cdecl; external LIBXML2_SO;

(*
 * Removing content.
 *)
function  xmlRemoveProp(attr: xmlAttrPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlUnsetProp(node: xmlNodePtr; name: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlUnsetNsProp(node: xmlNodePtr; ns: xmlNsPtr; name: PxmlChar): Longint; cdecl; external LIBXML2_SO;

(*
 * Internal, don't use.
 *)
procedure xmlBufferWriteChar(buf: xmlBufferPtr; aString: PChar); cdecl; external LIBXML2_SO;
procedure xmlBufferWriteQuotedString(buf: xmlBufferPtr; aString: PxmlChar); cdecl; external LIBXML2_SO;

(*
 * Namespace handling.
 *)
function  xmlReconciliateNs(doc: xmlDocPtr; tree: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Saving.
 *)
procedure xmlDocDumpFormatMemory(cur: xmlDocPtr; var mem: PxmlChar; size: PLongint; format: Longint); cdecl; external LIBXML2_SO;
procedure xmlDocDumpMemory(cur: xmlDocPtr; var mem: PxmlChar; size: PLongint); cdecl; external LIBXML2_SO;
procedure xmlDocDumpMemoryEnc(out_doc: xmlDocPtr; var doc_txt_ptr: PxmlChar; doc_txt_len: PLongint; txt_encoding: PChar); cdecl; external LIBXML2_SO;
procedure xmlDocDumpFormatMemoryEnc(out_doc: xmlDocPtr; var doc_txt_ptr: PxmlChar; doc_txt_len: PLongint; txt_encoding: PChar; format: Longint); cdecl; external LIBXML2_SO;
function  xmlDocFormatDump(f: PLibXml2File; cur: xmlDocPtr; format: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlDocDump(f: PLibXml2File; cur: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
procedure xmlElemDump(f: PLibXml2File; doc: xmlDocPtr; cur: xmlNodePtr); cdecl; external LIBXML2_SO;
function  xmlSaveFile(filename: PChar; cur: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlSaveFormatFile(filename: PChar; cur: xmlDocPtr; format: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlNodeDump(buf: xmlBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; level: Longint; format: Longint): Longint; cdecl; external LIBXML2_SO;
{ [pk] moved to libxml2.pas - needs declarations from xmlIO
function  xmlSaveFileTo(buf: xmlOutputBufferPtr; cur: xmlDocPtr; encoding: Char): Integer; cdecl; external LIBXML2_SO;
function  xmlSaveFormatFileTo(buf: xmlOutputBufferPtr; cur: xmlDocPtr; encoding: Char; format: Integer): Integer; cdecl; external LIBXML2_SO;
procedure xmlNodeDumpOutput(buf: xmlOutputBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; level: Integer; format: Integer; encoding: PChar); cdecl; external LIBXML2_SO;
}
function  xmlSaveFormatFileEnc(filename: PChar; cur: xmlDocPtr; encoding: PChar; format: Integer): Integer; cdecl; external LIBXML2_SO;
function  xmlSaveFileEnc(filename: PChar; cur: xmlDocPtr; encoding: PChar): Longint; cdecl; external LIBXML2_SO;


(*
 * XHTML
 *)
function xmlIsXHTML(systemID, publicID: PxmlChar): Longint; cdecl; external LIBXML2_SO;

(*
 * Compression.
 *)
function  xmlGetDocCompressMode(doc: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
procedure xmlSetDocCompressMode(doc: xmlDocPtr; mode: Longint); cdecl; external LIBXML2_SO;
function  xmlGetCompressMode: Longint; cdecl; external LIBXML2_SO;                           
procedure xmlSetCompressMode(mode: Longint); cdecl; external LIBXML2_SO;

