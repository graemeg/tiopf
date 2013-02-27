// CVS-REV:gnome-xml/include/libxml/HTMLparser.h:1.28:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * HTMLparser.h : interface for an HTML 4.0 non-verifying parser
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  (*
   * Most of the back-end structures from XML and HTML are shared.
   *)
  htmlParserCtxt = xmlParserCtxt;
  htmlParserCtxtPtr = xmlParserCtxtPtr;
  htmlParserNodeInfo = xmlParserNodeInfo;
  htmlSAXHandler = xmlSAXHandler;
  htmlSAXHandlerPtr = xmlSAXHandlerPtr;
  htmlParserInput = xmlParserInput;
  htmlParserInputPtr = xmlParserInputPtr;
  htmlDocPtr = xmlDocPtr;
  htmlNodePtr = xmlNodePtr;

  (*
   * Internal description of an HTML element, representing HTML 4.01
   * and XHTML 1.0 (which share the same structure).
   *)
  htmlElemDescPtr = ^htmlElemDesc;
  htmlElemDesc = record
    name: PChar;                    (* The tag name *)
    startTag: Char;                 (* Whether the start tag can be implied *)
    endTag: Char;                   (* Whether the end tag can be implied *)
    saveEndTag: Char;               (* Whether the end tag should be saved *)
    empty: Char;                    (* Is this an empty element ? *)
    depr: Char;                     (* Is this a deprecated element ? *)
    dtd: Char;                      (* 1: only in Loose DTD, 2: only Frameset one *)
    isinline: Char;                 (* is this a block 0 or inline 1 element *)
    desc: PChar;                    (* the description *)
    subelts : ^PChar;               (* allowed sub-elements of this element *)
    defaultsubelt : PChar;          (* subelement for suggested auto-repair
                                       if necessary or NULL *)
    attrs_opt : ^PChar;             (* Optional Attributes *)
    attrs_depr : ^PChar;            (* Additional deprecated attributes *)
    attrs_req : ^PChar;             (* Required attributes *)
  end;

  (*
   * Internal description of an HTML entity.
   *)
  htmlEntityDescPtr = ^htmlEntityDesc;
  htmlEntityDesc = record
    value: DWORD;                   (* the UNICODE value for the character *)
    name: PChar;                    (* The entity name *)
    desc: PChar;                    (* the description *)
  end;

(*
 * There is only few public functions.
 *)
function  htmlTagLookup(tag: PxmlChar): htmlElemDescPtr; cdecl; external LIBXML2_SO;
function  htmlEntityLookup(name: PxmlChar): htmlEntityDescPtr; cdecl; external LIBXML2_SO;
function  htmlEntityValueLookup(value: DWORD): htmlEntityDescPtr; cdecl; external LIBXML2_SO;
function  htmlIsAutoClosed(doc: htmlDocPtr; elem: htmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  htmlAutoCloseTag(doc: htmlDocPtr; name: PxmlChar; elem: htmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  htmlParseEntityRef(ctxt: htmlParserCtxtPtr; str: PPxmlChar): htmlEntityDescPtr; cdecl; external LIBXML2_SO;
function  htmlParseCharRef(ctxt: htmlParserCtxtPtr): Longint; cdecl; external LIBXML2_SO;
procedure htmlParseElement(ctxt: htmlParserCtxtPtr); cdecl; external LIBXML2_SO;
function  htmlParseDocument(ctxt: htmlParserCtxtPtr): Longint; cdecl; external LIBXML2_SO; 
function  htmlSAXParseDoc(cur: PxmlChar; encoding: PChar; sax: htmlSAXHandlerPtr; userData: Pointer): htmlDocPtr; cdecl; external LIBXML2_SO;
function  htmlParseDoc(cur: PxmlChar; encoding: PChar): htmlDocPtr; cdecl; external LIBXML2_SO;
function  htmlSAXParseFile(filename: PChar; encoding: PChar; sax: htmlSAXHandlerPtr; userData: Pointer): htmlDocPtr; cdecl; external LIBXML2_SO;
function  htmlParseFile(filename: PChar; encoding: PChar): htmlDocPtr; cdecl; external LIBXML2_SO;
function  UTF8ToHtml(aOut: PByte; outlen: PLongint; aIn: PByte; inlen: PLongint): Longint; cdecl; external LIBXML2_SO;
function  htmlEncodeEntities(aOut: PByte; outlen: PLongint; aIn: PByte; inlen: PLongint; quoteChar: Longint): Longint; cdecl; external LIBXML2_SO;
function  htmlIsScriptAttribute(name: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  htmlHandleOmittedElem(val: Longint): Longint; cdecl; external LIBXML2_SO;

(**
 * Interfaces for the Push mode.
 *)
procedure htmlFreeParserCtxt(ctxt: htmlParserCtxtPtr); cdecl; external LIBXML2_SO;
function  htmlCreatePushParserCtxt(sax: htmlSAXHandlerPtr; user_data: Pointer; chunk: PChar; size: Longint; filename: PChar; enc: xmlCharEncoding): htmlParserCtxtPtr; cdecl; external LIBXML2_SO;
function  htmlParseChunk(ctxt: htmlParserCtxtPtr; chunk: PChar; size: Longint; terminate: Longint): Longint; cdecl; external LIBXML2_SO;
(* NRK/Jan2003: further knowledge of HTML structure
 *)


type
  htmlStatus = Integer;
const
  HTML_NA           = 0;    (* something we don't check at all *)
  HTML_INVALID      = $1;
  HTML_DEPRECATED   = $2;
  HTML_VALID        = $4;
  HTML_REQUIRED     = $c;   (* VALID bit set so ( & HTML_VALID ) is TRUE *)
(* Using htmlElemDesc rather than name here, to emphasise the fact
   that otherwise there's a lookup overhead
*)
function htmlAttrAllowed(_para1: htmlElemDescPtr; _para2: PxmlChar; _para3: Longint): htmlStatus; cdecl; external LIBXML2_SO;
function htmlElementAllowedHere(_para1: htmlElemDescPtr; _para2: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function htmlElementStatusHere(_para1: htmlElemDescPtr; _para2: htmlElemDescPtr): htmlStatus; cdecl; external LIBXML2_SO;
function htmlNodeStatus(_para1: htmlNodePtr; _para2: Longint): htmlStatus; cdecl; external LIBXML2_SO;
{TODO: translate macros
#define htmlDefaultSubelement(elt) elt->defaultsubelt
#define htmlElementAllowedHereDesc(parent,elt) \
	htmlElementAllowedHere((parent), (elt)->name)
#define htmlRequiredAttrs(elt) (elt)->attrs_req
}

