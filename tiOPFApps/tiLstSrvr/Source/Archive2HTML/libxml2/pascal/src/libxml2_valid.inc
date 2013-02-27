// CVS-REV:gnome-xml/include/libxml/valid.h:1.39:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

(*
 * valid.h : interface to the DTD handling and the validity checking
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  (*
   * Validation state added for non-determinist content model.
   *)
  xmlValidStatePtr = Pointer;

  (**
   * xmlValidityErrorFunc:
   * @ctx:  an xmlValidCtxtPtr validity error context
   * @msg:  the string to format *printf like vararg
   * @...:  remaining arguments to the format
   *
   * Callback called when a validity error is found. This is a message
   * oriented function similar to an *printf function.
   *)
  xmlValidityErrorFunc = procedure(ctx: Pointer; msg: PChar); cdecl; {$IFDEF HAS_TVA} varargs; {$ENDIF}
  (**
   * xmlValidityWarningFunc:
   * @ctx:  an xmlValidCtxtPtr validity error context
   * @msg:  the string to format *printf like vararg
   * @...:  remaining arguments to the format
   *
   * Callback called when a validity warning is found. This is a message
   * oriented function similar to an *printf function.
   *)
  xmlValidityWarningFunc = procedure(ctx: Pointer; msg: PChar); cdecl; {$IFDEF HAS_TVA} varargs; {$ENDIF}

  (**
   * xmlValidCtxt:
   * an xmlValidCtxt is used for error reporting when validating.
   *)
  xmlValidCtxtPtr = ^xmlValidCtxt;
  xmlValidCtxt = record
    userData: Pointer;                  (* user specific data block  *)
    error: xmlValidityErrorFunc;        (* the callback in case of errors  *)
    warning: xmlValidityWarningFunc;    (* the callback in case of warning  *)

    (* Node analysis stack used when validating within entities *)
    node: xmlNodePtr;                   (* Current parsed Node  *)
    nodeNr: Longint;                    (* Depth of the parsing stack  *)
    nodeMax: Longint;                   (* Max depth of the parsing stack  *)
    nodeTab: ^xmlNodePtr;               (* array of nodes  *)
    finishDtd: Longint;                 (* finished validating the Dtd ?  *)
    doc: xmlDocPtr;                     (* the document  *)
    valid: Longint;                     (* temporary validity check result  *)

    (* state state used for non-determinist content validation *)
    vstate: xmlValidStatePtr;           (* current state  *)
    vstateNr: Longint;                  (* Depth of the validation stack  *)
    vstateMax: Longint;                 (* Max depth of the validation stack  *)
    vstateTab: xmlValidStatePtr;        (* array of validation states  *)

{$IFDEF LIBXML_REGEXP_ENABLED}
    am: xmlAutomataPtr;                 (* the automata *)
    state: xmlAutomataStatePtr;         (* used to build the automata *)
{$ELSE}
    am: Pointer;
    state: Pointer;
{$ENDIF}
  end;

  (*
   * All notation declarations are stored in a table.
   * There is one table per DTD.
   *)
  xmlNotationTable = xmlHashTable;
  xmlNotationTablePtr = ^xmlNotationTable;

  (*
   * All element declarations are stored in a table.
   * There is one table per DTD.
   *)
  xmlElementTable = xmlHashTable;
  xmlElementTablePtr = ^xmlElementTable;

  (*
   * All attribute declarations are stored in a table.
   * There is one table per DTD.
   *)
  xmlAttributeTable = xmlHashTable;
  xmlAttributeTablePtr = ^xmlAttributeTable;

  (*
   * All IDs attributes are stored in a table.
   * There is one table per document.
   *)
  xmlIDTable = xmlHashTable;
  xmlIDTablePtr = ^xmlIDTable;

  (*
   * All Refs attributes are stored in a table.
   * There is one table per document.
   *)
  xmlRefTable = xmlHashTable;
  xmlRefTablePtr = ^xmlRefTable;

(* helper *)
function  xmlSplitQName2(name: PxmlChar; prefix: PPxmlChar): PxmlChar; cdecl; external LIBXML2_SO;

(* Notation *)
function  xmlAddNotationDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; name: PxmlChar; PublicID: PxmlChar; SystemID: PxmlChar): xmlNotationPtr; cdecl; external LIBXML2_SO;
function  xmlCopyNotationTable(table: xmlNotationTablePtr): xmlNotationTablePtr; cdecl; external LIBXML2_SO;
procedure xmlFreeNotationTable(table: xmlNotationTablePtr); cdecl; external LIBXML2_SO;
procedure xmlDumpNotationDecl(buf: xmlBufferPtr; nota: xmlNotationPtr); cdecl; external LIBXML2_SO;
procedure xmlDumpNotationTable(buf: xmlBufferPtr; table: xmlNotationTablePtr); cdecl; external LIBXML2_SO;

(* Element Content *)
function  xmlNewElementContent(name: PxmlChar; aType: xmlElementContentType): xmlElementContentPtr; cdecl; external LIBXML2_SO;
function  xmlCopyElementContent(content: xmlElementContentPtr): xmlElementContentPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeElementContent(cur: xmlElementContentPtr); cdecl; external LIBXML2_SO;
procedure xmlSnprintfElementContent(buf: PChar; size: Integer; content: xmlElementContentPtr; glob: Longint); cdecl; external LIBXML2_SO; {$IFDEF HAS_DEPR} deprecated; {$ENDIF}
procedure xmlSprintfElementContent(buf: PChar; content: xmlElementContentPtr; glob: Longint); cdecl; external LIBXML2_SO; {$IFDEF HAS_DEPR} deprecated; {$ENDIF}

(* Element *)
function  xmlAddElementDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; name: PxmlChar; aType: xmlElementTypeVal; content: xmlElementContentPtr): xmlElementPtr; cdecl; external LIBXML2_SO;
function  xmlCopyElementTable(table: xmlElementTablePtr): xmlElementTablePtr; cdecl; external LIBXML2_SO;
procedure xmlFreeElementTable(table: xmlElementTablePtr); cdecl; external LIBXML2_SO;
procedure xmlDumpElementTable(buf: xmlBufferPtr; table: xmlElementTablePtr); cdecl; external LIBXML2_SO;
procedure xmlDumpElementDecl(buf: xmlBufferPtr; elem: xmlElementPtr); cdecl; external LIBXML2_SO;

(* Enumeration *)
function  xmlCreateEnumeration(name: PxmlChar): xmlEnumerationPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeEnumeration(cur: xmlEnumerationPtr); cdecl; external LIBXML2_SO;
function  xmlCopyEnumeration(cur: xmlEnumerationPtr): xmlEnumerationPtr; cdecl; external LIBXML2_SO;

(* Attribute *)
function  xmlAddAttributeDecl(ctxt: xmlValidCtxtPtr; dtd: xmlDtdPtr; elem: PxmlChar; name: PxmlChar; ns: PxmlChar; aType: xmlAttributeType; def: xmlAttributeDefault; defaultValue: PxmlChar; tree: xmlEnumerationPtr): xmlAttributePtr; cdecl; external LIBXML2_SO;
function  xmlCopyAttributeTable(table: xmlAttributeTablePtr): xmlAttributeTablePtr; cdecl; external LIBXML2_SO;
procedure xmlFreeAttributeTable(table: xmlAttributeTablePtr); cdecl; external LIBXML2_SO;
procedure xmlDumpAttributeTable(buf: xmlBufferPtr; table: xmlAttributeTablePtr); cdecl; external LIBXML2_SO;
procedure xmlDumpAttributeDecl(buf: xmlBufferPtr; attr: xmlAttributePtr); cdecl; external LIBXML2_SO;

(* IDs *)
function  xmlAddID(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; value: PxmlChar; attr: xmlAttrPtr): xmlIDPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeIDTable(table: xmlIDTablePtr); cdecl; external LIBXML2_SO;

function  xmlGetID(doc: xmlDocPtr; ID: PxmlChar): xmlAttrPtr; cdecl; external LIBXML2_SO;
function  xmlIsID(doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlRemoveID(doc: xmlDocPtr; attr: xmlAttrPtr): Longint; cdecl; external LIBXML2_SO;

(* IDREFs *)
function  xmlAddRef(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; value: PxmlChar; attr: xmlAttrPtr): xmlRefPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeRefTable(table: xmlRefTablePtr); cdecl; external LIBXML2_SO;
function  xmlIsRef(doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlRemoveRef(doc: xmlDocPtr; attr: xmlAttrPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlGetRefs(doc: xmlDocPtr; ID: PxmlChar): xmlListPtr; cdecl; external LIBXML2_SO;

(**
 * The public function calls related to validity checking.
 *)
function  xmlValidateRoot(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateElementDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlElementPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidNormalizeAttributeValue(doc: xmlDocPtr; elem: xmlNodePtr; name: PxmlChar; value: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlValidCtxtNormalizeAttributeValue(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; name, value: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlValidateAttributeDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; attr: xmlAttributePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateAttributeValue(aType: xmlAttributeType; value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateNotationDecl(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; nota: xmlNotationPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateDtd(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; dtd: xmlDtdPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateDtdFinal(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateDocument(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateOneElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateOneAttribute(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; attr: xmlAttrPtr; value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateOneNamespace(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; prefix: PxmlChar; ns: xmlNsPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateDocumentFinal(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateNotationUse(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; notationName: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlIsMixedElement(doc: xmlDocPtr; name: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlGetDtdAttrDesc(dtd: xmlDtdPtr; elem: PxmlChar; name: PxmlChar): xmlAttributePtr; cdecl; external LIBXML2_SO;
function  xmlGetDtdQAttrDesc(dtd: xmlDtdPtr; elem: PxmlChar; name, prefix: PxmlChar): xmlAttributePtr; cdecl; external LIBXML2_SO;
function  xmlGetDtdNotationDesc(dtd: xmlDtdPtr; name: PxmlChar): xmlNotationPtr; cdecl; external LIBXML2_SO;
function  xmlGetDtdQElementDesc(dtd: xmlDtdPtr; name, prefix: PxmlChar): xmlElementPtr; cdecl; external LIBXML2_SO;
function  xmlGetDtdElementDesc(dtd: xmlDtdPtr; name: PxmlChar): xmlElementPtr; cdecl; external LIBXML2_SO;
function  xmlValidGetValidElements(prev: xmlNodePtr; next: xmlNodePtr; list: PPxmlChar; max: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlValidGetPotentialChildren(ctree: xmlElementContentPtr; list: PPxmlChar; len: PLongint; max: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateNameValue(value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateNamesValue(value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateNmtokenValue(value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlValidateNmtokensValue(value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
{$IFDEF LIBXML_REGEXP_ENABLED}
(*
 * Validation based on the regexp support
 *)
function  xmlValidBuildContentModel(ctxt: xmlValidCtxtPtr; elem: xmlElementPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlValidatePushElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; qname: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlValidatePushCData(ctxt: xmlValidCtxtPtr; data: PxmlChar; len: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlValidatePopElement(ctxt: xmlValidCtxtPtr; doc: xmlDocPtr; elem: xmlNodePtr; qname: PxmlChar): Longint; cdecl; external LIBXML2_SO;
{$ENDIF}

