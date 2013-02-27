// CVS-REV:gnome-xml/include/libxml/c14n.h:1.2:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * "Canonical XML" implementation 
 * http://www.w3.org/TR/xml-c14n
 * 
 * "Exclusive XML Canonicalization" implementation
 * http://www.w3.org/TR/xml-exc-c14n
 
 * See Copyright for the status of this software.
 *
 * Author: Aleksey Sanin <aleksey@aleksey.com>
 *)

(*
 * XML Canonicazation
 * http://www.w3.org/TR/xml-c14n
 *
 * Exclusive XML Canonicazation
 * http://www.w3.org/TR/xml-exc-c14n
 *
 * Canonical form of an XML document could be created if and only if
 *  a) default attributes (if any) are added to all nodes
 *  b) all character and parsed entity references are resolved
 * In order to achive this in libxml2 the document MUST be loaded with
 * following global setings:
 *
 *    xmlLoadExtDtdDefaultValue = XML_DETECT_IDS | XML_COMPLETE_ATTRS;
 *    xmlSubstituteEntitiesDefault(1);
 *
 * or corresponding parser context setting:
 *    xmlParserCtxtPtr ctxt;
 *
 *    ...
 *    ctxt->loadsubset = XML_DETECT_IDS | XML_COMPLETE_ATTRS;
 *    ctxt->replaceEntities = 1;
 *    ...
 *)


function  xmlC14NDocSaveTo (doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: Longint; inclusive_ns_prefixes: PPxmlChar; with_comments: Longint; buf: xmlOutputBufferPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlC14NDocDumpMemory (doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: Longint; inclusive_ns_prefixes: PPxmlChar; with_comments: Longint; doc_txt_ptr: PPxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlC14NDocSave (doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: Longint; inclusive_ns_prefixes: PPxmlChar; with_comments: Longint; filename: PChar; compression: Longint): Longint; cdecl; external LIBXML2_SO;

(**
 * This is the core C14N function
 *)
type
  xmlC14NIsVisibleCallback = function (user_data: Pointer; node: xmlNodePtr; parent: xmlNodePtr): Longint; cdecl;

function xmlC14NExecute(doc: xmlDocPtr; is_visible_callback: xmlC14NIsVisibleCallback; user_data: Pointer; exclusive: Longint; inclusive_ns_prefixes: PPxmlChar; with_comments: Longint; buf: xmlOutputBufferPtr): Longint; cdecl; external LIBXML2_SO;

