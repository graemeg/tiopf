// CVS-REV:gnome-xml/include/libxml/debugXML.h:1.22:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}

(*
 * debugXML.h : Interfaces to a set of routines used for debugging the tree
 *              produced by the XML parser.
 *
 * Daniel Veillard <daniel@veillard.com>
 *)

(*
 * The standard Dump routines.
 *)
procedure xmlDebugDumpString(aOutput: PLibXml2File; str: PxmlChar); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpAttr(aOutput: PLibXml2File; attr: xmlAttrPtr; depth: Longint); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpAttrList(aOutput: PLibXml2File; attr: xmlAttrPtr; depth: Longint); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpOneNode(aOutput: PLibXml2File; node: xmlNodePtr; depth: Longint); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpNode(aOutput: PLibXml2File; node: xmlNodePtr; depth: Longint); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpNodeList(aOutput: PLibXml2File; node: xmlNodePtr; depth: Longint); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpDocumentHead(aOutput: PLibXml2File; doc: xmlDocPtr); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpDocument(aOutput: PLibXml2File; doc: xmlDocPtr); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpDTD(aOutput: PLibXml2File; dtd: xmlDtdPtr); cdecl; external LIBXML2_SO;
procedure xmlDebugDumpEntities(aOutput: PLibXml2File; doc: xmlDocPtr); cdecl; external LIBXML2_SO;
procedure xmlLsOneNode(aOutput: PLibXml2File; node: xmlNodePtr); cdecl; external LIBXML2_SO;
function  xmlBoolToText(boolval: Longint): PChar; cdecl; external LIBXML2_SO;

(****************************************************************
 *                                                              *
 *         The XML shell related structures and functions       *
 *                                                              *
 ****************************************************************)

type
  (**
   * xmlShellReadlineFunc:
   * @prompt:  a string prompt
   *
   * This is a generic signature for the XML shell input function.
   *
   * Returns a string which will be freed by the Shell.
   *)
  xmlShellReadlineFunc = function(prompt: PChar): PChar; cdecl;

  (**
   * xmlShellCtxt:
   *
   * A debugging shell context.
   * TODO: add the defined function tables.
   *)
  xmlShellCtxtPtr = ^xmlShellCtxt;
  xmlShellCtxt = record
    filename: PChar;
    doc: xmlDocPtr;
    node: xmlNodePtr;
    pctxt: xmlXPathContextPtr;
    loaded: Longint;
    output: PLibXml2File;
    input: xmlShellReadlineFunc;
  end;

  (**
   * xmlShellCmd:
   * @ctxt:  a shell context
   * @arg:  a string argument
   * @node:  a first node
   * @node2:  a second node
   *
   * This is a generic signature for the XML shell functions.
   *
   * Returns an int, negative returns indicating errors.
   *)
  xmlShellCmd = function(ctxt: xmlShellCtxtPtr; arg: PChar; node, node2: xmlNodePtr): Longint; cdecl;

procedure xmlShellPrintXPathError(errorType: Longint; arg: PChar); cdecl; external LIBXML2_SO;
procedure xmlShellPrintNode(node: xmlNodePtr); cdecl; external LIBXML2_SO;
procedure xmlShellPrintXPathResult(list: xmlXPathObjectPtr); cdecl; external LIBXML2_SO;
function  xmlShellList(ctxt: xmlShellCtxtPtr; arg: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellBase(ctxt: xmlShellCtxtPtr; arg: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellDir(ctxt: xmlShellCtxtPtr; arg: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellCat(ctxt: xmlShellCtxtPtr; arg: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellLoad(ctxt: xmlShellCtxtPtr; filename: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellWrite(ctxt: xmlShellCtxtPtr; filename: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellSave(ctxt: xmlShellCtxtPtr; filename: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellValidate(ctxt: xmlShellCtxtPtr; dtd: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellDu(ctxt: xmlShellCtxtPtr; arg: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlShellPwd(ctxt: xmlShellCtxtPtr; buffer: PChar; node, node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;

(*
 * The Shell interface.
 *)
procedure xmlShell(doc: xmlDocPtr; filename: PChar; input: xmlShellReadlineFunc; aOutput: PLibXml2File); cdecl; external LIBXML2_SO;

