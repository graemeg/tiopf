// CVS-REV:libxslt/libxslt/extensions.h:1.13:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * extension.h: interface for the extension support
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

(**
 * Extension Modules API.
 *)

(**
 * xsltStyleExtInitFunction:
 * @ctxt: an XSLT stylesheet
 * @URI: the namespace URI for the extension
 *
 * A function called at initialization time of an XSLT extension module.
 *
 * Returns a Pointer to the module specific data for this transformation.
 *)
type
  xsltStyleExtInitFunction = function (style: xsltStylesheetPtr; URI: PxmlChar): Pointer; cdecl;

  (**
   * xsltStyleExtShutdownFunction:
   * @ctxt: an XSLT stylesheet
   * @URI: the namespace URI for the extension
   * @data: the data associated to this module
   *
   * A function called at shutdown time of an XSLT extension module.
   *)
  xsltStyleExtShutdownFunction = function (style: xsltStylesheetPtr; URI: PxmlChar; data: Pointer): Pointer; cdecl;

  (**
   * xsltExtInitFunction:
   * @ctxt: an XSLT transformation context
   * @URI: the namespace URI for the extension
   *
   * A function called at initialization time of an XSLT extension module.
   *
   * Returns a Pointer to the module specific data for this transformation.
   *)
  xsltExtInitFunction = function (ctxt: xsltTransformContextPtr; URI: PxmlChar): Pointer; cdecl;

  (**
   * xsltExtShutdownFunction:
   * @ctxt: an XSLT transformation context
   * @URI: the namespace URI for the extension
   * @data: the data associated to this module
   *
   * A function called at shutdown time of an XSLT extension module.
   *)
  xsltExtShutdownFunction = function (ctxt: xsltTransformContextPtr; URI: PxmlChar; data: Pointer): Pointer; cdecl;

function  xsltRegisterExtModule(URI: PxmlChar; initFunc: xsltExtInitFunction; shutdownFunc: xsltExtShutdownFunction): Longint; cdecl; external LIBXSLT_SO;
function  xsltRegisterExtModuleFull(URI: PxmlChar; initFunc: xsltExtInitFunction; shutdownFunc: xsltExtShutdownFunction; styleInitFunc: xsltStyleExtInitFunction; styleShutdownFunc: xsltStyleExtShutdownFunction): Longint; cdecl; external LIBXSLT_SO;
function  xsltUnregisterExtModule(URI: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
function  xsltGetExtData(ctxt: xsltTransformContextPtr; URI: PxmlChar): Pointer; cdecl; external LIBXSLT_SO;
function  xsltStyleGetExtData(style: xsltStylesheetPtr; URI: PxmlChar): Pointer; cdecl; external LIBXSLT_SO;
procedure xsltShutdownCtxtExts(ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO;
procedure xsltShutdownExts(style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;
function  xsltXPathGetTransformContext(ctxt: xmlXPathParserContextPtr): xsltTransformContextPtr; cdecl; external LIBXSLT_SO;

(*
 * extension functions
 *)
function  xsltRegisterExtModuleFunction(name: PxmlChar; URI: PxmlChar; function_: xmlXPathFunction): Longint; cdecl; external LIBXSLT_SO;
function  xsltExtFunctionLookup(ctxt: xsltTransformContextPtr; name: PxmlChar; URI: PxmlChar): xmlXPathFunction; cdecl; external LIBXSLT_SO;
function  xsltExtModuleFunctionLookup(name: PxmlChar; URI: PxmlChar): xmlXPathFunction; cdecl; external LIBXSLT_SO;
function  xsltUnregisterExtModuleFunction(name: PxmlChar; URI: PxmlChar): Longint; cdecl; external LIBXSLT_SO;

(*
 * extension elements
 *)
type
  xsltPreComputeFunction = function (style: xsltStylesheetPtr;inst: xmlNodePtr; aFunction: xsltTransformFunction): xsltElemPreCompPtr; cdecl;

function  xsltNewElemPreComp(style: xsltStylesheetPtr; inst: xmlNodePtr; transformfunction: xsltTransformFunction): xsltElemPreCompPtr; cdecl; external LIBXSLT_SO;
procedure xsltInitElemPreComp(comp: xsltElemPreCompPtr; style: xsltStylesheetPtr; inst: xmlNodePtr; function_: xsltTransformFunction; freeFunc: xsltElemPreCompDeallocator); cdecl; external LIBXSLT_SO;
function  xsltRegisterExtModuleElement(name: PxmlChar; URI: PxmlChar; precomp: xsltPreComputeFunction; transform: xsltTransformFunction): Longint; cdecl; external LIBXSLT_SO;
function  xsltExtElementLookup(ctxt: xsltTransformContextPtr; name: PxmlChar; URI: PxmlChar): xsltTransformFunction; cdecl; external LIBXSLT_SO;
function  xsltExtModuleElementLookup(name: PxmlChar; URI: PxmlChar): xsltTransformFunction; cdecl; external LIBXSLT_SO;
function  xsltExtModuleElementPreComputeLookup(name: PxmlChar; URI: PxmlChar): xsltPreComputeFunction; cdecl; external LIBXSLT_SO;
function  xsltUnregisterExtModuleElement(name: PxmlChar; URI: PxmlChar): Longint; cdecl; external LIBXSLT_SO;

(*
 * top-level elements
 *)
type
  xsltTopLevelFunction = procedure (style: xsltStylesheetPtr;inst: xmlNodePtr); cdecl;

function  xsltRegisterExtModuleTopLevel(name: PxmlChar; URI: PxmlChar; function_: xsltTopLevelFunction): Longint; cdecl; external LIBXSLT_SO;
function  xsltExtModuleTopLevelLookup(name: PxmlChar; URI: PxmlChar): xsltTopLevelFunction; cdecl; external LIBXSLT_SO;
function  xsltUnregisterExtModuleTopLevel(name: PxmlChar; URI: PxmlChar): Longint; cdecl; external LIBXSLT_SO;

(* These 2 functions are deprecated for use within modules *)
function  xsltRegisterExtFunction(ctxt: xsltTransformContextPtr; name: PxmlChar; URI: PxmlChar; function_: xmlXPathFunction): Longint; cdecl; external LIBXSLT_SO; {$IFDEF HAS_DEPR} deprecated; {$ENDIF}
function  xsltRegisterExtElement(ctxt: xsltTransformContextPtr; name: PxmlChar; URI: PxmlChar; function_: xsltTransformFunction): Longint; cdecl; external LIBXSLT_SO; {$IFDEF HAS_DEPR} deprecated; {$ENDIF}

(*
 * Extension Prefix handling API.
 * Those are used by the XSLT (pre)processor.
 *)
function  xsltRegisterExtPrefix(style: xsltStylesheetPtr; prefix: PxmlChar; URI: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
function  xsltCheckExtPrefix(style: xsltStylesheetPtr; prefix: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
function  xsltInitCtxtExts(ctxt: xsltTransformContextPtr): Longint; cdecl; external LIBXSLT_SO;
procedure xsltFreeCtxtExts(ctxt: xsltTransformContextPtr); cdecl; external LIBXSLT_SO;
procedure xsltFreeExts(style: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;
function  xsltPreComputeExtModuleElement(style: xsltStylesheetPtr; inst: xmlNodePtr): xsltElemPreCompPtr; cdecl; external LIBXSLT_SO;

(**
 * Test module http: //xmlsoft.org/XSLT/
 *)
procedure xsltRegisterTestModule; cdecl; external LIBXSLT_SO;
procedure xsltDebugDumpExtensions(output: PLibXml2File); cdecl; external LIBXSLT_SO;

