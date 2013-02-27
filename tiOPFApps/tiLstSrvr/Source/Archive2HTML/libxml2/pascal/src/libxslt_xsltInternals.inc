// CVS-REV:libxslt/libxslt/xsltInternals.h:1.75:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Uwe Fechner <ufechner@4commerce.de> - 4commerce technologies AG
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * xsltInternals.h: internal data structures, constants and functions used
 *                  by the XSLT engine.
 *                  They are not part of the API or ABI, i.e. they can change
 *                  without prior notice, use carefully.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)


const
  (**
   * XSLT_MAX_SORT:
   *
   * Max number of specified xsl: sort on an element.
  *)
  XSLT_MAX_SORT = 15;

  (**
   * XSLT_PAT_NO_PRIORITY:
   *
   * Specific value for pattern without priority expressed.
   *)
  XSLT_PAT_NO_PRIORITY = -(12345789);

  (**
   * XSLT_RUNTIME_EXTRA_LST:
   * @ctxt: the transformation context
   * @nr: the index
   *
   * Macro used to access extra information stored in the context
   *)
/// #define XSLT_RUNTIME_EXTRA_LST(ctxt, nr) (ctxt)->extras[(nr)].info

  (**
   * XSLT_RUNTIME_EXTRA_FREE:
   * @ctxt: the transformation context
   * @nr: the index
   *
   * Macro used to free extra information stored in the context
   *)
/// #define XSLT_RUNTIME_EXTRA_FREE(ctxt, nr) (ctxt)->extras[(nr)].deallocate

  (**
   * XSLT_RUNTIME_EXTRA:
   * @ctxt: the transformation context
   * @nr: the index
   *
   * Macro used to define extra information stored in the context
   *)
/// #define XSLT_RUNTIME_EXTRA(ctxt, nr) (ctxt)->extras[(nr)].val

type
  xsltStylesheetPtr=^xsltStylesheet;

  xsltTemplatePtr = ^xsltTemplate;
  xsltTemplate = record
    next: xsltTemplatePtr;                       (* chained list sorted by priority *)
    style: xsltStylesheetPtr;                    (* the containing stylesheet *)
    match: PxmlChar;                             (* the matching string *)
    priority: double;                            (* as given from the stylesheet, not computed *)
    name: PxmlChar;                              (* the local part of the name QName *)
    nameURI: PxmlChar;                           (* the URI part of the name QName *)
    mode: PxmlChar;                              (* the local part of the mode QName *)
    modeURI: PxmlChar;                           (* the URI part of the mode QName *)
    content: xmlNodePtr;                         (* the template replacement value *)
    elem: xmlNodePtr;                            (* the source element *)

    inheritedNsNr: Longint;                      (* number of inherited namespaces *)
    inheritedNs: PxmlNsPtr;                      (* inherited non-excluded namespaces *)

    (* Profiling informations *)
    nbCalls: Longint;                            (* the number of time the template was called *)
    time: dword;                                 (* the time spent in this template *)
  end;

  (**
   * xsltDecimalFormat:
   *
   * Data structure of decimal-format.
   *)
  xsltDecimalFormatPtr = ^xsltDecimalFormat;
  xsltDecimalFormat = record
    next: xsltDecimalFormatPtr;                  (* chained list *)
    name: PxmlChar;
    (* Used for interpretation of pattern *)
    digit: PxmlChar;
    patternSeparator: PxmlChar;
    (* May appear in result *)
    minusSign: PxmlChar;
    infinity: PxmlChar;
    noNumber: PxmlChar;                          (* Not-a-number *)
    (* Used for interpretation of pattern and may appear in result *)
    decimalPoint: PxmlChar;
    grouping: PxmlChar;
    percent: PxmlChar;
    permille: PxmlChar;
    zeroDigit: PxmlChar;
  end;

  (**
   * xsltDocument:
   *
   * Data structure associated to a parsed document.
   *)
  xsltDocumentPtr = ^xsltDocument;
  xsltDocument = record
    next: xsltDocumentPtr;                       (* documents are kept in a chained list *)
    main: Longint;                               (* is this the main document *)
    doc: xmlDocPtr;                              (* the parsed document *)
    keys: Pointer;                               (* key tables storage *)
  end;

  xsltTransformContextPtr = ^xsltTransformContext;

  (**
   * xsltElemPreComp:
   *
   * The in-memory structure corresponding to element precomputed data,
   * designed to be extended by extension implementors.
   *)
  xsltElemPreCompPtr = ^xsltElemPreComp;

  (**
   * xsltTransformFunction:
   * @ctxt: the XSLT transformation context
   * @node: the input node
   * @inst: the stylesheet node
   * @comp: the compiled information from the stylesheet
   *
   * Signature of the function associated to elements part of the
   * stylesheet language like xsl: if or xsl: apply-templates.
   *)
  xsltTransformFunction = procedure(ctxt: xsltTransformContextPtr; node, inst: xmlNodePtr; comp: xsltElemPreCompPtr); cdecl;

  (**
   * xsltSortFunc:
   * @ctxt:    a transformation context
   * @sorts:   the node-set to sort
   * @nbsorts: the number of sorts
   *
   * Signature of the function to use during sorting
   *)
  xsltSortFunc = procedure (ctxt: xsltTransformContextPtr; var sorts: xmlNodePtr; nbsorts: Longint); cdecl;

  xsltStyleType = (
    DUMMY__XSLT_FUNC_0, // this is for D5 compatibility
    XSLT_FUNC_COPY, // = 1,
    XSLT_FUNC_SORT,
    XSLT_FUNC_TEXT,
    XSLT_FUNC_ELEMENT,
    XSLT_FUNC_ATTRIBUTE,
    XSLT_FUNC_COMMENT,
    XSLT_FUNC_PI,
    XSLT_FUNC_COPYOF,
    XSLT_FUNC_VALUEOF,
    XSLT_FUNC_NUMBER,
    XSLT_FUNC_APPLYIMPORTS,
    XSLT_FUNC_CALLTEMPLATE,
    XSLT_FUNC_APPLYTEMPLATES,
    XSLT_FUNC_CHOOSE,
    XSLT_FUNC_IF,
    XSLT_FUNC_FOREACH,
    XSLT_FUNC_DOCUMENT,
    XSLT_FUNC_WITHPARAM,
    XSLT_FUNC_PARAM,
    XSLT_FUNC_VARIABLE,
    XSLT_FUNC_WHEN,
    XSLT_FUNC_EXTENSION
  );
  (**
   * xsltElemPreCompDeallocator:
   * @comp: the #xsltElemPreComp to free up
   *
   * Deallocates an #xsltElemPreComp structure.
   *)
  xsltElemPreCompDeallocator = procedure(comp: xsltElemPreCompPtr); cdecl;

  (**
   * xsltElemPreComp:
   *
   * The in-memory structure corresponding to element precomputed data,
   * designed to be extended by extension implementors.
   *)
  xsltElemPreComp = record
    next: xsltElemPreCompPtr;                    (* chained list *)
    type_: xsltStyleType;                        (* type of the element *)
    func: xsltTransformFunction;                 (* handling function *)
    inst: xmlNodePtr;                            (* the instruction *)
    (* end of common part *)
    free: xsltElemPreCompDeallocator;            (* the deallocator *)
  end;

  (**
   * xsltStylePreComp:
   *
   * The in-memory structure corresponding to XSLT stylesheet constructs
   * precomputed data.
   *)
  xsltStylePreCompPtr = ^xsltStylePreComp;
  xsltStylePreComp = record
    next: xsltElemPreCompPtr;                    (* chained list *)
    type_: xsltStyleType;                        (* type of the element *)
    func: xsltTransformFunction;                 (* handling function *)
    inst: xmlNodePtr;                            (* the instruction *)
    (*
     * Pre computed values
     *)
    stype: PxmlChar;                             (* sort *)
    has_stype: Longint;                          (* sort *)
    number: Longint;                             (* sort *)
    order: PxmlChar;                             (* sort *)
    has_order: Longint;                          (* sort *)
    descending: Longint;                         (* sort *)
    lang: PxmlChar;                              (* sort *)
    has_lang: Longint;                           (* sort *)
    case_order: PxmlChar;                        (* sort *)
    lower_first: Longint;                        (* sort *)
    use: PxmlChar;                               (* copy, element *)
    has_use: Longint;                            (* copy, element *)
    noescape: Longint;                           (* text *)
    name: PxmlChar;                              (* element, attribute, pi *)
    has_name: Longint;                           (* element, attribute, pi *)
    ns: PxmlChar;                                (* element *)
    has_ns: Longint;                             (* element *)
    mode: PxmlChar;                              (* apply-templates *)
    modeURI: PxmlChar;                           (* apply-templates *)
    test: PxmlChar;                              (* if *)
    templ: xsltTemplatePtr;                      (* call-template *)
    select: PxmlChar;                            (* sort, copy-of, value-of, apply-templates *)
    ver11: Longint;                              (* document *)
    filename: PxmlChar;                          (* document URL *)
    has_filename: Longint;                       (* document *)
    numdata: xsltNumberData;                     (* number *)
    comp: xmlXPathCompExprPtr;                   (* a precompiled XPath expression *)
    nsList: PxmlNsPtr;                           (* the namespaces in scope *)
    nsNr: Longint;                               (* the number of namespaces in scope *)
  end;

  (*
   * The in-memory structure corresponding to an XSLT Variable
   * or Param.
   *)
  xsltStackElemPtr = ^xsltStackElem;
  xsltStackElem = record
    next: xsltStackElemPtr;                      (* chained list *)
    comp: xsltStylePreCompPtr;                   (* the compiled form *)
    computed: Longint;                           (* was the evaluation done *)
    name: PxmlChar;                              (* the local part of the name QName *)
    nameURI: PxmlChar;                           (* the URI part of the name QName *)
    select: PxmlChar;                            (* the eval string *)
    tree: xmlNodePtr;                            (* the tree if no eval string or the location *)
    value: xmlXPathObjectPtr;                    (* The value if computed *)
  end;

  (*
   * The in-memory structure corresponding to an XSLT Stylesheet.
   * NOTE: most of the content is simply linked from the doc tree
   *       structure, no specific allocation is made.
  *)
//  xsltStylesheetPtr = ^xsltStylesheet;
  xsltStylesheet = record
    (*
     * The stylesheet import relation is kept as a tree.
    *)
    parent: xsltStylesheetPtr;
    next: xsltStylesheetPtr;
    imports: xsltStylesheetPtr;
    docList: xsltDocumentPtr;                    (* the include document list *)
    (*
     * General data on the style sheet document.
     *)
    doc: xmlDocPtr;                              (* the parsed XML stylesheet *)
    stripSpaces: xmlHashTablePtr;                (* the hash table of the strip-space and
                                                    preserve space elements *)
    stripAll: Longint;                           (* strip-space   (1) preserve-space   (-1) *)
    cdataSection: xmlHashTablePtr;               (* the hash table of the cdata-section *)
    (*
     * Global variable or parameters.
     *)
    variables: xsltStackElemPtr;                 (* linked list of param and variables *)
    (*
     * Template descriptions.
     *)
    templates: xsltTemplatePtr;                  (* the ordered list of templates *)
    templatesHash: Pointer;                      (* hash table or wherever compiled templates
                                                        informations are stored *)
    rootMatch: Pointer;                          (* template based on / *)
    keyMatch: Pointer;                           (* template based on key() *)
    elemMatch: Pointer;                          (* template based on   *)
    attrMatch: Pointer;                          (* template based on @  *)
    parentMatch: Pointer;                        (* template based on .. *)
    textMatch: Pointer;                          (* template based on text() *)
    piMatch: Pointer;                            (* template based on processing-instruction() *)
    commentMatch: Pointer;                       (* template based on comment() *)
    (*
     * Namespace aliases.
     *)
    nsAliases: xmlHashTablePtr;                  (* the namespace alias hash tables *)
    (*
     * Attribute sets.
     *)
    attributeSets: xmlHashTablePtr;              (* the attribute sets hash tables *)
    (*
     * Namespaces.
     *)
    nsHash: xmlHashTablePtr;                     (* the set of namespaces in use *)
    nsDefs: Pointer;                             (* the namespaces defined *)
    (*
     * Key definitions.
     *)
    keys: Pointer;                               (* key definitions *)
    (*
     * Output related stuff.
     *)
    method: PxmlChar;                            (* the output method *)
    methodURI: PxmlChar;                         (* associated namespace if any *)
    version: PxmlChar;                           		(* version string *)
    encoding: PxmlChar;                          (* encoding string *)
    omitXmlDeclaration: Longint;                 (* omit-xml-declaration = "yes" | "no" *)
    (*
     * Number formatting.
     *)
    decimalFormat: xsltDecimalFormatPtr;
    standalone: Longint;                         (* standalone = "yes" | "no" *)
    doctypePublic: PxmlChar;                     (* doctype-public string *)
    doctypeSystem: PxmlChar;                     (* doctype-system string *)
    indent: Longint;                             (* should output being indented *)
    mediaType: PxmlChar;                         (* media-type string *)
    (*
     * Precomputed blocks.
     *)
    preComps: xsltElemPreCompPtr;                (* list of precomputed blocks *)
    warnings: Longint;                           (* number of warnings found at compilation *)
    errors: Longint;                             (* number of errors found at compilation *)
    exclPrefix: PxmlChar;                        (* last excluded prefixes *)
    exclPrefixTab: PPxmlChar;                    (* array of excluded prefixes *)
    exclPrefixNr: Longint;                       (* number of excluded prefixes in scope *)
    exclPrefixMax: Longint;                      (* size of the array *)
    _private: Pointer;                           (* user defined data *)
    (*
     * Extensions.
     *)
    extInfos: xmlHashTablePtr;                   (* the extension data *)
    extrasNr: Longint;                           (* the number of extras required *)
  end;

  (*
   * The in-memory structure corresponding to an XSLT Transformation.
   *)
  xsltOutputType = (
    XSLT_OUTPUT_XML, // = 0,
    XSLT_OUTPUT_HTML,
    XSLT_OUTPUT_TEXT
  );

  xsltTransformState = (
    XSLT_STATE_OK, // = 0,
    XSLT_STATE_ERROR,
    XSLT_STATE_STOPPED
  );

  xsltTransformContext = record
    style: xsltStylesheetPtr;                    (* the stylesheet used *)
    type_: xsltOutputType;                       (* the type of output *)
    templ: xsltTemplatePtr;                      (* the current template *)
    templNr: Longint;                            (* Nb of templates in the stack *)
    templMax: Longint;                           (* Size of the templtes stack *)
    templTab: xsltTemplatePtr;                   (* the template stack *)
    vars: xsltStackElemPtr;                      (* the current variable list *)
    varsNr: Longint;                             (* Nb of variable list in the stack *)
    varsMax: Longint;                            (* Size of the variable list stack *)
    varsTab: xsltStackElemPtr;                   (* the variable list stack *)
    varsBase: Longint;                           (* the var base for current templ *)
    (*
     * Extensions
     *)
    extFunctions: xmlHashTablePtr;               (* the extension functions *)
    extElements: xmlHashTablePtr;                (* the extension elements *)
    extInfos: xmlHashTablePtr;                   (* the extension data *)
    mode: PxmlChar;                              (* the current mode *)
    modeURI: PxmlChar;                           (* the current mode URI *)
    docList: xsltDocumentPtr;                    (* the document list *)
    document: xsltDocumentPtr;                   (* the current document *)
    node: xmlNodePtr;                            (* the current node being processed *)
    nodeList: xmlNodeSetPtr;                     (* the current node list *)
    (* xmlNodePtr current;			the node *)
    output: xmlDocPtr;                           (* the resulting document *)
    insert: xmlNodePtr;                          (* the insertion node *)
    xpathCtxt: xmlXPathContextPtr;               (* the XPath context *)
    state: xsltTransformState;                   (* the current state *)
    (*
     * Global variables
     *)
    globalVars: xmlHashTablePtr;                 (* the global variables and params *)
    inst: xmlNodePtr;                            (* the instruction in the stylesheet *)
    xinclude: Longint;                           (* should XInclude be processed *)
    outputFile: Pchar;                           (* the output URI if known *)
    profile: Longint;                            (* is this run profiled *)
    prof: Longint;                               (* the current profiled value *)
    profNr: Longint;                             (* Nb of templates in the stack *)
    profMax: Longint;                            (* Size of the templtaes stack *)
    profTab: PLongint;                           (* the profile template stack *)
    _private: Pointer;                           (* user defined data *)

    extrasNr: Longint;		                       (* the number of extras used *)
    extrasMax: Longint;                          (* the number of extras allocated *)
    extras: xsltRuntimeExtraPtr;                 (* extra per runtime informations *)

    styleList: xsltDocumentPtr;                  (* the stylesheet docs list *)
    sec: Pointer;                                (* the security preferences if any *)

    error: xmlGenericErrorFunc;		               (* a specific error handler *)
    errctx: Pointer;                             (* context for the error handler *)

    sortfunc: xsltSortFunc;                      (* a ctxt specific sort routine *) 
  end;

(**
 * CHECK_STOPPED:
 *
 * Macro to check if the XSLT processing should be stopped.
 * Will return from the function.
 *)
{-macro-
#define CHECK_STOPPED if (ctxt->state == XSLT_STATE_STOPPED) return;
}

(**
 * CHECK_STOPPEDE:
 *
 * Macro to check if the XSLT processing should be stopped.
 * Will goto the error: label.
 *)
{-macro-
#define CHECK_STOPPEDE if (ctxt->state == XSLT_STATE_STOPPED) goto error;
}

(**
 * CHECK_STOPPED0:
 *
 * Macro to check if the XSLT processing should be stopped.
 * Will return from the function with a 0 value.
 *)
{-macro-
#define CHECK_STOPPED0 if (ctxt->state == XSLT_STATE_STOPPED) return(0);
}

(*
 * Functions associated to the internal types
xsltDecimalFormatPtr	xsltDecimalFormatGetByName(xsltStylesheetPtr sheet,
						   xmlChar *name);
 *)
function  xsltNewStylesheet: xsltStylesheetPtr; cdecl; external LIBXSLT_SO;
function  xsltParseStylesheetFile(filename: PxmlChar): xsltStylesheetPtr; cdecl; external LIBXSLT_SO;
procedure xsltFreeStylesheet(sheet: xsltStylesheetPtr); cdecl; external LIBXSLT_SO;
function  xsltIsBlank(str: PxmlChar): Longint; cdecl; external LIBXSLT_SO;
procedure xsltFreeStackElemList(elem: xsltStackElemPtr); cdecl; external LIBXSLT_SO;
function  xsltDecimalFormatGetByName(sheet: xsltStylesheetPtr; name: PxmlChar): xsltDecimalFormatPtr; cdecl; external LIBXSLT_SO;
function  xsltParseStylesheetProcess(ret: xsltStylesheetPtr; doc: xmlDocPtr): xsltStylesheetPtr; cdecl; external LIBXSLT_SO;
procedure xsltParseStylesheetOutput(style: xsltStylesheetPtr; cur: xmlNodePtr); cdecl; external LIBXSLT_SO;
function  xsltParseStylesheetDoc(doc: xmlDocPtr): xsltStylesheetPtr; cdecl; external LIBXSLT_SO;
function xsltParseStylesheetImportedDoc(doc: xmlDocPtr): xsltStylesheetPtr; cdecl; external LIBXSLT_SO;
function  xsltLoadStylesheetPI(doc: xmlDocPtr): xsltStylesheetPtr; cdecl; external LIBXSLT_SO;
procedure xsltNumberFormat(ctxt: xsltTransformContextPtr; data: xsltNumberDataPtr; node: xmlNodePtr); cdecl; external LIBXSLT_SO;
function  xsltFormatNumberConversion(self: xsltDecimalFormatPtr; format: PxmlChar; number: double; result: PPxmlChar): TxmlXPathError; cdecl; external LIBXSLT_SO;
procedure xsltParseTemplateContent(style: xsltStylesheetPtr; templ: xmlNodePtr); cdecl; external LIBXSLT_SO;
function  xsltAllocateExtra(style: xsltStylesheetPtr): Longint; cdecl; external LIBXSLT_SO;
function  xsltAllocateExtraCtxt	(ctxt: xsltTransformContextPtr): Longint; cdecl; external LIBXSLT_SO;

