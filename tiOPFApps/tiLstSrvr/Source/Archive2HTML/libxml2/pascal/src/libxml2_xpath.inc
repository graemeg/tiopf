// CVS-REV:gnome-xml/include/libxml/xpath.h:1.54:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * xpath.c: interface for XML Path Language implementation
 *
 * Reference: W3C Working Draft 5 July 1999
 *            http://www.w3.org/Style/XSL/Group/1999/07/xpath-19990705.html
 *
 * See COPYRIGHT for the status of this software
 *
 * Author: daniel@veillard.com
 *)

type
  TxmlXPathError = Longint; //[pk]: prefixed with 'T' to avoid name collision with function xmlXPatherror
  xmlXPathObjectType = Longint;

const
  (**
   * The set of XPath error codes.
   *)
  XPATH_EXPRESSION_OK            = 0;
  XPATH_NUMBER_ERROR             = 1;
  XPATH_UNFINISHED_LITERAL_ERROR = 2;
  XPATH_START_LITERAL_ERROR      = 3;
  XPATH_VARIABLE_REF_ERROR       = 4;
  XPATH_UNDEF_VARIABLE_ERROR     = 5;
  XPATH_INVALID_PREDICATE_ERROR  = 6;
  XPATH_EXPR_ERROR               = 7;
  XPATH_UNCLOSED_ERROR           = 8;
  XPATH_UNKNOWN_FUNC_ERROR       = 9;
  XPATH_INVALID_OPERAND          = 10;
  XPATH_INVALID_TYPE             = 11;
  XPATH_INVALID_ARITY            = 12;
  XPATH_INVALID_CTXT_SIZE        = 13;
  XPATH_INVALID_CTXT_POSITION    = 14;
  XPATH_MEMORY_ERROR             = 15;
  XPTR_SYNTAX_ERROR              = 16;
  XPTR_RESOURCE_ERROR            = 17;
  XPTR_SUB_RESOURCE_ERROR        = 18;
  XPATH_UNDEF_PREFIX_ERROR       = 19;
  XPATH_ENCODING_ERROR           = 20;
  XPATH_INVALID_CHAR_ERROR       = 21;

  (*
   * An expression is evaluated to yield an object, which
   * has one of the following four basic types:
   *   - node-set
   *   - boolean
   *   - number
   *   - string
   *
   * @@ XPointer will add more types !
   *)
  XPATH_UNDEFINED   = 0;
  XPATH_NODESET     = 1;
  XPATH_BOOLEAN     = 2;
  XPATH_NUMBER      = 3;
  XPATH_STRING      = 4;
  XPATH_POINT       = 5;
  XPATH_RANGE       = 6;
  XPATH_LOCATIONSET = 7;
  XPATH_USERS       = 8;
  XPATH_XSLT_TREE   = 9;            (* An XSLT value tree, non modifiable *)

type
  xmlXPathParserContextPtr = ^xmlXPathParserContext;
  (*
   * A node-set (an unordered collection of nodes without duplicates).
   *)
  xmlNodeSetPtr = ^xmlNodeSet;
  xmlNodeSet = record
    nodeNr: Longint;                { number of nodes in the set  }
    nodeMax: Longint;               { size of the array as allocated  }
    nodeTab: PxmlNodePtr;           { array of nodes in no particular order  }
    (* @@ with_ns to check whether namespace nodes should be looked at @@ *)
  end;

  PxmlXPathObjectPtr = ^xmlXPathObjectPtr;
  xmlXPathObjectPtr = ^xmlXPathObject;
  xmlXPathObject = record
    type_: xmlXPathObjectType;
    nodesetval: xmlNodeSetPtr;
{$IFDEF LINUX}
    boolval: Longint;
{$ELSE}
    boolval: int64; //??? IS THIS REALLY OK ???
{$ENDIF}
    floatval: Double;
    stringval: PxmlChar;
    user: Pointer;
    index: Longint;
    user2: Pointer;
    index2: Longint;
  end;

  (**
   * xmlXPathConvertFunc:
   * @obj:  an XPath object
   * @type:  the number of the target type
   *
   * A conversion function is associated to a type and used to cast
   * the new type to primitive values.
   *
   * Returns -1 in case of error, 0 otherwise
   *)
  xmlXPathConvertFunc = function(obj: xmlXPathObjectPtr; aType: Integer): Integer; cdecl;

  (*
   * Extra type: a name and a conversion function.
   *)
  xmlXPathTypePtr = ^xmlXPathType;
  xmlXPathType = record
    name: PxmlChar; { the type name  }
    func: xmlXPathConvertFunc; { the conversion function  }
  end;

  (*
   * Extra variable: a name and a value.
   *)
  xmlXPathVariablePtr = ^xmlXPathVariable;
  xmlXPathVariable = record
    name: PxmlChar; { the variable name  }
    value: xmlXPathObjectPtr; { the value  }
  end;

  (**
   * xmlXPathEvalFunc:
   * @ctxt: an XPath parser context
   * @nargs: the number of arguments passed to the function
   *
   * an XPath evaluation function, the parameters are on the XPath context stack.
   *)
  xmlXPathEvalFunc = procedure(ctxt: xmlXPathParserContextPtr; nargs: Integer); cdecl;

  (*
   * Extra function: a name and a evaluation function.
   *)
  xmlXPathFuncPtr = ^xmlXPathFunct;
  xmlXPathFunct = record
    name: PxmlChar; { the function name  }
    func: xmlXPathEvalFunc; { the evaluation function  }
  end;

  (**
   * xmlXPathAxisFunc:
   * @ctxt:  the XPath interpreter context
   * @cur:  the previous node being explored on that axis
   *
   * An axis traversal function. To traverse an axis, the engine calls
   * the first time with cur == NULL and repeat until the function returns
   * NULL indicating the end of the axis traversal.
   *
   * Returns the next node in that axis or NULL if at the end of the axis.
   *)
  xmlXPathAxisFunc = function(ctxt: xmlXPathParserContextPtr; cur: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl;

  (*
   * Extra axis: a name and an axis function.
   *)
  xmlXPathAxisPtr = ^xmlXPathAxis;
  xmlXPathAxis = record
    name: PxmlChar; { the axis name  }
    func: xmlXPathAxisFunc; { the search function  }
  end;

  (**
   * xmlXPathContext:
   *
   * Expression evaluation occurs with respect to a context.
   * he context consists of:
   *    - a node (the context node)
   *    - a node list (the context node list)
   *    - a set of variable bindings
   *    - a function library
   *    - the set of namespace declarations in scope for the expression
   * Following the switch to hash tables, this need to be trimmed up at
   * the next binary incompatible release.
   *)
  xmlXPathContextPtr = ^xmlXPathContext;
  xmlXPathContext = record
    doc: xmlDocPtr;                 { The current document  }
    node: xmlNodePtr;               { The current node  }
    nb_variables_unused: Longint;   { unused (hash table)  }
    max_variables_unused: Longint;  { unused (hash table)  }
    varHash: xmlHashTablePtr;       { Hash table of defined variables  }
    nb_types: Longint;              { number of defined types  }
    max_types: Longint;             { max number of types  }
    types: xmlXPathTypePtr;         { Array of defined types  }
    nb_funcs_unused: Longint;       { unused (hash table)  }
    max_funcs_unused: Longint;      { unused (hash table)  }
    funcHash: xmlHashTablePtr;      { Hash table of defined funcs  }
    nb_axis: Longint;               { number of defined axis  }
    max_axis: Longint;              { max number of axis  }
    axis: xmlXPathAxisPtr;          { Array of defined axis  }

    (* the namespace nodes of the context node *)
    namespaces: PxmlNsPtr; { Array of namespaces  }
    nsNr: Longint;                  { number of namespace in scope  }
    user: Pointer;                  { function to free  }

    (* extra variables *)
    contextSize: Longint;           { the context size  }
    proximityPosition: Longint;     { the proximity position  }

    (* extra stuff for XPointer *)
    xptr: Longint;                  { it this an XPointer context  }
    here: xmlNodePtr;               { for here()  }
    origin: xmlNodePtr;             { for origin()  }

    (* the set of namespace declarations in scope for the expression *)
    nsHash: xmlHashTablePtr;        { The namespaces hash table  }
    varLookupFunc: Pointer;         { variable lookup func  }
    varLookupData: Pointer;         { variable lookup data  }

    (* Possibility to link in an extra item *)
    extra: Pointer;                 { needed for XSLT  }

    (* The function name and URI when calling a function *)
    function_: PxmlChar;
    functionURI: PxmlChar;          { The function name and URI when calling a function  }

    (* function lookup function and data *)
    funcLookupFunc: Pointer;        { function lookup func }
    funcLookupData: Pointer;        { function lookup data }

    (* temporary namespace lists kept for walking the namespace axis *)
    tmpNsList: xmlNsPtr;            { Array of namespaces }
    tmpNsNr: Longint;               { number of namespace in scope }
  end;

  (*
   * The structure of a compiled expression form is not public.
   *)
  xmlXPathCompExpr = record
  end;
  xmlXPathCompExprPtr = ^xmlXPathCompExpr;

  (**
   * xmlXPathParserContext:
   *
   * An XPath parser context. It contains pure parsing informations,
   * an xmlXPathContext, and the stack of objects.
   *)
  xmlXPathParserContext = record
    cur: PxmlChar;                  { the current char being parsed  }
    base: PxmlChar;                 { the full expression  }
    error: Longint;                 { error code  }
    context: xmlXPathContextPtr;    { the evaluation context  }
    value: xmlXPathObjectPtr;       { the current value  }
    valueNr: Longint;               { number of values stacked  }
    valueMax: Longint;              { max number of values stacked  }
    valueTab: PxmlXPathObjectPtr;   { stack of values  }
    comp: xmlXPathCompExprPtr;      { the precompiled expression  }
    xptr: Longint;                  { it this an XPointer expression  }
  end;

  (**
   * xmlXPathFunction:
   * @ctxt:  the XPath interprestation context
   * @nargs:  the number of arguments
   *
   * An XPath function.
   * The arguments (if any) are popped out from the context stack
   * and the result is pushed on the stack.
   *)
  xmlXPathFunction = procedure(ctxt: xmlXPathParserContextPtr; nargs: Integer); cdecl;

(************************************************************************
 *                                                                      *
 *      Public API                                                      *
 *                                                                      *
 ************************************************************************)

(**
 * Objects and Nodesets handling
 *)
(* TODO:
LIBXML_DLL_IMPORT extern Double xmlXPathNAN;
LIBXML_DLL_IMPORT extern Double xmlXPathPINF;
LIBXML_DLL_IMPORT extern Double xmlXPathNINF;
*)

function  xmlXPathIsNaN(val: Double): Longint; cdecl; external LIBXML2_SO;
function  xmlXPathIsInf(val: Double): Longint; cdecl; external LIBXML2_SO;

(* These macros may later turn into functions *)
(**
 * xmlXPathNodeSetGetLength:
 * @ns:  a node-set
 *
 * Implement a functionality similar to the DOM NodeList.length
 *
 * Returns the number of nodes in the node-set.
 *)
function  xmlXPathNodeSetGetLength(ns: xmlNodeSetPtr): Integer; { was macro }

(**
 * xmlXPathNodeSetItem:
 * @ns:  a node-set
 * @index:  index of a node in the set
 *
 * Implements a functionnality similar to the DOM NodeList.item().
 *
 * Returns the xmlNodePtr at the given @index in @ns or NULL if
 *         @index is out of range (0 to length-1)
 *)
function  xmlXPathNodeSetItem(ns: xmlNodeSetPtr; index: Integer): xmlNodePtr; { was macro }

(**
 * xmlXPathNodeSetIsEmpty:
 * @ns: a node-set
 *
 * Checks whether @ns is empty or not.
 *
 * Returns %TRUE if @ns is an empty node-set.
 *)
function  xmlXPathNodeSetIsEmpty(ns: xmlNodeSetPtr): boolean;

procedure xmlXPathFreeObject(obj: xmlXPathObjectPtr); cdecl; external LIBXML2_SO;
function  xmlXPathNodeSetCreate(val: xmlNodePtr): xmlNodeSetPtr; cdecl; external LIBXML2_SO;
procedure xmlXPathFreeNodeSetList(obj: xmlXPathObjectPtr); cdecl; external LIBXML2_SO;
procedure xmlXPathFreeNodeSet(obj: xmlNodeSetPtr); cdecl; external LIBXML2_SO;
function  xmlXPathObjectCopy(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPathCmpNodes(node1: xmlNodePtr; node2: xmlNodePtr): Longint; cdecl; external LIBXML2_SO;

(**
 * Conversion functions to basic types.
 *)
function  xmlXPathCastNumberToBoolean(val: Double): Longint; cdecl; external LIBXML2_SO;
function  xmlXPathCastStringToBoolean(val: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlXPathCastNodeSetToBoolean(ns: xmlNodeSetPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlXPathCastToBoolean(val: xmlXPathObjectPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlXPathCastBooleanToNumber(val: Longint): Double; cdecl; external LIBXML2_SO;
function  xmlXPathCastStringToNumber(val: PxmlChar): Double; cdecl; external LIBXML2_SO;
function  xmlXPathCastNodeToNumber(node: xmlNodePtr): Double; cdecl; external LIBXML2_SO;
function  xmlXPathCastNodeSetToNumber(ns: xmlNodeSetPtr): Double; cdecl; external LIBXML2_SO;
function  xmlXPathCastToNumber(val: xmlXPathObjectPtr): Double; cdecl; external LIBXML2_SO;
function  xmlXPathCastBooleanToString(val: Longint): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlXPathCastNumberToString(val: Double): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlXPathCastNodeToString(node: xmlNodePtr): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlXPathCastNodeSetToString(ns: xmlNodeSetPtr): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlXPathCastToString(val: xmlXPathObjectPtr): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlXPathConvertBoolean(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPathConvertNumber(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPathConvertString(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;

(**
 * Context handling.
 *)
procedure xmlXPathInit; cdecl; external LIBXML2_SO;
function  xmlXPathNewContext(doc: xmlDocPtr): xmlXPathContextPtr; cdecl; external LIBXML2_SO;
procedure xmlXPathFreeContext(ctxt: xmlXPathContextPtr); cdecl; external LIBXML2_SO;

(**
 * Evaluation functions.
 *)
function  xmlXPathEval(str: PxmlChar; ctxt: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPathEvalExpression(str: PxmlChar; ctxt: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPathEvalPredicate(ctxt: xmlXPathContextPtr; res: xmlXPathObjectPtr): Longint; cdecl; external LIBXML2_SO;

(**
 * Separate compilation/evaluation entry points.
 *)
function  xmlXPathCompile(str: PxmlChar): xmlXPathCompExprPtr; cdecl; external LIBXML2_SO;
function  xmlXPathCompiledEval(comp: xmlXPathCompExprPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
procedure xmlXPathFreeCompExpr(comp: xmlXPathCompExprPtr); cdecl; external LIBXML2_SO;

