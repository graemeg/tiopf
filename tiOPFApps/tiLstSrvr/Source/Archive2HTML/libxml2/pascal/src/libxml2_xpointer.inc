// CVS-REV:gnome-xml/include/libxml/xpointer.h:1.10:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * xpointer.h : API to handle XML Pointers
 *
 * World Wide Web Consortium Working Draft 03-March-1998
 * http://www.w3.org/TR/1998/WD-xptr-19980303
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  (*
   * A Location Set
   *)
  xmlLocationSetPtr = ^xmlLocationSet;
  xmlLocationSet = record
    locNr: Longint;                 { number of locations in the set  }
    locMax: Longint;                { size of the array as allocated  }
    locTab: PxmlXPathObjectPtr;     { array of locations  }
  end;

(*
 * Handling of location sets.
 *)
function  xmlXPtrLocationSetCreate(val: xmlXPathObjectPtr): xmlLocationSetPtr; cdecl; external LIBXML2_SO;
procedure xmlXPtrFreeLocationSet(obj: xmlLocationSetPtr); cdecl; external LIBXML2_SO;
function  xmlXPtrLocationSetMerge(val1: xmlLocationSetPtr; val2: xmlLocationSetPtr): xmlLocationSetPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewRange(start: xmlNodePtr; startindex: Longint; end_: xmlNodePtr; endindex: Longint): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewRangePoints(start: xmlXPathObjectPtr; end_: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewRangeNodePoint(start: xmlNodePtr; end_: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewRangePointNode(start: xmlXPathObjectPtr; end_: xmlNodePtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewRangeNodes(start: xmlNodePtr; end_: xmlNodePtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewLocationSetNodes(start: xmlNodePtr; end_: xmlNodePtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewLocationSetNodeSet(set_: xmlNodeSetPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewRangeNodeObject(start: xmlNodePtr; end_: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrNewCollapsedRange(start: xmlNodePtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
procedure xmlXPtrLocationSetAdd(cur: xmlLocationSetPtr; val: xmlXPathObjectPtr); cdecl; external LIBXML2_SO;
function  xmlXPtrWrapLocationSet(val: xmlLocationSetPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
procedure xmlXPtrLocationSetDel(cur: xmlLocationSetPtr; val: xmlXPathObjectPtr); cdecl; external LIBXML2_SO;
procedure xmlXPtrLocationSetRemove(cur: xmlLocationSetPtr; val: Longint); cdecl; external LIBXML2_SO;

(*
 * Functions.
 *)
function  xmlXPtrNewContext(doc: xmlDocPtr; here: xmlNodePtr; origin: xmlNodePtr): xmlXPathContextPtr; cdecl; external LIBXML2_SO;
function  xmlXPtrEval(str: PxmlChar; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl; external LIBXML2_SO;
procedure xmlXPtrRangeToFunction(ctxt: xmlXPathParserContextPtr; nargs: Longint); cdecl; external LIBXML2_SO;
function  xmlXPtrBuildNodeList(obj: xmlXPathObjectPtr): xmlNodePtr; cdecl; external LIBXML2_SO;
procedure xmlXPtrEvalRangePredicate(ctxt: xmlXPathParserContextPtr); cdecl; external LIBXML2_SO;

