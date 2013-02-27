// CVS-REV:gnome-xml/include/libxml/xmlautomata.h:1.7:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * automata.h: description of the API to build regexp automats
 *
 * See Copyright for the status of this software.
 *
 * Daniel Veillard <veillard@redhat.com>
 *)

type
  (* 
   * xmlAutomataPtr: 
   * 
   * A libxml automata description, It can be compiled into a regexp
   *)
  xmlAutomata = record end;
  xmlAutomataPtr = ^xmlAutomata;

  (*
   * xmlAutomataStatePtr:
   *
   * A state int the automata description,
   *)
  xmlAutomataState = record end;
  xmlAutomataStatePtr = ^xmlAutomataState;

(*
 * Building API
 *)
function  xmlNewAutomata: xmlAutomataPtr; cdecl; external LIBXML2_SO;
procedure xmlFreeAutomata(am: xmlAutomataPtr); cdecl; external LIBXML2_SO;
function  xmlAutomataGetInitState(am: xmlAutomataPtr): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataSetFinalState(am: xmlAutomataPtr; state: xmlAutomataStatePtr): Longint; cdecl; external LIBXML2_SO;
function  xmlAutomataNewState(am: xmlAutomataPtr): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataNewTransition(am: xmlAutomataPtr; aFrom, aTo: xmlAutomataStatePtr; token: PxmlChar; data: Pointer): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataNewCountTrans(am: xmlAutomataPtr; aFrom, aTo: xmlAutomataStatePtr; token: PxmlChar; min, max: Longint; data: Pointer): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataNewOnceTrans(am: xmlAutomataPtr; aFrom, aTo: xmlAutomataStatePtr; token: PxmlChar; min, max: Longint; data: Pointer): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataNewAllTrans(am: xmlAutomataPtr; aFrom, aTo: xmlAutomataStatePtr; lax: Longint): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataNewEpsilon(am: xmlAutomataPtr; aFrom, aTo: xmlAutomataStatePtr): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataNewCountedTrans(am: xmlAutomataPtr; aFrom, aTo: xmlAutomataStatePtr; aCounter: Longint): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataNewCounterTrans(am: xmlAutomataPtr; aFrom, aTo: xmlAutomataStatePtr; aCounter: Longint): xmlAutomataStatePtr; cdecl; external LIBXML2_SO;
function  xmlAutomataNewCounter(am: xmlAutomataPtr; min, max: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlAutomataCompile(am: xmlAutomataPtr): xmlRegexpPtr; cdecl; external LIBXML2_SO;
function  xmlAutomataIsDeterminist(am: xmlAutomataPtr): Longint; cdecl; external LIBXML2_SO;

