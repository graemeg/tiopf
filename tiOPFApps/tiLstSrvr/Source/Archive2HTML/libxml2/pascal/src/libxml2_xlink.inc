// CVS-REV:gnome-xml/include/libxml/xlink.h:1.12:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * xlink.h : interfaces to the hyperlinks detection module
 *
 * See Copyright for the status of this software.
 *
 * Related specification: http://www.w3.org/TR/xlink
 *                        http://www.w3.org/HTML/
 *     and XBase
 *
 * daniel@veillard.com
 *)

(**
 * Various defines for the various Link properties.
 *
 * NOTE: the link detection layer will try to resolve QName expansion
 *       of namespaces. If "foo" is the prefix for "http://foo.com/"
 *       then the link detection layer will expand role="foo:myrole"
 *       to "http://foo.com/:myrole".
 * NOTE: the link detection layer will expand URI-Refences found on
 *       href attributes by using the base mechanism if found.
 *)

type
  xlinkHRef = PxmlChar;
  xlinkRole = PxmlChar;
  xlinkTitle = PxmlChar;

  xlinkType = Longint;
const
  XLINK_TYPE_NONE         = 0;
  XLINK_TYPE_SIMPLE       = 1;
  XLINK_TYPE_EXTENDED     = 2;
  XLINK_TYPE_EXTENDED_SET = 3;

type
  xlinkShow = Longint;
const
  XLINK_SHOW_NONE    = 0;
  XLINK_SHOW_NEW     = 1;
  XLINK_SHOW_EMBED   = 2;
  XLINK_SHOW_REPLACE = 3;

type
  xlinkActuate = Longint;
const
  XLINK_ACTUATE_NONE      = 0;
  XLINK_ACTUATE_AUTO      = 1;
  XLINK_ACTUATE_ONREQUEST = 2;

type
  (**
   * xlinkNodeDetectFunc:
   * @ctx:  user data pointer
   * @node:  the node to check
   *
   * This is the prototype for the link detection routine.
   * It calls the default link detection callbacks upon link detection.
   *)
  xlinkNodeDetectFunc = procedure(ctx: Pointer; node: xmlNodePtr); cdecl;

  (**
   * The link detection module interact with the upper layers using
   * a set of callback registered at parsing time.
   *)

  (**
   * xlinkSimpleLinkFunk:
   * @ctx:  user data pointer
   * @node:  the node carrying the link
   * @href:  the target of the link
   * @role:  the role string
   * @title:  the link title
   *
   * This is the prototype for a simple link detection callback.
   *)
  xlinkSimpleLinkFunk = procedure(ctx: Pointer; node: xmlNodePtr; href: xlinkHRef; role: xlinkRole; title: xlinkTitle); cdecl;
  (**
   * xlinkExtendedLinkFunk:
   * @ctx:  user data pointer
   * @node:  the node carrying the link
   * @nbLocators: the number of locators detected on the link
   * @hrefs:  pointer to the array of locator hrefs
   * @roles:  pointer to the array of locator roles
   * @nbArcs: the number of arcs detected on the link
   * @from:  pointer to the array of source roles found on the arcs
   * @to:  pointer to the array of target roles found on the arcs
   * @show:  array of values for the show attributes found on the arcs
   * @actuate:  array of values for the actuate attributes found on the arcs
   * @nbTitles: the number of titles detected on the link
   * @title:  array of titles detected on the link
   * @langs:  array of xml:lang values for the titles
   *
   * This is the prototype for a extended link detection callback.
   *)
  xlinkExtendedLinkFunk = procedure(ctx: Pointer; node: xmlNodePtr; nbLocators: Integer; var hrefs: xlinkHRef; var roles: xlinkRole; nbArcs: Integer; var aFrom, aTo: xlinkRole; var show: xlinkShow; var actuate: xlinkActuate; nbTitles: Integer; var titles: xlinkTitle; var langs: PxmlChar); cdecl;
  (**
   * xlinkExtendedLinkSetFunk:
   * @ctx:  user data pointer
   * @node:  the node carrying the link
   * @nbLocators: the number of locators detected on the link
   * @hrefs:  pointer to the array of locator hrefs
   * @roles:  pointer to the array of locator roles
   * @nbTitles: the number of titles detected on the link
   * @title:  array of titles detected on the link
   * @langs:  array of xml:lang values for the titles
   *
   * This is the prototype for a extended link set detection callback.
   *)
  xlinkExtendedLinkSetFunk = procedure(ctx: Pointer; node: xmlNodePtr; nbLocators: Integer; var hrefs: xlinkHRef; var roles: xlinkRole; nbTitles: Integer; var titles: xlinkTitle; var langs: PxmlChar); cdecl;
  (**
   * This is the structure containing a set of Links detection callbacks.lt detection routine, can be overridden, th
   *
   * There is no default xlink callbacks, if one want to get link
   * recognition activated, those call backs must be provided before parsing.
   *)
  xlinkHandlerPtr = ^xlinkHandler;
  xlinkHandler = record
    simple: xlinkSimpleLinkFunk;
    extended: xlinkExtendedLinkFunk;
    set_: xlinkExtendedLinkSetFunk;
  end;

(*
 * The default detection routine, can be overridden, they call the default
 * detection callbacks.
 *)
function  xlinkGetDefaultDetect: xlinkNodeDetectFunc; cdecl; external LIBXML2_SO;
procedure xlinkSetDefaultDetect(func: xlinkNodeDetectFunc); cdecl; external LIBXML2_SO;

(*
 * Routines to set/get the default handlers.
 *)
function  xlinkGetDefaultHandler: xlinkHandlerPtr; cdecl; external LIBXML2_SO;
procedure xlinkSetDefaultHandler(handler: xlinkHandlerPtr); cdecl; external LIBXML2_SO;

(*
 * Link detection module itself.
 *)
function  xlinkIsLink(doc: xmlDocPtr; node: xmlNodePtr): xlinkType; cdecl; external LIBXML2_SO;

