// CVS-REV:gnome-xml/include/libxml/uri.h:1.14:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(**
 * uri.c: library of generic URI related routines
 *
 * Reference: RFC 2396
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  (**
   * xmlURI:
   *
   * A parsed URI reference. This is a struct containing the various fields
   * as described in RFC 2396 but separated for further processing.
   *)
  xmlURIPtr = ^xmlURI;
  xmlURI = record
    scheme: PChar;                  { the URI scheme  }
    opaque: PChar;                  { opaque part  }
    authority: PChar;               { the authority part  }
    server: PChar;                  { the server part  }
    user: PChar;                    { the user part  }
    port: Longint;                  { the port number  }
    path: PChar;                    { the path string  }
    query: PChar;                   { the query string  }
    fragment: PChar;                { the fragment identifier  }
    cleanup: Longint;               { parsing potentially unclean URI }
  end;

(*
 * This function is in tree.h:
 * xmlChar *  xmlNodeGetBase (xmlDocPtr doc,
 *                            xmlNodePtr cur);
 *)

function  xmlCreateURI: xmlURIPtr; cdecl; external LIBXML2_SO;
function  xmlBuildURI(URI: PxmlChar; base: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlParseURI(str: PChar): xmlURIPtr; cdecl; external LIBXML2_SO;
function  xmlParseURIReference(uri: xmlURIPtr; str: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlSaveUri(uri: xmlURIPtr): PxmlChar; cdecl; external LIBXML2_SO;
procedure xmlPrintURI(stream: PLibXml2File; uri: xmlURIPtr); cdecl; external LIBXML2_SO;
function  xmlURIEscapeStr(str: PxmlChar; list: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlURIUnescapeString(str: PChar; len: Longint; target: PChar): PChar; cdecl; external LIBXML2_SO;
function  xmlNormalizeURIPath(path: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlURIEscape(str: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
procedure xmlFreeURI(uri: xmlURIPtr); cdecl; external LIBXML2_SO;

