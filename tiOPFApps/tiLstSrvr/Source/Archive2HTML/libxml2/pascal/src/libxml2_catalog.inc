// CVS-REV:gnome-xml/include/libxml/catalog.h:1.20:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(**
 * catalog.h: interfaces of the Catalog handling system
 *
 * Reference:  SGML Open Technical Resolution TR9401:1997.
 *             http://www.jclark.com/sp/catalog.htm
 *
 *             XML Catalogs Working Draft 12 Jun 2001
 *             http://www.oasis-open.org/committees/entity/spec-2001-06-12.html
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

const
  (**
   * XML_CATALOGS_NAMESPACE:
   *
   * The namespace for the XML Catalogs elements.
   *)
  XML_CATALOGS_NAMESPACE = 'urn:oasis:names:tc:entity:xmlns:xml:catalog';
  (**
   * XML_CATALOG_PI:
   *
   * the specific XML Catalog Processing Instuction name.
   *)
  XML_CATALOG_PI = 'oasis-xml-catalog';

  (*
   * The API is voluntarily limited to general cataloging.
   *)
type
  xmlCatalogPrefer = Longint;
const
  XML_CATA_PREFER_NONE   = 0;
  XML_CATA_PREFER_PUBLIC = 1;
  XML_CATA_PREFER_SYSTEM = 2;

type
  xmlCatalogAllow = Longint;
const
  XML_CATA_ALLOW_NONE     = 0;
  XML_CATA_ALLOW_GLOBAL   = 1;
  XML_CATA_ALLOW_DOCUMENT = 2;
  XML_CATA_ALLOW_ALL      = 3;

type
  xmlCatalog = Pointer;
  xmlCatalogPtr = ^xmlCatalog;

(*
 * Operations on a given catalog.
 *)
function  xmlNewCatalog(sgml: Longint): xmlCatalogPtr; cdecl; external LIBXML2_SO;
function  xmlLoadACatalog(filename: PChar): xmlCatalogPtr; cdecl; external LIBXML2_SO;
function  xmlLoadSGMLSuperCatalog(filename: PChar): xmlCatalogPtr; cdecl; external LIBXML2_SO;
function  xmlConvertSGMLCatalog(catal: xmlCatalogPtr): Longint; cdecl; external LIBXML2_SO;
function  xmlACatalogAdd(catal: xmlCatalogPtr; _type: PxmlChar; orig: PxmlChar; replace: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlACatalogRemove(catal: xmlCatalogPtr; value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlACatalogResolve(catal: xmlCatalogPtr; pubID: PxmlChar; sysID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlACatalogResolveSystem(catal: xmlCatalogPtr; sysID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlACatalogResolvePublic(catal: xmlCatalogPtr; pubID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlACatalogResolveURI(catal: xmlCatalogPtr; URI: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
procedure xmlACatalogDump(catal: xmlCatalogPtr; outfile: PLibXml2File); cdecl; external LIBXML2_SO;
procedure xmlFreeCatalog(catal: xmlCatalogPtr); cdecl; external LIBXML2_SO;
function  xmlCatalogIsEmpty(catal: xmlCatalogPtr): Longint; cdecl; external LIBXML2_SO;

(*
 * Global operations.
 *)
procedure xmlInitializeCatalog; cdecl; external LIBXML2_SO;
function  xmlLoadCatalog(filename: PChar): Longint; cdecl; external LIBXML2_SO;
procedure xmlLoadCatalogs(paths: PChar); cdecl; external LIBXML2_SO;
procedure xmlCatalogCleanup; cdecl; external LIBXML2_SO;
procedure xmlCatalogDump(outfile: PLibXml2File); cdecl; external LIBXML2_SO;
function  xmlCatalogResolve(pubID: PxmlChar; sysID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlCatalogResolveSystem(sysID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlCatalogResolvePublic(pubID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlCatalogResolveURI(URI: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlCatalogAdd(_type: PxmlChar; orig: PxmlChar; replace: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlCatalogRemove(value: PxmlChar): Longint; cdecl; external LIBXML2_SO;
function  xmlParseCatalogFile(filename: PChar): xmlDocPtr; cdecl; external LIBXML2_SO;
function  xmlCatalogConvert: Longint; cdecl; external LIBXML2_SO;

(*
 * Strictly minimal interfaces for per-document catalogs used
 * by the parser.
 *)
procedure xmlCatalogFreeLocal(catalogs: Pointer); cdecl; external LIBXML2_SO;
function  xmlCatalogAddLocal(catalogs: Pointer; URL: PxmlChar): Pointer; cdecl; external LIBXML2_SO;
function  xmlCatalogLocalResolve(catalogs: Pointer; pubID: PxmlChar; sysID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlCatalogLocalResolveURI(catalogs: Pointer; URI: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;

(*
 * Preference settings.
 *)
function  xmlCatalogSetDebug(level: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlCatalogSetDefaultPrefer(prefer: xmlCatalogPrefer): xmlCatalogPrefer; cdecl; external LIBXML2_SO;
procedure xmlCatalogSetDefaults(allow: xmlCatalogAllow); cdecl; external LIBXML2_SO;
function  xmlCatalogGetDefaults: xmlCatalogAllow; cdecl; external LIBXML2_SO;

(* DEPRECATED interfaces *)
function  xmlCatalogGetSystem(sysID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO; {$IFDEF HAS_DEPR} deprecated; {$ENDIF}
function  xmlCatalogGetPublic(pubID: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO; {$IFDEF HAS_DEPR} deprecated; {$ENDIF}

