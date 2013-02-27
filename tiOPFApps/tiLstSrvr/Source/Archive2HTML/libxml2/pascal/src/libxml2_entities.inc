// CVS-REV:gnome-xml/include/libxml/entities.h:1.36:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * entities.h : interface for the XML entities handling
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  xmlEntityType = Longint;
const
  (*
   * The different valid entity types.
   *)
  XML_INTERNAL_GENERAL_ENTITY          = 1;
  XML_EXTERNAL_GENERAL_PARSED_ENTITY   = 2;
  XML_EXTERNAL_GENERAL_UNPARSED_ENTITY = 3;
  XML_INTERNAL_PARAMETER_ENTITY        = 4;
  XML_EXTERNAL_PARAMETER_ENTITY        = 5;
  XML_INTERNAL_PREDEFINED_ENTITY       = 6;

type
  (*
   * An unit of storage for an entity, contains the string, the value
   * and the linking data needed for the linking in the hash table.
   *)
  xmlEntityPtr = ^xmlEntity; //[pk] cannot be moved to tree.inc
  xmlEntity = record
    _private : pointer;             (* application data  *)
    type_ : xmlElementType;         (* XML_ENTITY_DECL, must be second !  *)
    name : PxmlChar;                (* Entity name  *)
    children : ^xmlNode;            (* First child link *)
    last : ^xmlNode;                (* Last child link *)
    parent : ^xmlDtd;               (* -> DTD  *)
    next : ^xmlNode;                (* next sibling link   *)
    prev : ^xmlNode;                (* previous sibling link   *)
    doc : ^xmlDoc;                  (* the containing document  *)
    orig : PxmlChar;                (* content without ref substitution  *)
    content : PxmlChar;             (* content or ndata if unparsed  *)
    length : longint;               (* the content length  *)
    etype : xmlEntityType;          (* The entity type  *)
    ExternalID : PxmlChar;          (* External identifier for PUBLIC  *)
    SystemID : PxmlChar;            (* URI for a SYSTEM or PUBLIC Entity  *)
    nexte : xmlEntityPtr;           (* unused  *)
    URI : PxmlChar;                 (* the full URI as computed  *)
    owner : Longint;                (* does the entity own the children *)
  end;

  (*
   * All entities are stored in an hash table.
   * there is 2 separate hash tables for global and parameter entities.
   *)
  xmlEntitiesTable = xmlHashTable;
  xmlEntitiesTablePtr = ^xmlEntitiesTable;

(*
 * External functions:
 *)
procedure xmlInitializePredefinedEntities; cdecl; external LIBXML2_SO;
function  xmlAddDocEntity(doc: xmlDocPtr; name: PxmlChar; aType: Longint; ExternalID: PxmlChar; SystemID: PxmlChar; content: PxmlChar): xmlEntityPtr; cdecl; external LIBXML2_SO;
function  xmlAddDtdEntity(doc: xmlDocPtr; name: PxmlChar; aType: Longint; ExternalID: PxmlChar; SystemID: PxmlChar; content: PxmlChar): xmlEntityPtr; cdecl; external LIBXML2_SO;
function  xmlGetPredefinedEntity(name: PxmlChar): xmlEntityPtr; cdecl; external LIBXML2_SO;
function  xmlGetDocEntity(doc: xmlDocPtr; name: PxmlChar): xmlEntityPtr; cdecl; external LIBXML2_SO;
function  xmlGetDtdEntity(doc: xmlDocPtr; name: PxmlChar): xmlEntityPtr; cdecl; external LIBXML2_SO;
function  xmlGetParameterEntity(doc: xmlDocPtr; name: PxmlChar): xmlEntityPtr; cdecl; external LIBXML2_SO;
function  xmlEncodeEntities(doc: xmlDocPtr; input: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlEncodeEntitiesReentrant(doc: xmlDocPtr; input: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlEncodeSpecialChars(doc: xmlDocPtr; input: PxmlChar): PxmlChar; cdecl; external LIBXML2_SO;
function  xmlCreateEntitiesTable: xmlEntitiesTablePtr; cdecl; external LIBXML2_SO;
function  xmlCopyEntitiesTable(table: xmlEntitiesTablePtr): xmlEntitiesTablePtr; cdecl; external LIBXML2_SO;
procedure xmlFreeEntitiesTable(table: xmlEntitiesTablePtr); cdecl; external LIBXML2_SO;
procedure xmlDumpEntitiesTable(buf: xmlBufferPtr; table: xmlEntitiesTablePtr); cdecl; external LIBXML2_SO;
procedure xmlDumpEntityDecl(buf: xmlBufferPtr; ent: xmlEntityPtr); cdecl; external LIBXML2_SO;
procedure xmlCleanupPredefinedEntities; cdecl; external LIBXML2_SO;

