unit libxml2;
{
  ------------------------------------------------------------------------------
  This unit collects all the translated headers of libxml2 (aka gnome-xml).
  2001-2002 (C) Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
  Project site:
    http://sourceforge.net/projects/libxml2-pas
  See also:
    http://www.xmlsoft.org  - the libxml2 homepage
    http://kozelka.hyperlink.cz  - my homepage
}

interface

uses
{$IFDEF WIN32}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
{
  Unit "libc" does not need to be used explicitly as it is part of the System unit.
  And, it is better to NOT present it here, because otherwise it would not be
  possible to compile the unit standalone or in a package.
}
{$ENDIF}
  iconv;
const
{$IFDEF WIN32}
  LIBXML2_SO = 'libxml2.dll';
{$ENDIF}
{$IFDEF LINUX}
  LIBXML2_SO = 'libxml2.so';
{$ENDIF}

{$I pasconfig.inc}

type
  DWORD = Cardinal;
  PLongint = ^Longint;
  PByte = ^Byte;
  PPChar = ^PChar;
  PLibXml2File = Pointer; //placeholder for 'FILE *' C-type
  xmlChar = Char;
  PxmlChar = PChar;
  PPxmlChar = ^PxmlChar;

{$DEFINE LIBXML_THREAD_ALLOC_ENABLED}
{$DEFINE LIBXML_THREAD_ENABLED}
{$DEFINE LIBXML_HTML_ENABLED}
{$DEFINE LIBXML_DOCB_ENABLED}
{$DEFINE LIBXML_REGEXP_ENABLED}

{$I libxml2_xmlwin32version.inc}

{$I libxml2_xmlmemory.inc}
{$I libxml2_xmlregexp.inc}
{$I libxml2_tree.inc}
{$I libxml2_encoding.inc}
{$I libxml2_xmlIO.inc}
{$I libxml2_hash.inc}
{$I libxml2_entities.inc}
{$I libxml2_list.inc}
{$I libxml2_xmlautomata.inc}
{$I libxml2_valid.inc}
{$I libxml2_parser.inc}
{$I libxml2_SAX.inc}
{$I libxml2_xpath.inc}
{$I libxml2_xpathInternals.inc}
{$I libxml2_xpointer.inc}
{$I libxml2_xmlerror.inc}
{$I libxml2_xlink.inc}
{$I libxml2_xinclude.inc}
{$I libxml2_debugXML.inc}
{$I libxml2_nanoftp.inc}
{$I libxml2_nanohttp.inc}
{$I libxml2_uri.inc}
{$I libxml2_HTMLparser.inc}
{$I libxml2_HTMLtree.inc}
{$I libxml2_parserInternals.inc}
{$I libxml2_DOCBparser.inc}
{$I libxml2_catalog.inc}
{$I libxml2_globals.inc}
{$I libxml2_threads.inc}
{$I libxml2_c14n.inc}

{$I libxml2_xmlunicode.inc}
{$I libxml2_schemasInternals.inc}
{$I libxml2_xmlschemastypes.inc}
{$I libxml2_xmlschemas.inc}
{$I libxml2_xmlreader.inc}
{$I libxml2_relaxng.inc}

// functions existing only in some compilers

{$IFNDEF HAS_STRNEXTCHAR_SUPPORT}
function StrNextChar(p: PChar): PChar;
{$ENDIF}

// functions that reference symbols defined later - by header file:

// tree.h
function  xmlSaveFileTo(buf: xmlOutputBufferPtr; cur: xmlDocPtr; encoding: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlSaveFormatFileTo(buf: xmlOutputBufferPtr; cur: xmlDocPtr; encoding: PChar; format: Longint): Longint; cdecl; external LIBXML2_SO;
procedure xmlNodeDumpOutput(buf: xmlOutputBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; level: Longint; format: Longint; encoding: PChar); cdecl; external LIBXML2_SO;

// xmlIO.h
function xmlNoNetExternalEntityLoader(URL: PChar; ID: PChar; ctxt: xmlParserCtxtPtr): xmlParserInputPtr; cdecl; external LIBXML2_SO;

procedure xmlFree(str: PxmlChar); cdecl;
{$IFNDEF USE_PASCAL_MM}
function xmlMalloc(size: size_t): Pointer; cdecl;
function xmlRealloc(ptr: Pointer; size: size_t): Pointer; cdecl;
function xmlMemStrdup(str: PChar): PChar; cdecl;
{$ENDIF}

implementation

uses
  SysUtils;

{$IFDEF USE_PASCAL_MM}
procedure xmlFree(str: PxmlChar); cdecl;
begin
  FreeMem(PChar(str)); //hopefully works under Kylix
end;
{$ELSE}
var
  libHandle: Cardinal;
  pXmlFree: pXmlFreeFunc;
  pXmlMalloc: pXmlMallocFunc;
  pXmlRealloc: pXmlReallocFunc;
  pXmlMemStrdup: pXmlStrdupFunc;

procedure xmlFree(str: PxmlChar); cdecl;
begin
  pXmlFree^(str);
end;

function xmlMalloc(size: size_t): Pointer; cdecl;
begin
  Result := pXmlMalloc^(size);
end;

function xmlRealloc(ptr: Pointer; size: size_t): Pointer; cdecl;
begin
  Result := pXmlRealloc^(ptr, size);
end;

function xmlMemStrdup(str: PChar): PChar; cdecl;
begin
  Result := pXmlMemStrdup^(str);
end;
{$ENDIF}

// macros from xpath.h

function xmlXPathNodeSetGetLength(ns: xmlNodeSetPtr): Integer;
begin
  if ns = nil then begin
    Result := 0;
  end else begin
    Result := ns^.nodeNr;
  end;
end;

function xmlXPathNodeSetItem(ns: xmlNodeSetPtr; index: Integer): xmlNodePtr;
var
  p: PxmlNodePtr;
begin
  Result := nil;
  if ns = nil then exit;
  if index < 0 then exit;
  if index >= ns^.nodeNr then exit;
  p := ns^.nodeTab;
  Inc(p, index);
  Result := p^;
end;

function xmlXPathNodeSetIsEmpty(ns: xmlNodeSetPtr): Boolean;
begin
  Result := ((ns = nil) or (ns^.nodeNr = 0) or (ns^.nodeTab = nil));
end;

// macros from parserInternals

procedure SKIP_EOL(var p: PxmlChar);
begin
  if (p^ = #13) then begin
    Inc(p);
    if (p^ = #10) then Inc(p);
  end;
  if (p^ = #10) then begin
    Inc(p);
    if (p^ = #13) then Inc(p);
  end;
end;

procedure MOVETO_ENDTAG(var p: PxmlChar);
begin
  while ((p^ <> #0) and (p^ <> '>')) do begin
    p := StrNextChar(p);
  end;
end;

procedure MOVETO_STARTTAG(var p: PxmlChar);
begin
  while ((p^ <> #0) and (p^ <> '<')) do begin
    p := StrNextChar(p);
  end;
end;

{$IFDEF USE_PASCAL_MM}
// Pascal memory handling
procedure PascalFreeFunc(ptr: Pointer); cdecl;
begin
  FreeMem(ptr);
end;

function PascalMallocFunc(size: size_t): Pointer; cdecl;
begin
  Result := AllocMem(size);
end;

function PascalReallocFunc(ptr: Pointer; size: size_t): Pointer; cdecl;
begin
  Result := ReallocMemory(ptr, size);
end;

function PascalStrdupFunc(str: PChar): PChar; cdecl;
var
  sz: Integer;
begin
  sz := StrLen(str);
  Result := AllocMem(sz + 1);
  Move(str^, Result^, sz);
end;
{$ENDIF}

{$IFNDEF HAS_STRNEXTCHAR_SUPPORT}
function StrNextChar(p: PChar): PChar;
begin
  Result := p + 1;
end;
{$ENDIF}

initialization
{$IFDEF USE_PASCAL_MM}
  // setup Pascal memory handler
  xmlMemSetup(@PascalFreeFunc, @PascalMallocFunc, @PascalReallocFunc, @PascalStrdupFunc);
{$ELSE}
  // The Delphi 'external' directive can only be used for functions and procedures,
  // but here we need to obtain the addresses of POINTERS to functions. We can
  // get to these addresses (and also those of other data values exported from
  // the DLL) by using GetProcAddress.
  libHandle := LoadLibrary(LIBXML2_SO);
  if libHandle <> 0 then begin
    pXmlFree := pXmlFreeFunc(GetProcAddress(libHandle, 'xmlFree'));
    pXmlMalloc := pXmlMallocFunc(GetProcAddress(libHandle, 'xmlMalloc'));
    pXmlRealloc := pXmlReallocFunc(GetProcAddress(libHandle, 'xmlRealloc'));
    pXmlMemStrdup := pXmlStrdupFunc(GetProcAddress(libHandle, 'xmlMemStrdup'));
    FreeLibrary(libHandle);
  end;
{$ENDIF}
end.

