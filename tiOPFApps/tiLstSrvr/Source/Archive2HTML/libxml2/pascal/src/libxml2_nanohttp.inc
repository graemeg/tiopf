// CVS-REV:gnome-xml/include/libxml/nanohttp.h:1.13:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * nanohttp.c: minimalist HTTP implementation to fetch external subsets.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)
procedure xmlNanoHTTPInit; cdecl; external LIBXML2_SO;
procedure xmlNanoHTTPCleanup; cdecl; external LIBXML2_SO;
procedure xmlNanoHTTPScanProxy(URL: PChar); cdecl; external LIBXML2_SO;
function  xmlNanoHTTPFetch(URL: PChar; filename: PChar; contentType: PPChar): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoHTTPMethod(URL: PChar; method: PChar; input: PChar; contentType: PPChar; headers: PChar; ilen: Longint): Pointer; cdecl; external LIBXML2_SO;
function  xmlNanoHTTPMethodRedir(URL: PChar; method: PChar; input: PChar; contentType: PPChar; redir: PPChar; headers: PChar; ilen: Longint): Pointer; cdecl; external LIBXML2_SO;
function  xmlNanoHTTPOpen(URL: PChar; contentType: PPChar): Pointer; cdecl; external LIBXML2_SO;
function  xmlNanoHTTPOpenRedir(URL: PChar; contentType: PPChar; redir: PPChar): Pointer; cdecl; external LIBXML2_SO;
function  xmlNanoHTTPReturnCode(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoHTTPAuthHeader(ctx: Pointer): PChar; cdecl; external LIBXML2_SO;
function  xmlNanoHTTPRead(ctx: Pointer; dest: Pointer; len: Longint): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoHTTPSave(ctxt: Pointer; filename: PChar): Longint; cdecl; external LIBXML2_SO;
procedure xmlNanoHTTPClose(ctx: Pointer); cdecl; external LIBXML2_SO;

