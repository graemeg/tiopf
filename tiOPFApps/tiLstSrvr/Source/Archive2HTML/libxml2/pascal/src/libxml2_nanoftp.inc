// CVS-REV:gnome-xml/include/libxml/nanoftp.h:1.11:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * nanohttp.c: minimalist FTP implementation to fetch external subsets.
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)

type
  (**
   * ftpListCallback:
   * @userData:  user provided data for the callback
   * @filename:  the file name (including "->" when links are shown)
   * @attrib:  the attribute string
   * @owner:  the owner string
   * @group:  the group string
   * @size:  the file size
   * @links:  the link count
   * @year:  the year
   * @month:  the month
   * @day:  the day
   * @hour:  the hour
   * @minute:  the minute
   *
   * A callback for the xmlNanoFTPList command.
   * Note that only one of year and day:minute are specified.
   *)
  ftpListCallback = procedure(userData: Pointer; filename, attrib, owner, group: PChar; size: Longint; links: Longint; year: Longint; month: Byte; day, hour, minute: Longint); cdecl;

  (**
   * ftpDataCallback:
   * @userData: the user provided context
   * @data: the data received
   * @len: its size in bytes
   *
   * A callback for the xmlNanoFTPGet command.
   *)
  ftpDataCallback = procedure(userData: Pointer; data: PChar; len: Longint); cdecl;

(*
 * Init.
 *)
procedure xmlNanoFTPInit; cdecl; external LIBXML2_SO;
procedure xmlNanoFTPCleanup; cdecl; external LIBXML2_SO;

(*
 * Creating/freeing contexts.
 *)
function  xmlNanoFTPNewCtxt(URL: PChar): Pointer; cdecl; external LIBXML2_SO;
procedure xmlNanoFTPFreeCtxt(ctx: Pointer); cdecl; external LIBXML2_SO;
function  xmlNanoFTPConnectTo(server: PChar; port: Longint): Pointer; cdecl; external LIBXML2_SO;

(*
 * Opening/closing session connections.
 *)
function  xmlNanoFTPOpen(URL: PChar): Pointer; cdecl; external LIBXML2_SO;
function  xmlNanoFTPConnect(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPClose(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPQuit(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
procedure xmlNanoFTPScanProxy(URL: PChar); cdecl; external LIBXML2_SO;
procedure xmlNanoFTPProxy(host: PChar; port: Longint; user: PChar; passwd: PChar; type_: Longint); cdecl; external LIBXML2_SO;
function  xmlNanoFTPUpdateURL(ctx: Pointer; URL: PChar): Longint; cdecl; external LIBXML2_SO;

(*
 * Rather internal commands.
 *)
function  xmlNanoFTPGetResponse(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPCheckResponse(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;

(*
 * CD/DIR/GET handlers.
 *)
function  xmlNanoFTPCwd(ctx: Pointer; directory: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPGetConnection(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPCloseConnection(ctx: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPList(ctx: Pointer; callback: ftpListCallback; userData: Pointer; filename: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPGetSocket(ctx: Pointer; filename: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPGet(ctx: Pointer; callback: ftpDataCallback; userData: Pointer; filename: PChar): Longint; cdecl; external LIBXML2_SO;
function  xmlNanoFTPRead(ctx: Pointer; dest: Pointer; len: Longint): Longint; cdecl; external LIBXML2_SO;

