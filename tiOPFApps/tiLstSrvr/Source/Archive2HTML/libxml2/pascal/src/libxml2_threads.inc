// CVS-REV:gnome-xml/include/libxml/threads.h:1.4:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(**
 * threads.c: set of generic threading related routines
 *
 * See Copyright for the status of this software.
 *
 * daniel@veillard.com
 *)
(*
 * xmlMutex are a simple mutual exception locks.
 *)
type
  xmlMutex = Pointer;
  xmlMutexPtr = ^xmlMutex;

(*
 * xmlRMutex are reentrant mutual exception locks.
 *)
type
  xmlRMutex = Pointer;
  xmlRMutexPtr = ^xmlRMutex;

function  xmlNewMutex: xmlMutexPtr; cdecl; external LIBXML2_SO;
procedure xmlMutexLock(tok: xmlMutexPtr); cdecl; external LIBXML2_SO;
procedure xmlMutexUnlock(tok: xmlMutexPtr); cdecl; external LIBXML2_SO;
procedure xmlFreeMutex(tok: xmlMutexPtr); cdecl; external LIBXML2_SO;

function  xmlNewRMutex: xmlRMutexPtr; cdecl; external LIBXML2_SO;
procedure xmlRMutexLock(tok: xmlRMutexPtr); cdecl; external LIBXML2_SO;
procedure xmlRMutexUnlock(tok: xmlRMutexPtr); cdecl; external LIBXML2_SO;
procedure xmlFreeRMutex(tok: xmlRMutexPtr); cdecl; external LIBXML2_SO;

(*
 * Library wide APIs.
 *)
procedure xmlInitThreads; cdecl; external LIBXML2_SO;
procedure xmlLockLibrary; cdecl; external LIBXML2_SO;
procedure xmlUnlockLibrary; cdecl; external LIBXML2_SO;
function  xmlGetThreadId: Longint; cdecl; external LIBXML2_SO;
function  xmlIsMainThread: Longint; cdecl; external LIBXML2_SO;
procedure xmlCleanupThreads; cdecl; external LIBXML2_SO;
function  xmlGetGlobalState: xmlGlobalStatePtr; cdecl; external LIBXML2_SO;

