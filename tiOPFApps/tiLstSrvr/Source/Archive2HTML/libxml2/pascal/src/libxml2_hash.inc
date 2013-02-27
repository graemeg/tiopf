// CVS-REV:gnome-xml/include/libxml/hash.h:1.11:
{
  ------------------------------------------------------------------------------
  Translated into pascal with help of h2pas utility from the FreePascal project.
  Petr Kozelka <pkozelka@email.cz>
  ------------------------------------------------------------------------------
}
(*
 * hash.h: chained hash tables
 *
 * Copyright (C) 2000 Bjorn Reese and Daniel Veillard.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE AUTHORS AND
 * CONTRIBUTORS ACCEPT NO RESPONSIBILITY IN ANY CONCEIVABLE MANNER.
 *
 * Author: bjorn.reese@systematic.dk
 *)

type
  (*
   * The hash table.
   *)
  xmlHashTablePtr = ^xmlHashTable;
  xmlHashTable = record end;

  (*
   * function types:
   *)
  (**
   * xmlHashDeallocator:
   * @payload:  the data in the hash
   * @name:  the name associated
   *
   * Callback to free data from a hash.
   *)
  xmlHashDeallocator = procedure(payload: Pointer; name: PxmlChar); cdecl;
  (**
   * xmlHashCopier:
   * @payload:  the data in the hash
   * @name:  the name associated
   *
   * Callback to copy data from a hash.
   *
   * Returns a copy of the data or NULL in case of error.
   *)
  xmlHashCopier = function(payload: Pointer; name: PxmlChar): Pointer; cdecl;
  (**
   * xmlHashScanner:
   * @payload:  the data in the hash
   * @data:  extra scannner data
   * @name:  the name associated
   *
   * Callback when scanning data in a hash with the simple scanner.
   *)
  xmlHashScanner = procedure(payload: Pointer; data: Pointer; name: PxmlChar); cdecl;
  (**
   * xmlHashScannerFull:
   * @payload:  the data in the hash
   * @data:  extra scannner data
   * @name:  the name associated
   * @name2:  the second name associated
   * @name3:  the third name associated
   *
   * Callback when scanning data in a hash with the full scanner.
   *)
  xmlHashScannerFull = function(payload: Pointer; data: Pointer; name, name2, name3: PxmlChar): Pointer; cdecl;

(*
 * Constructor and destructor.
 *)
function  xmlHashCreate(size: Longint): xmlHashTablePtr; cdecl; external LIBXML2_SO;
procedure xmlHashFree(table: xmlHashTablePtr; f: xmlHashDeallocator); cdecl; external LIBXML2_SO;

(*
 * Add a new entry to the hash table.
 *)
function  xmlHashAddEntry(table: xmlHashTablePtr; name: PxmlChar; userdata: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlHashUpdateEntry(table: xmlHashTablePtr; name: PxmlChar; userdata: Pointer; f: xmlHashDeallocator): Longint; cdecl; external LIBXML2_SO;
function  xmlHashAddEntry2(table: xmlHashTablePtr; name: PxmlChar; name2: PxmlChar; userdata: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlHashUpdateEntry2(table: xmlHashTablePtr; name: PxmlChar; name2: PxmlChar; userdata: Pointer; f: xmlHashDeallocator): Longint; cdecl; external LIBXML2_SO;
function  xmlHashAddEntry3(table: xmlHashTablePtr; name: PxmlChar; name2: PxmlChar; name3: PxmlChar; userdata: Pointer): Longint; cdecl; external LIBXML2_SO;
function  xmlHashUpdateEntry3(table: xmlHashTablePtr; name: PxmlChar; name2: PxmlChar; name3: PxmlChar; userdata: Pointer; f: xmlHashDeallocator): Longint; cdecl; external LIBXML2_SO;

(*
 * Remove an entry from the hash table.
 *)
function  xmlHashRemoveEntry(table: xmlHashTablePtr; name: PxmlChar; f: xmlHashDeallocator): Longint; cdecl; external LIBXML2_SO;
function  xmlHashRemoveEntry2(table: xmlHashTablePtr; name: PxmlChar; name2: PxmlChar; f: xmlHashDeallocator): Longint; cdecl; external LIBXML2_SO;
function  xmlHashRemoveEntry3(table: xmlHashTablePtr; name: PxmlChar; name2: PxmlChar; name3: PxmlChar; f: xmlHashDeallocator): Longint; cdecl; external LIBXML2_SO;

(*
 * Retrieve the userdata.
 *)
function  xmlHashLookup(table: xmlHashTablePtr; name: PxmlChar): Pointer; cdecl; external LIBXML2_SO;
function  xmlHashLookup2(table: xmlHashTablePtr; name: PxmlChar; name2: PxmlChar): Pointer; cdecl; external LIBXML2_SO;
function  xmlHashLookup3(table: xmlHashTablePtr; name: PxmlChar; name2: PxmlChar; name3: PxmlChar): Pointer; cdecl; external LIBXML2_SO;

(*
 * Helpers.
 *)
function  xmlHashCopy(table: xmlHashTablePtr; f: xmlHashCopier): xmlHashTablePtr; cdecl; external LIBXML2_SO;
function  xmlHashSize(table: xmlHashTablePtr): Longint; cdecl; external LIBXML2_SO;
procedure xmlHashScan(table: xmlHashTablePtr; f: xmlHashScanner; data: Pointer); cdecl; external LIBXML2_SO;
procedure xmlHashScan3(table: xmlHashTablePtr; name, name2, name3: PxmlChar; f: xmlHashScanner; data: Pointer); cdecl; external LIBXML2_SO;
procedure xmlHashScanFull(table: xmlHashTablePtr; f: xmlHashScannerFull; data: Pointer); cdecl; external LIBXML2_SO;
procedure xmlHashScanFull3(table: xmlHashTablePtr; name, name2, name3: PxmlChar; f: xmlHashScannerFull; data: Pointer); cdecl; external LIBXML2_SO;

