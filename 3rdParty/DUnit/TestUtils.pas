{ #(@)$Id: TestUtils.pas,v 1.35 2013/06/13 09:55:00 jarrodh Exp $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 1.35 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Serge Beaumont
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2008.
 * All rights reserved.
 *
 * Contributor(s):
 * Serge Beaumont <beaumose@iquip.nl>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * Jon Bertrand <jonbsfnet@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *)

{$I DUnit.inc}

unit TestUtils;

interface
uses
{$IFDEF LINUX}
  Types;
{$ELSE}
  Windows;
{$ENDIF}
//  SysUtils,
//  Classes;

const
  rcs_id: string = '#(@)$Id: TestUtils.pas,v 1.35 2013/06/13 09:55:00 jarrodh Exp $';

{$IFDEF MSWINDOWS}
function CallerAddr: Pointer;
{$ELSE}
function  CallerAddr: Pointer; {$IFNDEF CLR} assembler; {$ENDIF}
{$ENDIF}

implementation

function IsBadPointer(const P: Pointer): boolean; {$IFNDEF CLR} register; {$ENDIF}
begin
  try
    Result :=
        (P = nil)
{$IFNDEF CLR}
        or ((Pointer(P^) <> P) and (Pointer(P^) = P));
{$ENDIF}
  except
    Result := true;
  end
end;

{$IFDEF MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
function RtlCaptureStackBackTrace(FramesToSkip: ULONG; FramesToCapture: ULONG;
  out BackTrace: Pointer; BackTraceHash: PULONG): USHORT; stdcall;
  external 'kernel32.dll' name 'RtlCaptureStackBackTrace' delayed;
{$WARN SYMBOL_PLATFORM ON}

// 32-bit and 64-bit compatible
// Source: http://stackoverflow.com/questions/12022862/what-does-dunit2s-calleraddr-function-do-and-how-do-i-convert-it-to-64-bits
function CallerAddr: Pointer;
begin
  // Skip 2 Frames, one for the return of CallerAddr and one for the
  // return of RtlCaptureStackBackTrace
  if RtlCaptureStackBackTrace(2, 1, Result, nil) > 0 then
  begin
    if not IsBadPointer(Result) then
      Result := Pointer(NativeInt(Result) - 5)
    else
      Result := nil;
  end
  else
  begin
    Result := nil;
  end;
end;
{$ELSE}
function CallerAddr: Pointer; {$IFNDEF CLR} assembler; {$ENDIF}
{$IFDEF CLR}
begin
  Result := nil;
end;
{$ELSE}
{
TODO: If FPC then use something like this:
var
  bp: Pointer;
begin
  bp := get_caller_frame(get_frame);
  if bp <> nil then
    Result := get_caller_addr(bp)
  else
    Result := nil;
end;
}
const
  CallerIP = $4;
asm
   mov   eax, ebp
   call  IsBadPointer
   test  eax,eax
   jne   @@Error

   mov   eax, [ebp].CallerIP
   sub   eax, 5   // 5 bytes for call

   push  eax
   call  IsBadPointer
   test  eax,eax
   pop   eax
   je    @@Finish

@@Error:
   xor eax, eax
@@Finish:
end;
{$ENDIF}
{$ENDIF}

end.

