unit dkAbnfUtils;

{$I ADOMXMLWarn.inc}

// dkAbnfUtils 1.0.1
// Delphi 4 to 2009 and Kylix 3 Implementation
// December 2007
//
//
// LICENSE
//
// The contents of this file are subject to the Mozilla Public License Version
// 1.1 (the "License"); you may not use this file except in compliance with
// the License. You may obtain a copy of the License at
// "http://www.mozilla.org/MPL/"
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// The Original Code is "dkAbnfUtils.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 2003-2007 Dieter Köhler. All Rights Reserved.
//
// Alternatively, the contents of this file may be used under the terms of the
// GNU General Public License Version 2 or later (the "GPL"), in which case the
// provisions of the GPL are applicable instead of those above. If you wish to
// allow use of your version of this file only under the terms of the GPL, and
// not to allow others to use your version of this file under the terms of the
// MPL, indicate your decision by deleting the provisions above and replace them
// with the notice and other provisions required by the GPL. If you do not delete
// the provisions above, a recipient may use your version of this file under the
// terms of any one of the MPL or the GPL.

// HISTORY
//
// 2007-12-03 1.0.1 Made .NET compatible.
// 2003-08-03 1.0.0

interface

// Augmented Backus-Naur Form (ABNF) Core Rules according to RFC 2234, sect. 6.1.
function IsAbnfALPHAWideChar(C: WideChar): Boolean;
function IsAbnfBITWideChar(C: WideChar): Boolean;
function IsAbnfCHARWideChar(C: WideChar): Boolean;
function IsAbnfCRWideChar(C: WideChar): Boolean;
function IsAbnfCRLFWideStr(S: WideString): Boolean;
function IsAbnfCTLWideChar(C: WideChar): Boolean;
function IsAbnfDIGITWideChar(C: WideChar): Boolean;
function IsAbnfDQUOTEWideChar(C: WideChar): Boolean;
function IsAbnfHEXDIGWideChar(C: WideChar): Boolean;
function IsAbnfHTABWideChar(C: WideChar): Boolean;
function IsAbnfLFWideChar(C: WideChar): Boolean;
function IsAbnfLWSPWideStr(S: WideString): Boolean;
function IsAbnfOCTETWideChar(C: WideChar): Boolean;
function IsAbnfSPWideChar(C: WideChar): Boolean;
function IsAbnfVCHARWideChar(C: WideChar): Boolean;
function IsAbnfWSPWideChar(C: WideChar): Boolean;

function IsAbnfALPHAChar(C: Char): Boolean;
function IsAbnfBITChar(C: Char): Boolean;
function IsAbnfCHARChar(C: Char): Boolean;
function IsAbnfCRChar(C: Char): Boolean;
function IsAbnfCRLFStr(S: string): Boolean;
function IsAbnfCTLChar(C: Char): Boolean;
function IsAbnfDIGITChar(C: Char): Boolean;
function IsAbnfDQUOTEChar(C: Char): Boolean;
function IsAbnfHEXDIGChar(C: Char): Boolean;
function IsAbnfHTABChar(C: Char): Boolean;
function IsAbnfLFChar(C: Char): Boolean;
function IsAbnfLWSPStr(S: string): Boolean;
function IsAbnfOCTETChar(C: Char): Boolean;
function IsAbnfSPChar(C: Char): Boolean;
function IsAbnfVCHARChar(C: Char): Boolean;
function IsAbnfWSPChar(C: Char): Boolean;

implementation

function IsAbnfALPHAWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0041..$005A, $0061..$007A:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfBITWideChar(C: WideChar): Boolean;
begin
  if (C = '0') or (C = '1') then
    Result := True
  else
    Result := False;
end;

function IsAbnfCHARWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0001..$007F:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfCRWideChar(C: WideChar): Boolean;
begin
  if C = #$0D then
    Result := True
  else
    Result := False;
end;

function IsAbnfCRLFWideStr(S: wideString): Boolean;
begin
  if S = #$0D#$0A then
    Result := True
  else
    Result := False;
end;

function IsAbnfCTLWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0000..$001F, $007F:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfDIGITWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0030..$0039:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfDQUOTEWideChar(C: WideChar): Boolean;
begin
  if C = #$22 then
    Result := True
  else
    Result := False;
end;

function IsAbnfHEXDIGWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0030..$0039, $0041..$0046:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfHTABWideChar(C: WideChar): Boolean;
begin
  if C = #$09 then
    Result := True
  else
    Result := False;
end;

function IsAbnfLFWideChar(C: WideChar): Boolean;
begin
  if C = #$0A then
    Result := True
  else
    Result := False;
end;

function IsAbnfLWSPWideStr(S: WideString): Boolean;
var
  I, L: integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end
  else
    Result := True;
  I := 0;
  while I < L do
  begin
    Inc(I);
    case Word(S[I]) of
      $0020, $0009: ; // SP or TAB --> Do nothing, because everthing is alright
      $000D:
        begin // CR --> Look for LF
          if I = L then
          begin
            Result := False;
            Exit;
          end;
          Inc(I);
          if S[I] <> #$0A then
          begin
            Result := False;
            Exit;
          end;
        end;
    else
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function IsAbnfOCTETWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0000..$00FF:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfSPWideChar(C: WideChar): Boolean;
begin
  if C = #$20 then
    Result := True
  else
    Result := False;
end;

function IsAbnfVCHARWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0021..$007E:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfWSPWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0020, $0009:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfALPHAChar(C: Char): Boolean;
begin
  case Byte(C) of
    $41..$5A, $61..$7A:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfBITChar(C: Char): Boolean;
begin
  if (C = '0') or (C = '1') then
    Result := True
  else
    Result := False;
end;

function IsAbnfCHARChar(C: Char): Boolean;
begin
  case Byte(C) of
    $01..$7F:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfCRChar(C: Char): Boolean;
begin
  if C = #$0D then
    Result := True
  else
    Result := False;
end;

function IsAbnfCRLFStr(S: string): Boolean;
begin
  if S = #$0D#$0A then
    Result := True
  else
    Result := False;
end;

function IsAbnfCTLChar(C: Char): Boolean;
begin
  case Byte(C) of
    $00..$1F, $7F:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfDIGITChar(C: Char): Boolean;
begin
  case Byte(C) of
    $30..$39:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfDQUOTEChar(C: Char): Boolean;
begin
  if C = #$22 then
    Result := True
  else
    Result := False;
end;

function IsAbnfHEXDIGChar(C: Char): Boolean;
begin
  case Byte(C) of
    $30..$39, $41..$46:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfHTABChar(C: Char): Boolean;
begin
  if C = #$09 then
    Result := True
  else
    Result := False;
end;

function IsAbnfLFChar(C: Char): Boolean;
begin
  if C = #$0A then
    Result := True
  else
    Result := False;
end;

function IsAbnfLWSPStr(S: string): Boolean;
var
  I, L: integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end
  else
    Result := True;
  I := 0;
  while I < L do
  begin
    Inc(I);
    case Byte(S[I]) of
      $20, $09: ; // SP or TAB --> Do nothing, because everthing is alright
      $0D:
        begin // CR --> Look for LF
          if I = L then
          begin
            Result := False;
            Exit;
          end;
          Inc(I);
          if S[I] <> #$0A then
          begin
            Result := False;
            Exit;
          end;
        end;
    else
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function IsAbnfOCTETChar(C: Char): Boolean;
begin
  case Byte(C) of
    $00..$FF:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfSPChar(C: Char): Boolean;
begin
  if C = #$20 then
    Result := True
  else
    Result := False;
end;

function IsAbnfVCHARChar(C: Char): Boolean;
begin
  case Byte(C) of
    $21..$7E:
      Result := True;
  else
    Result := False;
  end;
end;

function IsAbnfWSPChar(C: Char): Boolean;
begin
  case Byte(C) of
    $20, $09:
      Result := True;
  else
    Result := False;
  end;
end;

end.

