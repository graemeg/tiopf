unit dkUriUtils;

{$I ADOMXMLWarn.inc}

// dkUriUtils 1.0.3
// Delphi 4 to 2009 and Kylix 3 Implementation
// September 2008
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
// The Original Code is "UriUtils.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 2003-2008 Dieter Köhler. All Rights Reserved.
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
// 2008-09-28 1.0.3 Internal modifications.
// 2007-12-03 1.0.2 Made .NET compliant. Minor internal modifications.
// 2007-07-05 1.0.1 Support for Linux filenames under Kylix added. 
// 2003-08-03 1.0.0

interface

uses
  SysUtils;

// URI Rules (cf. RFC 2396, App. A)

function IsUriURI_referenceWideStr(S: WideString): Boolean;
function IsUriAbsoluteURIWideStr(S: WideString): Boolean;
function IsUriRelativeURIWideStr(S: WideString): Boolean;
function IsUriHier_partWideStr(S: WideString): Boolean;
function IsUriOpaque_partWideStr(S: WideString): Boolean;
function IsUriNet_pathWideStr(S: WideString): Boolean;
function IsUriAbs_pathWideStr(S: WideString): Boolean;
function IsUriRel_pathWideStr(S: WideString): Boolean;
function IsUriRel_segmentWideStr(S: WideString): Boolean;
function IsUriSchemeWideStr(S: WideString): Boolean;
function IsUriAuthorityWideStr(S: WideString): Boolean;
function IsUriReg_nameWideStr(S: WideString): Boolean;
function IsUriServerWideStr(S: WideString): Boolean;
function IsUriUserinfoWideStr(S: WideString): Boolean;
function IsUriHostPortWideStr(S: WideString): Boolean;
function IsUriHostWideStr(S: WideString): Boolean;
function IsUriHostnameWideStr(S: WideString): Boolean;
function IsUriDomainlabelWideStr(S: WideString): Boolean;
function IsUriToplabelWideStr(S: WideString): Boolean;
function IsUriIPv4addressWideStr(S: WideString): Boolean;
function IsUriPortWideStr(S: WideString): Boolean;
function IsUriPathWideStr(S: WideString): Boolean;
function IsUriPath_segmentsWideStr(S: WideString): Boolean;
function IsUriSegmentWideStr(S: WideString): Boolean;
function IsUriParamWideStr(S: WideString): Boolean;
function IsUriQueryWideStr(S: WideString): Boolean;
function IsUriFragmentWideStr(S: WideString): Boolean;
function IsUriUricWideStr(S: WideString): Boolean;
function IsUriReservedWideChar(C: WideChar): Boolean;
function IsUriUnreservedWideChar(C: WideChar): Boolean;
function IsUriMarkWideChar(C: WideChar): Boolean;
function IsUriHexWideChar(C: WideChar): Boolean;
function IsUriAlphanumWideChar(C: WideChar): Boolean;
function IsUriAlphaWideChar(C: WideChar): Boolean;
function IsUriDigitWideChar(C: WideChar): Boolean;

function IsUriURI_referenceStr(S: string): Boolean;
function IsUriAbsoluteURIStr(S: string): Boolean;
function IsUriRelativeURIStr(S: string): Boolean;
function IsUriHier_partStr(S: string): Boolean;
function IsUriOpaque_partStr(S: string): Boolean;
function IsUriNet_pathStr(S: string): Boolean;
function IsUriAbs_pathStr(S: string): Boolean;
function IsUriRel_pathStr(S: string): Boolean;
function IsUriRel_segmentStr(S: string): Boolean;
function IsUriSchemeStr(S: string): Boolean;
function IsUriAuthorityStr(S: string): Boolean;
function IsUriReg_nameStr(S: string): Boolean;
function IsUriServerStr(S: string): Boolean;
function IsUriUserinfoStr(S: string): Boolean;
function IsUriHostPortStr(S: string): Boolean;
function IsUriHostStr(S: string): Boolean;
function IsUriHostnameStr(S: string): Boolean;
function IsUriDomainlabelStr(S: string): Boolean;
function IsUriToplabelStr(S: string): Boolean;
function IsUriIPv4addressStr(S: string): Boolean;
function IsUriPortStr(S: string): Boolean;
function IsUriPathStr(S: string): Boolean;
function IsUriPath_segmentsStr(S: string): Boolean;
function IsUriSegmentStr(S: string): Boolean;
function IsUriParamStr(S: string): Boolean;
function IsUriQueryStr(S: string): Boolean;
function IsUriFragmentStr(S: string): Boolean;
function IsUriUricStr(S: string): Boolean;
function IsUriReservedChar(C: Char): Boolean;
function IsUriUnreservedChar(C: Char): Boolean;
function IsUriMarkChar(C: Char): Boolean;
function IsUriHexChar(C: Char): Boolean;
function IsUriAlphanumChar(C: Char): Boolean;
function IsUriAlphaChar(C: Char): Boolean;
function IsUriDigitChar(C: Char): Boolean;

// URI conversion functions

type
  TUtilsFilenameToUriOptions = set of (fuSetLocalhost, fuPlainColon);

function FilenameToUriStr(const Path: TFilename;
  const Opt: TUtilsFilenameToUriOptions): string;
function FilenameToUriWideStr(const Path: TFilename;
  const Opt: TUtilsFilenameToUriOptions): WideString;
function ResolveRelativeUriStr(const BaseUri,
  RelUri: string;
  var ResultUri: string): Boolean;
function ResolveRelativeUriWideStr(const BaseUri,
  RelUri: WideString;
  var ResultUri: WideString): Boolean;
function UriStrToFilename(const Uri: string;
  var Path: TFilename;
  var Authority,
  Query,
  Fragment: string): Boolean;
function UriWideStrToFilename(const Uri: WideString;
  var Path: TFilename;
  var Authority,
  Query,
  Fragment: WideString): Boolean;

// URI analysis

type
  TUriStrAnalyzer = class
  protected
    FUriAuthority: string;
    FUriFragment: string;
    FUriQuery: string;
    FUriPath: string;
    FUriScheme: string;
    FHasUriAuthority: Boolean;
    FHasUriFragment: Boolean;
    FHasUriQuery: Boolean;
    FHasUriScheme: Boolean;
    function GetUriReference: string; virtual;
  public
    constructor Create;
    function SetUriAuthority(const Value: string;
      const IsDefined: Boolean): Boolean; virtual;
    function SetUriFragment(const Value: string;
      const IsDefined: Boolean): Boolean; virtual;
    function SetUriPath(const Value: string): Boolean; virtual;
    function SetUriQuery(const Value: string;
      const IsDefined: Boolean): Boolean; virtual;
    function SetUriReference(const Value: string): Boolean; virtual;
    function SetUriScheme(const Value: string;
      const IsDefined: Boolean): Boolean; virtual;
    property HasUriAuthority: Boolean read FHasUriAuthority;
    property HasUriFragment: Boolean read FHasUriFragment;
    property HasUriQuery: Boolean read FHasUriQuery;
    property HasUriScheme: Boolean read FHasUriScheme;
    property UriAuthority: string read FUriAuthority;
    property UriFragment: string read FUriFragment;
    property UriPath: string read FUriPath;
    property UriQuery: string read FUriQuery;
    property UriReference: string read GetUriReference;
    property UriScheme: string read FUriScheme;
  end;

  TUriWideStrAnalyzer = class
  protected
    FUriAuthority: WideString;
    FUriFragment: WideString;
    FUriQuery: WideString;
    FUriPath: WideString;
    FUriScheme: WideString;
    FHasUriAuthority: Boolean;
    FHasUriFragment: Boolean;
    FHasUriQuery: Boolean;
    FHasUriScheme: Boolean;
    function GetUriReference: WideString; virtual;
  public
    constructor Create;
    function SetUriAuthority(const Value: WideString;
      const IsDefined: Boolean): Boolean; virtual;
    function SetUriFragment(const Value: WideString;
      const IsDefined: Boolean): Boolean; virtual;
    function SetUriPath(const Value: WideString): Boolean; virtual;
    function SetUriQuery(const Value: WideString;
      const IsDefined: Boolean): Boolean; virtual;
    function SetUriReference(const Value: WideString): Boolean; virtual;
    function SetUriScheme(const Value: WideString;
      const IsDefined: Boolean): Boolean; virtual;
    property HasUriAuthority: Boolean read FHasUriAuthority;
    property HasUriFragment: Boolean read FHasUriFragment;
    property HasUriQuery: Boolean read FHasUriQuery;
    property HasUriScheme: Boolean read FHasUriScheme;
    property UriAuthority: WideString read FUriAuthority;
    property UriFragment: WideString read FUriFragment;
    property UriPath: WideString read FUriPath;
    property UriQuery: WideString read FUriQuery;
    property UriReference: WideString read GetUriReference;
    property UriScheme: WideString read FUriScheme;
  end;

implementation

uses
  dkWideStringUtils, dkAbnfUtils, Classes;

function IsUriURI_referenceWideStr(S: WideString): Boolean;
var
  DcPos: Integer;
  S1: string;
begin
  DcPos := Pos('#', S);
  if DcPos > 0 then
  begin
    S1 := Copy(S, 1, DcPos - 1);
    Result := (IsUriAbsoluteURIWideStr(S1)
      or IsUriRelativeURIWideStr(S1)
      or (S1 = ''))
      and IsUriFragmentWideStr(Copy(S, DcPos + 1, length(S) - DcPos));
  end
  else
    Result := IsUriAbsoluteURIWideStr(S) or IsUriRelativeURIWideStr(S) or (S =
      '');
end;

function IsUriAbsoluteURIWideStr(S: WideString): Boolean;
var
  ColonPos: Integer;
  S1: string;
begin
  ColonPos := Pos(':', S);
  if ColonPos > 0 then
  begin
    S1 := Copy(S, ColonPos + 1, Length(S) - ColonPos);
    Result := IsUriSchemeWideStr(Copy(S, 1, ColonPos - 1)) and
      (IsUriHier_partWideStr(S1) or IsUriOpaque_partWideStr(S1));
  end
  else
    Result := False;
end;

function IsUriRelativeURIWideStr(S: WideString): Boolean;
var
  QmPos: Integer;
  S1: string;
begin
  QmPos := Pos(#63, S);
  if QmPos > 0 then
  begin
    S1 := Copy(S, 1, QmPos - 1);
    Result := (IsUriNet_PathWideStr(S1)
      or IsUriAbs_PathWideStr(S1)
      or IsUriRel_PathWideStr(S1))
      and IsUriQueryWideStr(Copy(S, QmPos + 1, Length(S) - QmPos));
  end
  else
    Result := IsUriNet_PathWideStr(S) or IsUriAbs_PathWideStr(S) or
      IsUriRel_PathWideStr(S);
end;

function IsUriHier_partWideStr(S: WideString): Boolean;
var
  QmPos: Integer;
  S1: string;
begin
  QmPos := Pos(#63, S);
  if QmPos > 0 then
  begin
    S1 := Copy(S, 1, QmPos - 1);
    Result := (IsUriNet_PathWideStr(S1)
      or IsUriAbs_PathWideStr(S1))
      and IsUriQueryWideStr(Copy(S, QmPos + 1, Length(S) - QmPos));
  end
  else
    Result := IsUriNet_PathWideStr(S) or IsUriAbs_PathWideStr(S);
end;

function IsUriOpaque_partWideStr(S: WideString): Boolean;
begin
  if S = '' then
  begin
    Result := False;
    Exit;
  end;
  if S[1] = '/' then
  begin
    Result := False;
    Exit;
  end;
  Result := IsUriUricWideStr(S);
end;

function IsUriNet_PathWideStr(S: WideString): Boolean;
var
  SlashPos: Integer;
begin
  if Copy(S, 1, 2) <> '//' then
  begin
    Result := False;
    Exit;
  end;
  S := Copy(S, 3, Length(S) - 2);
  SlashPos := Pos('/', S);
  if SlashPos > 0 then
  begin
    Result := IsUriAuthorityWideStr(Copy(S, 1, SlashPos - 1)) and
      IsUriAbs_PathWideStr(Copy(S, SlashPos, Length(S) - SlashPos + 1));
  end
  else
    Result := IsUriAuthorityWideStr(S);
end;

function IsUriAbs_PathWideStr(S: WideString): Boolean;
begin
  if S = '' then
  begin
    Result := False;
    Exit;
  end;
  if S[1] <> '/' then
  begin
    Result := False;
    Exit;
  end;
  Result := IsUriPath_segmentsWideStr(Copy(S, 2, Length(S) - 1));
end;

function IsUriRel_PathWideStr(S: WideString): Boolean;
var
  SlashPos: Integer;
begin
  SlashPos := Pos('/', S);
  if SlashPos > 0 then
  begin
    Result := IsUriRel_segmentWideStr(Copy(S, 1, SlashPos - 1)) and
      IsUriAbs_PathWideStr(Copy(S, SlashPos, Length(S) - SlashPos + 1));
  end
  else
    Result := IsUriRel_segmentWideStr(S);
end;

function IsUriRel_segmentWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedWideChar(S[I])
      or (S[I] = ';') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriSchemeWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  if not IsAbnfALPHAWideChar(S[1]) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for I := 2 to L do
    if not (isAbnfALPHAWideChar(S[I])
      or isAbnfDIGITWideChar(S[I])
      or (S[I] = '+')
      or (S[I] = '-')
      or (S[I] = '.')
      ) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsUriAuthorityWideStr(S: WideString): Boolean;
begin
  Result := IsUriServerWideStr(S) or IsUriReg_nameWideStr(S);
end;

function IsUriReg_nameWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedWideChar(S[I])
      or (S[I] = '$') or (S[I] = ',') or (S[I] = ';')
      or (S[I] = ':') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriServerWideStr(S: WideString): Boolean;
var
  AtPos, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := True;
    Exit;
  end;
  AtPos := Pos('@', S);
  if AtPos > 0 then
  begin
    Result := IsUriUserinfoWideStr(Copy(S, 1, AtPos - 1)) and
      IsUriHostportWideStr(Copy(S, AtPos + 1, L - AtPos));
  end
  else
    Result := IsUriHostportWideStr(S);
end;

function IsUriUserinfoWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  Result := True;
  if L = 0 then
    Exit;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedWideChar(S[I])
      or (S[I] = ';') or (S[I] = ':') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriHostPortWideStr(S: WideString): Boolean;
var
  ColonPos: Integer;
begin
  ColonPos := Pos(':', S);
  if ColonPos > 0 then
  begin
    Result := IsUriHostWideStr(Copy(S, 1, ColonPos - 1)) and
      IsUriPortWideStr(Copy(S, ColonPos + 1, Length(S) - ColonPos));
  end
  else
    Result := IsUriHostWideStr(S);
end;

function IsUriHostWideStr(S: WideString): Boolean;
begin
  Result := IsUriHostnameWideStr(S) or IsUriIPv4addressWideStr(S);
end;

function IsUriHostnameWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  if S[L] = '.' then
    Dec(L);
  I := L;
  while I > 0 do
  begin
    if S[I] = '.' then
      break;
    Dec(I);
  end;
  if not IsUriToplabelWideStr(Copy(S, I + 1, L - I)) then
  begin
    Result := False;
    Exit;
  end;
  while I > 0 do
  begin
    L := I;
    if S[L] = '.' then
      Dec(L);
    I := L;
    while I > 0 do
    begin
      if S[I] = '.' then
        break;
      Dec(I);
    end;
    if not IsUriDomainlabelWideStr(Copy(S, I + 1, L - I)) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriDomainlabelWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  if not (IsUriAlphanumWideChar(S[1]) and IsUriAlphanumWideChar(S[L])) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  I := 1;
  while I < L do
  begin
    Inc(I);
    if not (isUriAlphanumWideChar(S[I]) or (S[I] = '-')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriToplabelWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  if not (IsUriAlphaWideChar(S[1]) and IsUriAlphanumWideChar(S[L])) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  I := 1;
  while I < L do
  begin
    Inc(I);
    if not (isUriAlphanumWideChar(S[I]) or (S[I] = '-')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriIPv4addressWideStr(S: WideString): Boolean;
var
  digitNo, colonNo, I, L: Integer;
  digitFound: Boolean;
begin
  Result := False;
  L := Length(S);
  I := 0;
  digitNo := 0;
  colonNo := 0;
  digitFound := False;
  while I < L do
  begin
    if IsUriDigitWideChar(S[I]) then
    begin
      if not digitFound then
      begin
        digitFound := True;
        Inc(digitNo);
      end;
    end
    else if S[I] = '.' then
    begin
      if not digitFound then
        Exit;
      digitFound := False;
      Inc(colonNo);
    end
    else
      Exit;
  end;
  if (colonNo = 3) and (digitNo = 4) then
    Result := True;
end;

function IsUriPortWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  Result := True;
  L := Length(S);
  for I := 1 to L do
    if not IsUriDigitWideChar(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsUriPathWideStr(S: WideString): Boolean;
begin
  if IsUriAbs_PathWideStr(S) or IsUriOpaque_partWideStr(S) or (S = '') then
    Result := True
  else
    Result := False;
end;

function IsUriPath_segmentsWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  Result := True;
  if L = 0 then
    Exit;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedWideChar(S[I])
      or (S[I] = ':') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',') or (S[I] = ';') or (S[I] = '/')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriSegmentWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  Result := True;
  if L = 0 then
    Exit;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedWideChar(S[I])
      or (S[I] = ':') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',') or (S[I] = ';')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriParamWideStr(S: WideString): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  Result := True;
  if L = 0 then
    Exit;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedWideChar(S[I])
      or (S[I] = ':') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriQueryWideStr(S: WideString): Boolean;
begin
  if S = '' then
    Result := True
  else
    Result := IsUriUricWideStr(S);
end;

function IsUriFragmentWideStr(S: WideString): Boolean;
begin
  if S = '' then
    Result := True
  else
    Result := IsUriUricWideStr(S);
end;

function IsUriUricWideStr(S: WideString): Boolean;
var
  I, L: Integer;
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
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexWideChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriReservedWideChar(S[I]) or IsUriUnreservedWideChar(S[I]))
      then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriReservedWideChar(C: WideChar): Boolean;
begin
  if (C = ';') or (C = '/') or (C = #63) or (C = ':') or (C = '@') or (C = '&')
    or
    (C = '=') or (C = '+') or (C = '$') or (C = ',') then
    Result := True
  else
    Result := False;
end;

function IsUriUnreservedWideChar(C: WideChar): Boolean;
begin
  if IsUriAlphanumWideChar(C) or IsUriMarkWideChar(C) then
    Result := True
  else
    Result := False;
end;

function IsUriMarkWideChar(C: WideChar): Boolean;
begin
  if (C = '-') or (C = '_') or (C = '.') or (C = '!') or (C = '~') or (C = '*')
    or
    (C = #39) or (C = '(') or (C = ')') then
    Result := True
  else
    Result := False;
end;

function IsUriHexWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0030..$0039, $0041..$0046, $0061..$0066: // 0..9 , A..F , a..f
      Result := True;
  else
    Result := False;
  end;
end;

function IsUriAlphanumWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0030..$0039, $0041..$005A, $0061..$007A:
      Result := True;
  else
    Result := False;
  end;
end;

function IsUriAlphaWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0041..$005A, $0061..$007A:
      Result := True;
  else
    Result := False;
  end;
end;

function IsUriDigitWideChar(C: WideChar): Boolean;
begin
  case Word(C) of
    $0030..$0039:
      Result := True;
  else
    Result := False;
  end;
end;

function IsUriURI_referenceStr(S: string): Boolean;
var
  DcPos: Integer;
  S1: string;
begin
  DcPos := Pos('#', S);
  if DcPos > 0 then
  begin
    S1 := Copy(S, 1, DcPos - 1);
    Result := (IsUriAbsoluteURIStr(S1)
      or IsUriRelativeURIStr(S1)
      or (S1 = ''))
      and IsUriFragmentStr(Copy(S, DcPos + 1, Length(S) - DcPos));
  end
  else
    Result := IsUriAbsoluteURIStr(S) or IsUriRelativeURIStr(S) or (S = '');
end;

function IsUriAbsoluteURIStr(S: string): Boolean;
var
  ColonPos: Integer;
  S1: string;
begin
  ColonPos := Pos(':', S);
  if ColonPos > 0 then
  begin
    S1 := Copy(S, ColonPos + 1, Length(S) - ColonPos);
    Result := IsUriSchemeStr(Copy(S, 1, ColonPos - 1)) and
      (IsUriHier_partStr(S1) or IsUriOpaque_partStr(S1));
  end
  else
    Result := False;
end;

function IsUriRelativeURIStr(S: string): Boolean;
var
  QmPos: Integer;
  S1: string;
begin
  QmPos := Pos('?', S);
  if QmPos > 0 then
  begin
    S1 := Copy(S, 1, QmPos - 1);
    Result := (IsUriNet_PathStr(S1)
      or IsUriAbs_PathStr(S1)
      or IsUriRel_PathStr(S1))
      and IsUriQueryStr(Copy(S, QmPos + 1, Length(S) - QmPos));
  end
  else
    Result := IsUriNet_PathStr(S) or IsUriAbs_PathStr(S) or IsUriRel_PathStr(S);
end;

function IsUriHier_partStr(S: string): Boolean;
var
  QmPos: Integer;
  S1: string;
begin
  QmPos := Pos('?', S);
  if QmPos > 0 then
  begin
    S1 := Copy(S, 1, QmPos - 1);
    Result := (IsUriNet_PathStr(S1)
      or IsUriAbs_PathStr(S1))
      and IsUriQueryStr(Copy(S, QmPos + 1, Length(S) - QmPos));
  end
  else
    Result := IsUriNet_PathStr(S) or IsUriAbs_PathStr(S);
end;

function IsUriOpaque_partStr(S: string): Boolean;
begin
  if S = '' then
  begin
    Result := False;
    Exit;
  end;
  if S[1] = '/' then
  begin
    Result := False;
    Exit;
  end;
  Result := IsUriUricStr(S);
end;

function IsUriNet_PathStr(S: string): Boolean;
var
  SlashPos: Integer;
begin
  if Copy(S, 1, 2) <> '//' then
  begin
    Result := False;
    Exit;
  end;
  S := Copy(S, 3, Length(S) - 2);
  SlashPos := Pos('/', S);
  if SlashPos > 0 then
  begin
    Result := IsUriAuthorityStr(Copy(S, 1, SlashPos - 1)) and
      IsUriAbs_PathStr(Copy(S, SlashPos, Length(S) - SlashPos + 1));
  end
  else
    Result := IsUriAuthorityStr(S);
end;

function IsUriAbs_PathStr(S: string): Boolean;
begin
  if S = '' then
  begin
    Result := False;
    Exit;
  end;
  if S[1] <> '/' then
  begin
    Result := False;
    Exit;
  end;
  Result := IsUriPath_segmentsStr(Copy(S, 2, Length(S) - 1));
end;

function IsUriRel_PathStr(S: string): Boolean;
var
  SlashPos: Integer;
begin
  SlashPos := Pos('/', S);
  if SlashPos > 0 then
  begin
    Result := IsUriRel_segmentStr(Copy(S, 1, SlashPos - 1)) and
      IsUriAbs_PathStr(Copy(S, SlashPos, Length(S) - SlashPos + 1));
  end
  else
    Result := IsUriRel_segmentStr(S);
end;

function IsUriRel_segmentStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedChar(S[I])
      or (S[I] = ';') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriSchemeStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  if not isAbnfALPHAChar(S[1]) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for I := 2 to L do
    if not (isAbnfALPHAChar(S[I])
      or isAbnfDIGITChar(S[I])
      or (S[I] = '+')
      or (S[I] = '-')
      or (S[I] = '.')
      ) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsUriAuthorityStr(S: string): Boolean;
begin
  Result := IsUriServerStr(S) or IsUriReg_nameStr(S);
end;

function IsUriReg_nameStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedChar(S[I])
      or (S[I] = '$') or (S[I] = ',') or (S[I] = ';')
      or (S[I] = ':') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriServerStr(S: string): Boolean;
var
  AtPos, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := True;
    Exit;
  end;
  AtPos := Pos('@', S);
  if AtPos > 0 then
  begin
    Result := IsUriUserinfoStr(Copy(S, 1, AtPos - 1)) and
      IsUriHostportStr(Copy(S, AtPos + 1, L - AtPos));
  end
  else
    Result := IsUriHostportStr(S);
end;

function IsUriUserinfoStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  Result := True;
  if L = 0 then
    Exit;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedChar(S[I])
      or (S[I] = ';') or (S[I] = ':') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriHostPortStr(S: string): Boolean;
var
  ColonPos: Integer;
begin
  ColonPos := Pos(':', S);
  if ColonPos > 0 then
  begin
    Result := IsUriHostStr(Copy(S, 1, ColonPos - 1)) and
      IsUriPortStr(Copy(S, ColonPos + 1, Length(S) - ColonPos));
  end
  else
    Result := IsUriHostStr(S);
end;

function IsUriHostStr(S: string): Boolean;
begin
  Result := IsUriHostnameStr(S) or IsUriIPv4addressStr(S);
end;

function IsUriHostnameStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  if S[L] = '.' then
    Dec(L);
  I := L;
  while I > 0 do
  begin
    if S[I] = '.' then
      break;
    Dec(I);
  end;
  if not IsUriToplabelStr(Copy(S, I + 1, L - I)) then
  begin
    Result := False;
    Exit;
  end;
  while I > 0 do
  begin
    L := I;
    if S[L] = '.' then
      Dec(L);
    I := L;
    while I > 0 do
    begin
      if S[I] = '.' then
        break;
      Dec(I);
    end;
    if not IsUriDomainlabelStr(Copy(S, I + 1, L - I)) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriDomainlabelStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  if not (IsUriAlphanumChar(S[1]) and IsUriAlphanumChar(S[L])) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  I := 1;
  while I < L do
  begin
    Inc(I);
    if not (isUriAlphanumChar(S[I]) or (S[I] = '-')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriToplabelStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  if L = 0 then
  begin
    Result := False;
    Exit;
  end;
  if not (IsUriAlphaChar(S[1]) and IsUriAlphanumChar(S[L])) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  I := 1;
  while I < L do
  begin
    Inc(I);
    if not (isUriAlphanumChar(S[I]) or (S[I] = '-')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriIPv4addressStr(S: string): Boolean;
var
  DigitNo, ColonNo, I, L: Integer;
  DigitFound: Boolean;
begin
  Result := False;
  L := Length(S);
  I := 0;
  DigitNo := 0;
  ColonNo := 0;
  DigitFound := False;
  while I < L do
  begin
    if IsUriDigitChar(S[I]) then
    begin
      if not DigitFound then
      begin
        DigitFound := True;
        Inc(DigitNo);
      end;
    end
    else if S[I] = '.' then
    begin
      if not DigitFound then
        Exit;
      DigitFound := False;
      Inc(ColonNo);
    end
    else
      Exit;
  end;
  if (ColonNo = 3) and (DigitNo = 4) then
    Result := True;
end;

function IsUriPortStr(S: string): Boolean;
var
  I, L: Integer;
begin
  Result := True;
  L := Length(S);
  for I := 1 to L do
    if not IsUriDigitChar(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsUriPathStr(S: string): Boolean;
begin
  if IsUriAbs_PathStr(S) or IsUriOpaque_partStr(S) or (S = '') then
    Result := True
  else
    Result := False;
end;

function IsUriPath_segmentsStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  Result := True;
  if L = 0 then
    Exit;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedChar(S[I])
      or (S[I] = ':') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',') or (S[I] = ';') or (S[I] = '/')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriSegmentStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  Result := True;
  if L = 0 then
    Exit;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedChar(S[I])
      or (S[I] = ':') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',') or (S[I] = ';')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriParamStr(S: string): Boolean;
var
  I, L: Integer;
begin
  L := Length(S);
  Result := True;
  if L = 0 then
    Exit;
  I := 0;
  while I < L do
  begin
    Inc(I);
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriUnreservedChar(S[I])
      or (S[I] = ':') or (S[I] = '@') or (S[I] = '&')
      or (S[I] = '=') or (S[I] = '+') or (S[I] = '$')
      or (S[I] = ',')) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriQueryStr(S: string): Boolean;
begin
  if S = '' then
    Result := True
  else
    Result := IsUriUricStr(S);
end;

function IsUriFragmentStr(S: string): Boolean;
begin
  if S = '' then
    Result := True
  else
    Result := IsUriUricStr(S);
end;

function IsUriUricStr(S: string): Boolean;
var
  I, L: Integer;
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
    if S[I] = '%' then
    begin
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
      if I = L then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      if not IsUriHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not (IsUriReservedChar(S[I]) or IsUriUnreservedChar(S[I])) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsUriReservedChar(C: Char): Boolean;
begin
  if (C = ';') or (C = '/') or (C = '?') or (C = ':') or (C = '@') or (C = '&')
    or
    (C = '=') or (C = '+') or (C = '$') or (C = ',') then
    Result := True
  else
    Result := False;
end;

function IsUriUnreservedChar(C: Char): Boolean;
begin
  if IsUriAlphanumChar(C) or IsUriMarkChar(C) then
    Result := True
  else
    Result := False;
end;

function IsUriMarkChar(C: Char): Boolean;
begin
  if (C = '-') or (C = '_') or (C = '.') or (C = '!') or (C = '~') or (C = '*')
    or
    (C = #39) or (C = '(') or (C = ')') then
    Result := True
  else
    Result := False;
end;

function IsUriHexChar(C: Char): Boolean;
begin
  case Byte(C) of
    $30..$39, $41..$46, $61..$66: // 0..9 , A..F , a..f
      Result := True;
  else
    Result := False;
  end;
end;

function IsUriAlphanumChar(C: Char): Boolean;
begin
  case Byte(C) of
    $30..$39, $41..$5A, $61..$7A:
      Result := True;
  else
    Result := False;
  end;
end;

function IsUriAlphaChar(C: Char): Boolean;
begin
  case Byte(C) of
    $41..$5A, $61..$7A:
      Result := True;
  else
    Result := False;
  end;
end;

function IsUriDigitChar(C: Char): Boolean;
begin
  case Byte(C) of
    $30..$39:
      Result := True;
  else
    Result := False;
  end;
end;

function ResolveRelativeUriStr(const BaseUri,
  RelUri: string;
  var ResultUri: string): Boolean;
var
  BaseUriAnalyzer, RelUriAnalyzer: TUriStrAnalyzer;
  I, SlashPos, QueryIndex: Integer;
  PathBuffer: string;
  Segments: TStringList;
begin
  ResultUri := '';
  BaseUriAnalyzer := TUriStrAnalyzer.Create;
  RelUriAnalyzer := TUriStrAnalyzer.Create;
  try
    Result := BaseUriAnalyzer.SetUriReference(BaseUri);
    Result := (RelUriAnalyzer.SetUriReference(RelUri) and Result);
    Result := ((BaseUriAnalyzer.HasUriScheme or RelUriAnalyzer.HasUriScheme) and
      Result);
    if not Result then
      Exit; // BaseUri is not an absolute URI reference, or BaseUri or RelUri is malformed
    with RelUriAnalyzer do
    begin
      if (UriPath = '') and not (HasUriScheme or HasUriAuthority or HasUriQuery)
        then
      begin
        // Same document reference detected
        BaseUriAnalyzer.SetUriFragment(UriFragment, HasUriFragment);
        ResultUri := BaseUriAnalyzer.UriReference;
        Exit;
      end;
      if HasUriScheme then
      begin
        // RelUri is an absolute URI --> we are done.
        ResultUri := RelUri;
        Exit;
      end;
      // inherit scheme:
      SetUriScheme(BaseUriAnalyzer.UriScheme, BaseUriAnalyzer.HasUriScheme);
      if not HasUriAuthority then
      begin
        // inherit Authority:
        SetUriAuthority(BaseUriAnalyzer.UriAuthority,
          BaseUriAnalyzer.HasUriAuthority);
        if not (Copy(UriPath, 1, 1) = '/') then
        begin
          // analyze Paths:
          Segments := TStringList.Create;
          try
            SlashPos := LastDelimiter('/', BaseUriAnalyzer.UriPath);
            if SlashPos > 0 then
              PathBuffer := Copy(BaseUriAnalyzer.UriPath, 2, SlashPos - 1)
                // Copy Path without last segment and first Character which is always '/'
            else
              PathBuffer := '';
            PathBuffer := Concat(PathBuffer, UriPath);
            with Segments do
            begin
              // cut PathBuffer into Segments:
              SlashPos := Pos('/', PathBuffer);
              while SlashPos > 0 do
              begin
                Add(Copy(PathBuffer, 1, SlashPos - 1));
                PathBuffer := Copy(PathBuffer, SlashPos + 1, Length(PathBuffer)
                  - SlashPos);
                SlashPos := Pos('/', PathBuffer);
              end; {while ...}
              Add(PathBuffer);
              if (PathBuffer = '..') or (PathBuffer = '.') then
                Add('');
                  // Necessary to preserve ending '/' under some circumstances
              // remove '.' Segments:
              QueryIndex := IndexOf('.');
              while QueryIndex > -1 do
              begin
                Delete(QueryIndex);
                QueryIndex := IndexOf('.');
              end;
              // remove '<segment>/..' Segments:
              QueryIndex := IndexOf('..');
              while QueryIndex > 0 do
              begin
                Delete(QueryIndex);
                Delete(Pred(QueryIndex));
                QueryIndex := IndexOf('..');
              end;
              // test for malformed Path:
              if Count > 0 then
                if Strings[0] = '..' then
                begin
                  Result := False;
                  Exit;
                end;
              PathBuffer := '';
              for I := 0 to Pred(Count) do
                PathBuffer := Concat(PathBuffer, '/', Strings[I]);
              SetUriPath(PathBuffer);
            end; {with Segments ...}
          finally
            Segments.Free;
          end;
        end; {if not (Copy(UriPath,1,1) = '/') ...}
      end; {if not HasAuthorityScheme ...}
      ResultUri := UriReference;
    end; {with RelUriAnalyzer ...}
  finally
    BaseUriAnalyzer.Free;
    RelUriAnalyzer.Free;
  end;
end;

function ResolveRelativeUriWideStr(const BaseUri,
  RelUri: WideString; var ResultUri: WideString): Boolean;
var
  BaseUriAnalyzer, RelUriAnalyzer: TUriWideStrAnalyzer;
  I, SlashPos, QueryIndex: Integer;
  PathBuffer: WideString;
  Segments: TUtilsWideStringList;
begin
  ResultUri := '';
  BaseUriAnalyzer := TUriWideStrAnalyzer.Create;
  RelUriAnalyzer := TUriWideStrAnalyzer.Create;
  try
    Result := BaseUriAnalyzer.SetUriReference(BaseUri);
    Result := (RelUriAnalyzer.SetUriReference(RelUri) and Result);
    Result := ((BaseUriAnalyzer.HasUriScheme or RelUriAnalyzer.HasUriScheme) and
      Result);
    if not Result then
      Exit; // BaseUri is not an absolute URI reference, or BaseUri or RelUri is malformed
    with RelUriAnalyzer do
    begin
      if (UriPath = '') and not (HasUriScheme or HasUriAuthority or HasUriQuery)
        then
      begin
        // Same document reference detected
        BaseUriAnalyzer.SetUriFragment(UriFragment, HasUriFragment);
        ResultUri := BaseUriAnalyzer.UriReference;
        Exit;
      end;
      if HasUriScheme then
      begin
        // RelUri is an absolute URI --> we are done.
        ResultUri := RelUri;
        Exit;
      end;
      // Inherit scheme:
      SetUriScheme(BaseUriAnalyzer.UriScheme, BaseUriAnalyzer.HasUriScheme);
      if not HasUriAuthority then
      begin
        // Inherit Authority:
        SetUriAuthority(BaseUriAnalyzer.UriAuthority,
          BaseUriAnalyzer.HasUriAuthority);
        if not (Copy(UriPath, 1, 1) = '/') then
        begin
          // Analyze Paths:
          Segments := TUtilsWideStringList.Create;
          try
            SlashPos := LastDelimiter('/', BaseUriAnalyzer.UriPath);
            if SlashPos > 0 then
              PathBuffer := Copy(BaseUriAnalyzer.UriPath, 2, SlashPos - 1)
                // Copy Path without last segment and first Character which is always '/'
            else
              PathBuffer := '';
            PathBuffer := Concat(PathBuffer, UriPath);
            with Segments do
            begin
              // Cut PathBuffer into Segments:
              SlashPos := Pos('/', PathBuffer);
              while SlashPos > 0 do
              begin
                Add(Copy(PathBuffer, 1, SlashPos - 1));
                PathBuffer := Copy(PathBuffer, SlashPos + 1, Length(PathBuffer)
                  - SlashPos);
                SlashPos := Pos('/', PathBuffer);
              end; {while ...}
              Add(PathBuffer);
              if (PathBuffer = '..') or (PathBuffer = '.') then
                Add('');
                  // Necessary to preserve ending '/' under some circumstances
              // Remove '.' Segments:
              QueryIndex := IndexOf('.');
              while QueryIndex > -1 do
              begin
                Delete(QueryIndex);
                QueryIndex := IndexOf('.');
              end;
              // Remove '<segment>/..' Segments:
              QueryIndex := IndexOf('..');
              while QueryIndex > 0 do
              begin
                Delete(QueryIndex);
                Delete(Pred(QueryIndex));
                QueryIndex := IndexOf('..');
              end;
              // Test for malformed Path:
              if Count > 0 then
                if WideStrings[0] = '..' then
                begin
                  Result := False;
                  Exit;
                end;
              PathBuffer := '';
              for I := 0 to Pred(Count) do
                PathBuffer := Concat(PathBuffer, '/', WideStrings[I]);
              SetUriPath(PathBuffer);
            end; {with Segments ...}
          finally
            Segments.Free;
          end;
        end; {if not (Copy(UriPath,1,1) = '/') ...}
      end; {if not HasAuthorityScheme ...}
      ResultUri := UriReference;
    end; {with RelUriAnalyzer ...}
  finally
    BaseUriAnalyzer.Free;
    RelUriAnalyzer.Free;
  end;
end;

function FilenameToUriStr(const Path: TFilename;
  const Opt: TUtilsFilenameToUriOptions): string;
var
  I, L: Integer;
begin
  if fuSetLocalhost in Opt then
    Result := 'file://localhost'
  else
    Result := 'file://';
  L := Length(Path);
  if L > 0 then
  begin
    // Add leading '/':
    Result := Concat(Result, '/');
    I := 1;
    while I <= L do
    begin
      case Byte(Path[I]) of
        // A-z      a-z         0-9    !     '()*      -   .    _    ~
        $41..$5A, $61..$7A, $30..$39, $21, $27..$2A, $2D, $2E, $5F, $7E:
          Result := Concat(Result, Path[I]);
        // special treatment for colons (':'):
        $3A:
          if fuPlainColon in Opt then
            Result := Concat(Result, ':')
          else
            Result := Concat(Result, '%3a');
{$IFDEF LINUX}
        // keep '/' in Linux filenames.
        $2F:
          Result := Concat(Result, '/');
{$ELSE}
        // translate '\' to '/' in Windows filenames:
        $5C:
          Result := Concat(Result, '/');
{$ENDIF}
      else
        // calculate escape sequence:
        Result := Concat(Result, '%', IntToHex(Byte(Path[I]), 2));
      end;
      Inc(I);
    end; {while ...}
  end; {if ...}
end;

function FilenameToUriWideStr(const Path: TFilename;
  const Opt: TUtilsFilenameToUriOptions): WideString;
var
  I, L: Integer;
begin
  if fuSetLocalhost in Opt then
    Result := 'file://localhost'
  else
    Result := 'file://';
  L := Length(Path);
  if L > 0 then
  begin
    // Add leading '/':
    Result := Concat(Result, '/');
    I := 1;
    while I <= L do
    begin
      case Byte(Path[I]) of
        // A-z       a-z       0-9     !     '()*     -    .    _    ~
        $41..$5A, $61..$7A, $30..$39, $21, $27..$2A, $2D, $2E, $5F, $7E:
          Result := Concat(Result, WideString(WideChar(Byte(Path[I]))));
        // special treatment for colons (':'):
        $3A:
          if fuPlainColon in Opt then
            Result := Concat(Result, ':')
          else
            Result := Concat(Result, '%3a');
{$IFDEF LINUX}
        // keep '/' in Linux filenames.
        $2F:
          Result := Concat(Result, '/');
{$ELSE}
        // translate '\' to '/' in Windows filenames:
        $5C:
          Result := Concat(Result, '/');
{$ENDIF}
      else
        // calculate escape sequence:
        Result := Concat(Result, '%', IntToHex(Byte(Path[I]), 2));
      end;
      Inc(I);
    end; {while ...}
  end; {if ...}
end;

function UriStrToFilename(const Uri: string;
  var Path: TFilename;
  var Authority,
  Query,
  Fragment: string): Boolean;
var
  UriAnalyzer: TUriStrAnalyzer;
  PathBuffer: string; // Used to increase performance
  I, L: Integer;
begin
  Path := '';
  Query := '';
  Fragment := '';
  Result := False;
  UriAnalyzer := TUriStrAnalyzer.Create;
  try
    with UriAnalyzer do
    begin
      if SetUriReference(uri) then
      begin
        if CompareText(UriScheme, 'file') = 0 then
        begin
          Result := True;
          PathBuffer := UriPath;
          L := Length(PathBuffer);
          if L > 0 then
          begin
            // remove leading '/':
            Dec(L);
            PathBuffer := Copy(PathBuffer, 2, L);
            I := 1;
            while I <= L do
            begin
              if PathBuffer[I] = '%' then
              begin
                // Resolve escape sequence:
                Path := Concat(Path, Chr(StrToInt(Concat('x', PathBuffer[I + 1],
                  PathBuffer[I + 2]))));
                I := I + 2;
              end
{$IFNDEF LINUX}
              // translate '/' to '\' for Windows filenames:
              else if PathBuffer[I] = '/' then
              begin
                Path := Concat(Path, '\');
              end
{$ENDIF}
              else
                Path := Concat(Path, PathBuffer[I]);
              Inc(I);
            end; {while ...}
          end; {if ...}
          Authority := UriAuthority;
          if HasUriQuery then
            Query := Concat('?', UriQuery);
          if HasUriFragment then
            Fragment := Concat('#', UriFragment);
        end; {if ...}
      end; {if ...}
    end; {with ...}
  finally
    UriAnalyzer.Free;
  end;
end;

function UriWideStrToFilename(const Uri: WideString;
  var Path: TFilename;
  var Authority,
  Query,
  Fragment: WideString): Boolean;
var
  UriAnalyzer: TUriWideStrAnalyzer;
  PathBuffer: WideString; // Used to increase performance
  I, L: Integer;
begin
  Path := '';
  Query := '';
  Fragment := '';
  Result := False;
  UriAnalyzer := TUriWideStrAnalyzer.Create;
  try
    with UriAnalyzer do
    begin
      if SetUriReference(Uri) then
      begin
        if CompareText(UriScheme, 'file') = 0 then
        begin
          Result := True;
          PathBuffer := UriPath;
          L := Length(PathBuffer);
          if L > 0 then
          begin
            // remove leading '/':
            Dec(L);
            PathBuffer := Copy(PathBuffer, 2, L);
            I := 1;
            while I <= L do
            begin
              if PathBuffer[I] = '%' then
              begin
                // Resolve escape sequence:
                Path := Concat(Path, Chr(StrToInt(Concat(WideString('x'), PathBuffer[I + 1],
                  PathBuffer[I + 2]))));
                I := I + 2;
              end
{$IFNDEF LINUX}
              // translate '/' to '\' for Windows filenames:
              else if PathBuffer[I] = '/' then
              begin
                Path := Concat(Path, '\');
              end
{$ENDIF}
              else
                Path := Concat(Path, PathBuffer[I]);
              Inc(I);
            end; {while ...}
          end; {if ...}
          Authority := UriAuthority;
          if HasUriQuery then
            Query := Concat('?', UriQuery);
          if HasUriFragment then
            Fragment := Concat('#', UriFragment);
        end; {if ...}
      end; {if ...}
    end; {with ...}
  finally
    UriAnalyzer.Free;
  end;
end;

{ TUriStrAnalyzer }

constructor TUriStrAnalyzer.Create;
begin
  inherited Create;
  SetUriReference('');
end;

function TUriStrAnalyzer.GetUriReference: string;
begin
  Result := '';
  if FHasUriScheme then
    Result := Concat(Result, FUriScheme, ':');
  if FHasUriAuthority then
    Result := Concat(Result, '//', FUriAuthority);
  Result := Concat(Result, FUriPath);
  if FHasUriQuery then
    Result := Concat(Result, '?', FUriQuery);
  if FHasUriFragment then
    Result := Concat(Result, '#', FUriFragment);
end;

function TUriStrAnalyzer.SetUriAuthority(const Value: string;
  const IsDefined: Boolean): Boolean;
begin
  Result := True;
  FHasUriAuthority := IsDefined;
  if IsDefined then
  begin
    if IsUriAuthorityStr(Value) then
      FUriAuthority := Value
    else
    begin
      FUriAuthority := '';
      Result := False;
    end;
  end
  else
    FUriAuthority := '';
end;

function TUriStrAnalyzer.SetUriFragment(const Value: string;
  const IsDefined: Boolean): Boolean;
begin
  Result := True;
  FHasUriFragment := IsDefined;
  if IsDefined then
  begin
    if IsUriFragmentStr(Value) then
      FUriFragment := Value
    else
    begin
      FUriFragment := '';
      Result := False;
    end;
  end
  else
    FUriFragment := '';
end;

function TUriStrAnalyzer.SetUriPath(const Value: string): Boolean;
begin
  Result := IsUriPathStr(Value);
  if Result then
    FUriPath := Value
  else
    FUriPath := '';
end;

function TUriStrAnalyzer.SetUriQuery(const Value: string;
  const IsDefined: Boolean): Boolean;
begin
  Result := True;
  FHasUriQuery := IsDefined;
  if IsDefined then
  begin
    if IsUriQueryStr(Value) then
      FUriQuery := Value
    else
    begin
      FUriQuery := '';
      Result := False;
    end;
  end
  else
    FUriQuery := '';
end;

function TUriStrAnalyzer.SetUriReference(const Value: string): Boolean;
var
  ColonPos, DcPos, QmPos, SlashPos: Integer;
  S: string;
begin
  ColonPos := Pos(':', Value);
  Result := SetUriScheme(Copy(Value, 1, ColonPos - 1), (ColonPos > 0));
  S := Copy(Value, ColonPos + 1, Length(Value) - ColonPos);

  DcPos := Pos('#', S);
  if DcPos > 0 then
  begin
    Result := (SetUriFragment(Copy(S, DcPos + 1, Length(S) - DcPos), True) and
      Result);
    S := Copy(S, 1, DcPos - 1);
  end
  else
    SetUriFragment('', False);

  QmPos := Pos('?', S);
  if QmPos > 0 then
  begin
    Result := (SetUriQuery(Copy(S, QmPos + 1, Length(S) - QmPos), True) and
      Result);
    S := Copy(S, 1, QmPos - 1);
  end
  else
    SetUriQuery('', False);

  if Copy(S, 1, 2) = '//' then
  begin
    S := Copy(S, 3, Length(S) - 2);
    SlashPos := Pos('/', S);
    if SlashPos > 0 then
    begin
      Result := (SetUriAuthority(Copy(S, 1, SlashPos - 1), True) and Result);
      S := Copy(S, SlashPos, Length(S) - SlashPos + 1);
    end
    else
    begin
      Result := (SetUriAuthority(S, True) and Result);
      S := '';
    end;
  end
  else
    SetUriAuthority('', False);

  Result := SetUriPath(S) and Result;

  if not Result then
    SetUriReference('');
end;

function TUriStrAnalyzer.SetUriScheme(const Value: string;
  const IsDefined: Boolean): Boolean;
begin
  Result := True;
  FHasUriScheme := IsDefined;
  if IsDefined then
  begin
    if IsUriSchemeStr(Value) then
      FUriScheme := Value
    else
    begin
      FUriScheme := '';
      Result := False;
    end;
  end
  else
    FUriScheme := '';
end;

{ TUriWideStrAnalyzer }

constructor TUriWideStrAnalyzer.Create;
begin
  inherited Create;
  SetUriReference('');
end;

function TUriWideStrAnalyzer.GetUriReference: WideString;
begin
  Result := '';
  if FHasUriScheme then
    Result := Concat(Result, FUriScheme, ':');
  if FHasUriAuthority then
    Result := Concat(Result, '//', FUriAuthority);
  Result := Concat(Result, FUriPath);
  if FHasUriQuery then
    Result := Concat(Result, #63, FUriQuery);
  if FHasUriFragment then
    Result := Concat(Result, '#', FUriFragment);
end;

function TUriWideStrAnalyzer.SetUriAuthority(const Value: WideString;
  const IsDefined: Boolean): Boolean;
begin
  Result := True;
  FHasUriAuthority := IsDefined;
  if IsDefined then
  begin
    if IsUriAuthorityWideStr(Value) then
      FUriAuthority := Value
    else
    begin
      FUriAuthority := '';
      Result := False;
    end;
  end
  else
    FUriAuthority := '';
end;

function TUriWideStrAnalyzer.SetUriFragment(const Value: WideString;
  const IsDefined: Boolean): Boolean;
begin
  Result := True;
  FHasUriFragment := IsDefined;
  if IsDefined then
  begin
    if IsUriFragmentWideStr(Value) then
      FUriFragment := Value
    else
    begin
      FUriFragment := '';
      Result := False;
    end;
  end
  else
    FUriFragment := '';
end;

function TUriWideStrAnalyzer.SetUriPath(const Value: WideString): Boolean;
begin
  Result := IsUriPathWideStr(Value);
  if Result then
    FUriPath := Value
  else
    FUriPath := '';
end;

function TUriWideStrAnalyzer.SetUriQuery(const Value: WideString;
  const IsDefined: Boolean): Boolean;
begin
  Result := True;
  FHasUriQuery := IsDefined;
  if IsDefined then
  begin
    if IsUriQueryWideStr(Value) then
      FUriQuery := Value
    else
    begin
      FUriQuery := '';
      Result := False;
    end;
  end
  else
    FUriQuery := '';
end;

function TUriWideStrAnalyzer.SetUriReference(const Value: WideString): Boolean;
var
  ColonPos, DcPos, QmPos, SlashPos: Integer;
  S: WideString;
begin
  ColonPos := Pos(':', Value);
  Result := SetUriScheme(Copy(Value, 1, ColonPos - 1), (ColonPos > 0));
  S := Copy(Value, ColonPos + 1, Length(Value) - ColonPos);

  DcPos := Pos('#', S);
  if DcPos > 0 then
  begin
    Result := (SetUriFragment(Copy(S, DcPos + 1, Length(S) - DcPos), True) and
      Result);
    S := Copy(S, 1, DcPos - 1);
  end
  else
    SetUriFragment('', False);

  QmPos := Pos('?', S);
  if QmPos > 0 then
  begin
    Result := (SetUriQuery(Copy(S, QmPos + 1, Length(S) - QmPos), True) and
      Result);
    S := Copy(S, 1, QmPos - 1);
  end
  else
    SetUriQuery('', False);

  if Copy(S, 1, 2) = '//' then
  begin
    S := Copy(S, 3, Length(S) - 2);
    SlashPos := Pos('/', S);
    if SlashPos > 0 then
    begin
      Result := (SetUriAuthority(Copy(S, 1, SlashPos - 1), True) and Result);
      S := Copy(S, SlashPos, Length(S) - SlashPos + 1);
    end
    else
    begin
      Result := (SetUriAuthority(S, True) and Result);
      S := '';
    end;
  end
  else
    SetUriAuthority('', False);

  Result := SetUriPath(S) and Result;

  if not Result then
    SetUriReference('');
end;

function TUriWideStrAnalyzer.SetUriScheme(const Value: WideString;
  const IsDefined: Boolean): Boolean;
begin
  Result := True;
  FHasUriScheme := IsDefined;
  if IsDefined then
  begin
    if IsUriSchemeWideStr(Value) then
      FUriScheme := Value
    else
    begin
      FUriScheme := '';
      Result := False;
    end;
  end
  else
    FUriScheme := '';
end;

end.

