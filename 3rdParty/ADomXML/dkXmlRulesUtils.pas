unit dkXmlRulesUtils;

{$I ADOMXMLWarn.inc}

// dkXmlRulesUtils 1.0.7
// Delphi 4 to 2009 and Kylix 3 Implementation
// February 2009
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
// The Original Code is "dkXmlRulesUtils.pas".
//
// The Initial Developer of the Original Code is Dieter Köhler (Heidelberg,
// Germany, "http://www.philo.de/"). Portions created by the Initial Developer
// are Copyright (C) 2003-2009 Dieter Köhler. All Rights Reserved.
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
// 2009-02-23 1.0.7 Deprecated marks excluded for Delphi 4 and 5.
// 2008-12-03 1.0.6 Rules for obsolet XML character classes marked as deprecated.
// 2008-11-29 1.0.5 Implementing changes in XML specification, 5th edition.
// 2007-12-03 1.0.4 Made .NET compliant.
// 2004-09-19 1.0.3 Bug fix in IsXmlNmtokens and IsXmlNames.
// 2004-06-01 1.0.2 Small revisions.
// 2003-11-16 1.0.1 Small revisions.
// 2003-08-03 1.0.0

{$IFDEF VER120}
  {$DEFINE NO_DEPRECATED}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE NO_DEPRECATED}
{$ENDIF}

interface

function GetXmlWhitespaceWideString: WideString;

function IsXmlBaseCharCodePoint(const UCS4: Longint): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlCharCodePoint(const UCS4: Longint): Boolean;
function IsXmlCombiningCharCodePoint(const UCS4: Longint): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlDecDigitCodePoint(const UCS4: Longint): Boolean;
function IsXmlDigitCodePoint(const UCS4: Longint): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlEncNameFollowingCharCodePoint(const UCS4: Longint): Boolean;
function IsXmlEncNameLeadingCharCodePoint(const UCS4: Longint): Boolean;
function IsXmlExtenderCodePoint(const UCS4: Longint): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlHexDigitCodePoint(const UCS4: Longint): Boolean;
function IsXmlIdeographicCodePoint(const UCS4: Longint): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlLetterCodePoint(const UCS4: Longint): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlNameCharCodePoint(const UCS4: Longint): Boolean;
function IsXmlNameStartCharCodePoint(const UCS4: Longint): Boolean;
function IsXmlWhiteSpaceCodePoint(const UCS4: Longint): Boolean;
function IsXmlWhiteSpaceOrNullCodePoint(const UCS4: Longint): Boolean;

function IsXmlBaseChar(const S: WideChar): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlChar(const S: WideChar): Boolean;
function IsXmlCombiningChar(const S: WideChar): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlDigit(const S: WideChar): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlExtender(const S: WideChar): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlIdeographic(const S: WideChar): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlLetter(const S: WideChar): Boolean; {$IFNDEF NO_DEPRECATED}deprecated;{$ENDIF}
function IsXmlNameChar(const S: WideChar): Boolean;
function IsXmlNameStartChar(const S: WideChar): Boolean;
function IsXmlPubidChar(const S: WideChar): Boolean;
function IsXmlWhiteSpace(const S: WideChar): Boolean;
function IsXmlWhiteSpaceOrNull(const S: WideChar): Boolean;

function IsXmlAttValue(const S: WideString): Boolean;
function IsXmlCData(const S: WideString): Boolean;
function IsXmlCharData(const S: WideString): Boolean;
function IsXmlCharRef(const S: WideString): Boolean;
function IsXmlChars(const S: WideString): Boolean;
function IsXmlComment(const S: WideString): Boolean;
function IsXmlEncName(const S: WideString): Boolean;
function IsXmlEntityRef(const S: WideString): Boolean;
function IsXmlEntityValue(const S: WideString): Boolean;
function IsXmlEntityValueChars(const S: WideString): Boolean;
function IsXmlName(const S: WideString): Boolean;
function IsXmlNames(const S: WideString): Boolean;
function IsXmlNmtoken(const S: WideString): Boolean;
function IsXmlNmtokens(const S: WideString): Boolean;
function IsXmlPEReference(const S: WideString): Boolean;
function IsXmlPITarget(const S: WideString): Boolean;
function IsXmlPredefinedEntityName(const S: WideString): Boolean;
function IsXmlPubidChars(const S: WideString): Boolean;
function IsXmlPubidLiteral(const S: WideString): Boolean;
function IsXmlReference(const S: WideString): Boolean;
function IsXmlS(const S: WideString): Boolean;
function IsXmlStringType(const S: WideString): Boolean;
function IsXmlSystemChars(const S: WideString): Boolean;
function IsXmlSystemLiteral(const S: WideString): Boolean;
function IsXmlTokenizedType(const S: WideString): Boolean;
function IsXmlVersionNum(const S: WideString): Boolean;

function IsXmlDefaultAttName(const S: WideString): Boolean;
function IsXmlLocalPart(const S: WideString): Boolean;
function IsXmlNCName(const S: WideString): Boolean;
function IsXmlNCNameChar(const S: WideChar): Boolean;
function IsXmlNCNameStartChar(const S: WideChar): Boolean;
function IsXmlNSAttName(const S: WideString): Boolean;
function IsXmlPrefix(const S: WideString): Boolean;
function IsXmlPrefixedAttName(const S: WideString): Boolean;
function IsXmlQName(const S: WideString): Boolean;

implementation

uses
  dkWideStringUtils;

function GetXmlWhitespaceWideString: WideString;
begin
  Result := #$0020#$000D#$000A#$0009;
end;

function IsXmlCharCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $0009, $000A, $000D, $0020..$D7FF, $E000..$FFFD, $10000..$10FFFF:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlWhiteSpaceCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $0009, $000A, $000D, $0020:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlWhiteSpaceOrNullCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $0000, $0009, $000A, $000D, $0020:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlLetterCodePoint(const UCS4: Longint): Boolean;
begin
  Result := IsXmlIdeographicCodePoint(UCS4) or IsXmlBaseCharCodePoint(UCS4);
end;

function IsXmlBaseCharCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $0041..$005A, $0061..$007A, $00C0..$00D6, $00D8..$00F6, $00F8..$00FF,
    $0100..$0131, $0134..$013E, $0141..$0148, $014A..$017E, $0180..$01C3,
    $01CD..$01F0, $01F4..$01F5, $01FA..$0217, $0250..$02A8, $02BB..$02C1,
    $0386, $0388..$038A, $038C, $038E..$03A1, $03A3..$03CE, $03D0..$03D6,
    $03DA, $03DC, $03DE, $03E0, $03E2..$03F3, $0401..$040C, $040E..$044F,
    $0451..$045C, $045E..$0481, $0490..$04C4, $04C7..$04C8, $04CB..$04CC,
    $04D0..$04EB, $04EE..$04F5, $04F8..$04F9, $0531..$0556, $0559,
    $0561..$0586, $05D0..$05EA, $05F0..$05F2, $0621..$063A, $0641..$064A,
    $0671..$06B7, $06BA..$06BE, $06C0..$06CE, $06D0..$06D3, $06D5,
    $06E5..$06E6, $0905..$0939, $093D, $0958..$0961, $0985..$098C,
    $098F..$0990, $0993..$09A8, $09AA..$09B0, $09B2, $09B6..$09B9,
    $09DC..$09DD, $09DF..$09E1, $09F0..$09F1, $0A05..$0A0A, $0A0F..$0A10,
    $0A13..$0A28, $0A2A..$0A30, $0A32..$0A33, $0A35..$0A36, $0A38..$0A39,
    $0A59..$0A5C, $0A5E, $0A72..$0A74, $0A85..$0A8B, $0A8D, $0A8F..$0A91,
    $0A93..$0AA8, $0AAA..$0AB0, $0AB2..$0AB3, $0AB5..$0AB9, $0ABD, $0AE0,
    $0B05..$0B0C, $0B0F..$0B10, $0B13..$0B28, $0B2A..$0B30, $0B32..$0B33,
    $0B36..$0B39, $0B3D, $0B5C..$0B5D, $0B5F..$0B61, $0B85..$0B8A,
    $0B8E..$0B90, $0B92..$0B95, $0B99..$0B9A, $0B9C, $0B9E..$0B9F,
    $0BA3..$0BA4, $0BA8..$0BAA, $0BAE..$0BB5, $0BB7..$0BB9, $0C05..$0C0C,
    $0C0E..$0C10, $0C12..$0C28, $0C2A..$0C33, $0C35..$0C39, $0C60..$0C61,
    $0C85..$0C8C, $0C8E..$0C90, $0C92..$0CA8, $0CAA..$0CB3, $0CB5..$0CB9,
    $0CDE, $0CE0..$0CE1, $0D05..$0D0C, $0D0E..$0D10, $0D12..$0D28,
    $0D2A..$0D39, $0D60..$0D61, $0E01..$0E2E, $0E30, $0E32..$0E33,
    $0E40..$0E45, $0E81..$0E82, $0E84, $0E87..$0E88, $0E8A, $0E8D,
    $0E94..$0E97, $0E99..$0E9F, $0EA1..$0EA3, $0EA5, $0EA7, $0EAA..$0EAB,
    $0EAD..$0EAE, $0EB0, $0EB2..$0EB3, $0EBD, $0EC0..$0EC4, $0F40..$0F47,
    $0F49..$0F69, $10A0..$10C5, $10D0..$10F6, $1100, $1102..$1103,
    $1105..$1107, $1109, $110B..$110C, $110E..$1112, $113C, $113E, $1140,
    $114C, $114E, $1150, $1154..$1155, $1159, $115F..$1161, $1163, $1165,
    $1167, $1169, $116D..$116E, $1172..$1173, $1175, $119E, $11A8, $11AB,
    $11AE..$11AF, $11B7..$11B8, $11BA, $11BC..$11C2, $11EB, $11F0, $11F9,
    $1E00..$1E9B, $1EA0..$1EF9, $1F00..$1F15, $1F18..$1F1D, $1F20..$1F45,
    $1F48..$1F4D, $1F50..$1F57, $1F59, $1F5B, $1F5D, $1F5F..$1F7D,
    $1F80..$1FB4, $1FB6..$1FBC, $1FBE, $1FC2..$1FC4, $1FC6..$1FCC,
    $1FD0..$1FD3, $1FD6..$1FDB, $1FE0..$1FEC, $1FF2..$1FF4, $1FF6..$1FFC,
    $2126, $212A..$212B, $212E, $2180..$2182, $3041..$3094, $30A1..$30FA,
    $3105..$312C, $AC00..$D7A3:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlHexDigitCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $0030..$0039, $0041..$0046, $0061..$0066: // 0..9, A..F, a..f
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlIdeographicCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $4E00..$9FA5, $3007, $3021..$3029:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlCombiningCharCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $0300..$0345, $0360..$0361, $0483..$0486, $0591..$05A1, $05A3..$05B9,
    $05BB..$05BD, $05BF, $05C1..$05C2, $05C4, $064B..$0652, $0670,
    $06D6..$06DC, $06DD..$06DF, $06E0..$06E4, $06E7..$06E8, $06EA..$06ED,
    $0901..$0903, $093C, $093E..$094C, $094D, $0951..$0954, $0962..$0963,
    $0981..$0983, $09BC, $09BE, $09BF, $09C0..$09C4, $09C7..$09C8,
    $09CB..$09CD, $09D7, $09E2..$09E3, $0A02, $0A3C, $0A3E, $0A3F,
    $0A40..$0A42, $0A47..$0A48, $0A4B..$0A4D, $0A70..$0A71, $0A81..$0A83,
    $0ABC, $0ABE..$0AC5, $0AC7..$0AC9, $0ACB..$0ACD, $0B01..$0B03, $0B3C,
    $0B3E..$0B43, $0B47..$0B48, $0B4B..$0B4D, $0B56..$0B57, $0B82..$0B83,
    $0BBE..$0BC2, $0BC6..$0BC8, $0BCA..$0BCD, $0BD7, $0C01..$0C03,
    $0C3E..$0C44, $0C46..$0C48, $0C4A..$0C4D, $0C55..$0C56, $0C82..$0C83,
    $0CBE..$0CC4, $0CC6..$0CC8, $0CCA..$0CCD, $0CD5..$0CD6, $0D02..$0D03,
    $0D3E..$0D43, $0D46..$0D48, $0D4A..$0D4D, $0D57, $0E31, $0E34..$0E3A,
    $0E47..$0E4E, $0EB1, $0EB4..$0EB9, $0EBB..$0EBC, $0EC8..$0ECD,
    $0F18..$0F19, $0F35, $0F37, $0F39, $0F3E, $0F3F, $0F71..$0F84,
    $0F86..$0F8B, $0F90..$0F95, $0F97, $0F99..$0FAD, $0FB1..$0FB7, $0FB9,
    $20D0..$20DC, $20E1, $302A..$302F, $3099, $309A:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlDecDigitCodePoint(const UCS4: Longint): Boolean;
begin
  if (UCS4 >= $0030) and (UCS4 <= $0039) {// 0..9} then
    Result := True
  else
    Result := False;
end;

function IsXmlDigitCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $0030..$0039, $0660..$0669, $06F0..$06F9, $0966..$096F, $09E6..$09EF,
    $0A66..$0A6F, $0AE6..$0AEF, $0B66..$0B6F, $0BE7..$0BEF, $0C66..$0C6F,
    $0CE6..$0CEF, $0D66..$0D6F, $0E50..$0E59, $0ED0..$0ED9, $0F20..$0F29:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlExtenderCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    $00B7, $02D0, $02D1, $0387, $0640, $0E46, $0EC6, $3005, $3031..$3035,
    $309D..$309E, $30FC..$30FE:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlNameCharCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    // [-] , [.] ,   [0..9]   ,
    $002D, $002E, $0030..$0039, $00B7, $0300..$036F, $203F..$2040:
      Result := True;
  else
    Result := IsXmlNameStartCharCodePoint(UCS4);
  end;
end;

function IsXmlNameStartCharCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    // [:] ,   [A..Z]   , [_] ,   [a..z]
    $003A, $0041..$005A, $005F, $0061..$007A,
    $00C0..$00D6, $00D8..$00F6, $00F8..$02FF, $0370..$037D, $037F..$1FFF,
    $200C..$200D, $2070..$218F, $2C00..$2FEF, $3001..$D7FF, $F900..$FDCF,
    $FDF0..$FFFD, $10000..$EFFFF:
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlEncNameLeadingCharCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    //   [A..Z]  ,   [a..z]
    $0041..$005A, $0061..$007A:
      Result := True
  else
    Result := False;
  end;
end;

function IsXmlEncNameFollowingCharCodePoint(const UCS4: Longint): Boolean;
begin
  case UCS4 of
    // [-] , [.] ,   [0..9]   ,   [A..Z]   , [_] ,   [a..z]
    $002D, $002E, $0030..$0039, $0041..$005A, $005F, $0061..$007A:
      Result := True
  else
    Result := False;
  end;
end;

function IsXmlChar(const S: WideChar): Boolean;
begin
  case Word(S) of
    $0009, $000A, $000D, $0020..$FFFD: // Unicode below $10000 or surrogate
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlWhiteSpace(const S: WideChar): Boolean;
begin
  Result := IsXmlWhiteSpaceCodePoint(word(S));
end;

function IsXmlWhiteSpaceOrNull(const S: WideChar): Boolean;       
begin
  Result := IsXmlWhiteSpaceOrNullCodePoint(word(S));
end;

function IsXmlLetter(const S: WideChar): Boolean;
begin
  Result := IsXmlIdeographic(S) or IsXmlBaseChar(S);
end;

function IsXmlBaseChar(const S: WideChar): Boolean;
begin
  Result := IsXmlBaseCharCodePoint(word(S));
end;

function IsXmlIdeographic(const S: WideChar): Boolean;
begin
  Result := IsXmlIdeographicCodePoint(word(S));
end;

function IsXmlCombiningChar(const S: WideChar): Boolean;
begin
  Result := IsXmlCombiningCharCodePoint(word(S));
end;

function IsXmlDigit(const S: WideChar): Boolean;
begin
  Result := IsXmlDigitCodePoint(word(S));
end;

function IsXmlExtender(const S: WideChar): Boolean;
begin
  Result := IsXmlExtenderCodePoint(word(S));
end;

function IsXmlNameChar(const S: WideChar): Boolean;
begin
  case Word(S) of
    // [-] , [.] ,   [0..9]   ,
    $002D, $002E, $0030..$0039, $00B7, $0300..$036F, $203F..$2040:
      Result := True;
  else
    Result := IsXmlNameStartChar(S);
  end;
end;

function IsXmlNameStartChar(const S: WideChar): Boolean;
begin
  case Word(S) of
    // [:] ,   [A..Z]   , [_] ,   [a..z]
    $003A, $0041..$005A, $005F, $0061..$007A,
    $00C0..$00D6, $00D8..$00F6, $00F8..$02FF, $0370..$037D, $037F..$1FFF,
    $200C..$200D, $2070..$218F, $2C00..$2FEF, $3001..$DFFF, $F900..$FDCF,
    $FDF0..$FFFD:  // Surrogates ($D800..$DFFF) are acceptable.
      Result := True;
  else
    Result := False;
  end;
end;

function IsXmlPubidChar(const S: WideChar): Boolean;
begin
  if (S = #$20) or (S = #$D) or (S = #$A) or
    ((S >= 'a') and (S <= 'z')) or
    ((S >= 'A') and (S <= 'Z')) or
    ((S >= '0') and (S <= '9')) or
    (S = '-') or (S = #$27) or (S = '(') or (S = ')') or (S = '+') or (S = ',')
      or
    (S = '.') or (S = '/') or (S = ':') or (S = '=') or (S = WideChar('?')) or (S
      = ';') or
    (S = '!') or (S = '*') or (S = '#') or (S = '@') or (S = '$') or (S = '_')
      or
    (S = '%') then
    Result := True
  else
    Result := False;
end;

function IsXmlChars(const S: WideString): Boolean;
var
  C: Word;
  I, L: Integer;
begin
  Result := True;
  I := 0;
  L := Length(S);
  while I < L do
  begin
    Inc(I);
    C := Ord(S[I]);
    case C of
      $0009, $000A, $000D, $0020..$D7FF, $E000..$FFFD: // Unicode below $FFFF
        ; // do nothing.
      $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
        begin
          if I = L then
          begin
            Result := False;
            Break;
          end; // End of WideString --> No low surrogate found
          Inc(I);
          C := Ord(S[I]);
          if not IsUtf16LowSurrogate(C) then
          begin
            Result := False;
            Break;
          end; // No low surrogate found
        end;
    else
      begin
        Result := False;
        Break;
      end;
    end; {case ...}
  end; {while ...}
end;

function IsXmlS(const S: WideString): Boolean;
var
  I: Integer;
begin
  if Length(S) = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for I := 1 to Length(S) do
    if not IsXmlWhiteSpace(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsXmlName(const S: WideString): Boolean;
var
  C: Word;
  I: Integer;
  NameStart: Boolean;
begin
  if Length(S) = 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  NameStart := True;
  I := 0;
  while I < Length(S) do
  begin
    Inc(I);
    C := Ord(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I = Length(S) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Ord(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
      NameStart := False;
    end
    else if NameStart then begin
      if not IsXmlNameStartChar(WideChar(C)) then
      begin
        Result := False;
        Exit;
      end;
      NameStart := False;
    end else begin
      if not IsXmlNameChar(WideChar(C)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function IsXmlNames(const S: WideString): Boolean;
const
  SPACE: WideChar = #$20;
var
  C: Word;
  I: Integer;
  SpaceFound: Boolean;
begin
  if Length(S) = 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  SpaceFound := True;
  I := 0;
  while I < Length(S) do
  begin
    Inc(I);
    C := Ord(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I = Length(S) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Ord(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
      SpaceFound := False;
    end
    else if SpaceFound then begin
      if not IsXmlNameStartChar(WideChar(C)) then
      begin
        Result := False;
        Exit;
      end;
      SpaceFound := False;
    end else begin
      if S[I] = SPACE then
      begin
        SpaceFound := True;
      end
      else if not IsXmlNameChar(WideChar(C)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
  if SpaceFound then
    Result := False;
end;

function IsXmlNmtoken(const S: WideString): Boolean;
var
  C: Word;
  I: Integer;
begin
  if Length(S) = 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  I := 0;
  while I < Length(S) do
  begin
    Inc(I);
    C := Ord(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I = Length(S) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Ord(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
    end else begin
      if not IsXmlNameChar(WideChar(C)) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function IsXmlNmtokens(const S: WideString): Boolean;
const
  SPACE: WideChar = #$20;
var
  C: Word;
  I: Integer;
  SpaceFound: Boolean;
begin
  if Length(S) = 0 then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  SpaceFound := True;
  I := 0;
  while I < Length(S) do
  begin
    Inc(I);
    C := Ord(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I = Length(S) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Ord(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
      SpaceFound := False;
    end
    else if S[I] = SPACE then
    begin
      if SpaceFound then
      begin
        Result := False;
        Exit;
      end;
      SpaceFound := True;
    end
    else if IsXmlNameChar(WideChar(C)) then
    begin
      SpaceFound := False;
    end
    else
    begin
      Result := False;
      Exit;
    end;
  end;
  if SpaceFound then
    Result := False;
end;

function IsXmlCharRef(const S: WideString): Boolean;
var
  C: WideChar;
  I: Integer;
begin
  Result := True;
  if Copy(S, Length(S), 1) <> ';' then
  begin
    Result := False;
    Exit;
  end;
  if Copy(S, 1, 3) = '&#x' then
  begin
    if Length(S) < 5 then
    begin
      Result := False;
      Exit;
    end;
    for I := 4 to Length(S) - 1 do
    begin
      C := S[I];
      if not ((C >= '0') and (C <= '9'))
        and not ((C >= 'a') and (C <= 'f'))
        and not ((C >= 'A') and (C <= 'F')) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end
  else
  begin
    if Length(S) < 4 then
    begin
      Result := False;
      Exit;
    end;
    if Copy(S, 1, 2) <> '&#' then
    begin
      Result := False;
      Exit;
    end;
    for I := 3 to Length(S) - 1 do
    begin
      C := S[I];
      if not ((C >= '0') and (C <= '9')) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function IsXmlEntityRef(const S: WideString): Boolean;
begin
  if Pos('&', S) <> 1 then
  begin
    Result := False;
    Exit;
  end;
  if Copy(S, Length(S), 1) <> ';' then
  begin
    Result := False;
    Exit;
  end;
  Result := IsXmlName(Copy(S, 2, Length(S) - 2));
end;

function IsXmlPEReference(const S: WideString): Boolean;
begin
  if Pos('%', S) <> 1 then
  begin
    Result := False;
    Exit;
  end;
  if Copy(S, Length(S), 1) <> ';' then
  begin
    Result := False;
    Exit;
  end;
  Result := IsXmlName(Copy(S, 2, Length(S) - 2));
end;

function IsXmlReference(const S: WideString): Boolean;
begin
  if IsXmlEntityRef(s) or IsXmlCharRef(s) then
    Result := True
  else
    Result := False;
end;

function IsXmlEntityValue(const S: WideString): Boolean;
const
  SQ: WideChar = #39; // code of '
  DQ: WideChar = #34; // code of "
var
  C, C2, ForbittenQuote: Word;
  I, J, Indexpos: Integer;
begin
  Result := True;
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  if (S[Length(S)] = SQ) and (S[1] = SQ) {single quotes} then
    ForbittenQuote := Ord(SQ)
  else if (S[Length(S)] = DQ) and (S[1] = DQ) {double quotes} then
    ForbittenQuote := Ord(DQ)
  else
  begin
    Result := False;
    Exit;
  end;

  I := 2;
  while I < Length(S) do
  begin
    C := Ord(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I + 1 = Length(S) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Ord(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
    end;
    if not IsXmlChar(WideChar(C)) then
    begin
      Result := False;
      Exit;
    end;
    if C = ForbittenQuote then
    begin
      Result := False;
      Exit;
    end;
    if C = Ord('%') then
    begin {PEReference?}
      Indexpos := -1;
      for J := I + 1 to Length(S) - 1 do
      begin
        C2 := Ord(S[J]);
        if C2 = Ord(';') then
        begin
          Indexpos := J;
          Break;
        end;
      end;
      if Indexpos = -1 then
      begin
        Result := False;
        Exit;
      end;
      if not IsXmlPEReference(Copy(S, I, Indexpos - I + 1)) then
      begin
        Result := False;
        Exit;
      end;
      I := Indexpos;
    end;
    if C = Ord('&') then
    begin {Reference?}
      Indexpos := -1;
      for J := I + 1 to Length(S) - 1 do
      begin
        C2 := Ord(S[J]);
        if C2 = Ord(';') then
        begin
          Indexpos := J;
          Break;
        end;
      end;
      if Indexpos = -1 then
      begin
        Result := False;
        Exit;
      end;
      if not IsXmlReference(Copy(S, I, Indexpos - I + 1)) then
      begin
        Result := False;
        Exit;
      end;
      I := Indexpos;
    end;
    Inc(I);
  end;
end;

function IsXmlEntityValueChars(const S: WideString): Boolean;
// Returns 'True' if S consists only of legal XML characters and
// legal XML references and there are either only single or only
// double quotation marks in S, or if S is an empty WideString.
// Otherwise 'False' is returned.
const
  SEMICOLON: WideChar = #$3B; // ';'
var
  firstCharPos, I, L: Integer;
  SChar: WideChar;
  DQFound, SQFound: Boolean;
begin
  Result := True;
  DQFound := False;
  SQFound := False;
  I := 0;
  L := Length(S);
  while I < L do
  begin
    Inc(I);
    SChar := S[I];
    case Word(SChar) of
      $0022: // Double quote (")
        if SQFound then
        begin
          Result := False;
          Exit;
        end
        else
          DQFound := True;
      $0026: // Ampersand (&)
        begin
          Result := False;
          firstCharPos := I;
          while I < L do
          begin
            Inc(I);
            SChar := S[I];
            if SChar = SEMICOLON then
            begin
              if IsXmlReference(Copy(S, firstCharPos, I - firstCharPos + 1))
                then
              begin
                Result := True;
                Break
              end
              else
                Exit;
            end; {if ...}
          end; {while ...}
        end;
      $0027: // Single quote (')
        if DQFound then
        begin
          Result := False;
          Exit;
        end
        else
          SQFound := True;
      $0009, $000A, $000D, $0020, $0021, $0023..$0024, $0028..$D7FF,
        $E000..$FFFD: // Unicode below $FFFF, except $0025 (%).
        ; // do nothing.
      $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
        begin
          if I = L then
          begin
            Result := False;
            Exit;
          end; // End of WideString --> No low surrogate found
          Inc(I);
          SChar := S[I];
          if not IsUtf16LowSurrogate(Ord(SChar)) then
          begin
            Result := False;
            Exit;
          end; // No low surrogate found
        end;
    else
      begin
        Result := False;
        Exit;
      end;
    end; {case ...}
  end; {while ...}
end;

function IsXmlAttValue(const S: WideString): Boolean;
const
  SQ: WideChar = #39; // code of '
  DQ: WideChar = #34; // code of "
var
  C, C2, ForbittenQuote: Word;
  I, J, Indexpos: Integer;
begin
  Result := True;
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  if (S[Length(S)] = SQ) and (S[1] = SQ) {single quotes} then
    ForbittenQuote := Ord(SQ)
  else if (S[Length(S)] = DQ) and (S[1] = DQ) {double quotes} then
    ForbittenQuote := Ord(DQ)
  else
  begin
    Result := False;
    Exit;
  end;

  I := 2;
  while I < Length(S) do
  begin
    C := Ord(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I + 1 = Length(s) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Ord(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
    end;
    if not IsXmlChar(WideChar(C)) then
    begin
      Result := False;
      Exit;
    end;
    if C = ForbittenQuote then
    begin
      Result := False;
      Exit;
    end;
    if C = Ord('<') then
    begin
      Result := False;
      Exit;
    end;
    if C = Ord('&') then
    begin {Reference?}
      Indexpos := -1;
      for J := I + 1 to Length(S) - 1 do
      begin
        C2 := Ord(S[J]);
        if C2 = Ord(';') then
        begin
          Indexpos := J;
          Break;
        end;
      end;
      if Indexpos = -1 then
      begin
        Result := False;
        Exit;
      end;
      if not IsXmlReference(Copy(S, I, Indexpos - I + 1)) then
      begin
        Result := False;
        Exit;
      end;
      I := Indexpos;
    end;
    Inc(I);
  end;
end;

function IsXmlSystemLiteral(const S: WideString): Boolean;
const
  SQ: WideChar = #39; // code of '
  DQ: WideChar = #34; // code of "
var
  C, ForbittenQuote: Word;
  I, L: Integer;
begin
  Result := True;
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  if (S[Length(S)] = SQ) and (S[1] = SQ) {single quotes} then
    ForbittenQuote := Ord(SQ)
  else if (S[Length(S)] = DQ) and (S[1] = DQ) {double quotes} then
    ForbittenQuote := Ord(DQ)
  else
  begin
    Result := False;
    Exit;
  end;

  I := 1;
  L := Length(S) - 1;
  while I < L do
  begin
    Inc(I);
    C := Ord(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I + 1 = Length(s) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Ord(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
    end
    else if not IsXmlChar(WideChar(C)) then
    begin
      Result := False;
      Exit;
    end;
    if C = ForbittenQuote then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsXmlSystemChars(const S: WideString): Boolean;
// Returns 'True' if all characters in S are legal XML characters
// and there are either only single or only double quotation marks
// in S, or if S is an empty WideString.
// Otherwise 'False' is returned.
var
  I, L: Integer;
  SChar: WideChar;
  DQFound, SQFound: Boolean;
begin
  Result := True;
  DQFound := False;
  SQFound := False;
  I := 0;
  L := Length(S);
  while I < L do
  begin
    Inc(I);
    SChar := S[I];
    case Word(SChar) of
      $0022: // Double quote (")
        if SQFound then
        begin
          Result := False;
          Break;
        end
        else
          DQFound := True;
      $0027: // Single quote (')
        if DQFound then
        begin
          Result := False;
          Break;
        end
        else
          SQFound := True;
      $0009, $000A, $000D, $0020, $0021, $0023..$0026, $0028..$D7FF,
        $E000..$FFFD: // Unicode below $FFFF
        ; // do nothing.
      $D800..$DBFF: // High surrogate of Unicode character [$10000..$10FFFF]
        begin
          if I = L then
          begin
            Result := False;
            Break;
          end; // End of WideString --> No low surrogate found
          Inc(I);
          SChar := S[I];
          if not IsUtf16LowSurrogate(Ord(SChar)) then
          begin
            Result := False;
            Break;
          end; // No low surrogate found
        end;
    else
      begin
        Result := False;
        Break;
      end;
    end; {case ...}
  end; {while ...}
end;

function IsXmlPubidLiteral(const S: WideString): Boolean;
const
  SQ: WideChar = #39; // code of '
  DQ: WideChar = #34; // code of "
var
  I: Integer;
  SChar: WideChar;
begin
  Result := True;
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  if (S[Length(S)] = SQ) and (S[1] = SQ) then
  begin // single quotes
    for I := 2 to Length(S) - 1 do
    begin
      SChar := S[I];
      if (SChar = SQ) or not IsXmlPubidChar(SChar) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end
  else if (S[Length(S)] = DQ) and (S[1] = DQ) then
  begin // double quotes
    for I := 2 to Length(S) - 1 do
    begin
      SChar := S[I];
      if not IsXmlPubidChar(SChar) then
      begin
        Result := False;
        Exit;
      end;
    end;
  end
  else
  begin
    Result := False;
    Exit;
  end;
end;

function IsXmlPredefinedEntityName(const S: WideString): Boolean;
begin
  if (S = 'lt') or
    (S = 'gt') or
    (S = 'amp') or
    (S = 'apos') or
    (S = 'quot') then
    Result := True
  else
    Result := False;
end;

function IsXmlPubidChars(const S: WideString): Boolean;
// Returns 'True' if all characters in S belong to the XML PubidChar class
// (see XML 1.0, 2nd ed., prod. [13], or if S is an empty WideString.
// Otherwise 'False' is returned.
var
  I: Integer;
  SChar: WideChar;
begin
  Result := True;
  for I := 1 to Length(S) do
  begin
    SChar := S[I];
    if not IsXmlPubidChar(SChar) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsXmlComment(const S: WideString): Boolean;
var
  C: Word;
  I, LengthS: Integer;
  S2: WideString;
begin
  Result := True;
  LengthS := Length(S);
  if LengthS < 7 then
  begin
    Result := False;
    Exit;
  end;
  if Copy(S, 1, 4) <> '<!--' then
  begin
    Result := False;
    Exit;
  end;
  if Copy(S, LengthS - 2, 3) <> '-->' then
  begin
    Result := False;
    Exit;
  end;
  if LengthS = 7 then
    Exit; // Empty comment --> ok.
  C := Ord(S[LengthS - 3]);
  if C = Ord('-') then
  begin
    Result := False;
    Exit;
  end;
  S2 := Copy(S, 5, LengthS - 7);
  if Pos(WideString('--'), S2) > 0 then
  begin
    Result := False;
    Exit;
  end;
  I := 0;
  while I < Pred(Length(S2)) do
  begin
    Inc(I);
    C := Ord(S2[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I = Length(S2) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Ord(S2[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
    end;
    if not IsXmlChar(WideChar(C)) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsXmlCData(const S: WideString): Boolean;
var
  C: Word;
  I: Integer;
begin
  Result := True;
  if Pos(WideString(']]>'), S) > 0 then
  begin
    Result := False;
    Exit;
  end;
  I := 0;
  while I < Length(S) do
  begin
    Inc(I);
    C := Word(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I = Length(s) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Word(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
    end;
    if not IsXmlChar(WideChar(C)) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsXmlCharData(const S: WideString): Boolean;
var
  C: Word;
  I: Integer;
begin
  Result := True;
  I := 0;
  while I < Length(S) do
  begin
    Inc(I);
    C := Word(S[I]);
    if IsUtf16LowSurrogate(C) then
    begin
      Result := False;
      Exit;
    end;
    if IsUtf16HighSurrogate(C) then
    begin
      if I = Length(S) then
      begin
        Result := False;
        Exit;
      end;
      Inc(I);
      C := Word(S[I]);
      if not IsUtf16LowSurrogate(C) then
      begin
        Result := False;
        Exit;
      end;
    end;
    if not IsXmlChar(WideChar(C)) then
    begin
      Result := False;
      Exit;
    end;
    if C = Ord('<') then
    begin
      Result := False;
      Exit;
    end;
    if C = Ord('&') then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsXmlPITarget(const S: WideString): Boolean;
begin
  Result := IsXmlName(S);
  if Length(S) = 3 then
    if ((S[1] = 'X') or (S[1] = 'x')) and
      ((S[2] = 'M') or (S[2] = 'm')) and
      ((S[3] = 'L') or (S[3] = 'l')) then
      Result := False;
end;

function IsXmlVersionNum(const S: WideString): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Length(S) < 3 then
  begin
    Result := False;
    Exit;
  end;
  if (S[1] <> '1') or (S[2] = ',') then
  begin
    Result := False;
    Exit;
  end;
  for I := 3 to Length(S) do
  begin
    if not ( Word(S[I]) in [$0030..$0039] ) then   // Check for '0'..'9'.
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsXmlEncName(const S: WideString): Boolean;
var
  I: Integer;
begin
  Result := True;
  if Length(S) = 0 then
  begin
    Result := False;
    Exit;
  end;
  if not IsXmlEncNameLeadingCharCodePoint(ord(S[1])) then
  begin
    Result := False;
    Exit;
  end;
  for I := 2 to Length(S) do
  begin
    if not IsXmlEncNameFollowingCharCodePoint(ord(S[I])) then
    begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsXmlStringType(const S: WideString): Boolean;
begin
  if S = 'CDATA' then
    Result := True
  else
    Result := False;
end;

function IsXmlTokenizedType(const S: WideString): Boolean;
begin
  if (S = 'ID') or (S = 'IDREF') or (S = 'IDREFS') or (S = 'ENTITY') or
    (S = 'ENTITIES') or (S = 'NMTOKEN') or (S = 'NMTOKENS') then
    Result := True
  else
    Result := False;
end;

function IsXmlNCNameChar(const S: WideChar): Boolean;
begin
  if IsXmlNameChar(S) and not (S = ':') then
    Result := True
  else
    Result := False;
end;

function IsXmlNCNameStartChar(const S: WideChar): Boolean;
begin
  if IsXmlNameStartChar(S) and not (S = ':') then
    Result := True
  else
    Result := False;
end;

function IsXmlNCName(const S: WideString): Boolean;
var
  C: WideChar;
  I: Integer;
begin
  Result := True;
  if Length(S) = 0 then
  begin
    Result := False;
    Exit;
  end;
  C := S[1];
  if not IsXmlNCNameStartChar(C) then
  begin
    Result := False;
    Exit;
  end;
  for I := 2 to Length(S) do
    if not IsXmlNCNameChar(S[I]) then
    begin
      Result := False;
      Exit;
    end;
end;

function IsXmlDefaultAttName(const S: WideString): Boolean;
begin
  if S = 'xmlns' then
    Result := True
  else
    Result := False;
end;

function IsXmlPrefixedAttName(const S: WideString): Boolean;
var
  Piece: WideString;
begin
  if Copy(S, 1, 6) = 'xmlns:' then
  begin
    Piece := Copy(S, 7, Length(S) - 6);
    Result := IsXmlNCName(Piece);
  end
  else
    Result := False;
end;

function IsXmlNSAttName(const S: WideString): Boolean;
begin
  Result := (IsXmlPrefixedAttName(S) or IsXmlDefaultAttName(S));
end;

function IsXmlLocalPart(const S: WideString): Boolean;
begin
  Result := IsXmlNCName(S);
end;

function IsXmlPrefix(const S: WideString): Boolean;
begin
  Result := IsXmlNCName(S);
end;

function IsXmlQName(const S: WideString): Boolean;
var
  Colonpos: Integer;
  Prefix, LocalPart: WideString;
begin
  Colonpos := Pos(':', S);
  if Colonpos = 0 then
    Result := IsXmlLocalPart(S)
  else
  begin
    Prefix := Copy(S, 1, Colonpos - 1);
    LocalPart := Copy(S, Colonpos + 1, Length(S) - Colonpos);
    Result := IsXmlPrefix(Prefix) and IsXmlLocalPart(LocalPart);
  end;
end;

end.


