unit tiMime;

interface
uses
  Classes
 ;

function  tiStreamToMIMEEncodeString(const AStream: TStream): string;
procedure tiMIMEEncodeStringToStream(const AString: string; const AStream: TStream);

{
Unit:           DIMime.pas + DIMimeStreams.pas combined
Version:        1.7
Last Modified:  28 Dec 2005
Author:         Ralf Junker <delphi@yunqa.de>
Internet:       http://www.yunqa.de/delphi/mime/

Description:
  MIME (Base64) encoding and decoding routines according to RFC 2045.

  DIMime is a lightening fast MIME (Base64) Encoder and Decoder library.
  The core encoding and decoding routines are written in highly optimized
  Delphi Pascal which even beats most assembler code.

  Both MimeEncode and MimeDecode have a straightforward, flexible and highly
  effective interface which makes it easy to use them with memory buffers,
  strings, or any other of your preferred data types. Additional helper
  functions are available to convert strings or streams of practically
  unlimited size.

  The decoder is very error tolerant and does about spaces, linebreaks,
  or incomplete data, which are sometimes encountered in e-mail messages.

Legal:

  The contents are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is DIMime.pas and DIMimeStreams.pas.

  The Initial Developer of the Original Code is Ralf Junker <delphi@yunqa.de>.

  All Rights Reserved.

History:

DIMime 1.7 – 28 Dec 2005
------------------------------
- Compatibility with Delphi 2006 Win32.
- Fixed minor documentation typing mistakes.

DIMime 1.6 – 25. October 2001
------------------------------
- Library renamed to DIMime. Unit rjMime.pas renamed to DIMime.pas.
- Separated the main unit into two units: DIMime.pas now has no dependencies
  (except System.pas). All stream routines which require Classes.pas are now
  in DIMimeStreams.pas. This allows for smaller CIG console applications if
  Classes.pas is not needed.
- DIMimeStreams.pas also contains 3 new routines for working with files:
    procedure MimeEncodeFile(const InputFileName, OutputFileName: AnsiString);
    procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: AnsiString);
    procedure MimeDecodeFile(const InputFileName, OutputFileName: AnsiString);
- Fixed a bug in MimeEncodedSize which returned a wrong result when InputSize
  was zero. This could caused a problem when streams were not reset before
  encoding.
- Small speed improvements of encoding routines.
- Miscellaneous enhancements.

rjMime 1.50 – 1. February 2001
------------------------------
- Added support for line breaks (CRLF) during Mime encoding as required by
  RFC 2045. Since inserting line breaks is the default in RFC 2045, I changed
  the standard encoding functions to encode WITH line breaks. This may require
  changes to your code: Encoding without inserting line breaks is still
  provided by the ...NoCRLF procedures.
- If you migrate from rjMime 1.31 to rjMime 1.50, you might need to adjust
  your code depending on the required results. If you don't want to change
  any behaviour as compared to earlier versions of rjMime, simply add "NoCRLF"
  to all calls to Mime encoding functions.
- Please note that there is no MimeDecodeNoCRLF equivalent since the decoding
  routines skip line breaks and white space anyway.
- New DecodeHttpBasicAuthentication procedure according to RFC 1945. See
  description in help file for deatils.
- Version 1.50 also fixes a critical bug in MimeDecode (added missing @).
  This bug did not affect any of the other functions.

Version 1.31
------------
20.11.2000      Defined the OutputBuffer parameters as untyped "out"
                in the core encoding / decoding routines. This does not
                affect much but consistency in clarity.

Version 1.30
------------
17.11.2000      Changed the interface part to the core encoding and decoding
                routines from Pointer to Untyped. This way they no longer
                have to check if the pointers are not nil.

                Replaced all Integer types with Cardinal types
                where the sign was not needed (which was in all cases).

                Thanks to Robert Marqwart <robert_marquardt@gmx.de>
                for pointing these issues out.

Version 1.20
------------
20.06.2000      Bugfix for MimeEncodeStream: Wrong BUFFER_SIZE resulted in
                additional bytes stuffed inbetween the OutBuffer.

                Changed the order of the variables in the MimeEncode interface
                (moved InputBytesCount from last to 2nd, just after InputBuffer).
                Sorry for the inconvenience, but this way it is more consistent
                with MimeDecode. Now MimeEncode has the following variable order:

                procedure MimeEncode (
                  const InputBuffer: Pointer;
                  const InputBytesCount: Integer;
                  const OutputBuffer: Pointer);

                MimeDecode: Interface chage to make it a function: It now returns
                the number of bytes written to the output buffer not as a var
                but as the result of a function. This way coding is made
                simpler since not OutputCount variable needs to be defined.

                Introduced two new functions mostly for internal use:

                * MimeDecodePartial: This is necessary to decode large
                  blocks of data in multiple parts. On initialization,
                  call MimeDecodePartial with ByteBuffer := 0 and
                  ByteBufferSpace := 4. Then repeatedly call it again
                  with new data loaded into the buffer. At the end of all data,
                * MimeDecodePartialEnd writes the remaining bytes from ByteBuffer
                  to the outputbuffer (see MimeDecodeStream for an example).

                MimeDecodePartial_ are necessary to decode inconsitent data
                (with linebreaks, tabs, whitespace) in multiple parts
                like in MimeDecodeStream.

Version 1.10
------------
19.04.2000      Fixed a small bug in MimeEncode which sometimes screwed up
                the very first bytes of the encoded output.

                Added the following wrapper functions:
                * MimeEncodeString & MimeDecodeString
                * MimeEncodeStream & MimeDecodeStream

Version 1.01
------------
09.04.2000      Fixed a bug in MIME_DECODE_TABLE which caused wrong results
                decoding binary files.

Version 1.00
------------
17.01.2000      Initial Public Release

Copyright (c) Ralf Junker, The Delphi Inspiration 2000-2005
}

function MimeEncodeString(const s: AnsiString): AnsiString; overload;
function MimeEncodeString(const s: string): string; overload;

function MimeEncodeStringNoCRLF(const s: AnsiString): AnsiString; overload;
function MimeEncodeStringNoCRLF(const s: string): string; overload;

function MimeDecodeString(const s: AnsiString): AnsiString; overload;
function MimeDecodeString(const s: string): string; overload;

function MimeEncodedSize(const InputSize: Cardinal): Cardinal;

function MimeEncodedSizeNoCRLF(const InputSize: Cardinal): Cardinal;

function MimeDecodedSize(const InputSize: Cardinal): Cardinal;

procedure DecodeHttpBasicAuthentication(const BasicCredentials: AnsiString; out UserId, Password: AnsiString);

procedure MimeEncode(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);

procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);

procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);

function MimeDecode(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;

function MimeDecodePartial(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;

function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;

procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);

procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);

procedure MimeEncodeFile(const InputFileName, OutputFileName: string);

procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: string);

procedure MimeDecodeFile(const InputFileName, OutputFileName: string);

implementation
uses
   SysUtils
  ,tiConstants
  ,tiUtils
  ;

const
  MIME_ENCODED_LINE_BREAK = 76;

  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;

  MIME_BUFFER_SIZE  = MIME_DECODED_LINE_BREAK * 3 * 4 * 4;

  MIME_ENCODE_TABLE: array[0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072,
    073, 074, 075, 076, 077, 078, 079, 080,
    081, 082, 083, 084, 085, 086, 087, 088,
    089, 090, 097, 098, 099, 100, 101, 102,
    103, 104, 105, 106, 107, 108, 109, 110,
    111, 112, 113, 114, 115, 116, 117, 118,
    119, 120, 121, 122, 048, 049, 050, 051,
    052, 053, 054, 055, 056, 057, 043, 047);

  MIME_PAD_CHAR = Byte('=');

  MIME_DECODE_TABLE: array[Byte] of Cardinal = (
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 062, 255, 255, 255, 063,
    052, 053, 054, 055, 056, 057, 058, 059,
    060, 061, 255, 255, 255, 255, 255, 255,
    255, 000, 001, 002, 003, 004, 005, 006,
    007, 008, 009, 010, 011, 012, 013, 014,
    015, 016, 017, 018, 019, 020, 021, 022,
    023, 024, 025, 255, 255, 255, 255, 255,
    255, 026, 027, 028, 029, 030, 031, 032,
    033, 034, 035, 036, 037, 038, 039, 040,
    041, 042, 043, 044, 045, 046, 047, 048,
    049, 050, 051, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);



type
  PByte4 = ^TByte4;
  TByte4 = packed record
    b1, b2, b3, b4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    b1, b2, b3: Byte;
  end;

  PCardinal = ^Cardinal;

function _OnlyAnsi(const s: string): boolean; overload;
var
  i: integer;
begin
  result := true;
  for i := 1 to Length(s) do
    if Ord(s[i]) > $7F then
    begin
      result := false;
      break;
    end;
end;

function _OnlyAnsi(const s: AnsiString): boolean; overload;
var
  i: integer;
begin
  result := true;
  for i := 1 to Length(s) do
    if Ord(s[i]) > $7F then
    begin
      result := false;
      break;
    end;
end;

function MimeEncodeString(const s: AnsiString): AnsiString;
var
  l: Cardinal;
begin
  if Pointer(s) <> nil then
    begin
      l := Length(s);
      SetString(Result, nil, MimeEncodedSize(l));
      MimeEncode(Pointer(s)^, l, Pointer(Result)^);
    end
  else
    Result := '';
end;

function MimeEncodeString(const s: string): string;
var
  LAnsiString: AnsiString;
  LNumBytes: Longint;
begin
  // If source contains non-Ansi characters then puch the byte data
  // into an ansi string so as not to lose data in a string conversion.
  if _OnlyAnsi(s) then
    LAnsiString := AnsiString(s)
  else begin
    LNumBytes := Length(s) * SizeOf(Char);
    SetLength(LAnsiString, LNumBytes);
    Move(Pointer(s)^, Pointer(LAnsiString)^, LNumBytes);
  end;
  result := string(MimeEncodeString(LAnsiString));
end;

function MimeEncodeStringNoCRLF(const s: AnsiString): AnsiString;
var
  l: Cardinal;
begin
  if Pointer(s) <> nil then
    begin
//IanK - Cardinal() type conversion a no-no for Win64.
//     http://docwiki.embarcadero.com/RADStudio/XE3/en/Converting_32-bit_Delphi_Applications_to_64-bit_Windows
//  l := PCardinal(PAnsiChar(s) - 4)^; Might be correct, but not sure
      {$IFDEF WIN64}
      Assert(False,'Attention required in tiStreams.MimeEncodeStringNoCRLF for WIN64 compatibility');
      {$ELSE}
      l := PCardinal(Cardinal(s) - 4)^;  // This doesn't compile in WIN64
      {$ENDIF WIN64}
      SetString(Result, nil, MimeEncodedSizeNoCRLF(l));
      MimeEncodeNoCRLF(Pointer(s)^, l, Pointer(Result)^);
    end
  else
    Result := '';
end;

function MimeEncodeStringNoCRLF(const s: string): string;
var
  LAnsiString: AnsiString;
  LNumBytes: Longint;
begin
  // If source contains non-Ansi characters then push the byte data
  // into an ansi string so as not to lose data in a string conversion.
  if _OnlyAnsi(s) then
    LAnsiString := AnsiString(s)
  else begin
    LNumBytes := Length(s) * SizeOf(Char);
    SetLength(LAnsiString, LNumBytes);
    Move(Pointer(s)^, Pointer(LAnsiString)^, LNumBytes);
  end;
  result := string(MimeEncodeStringNoCRLF(LAnsiString));
end;

function MimeDecodeString(const s: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  l: Cardinal;
begin
  if Pointer(s) <> nil then
    begin
      l := Length(s);
      SetString(Result, nil, MimeDecodedSize(l));
      ByteBuffer := 0;
      ByteBufferSpace := 4;
      l := MimeDecodePartial(Pointer(s)^, l, Pointer(Result)^, ByteBuffer, ByteBufferSpace);
      Inc(l, MimeDecodePartialEnd(Pointer(PAnsiChar(Result) + l)^, ByteBuffer, ByteBufferSpace));
      SetLength(Result, l);
    end
  else
    Result := '';
end;

function MimeDecodeString(const s: string): string;
var
  LAnsiString: AnsiString;
  LNumUnicodeChars: Longint;
begin
  LAnsiString := MimeDecodeString(AnsiString(s));
  // If encoded contains non-Ansi characters then push the byte data
  // back into a unicode string.
  if _OnlyAnsi(LAnsiString) then
    Result := string(LAnsiString)
  else begin
    LNumUnicodeChars := Length(LAnsiString) div SizeOf(Char);
    SetLength(Result, LNumUnicodeChars);
    Move(Pointer(LAnsiString)^, Pointer(Result)^, Length(LAnsiString));
  end;
end;

procedure DecodeHttpBasicAuthentication(const BasicCredentials: AnsiString; out UserId, Password: AnsiString);
label
  Fail;
const
  LBasic = 6;
var
  DecodedPtr, p: PAnsiChar;
  i, l: Cardinal;
begin
  p := Pointer(BasicCredentials);
  if p = nil then goto Fail;

  l := Length(BasicCredentials);
  if l <= LBasic then goto Fail;

  Dec(l, LBasic);
  Inc(p, LBasic);

  GetMem(DecodedPtr, MimeDecodedSize(l));
  l := MimeDecode(p^, l, DecodedPtr^);

  i := 0;
  p := DecodedPtr;
  while (l > 0) and (p[i] <> ':') do
    begin
      Inc(i);
      Dec(l);
    end;

  SetString(UserId, DecodedPtr, i);
  if l > 1 then
    SetString(Password, DecodedPtr + i + 1, l - 1)
  else
    Password := '';

  FreeMem(DecodedPtr);
  Exit;

  Fail:
  UserId := '';
  Password := '';
end;

function MimeEncodedSize(const InputSize: Cardinal): Cardinal;
begin
  if InputSize > 0 then
    Result := (InputSize + 2) div 3 * 4 + (InputSize - 1) div MIME_DECODED_LINE_BREAK * 2
  else
    Result := InputSize;
end;

function MimeEncodedSizeNoCRLF(const InputSize: Cardinal): Cardinal;
begin
  Result := (InputSize + 2) div 3 * 4;
end;

function MimeDecodedSize(const InputSize: Cardinal): Cardinal;
begin
  Result := (InputSize + 3) div 4 * 3;
end;

procedure MimeEncode(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  iDelta, ODelta: Cardinal;
begin
  MimeEncodeFullLines(InputBuffer, InputByteCount, OutputBuffer);
  iDelta := InputByteCount div MIME_DECODED_LINE_BREAK;
  ODelta := iDelta * (MIME_ENCODED_LINE_BREAK + 2);
  iDelta := iDelta * MIME_DECODED_LINE_BREAK;
  MimeEncodeNoCRLF(Pointer(Cardinal(@InputBuffer) + iDelta)^, InputByteCount - iDelta, Pointer(Cardinal(@OutputBuffer) + ODelta)^);
end;

procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  b, InnerLimit, OuterLimit: Cardinal;
  InPtr: PByte3;
  OutPtr: PByte4;
begin

  if InputByteCount < MIME_DECODED_LINE_BREAK then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := Cardinal(InPtr);
  Inc(OuterLimit, InputByteCount);

  repeat

    repeat

      b := InPtr^.b1;
      b := b shl 8;
      b := b or InPtr^.b2;
      b := b shl 8;
      b := b or InPtr^.b3;
      Inc(InPtr);

      OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b1 := MIME_ENCODE_TABLE[b];
      Inc(OutPtr);
    until Cardinal(InPtr) >= InnerLimit;

    OutPtr^.b1 := 13;
    OutPtr^.b2 := 10;
    {$IFDEF WIN64}
    Assert(False,'Attention required in tiStreams.MimeEncodeFullLines for WIN64 compatibility');
    {$ELSE}
    Inc(Cardinal(OutPtr), 2);  // This doesn't compile in WIN64
    {$ENDIF WIN64}

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  b, InnerLimit, OuterLimit: Cardinal;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  if InputByteCount = 0 then Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, OuterLimit);

  while Cardinal(InPtr) < InnerLimit do
    begin

      b := InPtr^.b1;
      b := b shl 8;
      b := b or InPtr^.b2;
      b := b shl 8;
      b := b or InPtr^.b3;
      Inc(InPtr);

      OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b1 := MIME_ENCODE_TABLE[b];
      Inc(OutPtr);
    end;

  case InputByteCount - OuterLimit of
    1:
      begin
        b := InPtr^.b1;
        b := b shl 4;
        OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr.b1 := MIME_ENCODE_TABLE[b];
        OutPtr.b3 := MIME_PAD_CHAR;
        OutPtr.b4 := MIME_PAD_CHAR;
      end;
    2:
      begin
        b := InPtr^.b1;
        b := b shl 8;
        b := b or InPtr^.b2;
        b := b shl 2;
        OutPtr.b3 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr.b2 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr.b1 := MIME_ENCODE_TABLE[b];
        OutPtr.b4 := MIME_PAD_CHAR;
      end;
  end;
end;

function MimeDecode(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  Result := MimeDecodePartial(InputBuffer, InputBytesCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  Inc(Result, MimeDecodePartialEnd(Pointer(Cardinal(@OutputBuffer) + Result)^, ByteBuffer, ByteBufferSpace));
end;

function MimeDecodePartial(const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var
  lByteBuffer, lByteBufferSpace, c: Cardinal;
  InPtr, OuterLimit: ^Byte;
  OutPtr: PByte3;
begin
  if InputBytesCount > 0 then
    begin
      InPtr := @InputBuffer;
      {$IFDEF WIN64}
      Assert(False,'Attention required in tiStreams.MimeDecodePartial for WIN64 compatibility');
      {$ELSE}
      Cardinal(OuterLimit) := Cardinal(InPtr) + InputBytesCount; // This doesn't compile in WIN64
      {$ENDIF WIN64}
      OutPtr := @OutputBuffer;
      lByteBuffer := ByteBuffer;
      lByteBufferSpace := ByteBufferSpace;
      while InPtr <> OuterLimit do
        begin

          c := MIME_DECODE_TABLE[InPtr^];
          Inc(InPtr);
          if c = $FF then Continue;
          lByteBuffer := lByteBuffer shl 6;
          lByteBuffer := lByteBuffer or c;
          Dec(lByteBufferSpace);

          if lByteBufferSpace <> 0 then Continue;

          OutPtr^.b3 := Byte(lByteBuffer);
          lByteBuffer := lByteBuffer shr 8;
          OutPtr^.b2 := Byte(lByteBuffer);
          lByteBuffer := lByteBuffer shr 8;
          OutPtr^.b1 := Byte(lByteBuffer);
          lByteBuffer := 0;
          Inc(OutPtr);
          lByteBufferSpace := 4;
        end;
      ByteBuffer := lByteBuffer;
      ByteBufferSpace := lByteBufferSpace;
      Result := Cardinal(OutPtr) - Cardinal(@OutputBuffer);
    end
  else
    Result := 0;
end;

function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
var
  lByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        lByteBuffer := ByteBuffer shr 2;
        PByte3(@OutputBuffer)^.b2 := Byte(lByteBuffer);
        lByteBuffer := lByteBuffer shr 8;
        PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 2;
      end;
    2:
      begin
        lByteBuffer := ByteBuffer shr 4;
        PByte3(@OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer       : array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer      : array[0..(MIME_BUFFER_SIZE + 2) div 3 * 4 + MIME_BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
  BytesRead         : Cardinal;
  IDelta, ODelta    : Cardinal;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));

  while BytesRead = SizeOf(InputBuffer) do
    begin
      MimeEncodeFullLines(InputBuffer, SizeOf(InputBuffer), OutputBuffer);
      OutputStream.Write(OutputBuffer, SizeOf(OutputBuffer));
      BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
    end;

  MimeEncodeFullLines(InputBuffer, BytesRead, OutputBuffer);

  IDelta := BytesRead div MIME_DECODED_LINE_BREAK;
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  MimeEncodeNoCRLF(Pointer(Cardinal(@InputBuffer) + IDelta)^, BytesRead - IDelta, Pointer(Cardinal(@OutputBuffer) + ODelta)^);

  OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
end;

procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer       : array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer      : array[0..((MIME_BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead         : Cardinal;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead = SizeOf(InputBuffer) do
    begin
      MimeEncodeNoCRLF(InputBuffer, SizeOf(InputBuffer), OutputBuffer);
      OutputStream.Write(OutputBuffer, SizeOf(OutputBuffer));
      BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
    end;

  MimeEncodeNoCRLF(InputBuffer, BytesRead, OutputBuffer);
  OutputStream.Write(OutputBuffer, MimeEncodedSizeNoCRLF(BytesRead));
end;

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer       : array[0..MIME_BUFFER_SIZE - 1] of Byte;
  OutputBuffer      : array[0..(MIME_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead         : Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead > 0 do
    begin
      OutputStream.Write(OutputBuffer, MimeDecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
      BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
    end;
  OutputStream.Write(OutputBuffer, MimeDecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

procedure MimeEncodeFile(const InputFileName, OutputFileName: string);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: string);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStreamNoCRLF(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure MimeDecodeFile(const InputFileName, OutputFileName: string);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeDecodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

function tiStreamToMIMEEncodeString(const AStream: TStream): string;
var
  LStream : TStringStream;
  LPos: integer;
begin
  LPos:= AStream.Position;
  try
    LStream := TStringStream.Create('');
    try
      AStream.Position := 0;
      MimeEncodeStream(AStream, LStream);
      result := LStream.DataString;
    finally
      LStream.Free;
    end;
  finally
    AStream.Position:= LPos;
  end;
end;

procedure tiMIMEEncodeStringToStream(const AString: string; const AStream: TStream);
var
  lStream: TStringStream;
begin
  lStream:= TStringStream.Create(AString);
  try
    AStream.Size := 0;
    MimeDecodeStream(lStream, AStream);
    AStream.Position := 0;
  finally
    lStream.Free;
  end;
end;

end.
