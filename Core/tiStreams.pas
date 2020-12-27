unit tiStreams;

{$I tiDefines.inc}

{ When defined, it will use the FPC copied base64.pas unit instead of
  the jrMIME code (latter code not being very portable) }
{$define USE_BASE64_UNIT}

interface
uses
   tiBaseObject
  ,Classes
  ,Contnrs
 ;

const
  cStreamStartSize = 2000000;
  cStreamGrowBy    =  500000;

  cErrorBlockSizeMismatch = 'BlockSize/BlockAsString size mismatch. BlockSize=%d, Lenght(BlockAsString)=%d';
  cErrorBlockMissing = 'Block %d of %d missing from sequence';

type

  TtiPreSizedStream = class(TtiBaseObject)
  private
    FStream : TMemoryStream;
    FInitialSize: Int64;
    FStreamSize : Int64;
    FGrowBy    : Int64;
    FDataSize  : Int64;
    function GetPosition: Int64;
  public
    constructor Create(AInitialSize, AGrowBy : Int64);
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Write(const AStr: string);
    procedure   WriteLn(const AStr: string = '');
    function    AsString: string;
    procedure   SaveToFile(const AFileName: string);
    property    Size: Int64 read FDataSize;
    property    Position: Int64 read GetPosition;
  end;

  {: Adds ReadLn and WriteLn to a TFileStream}
  TtiFileStream = class(TFileStream)
  private
    FLineDelim: string;
    FLineDelimLen : Byte;
    function GetLineDelim: string;
    procedure SetLineDelim(const AValue: string);
  public
    constructor Create(const AFileName: string; Mode: Word);
    constructor CreateReadWrite(const AFileName : string; pOverwrite : boolean = false);
    constructor CreateReadOnly( const AFileName : string);
    property    LineDelim : string read GetLineDelim write SetLineDelim;
    procedure   Write(const AString : string); reintroduce;
    procedure   WriteLn(const AString : string = '');
    function    ReadLn : ansistring;
    function    EOF : boolean;
  end;

  {: Adds ReadLn and WriteLn to any stream }
  TtiLineStream = class(TtiBaseObject)
  private
    FStream: TStream;
    FLineDelim: string;
    FLineDelimLen: Byte;
    function    GetLineDelim: string;
    procedure   SetLineDelim(const AValue: string);
    function    GetPosition: Int64;
    procedure   SetPosition(const Pos: Int64);
    function    GetSize: Int64;
    procedure   SetSize(const NewSize: Int64);
    function    GetEOF: boolean;
  public
    constructor Create(const AStream: TStream);
    procedure   Write(const AString : string);
    procedure   WriteLn(const AString : string = '');
    function    ReadLn: string;
    function    Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
    property    Stream: TStream read FStream;
    property    LineDelim: string read GetLineDelim write SetLineDelim;
    property    Position: Int64 read GetPosition write SetPosition;
    property    Size: Int64 read GetSize write SetSize;
    property    EOF: boolean read GetEOF;
  end;

  {: Manage a stream in chunks, or blocks. Current interface supports access to the stream via the AsString property.
     This can be extended to support access via TStream if required. Stream is zero indexed, so a BlockIndex=0 is the
     first block of data.}
  TtiBlockStream = class(TtiBaseObject)
  private
    FList: TObjectList;
    FBlockSize: Longword;
    function    GetBlockAsString(ABlockIndex: Longword): string;
    procedure   SetBlockAsString(ABlockIndex: Longword; const AValue: string);
    procedure   SetAsString(const AValue: string);
    function    GetAsString: string;
    function    GetBlockCount: Longword;
    procedure   Sort;
    procedure   ValidateBlockSequence;
    function    FindByBlockIndex(ABlockIndex: Longword): TtiBaseObject;
  public
    constructor Create(ABlockSize: Longword); overload; virtual;
    constructor Create(const AData: string; ABlockSize: Longword); overload; virtual;
    destructor  Destroy; override;
    procedure   AssignToStream(AStream: TStream);

    property    BlockSize: Longword Read FBlockSize;
    property    BlockCount: Longword Read GetBlockCount;
    property    BlockAsString[ABlockIndex: Longword]: string Read GetBlockAsString Write SetBlockAsString;
    property    AsString: string Read GetAsString Write SetAsString;

  end;

function  tiStreamToMIMEEncodeString(const AStream: TStream): string;
procedure tiMIMEEncodeStringToStream(const AString: string; const AStream: TStream);
function tiStreamReadToNextToken(const AStream: TStream; const AToken: string): string; overload;
procedure tiStreamReadToNextToken(const AStream: TStream; const AToken: string; const AOutput: TStream); overload;

{
Unit:           rjMime
Version:        1.31
Last Modified:  20. November 2000
Author:         Ralf Junker <ralfjunker@gmx.de>
Internet:       http://www.zeitungsjunge.de/delphi

Description:    Ligtening fast Mime (Base64) Encoding and Decoding routines.
                More detailed descriptions follow the declarations of the
                functions and procedures below.

Legal:          This software is provided 'as-is', without any express or
                implied warranty. In no event will the author be held liable
                for any  damages arising from the use of this software.

                Permission is granted to anyone to use this software for any
                purpose, including commercial applications, and to alter it
                and redistribute it freely, subject to the following
                restrictions:

                1. The origin of this software must not be misrepresented,
                   you must not claim that you wrote the original software.
                   If you use this software in a product, an acknowledgment
                   in the product documentation would be appreciated but is
                   not required.

                2. Altered source versions must be plainly marked as such, and
                   must not be misrepresented as being the original software.

                3. This notice may not be removed or altered from any source
                   distribution.

History:

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

Copyright (c) 2000 Ralf Junker
}

function MimeEncodeString (const s: AnsiString): AnsiString;
{ MimeEncodeString takes a string, encodes it, and returns the result as a string.
  To decode the result string, use MimeDecodeString. }

function MimeEncodeStringNoCRLF(const s: AnsiString): AnsiString;

function MimeDecodeString (const s: AnsiString): AnsiString;
{ MimeDecodeString takes a a string, decodes it, and returns the result as a string.
  Use MimeDecodeString to decode a string previously encoded with MimeEncodeString. }


procedure MimeEncodeStream (const InputStream: TStream; const OutputStream: TStream);
{ MimeEncodeStream encodes InputStream starting at the current position
  up to the end and writes the result to OutputStream, again starting at
  the current position. When done, it will not reset either stream's positions,
  but leave InputStream at the last read position (i.e. the end) and
  OutputStream at the last write position (which can, but most not be the end).
  To encode the entire InputStream from beginning to end, make sure
  that its offset is positioned at the beginning of the stream. You can
  force this by issuing Seek (0, soFromBeginning) before calling this function. }

procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);

procedure MimeDecodeStream (const InputStream: TStream; const OutputStream: TStream);
{ MimeDecodeStream decodes InputStream starting at the current position
  up to the end and writes the result to OutputStream, again starting at
  the current position. When done, it will not reset either stream's positions,
  but leave InputStream at the last read position (i.e. the end) and
  OutputStream at the last write position (which can, but most not be the end).
  To decode the entire InputStream from beginning to end, make sure
  that its offset is positioned at the beginning of the stream. You can
  force this by issuing Seek (0, soFromBeginning) before calling this function. }


function MimeEncodedSize (const i: Cardinal): Cardinal;
{ Calculates the output size of i MimeEncoded bytes. Use for MimeEncode only. }

function MimeEncodedSizeNoCRLF (const i: Cardinal): Cardinal;

function MimeDecodedSize (const i: Cardinal): Cardinal;
{ Calculates the maximum output size of i MimeDecoded bytes.
  You may use it for MimeDecode to calculate the maximum amount of memory
  required for decoding in one single pass. }

procedure MimeEncode (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
{ The primary Mime encoding routine.

  CAUTTION: OutputBuffer must have enough memory allocated to take all encoded output.
  MimeEncodedSize (InputBytesCount) calculates this amount in bytes. MimeEncode will
  then fill the entire OutputBuffer, so there is no OutputBytesCount result for
  this procedure. Preallocating all memory at once (as required by MimeEncode)
  avoids the time-cosuming process of reallocation.

  If not all data fits into memory at once, you can use MimeEncode multiple times,
  but you must be very careful about the size of the InputBuffer.
  See comments on BUFFER_SIZE below for details. }

procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);

procedure MimeEncodeNoCRLF (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);

function MimeDecode (const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
{ The primary Mime decoding routines.

  CAUTION: OutputBuffer must have enough memory allocated to take all output.
  MimeDecodedSize (InputBytesCount) calculates this amount in bytes. There is
  no guarantee that all output will be filled after decoding. All decoding
  functions therefore return the acutal number of bytes written to OutputBuffer.
  Preallocating all memory at once (as is required by MimeDecode)
  avoids the time-cosuming process of reallocation. After calling
  MimeDecode, simply cut the allocated memory down to OutputBytesCount,
  i.e. SetLength (OutString, OutputBytesCount).

  If not all data fits into memory at once, you may NOT use MimeDecode multiple times.
  Instead, you must use the MimeDecodePartial_ functions.
  See MimeDecodeStream for an example. }

function MimeDecodePartial (const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
function MimeDecodePartialEnd (out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
{ The MimeDecodePartial_ functions are mostly for internal use.
  They serve the purpose of decoding very large data in multiple parts of
  smaller chunks, as used in MimeDecodeStream. }


implementation

uses
   tiUtils
  ,tiExcept
  ,tiConstants
  ,SysUtils
  ,Math
  {$IFDEF USE_BASE64_UNIT}
  ,tiBase64
  {$ENDIF}
 ;

const
  MIME_ENCODED_LINE_BREAK = 76;

  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;

  MIME_BUFFER_SIZE  = MIME_DECODED_LINE_BREAK * 3 * 4 * 4;

  EQUAL_SIGN         = Byte ('=');
  BUFFER_SIZE        = $3000;
 { CAUTION: For MimeEncodeStream and all other kinds of multi-buffered
   Mime encodings (i.e. Files etc.), BufferSize must be set to a multiple of 3.
   Even though the implementation of the Mime decoding routines below
   does not require a particular buffer size, they work fastest with sizes of
   multiples of four. The chosen size is a multiple of 3 and of 4 as well.
   The following numbers are, in addition, also divisible by 1024:
   $2400, $3000, $3C00, $4800, $5400, $6000, $6C00. }

 MIME_ENCODE_TABLE : array[0..63] of Byte = (
  065, 066, 067, 068, 069, 070, 071, 072, // 00 - 07
  073, 074, 075, 076, 077, 078, 079, 080, // 08 - 15
  081, 082, 083, 084, 085, 086, 087, 088, // 16 - 23
  089, 090, 097, 098, 099, 100, 101, 102, // 24 - 31
  103, 104, 105, 106, 107, 108, 109, 110, // 32 - 39
  111, 112, 113, 114, 115, 116, 117, 118, // 40 - 47
  119, 120, 121, 122, 048, 049, 050, 051, // 48 - 55
  052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63

 MIME_DECODE_TABLE : array[Byte] of Cardinal = (
  255, 255, 255, 255, 255, 255, 255, 255, //  00 -  07
  255, 255, 255, 255, 255, 255, 255, 255, //  08 -  15
  255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
  255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
  255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
  255, 255, 255, 062, 255, 255, 255, 063, //  40 -  47
  052, 053, 054, 055, 056, 057, 058, 059, //  48 -  55
  060, 061, 255, 255, 255, 255, 255, 255, //  56 -  63
  255, 000, 001, 002, 003, 004, 005, 006, //  64 -  71
  007, 008, 009, 010, 011, 012, 013, 014, //  72 -  79
  015, 016, 017, 018, 019, 020, 021, 022, //  80 -  87
  023, 024, 025, 255, 255, 255, 255, 255, //  88 -  95
  255, 026, 027, 028, 029, 030, 031, 032, //  96 - 103
  033, 034, 035, 036, 037, 038, 039, 040, // 104 - 111
  041, 042, 043, 044, 045, 046, 047, 048, // 112 - 119
  049, 050, 051, 255, 255, 255, 255, 255, // 120 - 127
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
  b1: Byte;
  b2: Byte;
  b3: Byte;
  b4: Byte;
 end;

 PByte3 = ^TByte3;
 TByte3 = packed record
  b1: Byte;
  b2: Byte;
  b3: Byte;
 end;


{ **************************************************************************** }
{ Wrapper functions & procedures }
{ **************************************************************************** }

{$IFDEF USE_BASE64_UNIT}
{ This is 32 & 64 bit safe }
function MimeEncodeString (const s: AnsiString): AnsiString;
var
  InStream: TStringStream;
  OutStream: TStringStream;
  b64encoder: TBase64EncodingStream;
begin
  if s = '' then
    Exit; { nothing to do }

  InStream := TStringStream.Create(s);
  try
    OutStream := TStringStream.Create('');
    b64encoder := TBase64EncodingStream.Create(OutStream);
    try
      b64encoder.CopyFrom(InStream, InStream.Size);
    finally
      b64encoder.Free;
      Result := OutStream.DataString;
      OutStream.Free;
    end;
  finally
    InStream.Free;
  end;
end;
{$ELSE}
function MimeEncodeString (const s: AnsiString): AnsiString;
label
  NothingToDo;
var
  l: Cardinal;
begin
  if Pointer(s) = nil then
    goto NothingToDo;
  l := Length(s);
  SetLength (Result, MimeEncodedSize(l));
  if Pointer (Result) = nil then
    goto NothingToDo;
  MimeEncode (Pointer (s)^, l, Pointer (Result)^);
  Exit; //==>

  NothingToDo:
  Result := '';
end;
{$ENDIF}

function MimeEncodeStringNoCRLF(const s: AnsiString): AnsiString;
var
  l: Cardinal;
begin
  if Pointer(s) <> nil then
    begin
      l := Length(s);
      SetString(Result, nil, MimeEncodedSizeNoCRLF(l));
      MimeEncodeNoCRLF(Pointer(s)^, l, Pointer(Result)^);
    end
  else
    Result := '';
end;

{$IFDEF USE_BASE64_UNIT}
{ This is 32 & 64 bit safe }
function MimeDecodeString(const s: AnsiString): AnsiString;
var
  b64decoder: TBase64DecodingStream;
  outputstream: TStringStream;
  inputstream: TStringStream;
begin
  if s = '' then
    exit; // nothing to do

  inputstream := TStringStream.Create(s);
  try
    outputstream := TStringStream.Create('');
    try
      b64decoder := TBase64DecodingStream.Create(inputstream, bdmMIME);
      outputstream.CopyFrom(b64decoder, b64decoder.Size);
      outputstream.Position := 0;
    finally
      b64decoder.Free;
      { destroying the decoder, flush'es the remained of the data, so don't read
        it earlier }
      Result := outputStream.DataString;
      outputstream.Free;
    end;
  finally
    inputstream.Free;
  end;
end;
{$ELSE}
function MimeDecodeString (const s: AnsiString): AnsiString;
label
 NothingToDo;
var
 ByteBuffer, ByteBufferSpace: Cardinal;
 l: Cardinal;
begin
 if Pointer (s) = nil then goto NothingToDo;
 l := Length(s);
 SetLength (Result, (l + 3) div 4 * 3);
 if Pointer (Result) = nil then goto NothingToDo;
 ByteBuffer := 0;
 ByteBufferSpace := 4;
 l := MimeDecodePartial (Pointer (s)^, l, Pointer (Result)^, ByteBuffer, ByteBufferSpace);
// Inc (l, MimeDecodePartialEnd (Pointer(PByte(Pointer(Result)) + l)^, ByteBuffer, ByteBufferSpace));
 Inc (l, MimeDecodePartialEnd (Pointer(PAnsiChar(Pointer(Result)) + l)^, ByteBuffer, ByteBufferSpace));
 SetLength (Result, l);
 Exit;
 NothingToDo:
 Result := '';
end;
{$ENDIF}

{$IFDEF USE_BASE64_UNIT}
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  b64encoder: TBase64EncodingStream;
begin
  b64encoder := TBase64EncodingStream.Create(OutputStream);
  try
    b64encoder.CopyFrom(InputStream, InputStream.Size);
  finally
    b64encoder.Free;
  end;
end;
{$ELSE}
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer       : array[0..BUFFER_SIZE - 1] of Byte;
  OutputBuffer      : array[0..(BUFFER_SIZE + 2) div 3 * 4 + BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
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
{$ENDIF}

procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer       : array[0..BUFFER_SIZE - 1] of Byte;
  OutputBuffer      : array[0..((BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  BytesRead         : Cardinal;
begin
  InputStream.Position:= 0;
  OutputStream.Size:= 0;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  while BytesRead = SizeOf(InputBuffer) do
  begin
    MimeEncodeNoCRLF(InputBuffer, SizeOf(InputBuffer), OutputBuffer);
    OutputStream.Write(OutputBuffer, SizeOf(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  end;
  if BytesRead > 0 then
  begin
    MimeEncodeNoCRLF(InputBuffer, BytesRead, OutputBuffer);
    OutputStream.Write(OutputBuffer, MimeEncodedSizeNoCRLF(BytesRead));
  end;
end;

{$IFDEF USE_BASE64_UNIT}
procedure MimeDecodeStream (const InputStream: TStream; const OutputStream: TStream);
var
  b64decoder: TBase64DecodingStream;
begin
  b64decoder := TBase64DecodingStream.Create(InputStream, bdmMIME);
  try
    OutputStream.CopyFrom(b64decoder, b64decoder.Size);
    OutputStream.Position:=0;
  finally
    b64decoder.Free;
  end;
end;
{$ELSE}
procedure MimeDecodeStream (const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer       : array[0..BUFFER_SIZE - 1] of Byte;
  OutputBuffer      : array[0.. (BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  BytesRead         : Cardinal;
begin
  InputStream.Position:= 0;
  OutputStream.Size:= 0;
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  BytesRead := InputStream.Read (InputBuffer, SizeOf (InputBuffer));
  while BytesRead > 0 do
  begin
    OutputStream.Write (OutputBuffer, MimeDecodePartial (InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read (InputBuffer, SizeOf (InputBuffer));
  end;
  OutputStream.Write (OutputBuffer, MimeDecodePartialEnd (OutputBuffer, ByteBuffer, ByteBufferSpace));
end;
{$ENDIF}

function MimeEncodedSize (const i: Cardinal): Cardinal;
begin
  if i > 0 then
    Result := (i + 2) div 3 * 4 + (i - 1) div MIME_DECODED_LINE_BREAK * 2
  else
    Result := i;
end;

function MimeEncodedSizeNoCRLF (const i: Cardinal): Cardinal;
begin
  Result := (i + 2) div 3 * 4;
end;

function MimeDecodedSize (const i: Cardinal): Cardinal;
begin
  Result := (i + 3) div 4 * 3;
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
    Inc(OutPtr, 2);   // was:  Inc(Cardinal(OutPtr), 2) but that doesn't compile under FPC

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;

procedure MimeEncodeNoCRLF (const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  b, InMax3         : Cardinal;
  InPtr, InLimitPtr : ^Byte;
  OutPtr            : PByte4;
begin
  if InputByteCount <= 0 then
    Exit; //==>
  InPtr := @InputBuffer;
  InMax3 := InputByteCount div 3 * 3;
  OutPtr := @OutputBuffer;
  PAnsiChar(InLimitPtr):= PAnsiChar(InPtr) + InMax3;
  while InPtr <> InLimitPtr do
  begin
    b := InPtr^;
    b := b shl 8;
    Inc (InPtr);
    b := b or InPtr^;
    b := b shl 8;
    Inc (InPtr);
    b := b or InPtr^;
    Inc (InPtr);
    // Write 4 bytes to OutputBuffer (in reverse order).
    OutPtr^.b4 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
    b := b shr 6;
    OutPtr^.b1 := MIME_ENCODE_TABLE[b];
    Inc (OutPtr);
  end;

  case InputByteCount - InMax3 of
    1:
      begin
        b := InPtr^;
        b := b shl 4;
        OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr^.b1 := MIME_ENCODE_TABLE[b];
        OutPtr^.b3 := EQUAL_SIGN;            // Fill remaining 2 bytes.
        OutPtr^.b4 := EQUAL_SIGN;
      end;
    2:
      begin
        b := InPtr^;
        Inc (InPtr);
        b := b shl 8;
        b := b or InPtr^;
        b := b shl 2;
        OutPtr^.b3 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr^.b2 := MIME_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr^.b1 := MIME_ENCODE_TABLE[b];
        OutPtr^.b4 := EQUAL_SIGN;            // Fill remaining byte.
      end;
  end; { case }
end;

function MimeDecode (const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer): Cardinal;
var
 ByteBuffer, ByteBufferSpace: Cardinal;
begin
 ByteBuffer := 0;
 ByteBufferSpace := 4;
 Result := MimeDecodePartial (InputBuffer, InputBytesCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
 Inc (Result, MimeDecodePartialEnd (Pointer (Cardinal (OutputBuffer) + Result)^, ByteBuffer, ByteBufferSpace));
end;

function MimeDecodePartial (const InputBuffer; const InputBytesCount: Cardinal; out OutputBuffer; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var
 lByteBuffer, lByteBufferSpace, c: Cardinal;
 InPtr, InLimitPtr : ^Byte;
 OutPtr            : PByte3;
begin
 if InputBytesCount > 0 then
  begin
   InPtr := @InputBuffer;
   PAnsiChar (InLimitPtr):= PAnsiChar(InPtr) + InputBytesCount;
   OutPtr := @OutputBuffer;
   lByteBuffer := ByteBuffer;
   lByteBufferSpace := ByteBufferSpace;
   while InPtr <> InLimitPtr do
    begin
     c := MIME_DECODE_TABLE[InPtr^];    // Read from InputBuffer.
     Inc (InPtr);
     if c = $FF then
      Continue;
     lByteBuffer := lByteBuffer shl 6;
     lByteBuffer := lByteBuffer or c;
     Dec (lByteBufferSpace);
     if lByteBufferSpace <> 0 then
      Continue;                         // Read 4 bytes from InputBuffer?
     OutPtr^.b3 := Byte (lByteBuffer);   // Write 3 bytes to OutputBuffer (in reverse order).
     lByteBuffer := lByteBuffer shr 8;
     OutPtr^.b2 := Byte (lByteBuffer);
     lByteBuffer := lByteBuffer shr 8;
     OutPtr^.b1 := Byte (lByteBuffer);
     lByteBuffer := 0;
     Inc (OutPtr);
     lByteBufferSpace := 4;
    end;
   ByteBuffer := lByteBuffer;
   ByteBufferSpace := lByteBufferSpace;
   Result := Cardinal (OutPtr) - Cardinal (@OutputBuffer);
  end
 else
  Result := 0;
end;

function MimeDecodePartialEnd (out OutputBuffer; const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
var
 lByteBuffer       : Cardinal;
begin
 case ByteBufferSpace of
  1:
   begin
    lByteBuffer := ByteBuffer shr 2;
    PByte3 (@OutputBuffer)^.b2 := Byte (lByteBuffer);
    lByteBuffer := lByteBuffer shr 8;
    PByte3 (@OutputBuffer)^.b1 := Byte (lByteBuffer);
    Result := 2;
   end;
  2:
   begin
    lByteBuffer := ByteBuffer shr 4;
    PByte3 (@OutputBuffer)^.b1 := Byte (lByteBuffer);
    Result := 1;
   end;
  else
   Result := 0;
 end;
end;

{$IFDEF USE_BASE64_UNIT}
{ This is 32 & 64 bit safe }
function tiStreamToMIMEEncodeString(const AStream: TStream): string;
var
  OutStream: TStringStream;
  b64encoder: TBase64EncodingStream;
  LPos: integer;
begin
  LPos:= AStream.Position;
  try
    OutStream := TStringStream.Create('');
    try
      AStream.Position := 0;

      b64encoder := TBase64EncodingStream.Create(OutStream);
      b64encoder.CopyFrom(AStream, AStream.Size);
    finally
      b64encoder.Free;
      result := OutStream.DataString;
      OutStream.Free;
    end;
  finally
    AStream.Position:= LPos;
  end;
end;
{$ELSE}
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
{$ENDIF}

{$IFDEF USE_BASE64_UNIT}
{ This is 32 & 64 bit safe }
procedure tiMIMEEncodeStringToStream(const AString: string; const AStream: TStream);
var
  lStream: TStringStream;
  b64encoder: TBase64DecodingStream;
begin
  lStream:= TStringStream.Create(AString);
  try
    AStream.Size := 0;

    b64encoder := TBase64DecodingStream.Create(AStream);
    b64encoder.CopyFrom(lStream, lStream.Size);

    AStream.Position := 0;
  finally
    b64encoder.Free;
    lStream.Free;
  end;
end;
{$ELSE}
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
{$ENDIF}

function tiStreamDiscoverLineDelim(AStream: TStream): string;
const
  cBufLen = 1024;
var
  crPos, LfPos : LongInt;
  ls: ansistring;
  lReadCount: LongInt;
  lOldPos: Int64;
begin
  lOldPos := AStream.Position;
  AStream.Seek(0, soFromBeginning);
  crPos := 0;
  lfPos := 0;
  // default
  Result := cLineEnding;

  while (crPos = 0) and (lfPos = 0) and (AStream.Position <> AStream.Size) do
  begin
    SetLength(ls, cBufLen);
    lReadCount := AStream.Read(ls[1], cBufLen);

    if lReadCount < cBufLen then
      SetLength(ls, lReadCount);

    crPos := Pos(Cr, ls);
    lfPos := Pos(Lf, ls);

    if (crPos = 0) and (lfPos > 0) then
      Result := Lf
    else if (crPos > 0) and (lfPos = 0) then
    begin
      if AStream.Position = AStream.Size then
        Result := Cr
      else
        // handle case of Cr at end of buffer - rewind to crPos
        AStream.Seek(crPos - 1 - lReadCount, soFromCurrent);
    end
    else if (crPos > 0) and (lfPos > 0) then
    begin
      Result := CrLf;
    end;
  end;

  // reset stream state
  AStream.Seek(lOldPos, soFromBeginning);
end;

function tiStreamReadToNextToken(const AStream: TStream; const AToken: string): AnsiString;
const
  cBufLen = 1024;
var
  lPos : LongInt;
  lReadCount: LongInt;
  lTrim: LongInt;
  lStart: Int64;
  ls: ansistring;
  lLineDelimLen: integer;
begin
  lLineDelimLen := Length(AToken);
  lStart := AStream.Position;
  lPos := 0;

  while (lPos = 0) and (AStream.Position <> AStream.Size) do
  begin
    SetLength(ls, cBufLen);
    lReadCount := AStream.Read(ls[1], cBufLen);

    if lReadCount < cBufLen then
      SetLength(ls, lReadCount);

    lPos := Pos(AToken, ls);

    if lPos <> 0 then
    begin
      lTrim := lReadCount - (lPos - 1);
      SetLength(Result, AStream.Position - lTrim - lStart);
      AStream.Seek(lStart, soFromBeginning);
      AStream.Read(Result[1], Length(Result));
      // skip over ALineDelim
      AStream.Seek(lLineDelimLen, soFromCurrent);
    end
    else if lReadCount = cBufLen then
      // rewind far enough to handle partial ALineDelim at end of current buffer
      AStream.Seek( 1 - lLineDelimLen, soFromCurrent)
    else
    begin
      SetLength(Result, AStream.Position - lStart);
      AStream.Seek(lStart, soFromBeginning);
      AStream.Read(Result[1], Length(Result));
    end;
  end;
end;

procedure tiStreamReadToNextToken(const AStream: TStream; const AToken: string; const AOutput: TStream); overload;
const
  cBufLen = 1024;
var
  lPos : LongInt;
  lReadCount: LongInt;
  lTrim: LongInt;
  lStart: Int64;
  lsAnsi: ansistring;
  ls: string;
  lLineDelimLen: integer;
  LOutputSize: integer;
begin
  lLineDelimLen := Length(AToken);
  lStart := AStream.Position;
  lPos := 0;

  while (lPos = 0) and (AStream.Position <> AStream.Size) do
  begin
    SetLength(lsAnsi, cBufLen);
    lReadCount := AStream.Read(lsAnsi[1], cBufLen);

    if lReadCount < cBufLen then
      SetLength(lsAnsi, lReadCount);
    ls := string(lsAnsi);

    lPos := Pos(AToken, ls);

    if lPos <> 0 then
    begin
      lTrim := lReadCount - (lPos - 1);
      LOutputSize:= (AStream.Position - lTrim - lStart);
      AOutput.Size:= LOutputSize;
      AOutput.Position:= 0;
      if LOutputSize > 0 then
      begin
        AStream.Seek(lStart, soFromBeginning);
        AOutput.CopyFrom(AStream, LOutputSize);
        // skip over ALineDelim
        AStream.Seek(lLineDelimLen, soFromCurrent);
      end;
    end
    else if lReadCount = cBufLen then
      // rewind far enough to handle partial ALineDelim at end of current buffer
      AStream.Seek( 1 - lLineDelimLen, soFromCurrent)
    else
    begin
      LOutputSize:=(AStream.Position - lStart);
      AOutput.Size:= LOutputSize;
      AOutput.Position:= 0;
      if LOutputSize > 0 then
      begin
        AStream.Seek(lStart, soFromBeginning);
        AOutput.CopyFrom(AStream, LOutputSize);
      end;
    end;
  end;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiFileStream
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiFileStream.Create(const AFileName: string; Mode: Word);
begin
  inherited Create(AFileName, Mode);
  LineDelim := '';
end;

function TtiFileStream.ReadLn: AnsiString;
begin
  Result := tiStreamReadToNextToken(Self, LineDelim);
end;

procedure TtiFileStream.Write(const AString: String);
begin
  tiAppendStringToStream(AString, Self);
end;

procedure TtiFileStream.WriteLn(const AString: string = '');
begin
  Write(AString + LineDelim);
end;

procedure TtiFileStream.SetLineDelim(const AValue: string);
begin
  FLineDelim := AValue;
  FLineDelimLen := Length(FLineDelim);
end;

function TtiFileStream.EOF: boolean;
begin
  result := (Position = Size);
end;

function TtiFileStream.GetLineDelim: string;
begin
  if FLineDelimLen = 0 then
    LineDelim := tiStreamDiscoverLineDelim(Self);

  Result := FLineDelim;
end;

constructor TtiFileStream.CreateReadWrite(const AFileName: string; pOverwrite : boolean = false);
begin
  if FileExists(AFileName) and (not pOverwrite) then
    Create(AFileName, fmOpenReadWrite or fmShareDenyWrite)
  else
    Create(AFileName, fmCreate or fmShareDenyWrite)
end;

constructor TtiFileStream.CreateReadOnly(const AFileName: string);
begin
  Create(AFileName, fmOpenRead or fmShareDenyNone);
end;

function TtiPreSizedStream.AsString: String;
var
  LPosition: Cardinal;
begin
  LPosition:= FStream.Position;
  FStream.Position := 0;
  SetLength(Result,  FDataSize);
  FStream.Read(Result[1], Integer(FDataSize));
  FStream.Position:= LPosition;
end;

procedure TtiPreSizedStream.Clear;
begin
  FStream.Clear;
  FStreamSize := FInitialSize;
  FStream.Size := FStreamSize;
  FDataSize := 0;
end;

constructor TtiPreSizedStream.Create(AInitialSize, AGrowBy: Int64);
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FInitialSize := AInitialSize;
  FStreamSize := FInitialSize;
  FGrowBy := AGrowBy;
  FStream.Size := FStreamSize;
end;

destructor TtiPreSizedStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TtiPreSizedStream.GetPosition: Int64;
begin
  result:= FStream.Position;
end;

procedure TtiPreSizedStream.SaveToFile(const AFileName: string);
begin
  Assert(AFileName <> '', 'AFileName not assigned');
  tiForceDirectories(AFileName);
  FStream.Size := FDataSize;
  FStream.SaveToFile(AFileName);
  FStream.Seek(0, soFromEnd);
end;

procedure TtiPreSizedStream.Write(const AStr: string);
var
  lPC : Pointer;
  lLen : Integer;
begin
  lPC := Pointer(AStr);
  lLen := length(AStr); // * sizeof(char);
  while FStreamSize < FDataSize + lLen do
  begin
    Inc(FStreamSize, FGrowBy);
    FStream.Size := FStreamSize;
  end;
  FStream.WriteBuffer(lPC^, lLen);
  Inc(FDataSize, lLen);
end;

procedure TtiPreSizedStream.WriteLn(const AStr: string);
begin
  Write(AStr + tiLineEnd);
end;

type
  TtiBlockStreamItem = class(TtiBaseObject)
  private
    FStream: TMemoryStream;
    FBlockIndex: Longword;
    function  GetAsString: string;
    procedure SetAsString(const AValue: string);
    function  GetDataSize: Longword;
  public
    constructor Create(AData: string; ABlockIndex: Longword);
    destructor  Destroy; override;
    procedure   AppendToStream(AStream: TStream);

    property    AsString: string Read GetAsString Write SetAsString;
    property    BlockIndex: Longword Read FBlockIndex;
    property    DataSize: Longword Read GetDataSize;
  end;

{ TtiBlockStream }

constructor TtiBlockStream.Create(ABlockSize: Longword);
begin
  Assert(ABlockSize<>0, 'ABlockSize = 0');
  inherited Create;
  FList:= TObjectList.Create(True);
  FBlockSize:= ABlockSize;
end;


procedure TtiBlockStream.SetAsString(const AValue: string);
var
  LPos: Longword;
  LBlockIndex: Longword;
  LS: string;
begin
  FList.Clear;
  LPos:= 0;
  LBlockIndex:= 0;
  while LPos < Longword(Length(AValue)) do
  begin
    LS:= Copy(AValue, LPos+1, BlockSize);
    FList.Add(TtiBlockStreamItem.Create(LS, LBlockIndex));
    Inc(LPos, BlockSize);
    Inc(LBlockIndex);
  end;
end;


constructor TtiBlockStream.Create(const AData: string;ABlockSize: Longword);
begin
  Create(ABlockSize);
  SetAsString(AData);
end;


function TtiBlockStream.GetBlockAsString(ABlockIndex: Longword): string;
begin
  Assert(ABlockIndex < Longword(FList.Count), 'ABlockIndex >= FList.Count');
  Result := (FList.Items[ABlockIndex] as TtiBlockStreamItem).AsString
end;


procedure TtiBlockStream.SetBlockAsString(ABlockIndex: Longword;const AValue: string);
var
  LLast: TtiBlockStreamItem;
  LItem: TtiBlockStreamItem;
begin
  if Longword(Length(AValue)) > BlockSize then
      raise EtiOPFDataException.CreateFmt(cErrorBlockSizeMismatch, [BlockSize, Length(AValue)]);

  LItem:= FindByBlockIndex(ABlockIndex) as TtiBlockStreamItem;
  if LItem <> nil then
    LItem.AsString:= AValue
  else
  begin
    if FList.Count > 0 then
    begin
      LLast:= TtiBlockStreamItem(FList.Last);
      if (LLast.DataSize <> BlockSize) and
         (LLast.BlockIndex = Longword(FList.Count-1)) then
        raise EtiOPFDataException.CreateFmt(cErrorBlockSizeMismatch, [BlockSize, LLast.DataSize]);
    end;
    FList.Add(TtiBlockStreamItem.Create(AValue, ABlockIndex));
    Sort;
  end;
end;

function TtiBlockStream.GetAsString: string;
var
  LStream: TStringStream;
begin
  LStream:= TStringStream.Create('');
  try
    AssignToStream(LStream);
    Result:= LStream.DataString;
  finally
    LStream.Free;
  end;
end;


function TtiBlockStream.GetBlockCount: Longword;
begin
  Result:= FList.Count;
end;


destructor TtiBlockStream.Destroy;
begin
  FList.Free;
  inherited;
end;


function _CompareBlockStreamItems(AItem1, AItem2: Pointer): Integer;
var
  LItem1: TtiBlockStreamItem;
  LItem2: TtiBlockStreamItem;
begin
  Assert(TtiBaseObject(AItem1).TestValid(TtiBlockStreamItem), CTIErrorInvalidObject);
  Assert(TtiBaseObject(AItem2).TestValid(TtiBlockStreamItem), CTIErrorInvalidObject);
  LItem1:= TtiBlockStreamItem(AItem1);
  LItem2:= TtiBlockStreamItem(AItem2);
  Result:= CompareValue(LItem1.BlockIndex, LItem2.BlockIndex);
end;


procedure TtiBlockStream.Sort;
begin
  FList.Sort(_CompareBlockStreamItems);
end;


procedure TtiBlockStream.AssignToStream(AStream: TStream);
var
  i: Integer;
begin
  Assert(AStream<>nil, 'AStream not assigned');
  ValidateBlockSequence;
  Sort;
  AStream.Size:= 0;
  for i:= 0 to FList.Count-1 do
    (FList.Items[i] as TtiBlockStreamItem).AppendToStream(AStream);
end;


procedure TtiBlockStream.ValidateBlockSequence;
var
  i: Integer;
  LBlockIndex: Integer;
begin
  for i:= 0 to FList.Count - 1 do
  begin
    LBlockIndex:= (FList.Items[i] as TtiBlockStreamItem).BlockIndex;
    if LBlockIndex <> i then
      raise EtiOPFDataException.CreateFmt(cErrorBlockMissing, [i, FList.Count-1]);
  end;
end;


function TtiBlockStream.FindByBlockIndex(ABlockIndex: Longword): TtiBaseObject;
var
  i: Integer;
begin
  for i:= 0 to FList.Count-1 do
    if (FList.Items[i] as TtiBlockStreamItem).BlockIndex = ABlockIndex then
    begin
      Result:= FList.Items[i] as TtiBlockStreamItem;
      Exit; //==>
    end;
  Result:= nil;
end;


{ TtiBlockStreamItem }

procedure TtiBlockStreamItem.AppendToStream(AStream: TStream);
begin
  Assert(AStream<>nil, 'AStream not assigned');
  FStream.Position:= 0;
  AStream.Position:= AStream.Size;
  AStream.CopyFrom(FStream, FStream.Size);
end;


constructor TtiBlockStreamItem.Create(AData: string; ABlockIndex: Longword);
begin
  inherited Create;
  FStream:= TMemoryStream.Create;
  FBlockIndex:= ABlockIndex;
  AsString:= AData;
end;


destructor TtiBlockStreamItem.Destroy;
begin
  FStream.Free;
  inherited;
end;


function TtiBlockStreamItem.GetAsString: string;
begin
  Result:= tiStreamToString(FStream);
end;


function TtiBlockStreamItem.GetDataSize: Longword;
begin
  Result:= FStream.Size;
end;


procedure TtiBlockStreamItem.SetAsString(const AValue: string);
begin
  tiStringToStream(AValue, FStream);
end;

{ TtiLineStream }
constructor TtiLineStream.Create(const AStream: TStream);
begin
  Assert(Assigned(AStream), CTIErrorInvalidObject);
  inherited Create;
  FStream := AStream;
  LineDelim := '';
end;

function TtiLineStream.GetEOF: boolean;
begin
  result := (FStream.Position = FStream.Size);
end;

function TtiLineStream.GetLineDelim: string;
begin
  if FLineDelimLen = 0 then
    LineDelim := tiStreamDiscoverLineDelim(FStream);
  Result := FLineDelim;
end;

function TtiLineStream.GetPosition: Int64;
begin
  result := FStream.Position;
end;

function TtiLineStream.GetSize: Int64;
begin
  result := FStream.Size;
end;

function TtiLineStream.ReadLn: string;
begin
  Result := tiStreamReadToNextToken(FStream, LineDelim);
end;

function TtiLineStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result := FStream.Seek(Offset, Origin);
end;

procedure TtiLineStream.SetLineDelim(const AValue: string);
begin
  FLineDelim := AValue;
  FLineDelimLen := Length(FLineDelim);
end;

procedure TtiLineStream.SetPosition(const Pos: Int64);
begin
  FStream.Position := Pos;
end;

procedure TtiLineStream.SetSize(const NewSize: Int64);
begin
  FStream.Size := NewSize;
end;

procedure TtiLineStream.Write(const AString: string);
begin
  tiAppendStringToStream(AString, FStream);
end;

procedure TtiLineStream.WriteLn(const AString: string);
begin
  Write(AString + LineDelim);
end;

end.
