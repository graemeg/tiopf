unit tiCompressZLib;

{$I tiDefines.inc}

interface
uses
  tiCompress
  ,Classes
 ;

type

  // Implement TtiCompress using the ZLib library that comes with Delphi
  TtiCompressZLib = class(TtiCompressAbs)
  private
  public
    function  CompressStream(  AFrom : TStream; ATo : TStream): Extended; override;
    procedure DecompressStream(AFrom : TStream; ATo : TStream); override;
    function  CompressBuffer(  const AFrom: Pointer ; const AFromSize : Integer;
                                out   ATo:   Pointer ; out   AToSize  : Integer): Extended; override;
    procedure DecompressBuffer(const AFrom: Pointer ; const AFromSize : Integer;
                                out   ATo:   Pointer ; out   AToSize  : Integer); override;
    function  CompressString(  const AFrom : string; var ATo : string)  : Extended; override;
    procedure DecompressString(const AFrom : string; var ATo : string)  ; override;
    function  CompressFile(    const AFrom : string; const ATo : string): Extended; override;
    procedure DecompressFile(  const AFrom : string; const ATo : string); override;
  end;

implementation
uses
  {$IFDEF FPC}
  dzlib
//  PasZLib     { Could possibly use PasZLib directly in the future }
  {$ELSE}
  ZLib
  {$ENDIF}
  ,SysUtils
  ,tiConstants
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiCompressZLib
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// Compress a buffer
function TtiCompressZLib.CompressBuffer(const AFrom: Pointer;
  const AFromSize: Integer; out ATo: Pointer; out AToSize: Integer): Extended;
{$IFDEF FPC}
//var
//  vFromSize, vToSize : Cardinal;
{$ENDIF}
begin
  {$IFDEF FPC}
  dzLib.CompressBuf(AFrom, AFromSize, ATo, AToSize);
  { Could possibly use PasZLib directly in the future }
//  vFromSize := AFromSize;
//  vToSize := AToSize;
//  PasZLib.Compress(ATo, vToSize, AFrom, vFromSize);
  {$ELSE}
      ZLib.ZCompress(AFrom, AFromSize, ATo, AToSize);
  {$ENDIF}
  if AFromSize <> 0 then
    result := AToSize / AFromSize * 100
  else
    result := 0;
end;

// Compress a file
function TtiCompressZLib.CompressFile(const AFrom : string; const ATo: string): Extended;
var
  lStreamFrom : TFileStream;
  lStreamTo  : TFileStream;
begin
  lStreamFrom := TFileStream.Create(AFrom, fmOpenRead or fmShareExclusive);
  try
    lStreamTo  := TFileStream.Create(ATo, fmCreate or fmShareExclusive);
    try
      result := CompressStream(lStreamFrom, lStreamTo);
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

// Compress a stream
function TtiCompressZLib.CompressStream(AFrom, ATo: TStream): Extended;
var
  liFromSize : integer;
  liToSize  : integer;
  lBufFrom  : Pointer;
  lBufTo    : Pointer;
begin
  Assert(AFrom <> nil, 'From stream unassigned');
  Assert(ATo <> nil, 'To stream unassigned');

  try
    AFrom.Position := 0;

    if AFrom.Size = 0 then
    begin
      ATo.Size := 0;
      result := 0;
      Exit; //==>
    end;

    liFromSize := AFrom.Size;
    GetMem(lBufFrom, liFromSize);
    try
      AFrom.ReadBuffer(lBufFrom^, liFromSize);
      try
        result := CompressBuffer(lBufFrom, liFromSize, lBufTo, liToSize);
        ATo.Size := 0;
        ATo.WriteBuffer(lBufTo^, liToSize);
      finally
        FreeMem(lBufTo);
      end;
    finally
      FreeMem(lBufFrom);
    end;

    AFrom.Position := 0;
    ATo.Position := 0;

  except
    on e:exception do
      raise exception.Create('Error in TtiCompressZLib.CompressStream. Message: ' +
                              e.message);
  end;

end;

// Compress a string
function TtiCompressZLib.CompressString(const AFrom: string;
  var ATo: string): Extended;
var
  lStreamFrom : TStringStream;
  lStreamTo  : TStringStream;
begin
  { TODO : Perform this compression directly on the string as a buffer,
           don't go through the stream stage. }
  lStreamFrom := TStringStream.Create(AFrom);
  try
    lStreamTo  := TStringStream.Create('');
    try
      result := CompressStream(lStreamFrom, lStreamTo);
      ATo  := lStreamTo.DataString;
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

// Decompress a buffer
procedure TtiCompressZLib.DecompressBuffer(const AFrom: Pointer;
  const AFromSize: Integer; out ATo: Pointer; out AToSize: Integer);
{$IFDEF FPC}
//var
//  vFromSize, vToSize : cardinal;
{$ENDIF}
begin
  {$IFDEF FPC}
  dzlib.DecompressBuf(AFrom, AFromSize, AFromSize*2, ATo, AToSize);
  { Could possibly use PasZLib directly in the future }
//  vFromSize := AFromSize;
//  vToSize := AToSize;
//  PasZLib.uncompress(ATo, vToSize, AFrom, vFromSize);
  {$ELSE}
     ZLib.ZDecompress(AFrom, AFromSize, ATo, AToSize, AFromSize*2);
  {$ENDIF}
end;

// Decompress a file
procedure TtiCompressZLib.DecompressFile(const AFrom, ATo: string);
var
  lStreamFrom : TFileStream;
  lStreamTo  : TFileStream;
begin
  lStreamFrom := TFileStream.Create(AFrom, fmOpenRead or fmShareExclusive);
  try
    lStreamTo  := TFileStream.Create(ATo, fmCreate or fmShareExclusive);
    try
      DecompressStream(lStreamFrom, lStreamTo);
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

// Decompress a stream
procedure TtiCompressZLib.DecompressStream(AFrom, ATo: TStream);
var
  liToSize : integer;
  liFromSize : integer;
  lBufFrom : Pointer;
  lBufTo  : Pointer;
begin

  try
    if AFrom.Size = 0 then
    begin
      ATo.Size := 0;
      Exit; //==>
    end;

    AFrom.Position := 0;
    ATo.Size := 0;
    liFromSize := AFrom.Size;
    GetMem(lBufFrom,    liFromSize);
    try
      try
        AFrom.ReadBuffer(lBufFrom^, liFromSize);
        DecompressBuffer(lBufFrom, liFromSize, lBufTo, liToSize);
        ATo.Size := 0;
        ATo.WriteBuffer(lBufTo^, liToSize);
      finally
        FreeMem(lBufTo);
      end;
    finally
      FreeMem(lBufFrom);
    end;
    AFrom.Position := 0;
    ATo.Position := 0;
  except
    on e:exception do
      raise exception.Create('Error in TtiCompressZLib.DeCompressStream. Message: ' +
                              e.message);
  end;

end;

// Decompress a string
procedure TtiCompressZLib.DecompressString(const AFrom: string;
  var ATo: string);
var
  lStreamFrom : TStringStream;
  lStreamTo  : TStringStream;
begin
  lStreamFrom := TStringStream.Create(AFrom);
  try
    lStreamTo  := TStringStream.Create('');
    try
      DecompressStream(lStreamFrom, lStreamTo);
      ATo  := lStreamTo.DataString;
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

initialization
  // Register the TtiCompressZLib class with the factory
  gCompressFactory.RegisterClass(cgsCompressZLib, TtiCompressZLib);
  gtiCompressClass := TtiCompressZLib;
  
end.

