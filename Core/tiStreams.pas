unit tiStreams;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,Classes
{$IFDEF IOS}
  ,System.Generics.Defaults
  ,Generics.Collections
{$ELSE}
  ,Contnrs
{$ENDIF IOS}
 ;

const
  cStreamStartSize = 1024;
  cStreamGrowBy    = 1024;

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
    procedure   AssignTo(const AStreamTo: TStream);
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
    function    ReadLn : string;
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


  {: Manage a stream in chunks, or blocks. Current interface supports access to the stream via the AsString property.
     This can be extended to support access via TStream if required. Stream is zero indexed, so a BlockIndex=0 is the
     first block of data.}
  TtiBlockStream = class(TtiBaseObject)
  private
{$IFDEF IOS}
    FList: TObjectList<TtiBlockStreamItem>;
{$ELSE}
    FList: TObjectList;
{$ENDIF IOS}
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

function tiStreamReadToNextToken(const AStream: TStream; const AToken: string): string; overload;
procedure tiStreamReadToNextToken(const AStream: TStream; const AToken: string; const AOutput: TStream); overload;

implementation

uses
   tiUtils
  ,tiExcept
  ,tiConstants
  ,SysUtils
  ,Math
{$IFNDEF IOS}
  ,tiMime
{$ENDIF IOS}
 ;

{$IFDEF IOS}
type
  AnsiString = Array of Byte;
{$ENDIF IOS}

function tiStreamDiscoverLineDelim(AStream: TStream): string;
const
  cBufLen = 1024;
var
  crPos, LfPos : LongInt;
  lsAnsi: ansistring;
  ls: string;
  lReadCount: LongInt;
  lOldPos: Int64;
begin
  lOldPos := AStream.Position;
  AStream.Seek(0, soFromBeginning);
  crPos := 0;
  lfPos := 0;
  // default
  if (AStream.Size = 0) then
    Result := tiLineEnd
  else
    Result := CrLf;

  while (crPos = 0) and (lfPos = 0) and (AStream.Position <> AStream.Size) do
  begin
    SetLength(lsAnsi, cBufLen);
    lReadCount := AStream.Read(lsAnsi[1], cBufLen);

    if lReadCount < cBufLen then
      SetLength(lsAnsi, lReadCount);
    ls := string(lsAnsi);

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
    end;
  end;

  // reset stream state
  AStream.Seek(lOldPos, soFromBeginning);
end;

function tiStreamReadToNextToken(const AStream: TStream; const AToken: string): string;
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
  LAnsiResult: AnsiString;
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
      SetLength(LAnsiResult, AStream.Position - lTrim - lStart);
      AStream.Seek(lStart, soFromBeginning);
      AStream.Read(LAnsiResult[1], Length(LAnsiResult));
      // skip over ALineDelim
      AStream.Seek(lLineDelimLen, soFromCurrent);
    end
    else if lReadCount = cBufLen then
      // rewind far enough to handle partial ALineDelim at end of current buffer
      AStream.Seek( 1 - lLineDelimLen, soFromCurrent)
    else
    begin
      SetLength(LAnsiResult, AStream.Position - lStart);
      AStream.Seek(lStart, soFromBeginning);
      AStream.Read(LAnsiResult[1], Length(LAnsiResult));
    end;
  end;

  Result := string(LAnsiResult);
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

function TtiFileStream.ReadLn: string;
begin
  Result := tiStreamReadToNextToken(Self, LineDelim);
end;

procedure TtiFileStream.Write(const AString: string);
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
{$IFNDEF IOS}
    LineDelim := tiStreamDiscoverLineDelim(Self);
{$ELSE}
    LineDelim := CR;  {TODO: IOS calculation of LineDelim }
{$ENDIF IOS}

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

procedure TtiPreSizedStream.AssignTo(const AStreamTo: TStream);
var
  LSavePosition: Integer;
begin
  Assert(AStreamTo <> nil, 'AStreamTo not assigned');
  if FDataSize > 0 then
  begin
    LSavePosition:= FStream.Position;
    FStream.Position:= 0;
    AStreamTo.Size:= 0;
    AStreamTo.CopyFrom(FStream, FDataSize);
    FStream.Position:= LSavePosition;
  end else
    AStreamTo.Size:= 0;
end;

function TtiPreSizedStream.AsString: string;
var
  LPosition: Cardinal;
  LAnsiResult: AnsiString;
begin
  LPosition:= FStream.Position;
  FStream.Position := 0;
  SetLength(LAnsiResult,  FDataSize);
  FStream.Read(LAnsiResult[1], FDataSize);
  FStream.Position:= LPosition;
  Result := string(LAnsiResult);
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
  LAnsiStr: AnsiString;
  lPC : Pointer;
  lLen : Integer;
begin
  LAnsiStr := AnsiString(AStr);
  lPC := Pointer(LAnsiStr);
  lLen := length(LAnsiStr); // * sizeof(char);
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

{ TtiBlockStream }

constructor TtiBlockStream.Create(ABlockSize: Longword);
begin
  Assert(ABlockSize<>0, 'ABlockSize = 0');
  inherited Create;
{$IFDEF IOS}
  FList:= TObjectList<TtiBlockStreamItem>.Create(True);
{$ELSE}
  FList:= TObjectList.Create(True);
{$ENDIF IOS}
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
{$IFDEF IOS}
  FList.Sort(TComparer<TtiBlockStreamItem>.Construct(
   function (const L, R: TtiBlockStreamItem): integer
   begin
     result := _CompareBlockStreamItems(L, R);
   end
   ));
{$ELSE}
  FList.Sort(_CompareBlockStreamItems);
{$ENDIF IOS}
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
