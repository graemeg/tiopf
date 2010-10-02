unit tiStreams_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry,
  {$ENDIF}
  tiTestFramework
 ;

type

  TTestTIStream = class(TtiTestCase)
  private
    procedure TestFileReadLn(const AStr, pLineDelim : string; ACount : integer);
    procedure TestLineReadLn(const AStr, pLineDelim : string; ACount : integer);
    procedure TestFileEOF(ACount : integer);
    procedure TestLineEOF(ACount : integer);
  protected
  published
    procedure FileStream_Props;
    procedure FileStream_ReadLn1;
    procedure FileStream_ReadLn1WithLineEnd;
    procedure FileStream_ReadLn100;
    procedure FileStream_ReadLn1000;
    procedure FileStream_ReadLn2000;
    procedure FileStream_LineDelims;
    procedure FileStream_LineDelims2;
    procedure FileStream_EOF;
    procedure FileStream_Write;
    procedure FileStream_WriteLn;
    procedure PreSizedStream_Write;
    procedure PreSizedStream_WriteLn;
    procedure PreSizedStream_Clear;
    procedure PreSizedStream_AsString;

    procedure LineStream_Props;
    procedure LineStream_ReadLn1;
    procedure LineStream_ReadLn1WithLineEnd;
    procedure LineStream_ReadLn100;
    procedure LineStream_ReadLn1000;
    procedure LineStream_ReadLn2000;
    procedure LineStream_LineDelims;
    procedure LineStream_LineDelims2;
    procedure LineStream_EOF;
    procedure LineStream_Write;
    procedure LineStream_WriteLn;

    procedure BlockStream_GetBlockAsString;
    procedure BlockStream_SetBlockAsString_InSequence;
    procedure BlockStream_SetBlockAsString_OutOfSequence;
    procedure BlockStream_SetBlockAsString_BadInput;
  end;


procedure RegisterTests;


implementation
uses
   tiStreams
  ,tiUtils
  ,tiTestDependencies
  ,tiExcept
  ,tiConstants
  ,Classes
  ,SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
 ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIStream);
end;

{ TTestTIStream }

procedure TTestTIStream.TestFileReadLn(const AStr, pLineDelim: string; ACount: integer);
var
  ls : string;
  lStream : TtiFileStream;
  i : integer;
begin
  ls := '';
  for i := 1 to ACount do
    ls := ls + tiPad0(IntToStr(i),2) + AStr + pLineDelim;
  tiStringToFile(ls, TempFileName);
  lStream := TtiFileStream.CreateReadOnly(TempFileName);
  try
    lStream.LineDelim := pLineDelim;
    for i := 1 to ACount do
    begin
      ls := lStream.ReadLn;
      CheckEquals(tiPad0(IntToStr(i),2) + AStr, ls, '#' + IntToStr(i));
    end;
  finally
    lStream.Free;
  end;
end;


procedure TTestTIStream.TestLineReadLn(const AStr, pLineDelim: string; ACount: integer);
var
  ls : string;
  lStream : TMemoryStream;
  lLineStream : TtiLineStream;
  i : integer;
begin
  ls := '';
  for i := 1 to ACount do
    ls := ls + tiPad0(IntToStr(i),2) + AStr + pLineDelim;

  lStream := nil;
  lLineStream := nil;
  try
    lStream := TMemoryStream.Create;
    lLineStream := TtiLineStream.Create(lStream);

    tiStringToStream(ls, lStream);
    lLineStream.LineDelim := pLineDelim;
    for i := 1 to ACount do
    begin
      ls := lLineStream.ReadLn;
      CheckEquals(tiPad0(IntToStr(i),2) + AStr, ls, '#' + IntToStr(i));
    end;
  finally
    lLineStream.Free;
    lStream.Free;
  end;
end;


procedure TTestTIStream.TestFileEOF(ACount: integer);
var
  ls: string;
  lStream: TtiFileStream;
  i, lCount: integer;

const
  testString: string = 'test' + cLineEnding;

begin
  SetLength(ls, 0);
  for i := 1 to ACount do
     ls := ls + testString;

  tiStringToFile(ls, TempFileName);
  lStream := TtiFileStream.Create(TempFileName, fmOpenRead or fmShareDenyNone);
  try
    lCount := 0;
    while not lStream.EOF do
    begin
      Inc(lCount);
      lStream.ReadLn;
    end;
    CheckEquals(ACount, lCount);
  finally
    lStream.Free;
  end;
end;


procedure TTestTIStream.TestLineEOF(ACount: integer);
var
  ls: string;
  lStream: TMemoryStream;
  lLineStream: TtiLineStream;
  i, lCount: integer;

const
  testString: string = 'test' + cLineEnding;

begin
  SetLength(ls, 0);
  for i := 1 to ACount do
     ls := ls + testString;

  lStream := nil;
  lLineStream := nil;
  try
    lStream := TMemoryStream.Create;
    lLineStream := TtiLineStream.Create(lStream);

    tiStringToStream(ls, lStream);
    lCount := 0;
    while not lLineStream.EOF do
    begin
      Inc(lCount);
      lLineStream.ReadLn;
    end;
    CheckEquals(ACount, lCount);
  finally
    lLineStream.Free;
    lStream.Free;
  end;
end;


procedure TTestTIStream.FileStream_Props;
var
  lData : TtiFileStream;
begin
  lData := TtiFileStream.Create(TempFileName, fmCreate or fmShareDenyNone);
  try
    CheckEquals(tiLineEnd, lData.LineDelim, 'LineDelim');
    lData.LineDelim := Cr;
    CheckEquals(Cr, lData.LineDelim, 'LineDelim');
  finally
    lData.Free;
  end;
end;


procedure TTestTIStream.FileStream_ReadLn100;
begin
  TestFileReadLn('test', tiLineEnd, 99);
end;


procedure TTestTIStream.FileStream_ReadLn1;
var
  ls : string;
  lStream : TtiFileStream;
begin
  tiStringToFile('test', TempFileName);
  lStream := TtiFileStream.CreateReadOnly(TempFileName);
  try
    ls := lStream.ReadLn;
    CheckEquals('test', ls);
  finally
    lStream.Free;
  end;
end;


procedure TTestTIStream.FileStream_Write;
var
  ls : string;
  lStream : TtiFileStream;
begin
  lStream := TtiFileStream.CreateReadWrite(TempFileName);
  try
    lStream.Write('test');
  finally
    lStream.Free;
  end;
  ls := tiFileToString(TempFileName);
  CheckEquals('test', ls);
end;


procedure TTestTIStream.FileStream_WriteLn;
var
  ls : string;
  lStream : TtiFileStream;
begin
  lStream := TtiFileStream.CreateReadWrite(TempFileName);
  try
    lStream.WriteLn('test');
  finally
    lStream.Free;
  end;
  ls := tiFileToString(TempFileName);
  CheckEquals('test'+tiLineEnd, ls);

  tiDeleteFile(TempFileName);

  lStream := TtiFileStream.CreateReadWrite(TempFileName);
  try
    lStream.LineDelim := Cr;
    lStream.WriteLn('test');
  finally
    lStream.Free;
  end;
  ls := tiFileToString(TempFileName);
  CheckEquals('test'+Cr, ls);
end;


procedure TTestTIStream.LineStream_EOF;
begin
  TestLineEOF(0);
  TestLineEOF(1);
  TestLineEOF(2);
  TestLineEOF(10);
  TestLineEOF(1000);
end;


procedure TTestTIStream.LineStream_LineDelims;
begin
  TestLineReadLn('test', Cr, 10);
  TestLineReadLn('test', Lf, 10);
  TestLineReadLn('test', '|', 10);
  TestLineReadLn('test', '<p>', 10);
  TestLineReadLn('test', 'MyLineEnd', 10);
end;


procedure TTestTIStream.LineStream_LineDelims2;

  procedure _Test(ALineDelim, AError: string);
  var
    ls : string;
    lStream : TMemoryStream;
    lLineStream : TtiLineStream;
  begin
    lStream := nil;
    lLineStream := nil;
    try
      lStream := TMemoryStream.Create;
      lLineStream := TtiLineStream.Create(lStream);
      tiStringToStream('test' + ALineDelim, lStream);
      ls := lLineStream.ReadLn;
      CheckEquals(ALineDelim, lLineStream.LineDelim, AError);
      CheckEquals('test', ls, AError);
    finally
      lLineStream.Free;
      lStream.Free;
    end;
  end;

begin
  _Test(CrLf, 'CrLf');
  _Test(Cr, 'Cr');
  _Test(Lf, 'Lf');
end;


procedure TTestTIStream.LineStream_Props;
var
  lStream : TMemoryStream;
  lLineStream : TtiLineStream;
begin
  lStream := nil;
  lLineStream := nil;
  try
    lStream := TMemoryStream.Create;
    lLineStream := TtiLineStream.Create(lStream);

    Check(lLineStream.Stream = lStream, 'Stream');
    CheckEquals(0, lLineStream.Position, 'Position');
    CheckEquals(0, lLineStream.Size, 'Size');
    CheckEquals(true, lLineStream.EOF, 'EOF');
    CheckEquals(tiLineEnd, lLineStream.LineDelim, 'LineDelim');

    lLineStream.LineDelim := Cr;
    CheckEquals(Cr, lLineStream.LineDelim, 'LineDelim');

    tiStringToStream('1234567890', lStream);
    lStream.Position := 5;
    CheckEquals(5, lLineStream.Position, 'Position');
    CheckEquals(10, lLineStream.Size, 'Size');
    CheckEquals(false, lLineStream.EOF, 'EOF');
  finally
    lLineStream.Free;
    lStream.Free;
  end;
end;


procedure TTestTIStream.LineStream_ReadLn1;
var
  ls : string;
  lStream : TMemoryStream;
  lLineStream : TtiLineStream;
begin
  lStream := nil;
  lLineStream := nil;
  try
    lStream := TMemoryStream.Create;
    lLineStream := TtiLineStream.Create(lStream);
    tiStringToStream('test', lStream);
    ls := lLineStream.ReadLn;
    CheckEquals('test', ls);
  finally
    lLineStream.Free;
    lStream.Free;
  end;
end;


procedure TTestTIStream.LineStream_ReadLn100;
begin
  TestLineReadLn('test', tiLineEnd, 99);
end;


procedure TTestTIStream.LineStream_ReadLn1000;
begin
  TestLineReadLn('test', tiLineEnd, 1000);
end;


procedure TTestTIStream.LineStream_ReadLn1WithLineEnd;
begin
  TestLineReadLn('test', tiLineEnd, 1);
end;


procedure TTestTIStream.LineStream_ReadLn2000;
begin
  TestLineReadLn('0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
              tiLineEnd,
              2000);
end;


procedure TTestTIStream.LineStream_Write;
var
  ls : string;
  lStream : TMemoryStream;
  lLineStream : TtiLineStream;
begin
  lStream := nil;
  lLineStream := nil;
  try
    lStream := TMemoryStream.Create;
    lLineStream := TtiLineStream.Create(lStream);

    lLineStream.Write('test');
    ls := tiStreamToString(lStream);
    CheckEquals('test', ls);
  finally
    lLineStream.Free;
    lStream.Free;
  end;
end;


procedure TTestTIStream.LineStream_WriteLn;
var
  ls : string;
  lStream : TMemoryStream;
  lLineStream : TtiLineStream;
begin
  lStream := nil;
  lLineStream := nil;
  try
    lStream := TMemoryStream.Create;
    lLineStream := TtiLineStream.Create(lStream);

    lLineStream.WriteLn('test');
    ls := tiStreamToString(lStream);
    CheckEquals('test'+tiLineEnd, ls);

    lStream.Clear;
    lLineStream.LineDelim := Cr;
    lLineStream.WriteLn('test');
    ls := tiStreamToString(lStream);
    CheckEquals('test'+Cr, ls);
  finally
    lLineStream.Free;
    lStream.Free;
  end;
end;


procedure TTestTIStream.FileStream_ReadLn1WithLineEnd;
begin
  TestFileReadLn('test', tiLineEnd, 1);
end;


procedure TTestTIStream.FileStream_ReadLn1000;
begin
  TestFileReadLn('test', tiLineEnd, 1000);
end;


procedure TTestTIStream.FileStream_ReadLn2000;
begin
  TestFileReadLn('0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
              tiLineEnd,
              2000);
end;


procedure TTestTIStream.FileStream_EOF;
begin
  TestFileEOF(0);
  TestFileEOF(1);
  TestFileEOF(2);
  TestFileEOF(10);
  TestFileEOF(1000);
end;


procedure TTestTIStream.FileStream_LineDelims;
begin
  TestFileReadLn('test', Cr, 10);
  TestFileReadLn('test', Lf, 10);
  TestFileReadLn('test', '|', 10);
  TestFileReadLn('test', '<p>', 10);
  TestFileReadLn('test', 'MyLineEnd', 10);
end;


procedure TTestTIStream.FileStream_LineDelims2;

  procedure _Test(ALineDelim, AError: string);
  var
    ls : string;
    lStream : TtiFileStream;
  begin
    tiStringToFile('test' + ALineDelim, TempFileName);
    lStream := TtiFileStream.CreateReadOnly(TempFileName);
    try
      ls := lStream.ReadLn;
      CheckEquals(ALineDelim, lStream.LineDelim, AError);
      CheckEquals('test', ls, AError);
    finally
      lStream.Free;
    end;
  end;

begin
  _Test(CrLf, 'CrLf');
  _Test(Cr, 'Cr');
  _Test(Lf, 'Lf');
end;

procedure TTestTIStream.PreSizedStream_AsString;
var
  LStream: TtiPreSizedStream;
begin
  LStream:= TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
  try
    CheckEquals(0, LStream.Position);
    LStream.Write('ABC');
    CheckEquals(3, LStream.Size, 'ABC');
    CheckEquals(3, LStream.Position);
    CheckEquals('ABC', LStream.AsString);
    CheckEquals(3, LStream.Position);

    LStream.Write('123');
    CheckEquals(6, LStream.Size, 'ABC');
    CheckEquals(6, LStream.Position);
    CheckEquals('ABC123', LStream.AsString);
    CheckEquals(6, LStream.Position);

  finally
    LStream.Free;
  end;
end;

procedure TTestTIStream.PreSizedStream_Clear;
var
  lStream: TtiPreSizedStream;
begin
  lStream:= TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
  try
    lStream.Write('test');
    CheckEquals(4, lStream.Size, 'Size');
    lStream.Clear;
    CheckEquals(0, lStream.Size, 'Size');
    CheckEquals('', lStream.AsString, 'AsString');
  finally
    lStream.Free;
  end;
end;


procedure TTestTIStream.PreSizedStream_Write;
var
  lStream: TtiPreSizedStream;
begin
  lStream:= TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
  try
    lStream.Write('test');
    CheckEquals(4, lStream.Size, 'Size');
    CheckEquals('test', lStream.AsString, 'AsString');
  finally
    lStream.Free;
  end;
end;

procedure TTestTIStream.PreSizedStream_WriteLn;
var
  lStream: TtiPreSizedStream;
begin
  lStream:= TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
  try
    lStream.WriteLn('test');
    CheckEquals(4 + Length(tiLineEnd), lStream.Size, 'Size');
    CheckEquals('test'+tiLineEnd, lStream.AsString, 'AsString');
  finally
    lStream.Free;
  end;
end;


procedure TTestTIStream.BlockStream_GetBlockAsString;
var
  LStream: TtiBlockStream;
  L: string;
begin
  LStream:= TtiBlockStream.Create('abcDEFghiJKLmn', 3);
  try
    CheckEquals(3, LStream.BlockSize,  'BlockSize');
    CheckEquals(5, LStream.BlockCount, 'BlockCount');
    CheckEquals('abcDEFghiJKLmn', LStream.AsString);

    L:= LStream.BlockAsString[0];
    CheckEquals('abc', L);

    L:= LStream.BlockAsString[1];
    CheckEquals('DEF', L);

    L:= LStream.BlockAsString[2];
    CheckEquals('ghi', L);

    L:= LStream.BlockAsString[3];
    CheckEquals('JKL', L);

    L:= LStream.BlockAsString[4];
    CheckEquals('mn', L);

  finally
    LStream.Free;
  end;
end;


procedure TTestTIStream.BlockStream_SetBlockAsString_InSequence;
var
  LStream: TtiBlockStream;
begin
  LStream:= TtiBlockStream.Create(3);
  try
    LStream.BlockAsString[0]:= 'abc';
    LStream.BlockAsString[1]:= 'DEF';
    LStream.BlockAsString[2]:= '01';
    CheckEquals('abcDEF01', LStream.AsString, 'AsString');
    CheckEquals('abc', LStream.BlockAsString[0], '#0');
    CheckEquals('DEF', LStream.BlockAsString[1], '#1');
    CheckEquals('01',  LStream.BlockAsString[2], '#2');
  finally
    LStream.Free;
  end;
end;


procedure TTestTIStream.BlockStream_SetBlockAsString_OutOfSequence;
var
  LStream: TtiBlockStream;
begin
  LStream:= TtiBlockStream.Create(3);
  try
    LStream.BlockAsString[1]:= 'DEF';
    LStream.BlockAsString[3]:= '01';
    LStream.BlockAsString[0]:= 'abc';
    LStream.BlockAsString[2]:= 'ghi';

    CheckEquals('abcDEFghi01', LStream.AsString, 'AsString');
    CheckEquals('abc', LStream.BlockAsString[0], '#0');
    CheckEquals('DEF', LStream.BlockAsString[1], '#1');
    CheckEquals('ghi', LStream.BlockAsString[2], '#2');
    CheckEquals('01',  LStream.BlockAsString[3], '#3');

  finally
    LStream.Free;
  end;
end;


procedure TTestTIStream.BlockStream_SetBlockAsString_BadInput;
var
  LStream: TtiBlockStream;
begin
  LStream:= TtiBlockStream.Create(3);
  try
    try
      LStream.BlockAsString[0]:= 'abcd';
      Fail('Exception not raised');
    except
      on e:Exception do
        CheckIs(e, EtiOPFDataException);
    end;
  finally
    LStream.Free;
  end;

  LStream:= TtiBlockStream.Create(3);
  try
    try
      LStream.BlockAsString[0]:= 'ab';
      LStream.BlockAsString[1]:= 'cde';
      Fail('Exception not raised');
    except
      on e:Exception do
        CheckIs(e, EtiOPFDataException);
    end;
  finally
    LStream.Free;
  end;
end;


end.
