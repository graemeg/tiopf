unit tiStreams_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry
  {$ELSE}
  TestFramework
  {$ENDIF}
  ,tiTestFramework
 ;

type

  TTestTIStream = class(TtiTestCase)
  private
    procedure TestReadLn(const AStr, pLineDelim : string; ACount : integer);
    procedure TestEOF(ACount : integer);
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
  ,Classes
  ,SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
 ;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIStream);
end;

{ TTestTIStream }

procedure TTestTIStream.FileStream_Props;
var
  lData : TtiFileStream;
begin
  lData := TtiFileStream.Create(TempFileName, fmCreate or fmShareDenyNone);
  try
    CheckEquals(CrLf, lData.LineDelim, 'LineDelim');
    lData.LineDelim := Cr;
    CheckEquals(Cr, lData.LineDelim, 'LineDelim');
  finally
    lData.Free;
  end;
end;


procedure TTestTIStream.FileStream_ReadLn100;
begin
  TestReadLn('test', CrLf, 99);
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
  CheckEquals('test'+CrLf, ls);

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


procedure TTestTIStream.FileStream_ReadLn1WithLineEnd;
begin
  TestReadLn('test', CrLf, 1);
end;


procedure TTestTIStream.FileStream_ReadLn1000;
begin
  TestReadLn('test', CrLf, 1000);
end;


procedure TTestTIStream.TestReadLn(const AStr, pLineDelim: string; ACount: integer);
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


procedure TTestTIStream.FileStream_ReadLn2000;
begin
  TestReadLn('0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
              '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ',
              CrLf,
              2000);
end;


procedure TTestTIStream.FileStream_EOF;
begin
  TestEOF(0);
  TestEOF(1);
  TestEOF(2);
  TestEOF(10);
  TestEOF(1000);
end;


procedure TTestTIStream.FileStream_LineDelims;
begin
  TestReadLn('test', Cr, 10);
  TestReadLn('test', Lf, 10);
  TestReadLn('test', '|', 10);
  TestReadLn('test', '<p>', 10);
  TestReadLn('test', 'MyLineEnd', 10);
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

procedure TTestTIStream.TestEOF(ACount: integer);
var
  ls: string;
  lStream: TtiFileStream;
  i, lCount: integer;

const
  testString: string = 'test' + #13#10;

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
    CheckEquals(6, lStream.Size, 'Size');
    CheckEquals('test'#13#10, lStream.AsString, 'AsString');
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
