unit tiCompress_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework
 ;

type

  TTestTICompress = class(TtiTestCase)
  private
    function  GetTestString : string;
    procedure Do_FileCompression(  const pCompressionType : string; AValue: string);
    procedure Do_StringCompression(const pCompressionType : string; AValue: string);
    procedure Do_StreamCompression(const pCompressionType : string; AValue: string);
  published
    procedure None_FileCompression;
    procedure None_StringCompression;
    procedure None_StreamCompression;

    procedure ZLib_FileCompression;
    procedure ZLib_StringCompression;
    procedure ZLib_StreamCompression;

    procedure tiCompressAndDecompressString;
    procedure tiCompressAndDecompressStream;
    procedure tiCompressAndDecompressStringToFile;
    procedure tiDecompressFileToStream;

  end;

procedure RegisterTests;

implementation
uses
  tiCompress
  ,tiUtils
  ,Classes
  ,SysUtils
  ,tiCompressNone
  ,tiCompressZLib
  ,tiTestDependencies
  ,tiConstants
 ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTICompress);
end;

{ TTestTICompress }

procedure TTestTICompress.Do_FileCompression(const pCompressionType : string; AValue: string);
var
  lCompress : TtiCompressAbs;
  //lRatio  : Extended;
  lBefore : string;
  lAfter : string;
  lFileNameBefore : string;
  lFileNameAfter : string;
begin
  lFileNameBefore := tiGetTempDir + '\CompressionTestBefore.txt';
  lFileNameAfter := tiGetTempDir + '\CompressionTestAfter.txt';
  lBefore := AValue;
  tiStringToFile(lBefore, lFileNameBefore);
  lCompress := gCompressFactory.CreateInstance(pCompressionType);
  try
    {lRatio :=} lCompress.CompressFile(  lFileNameBefore,
                                         lFileNameAfter);
    //Log([ 'Compression ratio for file', pCompressionType, lRatio ]);
    tiDeleteFile(lFileNameBefore);
    lCompress.DecompressFile(lFileNameAfter,
                              lFileNameBefore);
  finally
    lCompress.Free;
  end;

  lAfter := tiFileToString(lFileNameBefore);

  Check(lAfter = lBefore,
         'Compression failed. Files are not the same.');
  tiDeleteFile(lFileNameBefore);
  tiDeleteFile(lFileNameAfter);

end;

procedure TTestTICompress.Do_StreamCompression(const pCompressionType : string; AValue: string);
var
  lBefore : TStringStream;
  lCompressed : TStringStream;
  lAfter : TStringStream;
  lCompress : TtiCompressAbs;
  //lRatio  : Extended;
begin

  lBefore := TStringStream.Create(AValue);
  lCompressed := TStringStream.Create('');
  lAfter := TStringStream.Create('');
  try
    lCompress := gCompressFactory.CreateInstance(pCompressionType);
    try
      {lRatio :=} lCompress.CompressStream(lBefore, lCompressed);
      //Log([ 'Compression ratio for stream', pCompressionType, lRatio ]);
      lCompress.DecompressStream(lCompressed, lAfter);
      Check(lBefore.DataString = lAfter.DataString,
             'Compression failed. Streams are not the same.');
    finally
      lCompress.Free;
    end;
  finally
    lBefore.Free;
    lCompressed.Free;
    lAfter.Free;
  end;
end;

procedure TTestTICompress.Do_StringCompression(const pCompressionType : string; AValue: string);
var
  lCompress   : TtiCompressAbs;
  lBefore    : string;
  lCompressed  : string;
  lAfter : string;
  //lRatio     : Extended;
begin

  // Create the appropriate TtiCompress concrete
  lCompress := gCompressFactory.CreateInstance(pCompressionType);
  try
    // Get some text to compress
    lBefore := AValue;
    // Compress the text, returning the compression ratio
    {lRatio :=} lCompress.CompressString(lBefore,
                                        lCompressed);
    //Log([ 'Compression ration for string', pCompressionType, lRatio ]);
    // Decompress the text
    lCompress.DecompressString(lCompressed,
                                lAfter);
  finally
    lCompress.Free;
  end;

  Check(lBefore = lAfter,
         'Compression failed. Strings are not the same.');

end;

function TTestTICompress.GetTestString: string;
var
  i     : integer;
  lsLine : string;
begin
  lsLine := '';
  for i := 1 to 1000 do
    lsLine := lsLine + Chr(ord('A')+random(ord('z')-ord('A')));
  for i := 1 to 200 do
    result := result + lsLine + #13 + #10;
end;

procedure TTestTICompress.None_FileCompression;
begin
  Do_FileCompression(cgsCompressNone, '');
  Do_FileCompression(cgsCompressNone, GetTestString);
end;

procedure TTestTICompress.None_StreamCompression;
begin
  Do_StreamCompression(cgsCompressNone, '');
  Do_StreamCompression(cgsCompressNone, GetTestString);
end;

procedure TTestTICompress.None_StringCompression;
begin
  Do_StringCompression(cgsCompressNone, '');
  Do_StringCompression(cgsCompressNone, GetTestString);
end;

procedure TTestTICompress.tiDecompressFileToStream;
var
  LBefore: string;
  LAfter: string;
  LStreamAfter: TMemoryStream;
  LFileName: string;
begin
  LFileName:= TempFileName('tiCompressAndDecompressString.zlib');
  LBefore:= GetTestString;
  LStreamAfter:= TMemoryStream.Create;
  try
    tiCompressStringToFile(LBefore, LFileName);
    tiCompress.tiDecompressFileToStream(LFileName, LStreamAfter);
    LAfter:= tiCompress.tiDecompressFileToString(LFileName);
    CheckEquals(LBefore, LAfter);
  finally
    LStreamAfter.Free;
  end;
end;

procedure TTestTICompress.tiCompressAndDecompressStream;
var
  LBefore: TMemoryStream;
  LDuring: TMemoryStream;
  LAfter: TMemoryStream;
begin
  LBefore:= nil;
  LDuring:= nil;
  LAfter:= nil;
  try
    LBefore:= TMemoryStream.Create;
    LDuring:= TMemoryStream.Create;
    LAfter:= TMemoryStream.Create;
    tiStringToStream(GetTestString, LBefore);
    tiCompress.tiCompressStream(LBefore, LDuring);
    tiDeCompressStream(LDuring, LAfter);
    CheckEquals(LBefore, LAfter);
  finally
    LBefore.Free;
    LDuring.Free;
    LAfter.Free;
  end;
end;

procedure TTestTICompress.tiCompressAndDecompressStringToFile;
var
  LBefore: string;
  LAfter: string;
  LFileName: string;
begin
  LFileName:= TempFileName('tiCompressAndDecompressString.zlib');
  LBefore:= GetTestString;
  tiCompress.tiCompressStringToFile(LBefore, LFileName);
  LAfter:= tiCompress.tiDecompressFileToString(LFileName);
  CheckEquals(LBefore, LAfter);
end;

procedure TTestTICompress.tiCompressAndDecompressString;
var
  LBefore: string;
  LDuring: string;
  LAfter: string;
begin
  LBefore:= GetTestString;
  LDuring:= tiCompress.tiCompressString(LBefore);
  LAfter:= tiCompress.tiDeCompressString(LDuring);
  CheckEquals(LBefore, LAfter);
end;

procedure TTestTICompress.ZLib_FileCompression;
begin
  Do_FileCompression(cgsCompressZLib, '');
  Do_FileCompression(cgsCompressZLib, GetTestString);
end;

procedure TTestTICompress.ZLib_StreamCompression;
begin
  Do_StreamCompression(cgsCompressZLib, '');
  Do_StreamCompression(cgsCompressZLib, GetTestString);
end;

procedure TTestTICompress.ZLib_StringCompression;
begin
  Do_StringCompression(cgsCompressZLib, '');
  Do_StringCompression(cgsCompressZLib, GetTestString);
end;

end.


