unit tiMIMEMultiPartFormDataDecoder_tst;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork,
   tiMIMEMultipartFormDataDecoder;

type

  TtiMIMEMultiPartFormDataDecoderTestCase = class(TtiTestCase)
  private
    FMimeDecoder: TtiMIMEMultipartFormDataDecoder;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function PathToInputFiles: string;
    procedure MIMEiMultipartFormDataDecoder_Execute(
      const AInputFileName: string;
      const AReturnFormat: string;
      const AEmail: string;
      const AFieldFileName: string;
      const ADataFileName: string);
  published
    procedure MIMEiMultipartFormDataDecoder_Execute_Text;
    procedure MIMEiMultipartFormDataDecoder_Execute_Text_EmptyEmail;
    procedure MIMEiMultipartFormDataDecoder_Execute_bmp;
    procedure MIMEiMultipartFormDataDecoder_Execute_Doc;
    procedure MIMEiMultipartFormDataDecoder_Execute_PDF;
    procedure MIMEiMultipartFormDataDecoder_StringValue;
    procedure MIMEiMultipartFormDataDecoder_FileName;
    procedure MIMEiMultipartFormDataDecoder_FileData;
    procedure MIMEiMultipartFormDataDecoder_ValueExists;
    procedure MIMEiMultipartFormDataDecoder_SaveFileDataToFile;
  end;

procedure RegisterTests;

implementation
uses
  tiUtils,
  tiDialogs,
  SysUtils,
  Classes,
  tiTestDependencies
  ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TtiMIMEMultiPartFormDataDecoderTestCase);
end;

{ TtiMIMEMultiPartFormDataDecoderTestCase }

procedure TtiMIMEMultiPartFormDataDecoderTestCase.SetUp;
begin
  inherited;
  FMimeDecoder:= TtiMIMEMultipartFormDataDecoder.Create;
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.TearDown;
begin
  FMimeDecoder.Free;
  inherited;
end;

function TtiMIMEMultiPartFormDataDecoderTestCase.PathToInputFiles: string;
begin
  result:= tiGetEXEPath + '\DUnitSupportFiles\MIMEUpload\';
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_Execute(
  const AInputFileName, AReturnFormat, AEmail, AFieldFileName,
  ADataFileName: string);
var
  LStream: TMemoryStream;
  LExpected: TMemoryStream;
begin
  LStream:= nil;
  try
    LStream:= TMemoryStream.Create;
    tiFileToStream(PathToInputFiles + AInputFileName, LStream);

    FMimeDecoder.Execute(LStream);
    CheckEquals(3, FMimeDecoder.ItemList.Count);
    CheckEquals('pdf_file', FMimeDecoder.ItemList.Items[0].FieldName);
    CheckEquals(AFieldFileName, FMimeDecoder.ItemList.Items[0].FileName);

    LExpected:= TMemoryStream.Create;
    try
      LExpected.LoadFromFile(PathToInputFiles + ADataFileName);
      CheckStreamContentsSame(LExpected, FMimeDecoder.ItemList.Items[0].Data);
    finally
      LExpected.Free;
    end;

    CheckEquals('return_format', FMimeDecoder.ItemList.Items[1].FieldName);
    CheckEquals(AReturnFormat, FMimeDecoder.ItemList.Items[1].DataAsString);

    CheckEquals('email', FMimeDecoder.ItemList.Items[2].FieldName);
    CheckEquals(AEmail, FMimeDecoder.ItemList.Items[2].DataAsString);
  finally
    LStream.Free;
  end;
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_Execute_Text;
begin
  MIMEiMultipartFormDataDecoder_Execute(
  'FormParams-text.dat',
  'QIF',
  'test',
  'ATestUpload.txt',
  'MIMEUploadTest.txt');
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_Execute_Text_EmptyEmail;
begin
  MIMEiMultipartFormDataDecoder_Execute(
  'FormParams-text-emptyemail.dat',
  'QIF',
  '',
  'ATestUpload.txt',
  'MIMEUploadTest.txt');
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_FileData;
var
  LStream : TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    MIMEiMultipartFormDataDecoder_Execute(
    'FormParams-text-emptyemail.dat',
    'QIF',
    '',
    'ATestUpload.txt',
    'MIMEUploadTest.txt');

    FMimeDecoder.FileData('pdf_file',LStream);
    CheckEquals(tiStreamtoString(LStream),'AAAAAAAA'+ CrLf + 'BBBBBBBB'+ CrLf + 'CCCCCCCC'+ CrLf);
  finally
    LStream.Free;
  end;
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_FileName;
begin
  MIMEiMultipartFormDataDecoder_Execute(
    'FormParams-text-emptyemail.dat',
    'QIF',
    '',
    'ATestUpload.txt',
    'MIMEUploadTest.txt');

  CheckEquals(FMimeDecoder.FileName('pdf_file'),'ATestUpload.txt');
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_SaveFileDataToFile;
begin
  MIMEiMultipartFormDataDecoder_Execute(
  'FormParams-text.dat',
  'QIF',
  'test',
  'ATestUpload.txt',
  'MIMEUploadTest.txt');

  FMimeDecoder.SaveFileDataToFile('email',PathToInputFiles +'test.out');
  CheckEquals('test', tiFileToString(PathToInputFiles +'test.out'));
  tiDeleteFile(PathToInputFiles +'test.out');
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_StringValue;
begin
  MIMEiMultipartFormDataDecoder_Execute(
      'FormParams-text.dat',
      'QIF',
      'test',
      'ATestUpload.txt',
      'MIMEUploadTest.txt');

  CheckEquals(FMimeDecoder.StringValue('email'),'test');
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_ValueExists;
begin
  MIMEiMultipartFormDataDecoder_Execute(
  'FormParams-text-emptyemail.dat',
  'QIF',
  '',
  'ATestUpload.txt',
  'MIMEUploadTest.txt');
  if not FMimeDecoder.ValueExists('email')
  then
    Check(false,'email section not found when it should have been');
  if FMimeDecoder.ValueExists('email1')
  then
    Check(false,'section email1 found when it shouldnt have been');
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_Execute_PDF;
begin
  MIMEiMultipartFormDataDecoder_Execute(
  'FormParams-pdf.dat',
  'CSV',
  'test text',
  'MIMEUploadTest.pdf',
  'MIMEUploadTest.pdf');
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_Execute_Doc;
begin
  MIMEiMultipartFormDataDecoder_Execute(
  'FormParams-doc.dat',
  'CSV',
  'test text',
  'MIMEUploadTest.doc',
  'MIMEUploadTest.doc');
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_Execute_bmp;
begin
  MIMEiMultipartFormDataDecoder_Execute(
  'FormParams-bmp.dat',
  'Text',
  'test',
  'ATestUpload.bmp',
  'MIMEUploadTest.bmp');
end;

end.
