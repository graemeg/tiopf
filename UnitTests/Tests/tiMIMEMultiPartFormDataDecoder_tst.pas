unit tiMIMEMultiPartFormDataDecoder_tst;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork;

type

  TtiMIMEMultiPartFormDataDecoderTestCase = class(TtiTestCase)
  private
  protected
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
  end;

procedure RegisterTests;

implementation
uses
  tiUtils, tiDialogs,
  SysUtils,
  Classes,
  tiTestDependencies,
  tiMIMEMultipartFormDataDecoder;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TtiMIMEMultiPartFormDataDecoderTestCase);
end;

{ TtiMIMEMultiPartFormDataDecoderTestCase }

function TtiMIMEMultiPartFormDataDecoderTestCase.PathToInputFiles: string;
begin
  result:= tiGetEXEPath + '\DUnitSupportFiles\MIMEUpload\';
end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_Execute(
  const AInputFileName, AReturnFormat, AEmail, AFieldFileName,
  ADataFileName: string);
var
  LDecoder: TtiMIMEMultipartFormDataDecoder;
  LStream: TMemoryStream;
  LExpected: TMemoryStream;
begin
  LStream:= nil;
  LDecoder:= nil;
  try
    LStream:= TMemoryStream.Create;
    tiFileToStream(PathToInputFiles + AInputFileName, LStream);
    LDecoder:= TtiMIMEMultipartFormDataDecoder.Create;
    LDecoder.Execute(LStream);

    CheckEquals(3, LDecoder.ItemList.Count);
    CheckEquals('pdf_file', LDecoder.ItemList.Items[0].FieldName);
    CheckEquals(AFieldFileName, LDecoder.ItemList.Items[0].FileName);

    LExpected:= TMemoryStream.Create;
    try
      LExpected.LoadFromFile(PathToInputFiles + ADataFileName);
      CheckStreamContentsSame(LExpected, LDecoder.ItemList.Items[0].Data);
    finally
      LExpected.Free;
    end;

    CheckEquals('return_format', LDecoder.ItemList.Items[1].FieldName);
    CheckEquals(AReturnFormat, LDecoder.ItemList.Items[1].DataAsString);

    CheckEquals('email', LDecoder.ItemList.Items[2].FieldName);
    CheckEquals(AEmail, LDecoder.ItemList.Items[2].DataAsString);

  finally
    LStream.Free;
    LDecoder.Free;
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
begin

end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_FileName;
begin

end;

procedure TtiMIMEMultiPartFormDataDecoderTestCase.MIMEiMultipartFormDataDecoder_StringValue;
begin

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
