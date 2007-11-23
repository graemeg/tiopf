unit tiEncrypt_TST;

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

  TTestTIEncrypt = class(TtiTestCase)
  private
    function  GetTestString : string;
  protected
    procedure Do_StringEncryption(const pEncryptionType : string; const pSeed : string = ''; const pIntSeed : Int64 = 0);
    procedure Do_StreamEncryption(const pEncryptionType : string; const pSeed : string = ''; const pIntSeed : Int64 = 0);
  published
    procedure None_StringEncryption;
    procedure None_StreamEncryption;
    { will add these back shortly }
    {$IFNDEF FPC}
    procedure Simple_StringEncryption;
    procedure Simple_StreamEncryption;
    {$ENDIF}
    procedure DES_StreamEncryption;
    procedure DES_StringEncryption;
    procedure Blowfish_StreamEncryption;
    procedure Blowfish_StringEncryption;
  end;


procedure RegisterTests;

implementation
uses
  Classes
  ,tiTestDependencies
  ,tiEncrypt
  ,tiEncryptSimple
  ,tiEncryptNone
  ,tiEncryptDES
  ,tiEncryptBlowfish
 ;

const
  uSeed = 'blah1$blah2#yada3&yada4';
  uIntSeed = 61431572978;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIEncrypt);
end;


procedure TTestTIEncrypt.Do_StreamEncryption(const pEncryptionType : string; const pSeed : string; const pIntSeed : Int64);
var
  lBefore   : TStringStream;
  lEncrypted : TStringStream;
  lAfter    : TStringStream;
  lEncrypt  : TtiEncryptAbs;
  lZero     : Byte;
  lsTest    : string;
begin

  lBefore   := TStringStream.Create('');
  lEncrypted := TStringStream.Create('');
  lAfter    := TStringStream.Create('');
  lZero     := 0;
  lsTest    := GetTestString;
  try
    lBefore.WriteString(lsTest);  // some text
    lBefore.Write(lZero, 1); // Embed zero to simulate binary data
    lBefore.WriteString(lsTest); // and some more text
    lEncrypt := gEncryptFactory.CreateInstance(pEncryptionType);
    lEncrypt.Seed   := pSeed;
    lEncrypt.IntSeed := pIntSeed;
    try
      lEncrypt.EncryptStream(lBefore, lEncrypted);
      lEncrypt.DecryptStream(lEncrypted, lAfter);
    finally
      lEncrypt.Free;
    end;

  if pIntSeed <> 0 then
    Check(lBefore.DataString = lAfter.DataString, 'Seeded Encryption failed. Streams are not the same.') 
  else
    Check(lBefore.DataString = lAfter.DataString, 'Encryption failed. Streams are not the same.');

  finally
    lBefore.Free;
    lEncrypted.Free;
    lAfter.Free;
  end;
end;


procedure TTestTIEncrypt.Do_StringEncryption(const pEncryptionType : string; const pSeed : string; const pIntSeed : Int64);
var
  lsBefore : string;
  lsEncrypt : string;
  lsAfter  : string;
  lEncrypt : TtiEncryptAbs;
begin
  lsBefore := GetTestString; // was FsTest
  lEncrypt := gEncryptFactory.CreateInstance(pEncryptionType);
  lEncrypt.Seed   := pSeed;
  lEncrypt.IntSeed := pIntSeed;

  try
    lsEncrypt := lEncrypt.EncryptString(lsBefore);
    lsAfter  := lEncrypt.DecryptString(lsEncrypt);
  finally
    lEncrypt.Free;
  end;

  if pIntSeed <> 0 then
    Check(lsBefore = lsAfter, 'Seeded Encryption failed. Strings are not the same.')
  else
    Check(lsBefore = lsAfter, 'Encryption failed. Strings are not the same.');
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TEncryptTest
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//procedure TEncryptTest.Execute(const psTest : string);
//var
//  i : integer;
//begin
//  FsMessageText := '';
//  FsTest := psTest;
//  for i := 1 to 5 do begin
//    FsMessageText := FsMessageText + 'Test #' + IntToStr(i) + #13;
//    TestString;
//    TestStream;
//    TestFile  ;
//    FsMessageText := FsMessageText + #13;
//    FEncrypt.NewSeed;
//  end;
//end;


procedure TTestTIEncrypt.None_StreamEncryption;
begin
  Do_StreamEncryption(cgsEncryptionNone);
  Do_StreamEncryption(cgsEncryptionNone, uSeed, uIntSeed);
end;


procedure TTestTIEncrypt.None_StringEncryption;
begin
  Do_StringEncryption(cgsEncryptionNone);
  Do_StringEncryption(cgsEncryptionNone, uSeed, uIntSeed);
end;


{$IFNDEF FPC}
procedure TTestTIEncrypt.Simple_StreamEncryption;
begin
  Do_StreamEncryption(cgsEncryptionSimple);
  Do_StreamEncryption(cgsEncryptionSimple, uSeed, uIntSeed);
end;
{$ENDIF}


{$IFNDEF FPC}
procedure TTestTIEncrypt.Simple_StringEncryption;
begin
  Do_StringEncryption(cgsEncryptionSimple);
  Do_StringEncryption(cgsEncryptionSimple, uSeed, uIntSeed);
end;
{$ENDIF}


procedure TTestTIEncrypt.DES_StreamEncryption;
begin
  Do_StreamEncryption(cgsEncryptionDES);
  Do_StreamEncryption(cgsEncryptionDES, uSeed, uIntSeed);
end;


procedure TTestTIEncrypt.DES_StringEncryption;
begin
  Do_StringEncryption(cgsEncryptionDES);
  Do_StringEncryption(cgsEncryptionDES, uSeed, uIntSeed);
end;


procedure TTestTIEncrypt.Blowfish_StreamEncryption;
begin
  Do_StreamEncryption(cgsEncryptionBlowfish);
  Do_StreamEncryption(cgsEncryptionBlowfish, uSeed, uIntSeed);
end;


procedure TTestTIEncrypt.Blowfish_StringEncryption;
begin
  Do_StringEncryption(cgsEncryptionBlowfish);
  Do_StringEncryption(cgsEncryptionBlowfish, uSeed, uIntSeed);
end;


function TTestTIEncrypt.GetTestString: string;
var
  i     : integer;
  lsLine : string;
begin
  SetLength(lsLine, 1000);
  for i := 1 to 1000 do
    lsLine[ i ]:= Chr(ord('A')+random(ord('z')-ord('A')));
  for i := 1 to 200 do
    result := result + lsLine + #13 + #10;
  result := result + 'x'; // make it an odd number
end;

end.


