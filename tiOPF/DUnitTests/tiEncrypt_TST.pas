{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:
    June, 2002, Peter Hinrichsen, Created
  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiEncrypt_TST;

interface
uses
  TestFrameWork
  ,tiEncryptAbs
  ,tiEncryptSimple
  ,tiEncryptNone
  ,tiEncryptDES
  ,tiEncryptBlowfish
  ;

type

  TTestTIEncrypt = class( TTestCase )
  private
    function  GetTestString : string ;
  protected
    procedure Do_StringEncryption( const pEncryptionType : string; const pSeed : string = '' ; const pIntSeed : Int64 = 0 ) ;
    procedure Do_StreamEncryption( const pEncryptionType : string; const pSeed : string = '' ; const pIntSeed : Int64 = 0 ) ;
  published
    procedure None_StringEncryption;
    procedure None_StreamEncryption;
    procedure Simple_StringEncryption;
    procedure Simple_StreamEncryption;
    procedure DES_StreamEncryption;
    procedure DES_StringEncryption;
    procedure Blowfish_StreamEncryption;
    procedure Blowfish_StringEncryption;
  end ;


procedure RegisterTests ;

implementation
uses
  Classes
//  ,tiDBConnectionSetupAbs_TST
  ,tiDUnitDependencies
  ;

const
  uSeed = 'blah1$blah2#yada3&yada4';
  uIntSeed = 61431572978;

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTIEncrypt.Suite );
end ;


procedure TTestTIEncrypt.Do_StreamEncryption( const pEncryptionType : string; const pSeed : string ; const pIntSeed : Int64 ) ;
var
  lBefore    : TStringStream ;
  lEncrypted : TStringStream ;
  lAfter     : TStringStream ;
  lEncrypt   : TtiEncryptAbs ;
  lZero      : Byte;
  lsTest     : string ;
begin

  lBefore    := TStringStream.Create( '' ) ;
  lEncrypted := TStringStream.Create( '' ) ;
  lAfter     := TStringStream.Create( '' ) ;
  lZero      := 0;
  lsTest     := GetTestString;
  try
    lBefore.WriteString( lsTest ) ;  // some text
    lBefore.Write( lZero, 1 ) ; // Embed zero to simulate binary data
    lBefore.WriteString( lsTest ) ; // and some more text
    lEncrypt := gEncryptFactory.CreateInstance( pEncryptionType ) ;
    lEncrypt.Seed    := pSeed;
    lEncrypt.IntSeed := pIntSeed;
    try
      lEncrypt.EncryptStream( lBefore, lEncrypted ) ;
      lEncrypt.DecryptStream( lEncrypted, lAfter ) ;
    finally
      lEncrypt.Free ;
    end ;

  if pIntSeed <> 0 then
    Check( lBefore.DataString = lAfter.DataString, 'Seeded Encryption failed. Streams are not the same.' ) 
  else
    Check( lBefore.DataString = lAfter.DataString, 'Encryption failed. Streams are not the same.' ) ;

  finally
    lBefore.Free ;
    lEncrypted.Free ;
    lAfter.Free ;
  end;
end;

procedure TTestTIEncrypt.Do_StringEncryption( const pEncryptionType : string; const pSeed : string ; const pIntSeed : Int64 ) ;
var
  lsBefore  : string ;
  lsEncrypt : string ;
  lsAfter   : string ;
  lEncrypt  : TtiEncryptAbs ;
begin
  lsBefore := GetTestString ; // was FsTest
  lEncrypt := gEncryptFactory.CreateInstance( pEncryptionType ) ;
  lEncrypt.Seed    := pSeed;
  lEncrypt.IntSeed := pIntSeed;

  try
    lsEncrypt := lEncrypt.EncryptString( lsBefore ) ;
    lsAfter   := lEncrypt.DecryptString( lsEncrypt ) ;
  finally
    lEncrypt.Free ;
  end ;

  if pIntSeed <> 0 then
    Check( lsBefore = lsAfter, 'Seeded Encryption failed. Strings are not the same.' )
  else
    Check( lsBefore = lsAfter, 'Encryption failed. Strings are not the same.' ) ;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TEncryptTest
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//procedure TEncryptTest.Execute( const psTest : string ) ;
//var
//  i : integer ;
//begin
//  FsMessageText := '' ;
//  FsTest := psTest ;
//  for i := 1 to 5 do begin
//    FsMessageText := FsMessageText + 'Test #' + IntToStr( i ) + #13 ;
//    TestString ;
//    TestStream ;
//    TestFile   ;
//    FsMessageText := FsMessageText + #13 ;
//    FEncrypt.NewSeed ;
//  end ;
//end;


procedure TTestTIEncrypt.None_StreamEncryption;
begin
  Do_StreamEncryption( cgsEncryptionNone ) ;
  Do_StreamEncryption( cgsEncryptionNone, uSeed, uIntSeed ) ;
end;

procedure TTestTIEncrypt.None_StringEncryption;
begin
  Do_StringEncryption( cgsEncryptionNone ) ;
  Do_StringEncryption( cgsEncryptionNone, uSeed, uIntSeed ) ;
end;

procedure TTestTIEncrypt.Simple_StreamEncryption;
begin
  Do_StreamEncryption( cgsEncryptionSimple ) ;
  Do_StreamEncryption( cgsEncryptionSimple, uSeed, uIntSeed ) ;
end;

procedure TTestTIEncrypt.Simple_StringEncryption;
begin
  Do_StringEncryption( cgsEncryptionSimple ) ;
  Do_StringEncryption( cgsEncryptionSimple, uSeed, uIntSeed ) ;
end;

procedure TTestTIEncrypt.DES_StreamEncryption;
begin
  Do_StreamEncryption( cgsEncryptionDES ) ;
  Do_StreamEncryption( cgsEncryptionDES, uSeed, uIntSeed ) ;
end;

procedure TTestTIEncrypt.DES_StringEncryption;
begin
  Do_StringEncryption( cgsEncryptionDES ) ;
  Do_StringEncryption( cgsEncryptionDES, uSeed, uIntSeed ) ;
end;

procedure TTestTIEncrypt.Blowfish_StreamEncryption;
begin
  Do_StreamEncryption( cgsEncryptionBlowfish ) ;
  Do_StreamEncryption( cgsEncryptionBlowfish, uSeed, uIntSeed ) ;
end;

procedure TTestTIEncrypt.Blowfish_StringEncryption;
begin
  Do_StringEncryption( cgsEncryptionBlowfish ) ;
  Do_StringEncryption( cgsEncryptionBlowfish, uSeed, uIntSeed ) ;
end;

function TTestTIEncrypt.GetTestString: string;
var
  i      : integer ;
  lsLine : string ;
begin
  SetLength(lsLine, 1000) ;
  for i := 1 to 1000 do
    lsLine[ i ] := Chr( ord('A')+random(ord('z')-ord('A'))) ;
  for i := 1 to 200 do
    result := result + lsLine + #13 + #10 ;
  result := result + 'x' ; // make it an odd number
end;

end.

