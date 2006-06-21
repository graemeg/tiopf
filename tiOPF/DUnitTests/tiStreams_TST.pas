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

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiStreams_TST;

interface
uses
  TestFramework
  ;

type

  TTestTIStream = class( TTestCase )
  private
    procedure TestReadLn( const pStr, pLineDelim : string ; pCount : integer ) ;
    procedure TestEOF( pCount : integer ) ;
  protected
    procedure Setup ; override ;
    procedure TearDown ; override ;
  published
    procedure Props ;
    procedure ReadLn1;
    procedure ReadLn1WithLineEnd;
    procedure ReadLn100 ;
    procedure ReadLn1000 ;
    procedure ReadLn2000 ;
    procedure LineDelims ;
    procedure EOF ;
    procedure Write ;
    procedure WriteLn ;
  end;

procedure RegisterTests ;

implementation
uses
   tiStreams
//  ,tiDBConnectionSetupAbs_TST
  ,Classes
  ,SysUtils
  ,tiUtils
  ,tiDUnitDependencies
  ,Windows
  ;

var
  uTempFileName : string;

function TempFileName: string ;
var
  pcTemp : array[0..MAX_PATH] of char ;
begin
  if Length(uTempFileName) = 0 then begin
    GetTempPath( MAX_PATH, pcTemp );
    uTempFileName := tiAddTrailingSlash( string(pcTemp) )  + 'temp.txt';
    end ;
  result := uTempFileName;
end;


procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTIStream.Suite ) ;
end ;


{ TTestTIStream }

procedure TTestTIStream.Props;
var
  lData : TtiFileStream ;
begin
  lData := TtiFileStream.Create( TempFileName, fmCreate or fmShareDenyNone ) ;
  try
    CheckEquals( CrLf, lData.LineDelim, 'LineDelim' ) ;
    lData.LineDelim := Cr ;
    CheckEquals( Cr, lData.LineDelim, 'LineDelim' ) ;
  finally
    lData.Free ;
  end;
end;

procedure TTestTIStream.ReadLn100;
begin
  TestReadLn( 'test', CrLf, 99 ) ;
end;

procedure TTestTIStream.ReadLn1;
var
  ls : string ;
  lStream : TtiFileStream ;
begin
  tiStringToFile( 'test', TempFileName ) ;
  lStream := TtiFileStream.CreateReadOnly( TempFileName ) ;
  try
    ls := lStream.ReadLn ;
    CheckEquals( 'test', ls ) ;
  finally
    lStream.Free ;
  end;
end;

procedure TTestTIStream.Write;
var
  ls : string ;
  lStream : TtiFileStream ;
begin
  lStream := TtiFileStream.CreateReadWrite( TempFileName ) ;
  try
    lStream.Write('test') ;
  finally
    lStream.Free ;
  end;
  ls := tiFileToString( TempFileName ) ;
  CheckEquals( 'test', ls ) ;
end;

procedure TTestTIStream.WriteLn;
var
  ls : string ;
  lStream : TtiFileStream ;
begin
  lStream := TtiFileStream.CreateReadWrite( TempFileName ) ;
  try
    lStream.WriteLn('test') ;
  finally
    lStream.Free ;
  end;
  ls := tiFileToString( TempFileName ) ;
  CheckEquals( 'test'+CrLf, ls ) ;

  SysUtils.DeleteFile( TempFileName ) ;

  lStream := TtiFileStream.CreateReadWrite( TempFileName ) ;
  try
    lStream.LineDelim := Cr ;
    lStream.WriteLn('test') ;
  finally
    lStream.Free ;
  end;
  ls := tiFileToString( TempFileName ) ;
  CheckEquals( 'test'+Cr, ls ) ;

end;

procedure TTestTIStream.ReadLn1WithLineEnd;
begin
  TestReadLn( 'test', CrLf, 1 ) ;
end;

procedure TTestTIStream.ReadLn1000;
begin
  TestReadLn( 'test', CrLf, 1000 ) ;
end;

procedure TTestTIStream.TestReadLn(const pStr, pLineDelim: string; pCount: integer);
var
  ls : string ;
  lStream : TtiFileStream ;
  i : integer ;
begin
  ls := '' ;
  for i := 1 to pCount do
    ls := ls + tiPad0(IntToStr(i),2) + pStr + pLineDelim ;
  tiStringToFile( ls, TempFileName ) ;
  lStream := TtiFileStream.CreateReadOnly( TempFileName ) ;
  try
    lStream.LineDelim := pLineDelim ;
    for i := 1 to pCount do
    begin
      ls := lStream.ReadLn ;
      CheckEquals( tiPad0(IntToStr( i ),2) + pStr, ls, '#' + IntToStr(i)) ;
    end;
  finally
    lStream.Free ;
  end;
end;

procedure TTestTIStream.ReadLn2000;
begin
  TestReadLn( '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'+
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
              2000 ) ;
end;

procedure TTestTIStream.EOF;
begin
  TestEOF( 0 ) ;
  TestEOF( 1 ) ;
  TestEOF( 2 ) ;
  TestEOF( 10 ) ;
  TestEOF( 1000 ) ;
end;

procedure TTestTIStream.LineDelims;
begin
  TestReadLn( 'test', Cr, 10 ) ;
  TestReadLn( 'test', Lf, 10 ) ;
  TestReadLn( 'test', '|', 10 ) ;
  TestReadLn( 'test', '<p>', 10 ) ;
  TestReadLn( 'test', 'MyLineEnd', 10 ) ;
end;

procedure TTestTIStream.TestEOF(pCount: integer);
var
  ls : string ;
  lStream : TtiFileStream ;
  i, lCount : integer ;
begin
  ls := '' ;
  for i := 1 to pCount do
    ls := ls + 'test' + CrLf ;
  tiStringToFile( ls, TempFileName ) ;
  lStream := TtiFileStream.Create( TempFileName, fmOpenRead or fmShareDenyNone ) ;
  try
    lCount := 0 ;
    while not lStream.EOF do
    begin
      Inc(lCount);
      lStream.ReadLn;
    end ;
    CheckEquals( pCount, lCount ) ;
  finally
    lStream.Free ;
  end;
end;

procedure TTestTIStream.Setup;
begin
  SysUtils.DeleteFile(TempFileName);
end;

procedure TTestTIStream.TearDown;
begin
  SysUtils.DeleteFile(TempFileName);
end;

end.
