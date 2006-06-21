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
unit tiCompress_TST;

interface
uses
  TestFrameWork
  ;

type

  TTestTICompress = class( TTestCase )
  private
    function  GetTestString : string ;
    procedure Do_FileCompression( const pCompressionType : string ) ;
    procedure Do_StringCompression( const pCompressionType : string ) ;
    procedure Do_StreamCompression( const pCompressionType : string ) ;
  published
    procedure None_FileCompression ;
    procedure None_StringCompression ;
    procedure None_StreamCompression ;

    procedure ZLib_FileCompression ;
    procedure ZLib_StringCompression ;
    procedure ZLib_StreamCompression ;

  end ;

procedure RegisterTests ;

implementation
uses
  tiCompressAbs
  ,tiCompressNone
  ,tiCompressZLib
  ,tiUtils
  ,Classes
  ,SysUtils
  ,tiLog
  ,tiDUnitDependencies
  ,cTIPersist
  ;

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTICompress.Suite );
end ;

{ TTestTICompress }

procedure TTestTICompress.Do_FileCompression( const pCompressionType : string ) ;
var
  lCompress : TtiCompressAbs ;
  lRatio   : real ;
  lBefore : string ;
  lAfter : string ;
  lFileNameBefore : string ;
  lFileNameAfter : string ;
begin
  lFileNameBefore := tiGetTempDir + '\CompressionTestBefore.txt' ;
  lFileNameAfter  := tiGetTempDir + '\CompressionTestAfter.txt' ;
  lBefore := GetTestString ;
  tiStringToFile( lBefore, lFileNameBefore ) ;
  lCompress := gCompressFactory.CreateInstance( pCompressionType ) ;
  try
    lRatio := lCompress.CompressFile(   lFileNameBefore,
                                         lFileNameAfter ) ;
    LogArray([ 'Compression ration for file', pCompressionType, lRatio ]);
    DeleteFile( lFileNameBefore ) ;
    lCompress.DecompressFile( lFileNameAfter,
                              lFileNameBefore ) ;
  finally
    lCompress.Free ;
  end ;

  lAfter := tiFileToString( lFileNameBefore ) ;

  Check( lAfter = lBefore,
         'Compression failed. Files are not the same.' ) ;
  DeleteFile(lFileNameBefore);
  DeleteFile(lFileNameAfter);

end;

procedure TTestTICompress.Do_StreamCompression( const pCompressionType : string ) ;
var
  lBefore : TStringStream ;
  lCompressed : TStringStream ;
  lAfter : TStringStream ;
  lCompress : TtiCompressAbs ;
  lRatio   : real ;
begin

  lBefore := TStringStream.Create( GetTestString ) ;
  lCompressed := TStringStream.Create( '' ) ;
  lAfter := TStringStream.Create( '' ) ;
  try
    lCompress := gCompressFactory.CreateInstance( pCompressionType ) ;
    try
      lRatio := lCompress.CompressStream( lBefore, lCompressed ) ;
      LogArray([ 'Compression ration for stream', pCompressionType, lRatio ]);
      lCompress.DecompressStream( lCompressed, lAfter ) ;
      Check( lBefore.DataString = lAfter.DataString,
             'Compression failed. Streams are not the same.' ) ;
    finally
      lCompress.Free ;
    end ;
  finally
    lBefore.Free ;
    lCompressed.Free ;
    lAfter.Free ;
  end ;
end;

procedure TTestTICompress.Do_StringCompression( const pCompressionType : string ) ;
var
  lCompress    : TtiCompressAbs ;
  lBefore     : string ;
  lCompressed   : string ;
  lAfter : string ;
  lRatio      : real ;
begin

  // Create the appropriate TtiCompress concrete
  lCompress := gCompressFactory.CreateInstance( pCompressionType ) ;
  try
    // Get some text to compress
    lBefore := GetTestString ;
    // Compress the text, returning the compression ratio
    lRatio := lCompress.CompressString( lBefore,
                                        lCompressed ) ;
    LogArray([ 'Compression ration for string', pCompressionType, lRatio ]);
    // Decompress the text
    lCompress.DecompressString( lCompressed,
                                lAfter ) ;
  finally
    lCompress.Free ;
  end ;

  Check( lBefore = lAfter,
         'Compression failed. Strings are not the same.' ) ;

end;

function TTestTICompress.GetTestString: string;
var
  i      : integer ;
  lsLine : string ;
begin
  lsLine := '' ;
  for i := 1 to 1000 do
    lsLine := lsLine + Chr( ord('A')+random(ord('z')-ord('A'))) ;
  for i := 1 to 200 do
    result := result + lsLine + #13 + #10 ;
end;

procedure TTestTICompress.None_FileCompression;
begin
  Do_FileCompression( cgsCompressNone ) ;
end;

procedure TTestTICompress.None_StreamCompression;
begin
  Do_StreamCompression( cgsCompressNone ) ;
end;

procedure TTestTICompress.None_StringCompression;
begin
  Do_StringCompression( cgsCompressNone ) ;
end;

procedure TTestTICompress.ZLib_FileCompression;
begin
  Do_FileCompression( cgsCompressZLib ) ;
end;

procedure TTestTICompress.ZLib_StreamCompression;
begin
  Do_StreamCompression( cgsCompressZLib ) ;
end;

procedure TTestTICompress.ZLib_StringCompression;
begin
  Do_StringCompression( cgsCompressZLib ) ;
end;

end.

