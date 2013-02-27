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

unit tiUtils_TST;

interface
uses
  Classes  // needed for TStringList
  ,TestFrameWork
  ;

type
  TTestTIUtils = class( TTestCase )
  private
    function  BuildLongString : string ;
    procedure CheckReadingFromNT( const pValue, pRegKey, pDescription : string ) ;
  protected
    // These methods exist in tiUtils, but have not been DUnit tested because
    // I can not work our a reliable way of performing the tests (or I don't
    // have currently have a use for the method so the test is not a priority!)

    // Difficult to test
    // procedure _tiShellExecute;
    // procedure tiRunEXEAndWait;
    // procedure tiShellExecute;
    // procedure tiGetEXEPath; Tricky to test because it must work for both an EXE and a DLL (ISAPI DLL)

    // Low priority
    // procedure tiBitToString; // Have not used this for a long time, so implement DUnit test when required again
    // procedure tiInt32ToBinString; // Have not used this for a long time, so implement DUnit test when required again
    // procedure tiIsBitSet; // Have not used this for a long time, so implement DUnit test when required again


  published
//    procedure tiDateToStr;
//    procedure tiDirectoryTreeToStringList;
//    procedure tiExtractDirToLevel;
//  protected
    procedure _tiFloatToStr;
    procedure Cr;
    procedure CrLf;
    procedure Lf;
    procedure tiAddEllipsis;
    procedure tiAddTrailingAnd;
    procedure tiAddTrailingComma;
    procedure tiAddTrailingOr;
    procedure tiAddTrailingSlash;
    procedure tiAddTrailingSpace;
    procedure tiAddTrailingValue;
    procedure tiBooleanToStr;
    procedure tiCheckSum;
    procedure tiCIStrTran;
    procedure tiCopyFile;
    procedure tiDateTimeToStr;
    procedure tiDateToPreviousWeekDayDate;
    procedure tiDateToStr;
    procedure tiDirectoryTreeToStringList;
    procedure tiEncodeDecodeWordBase26;
    procedure tiExtractDirToLevel;
    procedure tiExtractExtension;
    procedure tiExtractFileNameOnly;
    procedure TestCreateDir;     // Tests logic used in tiFileToStringList test
    procedure TestCreateFile;    // Tests logic used in tiFileToStringList test
    procedure tiFilesToStringList;
    procedure tiFileToString;
    procedure tiFloatToCommaStr;
    procedure tiFloatToCurrency;
    procedure tiFloatToCurrencyHide0;
    procedure tiFloatToStr;
    procedure tiForceRemoveDir ;
    procedure tiFmtException;
    procedure tiGetComputerName;
    procedure tiGetFileSize;
    procedure tiGetPropertyNames;
    procedure tiGetSimplePropType;
    procedure tiVarSimplePropType;
    procedure tiGetTempDir;
    procedure tiGetTempFile;
    procedure tiGetUserName;
    procedure tiGetWindowsSysDir;
    procedure tiHasSubDirectory;
    procedure tiIntToCommaStr;
    procedure tiIntToStrHide0;
    procedure tiIsFileReadOnly;
    procedure tiIsNumericProp;
    procedure tiIsReadWriteProp;
    procedure tiHasRTTIOnClass;
    procedure tiHasRTTIOnObject;
    procedure tiIsVariantOfType;
    procedure tiListToClipboard;
    procedure tiListToCSV;
    procedure tiListToStream;
    procedure tiStringToStream;
    procedure tiAppendStringToStream;
    procedure tiStreamToString;
//    procedure tiStreamToFile;
//    procedure tiFileToStream;
    procedure tiMixedCase;
    procedure tiMoveFile;
    procedure tiNumToken;
    procedure tiPad0;
    procedure tiPadC;
    procedure tiPadL;
    procedure tiPadR;
    procedure tiPosR;
    procedure tiReadFileDateSize;
    procedure tiRemoveCrLf;
    procedure tiRemoveDrive;
    procedure tiRemoveExtension;
    procedure tiRemoveLeading0;
    procedure tiRemoveLeadingSlash;
    procedure tiRemoveTrailingSlash;
    procedure tiRemoveTrailingValue;
    procedure tiReplicate;
    procedure tiSafeDiv;
    procedure tiSetFileDate;
    procedure tiSetFileReadOnly;
    procedure tiRound;
    procedure tiSetPrecision;
    procedure tiSpace;
    procedure tiStringToFile;
    procedure tiStrToBool;
    procedure tiStrToFloat;
    procedure tiStrToInt;
    procedure tiStrTran;
    procedure tiStrTran1;
    procedure tiSubStr;
    procedure tiSwapExt;
    procedure tiTimeToStr;
    procedure tiToken;
    procedure tiTrimL;
    procedure tiTrimR;
    procedure tiTrimTrailingWhiteSpace;
    procedure tiVariantArrayToString;
    procedure tiWildcardMatch;
    procedure tiYear;
    procedure tiIsEMailAddressValid ;
    procedure tiIsFileNameValid ;
  end ;

procedure RegisterTests ;

implementation
uses
  tiUtils
  ,tiLog
  ,SysUtils
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ELSE}
  ,FileCtrl
  {$ENDIF}
  ,tiDialogs
  ,TypInfo
  ,Contnrs
  {$IFDEF MSWINDOWS}
  ,ClipBrd
  ,Forms
  ,Windows
  ,tiWin32
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QClipBrd
  {$ENDIF LINUX}
  ,tiDUnitUtils
//  ,tiDBConnectionSetupAbs_TST
  ,tiDUnitINI
  ,tiDUnitDependencies
  ;

var
  uTempPath : string;

function TempFileName( const pFilename : string ): string ;
var
  pcTemp : array[0..MAX_PATH] of char ;
begin
  if Length(uTempPath) = 0 then begin
    GetTempPath( MAX_PATH, pcTemp );
    uTempPath := tiAddTrailingSlash( string(pcTemp) );
    end ;
  result := uTempPath + pFilename;
end;


type

  TTestGetPropNamesAbs = class( TPersistent )
  private
    FStringProp: string;
    FCharProp: Char;
    FShortStringStringProp: ShortString;
    FWideStringProp: WideString;
    FWideCharProp: WideChar;
    FInt64Prop: Int64;
    FIntProp: integer;
    FFloatProp: Real;
    FMethodProp: TNotifyEvent;
    FObjectProp: TObject;
    FDateTimeProp: TDateTime;
    FBoolProp: Boolean;
  protected
    property StringProp : string read FStringProp write FStringProp ;
    property ShortStringProp : ShortString read FShortStringStringProp write FShortStringStringProp ;
    property WideStringProp  : WideString Read FWideStringProp write FWideStringProp ;
    property CharProp : Char Read FCharProp write FCharProp ;
    property WideCharProp : WideChar Read FWideCharProp write FWideCharProp ;
    property IntProp : integer read FIntProp write FIntProp ;
    property Int64Prop : Int64 read FInt64Prop write FInt64Prop ;
    property BoolProp  : Boolean read FBoolProp write FBoolProp ;
    property DateTimeProp : TDateTime read FDateTimeProp write FDateTimeProp ;
    property FloatProp : Real read FFloatProp write FFloatProp ;

    property ObjectProp : TObject read FObjectProp write FObjectProp ;
    property MethodProp : TNotifyEvent read FMethodProp write FMethodProp ;
    // These are the leftovers
    // tkUnknown, tkEnumeration, tkSet, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray

    property ReadOnlyStringProp : string read FStringProp ;
    property ReadOnlyShortStringProp : ShortString read FShortStringStringProp ;
    property ReadOnlyWideStringProp  : WideString Read FWideStringProp ;
    property ReadOnlyCharProp : Char Read FCharProp ;
    property ReadOnlyWideCharProp : WideChar Read FWideCharProp ;
    property ReadOnlyIntProp : integer read FIntProp ;
    property ReadOnlyInt64Prop : Int64 read FInt64Prop ;
    property ReadOnlyBoolProp  : boolean read FBoolProp ;
    property ReadOnlyDateTimeProp : TDateTime read FDateTimeProp ;
    property ReadOnlyFloatProp : Real read FFloatProp ;
    property ReadOnlyObjectProp : TObject read FObjectProp ;
    property ReadOnlyMethodProp : TNotifyEvent read FMethodProp ;

  end ;

  TTestListToOutput = class( TTestGetPropNamesAbs )
  published
    property DateTimeProp ;
    property FloatProp ;
    property IntProp ;
    property StringProp ;
  end ;

  TTestListOfPersistents = class( TObjectList )
  public
    constructor create ;
    function AsString( pDelim : string = ',' ) : string ;
  end ;

function TTestListOfPersistents.AsString( pDelim : string = ',' ) : string;
var
  i : integer ;
  lLine : string ;
begin
  result :=                        
    'DateTimeProp' + pDelim +
    'FloatProp' + pDelim +
    'IntProp' + pDelim +
    'StringProp' ;
  for i := 0 to 9 do
  begin
    lLine := FormatDateTime( csWinDateTimeFormat, StrToDate( '01/01/2002' ) + i / 24 ) ;
    lLine := lLine + pDelim + FloatToStr( i + i / 10 ) ;
    lLine := lLine + pDelim + IntToStr( i ) ;
    lLine := lLine + pDelim + IntToStr( i ) ;
    if result <> '' then
      result := result + #13 + #10 ;
    result := result + lLine ;
  end ;
end;

constructor TTestListOfPersistents.create;
var
  lData : TTestListToOutput ;
  i : integer ;
begin
  inherited ;
  for i := 0 to 9 do
  begin
    lData := TTestListToOutput.Create ;
    lData.DateTimeProp := StrToDate( '01/01/2002' ) + i / 24 ;
    lData.FloatProp    := i + i /10 ;
    lData.StringProp   := IntToStr( i ) ;
    lData.IntProp      := i ;
    Add( lData ) ;
  end ;
end;

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTIUtils.Suite );
end ;

procedure TTestTIUtils.tiStrTran ;
begin
  CheckEquals( 'one two three', tiUtils.tiStrTran( 'one two three', 'ONE', 'a' ),     'Failed test 1' ) ;
  CheckEquals( 'a two three',   tiUtils.tiStrTran( 'one two three', 'one', 'a' ),     'Failed test 2' ) ;
  CheckEquals( 'one b three',   tiUtils.tiStrTran( 'one two three', 'two', 'b' ),     'Failed test 3' ) ;
  CheckEquals( 'one two c',     tiUtils.tiStrTran( 'one two three', 'three', 'c' ),   'Failed test 4' ) ;
  CheckEquals( 'd d two three', tiUtils.tiStrTran( 'one one two three', 'one', 'd' ), 'Failed test 5' ) ;
  CheckEquals( 'one e three e', tiUtils.tiStrTran( 'one two three two', 'two', 'e' ), 'Failed test 6' ) ;
end ;

procedure TTestTIUtils.tiStrTran1 ;
begin
  CheckEquals( 'one two three', tiUtils.tiStrTran1( 'one two three', 'ONE', 'a' ),     'Failed test 1' ) ;
  CheckEquals( 'a two three',   tiUtils.tiStrTran1( 'one two three', 'one', 'a' ),     'Failed test 2' ) ;
  CheckEquals( 'one b three',   tiUtils.tiStrTran1( 'one two three', 'two', 'b' ),     'Failed test 3' ) ;
  CheckEquals( 'one two c',     tiUtils.tiStrTran1( 'one two three', 'three', 'c' ),   'Failed test 4' ) ;
  CheckEquals( 'd d two three', tiUtils.tiStrTran1( 'one one two three', 'one', 'd' ), 'Failed test 5' ) ;
  CheckEquals( 'one e three e', tiUtils.tiStrTran1( 'one two three two', 'two', 'e' ), 'Failed test 6' ) ;
end ;

procedure TTestTIUtils.tiCIStrTran ;
begin
  Check( tiUtils.tiCIStrTran( 'one two three', 'ONE', 'a' ) = 'a two three', 'Failed test 1' ) ;
  Check( tiUtils.tiCIStrTran( 'one two three', 'TWO', 'b' ) = 'one b three', 'Failed test 2' ) ;
  Check( tiUtils.tiCIStrTran( 'one two three', 'THREE', 'c' ) = 'one two c', 'Failed test 3' ) ;
  Check( tiUtils.tiCIStrTran( 'one two three', 'one', 'a' ) = 'a two three', 'Failed test 4' ) ;
  Check( tiUtils.tiCIStrTran( 'one two three', 'two', 'b' ) = 'one b three', 'Failed test 5' ) ;
  Check( tiUtils.tiCIStrTran( 'one two three', 'three', 'c' ) = 'one two c', 'Failed test 6' ) ;
end ;

procedure TTestTIUtils.tiNumToken ;
begin
  CheckEquals( 0, tiUtils.tiNumToken( '', ',' ), 'Failed on 1' ) ;
  CheckEquals( 1, tiUtils.tiNumToken( 'adf adf', ',' ), 'Failed on 2' ) ;
  CheckEquals( 2, tiUtils.tiNumToken( 'adf,', ',' ), 'Failed on 3' ) ;
  CheckEquals( 2, tiUtils.tiNumToken( 'adf,adf', ',' ), 'Failed on 4' ) ;
  CheckEquals( 3, tiUtils.tiNumToken( 'adf,adf,adf', ',' ), 'Failed on 5' ) ;
end ;

procedure TTestTIUtils.tiToken ;
begin
  CheckEquals( '', tiUtils.tiToken( '', ',', 1 ), 'Failed on 1' ) ;
  CheckEquals( 'a', tiUtils.tiToken( 'a,b,c', ',', 1 ), 'Failed on 2' ) ;
  CheckEquals( 'b', tiUtils.tiToken( 'a,b,c', ',', 2 ), 'Failed on 3' ) ;
  CheckEquals( 'c', tiUtils.tiToken( 'a,b,c', ',', 3 ), 'Failed on 4' ) ;
  CheckEquals( '', tiUtils.tiToken( 'a,b,c', ',', 4 ), 'Failed on 5' ) ;
  CheckEquals( 'aa', tiUtils.tiToken( 'aa,bb,cc', ',', 1 ), 'Failed on 6' ) ;
  CheckEquals( 'bb', tiUtils.tiToken( 'aa,bb,cc', ',', 2 ), 'Failed on 7' ) ;
  CheckEquals( 'cc', tiUtils.tiToken( 'aa,bb,cc', ',', 3 ), 'Failed on 8' ) ;
  CheckEquals( '', tiUtils.tiToken( 'aa,bb,cc', ',', 4 ), 'Failed on 9' ) ;
end ;

procedure TTestTIUtils.tiSpace ;
begin
  CheckEquals( '', tiUtils.tiSpace(  0 ), 'Failed on  0' ) ;
  CheckEquals( ' ', tiUtils.tiSpace(  1 ), 'Failed on  1' ) ;
  CheckEquals( '  ', tiUtils.tiSpace(  2 ), 'Failed on  2' ) ;
  CheckEquals( '     ', tiUtils.tiSpace(  5 ), 'Failed on  5' ) ;
  CheckEquals( '          ', tiUtils.tiSpace( 10 ), 'Failed on 10' ) ;
end ;

procedure TTestTIUtils.tiPadR ;
begin
  CheckEquals( 'x', tiUtils.tiPadR( 'x', 1 ), 'Failed on 1' ) ;
  CheckEquals( 'x ', tiUtils.tiPadR( 'x', 2 ), 'Failed on 2' ) ;
  CheckEquals( 'x  ', tiUtils.tiPadR( 'x', 3 ), 'Failed on 3' ) ;
  CheckEquals( 'abc', tiUtils.tiPadR( 'abc', 3 ), 'Failed on 4' ) ;
  CheckEquals( 'ab', tiUtils.tiPadR( 'abc', 2 ), 'Failed on 5' ) ;
  CheckEquals( 'a', tiUtils.tiPadR( 'abc', 1 ), 'Failed on 6' ) ;
end ;

procedure TTestTIUtils.tiPadL ;
begin
  CheckEquals( 'x', tiUtils.tiPadL( 'x', 1 ), 'Failed on 1' ) ;
  CheckEquals( ' x', tiUtils.tiPadL( 'x', 2 ), 'Failed on 2' ) ;
  CheckEquals( '  x', tiUtils.tiPadL( 'x', 3 ), 'Failed on 3' ) ;
  CheckEquals( 'abc', tiUtils.tiPadL( 'abc', 3 ), 'Failed on 4' ) ;
  CheckEquals( 'bc', tiUtils.tiPadL( 'abc', 2 ), 'Failed on 5' ) ;
  CheckEquals( 'c', tiUtils.tiPadL( 'abc', 1 ), 'Failed on 6' ) ;
end ;

procedure TTestTIUtils.tiPadC ;
begin
  CheckEquals( 'x', tiUtils.tiPadC( 'x', 1 ), 'Failed on 1' ) ;
  CheckEquals( 'x ', tiUtils.tiPadC( 'x', 2 ), 'Failed on 2' ) ;
  CheckEquals( ' x ', tiUtils.tiPadC( 'x', 3 ), 'Failed on 3' ) ;
  CheckEquals( ' x  ', tiUtils.tiPadC( 'x', 4 ), 'Failed on 4' ) ;
  CheckEquals( '  x  ', tiUtils.tiPadC( 'x', 5 ), 'Failed on 5' ) ;
  CheckEquals( 'xx', tiUtils.tiPadC( 'xx', 2 ), 'Failed on 6' ) ;
  CheckEquals( 'xx ', tiUtils.tiPadC( 'xx', 3 ), 'Failed on 7' ) ;
  CheckEquals( ' xx ', tiUtils.tiPadC( 'xx', 4 ), 'Failed on 8' ) ;

  try
    tiUtils.tiPadC( 'xx', 1 ) ;
    Check( false, 'Exception was not raised' ) ;
  except
    // Do nothing
  end ;
end ;

procedure TTestTIUtils.tiPad0 ;
begin
  CheckEquals( '1', tiUtils.tiPad0( '1', 1 ), 'Failed on 1' ) ;
  CheckEquals( '01', tiUtils.tiPad0( '1', 2 ), 'Failed on 2' ) ;
  CheckEquals( '001', tiUtils.tiPad0( '1', 3 ), 'Failed on 3' ) ;
  CheckEquals( '000', tiUtils.tiPad0( '', 3 ), 'Failed on 4' ) ;
  // Perhaps not what you would expect, but anyway...
  CheckEquals( '123',   tiUtils.tiPad0( '1234', 3 ), 'Failed on 1' ) ;
end ;

procedure TTestTIUtils.tiRemoveLeading0 ;
begin
  CheckEquals( '', tiUtils.tiRemoveLeading0( '0' ), 'Failed on 1' ) ;
  CheckEquals( '', tiUtils.tiRemoveLeading0( '00' ), 'Failed on 2' ) ;
  CheckEquals( '123', tiUtils.tiRemoveLeading0( '123' ), 'Failed on 3' ) ;
  CheckEquals( '123', tiUtils.tiRemoveLeading0( '0123' ), 'Failed on 4' ) ;
  CheckEquals( '123', tiUtils.tiRemoveLeading0( '00123' ), 'Failed on 5' ) ;
  CheckEquals( 'a0123', tiUtils.tiRemoveLeading0( 'a0123' ), 'Failed on 6' ) ;
end ;


procedure TTestTIUtils.tiMixedCase ;
begin
  CheckEquals( 'A B C D', tiUtils.tiMixedCase( 'a B c D' ), 'Failed on ''a B c D''' ) ;
  CheckEquals( 'Abc', tiUtils.tiMixedCase( 'abc' ), 'Failed on ''abc''' ) ;
  CheckEquals( 'Abc', tiUtils.tiMixedCase( 'ABC' ), 'Failed on ''ABC''' ) ;
  // Not what we would really want. Use tiStringUtils instead
  CheckEquals( 'A.b.c.', tiUtils.tiMixedCase( 'a.b.c.' ), 'Failed on ''a.b..''' ) ;
  // Not what we would really want. Use tiStringUtils instead
  CheckEquals( 'King Charles Iii', tiUtils.tiMixedCase( 'king charles iii' ), 'Failed on ''king charles iii''' ) ;
end ;

procedure TTestTIUtils.tiReplicate ;
begin
  CheckEquals( 'x', tiUtils.tiReplicate( 'x', 1 ), 'Failed on 1' ) ;
  CheckEquals( 'xx', tiUtils.tiReplicate( 'x', 2 ), 'Failed on 2' ) ;
  CheckEquals( 'xxxxxxxxxx', tiUtils.tiReplicate( 'x', 10 ), 'Failed on 3' ) ;
  CheckEquals( '12', tiUtils.tiReplicate( '12', 1 ), 'Failed on 4' ) ;
  CheckEquals( '1212', tiUtils.tiReplicate( '12', 2 ), 'Failed on 5' ) ;
  CheckEquals( '121212', tiUtils.tiReplicate( '12', 3 ), 'Failed on 6' ) ;
  CheckEquals( '12121212', tiUtils.tiReplicate( '12', 4 ), 'Failed on 7' ) ;
end ;

procedure TTestTIUtils.tiAddTrailingValue ;
begin
  Check( tiUtils.tiAddTrailingValue( '', 'x', true )     = '',      'Failed on <empty string>' ) ;
  Check( tiUtils.tiAddTrailingValue( 'a', 'x', true )    = 'ax',    'Failed on <a>' ) ;
  Check( tiUtils.tiAddTrailingValue( 'axb', 'x', true )  = 'axbx',  'Failed on <axb>' ) ;
  Check( tiUtils.tiAddTrailingValue( 'axbx', 'x', true ) = 'axbxx', 'Failed on <axbx>' ) ;
  Check( tiUtils.tiAddTrailingValue( '', 'x', false )     = '',      'Failed on <empty string>' ) ;
  Check( tiUtils.tiAddTrailingValue( 'a', 'x', false )    = 'ax',    'Failed on <a>' ) ;
  Check( tiUtils.tiAddTrailingValue( 'axb', 'x', false )  = 'axbx',  'Failed on <axb>' ) ;
  Check( tiUtils.tiAddTrailingValue( 'axbx', 'x', false ) = 'axbx',  'Failed on <axbx>' ) ;
end ;

procedure TTestTIUtils.tiRemoveTrailingValue ;
begin
  CheckEquals( '', tiUtils.tiRemoveTrailingValue( 'x', 'x' ), 'Failed on 1' ) ;
  CheckEquals( '\abc', tiUtils.tiRemoveTrailingValue( '\abcx', 'x'  ), 'Failed on 2' ) ;
  CheckEquals( '\abc', tiUtils.tiRemoveTrailingValue( '\abc', 'x'  ), 'Failed on 3' ) ;
  CheckEquals( '\abc\def', tiUtils.tiRemoveTrailingValue( '\abc\defx', 'x'  ), 'Failed on 4' ) ;
  CheckEquals( '\abc\def', tiUtils.tiRemoveTrailingValue( '\abc\defx', 'x'  ), 'Failed on 5' ) ;
  CheckEquals( 'c:\abc\def', tiUtils.tiRemoveTrailingValue( 'c:\abc\defx', 'x'  ), 'Failed on 6' ) ;

  CheckEquals( 'x', tiUtils.tiRemoveTrailingValue( 'x', 'xx' ), 'Failed on 7' ) ;
  CheckEquals( '', tiUtils.tiRemoveTrailingValue( 'xx', 'xx' ), 'Failed on 8' ) ;
  CheckEquals( '\abc', tiUtils.tiRemoveTrailingValue( '\abcxx', 'xx'  ), 'Failed on 9' ) ;
  CheckEquals( '\abc', tiUtils.tiRemoveTrailingValue( '\abc', 'xx'  ), 'Failed on 10' ) ;
  CheckEquals( '\abc\def', tiUtils.tiRemoveTrailingValue( '\abc\defxx', 'xx'  ), 'Failed on 11' ) ;
  CheckEquals( '\abc\def', tiUtils.tiRemoveTrailingValue( '\abc\defxx', 'xx'  ), 'Failed on 12' ) ;
  CheckEquals( 'c:\abc\def', tiUtils.tiRemoveTrailingValue( 'c:\abc\defxx', 'xx'  ), 'Failed on 13' ) ;
end ;

procedure TTestTIUtils.tiAddTrailingComma ;
begin
  Check( tiUtils.tiAddTrailingComma( '' )     = '',      'Failed on <empty string>' ) ;
  Check( tiUtils.tiAddTrailingComma( 'a' )    = 'a,',    'Failed on <a>' ) ;
  Check( tiUtils.tiAddTrailingComma( 'a,b' )  = 'a,b,',  'Failed on <a,b>' ) ;
  Check( tiUtils.tiAddTrailingComma( 'a,b,' ) = 'a,b,,', 'Failed on <a,b,>' ) ;
end ;

procedure TTestTIUtils.tiAddTrailingAnd ;
begin
  Check( tiUtils.tiAddTrailingAnd( '' )         = '',         'Failed on <empty string>' ) ;
  Check( tiUtils.tiAddTrailingAnd( 'a=b' )      = 'a=b and ', 'Failed on <a=b>' ) ;
  Check( tiUtils.tiAddTrailingAnd( 'a=b and ' ) = 'a=b and ', 'Failed on <a=b and >' ) ;
end ;

procedure TTestTIUtils.tiAddTrailingOr ;
begin
  Check( tiUtils.tiAddTrailingOr( '' )        = '',        'Failed on <empty string>' ) ;
  Check( tiUtils.tiAddTrailingOr( 'a=b' )     = 'a=b or ', 'Failed on <a=b>' ) ;
  Check( tiUtils.tiAddTrailingOr( 'a=b or ' ) = 'a=b or ', 'Failed on <a=b and >' ) ;
end ;

procedure TTestTIUtils.tiAddTrailingSpace ;
begin
  Check( tiUtils.tiAddTrailingSpace( '' )     = '',      'Failed on <empty string>' ) ;
  Check( tiUtils.tiAddTrailingSpace( 'a' )    = 'a ',    'Failed on <a>' ) ;
  Check( tiUtils.tiAddTrailingSpace( 'a b' )  = 'a b ',  'Failed on <a b>' ) ;
  Check( tiUtils.tiAddTrailingSpace( 'a b ' ) = 'a b  ', 'Failed on <a b >' ) ;
end ;

  // Return the first position of pStrValue in pStrTarget from the right.
procedure TTestTIUtils.tiPosR ;
begin
  CheckEquals( 1, tiUtils.tiPosR( 'a',   'axxxxx' ), 'Failed on 1' ) ;
  CheckEquals( 2, tiUtils.tiPosR( 'a',   'xaxxxx' ), 'Failed on 2' ) ;
  CheckEquals( 6, tiUtils.tiPosR( 'a',   'xxxxxa' ), 'Failed on 3' ) ;
  CheckEquals( 1, tiUtils.tiPosR( 'abc', 'abcxxxxxx' ), 'Failed on 4' ) ;
  CheckEquals( 2, tiUtils.tiPosR( 'abc', 'xabcxxxxx' ), 'Failed on 5' ) ;
  CheckEquals( 4, tiUtils.tiPosR( 'abc', 'xxxabcxxx' ), 'Failed on 6' ) ;
  CheckEquals( 7, tiUtils.tiPosR( 'abc', 'xxxxxxabc' ), 'Failed on 7' ) ;
end ;

procedure TTestTIUtils.tiWildcardMatch ;
begin
  Check( tiUtils.tiWildCardMatch( 'c:\temp.txt', '*.txt' ), 'Failed on 1' ) ;
  Check( tiUtils.tiWildCardMatch( 'temp.txt', '*.txt' ), 'Failed on 2' ) ;
  Check( tiUtils.tiWildCardMatch( 'C:\Program files\System32\MyAp.exe', '*.exe' ), 'Failed on 3' ) ;

  Check( not tiUtils.tiWildCardMatch( 'c:\temp.txt', '*.TXT', true ), 'Failed on 4' ) ;
  Check( not tiUtils.tiWildCardMatch( 'temp.txt', '*.TXT', true ), 'Failed on 5' ) ;
  Check( not tiUtils.tiWildCardMatch( 'C:\Program files\System32\MyAp.exe', '*.EXE', true ), 'Failed on 6' ) ;

  Check(     tiUtils.tiWildCardMatch( 'abcdefg', 'abc*' ),    'Failed on 7' ) ;
  Check(     tiUtils.tiWildCardMatch( 'abcdefg', '*abc*' ),   'Failed on 8' ) ;
  Check( not tiUtils.tiWildCardMatch( 'abcdefg', '?bcd' ),    'Failed on 9' ) ;
  Check(     tiUtils.tiWildCardMatch( 'abcdefg', '?bcd*' ),   'Failed on 9' ) ;
  Check(     tiUtils.tiWildCardMatch( 'abcdefg', 'abc??fg' ), 'Failed on 10' ) ;

end ;

procedure TTestTIUtils.tiSubStr ;
begin
  CheckEquals( '', tiUtils.tiSubStr( '','','' ),                     'Failed on 1' ) ;
  CheckEquals( 'abc', tiUtils.tiSubStr( 'xxxabcyyy','xxx','yyy' ),   'Failed on 2' ) ;
  CheckEquals( 'abc', tiUtils.tiSubStr( 'xxx,abc;xxx',',',';' ),     'Failed on 3' ) ;
  CheckEquals( 'abc', tiUtils.tiSubStr( '<d>abc</d>','<d>','</d>' ), 'Failed on 4' ) ;
end ;

procedure TTestTIUtils.tiAddEllipsis ;
begin
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX', 13 ) = 'XXXXXXXXXX', 'Failed on 13' ) ;
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX', 12 ) = 'XXXXXXXXXX', 'Failed on 12' ) ;
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX', 11 ) = 'XXXXXXXXXX', 'Failed on 11' ) ;
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX', 10 ) = 'XXXXXXXXXX', 'Failed on 10' ) ;
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX',  9 ) = 'XXXXXX...',  'Failed on 9'  ) ;
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX',  8 ) = 'XXXXX...',   'Failed on 8'  ) ;
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX',  7 ) = 'XXXX...',    'Failed on 7'  ) ;
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX',  6 ) = 'XXX...',     'Failed on 6'  ) ;
  Check( tiUtils.tiAddEllipsis( 'XXXXXXXXXX',  5 ) = 'XX...',      'Failed on 5'  ) ;
end ;

procedure TTestTIUtils.tiTrimR ;
begin
  CheckEquals( '', tiUtils.tiTrimR( 'abc', 'abc', true  ), 'Failed on 1' ) ;
  CheckEquals( '', tiUtils.tiTrimR( 'abc', 'aBc', false ), 'Failed on 2' ) ;
  CheckEquals( '', tiUtils.tiTrimR( 'abcdef', 'abc', true ), 'Failed on 3' ) ;
  CheckEquals( '', tiUtils.tiTrimR( 'abcdef', 'aBc', false ), 'Failed on 4' ) ;
  CheckEquals( 'abc', tiUtils.tiTrimR( 'abcdefGhI', 'def', true ), 'Failed on 5' ) ;
  CheckEquals( 'abc', tiUtils.tiTrimR( 'abcDeFGhI', 'dEf', false ), 'Failed on 6' ) ;
end ;

procedure TTestTIUtils.tiTrimL ;
begin
  CheckEquals( '', tiUtils.tiTrimL( 'abc', 'abc', true  ), 'Failed on 1' ) ;
  CheckEquals( '', tiUtils.tiTrimL( 'abc', 'aBc', false ), 'Failed on 2' ) ;
  CheckEquals( '', tiUtils.tiTrimL( 'abcdef', 'def', true ), 'Failed on 3' ) ;
  CheckEquals( '', tiUtils.tiTrimL( 'abcdef', 'dEf', false ), 'Failed on 4' ) ;
  CheckEquals( 'GhI', tiUtils.tiTrimL( 'abcdefGhI', 'def', true ), 'Failed on 5' ) ;
  CheckEquals( 'GhI', tiUtils.tiTrimL( 'abcDeFGhI', 'dEf', false ), 'Failed on 6' ) ;
end ;

procedure TTestTIUtils.tiRemoveCrLf ;
const
  Cr = #13 ;
  Lf = #10 ;
begin
  CheckEquals( ' ', tiUtils.tiRemoveCrLf( Cr + Lf ), 'Failed on 1' ) ;
  CheckEquals( 'abc ', tiUtils.tiRemoveCrLf( 'abc' + Cr + Lf ), 'Failed on 2' ) ;
  CheckEquals( 'abc  ', tiUtils.tiRemoveCrLf( 'abc' + Cr + Lf + Cr + Lf ), 'Failed on 3' ) ;
  CheckEquals( 'abc   ', tiUtils.tiRemoveCrLf( 'abc' + Cr + Lf + Cr + Lf + Cr + Lf ), 'Failed on 4' ) ;
  CheckEquals( 'abc   def', tiUtils.tiRemoveCrLf( 'abc' + Cr + Lf + Cr + Lf + Cr + Lf + 'def' ), 'Failed on 5' ) ;
  CheckEquals( ' ', tiUtils.tiRemoveCrLf( Cr ), 'Failed on 6' ) ;
  CheckEquals( '  ', tiUtils.tiRemoveCrLf( Cr + Cr ), 'Failed on 7' ) ;
  CheckEquals( 'abc  ', tiUtils.tiRemoveCrLf( 'abc' + Cr + Cr ), 'Failed on 8' ) ;
end ;

  // Remove all the trailing white space characters ( #32, #10, #13 )
procedure TTestTIUtils.tiTrimTrailingWhiteSpace ;
const
  Cr = #13 ;
  Lf = #10 ;
begin
  CheckEquals( '', tiUtils.tiTrimTrailingWhiteSpace( '' ), 'Failed on 1' ) ;
  CheckEquals( 'abc', tiUtils.tiTrimTrailingWhiteSpace( 'abc' ), 'Failed on 2' ) ;
  CheckEquals( 'abc', tiUtils.tiTrimTrailingWhiteSpace( 'abc ' ), 'Failed on 3' ) ;
  CheckEquals( 'abc', tiUtils.tiTrimTrailingWhiteSpace( 'abc ' + Cr ), 'Failed on 4' ) ;
  CheckEquals( 'abc', tiUtils.tiTrimTrailingWhiteSpace( 'abc ' + Cr + Lf ), 'Failed on 5' ) ;
  CheckEquals( 'abc', tiUtils.tiTrimTrailingWhiteSpace( 'abc ' + Cr + Lf + Cr + Lf ), 'Failed on 6' ) ;
  CheckEquals( 'abc' + Cr + Lf + 'def', tiUtils.tiTrimTrailingWhiteSpace( 'abc' + Cr + Lf + 'def' ), 'Failed on 7' ) ;
end ;

procedure TTestTIUtils.tiGetTempFile ;
var
  i : integer ;
  lFileName : string ;
  lsl : TStringList ;
const
  cFileCount = 100 ;
begin
  lsl := TStringList.Create ;
  try
    lsl.Duplicates := dupError	 ;
    for i := 1 to cFileCount do
    begin
      lFileName := tiUtils.tiGetTempFile( 'tmp' ) ;
      lsl.Add( lFileName ) ;
      Check( not FileExists( lFileName ), 'File already exists' ) ;
      tiUtils.tiStringToFile( 'test', lFileName ) ;
    end ;
    CheckEquals( cFileCount, lsl.Count, 'Duplicate file names detected' ) ;
    for i := 0 to cFileCount - 1 do
      SysUtils.DeleteFile( lsl.Strings[i] ) ;
  finally
    lsl.Free ;
  end ;
end ;

procedure TTestTIUtils.tiAddTrailingSlash ;
begin
  Check( tiUtils.tiAddTrailingSlash( '' )     = '',     'Failed on <empty string>' ) ;
  Check( tiUtils.tiAddTrailingSlash( 'a' )    = 'a\',   'Failed on <a>' ) ;
  Check( tiUtils.tiAddTrailingSlash( 'a\b' )  = 'a\b\', 'Failed on <a\b>' ) ;
  Check( tiUtils.tiAddTrailingSlash( 'a\b\' ) = 'a\b\', 'Failed on <a\b\>' ) ;
end ;

procedure TTestTIUtils.tiRemoveTrailingSlash ;
begin
  CheckEquals( '', tiUtils.tiRemoveTrailingSlash( '\' ), 'Failed on 1' ) ;
  CheckEquals( '\abc', tiUtils.tiRemoveTrailingSlash( '\abc\' ), 'Failed on 2' ) ;
  CheckEquals( '\abc', tiUtils.tiRemoveTrailingSlash( '\abc' ), 'Failed on 3' ) ;
  CheckEquals( '\abc\def', tiUtils.tiRemoveTrailingSlash( '\abc\def\' ), 'Failed on 4' ) ;
  CheckEquals( '\abc\def', tiUtils.tiRemoveTrailingSlash( '\abc\def\' ), 'Failed on 2' ) ;
  CheckEquals( 'c:\abc\def', tiUtils.tiRemoveTrailingSlash( 'c:\abc\def\' ), 'Failed on 2' ) ;
end ;

procedure TTestTIUtils.tiRemoveLeadingSlash ;
begin
  CheckEquals( '', tiUtils.tiRemoveLeadingSlash( '\' ), 'Failed on 1' ) ;
  CheckEquals( 'abc', tiUtils.tiRemoveLeadingSlash( '\abc' ), 'Failed on 2' ) ;
  CheckEquals( 'abc', tiUtils.tiRemoveLeadingSlash( 'abc' ), 'Failed on 3' ) ;
  CheckEquals( 'abc\def', tiUtils.tiRemoveLeadingSlash( '\abc\def' ), 'Failed on 4' ) ;
  CheckEquals( 'abc\def\', tiUtils.tiRemoveLeadingSlash( '\abc\def\' ), 'Failed on 5' ) ;
end ;

procedure TTestTIUtils.tiGetTempDir    ;
begin
  CheckReadingFromNT(
    tiUtils.tiRemoveTrailingSlash( tiUtils.tiGetTempDir ),
    'TempDir',
    'TEMP directory (No trailing \)'
   ) ;
end ;

procedure TTestTIUtils.tiGetWindowsSysDir  ;
begin
  CheckReadingFromNT(
    tiUtils.tiGetWindowsSysDir,
    'WindowsSysDir',
    'Windows System Directory'
   ) ;
end ;

procedure TTestTIUtils.tiReadFileDateSize ;
  procedure _SetFileDate( pFileName : string ; pDate : TDateTime ) ;
  var
    lFileHandle : Integer ;
    lFileDate   : integer ;
  begin
    lFileDate   := DateTimeToFileDate( pDate ) ;
    lFileHandle := FileOpen( pFileName, fmOpenWrite or fmShareDenyNone);
    try
      FileSetDate( lFileHandle, lFileDate ) ;
    finally
      FileClose( lFileHandle ) ;
    end ;
  end ;

var
  lTargetDate : TDateTime ;
  lReadDate   : TDateTime ;
  lReadSize   : integer ;
  lFileName   : string ;

begin
  lFileName := TempFileName( 'DUnitTest.txt' );
  tiCreateTextFileOfSize( lFileName, 100 ) ;
  lTargetDate := EncodeDate( 1980, 1, 1 ) ;
  _SetFileDate( lFileName, lTargetDate ) ;
  tiUtils.tiReadFileDateSize( lFileName, lReadDate, lReadSize ) ;
  CheckEquals( lTargetDate, lReadDate, 'Failed on Date 1' ) ;
  CheckEquals( 100, lReadSize, 'Failed on Size 1' ) ;
  SysUtils.DeleteFile(lFileName);

  tiCreateTextFileOfSize( lFileName, 100 ) ;
  lTargetDate := EncodeDate( 2099, 12, 31 ) ;
  _SetFileDate( lFileName, lTargetDate ) ;
  tiUtils.tiReadFileDateSize( lFileName, lReadDate, lReadSize ) ;
  CheckEquals( lTargetDate, lReadDate, 'Failed on Date 2' ) ;
  CheckEquals( 100, lReadSize, 'Failed on Size 2' ) ;
  SysUtils.DeleteFile(lFileName);

end ;

procedure TTestTIUtils.tiSetFileDate ;
var
  lsl : TStringList ;
  lDate : TDateTime ;
  lFileName   : string ;

begin
  lFileName := TempFileName( 'DUnitTest.txt' );
  lsl := TStringList.Create ;
  try
    lsl.Text := BuildLongString ;
    lsl.SaveToFile( lFileName ) ;

    lDate := EncodeDate( 1980, 1, 1 ) ;
    tiUtils.tiSetFileDate( lFileName, lDate ) ;
    CheckEquals( lDate, FileDateToDateTime( FileAge( lFileName )), cdtOneSecond, 'Failed on 1' ) ;

    lDate := EncodeDate( 1980, 1, 1 ) ;
    tiUtils.tiSetFileDate( lFileName, lDate ) ;
    CheckEquals( lDate, FileDateToDateTime( FileAge( lFileName )), cdtOneSecond, 'Failed on 2' ) ;

    lDate := EncodeDate( 2090, 12, 31 ) ;
    tiUtils.tiSetFileDate( lFileName, lDate ) ;
    CheckEquals( lDate, FileDateToDateTime( FileAge( lFileName )), cdtOneSecond, 'Failed on 3' ) ;

    lDate := EncodeDate( 2002, 1, 1 ) + EncodeTime( 1, 0, 0, 0 ) ;
    tiUtils.tiSetFileDate( lFileName, lDate ) ;
    CheckEquals( lDate, FileDateToDateTime( FileAge( lFileName )), cdtOneSecond, 'Failed on 4' ) ;

    lDate := EncodeDate( 2002, 1, 1 ) + EncodeTime( 12, 0, 0, 0 ) ;
    tiUtils.tiSetFileDate( lFileName, lDate ) ;
    CheckEquals( lDate, FileDateToDateTime( FileAge( lFileName )), cdtOneSecond, 'Failed on 5' ) ;

    lDate := EncodeDate( 2002, 1, 1 ) + EncodeTime( 23, 59, 59, 0 ) ;
    tiUtils.tiSetFileDate( lFileName, lDate ) ;
    CheckEquals( lDate, FileDateToDateTime( FileAge( lFileName )), cdtOneSecond, 'Failed on 6' ) ;

    lDate := EncodeDate( 2002, 1, 1 ) + EncodeTime( 06, 06, 06, 0 );
    tiUtils.tiSetFileDate( lFileName, lDate ) ;
    CheckEquals( lDate, FileDateToDateTime( FileAge( lFileName )), cdtOneSecond, 'Failed on 7' ) ;
    SysUtils.DeleteFile(lFileName);

  finally
    lsl.Free;
  end ;
end ;

procedure TTestTIUtils.tiExtractFileNameOnly ;
begin
  Check( tiUtils.tiExtractFileNameOnly( 'test.txt' ) = 'test', 'Failed on 1' ) ;
  Check( tiUtils.tiExtractFileNameOnly( 'c:\temp\test.txt' ) = 'test', 'Failed on 2' ) ;
  Check( tiUtils.tiExtractFileNameOnly( 'c:\temp\test.' ) = 'test', 'Failed on 3' ) ;
  Check( tiUtils.tiExtractFileNameOnly( 'c:\temp\test' ) = 'test', 'Failed on 4' ) ;
  Check( tiUtils.tiExtractFileNameOnly( '\temp\test.txt' ) = 'test', 'Failed on 5' ) ;
  Check( tiUtils.tiExtractFileNameOnly( '\test.txt' ) = 'test', 'Failed on 6' ) ;
  Check( tiUtils.tiExtractFileNameOnly( '..\test.txt' ) = 'test', 'Failed on 2' ) ;
end ;

procedure TTestTIUtils.tiRemoveExtension ;
begin
  Check( tiUtils.tiRemoveExtension( 'test.txt' ) = 'test', 'Failed on 1' ) ;
  Check( tiUtils.tiRemoveExtension( 'c:\temp\test.txt' ) = 'c:\temp\test', 'Failed on 2' ) ;
  Check( tiUtils.tiRemoveExtension( 'c:\temp\test.' ) = 'c:\temp\test', 'Failed on 3' ) ;
  Check( tiUtils.tiRemoveExtension( 'c:\temp\test' ) = 'c:\temp\test', 'Failed on 4' ) ;
  Check( tiUtils.tiRemoveExtension( '\temp\test.txt' ) = '\temp\test', 'Failed on 5' ) ;
  Check( tiUtils.tiRemoveExtension( '\test.txt' ) = '\test', 'Failed on 6' ) ;
  Check( tiUtils.tiRemoveExtension( '..\test.txt' ) = '..\test', 'Failed on 2' ) ;
end ;

procedure TTestTIUtils.tiSwapExt ;
begin
  CheckEquals( 'test.txt',         tiUtils.tiSwapExt( 'test.txt', 'txt' ), 'Failed on 1' ) ;
  CheckEquals( 'test.hos',         tiUtils.tiSwapExt( 'test.txt', 'hos' ), 'Failed on 2' ) ;
  CheckEquals( 'c:\temp\test.txt', tiUtils.tiSwapExt( 'c:\temp\test.txt', 'txt' ), 'Failed on 3' ) ;
  CheckEquals( 'c:\temp\test.hos', tiUtils.tiSwapExt( 'c:\temp\test.txt', 'hos' ), 'Failed on 3' ) ;
  CheckEquals( 'c:\temp\test.txt', tiUtils.tiSwapExt( 'c:\temp\test', 'txt' ), 'Failed on 4' ) ;
  CheckEquals( 'c:\temp\test.',     tiUtils.tiSwapExt( 'c:\temp\test.txt', '' ), 'Failed on 5' ) ;
end ;

procedure TTestTIUtils.tiExtractExtension ;
begin
  Check( tiUtils.tiExtractExtension( 'test.txt' ) = 'txt', 'Failed on 1' ) ;
  Check( tiUtils.tiExtractExtension( 'c:\temp\test.txt' ) = 'txt', 'Failed on 2' ) ;
  Check( tiUtils.tiExtractExtension( 'c:\temp\test.' ) = '', 'Failed on 3' ) ;
  Check( tiUtils.tiExtractExtension( 'c:\temp\test' ) = '', 'Failed on 4' ) ;
end ;

procedure TTestTIUtils.tiCopyFile ;
var
  lslFrom : TStringList ;
  lslTo   : TStringList ;
  lFrom   : string ;
  lTo     : string ;

begin
  lFrom := TempFileName( 'DUnitTest_From.txt' );
  lTo   := TempFileName( 'DUnitTest_To.txt' );

  if FileExists( lFrom ) then
    SysUtils.DeleteFile( lFrom ) ;
  if FileExists( lTo ) then
    SysUtils.DeleteFile( lTo ) ;
  lslFrom := TStringList.Create ;
  try
    lslTo   := TStringList.Create ;
    try
      lslFrom.Text := BuildLongString ;
      lslFrom.SaveToFile( lFrom ) ;
      tiUtils.tiCopyFile( lFrom, lTo ) ;
      lslTo.LoadFromFile( lTo ) ;
      Check( lslFrom.Text = lslTo.Text ) ;
      SysUtils.DeleteFile( lFrom ) ;
      SysUtils.DeleteFile( lTo ) ;
    finally
      lslTo.Free ;
    end ;
  finally
    lslFrom.Free ;
  end ;
end ;

procedure TTestTIUtils.tiMoveFile ;
var
  lslFrom : TStringList ;
  lslTo   : TStringList ;
  ls : string ;
  i : integer ;
  j : integer ;
  lFrom   : string ;
  lTo     : string ;

begin
  lFrom := TempFileName( 'DUnitTest_From.txt' );
  lTo   := TempFileName( 'DUnitTest_To.txt' );

  if FileExists( lFrom ) then
    SysUtils.DeleteFile( lFrom ) ;
  if FileExists( lTo ) then
    SysUtils.DeleteFile( lTo ) ;

  lslFrom := TStringList.Create ;
  try
    lslTo   := TStringList.Create ;
    try
      for i := 0 to 1000 do
      begin
        for j := 1 to 255 do
          ls := ls + Chr( j ) ;
        ls := ls + #13 ;
      end ;
      lslFrom.Text := ls ;
      lslFrom.SaveToFile( lFrom ) ;
      tiUtils.tiMoveFile( lFrom, lTo ) ;
      lslTo.LoadFromFile( lTo ) ;
      Check( FileExists( lTo ), 'To file does not exist' ) ;
      Check( not FileExists( lFrom ), 'From file exists' ) ;
      Check( lslFrom.Text = lslTo.Text ) ;
    finally
      lslTo.Free ;
    end ;
  finally
    lslFrom.Free ;
  end ;
  if FileExists( lFrom ) then
    SysUtils.DeleteFile( lFrom ) ;
  if FileExists( lTo ) then
    SysUtils.DeleteFile( lTo ) ;
end ;

{
procedure TTestTIUtils._CreateFileOfSize( pFileName : string ; pSize : LongInt ) ;
var
  lFileStream : TFileStream ;
  lBuffer   : PChar ;
  lLen      : integer ;
  ls : string ;
  i : integer ;
begin
  ls := '' ;
  for i := 1 to pSize do
    ls := ls + Chr( Random( 255 + 1 )) ;
  if FileExists( pFileName ) then
    DeleteFile( pFileName ) ;
  lFileStream := TFileStream.Create( pFileName,
                                     fmCreate or fmShareCompat ) ;
  try
    lBuffer := PChar( ls ) ;
    lLen := length( ls ) ;
    lFileStream.write( lBuffer^, lLen ) ;
  finally
    lFileStream.Free ;
  end ;
end ;
}

procedure TTestTIUtils.tiGetFileSize ;
var
  lFileName   : string ;

begin
  lFileName := TempFileName( 'filesizetest.txt' );

  tiCreateTextFileOfSize( lFileName, 0 ) ;
  CheckEquals( 0, tiUtils.tiGetFileSize( lFileName ), 'Failed on 5' ) ;

  tiCreateTextFileOfSize( lFileName, 1 ) ;
  CheckEquals( 1, tiUtils.tiGetFileSize( lFileName ), 'Failed on 5' ) ;

  tiCreateTextFileOfSize( lFileName, 10 ) ;
  CheckEquals( 10, tiUtils.tiGetFileSize( lFileName ), 'Failed on 5' ) ;

  tiCreateTextFileOfSize( lFileName, 100 ) ;
  CheckEquals( 100, tiUtils.tiGetFileSize( lFileName ), 'Failed on 5' ) ;

  tiCreateTextFileOfSize( lFileName, 1000 ) ;
  CheckEquals( 1000, tiUtils.tiGetFileSize( lFileName ), 'Failed on 5' ) ;

  SysUtils.DeleteFile( lFileName ) ;

end ;

procedure TTestTIUtils.tiRemoveDrive ;
begin
  CheckEquals( '', tiUtils.tiRemoveDrive(      'c:' ), 'Failed on 1' ) ;
  CheckEquals( '\temp', tiUtils.tiRemoveDrive( 'c:\temp' ), 'Failed on 2' ) ;
  CheckEquals( '\temp\hos.txt', tiUtils.tiRemoveDrive( 'c:\temp\hos.txt' ), 'Failed on 3' ) ;
  CheckEquals( '\Program Files\My Program\run.bat', tiUtils.tiRemoveDrive( 'c:\Program Files\My Program\run.bat' ), 'Failed on 4' ) ;
end ;

procedure TTestTIUtils.tiSetFileReadOnly ;
var
  lsl : TStringList ;
  lCurrentState : integer ;
  lFileName   : string ;
const
  cReadOnly  = $00000001;
  cReadOnlyBit = 0 ;

begin
  lFileName := TempFileName( 'DUnitTest.txt' );

  lsl := TStringList.Create ;
  try
    lsl.Text := BuildLongString ;
    lsl.SaveToFile( lFileName ) ;
    try
      lCurrentState := tiWin32FileGetAttr( lFileName ) ;
      Check((lCurrentState and (1 shl cReadOnlyBit)) = 0, 'Failed on 1' ) ;
      tiUtils.tiSetFileReadOnly( lFileName, true ) ;
      lCurrentState := tiWin32FileGetAttr( lFileName ) ;
      Check((lCurrentState and (1 shl cReadOnlyBit)) <> 0, 'Failed on 2' ) ;
      tiUtils.tiSetFileReadOnly( lFileName, false ) ;
    finally
      SysUtils.DeleteFile( lFileName ) ;
    end ;
  finally
    lsl.Free ;
  end ;
end ;

procedure TTestTIUtils.tiIsFileReadOnly ;
var
  lsl : TStringList ;
  lCurrentState : integer ;
  lFileName   : string ;
const
  cReadOnly  = $00000001;

begin
  lFileName := TempFileName( 'DUnitTest.txt' );

  lsl := TStringList.Create ;
  try
    lsl.Text := BuildLongString ;
    lsl.SaveToFile( lFileName ) ;
    try
      Check( Not tiUtils.tiIsFileReadOnly( lFileName ), 'Not Failed on IsFileReadOnly' ) ;
      lCurrentState := tiWin32FileGetAttr( lFileName ) ;
      tiWin32FileSetAttr( lFileName, lCurrentState xor cReadOnly ) ;
      Check( tiUtils.tiIsFileReadOnly( lFileName ), 'Failed on IsFileReadOnly' ) ;
      // Clean up
      lCurrentState := tiWin32FileGetAttr( lFileName ) ;
      tiWin32FileSetAttr( lFileName, lCurrentState xor cReadOnly ) ;
    finally
      SysUtils.DeleteFile( lFileName ) ;
    end ;
  finally
    lsl.Free ;
  end ;
end ;

procedure TTestTIUtils.tiDirectoryTreeToStringList ;
var
  lsl : TStringList ;
  lTempPath : string ;
begin
  lTempPath := TempFileName( 'DUnitTests' ) ;

  lsl := TStringList.Create ;
  try

    ForceDirectories( lTempPath ) ;
    tiUtils.tiDirectoryTreeToStringList( lTempPath, lsl, true ) ;
    CheckEquals( 1, lsl.Count ) ;
    Check( SameText( lsl.Strings[0], lTempPath ),      'Failed on 0' ) ;

    ForceDirectories( lTempPath + '\Dir1\Dir1-1\Dir1-1-1' ) ;
    ForceDirectories( lTempPath + '\Dir1\Dir1-1\Dir1-1-2' ) ;
    ForceDirectories( lTempPath + '\Dir1\Dir1-1\Dir1-1-3' ) ;
    ForceDirectories( lTempPath + '\Dir1\Dir1-2\Dir1-2-1' ) ;
    ForceDirectories( lTempPath + '\Dir1\Dir1-2\Dir1-2-2' ) ;
    ForceDirectories( lTempPath + '\Dir1\Dir1-2\Dir1-2-3' ) ;
    ForceDirectories( lTempPath + '\Dir1\Dir1-3\Dir1-3-1' ) ;
    ForceDirectories( lTempPath + '\Dir1\Dir1-3\Dir1-3-2' ) ;
    ForceDirectories( lTempPath + '\Dir1\Dir1-3\Dir1-3-3' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-1\Dir2-1-1' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-1\Dir2-1-2' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-1\Dir2-1-3' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-2\Dir2-2-1' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-2\Dir2-2-2' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-2\Dir2-2-3' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-3\Dir2-3-1' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-3\Dir2-3-2' ) ;
    ForceDirectories( lTempPath + '\Dir2\Dir2-3\Dir2-3-3' ) ;

    tiUtils.tiDirectoryTreeToStringList( lTempPath, lsl, false ) ;
    Check( SameText( lsl.Strings[0], lTempPath ),      'Failed on 0' ) ;
    Check( SameText( lsl.Strings[1], lTempPath + '\Dir1' ), 'Failed on 1' ) ;
    Check( SameText( lsl.Strings[2], lTempPath + '\Dir2' ), 'Failed on 2' ) ;

    tiUtils.tiDirectoryTreeToStringList( lTempPath + '\', lsl, false ) ;
    Check( SameText( lsl.Strings[0], lTempPath ),      'Failed on 0a' ) ;
    Check( SameText( lsl.Strings[1], lTempPath + '\Dir1' ), 'Failed on 1a' ) ;
    Check( SameText( lsl.Strings[2], lTempPath + '\Dir2' ), 'Failed on 2a' ) ;

    tiUtils.tiDirectoryTreeToStringList( lTempPath, lsl, true ) ;
    Check( SameText( lsl.Strings[0],  lTempPath ), 'Failed on 3' ) ;
    Check( SameText( lsl.Strings[1],  lTempPath + '\Dir1' ), 'Failed on 4' ) ;
    Check( SameText( lsl.Strings[2],  lTempPath + '\Dir1\Dir1-1' ), 'Failed on 5' ) ;
    Check( SameText( lsl.Strings[3],  lTempPath + '\Dir1\Dir1-1\Dir1-1-1' ), 'Failed on 6' ) ;
    Check( SameText( lsl.Strings[4],  lTempPath + '\Dir1\Dir1-1\Dir1-1-2' ), 'Failed on 7' ) ;
    Check( SameText( lsl.Strings[5],  lTempPath + '\Dir1\Dir1-1\Dir1-1-3' ), 'Failed on 8' ) ;
    Check( SameText( lsl.Strings[6],  lTempPath + '\Dir1\Dir1-2' ), 'Failed on 9' ) ;
    Check( SameText( lsl.Strings[7],  lTempPath + '\Dir1\Dir1-2\Dir1-2-1' ), 'Failed on 10' ) ;
    Check( SameText( lsl.Strings[8],  lTempPath + '\Dir1\Dir1-2\Dir1-2-2' ), 'Failed on 11' ) ;
    Check( SameText( lsl.Strings[9],  lTempPath + '\Dir1\Dir1-2\Dir1-2-3' ), 'Failed on 12' ) ;
    Check( SameText( lsl.Strings[10], lTempPath + '\Dir1\Dir1-3' ), 'Failed on 1' ) ;
    Check( SameText( lsl.Strings[11], lTempPath + '\Dir1\Dir1-3\Dir1-3-1' ), 'Failed on 13' ) ;
    Check( SameText( lsl.Strings[12], lTempPath + '\Dir1\Dir1-3\Dir1-3-2' ), 'Failed on 14' ) ;
    Check( SameText( lsl.Strings[13], lTempPath + '\Dir1\Dir1-3\Dir1-3-3' ), 'Failed on 15' ) ;
    Check( SameText( lsl.Strings[14], lTempPath + '\Dir2' ), 'Failed on 16' ) ;
    Check( SameText( lsl.Strings[15], lTempPath + '\Dir2\Dir2-1' ), 'Failed on 17' ) ;
    Check( SameText( lsl.Strings[16], lTempPath + '\Dir2\Dir2-1\Dir2-1-1' ), 'Failed on 18' ) ;
    Check( SameText( lsl.Strings[17], lTempPath + '\Dir2\Dir2-1\Dir2-1-2' ), 'Failed on 19' ) ;
    Check( SameText( lsl.Strings[18], lTempPath + '\Dir2\Dir2-1\Dir2-1-3' ), 'Failed on 20' ) ;
    Check( SameText( lsl.Strings[19], lTempPath + '\Dir2\Dir2-2' ), 'Failed on 21' ) ;
    Check( SameText( lsl.Strings[20], lTempPath + '\Dir2\Dir2-2\Dir2-2-1' ), 'Failed on 22' ) ;
    Check( SameText( lsl.Strings[21], lTempPath + '\Dir2\Dir2-2\Dir2-2-2' ), 'Failed on 23' ) ;
    Check( SameText( lsl.Strings[22], lTempPath + '\Dir2\Dir2-2\Dir2-2-3' ), 'Failed on 24' ) ;
    Check( SameText( lsl.Strings[23], lTempPath + '\Dir2\Dir2-3' ), 'Failed on 25' ) ;
    Check( SameText( lsl.Strings[24], lTempPath + '\Dir2\Dir2-3\Dir2-3-1' ), 'Failed on 26' ) ;
    Check( SameText( lsl.Strings[25], lTempPath + '\Dir2\Dir2-3\Dir2-3-2' ), 'Failed on 27' ) ;
    Check( SameText( lsl.Strings[26], lTempPath + '\Dir2\Dir2-3\Dir2-3-3' ), 'Failed on 28' ) ;

  finally
    lsl.Free ;
  end ;

  tiDUnitForceRemoveDir( lTempPath ) ;

end ;

procedure TTestTIUtils.TestCreateDir;
var
  lRoot : string ;
begin
  lRoot := TempFileName( 'DUnit2' ) ;
  tiDUnitForceRemoveDir( lRoot ) ;
  Check( not DirectoryExists( lRoot ), 'Directory exists when it should not <' + lRoot + '>' ) ;
  if not ForceDirectories( lRoot ) then
    Fail('Unable to create directory <' + lRoot + '>');
  Check( DirectoryExists( lRoot ), 'Unable to create directory <' + lRoot + '>' ) ;
  tiDUnitForceRemoveDir(lRoot);
end;

procedure TTestTIUtils.TestCreateFile ;
var
  lsl : TStringList ;
  lRoot : string ;
begin
  lRoot := TempFileName( 'DUnitTests' ) ;
  tiDUnitForceRemoveDir( lRoot ) ;
  if not ForceDirectories( lRoot ) then
    Fail('Unable to create directory <' + lRoot + '>');
  Check( DirectoryExists( lRoot ), 'Unable to create directory <' + lRoot + '>' ) ;
  lsl := TStringList.Create ;
  try
    lsl.Text := 'test';
    lsl.SaveToFile(lRoot + '\temp.txt' ) ;
    Check( SysUtils.FileExists( lRoot + '\temp.txt'), 'File not found' ) ;
  finally
    lsl.Free;
  end;
  tiDUnitForceRemoveDir( lRoot ) ;
end ;

  // Copy all the files, from the directory pStrStartDir, matching the wildcard pStrWildCard
procedure TTestTIUtils.tiFilesToStringList ;
var
  lsl : TStringList ;
  i : integer ;
  lRoot : string ;
begin
  lRoot := TempFileName( 'DUnit' ) ;
  tiDUnitForceRemoveDir( lRoot ) ;
  if not ForceDirectories( lRoot ) then
    Fail('Unable to create directory <' + lRoot + '>');
  Check( DirectoryExists( lRoot ), 'Unable to create directory <' + lRoot + '>' ) ;
  for i := 0 to 100000 do begin end;

  lsl := TStringList.Create ;
  try
    lsl.Text := 'test' ;
    lsl.SaveToFile( lRoot + '\file1.txt' ) ;
    lsl.SaveToFile( lRoot + '\file2.csv' ) ;
    lsl.SaveToFile( lRoot + '\file3.exe' ) ;
    lsl.SaveToFile( lRoot + '\file4.txt' ) ;
    tiUtils.tiFilesToStringList( lRoot, '*.*', lsl, false ) ;

    CheckEquals( 4, lsl.Count, 'Count' ) ;
    CheckEquals( UpperCase(lsl.Strings[0]), UpperCase(lRoot + '\file1.txt')) ;
    CheckEquals( UpperCase(lsl.Strings[1]), UpperCase(lRoot + '\file2.csv')) ;
    CheckEquals( UpperCase(lsl.Strings[2]), UpperCase(lRoot + '\file3.exe')) ;
    CheckEquals( UpperCase(lsl.Strings[3]), UpperCase(lRoot + '\file4.txt')) ;

    tiUtils.tiFilesToStringList( lRoot, '*.txt', lsl, false ) ;
    CheckEquals( 2, lsl.Count, 'Count' ) ;
    CheckEquals( UpperCase(lsl.Strings[0]), UpperCase(lRoot + '\file1.txt')) ;
    CheckEquals( UpperCase(lsl.Strings[1]), UpperCase(lRoot + '\file4.txt')) ;
    tiUtils.tiFilesToStringList( lRoot, '*3.*', lsl, false ) ;
    CheckEquals( 1, lsl.Count, 'Count' ) ;
    CheckEquals( UpperCase(lsl.Strings[0]), UpperCase(lRoot + '\file3.exe')) ;

    if not ForceDirectories( lRoot + '\Dir1\' ) then
      Fail( 'Unable to create directory <' + lRoot + '\Dir1\' + '>'  ) ;
    Check( DirectoryExists( lRoot + '\Dir1\' ), 'Unable to create directory <' + lRoot + '\Dir1\' + '>' ) ;
    lsl.Text := 'test' ;

    lsl.SaveToFile( lRoot + '\Dir1\file1.txt' ) ;
    lsl.SaveToFile( lRoot + '\Dir1\file2.txt' ) ;
    lsl.SaveToFile( lRoot + '\Dir1\file3.txt' ) ;
    lsl.SaveToFile( lRoot + '\Dir1\file4.txt' ) ;

    tiUtils.tiFilesToStringList( lRoot, '*.*', lsl, true ) ;
    CheckEquals( 8, lsl.Count, 'Count' ) ;
    CheckEquals( UpperCase(lsl.Strings[0]), UpperCase(lRoot + '\file1.txt')) ;
    CheckEquals( UpperCase(lsl.Strings[1]), UpperCase(lRoot + '\file2.csv')) ;
    CheckEquals( UpperCase(lsl.Strings[2]), UpperCase(lRoot + '\file3.exe')) ;
    CheckEquals( UpperCase(lsl.Strings[3]), UpperCase(lRoot + '\file4.txt')) ;
    CheckEquals( UpperCase(lsl.Strings[4]), UpperCase(lRoot + '\Dir1\file1.txt')) ;
    CheckEquals( UpperCase(lsl.Strings[5]), UpperCase(lRoot + '\Dir1\file2.txt')) ;
    CheckEquals( UpperCase(lsl.Strings[6]), UpperCase(lRoot + '\Dir1\file3.txt')) ;
    CheckEquals( UpperCase(lsl.Strings[7]), UpperCase(lRoot + '\Dir1\file4.txt')) ;

  finally
    lsl.Free;
  end;
  tiDUnitForceRemoveDir( lRoot ) ;
end ;

procedure TTestTIUtils.tiHasSubDirectory ;
var
  lDirRoot : string ;
begin
  lDirRoot := TempFileName( 'HasSubDir' ) ;
  tiDUnitForceRemoveDir( lDirRoot ) ;
  Check( not tiUtils.tiHasSubDirectory( lDirRoot ), 'Failed on call 1' ) ;
  ForceDirectories( lDirRoot ) ;
  Check( not tiUtils.tiHasSubDirectory( lDirRoot + '\' ), 'Failed on call 2' ) ;
  ForceDirectories( lDirRoot + '\Level1' ) ;
  Check( tiUtils.tiHasSubDirectory( lDirRoot ), 'Failed on call 3' ) ;
  ForceDirectories( lDirRoot + '\Level1\' ) ;
  Check( tiUtils.tiHasSubDirectory( lDirRoot ), 'Failed on call 4' ) ;
  Check( not tiUtils.tiHasSubDirectory( lDirRoot + '\Level1\HOS' ), 'Failed on call 5' ) ;
  Check( not tiUtils.tiHasSubDirectory( lDirRoot + '\Level1\HOS\' ), 'Failed on call 6' ) ;
  tiDUnitForceRemoveDir( lDirRoot ) ;
end ;

procedure TTestTIUtils.tiStringToFile ;
var
  lFileStream : TFileStream ;
  lsFrom : string ;
  lsTo : string ;
  lBuffer   : PChar ;
  lFileName : string ;
begin
  lFileName := TempFileName( 'DUnitTest.txt' ) ;

  lsFrom := BuildLongString ;
  tiUtils.tiStringToFile( lsFrom, lFileName ) ;
  lFileStream := TFileStream.Create( lFileName,
                                     fmOpenReadWrite or fmShareDenyNone ) ;
  try
    GetMem( lBuffer, lFileStream.Size ) ;
    try
      lFileStream.read( lBuffer^, lFileStream.Size ) ;
      lsTo := String( lBuffer ) ;
    finally
      FreeMem( lBuffer ) ;
    end ;
  finally
    lFileStream.Free ;
  end ;

  CheckEquals( lsFrom, lsTo ) ;
  SysUtils.DeleteFile(lFileName);

end ;

procedure TTestTIUtils.tiFileToString ;
var
  lFileStream : TFileStream ;
  lsFrom : string ;
  lsTo : string ;
  lBuffer   : PChar ;
  lLen      : integer ;
  lFileName : string ;
begin
  lFileName := TempFileName( 'DUnitTest.txt' ) ;
  lsFrom := BuildLongString ;
  if FileExists( lFileName ) then
    SysUtils.DeleteFile( lFileName ) ;
  lFileStream := TFileStream.Create( lFileName,
                                     fmCreate or fmShareDenyNone ) ;
  try
    lBuffer := PChar( lsFrom ) ;
    lLen := length( lsFrom ) ;
    lFileStream.write( lBuffer^, lLen ) ;
  finally
    lFileStream.Free ;
  end ;
  lsTo := tiUtils.tiFileToString( lFileName ) ;
  CheckEquals( lsFrom, lsTo ) ;
  SysUtils.DeleteFile(lFileName);
end ;

  // Extract a directory name to a certain level.
  // eg tiExtractDirToLevel( 'c:\temp\dir', 0 ) gives 'c:'
  //    tiExtractDirToLevel( 'c:\temp\dir', 1 ) gives 'c:\temp'
procedure TTestTIUtils.tiExtractDirToLevel ;
const
  cDir = 'C:\Temp\DUnitTests\Dir1\Dir1-1\Dir1-1-3' ;
begin
  Check( SameText( tiUtils.tiExtractDirToLevel( cDir, 0 ), 'C:' ), 'Failed on 1' ) ;
  Check( SameText( tiUtils.tiExtractDirToLevel( cDir, 1 ), 'C:\Temp' ), 'Failed on 2') ;
  Check( SameText( tiUtils.tiExtractDirToLevel( cDir, 2 ), 'C:\Temp\DUnitTests' ), 'Failed on 3') ;
  Check( SameText( tiUtils.tiExtractDirToLevel( cDir, 3 ), 'C:\Temp\DUnitTests\Dir1' ), 'Failed on 4' ) ;
  Check( SameText( tiUtils.tiExtractDirToLevel( cDir, 4 ), 'C:\Temp\DUnitTests\Dir1\Dir1-1' ), 'Failed on 5') ;
  Check( SameText( tiUtils.tiExtractDirToLevel( cDir, 5 ), 'C:\Temp\DUnitTests\Dir1\Dir1-1\Dir1-1-3' ), 'Failed on 6') ;
  Check( SameText( tiUtils.tiExtractDirToLevel( cDir, 6 ), 'C:\Temp\DUnitTests\Dir1\Dir1-1\Dir1-1-3' ), 'Failed on 7') ;
end ;


procedure TTestTIUtils.tiSafeDiv ;
begin
  CheckEquals( 0, tiUtils.tiSafeDiv( 100, 0 ), 'Failed on 1' ) ;
  CheckEquals( 10, tiUtils.tiSafeDiv( 100, 10 ), 'Failed on 2' ) ;
  CheckEquals( 222.222, tiUtils.tiSafeDiv( 444.444, 2 ), cDUnitTestFloatPrecision, 'Failed on 3' ) ;
  CheckEquals( 0, tiUtils.tiSafeDiv( 444.444, 0 ), 'Failed on 4' ) ;
end ;

procedure TTestTIUtils.tiRound ;
begin
  CheckEquals(  1, tiUtils.tiRound( 1 ),              'Failed on    1' ) ;
  CheckEquals(  1, tiUtils.tiRound( 1.4 ),            'Failed on    2' ) ;
  CheckEquals(  1, tiUtils.tiRound( 1.49999999999 ),  'Failed on    3' ) ;
  CheckEquals(  2, tiUtils.tiRound( 1.5 ),            'Failed on    4' ) ;
  CheckEquals(  2, tiUtils.tiRound( 1.6 ),            'Failed on    5' ) ;
  CheckEquals( -1, tiUtils.tiRound( -1 ),             'Failed on  6' ) ;
  CheckEquals( -1, tiUtils.tiRound( -1.4 ),           'Failed on  7' ) ;
  CheckEquals( -1, tiUtils.tiRound( -1.49999999999 ), 'Failed on  8' ) ;
  CheckEquals( -2, tiUtils.tiRound( -1.5 ),           'Failed on  9' ) ;
  CheckEquals( -2, tiUtils.tiRound( -1.6 ),           'Failed on 10' ) ;
end ;

procedure TTestTIUtils.tiSetPrecision ;
const
  cDUnitTestTISetPrecision = 0.0000001 ;
begin
  CheckEquals( 1,       tiUtils.tiSetPrecision(  1.2345, 0 ), cDUnitTestTISetPrecision, 'Failed on  1' ) ;
  CheckEquals( 12,      tiUtils.tiSetPrecision(  12.345, 0 ), cDUnitTestTISetPrecision, 'Failed on  2' ) ;
  CheckEquals( 123,     tiUtils.tiSetPrecision(  123.45, 0 ), cDUnitTestTISetPrecision, 'Failed on  3' ) ;
  CheckEquals( 1235,    tiUtils.tiSetPrecision(  1234.5, 0 ), cDUnitTestTISetPrecision, 'Failed on  4' ) ;
  CheckEquals( 12345,   tiUtils.tiSetPrecision(  12345,  0 ), cDUnitTestTISetPrecision, 'Failed on  5' ) ;

  CheckEquals( 1.2,      tiUtils.tiSetPrecision( 1.2345, 1 ), cDUnitTestTISetPrecision, 'Failed on  6' ) ;
  CheckEquals( 12.3,     tiUtils.tiSetPrecision( 12.345, 1 ), cDUnitTestTISetPrecision, 'Failed on  7' ) ;
  CheckEquals( 123.5,    tiUtils.tiSetPrecision( 123.45, 1 ), cDUnitTestTISetPrecision, 'Failed on  8' ) ;
  CheckEquals( 1234.5,   tiUtils.tiSetPrecision( 1234.5, 1 ), cDUnitTestTISetPrecision, 'Failed on  9' ) ;
  CheckEquals( 12345.0,  tiUtils.tiSetPrecision( 12345,  1 ), cDUnitTestTISetPrecision, 'Failed on 10' ) ;

  CheckEquals( 1.23,     tiUtils.tiSetPrecision( 1.2345, 2 ), cDUnitTestTISetPrecision, 'Failed on 11' ) ;
  CheckEquals( 12.35,    tiUtils.tiSetPrecision( 12.345, 2 ), cDUnitTestTISetPrecision, 'Failed on 12' ) ;
  CheckEquals( 123.45,   tiUtils.tiSetPrecision( 123.45, 2 ), cDUnitTestTISetPrecision, 'Failed on 13' ) ;
  CheckEquals( 1234.50,  tiUtils.tiSetPrecision( 1234.5, 2 ), cDUnitTestTISetPrecision, 'Failed on 14' ) ;
  CheckEquals( 12345.00, tiUtils.tiSetPrecision( 12345,  2 ), cDUnitTestTISetPrecision, 'Failed on 15' ) ;

//  CheckEquals( 1.235,     tiUtils.tiSetPrecision( 1.2345, 3 ), cDUnitTestTISetPrecision, 'Failed on 16' ) ;
  CheckEquals( 12.345,    tiUtils.tiSetPrecision( 12.345, 3 ), cDUnitTestTISetPrecision, 'Failed on 17' ) ;
  CheckEquals( 123.450,   tiUtils.tiSetPrecision( 123.45, 3 ), cDUnitTestTISetPrecision, 'Failed on 18' ) ;
  CheckEquals( 1234.500,  tiUtils.tiSetPrecision( 1234.5, 3 ), cDUnitTestTISetPrecision, 'Failed on 19' ) ;
  CheckEquals( 12345.000, tiUtils.tiSetPrecision( 12345,  3 ), cDUnitTestTISetPrecision, 'Failed on 20' ) ;

  CheckEquals( 0,         tiUtils.tiSetPrecision( 1.2345, -1 ), cDUnitTestTISetPrecision, 'Failed on 21' ) ;
  CheckEquals( 10,        tiUtils.tiSetPrecision( 12.345, -1 ), cDUnitTestTISetPrecision, 'Failed on 22' ) ;
  CheckEquals( 120,       tiUtils.tiSetPrecision( 123.45, -1 ), cDUnitTestTISetPrecision, 'Failed on 23' ) ;
  CheckEquals( 1230,      tiUtils.tiSetPrecision( 1234.5, -1 ), cDUnitTestTISetPrecision, 'Failed on 24' ) ;
  CheckEquals( 12350,     tiUtils.tiSetPrecision( 12345,  -1 ), cDUnitTestTISetPrecision, 'Failed on 25' ) ;

  CheckEquals( 0,         tiUtils.tiSetPrecision( 1.2345, -2 ), cDUnitTestTISetPrecision, 'Failed on 26' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 12.345, -2 ), cDUnitTestTISetPrecision, 'Failed on 27' ) ;
  CheckEquals( 100,       tiUtils.tiSetPrecision( 123.45, -2 ), cDUnitTestTISetPrecision, 'Failed on 28' ) ;
  CheckEquals( 1200,      tiUtils.tiSetPrecision( 1234.5, -2 ), cDUnitTestTISetPrecision, 'Failed on 29' ) ;
  CheckEquals( 12300,     tiUtils.tiSetPrecision( 12345,  -2 ), cDUnitTestTISetPrecision, 'Failed on 30' ) ;

  CheckEquals( 0,         tiUtils.tiSetPrecision( 1.2345, -3 ), cDUnitTestTISetPrecision, 'Failed on 31' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 12.345, -3 ), cDUnitTestTISetPrecision, 'Failed on 32' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 123.45, -3 ), cDUnitTestTISetPrecision, 'Failed on 33' ) ;
  CheckEquals( 1000,      tiUtils.tiSetPrecision( 1234.5, -3 ), cDUnitTestTISetPrecision, 'Failed on 34' ) ;
  CheckEquals( 12000,     tiUtils.tiSetPrecision( 12345,  -3 ), cDUnitTestTISetPrecision, 'Failed on 35' ) ;

  CheckEquals( 0,         tiUtils.tiSetPrecision( 1.2345, -4 ), cDUnitTestTISetPrecision, 'Failed on 36' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 12.345, -4 ), cDUnitTestTISetPrecision, 'Failed on 37' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 123.45, -4 ), cDUnitTestTISetPrecision, 'Failed on 38' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 1234.5, -4 ), cDUnitTestTISetPrecision, 'Failed on 39' ) ;
  CheckEquals( 10000,     tiUtils.tiSetPrecision( 12345,  -4 ), cDUnitTestTISetPrecision, 'Failed on 40' ) ;

  CheckEquals( 0,         tiUtils.tiSetPrecision( 1.2345, -5 ), cDUnitTestTISetPrecision, 'Failed on 41' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 12.345, -5 ), cDUnitTestTISetPrecision, 'Failed on 42' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 123.45, -5 ), cDUnitTestTISetPrecision, 'Failed on 43' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 1234.5, -5 ), cDUnitTestTISetPrecision, 'Failed on 44' ) ;
  CheckEquals( 0,         tiUtils.tiSetPrecision( 12345,  -5 ), cDUnitTestTISetPrecision, 'Failed on 45' ) ;

end ;

procedure TTestTIUtils.tiDateToPreviousWeekDayDate ;
begin
  Check( tiUtils.tiDateToPreviousWeekDayDate( EncodeDate( 2002, 2, 25 )) =
         EncodeDate( 2002, 2, 22 ), 'Failed on 1') ;
  Check( tiUtils.tiDateToPreviousWeekDayDate( EncodeDate( 2002, 2, 26 )) =
         EncodeDate( 2002, 2, 25 ), 'Failed on 2') ;
  Check( tiUtils.tiDateToPreviousWeekDayDate( EncodeDate( 2002, 2, 27 )) =
         EncodeDate( 2002, 2, 26 ), 'Failed on 3') ;
  Check( tiUtils.tiDateToPreviousWeekDayDate( EncodeDate( 2002, 2, 28 )) =
         EncodeDate( 2002, 2, 27 ), 'Failed on 4') ;
  Check( tiUtils.tiDateToPreviousWeekDayDate( EncodeDate( 2002, 3, 1 )) =
         EncodeDate( 2002, 2, 28 ), 'Failed on 5') ;
  Check( tiUtils.tiDateToPreviousWeekDayDate( EncodeDate( 2002, 3, 2 )) =
         EncodeDate( 2002, 3, 1), 'Failed on 6') ;
  Check( tiUtils.tiDateToPreviousWeekDayDate( EncodeDate( 2002, 3, 3 )) =
         EncodeDate( 2002, 3, 1), 'Failed on 7') ;

end ;

procedure TTestTIUtils.tiYear  ;
var
  lDate : TDateTime ;
const
  cExpected = '01/01/2000' ;
begin
  lDate := EncodeDate( 2000, 1, 1 ) + EncodeTime( 6, 30, 15, 10 ) ;
  CheckEquals( 2000, tiUtils.tiYear( lDate )) ;
end ;

procedure TTestTIUtils.tiStrToInt ;
begin
  CheckEquals( 0, tiUtils.tiStrToInt( '0' )) ;
  CheckEquals( 0, tiUtils.tiStrToInt( 'hos' )) ;
  CheckEquals( 0, tiUtils.tiStrToInt( '1+1' )) ;
  CheckEquals( 1000, tiUtils.tiStrToInt( '1,000' )) ;
  CheckEquals( 1000000, tiUtils.tiStrToInt( '1,000,000' )) ;
  CheckEquals( 1000, tiUtils.tiStrToInt( '$ 1,000' )) ;
end ;

procedure TTestTIUtils.tiStrToFloat ;
begin
  CheckEquals( 0, tiUtils.tiStrToFloat( '0' ), 'Failed on ''0''' ) ;
  CheckEquals( 0, tiUtils.tiStrToFloat( 'hos' ), 'Failed on ''hos''') ;
  CheckEquals( 0, tiUtils.tiStrToFloat( '1+1' ), 'Failed on ''1+1''') ;
  CheckEquals( 1000, tiUtils.tiStrToFloat( '1,000' ), 'Failed on ''1,000''') ;
  CheckEquals( 1000000, tiUtils.tiStrToFloat( '1,000,000' ), 'Failed on ''1,000,000''') ;
  CheckEquals( 1000, tiUtils.tiStrToFloat( '$ 1,000' ), 'Failed on ''$ 1,000''') ;
end ;

procedure TTestTIUtils.tiDateToStr ;
var
  lDate : TDateTime ;
const
  cExpected = '01/01/2000' ;
begin
  lDate := EncodeDate( 2000, 1, 1 ) + EncodeTime( 6, 30, 15, 10 ) ;
  Check( tiUtils.tiDateToStr( lDate ) =
         cExpected,
         'Got <' + tiUtils.tiDateTimeToStr( lDate ) + '> expected <' + cExpected + '>' ) ;
end ;

procedure TTestTIUtils.tiDateTimeToStr ;
var
  lDate : TDateTime ;
const
  cExpected = '01/01/2000 06:30:15' ;
begin
  lDate := EncodeDate( 2000, 1, 1 ) + EncodeTime( 6, 30, 15, 10 ) ;
  Check( tiUtils.tiDateTimeToStr( lDate ) =
         cExpected,
         'Got <' + tiUtils.tiDateTimeToStr( lDate ) + '> expected <' + cExpected + '>' ) ;
end ;

procedure TTestTIUtils.tiTimeToStr ;
var
  lDate : TDateTime ;
const
  cExpected = '06:30:15' ;
begin
  lDate := EncodeDate( 2000, 1, 1 ) + EncodeTime( 6, 30, 15, 10 ) ;
  Check( tiUtils.tiTimeToStr( lDate ) =
         cExpected,
         'Got <' + tiUtils.tiTimeToStr( lDate ) + '> expected <' + cExpected + '>' ) ;
end ;

procedure TTestTIUtils.tiIntToStrHide0 ;
begin
  CheckEquals( '',  tiUtils.tiIntToStrHide0( 0 ), 'Failed on 0' ) ;
  CheckEquals( '10', tiUtils.tiIntToStrHide0( 10 ), 'Failed on 10' ) ;
  CheckEquals( '-10', tiUtils.tiIntToStrHide0( -10 ), 'Failed on -10' ) ;
end ;

procedure TTestTIUtils.tiIntToCommaStr ;
begin
  CheckEquals( '0', tiUtils.tiIntToCommaStr( 0 ), 'Failed on 0' ) ;
  CheckEquals( '10', tiUtils.tiIntToCommaStr( 10 ), 'Failed on 10' ) ;
  CheckEquals( '100', tiUtils.tiIntToCommaStr( 100 ), 'Failed on 100' ) ;
  CheckEquals( '1,000', tiUtils.tiIntToCommaStr( 1000 ), 'Failed on 1000' ) ;
  CheckEquals( '10,000', tiUtils.tiIntToCommaStr( 10000 ), 'Failed on 10000' ) ;
  CheckEquals( '100,000', tiUtils.tiIntToCommaStr( 100000 ), 'Failed on 100000' ) ;
  CheckEquals( '1,000,000', tiUtils.tiIntToCommaStr( 1000000 ), 'Failed on 1000000' ) ;
  CheckEquals( '-10', tiUtils.tiIntToCommaStr( -10 ), 'Failed on -10' ) ;
  CheckEquals( '-100', tiUtils.tiIntToCommaStr( -100 ), 'Failed on -100' ) ;
  CheckEquals( '-1,000', tiUtils.tiIntToCommaStr( -1000 ), 'Failed on -1000' ) ;
  CheckEquals( '-10,000', tiUtils.tiIntToCommaStr( -10000 ), 'Failed on -10000' ) ;
  CheckEquals( '-100,000', tiUtils.tiIntToCommaStr( -100000 ), 'Failed on -100000' ) ;
  CheckEquals( '-1,000,000', tiUtils.tiIntToCommaStr( -1000000 ), 'Failed on -1000000' ) ;
end ;

procedure TTestTIUtils.tiFloatToCurrencyHide0 ;
begin
  CheckEquals( tiUtils.tiFloatToCurrencyHide0( 0 ),       '' ) ;
  CheckEquals( tiUtils.tiFloatToCurrencyHide0( 0.01 ),    '$ 0.01' ) ;
  CheckEquals( tiUtils.tiFloatToCurrencyHide0( 0.001 ),   '' ) ;
  CheckEquals( tiUtils.tiFloatToCurrencyHide0( 0.005 ),   '$ 0.01' ) ;
  CheckEquals( tiUtils.tiFloatToCurrencyHide0( 100 ),     '$ 100.00' ) ;
  CheckEquals( tiUtils.tiFloatToCurrencyHide0( 1000 ),    '$ 1,000.00' ) ;
  CheckEquals( tiUtils.tiFloatToCurrencyHide0( 1000000 ), '$ 1,000,000.00' ) ;
end ;

procedure TTestTIUtils.tiFloatToCurrency ;
begin
  CheckEquals( tiUtils.tiFloatToCurrency( 0 ),       '$ 0.00' ) ;
  CheckEquals( tiUtils.tiFloatToCurrency( 0.01 ),    '$ 0.01' ) ;
  CheckEquals( tiUtils.tiFloatToCurrency( 0.001 ),   '$ 0.00' ) ;
  CheckEquals( tiUtils.tiFloatToCurrency( 0.005 ),   '$ 0.01' ) ;
  CheckEquals( tiUtils.tiFloatToCurrency( 100 ),     '$ 100.00' ) ;
  CheckEquals( tiUtils.tiFloatToCurrency( 1000 ),    '$ 1,000.00' ) ;
  CheckEquals( tiUtils.tiFloatToCurrency( 1000000 ), '$ 1,000,000.00' ) ;
end ;

procedure TTestTIUtils.tiBooleanToStr ;
begin
  Check( SameText( tiUtils.tiBooleanToStr( true ), cTrue ), 'Failed on <true>' ) ;
  Check( SameText( tiUtils.tiBooleanToStr( false ), cFalse ), 'Failed on <false>' ) ;
end ;

procedure TTestTIUtils.tiStrToBool ;
begin                                    
  CheckEquals( false, tiUtils.tiStrToBool( 'false' ), 'Failed on false' ) ;
  CheckEquals( false, tiUtils.tiStrToBool( 'False' ), 'Failed on False' ) ;
  CheckEquals( true,  tiUtils.tiStrToBool( 'true' ),  'Failed on true' ) ;
  CheckEquals( true,  tiUtils.tiStrToBool( 'True' ),  'Failed on True' ) ;
  CheckEquals( true,  tiUtils.tiStrToBool( 't' ),     'Failed on t' ) ;
  CheckEquals( true,  tiUtils.tiStrToBool( 'T' ),     'Failed on T' ) ;
  CheckEquals( false, tiUtils.tiStrToBool( 'f' ),     'Failed on f' ) ;
  CheckEquals( false, tiUtils.tiStrToBool( 'F' ),     'Failed on F' ) ;
  CheckEquals( true,  tiUtils.tiStrToBool( '1' ),     'Failed on 1' ) ;
  CheckEquals( false, tiUtils.tiStrToBool( '0' ),     'Failed on 0' ) ;
  CheckEquals( false, tiUtils.tiStrToBool( 'AnyOtherStrings' ), 'Failed on AnyOtherString' ) ;
end ;

procedure TTestTIUtils.tiFloatToStr ;
begin
  Check( tiUtils.tiFloatToStr( 1,      0 ) = '1' ) ;
  Check( tiUtils.tiFloatToStr( 12,     0 ) = '12' ) ;
  Check( tiUtils.tiFloatToStr( 123,    0 ) = '123' ) ;
  Check( tiUtils.tiFloatToStr( 1234,   0 ) = '1234' ) ;
  Check( tiUtils.tiFloatToStr( 1.1,    0 ) = '1' ) ;
  Check( tiUtils.tiFloatToStr( 12.1,   0 ) = '12' ) ;
  Check( tiUtils.tiFloatToStr( 123.1,  0 ) = '123' ) ;
  Check( tiUtils.tiFloatToStr( 1234.1, 0 ) = '1234' ) ;
  Check( tiUtils.tiFloatToStr( 1,      1 ) = '1.0' ) ;
  Check( tiUtils.tiFloatToStr( 12,     1 ) = '12.0' ) ;
  Check( tiUtils.tiFloatToStr( 123,    1 ) = '123.0' ) ;
  Check( tiUtils.tiFloatToStr( 1234,   1 ) = '1234.0' ) ;
  Check( tiUtils.tiFloatToStr( 1.1,    1 ) = '1.1' ) ;
  Check( tiUtils.tiFloatToStr( 12.1,   1 ) = '12.1' ) ;
  Check( tiUtils.tiFloatToStr( 123.1,  1 ) = '123.1' ) ;
  Check( tiUtils.tiFloatToStr( 1234.1, 1 ) = '1234.1' ) ;
  Check( tiUtils.tiFloatToStr( 1,      2 ) = '1.00' ) ;
  Check( tiUtils.tiFloatToStr( 12,     2 ) = '12.00' ) ;
  Check( tiUtils.tiFloatToStr( 123,    2 ) = '123.00' ) ;
  Check( tiUtils.tiFloatToStr( 1234,   2 ) = '1234.00' ) ;
  Check( tiUtils.tiFloatToStr( 1.1,    2 ) = '1.10' ) ;
  Check( tiUtils.tiFloatToStr( 12.1,   2 ) = '12.10' ) ;
  Check( tiUtils.tiFloatToStr( 123.1,  2 ) = '123.10' ) ;
  Check( tiUtils.tiFloatToStr( 1234.1, 2 ) = '1234.10' ) ;
  Check( tiUtils.tiFloatToStr( 1.14,   1 ) = '1.1' ) ;
  Check( tiUtils.tiFloatToStr( 12.15,  1 ) = '12.2' ) ;
end ;

procedure TTestTIUtils.tiFloatToCommaStr ;
begin
  Check( tiUtils.tiFloatToCommaStr( 1,      0 ) = '1' ) ;
  Check( tiUtils.tiFloatToCommaStr( 12,     0 ) = '12' ) ;
  Check( tiUtils.tiFloatToCommaStr( 123,    0 ) = '123' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1234,   0 ) = '1,234' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1.1,    0 ) = '1' ) ;
  Check( tiUtils.tiFloatToCommaStr( 12.1,   0 ) = '12' ) ;
  Check( tiUtils.tiFloatToCommaStr( 123.1,  0 ) = '123' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1234.1, 0 ) = '1,234' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1,      1 ) = '1.0' ) ;
  Check( tiUtils.tiFloatToCommaStr( 12,     1 ) = '12.0' ) ;
  Check( tiUtils.tiFloatToCommaStr( 123,    1 ) = '123.0' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1234,   1 ) = '1,234.0' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1.1,    1 ) = '1.1' ) ;
  Check( tiUtils.tiFloatToCommaStr( 12.1,   1 ) = '12.1' ) ;
  Check( tiUtils.tiFloatToCommaStr( 123.1,  1 ) = '123.1' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1234.1, 1 ) = '1,234.1' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1,      2 ) = '1.00' ) ;
  Check( tiUtils.tiFloatToCommaStr( 12,     2 ) = '12.00' ) ;
  Check( tiUtils.tiFloatToCommaStr( 123,    2 ) = '123.00' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1234,   2 ) = '1,234.00' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1.1,    2 ) = '1.10' ) ;
  Check( tiUtils.tiFloatToCommaStr( 12.1,   2 ) = '12.10' ) ;
  Check( tiUtils.tiFloatToCommaStr( 123.1,  2 ) = '123.10' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1234.1, 2 ) = '1,234.10' ) ;
  Check( tiUtils.tiFloatToCommaStr( 1.14,   1 ) = '1.1' ) ;
  Check( tiUtils.tiFloatToCommaStr( 12.15,  1 ) = '12.2' ) ;
end ;

procedure TTestTIUtils._tiFloatToStr ;
begin
  Check( tiUtils._tiFloatToStr( 1,      0, '###0' ) = '1' ) ;
  Check( tiUtils._tiFloatToStr( 12,     0, '###0' ) = '12' ) ;
  Check( tiUtils._tiFloatToStr( 123,    0, '###0' ) = '123' ) ;
  Check( tiUtils._tiFloatToStr( 1234,   0, '###0' ) = '1234' ) ;
  Check( tiUtils._tiFloatToStr( 1.1,    0, '###0' ) = '1' ) ;
  Check( tiUtils._tiFloatToStr( 12.1,   0, '###0' ) = '12' ) ;
  Check( tiUtils._tiFloatToStr( 123.1,  0, '###0' ) = '123' ) ;
  Check( tiUtils._tiFloatToStr( 1234.1, 0, '###0' ) = '1234' ) ;
  Check( tiUtils._tiFloatToStr( 1,      1, '###0' ) = '1.0' ) ;
  Check( tiUtils._tiFloatToStr( 12,     1, '###0' ) = '12.0' ) ;
  Check( tiUtils._tiFloatToStr( 123,    1, '###0' ) = '123.0' ) ;
  Check( tiUtils._tiFloatToStr( 1234,   1, '###0' ) = '1234.0' ) ;
  Check( tiUtils._tiFloatToStr( 1.1,    1, '###0' ) = '1.1' ) ;
  Check( tiUtils._tiFloatToStr( 12.1,   1, '###0' ) = '12.1' ) ;
  Check( tiUtils._tiFloatToStr( 123.1,  1, '###0' ) = '123.1' ) ;
  Check( tiUtils._tiFloatToStr( 1234.1, 1, '###0' ) = '1234.1' ) ;
  Check( tiUtils._tiFloatToStr( 1,      2, '###0' ) = '1.00' ) ;
  Check( tiUtils._tiFloatToStr( 12,     2, '###0' ) = '12.00' ) ;
  Check( tiUtils._tiFloatToStr( 123,    2, '###0' ) = '123.00' ) ;
  Check( tiUtils._tiFloatToStr( 1234,   2, '###0' ) = '1234.00' ) ;
  Check( tiUtils._tiFloatToStr( 1.1,    2, '###0' ) = '1.10' ) ;
  Check( tiUtils._tiFloatToStr( 12.1,   2, '###0' ) = '12.10' ) ;
  Check( tiUtils._tiFloatToStr( 123.1,  2, '###0' ) = '123.10' ) ;
  Check( tiUtils._tiFloatToStr( 1234.1, 2, '###0' ) = '1234.10' ) ;
  Check( tiUtils._tiFloatToStr( 1.14,   1, '###0' ) = '1.1' ) ;
  Check( tiUtils._tiFloatToStr( 12.15,  1, '###0' ) = '12.2' ) ;
end ;


  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  // *
  // *  Win32 API wrappers
  // *
  // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

procedure TTestTIUtils.tiGetUserName  ;
begin
  CheckReadingFromNT(
    tiUtils.tiGetUserName,
    'UserName',
    'User''s name'
   ) ;
end ;

procedure TTestTIUtils.tiGetComputerName  ;
begin
  CheckReadingFromNT(
    tiUtils.tiGetComputerName,
    'ComputerName',
    'name'
   ) ;
end ;

// This test is really inadequate, but then tiVariantArrayToString should be
// re-worked to remove the TStringList as the working holder of the data.
procedure TTestTIUtils.tiVariantArrayToString ;
var
  lFrom : OleVariant ;
  lTo   : string ;
const
  CrLf = #13 + #10 ;
begin
  lFrom := VarArrayOf(['xxx']);
  lTo :=
    '[' + CrLf +
    ' ' + 'xxx' +
    CrLf + ']' +
    CrLf ;

  CheckEquals( lTo, tiUtils.tiVariantArrayToString( lFrom ), 'Failed on 1 ('+IntToStr(VarType(lFrom))+')' ) ;

  lFrom := VarArrayOf(['xxx', 'yyy']);
  lTo :=
    '[' + CrLf +
    ' ' + 'xxx' + CrLf +
    ' ' + 'yyy' + CrLf +
    ']' + CrLf ;
  CheckEquals( lTo, tiUtils.tiVariantArrayToString( lFrom ), 'Failed on 2' ) ;

  lFrom := VarArrayOf(['xxx', 'yyy', 'zzz' ]);
  lTo :=
    '[' + CrLf +
    ' ' + 'xxx' + CrLf +
    ' ' + 'yyy' + CrLf +
    ' ' + 'zzz' + CrLf +
    ']' + CrLf ;
  CheckEquals( lTo, tiUtils.tiVariantArrayToString( lFrom ), 'Failed on 3' ) ;

  lFrom := VarArrayOf(['xxx', VarArrayOf(['yyy'])]);
  lTo :=
    '[' + CrLf +
    ' ' + 'xxx' + CrLf +
    '   [' + CrLf +
    '    ' + 'yyy' + CrLf +
    '   ]' + CrLf +
    ']' + CrLf ;

  CheckEquals( lTo, tiUtils.tiVariantArrayToString( lFrom ), 'Failed on 3' ) ;

  lFrom := VarArrayOf(['xxx', VarArrayOf(['yyy', 'zzz'])]);
  lTo :=
    '[' + CrLf +
    ' ' + 'xxx' + CrLf +
    '   [' + CrLf +
    '    ' + 'yyy' + CrLf +
    '    ' + 'zzz' + CrLf +
    '   ]' + CrLf +
    ']' + CrLf ;

  CheckEquals( lTo, tiUtils.tiVariantArrayToString( lFrom ), 'Failed on 3' ) ;

//  tiShowString( '|' +
//    tiUtils.tiVariantArrayToString( lFrom ) +
//    '|' + CrLf + CrLf +
//    '|' + lTo + '|' ) ;

end ;

procedure TTestTIUtils.tiIsVariantOfType ;

  procedure _tiIsVariantOfType(xVar : variant; xExpected : TVarType; xMsg : string);

    procedure __tiIsVariantOfType(xxCheck : TVarType; xxMsg : string);
    begin
      if xxCheck=xExpected then
        Check( tiUtils.tiIsVariantOfType( xVar, xxCheck ), xMsg )
      else
        Check( not tiUtils.tiIsVariantOfType( xVar, xxCheck ), xMsg + ' - '+xxMsg);
    end;

  begin
    __tiIsVariantOfType(varEmpty,'varEmpty');
    __tiIsVariantOfType(varNull,'varNull');
    __tiIsVariantOfType(varSmallint,'varSmallInt');
    __tiIsVariantOfType(varInteger,'varInteger');
    __tiIsVariantOfType(varSingle,'varSingle');
    __tiIsVariantOfType(varDouble,'varDouble');
    __tiIsVariantOfType(varDate,'varDate');
    __tiIsVariantOfType(varBoolean,'varBoolean');
    __tiIsVariantOfType(varOleStr,'varOleStr');
  end;
var
  lVar : OleVariant ;
//  lShortString : Char ;
  lSmallInt : Smallint;
  lInteger : Integer;
  lDouble : Double;      
  lDateTimeNow : TDateTime;
  lDateTimeDate : TDateTime;
  lOleString : WideString;
  lString : string;
  lBoolean : boolean;
  lCurrency : Currency;
begin
  lSmallInt := 123;
  lInteger := High(Integer);
  lDouble := 123.45678901234567890;
  lDateTimeNow := Now;
  lDateTimeDate := Date;
  lOleString := 'OLE STRING TEST';
  lString := 'STRING TEST';
  lBoolean := true;
  lCurrency := 12345678.9876;

  lVar := VarAsType('',varEmpty);
  _tiIsVariantOfType(lVar,varEmpty,'Failed with varEmpty');
//  Check( tiUtils.tiIsVariantOfType( lVar, varEmpty ), 'Failed with varEmpty' ) ;

  lVar := Null ;
  _tiIsVariantOfType(lVar,varNull,'Failed with varNull');
//  Check( tiUtils.tiIsVariantOfType( lVar, varNull ), 'Failed with varNull' ) ;

//  lVar := High( SmallInt ) ;
  // There is no other way to receive variant of type small int...
  lVar:=VarAsType(lSmallInt,varSmallint);
  _tiIsVariantOfType(lVar,varSmallInt,'Failed with VarSmallint');
//  Check( tiUtils.tiIsVariantOfType( lVar, VarSmallint ), 'Failed with VarSmallint' ) ;

//  lVar := High( Integer ) ;
  lVar:=lInteger;
  _tiIsVariantOfType(lVar,varInteger,'Failed with Integer');
//  Check( tiUtils.tiIsVariantOfType( lVar, varInteger ), 'Failed with varInteger' ) ;

//  lVar :=  123.456;
  lVar:=VarAsType(123.456,varSingle);
  _tiIsVariantOfType(lVar,varSingle,'Failed with VarSingle');
//  Check( tiUtils.tiIsVariantOfType( lVar, varSingle ), 'Failed with varSingle' ) ;

//  lVar := 123.456 ;
//  Check( tiUtils.tiIsVariantOfType( lVar, varD ), 'Failed with varSingle' ) ;

//  lVar := 123.45678901234567890 ;
  lVar:=lDouble;
  _tiIsVariantOfType(lVar,varDouble,'Failed with VarDouble');
//  Check( tiUtils.tiIsVariantOfType( lVar, varDouble ), 'Failed with varDouble' ) ;

//  lVar := Date ;
  lVar:=lDateTimeDate;
  _tiIsVariantOfType(lVar,varDate,'Failed with varDate - DATE');
//  Check( tiUtils.tiIsVariantOfType( lVar, varDate ), 'Failed with varDate' ) ;

//  lVar := Now ;
  lVar:=lDateTimeNow;
  _tiIsVariantOfType(lVar,varDate,'Failed with varDate - NOW');
  Check( tiUtils.tiIsVariantOfType( lVar, varDate ), 'Failed with varDate' ) ;

//  lVar := False ;
  lVar:=lBoolean;
  _tiIsVariantOfType(lVar,varBoolean,'Failed with varBoolean');
//  Check( tiUtils.tiIsVariantOfType( lVar, varBoolean ), 'Failed with varBoolean' ) ;

//  lVar := 'test' ;
  lVar:=lOleString;
  _tiIsVariantOfType(lVar,varOLEStr,'Failed with varOLEStr');
//  Check( tiUtils.tiIsVariantOfType( lVar, varOLEStr ), 'Failed with varOLEStr' ) ;

// Can't make this one work
//  lVar := 'test' ;
//  Check( tiUtils.tiIsVariantOfType( lVar, varString ), 'Failed with varString' ) ;

  lVar:=lCurrency;
  _tiIsVariantOfType(lVar,varCurrency,'Failed with varCurrency');
//  Check( tiUtils.tiIsVariantOfType( lVar, varCurrency ), 'Failed with varCurrency' ) ;

// These ones have not been tested
// varCurrency	Currency floating-point value (type Currency).
// varDispatch	Reference to an Automation object (an IDispatch interface pointer).
// varError	Operating system error code.
// varUnknown	Reference to an unknown COM object (an IUnknown interface pointer).
// varByte	8-bit unsigned integer (type Byte).
// varTypeMask	Bit mask for extracting type code.
// varArray	Bit indicating variant array.
// varByRef	Bit indicating variant contains a reference (rather than a value).

end ;

procedure TTestTIUtils.Lf ;
begin
  Check( tiUtils.Lf = #10 ) ;
  Check( tiUtils.Lf( 1 ) = #10 ) ;
  Check( tiUtils.Lf( 2 ) = #10 + #10 ) ;
  Check( tiUtils.Lf( 3 ) = #10 + #10 + #10 ) ;
  Check( tiUtils.Lf( 4 ) = #10 + #10 + #10 + #10 ) ;
  Check( tiUtils.Lf( 5 ) = #10 + #10 + #10 + #10 + #10 ) ;
end ;

procedure TTestTIUtils.Cr ;
begin
  Check( tiUtils.Cr = #13 ) ;
  Check( tiUtils.Cr( 1 ) = #13 ) ;
  Check( tiUtils.Cr( 2 ) = #13 + #13 ) ;
  Check( tiUtils.Cr( 3 ) = #13 + #13 + #13 ) ;
  Check( tiUtils.Cr( 4 ) = #13 + #13 + #13 + #13 ) ;
  Check( tiUtils.Cr( 5 ) = #13 + #13 + #13 + #13 + #13 ) ;
end ;

procedure TTestTIUtils.CrLf ;
begin
  Check( tiUtils.CrLF = #13 + #10 ) ;
  Check( tiUtils.CrLF( 1 ) = #13 + #10 ) ;
  Check( tiUtils.CrLF( 2 ) = #13 + #10 + #13 + #10 ) ;
  Check( tiUtils.CrLF( 3 ) = #13 + #10 + #13 + #10 + #13 + #10 ) ;
  Check( tiUtils.CrLF( 4 ) = #13 + #10 + #13 + #10 + #13 + #10 + #13 + #10 ) ;
  Check( tiUtils.CrLF( 5 ) = #13 + #10 + #13 + #10 + #13 + #10 + #13 + #10 + #13 + #10 ) ;
end ;

procedure TTestTIUtils.tiListToStream ;
var
  lStream : TStringStream ;
  lList   : TTestListOfPersistents ;
begin
  lStream := TStringStream.Create( '' ) ;
  try
    lList   := TTestListOfPersistents.Create ;
    try
      tiUtils.tiListToStream( lStream, lList, ',' ) ;
      CheckEquals( Integer(Length( lList.AsString )), Integer(lStream.Size )) ;
      CheckEquals( lList.AsString, lStream.DataString ) ;
    finally
      lList.Free ;
    end ;
  finally
    lStream.Free;
  end ;
end ;


procedure TTestTIUtils.tiListToCSV ;
var
  lFileStream : TFileStream ;
  lStringStream : TStringStream ;
  lList       : TTestListOfPersistents ;
  lStringFrom     : string ;
  lStringTo : string ;
  lFileName : string ;
begin
  lFileName := TempFileName( 'DUnitTest.txt' ) ;
  lList   := TTestListOfPersistents.Create ;
  try
    // ToDo: tiUtils.tiListToCSV could really do with more testing on the optional parameters.
    tiUtils.tiListToCSV( lList, nil, lFileName, false ) ;
    lStringFrom := lList.AsString ;
  finally
    lList.Free ;
  end ;

  lFileStream := TFileStream.Create( lFileName,
                                     fmOpenReadWrite or fmShareDenyNone ) ;
  try
    lStringStream := TStringStream.Create('');
    try
      lStringStream.CopyFrom(lFileStream,lFileStream.Size);
      lStringTo := lStringStream.DataString ;
    finally
      lStringStream.Free ;
    end;
  finally
    lFileStream.Free ;
  end ;
  CheckEquals( lStringFrom, lStringTo ) ;
  SysUtils.DeleteFile(lFileName);
end ;


procedure TTestTIUtils.tiListToClipboard ;
var
  lList   : TTestListOfPersistents ;
  lFrom   : string ;
  lTo     : string ;
  i : integer ;
begin
  lList   := TTestListOfPersistents.Create ;
  try
    tiUtils.tiListToClipboard( lList, nil, false ) ;
    lFrom := lList.AsString( #9 ) ;
    lTo   := Clipboard.AsText ;
    if Length(lFrom) <> Length(lTo) then
      Fail( Format( 'Lengths not the same: Expected <%s> Found: <%s>',
                    [lFrom, lTo])) ;
    for i := 1 to Length( lFrom ) do
      CheckEquals( lFrom[i], lTo[i], 'Char(' + IntToStr(i)+') missmatch' ) ;
  finally
    lList.Free ;
  end ;
end ;

// DUnit
procedure TTestTIUtils.tiFmtException ;
const
  cExceptionMessage = 'A test exception message' ;
  cClassName        = 'TTestTIUtils' ;
  cMethodName       = 'tiFmtException' ;
  cDetails          = 'Test excetpion details. bla, bla, bla' ;

  function _FormatException( const pMessage : string ;
                             const pClassName : string ;
                             const pMethodName : string ) : string ;
  begin
    result :=
      cgExceptionMessageStart + #13 +
      pMessage + #13 +
      cgExceptionMessageEnd + #13 + #13 +
      cgExceptionCallStackStart + #13 +
      pClassName + '.' + pMethodName + #13 +
      cgExceptionCallstackEnd ;
  end ;

const
  cFormattedMessage_1 =
      cgExceptionMessageStart   + #13 +
      cExceptionMessage         + '_2' + #13 +
      cExceptionMessage         + '_1' + #13 +
      cgExceptionMessageEnd     + #13 + #13 +
      cgExceptionCallStackStart + #13 +
      cClassName + '_2.' + cMethodName + '_2' + #13 +
      cClassName + '_1.' + cMethodName + '_1' + #13 +
      cgExceptionCallstackEnd ;

begin

  try
    tiUtils.tiFmtException( cExceptionMessage ) ;
  except
    on E:Exception do
    begin
      CheckIs( E, Exception ) ;
      CheckEquals( _FormatException( cExceptionMessage, '', '' ), e.message,
                   'Failed with 1 parameter. Expected ' ) ;
    end ;
  end ;

  try
    tiUtils.tiFmtException( cExceptionMessage, cClassName ) ;
  except
    on E:Exception do
    begin
      CheckIs( E, Exception ) ;
      CheckEquals( _FormatException( cExceptionMessage, cClassName, '' ),
                   e.message, 'Failed with 2 parameters'  ) ;
    end ;
  end ;

  try
    tiUtils.tiFmtException( cExceptionMessage, cClassName, cMethodName ) ;
  except
    on E:Exception do
    begin
      CheckIs( E, Exception ) ;
      CheckEquals( _FormatException( cExceptionMessage, cClassName, cMethodName),
                   e.message, 'Failed with 3 parameters' ) ;
    end ;
  end ;

  try
    tiUtils.tiFmtException( cExceptionMessage, cClassName, cMethodName ) ;
  except
    on E:Exception do
    begin
      CheckIs( E, Exception ) ;
      CheckEquals( _FormatException( cExceptionMessage, cClassName, cMethodName ),
                   e.message, 'Failed with 4 parameters' ) ;
    end ;
  end ;

  try
    try
      tiUtils.tiFmtException( cExceptionMessage + '_1',
                              cClassName        + '_1',
                              cMethodName       + '_1' ) ;
    except
      on E:Exception do
        tiUtils.tiFmtException( e, cExceptionMessage + '_2',
                                cClassName + '_2', cMethodName + '_2') ;
    end ;
  except
    on E:Exception do
    begin
      CheckIs( E, Exception ) ;
      CheckEquals( cFormattedMessage_1, e.message, 'Failed with 1 nested message' ) ;
    end ;
  end ;
end ;

type
  TTestGetPropNames = class( TTestGetPropNamesAbs )
  published
    property BoolProp ;
    property CharProp ;
    property DateTimeProp ;
    property FloatProp ;
    property Int64Prop ;
    property IntProp ;
    property MethodProp ;
    property ObjectProp ;
    property ReadOnlyBoolProp ;
    property ReadOnlyCharProp ;
    property ReadOnlyDateTimeProp ;
    property ReadOnlyFloatProp ;
    property ReadOnlyInt64Prop ;
    property ReadOnlyIntProp ;
    property ReadOnlyMethodProp ;
    property ReadOnlyObjectProp ;
    property ReadOnlyShortStringProp ;
    property ReadOnlyStringProp ;
    property ReadOnlyWideCharProp ;
    property ReadOnlyWideStringProp ;
    property ShortStringProp ;
    property StringProp ;
    property WideCharProp ;
    property WideStringProp ;
  end ;


procedure TTestTIUtils.tiGetSimplePropType ;
var
  lObj : TTestGetPropNames ;
begin
  lObj := TTestGetPropNames.Create ;
  try

    Check( tiUtils.tiGetSimplePropType( lObj, 'StringProp' )      = tiTKString, 'Failed on StringProp' ) ;
    Check( tiUtils.tiGetSimplePropType( lObj, 'ShortStringProp' ) = tiTKString, 'Failed on ShortStringProp' ) ;
    Check( tiUtils.tiGetSimplePropType( lObj, 'WideStringProp' )  = tiTKString, 'Failed on WideStringProp' ) ;
    Check( tiUtils.tiGetSimplePropType( lObj, 'CharProp' )        = tiTKString, 'Failed on CharProp' ) ;
    Check( tiUtils.tiGetSimplePropType( lObj, 'WideCharProp' )    = tiTKString, 'Failed on WideCharProp' ) ;

    Check( tiUtils.tiGetSimplePropType( lObj, 'IntProp' )         = tiTKInteger, 'Failed on IntProp' ) ;
    Check( tiUtils.tiGetSimplePropType( lObj, 'Int64Prop' )       = tiTKInteger, 'Failed on Int64Prop' ) ;
    Check( tiUtils.tiGetSimplePropType( lObj, 'BoolProp' )        = tiTKBoolean, 'Failed on BoolProp' ) ;

    Check( tiUtils.tiGetSimplePropType( lObj, 'FloatProp' )       = tiTKFloat, 'Failed on FloatProp' ) ;
    Check( tiUtils.tiGetSimplePropType( lObj, 'DateTimeProp' )    = tiTKDateTime, 'Failed on DateTimeProp' ) ;

    try
      tiUtils.tiGetSimplePropType( lObj, 'ObjectProp' ) ;
      Check( false, 'Failed on ObjectProp' ) ;
    except
      on e:exception do
        CheckIs( e, Exception, 'Failed on ObjectProp' ) ;
    end ;

    try
      tiUtils.tiGetSimplePropType( lObj, 'MethodProp' ) ;
      Check( false, 'Failed on MethodProp' ) ;
    except
      on e:exception do
        CheckIs( e, Exception, 'Failed on MethodProp' ) ;
    end ;

  finally
    lObj.Free;
  end ;
end ;


procedure TTestTIUtils.tiIsNumericProp ;
var
  lObj : TTestGetPropNames ;
begin
  lObj := TTestGetPropNames.Create ;
  try
    Check( not tiUtils.tiIsNumericProp( lObj, 'StringProp'      ), 'Failed on StringProp' ) ;
    Check( not tiUtils.tiIsNumericProp( lObj, 'ShortStringProp' ), 'Failed on ShortStringProp' ) ;
    Check( not tiUtils.tiIsNumericProp( lObj, 'WideStringProp'  ), 'Failed on WideStringProp' ) ;
    Check( not tiUtils.tiIsNumericProp( lObj, 'CharProp'        ), 'Failed on CharProp' ) ;
    Check( not tiUtils.tiIsNumericProp( lObj, 'WideCharProp'    ), 'Failed on WideCharProp' ) ;
    Check(     tiUtils.tiIsNumericProp( lObj, 'IntProp'         ), 'Failed on IntProp' ) ;
    Check(     tiUtils.tiIsNumericProp( lObj, 'Int64Prop'       ), 'Failed on Int64Prop' ) ;
    Check(     tiUtils.tiIsNumericProp( lObj, 'DateTimeProp'    ), 'Failed on DateTimeProp' ) ;
    Check(     tiUtils.tiIsNumericProp( lObj, 'FloatProp'       ), 'Failed on FloatProp' ) ;
    Check( not tiUtils.tiIsNumericProp( lObj, 'ObjectProp'      ), 'Failed on ObjectProp' ) ;
    Check( not tiUtils.tiIsNumericProp( lObj, 'MethodProp'      ), 'Failed on MethodProp' ) ;
  finally
    lObj.Free ;
  end ;
end ;

procedure TTestTIUtils.tiGetPropertyNames ;
var
  lsl : TStringList ;
  lObj : TPersistent ;
begin
  lsl := TStringList.Create ;
  try
    lObj := TTestGetPropNames.Create ;
    try
      tiUtils.tiGetPropertyNames( lObj, lsl, [ tkLString ] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on StringProp' ) ;
      CheckEquals( 'ReadOnlyStringProp', lsl.Strings[0], 'Failed on ReadOnlyStringProp' ) ;
      CheckEquals( 'StringProp', lsl.Strings[1], 'Failed on StringProp' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [ tkString] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on ShortStringProp' ) ;
      CheckEquals( 'ReadOnlyShortStringProp', lsl.Strings[0], 'Failed on ReadOnlyShortStringProp' ) ;
      CheckEquals( 'ShortStringProp', lsl.Strings[1], 'Failed on ShortStringProp' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [ tkWString ] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on WideStringProp' ) ;
      CheckEquals( 'ReadOnlyWideStringProp', lsl.Strings[0], 'Failed on ReadOnlyWideStringProp' ) ;
      CheckEquals( 'WideStringProp', lsl.Strings[1], 'Failed on WideStringProp' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [tkChar] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on CharProp' ) ;
      CheckEquals( 'CharProp', lsl.Strings[0], 'Failed on CharProp' ) ;
      CheckEquals( 'ReadOnlyCharProp', lsl.Strings[1], 'Failed on ReadOnlyCharProp' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [tkWChar] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on WideCharProp' ) ;
      CheckEquals( 'ReadOnlyWideCharProp', lsl.Strings[0], 'Failed on ReadOnlyWideCharProp' ) ;
      CheckEquals( 'WideCharProp', lsl.Strings[1], 'Failed on WideCharProp' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, ctkString ) ;
      CheckEquals( 10, lsl.Count, 'Failed testing ctkString' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [tkInteger] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on IntProp' ) ;
      CheckEquals( 'IntProp', lsl.Strings[0], 'Failed on IntProp' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [tkInt64] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on Int64Prop' ) ;
      CheckEquals( 'Int64Prop', lsl.Strings[0], 'Failed on Int64Prop' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, ctkInt ) ;
      CheckEquals( 4, lsl.Count, 'Failed testing ctkInt' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [tkFloat] ) ;
      CheckEquals( 4, lsl.Count, 'Failed on tkFloatProp' ) ;
      CheckEquals( 'DateTimeProp', lsl.Strings[0], 'Failed on tkFloatProp' ) ;
      CheckEquals( 'FloatProp',    lsl.Strings[1], 'Failed on tkFloatProp' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, ctkFloat ) ;
      CheckEquals( 4, lsl.Count, 'Failed testing ctkFloat' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, ctkNumeric ) ;
      CheckEquals( 8, lsl.Count, 'Failed testing ctkNumeric' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, ctkSimple ) ;
      CheckEquals( 18, lsl.Count, 'Failed testing ctkSimple' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [tkClass] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on ObjectProp' ) ;
      CheckEquals( 'ObjectProp', lsl.Strings[0], 'Failed on ObjectProp' ) ;

      tiUtils.tiGetPropertyNames( lObj, lsl, [tkMethod] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on MethodProp' ) ;
      CheckEquals( 'MethodProp', lsl.Strings[0], 'Failed on MethodProp' ) ;

    finally
      lObj.Free ;
    end ;

  finally
    lsl.Free ;
  end ;
end ;

procedure TTestTIUtils.tiIsReadWriteProp ;
var
  lObj : TTestGetPropNames ;
begin
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyStringProp' ),      'Failed on ReadOnlyStringProp'       ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyShortStringProp' ), 'Failed on ReadOnlyShortStringProp'  ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyWideStringProp' ),  'Failed on ReadOnlyWideStringProp '  ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyCharProp' ),        'Failed on ReadOnlyCharProp'         ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyWideCharProp' ),    'Failed on ReadOnlyWideCharProp'     ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyIntProp' ),         'Failed on ReadOnlyIntProp'          ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyInt64Prop' ),       'Failed on ReadOnlyInt64Prop'        ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyDateTimeProp' ),    'Failed on ReadOnlyDateTimeProp'     ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyFloatProp' ),       'Failed on ReadOnlyFloatProp'        ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyObjectProp' ),      'Failed on ReadOnlyObjectProp'       ) ;
  Check( not tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyMethodProp' ),      'Failed on ReadOnlyMethodProp'       ) ;

  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'StringProp' ),  'Failed on OnlyStringProp'    ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ShortStringProp' ), 'Failed on ShortStringProp'   ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'WideStringProp' ),  'Failed on WideStringProp '   ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'CharProp' ),        'Failed on CharProp'          ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'WideCharProp' ),    'Failed on WideCharProp'      ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'IntProp' ),         'Failed on IntProp'           ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'Int64Prop' ),       'Failed on Int64Prop'         ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'DateTimeProp' ),    'Failed on DateTimeProp'      ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'FloatProp' ),       'Failed on FloatProp'         ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'ObjectProp' ),      'Failed on ObjectProp'        ) ;
  Check( tiUtils.tiIsReadWriteProp( TTestGetPropNames, 'MethodProp' ),      'Failed on MethodProp'        ) ;

  lObj := TTestGetPropNames.Create ;
  try

    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyStringProp' ),      'Failed on ReadOnlyStringProp'       ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyShortStringProp' ), 'Failed on ReadOnlyShortStringProp'  ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyWideStringProp' ),  'Failed on ReadOnlyWideStringProp '  ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyCharProp' ),        'Failed on ReadOnlyCharProp'         ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyWideCharProp' ),    'Failed on ReadOnlyWideCharProp'     ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyIntProp' ),         'Failed on ReadOnlyIntProp'          ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyInt64Prop' ),       'Failed on ReadOnlyInt64Prop'        ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyBoolProp' ),        'Failed on ReadOnlyBoolProp'         ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyDateTimeProp' ),    'Failed on ReadOnlyDateTimeProp'     ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyFloatProp' ),       'Failed on ReadOnlyFloatProp'        ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyObjectProp' ),      'Failed on ReadOnlyObjectProp'       ) ;
    Check( not tiUtils.tiIsReadWriteProp( lObj, 'ReadOnlyMethodProp' ),      'Failed on ReadOnlyMethodProp'       ) ;

    Check( tiUtils.tiIsReadWriteProp( lObj, 'StringProp' ),  'Failed on OnlyStringProp'    ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'ShortStringProp' ), 'Failed on ShortStringProp'   ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'WideStringProp' ),  'Failed on WideStringProp '   ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'CharProp' ),        'Failed on CharProp'          ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'WideCharProp' ),    'Failed on WideCharProp'      ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'IntProp' ),         'Failed on IntProp'           ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'Int64Prop' ),       'Failed on Int64Prop'         ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'BoolProp' ),        'Failed on BoolProp'          ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'DateTimeProp' ),    'Failed on DateTimeProp'      ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'FloatProp' ),       'Failed on FloatProp'         ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'ObjectProp' ),      'Failed on ObjectProp'        ) ;
    Check( tiUtils.tiIsReadWriteProp( lObj, 'MethodProp' ),      'Failed on MethodProp'        ) ;

  finally
    lObj.Free ;
  end ;

end ;

function TTestTIUtils.BuildLongString: string;
var
  i, j : integer ;
begin
  result := ''  ;
  for i := 0 to 1000 do
  begin
    for j := 32 to 248 do
      result := result + Chr( j ) ;
    result := result + #13 ;
  end ;
end;

type
{$M+}
  TCheckRTTI_1 = class( TObject ) ;
{$M-}

  TCheckRTTI_2 = class( TObject ) ;

procedure TTestTIUtils.tiHasRTTIOnClass;
begin
  Check( not tiHasRTTI( TObject ), 'tiHasRTTI( TObject ) <> false' ) ;
  Check( tiHasRTTI( TPersistent ), 'tiHasRTTI( TPersistent ) <> true' ) ;
  Check( tiHasRTTI( TCheckRTTI_1 ), 'tiHasRTTI( TCheckRTTI_1 ) <> true' ) ;
  Check( not tiHasRTTI( TCheckRTTI_2 ), 'iHasRTTI( TCheckRTTI_2 ) <> false' ) ;
end;

procedure TTestTIUtils.tiHasRTTIOnObject;
var
  lObj : TObject ;
begin
  lObj := TObject.Create ;
  Check( not tiHasRTTI( lObj ), 'tiHasRTTI( TObject ) <> false' ) ;
  lObj.Free ;

  lObj := TPersistent.Create ;
  Check( tiHasRTTI( lObj ), 'tiHasRTTI( TObject ) <> true' ) ;
  lObj.Free ;

  lObj := TCheckRTTI_1.Create ;
  Check( tiHasRTTI( lObj ), 'tiHasRTTI( TCheckRTTI_1 ) <> true' ) ;
  lObj.Free ;

  lObj := TCheckRTTI_2.Create ;
  Check( not tiHasRTTI( lObj ), 'iHasRTTI( TCheckRTTI_2 ) <> false' ) ;
  lObj.Free ;

end;

procedure TTestTIUtils.CheckReadingFromNT( const pValue, pRegKey, pDescription : string ) ;
var
  lEntered : string ;
begin
  {$IFDEF MSWINDOWS}
    lEntered := gDUnitReg.ReadString( 'MachineSettings', pRegKey, '' ) ;
  {$ELSE}
    lEntered := gDUnitINI.ReadString( 'MachineSettings', pRegKey, '' ) ;
  {$ENDIF}
  if lEntered = '' then
  begin
  {$IFDEF MSWINDOWS}
    gDUnitReg.WriteString( 'MachineSettings', pRegKey, '' ) ;
    Fail('Please enter this computer''s ' + pDescription +
         ' to the "\' + gDUnitReg.CurrentPath +
         '\MachineSettings\' + pRegKey +
         '" section of the registry' );
  {$ELSE}
    lEntered := gDUnitINI.WriteString( 'MachineSettings', pRegKey, '' ) ;
    Fail('Please enter this computer''s ' + pDescription +
         ' in the ' + '[MachineSettings] "' + pRegKey +
         '" section of ' + gDUnitINI.FileName );
  {$ENDIF}
  end ;
  CheckEquals( UpperCase( lEntered ),
               UpperCase( pValue ), 'Confirm this computer''s ' + pDescription + ' ' + lEntered ) ;
end;

procedure TTestTIUtils.tiForceRemoveDir;
var
  lRoot : string ;
begin
  lRoot := TempFileName( 'DUnitTests\ForceRemoveDir' ) ;
  ForceDirectories( lRoot ) ;
  tiCreateTextFileOfSize( lRoot + '\file1.txt', 100 ) ;
  ForceDirectories( lRoot + '\dir1' ) ;
  tiCreateTextFileOfSize( lRoot + '\dir1\file2.txt', 100 ) ;
  tiUtils.tiSetFileReadOnly( lRoot + '\dir1\file2.txt', true ) ;
  ForceDirectories( lRoot + '\dir2' ) ;
  tiUtils.tiForceRemoveDir( lRoot ) ;
  Check( Not DirectoryExists( lRoot )) ;
  tiDUnitForceRemoveDir( lRoot ) ;

end;

procedure TTestTIUtils.tiIsEMailAddressValid;
begin
  Check( not tiUtils.tiIsEMailAddressValid( '' ), 'Failed on <>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'pwh' ), 'Failed on <pwh>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'pwh@' ), 'Failed on <pwh@>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'pwh@techinsite' ), 'Failed on <pwh@techinsite>' ) ;
  Check(     tiUtils.tiIsEMailAddressValid( 'pwh@techinsite.com' ), 'Failed on <pwh@techinsite.com>' ) ;
  Check(     tiUtils.tiIsEMailAddressValid( 'pwh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p(wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p)wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p<wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p>wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p@wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p,wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p;wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p:wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p/wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p/wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p"wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check(     tiUtils.tiIsEMailAddressValid( 'p.wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p[wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( 'p]wh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  Check( not tiUtils.tiIsEMailAddressValid( #127+'pwh@techinsite.com.au' ), 'Failed on <pwh@techinsite.com.au>' ) ;
  //ToDo: These tests could be more complete. For example, the
  //      name part of an email address has not been tested.
end;

procedure TTestTIUtils.tiIsFileNameValid;
begin
  Check( not tiUtils.tiIsFileNameValid( '' ), '<empty string>' ) ;
  Check( tiUtils.tiIsFileNameValid( 'a' ), 'a' ) ;
  Check( tiUtils.tiIsFileNameValid( tiUtils.tiReplicate( 'a', 255 )), tiUtils.tiReplicate( 'a', 255 ) ) ;
  Check( not tiUtils.tiIsFileNameValid( tiUtils.tiReplicate( 'a', 256 )), tiUtils.tiReplicate( 'a', 256 ) ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test\' ), 'test\' ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test/' ), 'test/' ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test:' ), 'test:' ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test*' ), 'test*' ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test?' ), 'test?' ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test"' ), 'test"' ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test>' ), 'test>' ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test<' ), 'test<' ) ;
  Check( not tiUtils.tiIsFileNameValid( 'test|' ), 'test|' ) ;
end;

procedure TTestTIUtils.tiCheckSum;
begin
  Check( tiUtils.tiCheckSum( '0' ) = 0, 'CheckSum of "0" <> 0' );
  Check( tiUtils.tiCheckSum( '01' ) = 9, 'CheckSum of "01" <> 9' );
  Check( tiUtils.tiCheckSum( '10' ) = 7, 'CheckSum of "10" <> 7' );
  // non-numeric chars should be treated as zero
  Check( tiUtils.tiCheckSum( '1A3' ) = 8, 'CheckSum of "1A3" <> 8' );
  Check( tiUtils.tiCheckSum( 'DEF67' ) = 3, 'CheckSum of "DEF67" <> 3' );
  Check( tiUtils.tiCheckSum( '08137919805' ) = 5, 'CheckSum of "08137919805" <> 5' );
end;

procedure TTestTIUtils.tiVarSimplePropType;
begin
  Check( tiUtils.tiVarSimplePropType( 'string' ) = tiTKString,   'Failed on tiTKString'   ) ;
  Check( tiUtils.tiVarSimplePropType( 123      ) = tiTKInteger,  'Failed on tiTKInteger'  ) ;
  Check( tiUtils.tiVarSimplePropType( true     ) = tiTKBoolean,  'Failed on tiTKBoolean'  ) ;
  Check( tiUtils.tiVarSimplePropType( 123.456  ) = tiTKFloat,    'Failed on tiTKFloat'    ) ;
  Check( tiUtils.tiVarSimplePropType( Now      ) = tiTKDateTime, 'Failed on tiTKDateTime' ) ;
end;

procedure TTestTIUtils.tiStreamToString;
var
  lStream : TStringStream ;
  lsFrom : string ;
  lsTo : string ;
begin
  lsFrom := tiCreateStringOfSize(3000);
  lStream := TStringStream.Create(lsFrom);
  try
    lsTo := tiUtils.tiStreamToString(lStream);
    CheckEquals(lsFrom, lsTo);
  finally
    lStream.Free;
  end ;
end;

procedure TTestTIUtils.tiStringToStream;
var
  lStream : TStringStream ;
  lsFrom : string ;
  lsTo : string ;
begin
  lsFrom := tiCreateStringOfSize(3000);
  lStream := TStringStream.Create('');
  try
    tiUtils.tiStringToStream(lsFrom, lStream);
    lsTo := lStream.DataString ;
    CheckEquals(lsFrom, lsTo);
  finally
    lStream.Free;
  end ;
end;

procedure TTestTIUtils.tiAppendStringToStream;
var
  lStream : TStringStream ;
  lsFrom : string ;
  lsTo : string ;
  lsStart: string;
begin
  lsStart := 'aaa' ;
  lsFrom := 'bbb' ;
  lStream := TStringStream.Create(lsStart);
  try
    tiUtils.tiAppendStringToStream(lsFrom, lStream);
    lsTo := lStream.DataString ;
    CheckEquals(lsStart + lsFrom, lsTo);
  finally
    lStream.Free;
  end ;

  lsStart := tiCreateStringOfSize(1000);
  lsFrom  := tiCreateStringOfSize(3000);
  lStream := TStringStream.Create(lsStart);
  try
    tiUtils.tiAppendStringToStream(lsFrom, lStream);
    lsTo := lStream.DataString ;
    CheckEquals(lsStart + lsFrom, lsTo);
  finally
    lStream.Free;
  end ;

end;

procedure TTestTIUtils.tiEncodeDecodeWordBase26;
var
  i   : Integer ;
  ls  : String ;
  li  : Integer ;
begin
  for i := Low(Word) to (High(Word) div 10) do
  begin
    ls := tiEncodeWordBase26(i);
    li := tiDecodeWordBase26(ls);
    CheckEquals(i, li, 'Failed on ' + IntToStr( i ) + ' ' + ls ) ;
  end ;
end;

end.
