{ Adds extra behaviour to TTestCase}

unit tiTestFramework;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiObject
  ,tiExcept
  ,tiOID
  ,tiConstants
  ,tiSmartPointer
  ,TestFramework
  ,TestExtensions
  ,inifiles
  ,Classes
  ,SysUtils
  ;

{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$define read_interface}
{$undef read_implementation}

const
  CErrorExceptionNotRaised = 'Exception not raised when it should have been';
  {$IFDEF FPC}
  sExpectedButWasFmt = 'Expected:'+cLineEnding+'"%s"'+cLineEnding+'But was:'+cLineEnding+'"%s"';
  sExpectedButWasAndMessageFmt = '%s' + cLineEnding + sExpectedButWasFmt;
  sMsgActualEqualsExpFmt = '%s'+cLineEnding+'Expected '+cLineEnding+'< %s > '+cLineEnding+'equals actual '+cLineEnding+'< %s >';
  sActualEqualsExpFmt = 'Expected '+cLineEnding+'< %s > '+cLineEnding+'equals actual '+cLineEnding+'< %s >';
  {$ENDIF}

type

  TtiPerformanceCounterResultType = Cardinal;

  TtiPerformanceCounter = class(TtiBaseObject)
  private
    FInitialized: boolean;
    FRunning: boolean;
    FStart: TtiPerformanceCounterResultType;
    FStop: TtiPerformanceCounterResultType;
    FPerformanceFactor: TtiPerformanceCounterResultType;
    procedure Initialize;
    function  GetTickCount: TtiPerformanceCounterResultType;
    function  GetPerformanceValue: TtiPerformanceCounterResultType;
  public
    constructor Create;
    procedure   Start;
    procedure   Stop;
    property    TickCount: TtiPerformanceCounterResultType read GetTickCount;
  end;

  EtiOPFDUnitException = class(EtiOPFException);

  // Abstract test case for testing non persistence related classes
  TtiTestCase = class(TTestCase)
  private
    FPerformanceCounter: TtiPerformanceCounter;
    FTempDirectory: string;
    FGC: ItiGC;
    function GetLongString: string;
    function  MakeDifferenceMessage(const AField1, AField2: TtiFieldAbs): string;
  protected
    property    GC: ItiGC read FGC;
    procedure   SetUpOnce; override;
    procedure   SetUp; override;
    procedure   TearDown; override;
    procedure   TearDownOnce;override;
    function    TempFileName(const AFilename: string = ''): string;
    procedure   UnderConstruction(const AMessage: string = '');
    procedure   IgnoreSlowUnitTest(const AMessage: string = '');

    property    LongString : string read GetLongString;
    // ToDo: Merge with TtiTestSetup Seed value generation methods
    function    tstIntToStr(pInt:Integer):string;
    function    tstIntToBool(pInt:Integer): boolean;
    function    tstIntToFloat(pInt:Integer): Extended;
    function    tstIntToDateTime(pInt:Integer): TDateTime;
    procedure   tstIntToStream(pInt:Integer; const AStream : TStream);
    function    tstStrToStr(const AStr: string; const AInc: Integer): string;
    function    tstStrToInt(const AStr: string; const AInc: Integer): integer;
    function    tstStrToFloat(const AStr: string; const AInc: Integer): real;
    procedure   LoadConfiguration(const iniFile :TCustomIniFile;
                                  const Section :string); {$IFNDEF FPC}override;{$ENDIF}
  public
    {$IFDEF FPC}
      // DUnit compatibility interface
      {$I FPCUnitHelper_intf.inc}
    {$ENDIF}
    class function TempDirectory: string; virtual; // Remove thsi function, replace with PathToTestTempDirectory
    class function PathToTestTempDirectory: string; // A better name
    function  PerformanceCounter: TtiPerformanceCounter;

    procedure Check(const ACondition: Boolean; AMessage: string; const AArgs: array of const); reintroduce; overload;
    function  AreStreamContentsSame(const AStream1, AStream2 : TStream; var AMessage : string; const AStartPosition: integer = 0): boolean;
    procedure CheckStreamContentsSame(const AStream1, AStream2 : TStream; const AMessage: string = ''; const AStartPosition: integer = 0);
    procedure CheckFormattedMessage(const AFormat: string; const AArgs: array of const; const AActual: string; const AMessage: string = '');
    procedure CheckObjectState(const AObjectState : TPerObjectState; const AData : TtiObject; const AMessage: string = '');
    procedure CheckFileContents(const AFileNameExpected, AFileNameActual: string);
    procedure CheckDirectoryTree(const APathToExpected, APathToActual: string);
    procedure CheckBinaryFileContents(const AFileNameExpected, AFileNameActual: string; const AStartPosition: integer = 0);
    procedure CheckGIFFileContents(const AFileNameExpected, AFileNameActual: string);
    procedure CheckNotifyOperation(const AExpected, AActual: TNotifyOperation; const AMessage: string = '');
    procedure CheckNearEnough(AExpected, AActual: Extended; const AMessage: string = ''); overload;
    procedure CheckNearEnough(const AExpected, AActual: Double); overload;
    procedure CheckNearEnough(const AExpected, AActual: Double; const AMessage: string); overload;
    procedure CheckNearEnough(const AExpected, AActual: Double; const AMessage: string; pArgs: array of const); overload;
    procedure CheckNearEnough(const AExpected, AActual, AFEPS: Double); overload;
    procedure CheckNearEnough(const AExpected, AActual, AFEPS: Double; const AString: string); overload;
    procedure CheckNearEnough(const AExpected: TDateTime; const AField: TtiFieldDateTime); overload;
    procedure CheckNearEnough(const AExpected: Real; const AField: TtiFieldFloat); overload;
    procedure CheckNearEnough(const AExpected, AActual: TtiFieldFloat); overload;
    procedure CheckNearestMillisecond(const AExpected: TDateTime; const AActual: TDateTime; const AMessage: string; AArgs: array of const); overload;
    procedure CheckNearestMillisecond(const AExpected: TDateTime; const AActual: TDateTime; const AMessage: string = ''); overload;
    procedure CheckDateTimeNearEnough(const ADateTime1: TDateTime; const ADateTime2: TDateTime; const AProximity: TDateTime; const AMessage: string = '');
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const APropName: String); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldString); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldInteger); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldFloat); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldDateTime); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldDate); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldBoolean); overload;

    procedure CheckTIObjectEqualsWithMessage(const AObj1: TtiObject; const AObj2: TtiObject; const AField1: TtiFieldString;   const AField2: TtiFieldString); overload;
    procedure CheckTIObjectEqualsWithMessage(const AObj1: TtiObject; const AObj2: TtiObject; const AField1: TtiFieldInteger;  const AField2: TtiFieldInteger); overload;
    procedure CheckTIObjectEqualsWithMessage(const AObj1: TtiObject; const AObj2: TtiObject; const AField1: TtiFieldFloat;    const AField2: TtiFieldFloat); overload;
    procedure CheckTIObjectEqualsWithMessage(const AObj1: TtiObject; const AObj2: TtiObject; const AField1: TtiFieldBoolean;  const AField2: TtiFieldBoolean); overload;
    procedure CheckTIObjectEqualsWithMessage(const AObj1: TtiObject; const AObj2: TtiObject; const AField1: TtiFieldDateTime; const AField2: TtiFieldDateTime); overload;

{$IFDEF OID_AS_INT64}
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; var AField1, AField2: TtiOID); overload;
{$ELSE}
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiOID); overload;
{$ENDIF}
    procedure TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldString); overload;
    procedure TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldInteger); overload;
    procedure TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldFloat); overload;
    procedure TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldDateTime); overload;
    procedure TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldBoolean); overload;
    procedure CheckEquals(const AExpected, AActual: TtiFieldString); overload;
    procedure CheckEquals(const AExpected, AActual: TtiFieldInteger); overload;
    procedure CheckEquals(const AExpected, AActual: TtiFieldFloat); overload;
    procedure CheckEquals(const AExpected, AActual: TtiFieldBoolean); overload;
    procedure CheckEquals(const AExpected, AActual: TtiFieldDateTime); overload;
    procedure CheckEquals(const AExpected, AActual: TtiFieldDate); overload;
    {$IFNDEF OID_AS_INT64}
    procedure CheckEquals(const AExpected, AActual: TtiOID); overload;
    {$ENDIF}
    procedure CheckEquals(const AValue: string;    const AField: TtiFieldString); overload;
    procedure CheckEquals(const AValue: Integer;   const AField: TtiFieldInteger); overload;
    procedure CheckEquals(const AValue: Real;      const AField: TtiFieldFloat); overload;
    procedure CheckEquals(const AValue: Boolean;   const AField: TtiFieldBoolean); overload;
    procedure CheckEquals(const AValue: TDateTime; const AField: TtiFieldDateTime); overload;
    procedure CheckEquals(const AValue: TStream;   const AField: TStream); overload;
    procedure CheckEquals(const AValue: String;   const AField: TtiFieldInteger); overload;
    procedure CheckEquals(const AExpected: string; AActual: integer); overload;

    procedure CheckEquals(const AField: TtiFieldString;   const AValue: string); overload;
    procedure CheckEquals(const AField: TtiFieldInteger;  const AValue: Integer); overload;
    procedure CheckEquals(const AField: TtiFieldFloat;    const AValue: Real); overload;
    procedure CheckEquals(const AField: TtiFieldBoolean;  const AValue: Boolean); overload;
    procedure CheckEquals(const AField: TtiFieldDateTime; const AValue: TDateTime); overload;

    procedure CheckLessThan(const AMustBeLessThan, AValue: Int64); overload;

    procedure CheckEquals(const AExpected: string; const AOID: TtiOID; const AMessage: string = ''); overload;

    {: Check a TtiObject's Equals Method by calling AData1.Equals(AData2), then changing APropName to ANewValue and trying again.
       APropName must be a string property.}
    procedure CheckTIObjectEqualsMethod(const AData1, AData2: TtiObject; const APropName, ANewValue: string); overload;
    {: Check a TtiObject's Equals Method by calling AData1.Equals(AData2), then changing APropName to ANewValue and trying again.
       APropName must be a real property.}
    procedure CheckTIObjectEqualsMethod(const AData1, AData2: TtiObject; const APropName: string; const ANewValue: Real); overload;
    {: Check a TtiObject's IsValid Method by calling AData.IsValid, then changing APropName to ANewValue and trying again.
       APropName must be a string property.}
    procedure CheckTIObjectIsValidMethod(const AData: TtiObject; const APropName: string; const AInvalidValue: String; const AErrorProperty: string = ''); overload;
    {: Check a TtiObject's IsValid Method by calling AData.IsValid, then changing APropName to ANewValue and trying again.
       APropName must be a integer property.}
    procedure CheckTIObjectIsValidMethod(const AData: TtiObject; const APropName: string; const AInvalidValue: Int64; const AErrorProperty: string = ''); overload;
    {: Check a TtiObject's IsValid Method by calling AData.IsValid, then changing APropName to ANewValue and trying again.
       APropName must be a real property.}
    procedure CheckTIObjectIsValidMethod(const AData: TtiObject; const APropName: string; const AInvalidValue: Real; const AErrorProperty: string = ''); overload;

    {: Check the INI file AINIFileName contains the string AExpected in the AINISection:AINIIdent location}
    procedure CheckINIFileEntry(const AExpected: string; const AINIFileName, AINISection, AINIIdent: string); overload;
    {: Check the INI file AINIFileName contains the TDateTime AExpected in the AINISection:AINIIdent location}
    procedure CheckINIFileEntry(const AExpected: TDateTime; const AINIFileName, AINISection, AINIIdent: string); overload;
    {: Check the INI file AINIFileName contains the Int64 AExpected in the AINISection:AINIIdent location}
    procedure CheckINIFileEntry(const AExpected: Int64; const AINIFileName, AINISection, AINIIdent: string); overload;
    {: Check the contents of an exception's message property}
    procedure CheckExceptionMessage(const AMessage : string; const AException : Exception); overload;
    procedure CheckExceptionMessage(const AMessage : string; const AArgs: array of const; const AException : Exception); overload;
    procedure CheckDateTimeEquals(const AExpected, AActual: TDateTime; const AMessage: string = '');

  end;

  TtiTestCaseClass    = class of TtiTestCase;

  TtiTestSetup = class(TtiBaseObject)
  private
    FTestCase: TtiTestCase;
  protected
    property    TC : TtiTestCase read FTestCase;
    procedure UnderConstruction(const AMessage: string = '');
  public
    constructor Create(const ATestCase : TtiTestCase); virtual ;
    // ToDo: Merge with TtiTestCase Seed value generation methods
    function    tvToStr(      const AValue: string; const AInc: integer = 0): string;
    function    tvToDateTime( const AValue: string; const AInc: integer = 0): TDateTime;
    function    tvToDate(     const AValue: string; const AInc: integer = 0): TDateTime;
    function    tvToInt(      const AValue: string; const AInc: integer = 0): Int64;
    function    tvToIntWithLimit(const AValue: string; const ALimit: integer): Int64;
    function    tvToFloat(    const AValue: string; const AInc: integer = 0): Real;
    function    tvToBoolean(  const AValue: string; const AInc: integer = 0): Boolean;
    procedure   tvToStream(   const AStream: TStream; const AValue: string);
  end;

function  tiCreateStringOfSize(const ASize : LongInt): string;
procedure tiCreateTextFileOfSize(const AFileName : string; const ASize : LongInt);
procedure tiDUnitForceRemoveDir(const ADirectory : string);
procedure tiFillTestingStream(const AStream : TStream; const ASize : integer);
procedure tiFillTestingStreamASCII(const AStream : TStream; const ASize : integer);
function  tiIncStr(const AValue: string; const AInc: Integer=1; const AMaxLength : integer = 999999): String;
function  tiIncStr1(const AValue: string; var AInc: Integer): String;
const
  cDUnitTestFloatPrecision = 0.000001;

implementation
uses
   tiUtils
  ,tiINI
  ,tiRTTI
  ,tiLog
  ,StrUtils
  ,DateUtils
  ,TypInfo
  ,Types
  {$IFDEF MSWINDOWS}
  ,Windows
  ,tiConsoleApp
  {$ENDIF}
  ,TestUtils
 ;

{ This lets us use a single include file for both the Interface and
  Implementation sections. }
{$undef read_interface}
{$define read_implementation}

var
  UTempPath : string = '';
  ULongString: string = '';

function  tiCreateStringOfSize(const ASize : LongInt): string;
var
  ls : Char;
begin
  result := '';
  While Length(Result)< ASize do
  begin
    ls := Chr(Random(126 - 32) + 32);
    result := result + ls;
    if (Length(Result)< ASize) and
       (Length(Result) mod 60 = 0) then
      Result := Result + cLineEnding;
  end;
end;

procedure tiCreateTextFileOfSize(const AFileName : string; const ASize : LongInt);
var
  lFileStream : TFileStream;
  lBuffer  : PChar;
  lLen     : integer;
  ls : string;
begin
  ls := tiCreateStringOfSize(ASize);

  if FileExists(AFileName) then
    tiDeleteFile(AFileName);
  lFileStream := TFileStream.Create(AFileName,
                                     fmCreate or fmShareDenyNone);
  try
    lBuffer := PChar(ls);
    lLen := length(ls);
    lFileStream.write(lBuffer^, lLen);
  finally
    lFileStream.Free;
  end;
end;

procedure tiDUnitForceRemoveDir(const ADirectory : string);
var
  LResult: DWord;
  LOutput: string;

begin
  tiUtils.tiForceRemoveDir(ADirectory);
(*
  // The easiest way I could think of to delete a directory and all
  // it's child directories and files.
  try
    {$IFDEF MSWINDOWS}

    LResult:=
      tiExecConsoleApp('cmd.exe',
                       '/X /C "rd ' + ADirectory + ' /S /Q"',
                       LOutput,
                       nil,
                       False);
    LOutput:= Trim(LOutput);
    if not LResult in [0,1] then
      raise Exception.CreateFmt(
        'Error calling tiExecConsoleApp in tiDUnitForceRemoveDir. ' +
        'Expected result ="0 or 1" but got "%d"', [LResult]);
    if LOutput <> '' then
      raise Exception.CreateFmt(
        'Error calling tiExecConsoleApp in tiDUnitForceRemoveDir. ' +
        'Expected Output ="" but got "%s"', [LOutput]);

    {$ENDIF MSWINDOWS}

    {$IFDEF LINUX}
    tiUtils.tiRunEXEAndWait('rm -f -R ' + ADirectory);
    {$ENDIF LINUX}
  except
    on e:exception do
      raise exception.Create('Error in tiDUnitForceRemoveDir(''' +
            ADirectory + '> Message: ' + e.message);
  end;
*)
  if DirectoryExists(ADirectory) then
      raise exception.CreateFmt('Error in tiDUnitForceRemoveDir. Failed to delete directory "%s"',
            [ADirectory]);

end;

procedure tiFillTestingStream(const AStream : TStream; const ASize : integer);
var
  LCount : integer;
  LWord : word;
  LChar : Char;
begin
  // Cloned from IdSoapTestingUtils.pas (IndySoap) by
  // Grahame Grieve & Andrew Cumming
  for LCount := 1 to (ASize div 2) do
    begin
    LWord := Random($7FFF);
    AStream.WriteBuffer(LWord, sizeof(Word));
    end;
  if ASize mod 2 = 1 then
    begin
    LChar := chr(32+Random(56));
    AStream.WriteBuffer(LChar, sizeof(Char));
    end;
  AStream.Position := 0;
end;

procedure tiFillTestingStreamASCII(const AStream : TStream; const ASize : integer);
var
  LCount : integer;
  LChar : Char;
begin
  for LCount := 1 to ASize do
    begin
    LChar := chr(32+Random(56));
    AStream.WriteBuffer(LChar, sizeof(Char));
    end;
  AStream.Position := 0;
end;

function tiIncStr(const AValue: string; const AInc: integer=1; const AMaxLength: integer = 999999): String;
var
  LLen: integer;
begin
  result:= IntToStr(StrToInt(AValue)+AInc);
  LLen:= Length(Result);
  if LLen > AMaxLength then
    Result:= Copy(Result, LLen - AMaxLength, AMaxLength);
end;

function  tiIncStr1(const AValue: string; var AInc: Integer): String;
begin
  result:= tiIncStr(AValue, AInc);
  Inc(AInc);
end;

class function TtiTestCase.TempDirectory: string;
{$IFDEF MSWINDOWS}
var
  pcTemp : array[0..MAX_PATH] of char;
{$ENDIF}
begin
  if Length(uTempPath) = 0 then
  begin
    {$IFDEF MSWINDOWS}
    GetTempPath(MAX_PATH, pcTemp);
    uTempPath := string(pcTemp);
    if Copy(uTempPath, Length(uTempPath), 1) = '\' then
      uTempPath := Copy(uTempPath, 1, Length(uTempPath)-1);
    {$ENDIF}
    {$IFDEF UNIX}
    uTempPath := '/tmp';
    {$ENDIF}
  end;
  result := uTempPath + PathDelim + 'TempDUnitFiles';
  if not DirectoryExists(result) then
    tiForceDirectories(result);
end;


function TtiTestCase.TempFileName(const AFilename : string = ''): string;
var
  LFileName: string;
begin
  if AFileName = '' then
    LFileName := 'temp.txt'
  else
    LFileName:= AFileName;
  Result := TempDirectory + PathDelim + LFilename;
end;

procedure TtiTestCase.CheckStreamContentsSame(
  const AStream1, AStream2: TStream;
  const AMessage: string = '';
  const AStartPosition: integer = 0);
var
  LResult : boolean;
  LMessage : string;
begin
  Check(True); // To Force OnCheckCalled to be called
  LMessage := '';
  LResult := AreStreamContentsSame(AStream1, AStream2, LMessage, AStartPosition);
  Check(LResult,  Trim(AMessage + CrLf + LMessage));
end;


function TtiTestCase.AreStreamContentsSame(
 const AStream1, AStream2: TStream;
  var AMessage : string;
  const AStartPosition: integer = 0): boolean;
var
   LByte1, LByte2 : byte;
begin
   result := true;

   if AStream1.Size <> AStream2.Size then
   begin
     result := false;
     AMessage := 'Streams have different sizes ('+inttostr(AStream1.Size)+'/'+
                 inttostr(AStream2.Size)+')';
     exit; //==>
   end;

{
   AStream1.Position := AStream1.Size-1;
   AStream2.Position := AStream1.Size-1;
   while (AStream1.Position >= 0) do
   begin
     AStream1.Read(LByte1, 1);
     AStream2.Read(LByte2, 1);
     if LByte1 <> LByte2 then
     begin
       result := false;
       AMessage := 'Streams Differ at position '+
                   inttostr(AStream1.Position)+' of '+
                   inttostr(AStream1.Size)+
                   ':'+inttostr(LByte1)+'/'+inttostr(LByte2);
       exit; //==>
     end;
     AStream1.Position := AStream1.Position-2;
     AStream2.Position := AStream2.Position-2;
   end;
}

   AStream1.Position := AStartPosition;
   AStream2.Position := AStartPosition;
   while (AStream1.Size - AStream1.Position > 0) do
   begin
     AStream1.Read(LByte1, 1);
     AStream2.Read(LByte2, 1);
     if LByte1 <> LByte2 then
     begin
       result := false;
       AMessage := 'Streams Differ at position '+
                   inttostr(AStream1.Position)+' of '+
                   inttostr(AStream1.Size)+
                   ':'+inttostr(LByte1)+'/'+inttostr(LByte2);
       exit; //==>
     end;
   end;

end;

procedure TtiTestCase.Check(const ACondition: Boolean; AMessage: string; const AArgs: array of const);
begin
  Check(ACondition, Format(AMessage, AArgs));
end;

procedure TtiTestCase.CheckFileContents(const AFileNameExpected,
  AFileNameActual: string);
var
  LFileContentsExpected: string;
  LFileContentsActual: string;
begin
  LFileContentsExpected:= tiFileToString(AFileNameExpected);
  LFileContentsActual:= tiFileToString(AFileNameActual);
  if Length(LFileContentsExpected) < 1024 then
    CheckEquals(
      LFileContentsExpected, LFileContentsActual,
      Format('File "%s" contents <> "%s" contents', [AFileNameExpected, AFileNameActual]))
  else
    Check(
      LFileContentsExpected = LFileContentsActual,
      Format('File "%s" contents <> "%s" contents', [AFileNameExpected, AFileNameActual]));

end;

procedure TtiTestCase.CheckDirectoryTree(
  const APathToExpected, APathToActual: string);
var
  LExpected: TStringList;
  LActual: TStringList;
  LFileNameExpected: string;
  LFileNameActual: string;
  LFileExt: string;
  i: integer;
begin
  LExpected:= nil;
  LActual:= nil;
  try
    LExpected:= TStringList.Create;
    LActual:= TStringList.Create;

    tiFilesToStringList(APathToExpected, '*.*', LExpected, true);
    tiFilesToStringList(APathToActual, '*.*', LActual, true);

    CheckEquals(LExpected.Count, LActual.Count, 'File Count');
    for i := 0 to LExpected.Count-1 do
    begin
      LFileNameExpected:= LExpected.Strings[i];
      LFileNameActual:= LActual.Strings[i];
      CheckEquals(
        UpperCase(ExtractFileName(LFileNameExpected)),
        UpperCase(ExtractFileName(LFileNameActual)),
        'File name');
      LFileExt:= tiExtractExtension(LFileNameExpected);
      if SameText(LFileExt, 'gif') then
        CheckGIFFileContents(LFileNameExpected, LFileNameActual)
      else
        CheckFileContents(LFileNameExpected, LFileNameActual);
    end;

  finally
    LExpected.Free;
    LActual.Free;
  end;
end;

procedure TtiTestCase.CheckBinaryFileContents(const AFileNameExpected,
  AFileNameActual: string; const AStartPosition: integer = 0);
var
  LStreamExpected: TMemoryStream;
  LStreamActual: TMemoryStream;
  LMessage: string;
begin
  LMessage:= Format(
    'Expected = "%s",' + CrLf + 'Actual = "%s',
    [AFileNameExpected, AFileNameActual]);

  LStreamExpected:= nil;
  LStreamActual:= nil;
  try
    LStreamExpected:= TMemoryStream.Create;
    LStreamActual:= TMemoryStream.Create;
    LStreamExpected.LoadFromFile(AFileNameExpected);
    LStreamActual.LoadFromFile(AFileNameActual);
    CheckStreamContentsSame(LStreamExpected, LStreamActual, LMessage, AStartPosition);
  finally
    LStreamExpected.Free;
    LStreamActual.Free;
  end;
end;

procedure TtiTestCase.CheckDateTimeEquals(const AExpected, AActual: TDateTime; const AMessage: string = '');
begin
  CheckEquals(DateTimeToStr(AExpected), DateTimeToStr(AActual), AMessage);
end;

{$IFDEF FPC}
// DUnit compatibility interface
  {$I FPCUnitHelper_impl.inc}
{$ENDIF}


procedure TtiTestCase.SetUp;
begin
  inherited;
  {$IFDEF FPC}
  SetupOnce;
  {$ENDIF}
  FGC := CreateGC;
end;

procedure TtiTestCase.SetUpOnce;
begin
  inherited;
  FTempDirectory := TempDirectory;
end;

procedure TtiTestCase.TearDown;
begin
  inherited;
  if DirectoryExists(FTempDirectory) then
    try tiForceRemoveDir(FTempDirectory) except end;
  GLog.Purge;
  FGC:= nil;
end;

procedure TtiTestCase.TearDownOnce;
begin
  FreeAndNil(FPerformanceCounter);
  inherited;
end;

function TtiTestCase.tstIntToBool(pInt: Integer): boolean;
begin
  Result := (pInt mod 2) = 0;
end;


function TtiTestCase.tstIntToDateTime(pInt: Integer): TDateTime;
var
  d: double;
begin
  d := 1000;  // removes compiler warning
  Result := pInt / d;
end;


function TtiTestCase.tstIntToFloat(pInt: Integer): Extended;
var
  e: extended;
begin
  e := 1000;  // removes compiler warning
  Result := pInt / e;
end;


function TtiTestCase.tstIntToStr(pInt: Integer): string;
begin
  Result := IntToStr(pInt);
end;


procedure TtiTestCase.tstIntToStream(pInt: Integer;const AStream: TStream);
begin
  tiStringToStream(tstIntToStr(pInt), AStream);
end;

function TtiTestCase.tstStrToFloat(const AStr: string;
  const AInc: Integer): real;
begin
  result:= StrToFloat(tiIncStr(AStr, AInc)) / 100;
end;

function TtiTestCase.tstStrToInt(const AStr: string;
  const AInc: Integer): integer;
begin
  result:= StrToInt(tiIncStr(AStr, AInc));
end;

function TtiTestCase.tstStrToStr(const AStr: string;
  const AInc: Integer): string;
begin
  result:= tiIncStr(AStr, AInc);
end;

procedure TtiTestCase.UnderConstruction(const AMessage: string = '');
begin
{$IFDEF IGNORE_UNDER_CONSTRUCTION}
  Check(True);
{$ELSE}
  if AMessage = '' then
    Fail('Under construction', CallerAddr)
  else
    Fail(AMessage, CallerAddr);
{$ENDIF}
end;

procedure TtiTestCase.CheckFormattedMessage(const AFormat: string;
  const AArgs: array of const; const AActual: string;
  const AMessage: string = '');
begin
  CheckEquals(Format(AFormat, AArgs), AActual, AMessage);
end;

procedure TtiTestCase.CheckGIFFileContents(
  const AFileNameExpected,  AFileNameActual: string);
begin
  // There can be noise somewhere in the first 792 bytes of a GIF.
  // Not sure why.
  CheckBinaryFileContents(AFileNameExpected, AFileNameActual, 791)
end;

procedure TtiTestCase.CheckINIFileEntry(const AExpected: string;
  const AINIFileName, AINISection, AINIIdent: string);
var
  LINI: TtiINIFile;
  LActual: string;
begin
  LINI:= TtiINIFile.Create(AINIFileName);
  try
    LActual:= LINI.ReadString(AINISection, AINIIdent, '');
    CheckEquals(AExpected, LActual);
  finally
    LINI.Free;
  end;
end;

procedure TtiTestCase.CheckINIFileEntry(const AExpected: TDateTime;
  const AINIFileName, AINISection, AINIIdent: string);
var
  LINI: TtiINIFile;
  LActual: TDateTime;
begin
  LINI:= TtiINIFile.Create(AINIFileName);
  try
    LActual:= LINI.ReadDateTime(AINISection, AINIIdent, 0);
    CheckNearEnough(AExpected, LActual);
  finally
    LINI.Free;
  end;
end;

procedure TtiTestCase.CheckINIFileEntry(const AExpected: Int64; const AINIFileName, AINISection, AINIIdent: string);
var
  LINI: TtiINIFile;
  LActual: Int64;
begin
  LINI:= TtiINIFile.Create(AINIFileName);
  try
    LActual:= LINI.ReadInteger(AINISection, AINIIdent, 0);
    CheckEquals(AExpected, LActual);
  finally
    LINI.Free;
  end;
end;

procedure TtiTestCase.CheckLessThan(const AMustBeLessThan, AValue: Int64);
const
  CCheckLessThanMessage =
    'Must be less than "%d" but was "%d"';
begin
  Check(AValue<AMustBeLessThan, CCheckLessThanMessage, [AMustBeLessThan, AValue]);
end;

procedure TtiTestCase.CheckNearEnough(AExpected, AActual: Extended; const AMessage: string = '');
begin
  Check(True); // To suppress DUnit warning
  CheckEquals(AExpected, AActual, 0.000001, AMessage);
end;


function TtiTestCase.GetLongString: string;
begin
  result:= ULongString;
end;

procedure TtiTestCase.IgnoreSlowUnitTest(const AMessage: string);
begin
{$IFDEF IGNORE_SLOW_UNIT_TESTS}
  if AMessage = '' then
    Fail('Slow unit test ignored', CallerAddr)
  else
    Fail(AMessage, CallerAddr);
{$ENDIF}
end;

procedure TtiTestCase.LoadConfiguration(const iniFile: TCustomIniFile;
                                        const Section: string);
var
  LSubstSection: string;
begin
  LSubstSection := AnsiReplaceStr(Section, 'Text.exe', 'GUI.exe');
  {$IFNDEF FPC}
  inherited LoadConfiguration(inifile, LSubstSection);;
  {$ENDIF}
end;

procedure TtiTestCase.CheckObjectState(
  const AObjectState: TPerObjectState;
  const AData: TtiObject;
  const AMessage: string = '');
begin
  Assert(AData.TestValid, CTIErrorInvalidObject);
  Check(AObjectState = AData.ObjectState,
        'ObjectState: Expected <' +
        GetEnumName(TypeInfo(TPerObjectState), Ord(AObjectState)) +
        '> but got <' +
        AData.ObjectStateAsString +
        '> on ' + AData.ClassName + '. ' + AMessage);
end;

procedure TtiTestCase.CheckNotifyOperation(const AExpected,
  AActual: TNotifyOperation; const AMessage: string);
begin
  Check(AExpected = AActual, 'NotifyOperation: Expected <' +
      GetEnumName(TypeInfo(TNotifyOperation), Ord(AExpected)) +
      '> but got <' +
      GetEnumName(TypeInfo(TNotifyOperation), Ord(AActual)) +
      '>. ' + AMessage);
end;

class function TtiTestCase.PathToTestTempDirectory: string;
begin
  result:= TempDirectory;
end;

function TtiTestCase.PerformanceCounter: TtiPerformanceCounter;
begin
  if FPerformanceCounter=nil then
    FPerformanceCounter:= TtiPerformanceCounter.Create;
  result:= FPerformanceCounter;
end;

procedure TtiTestCase.CheckNearEnough(const AExpected, AActual: Double);
begin
  CheckNearEnough(AExpected, AActual, 0.0001);
end;

procedure TtiTestCase.CheckNearEnough(const AExpected, AActual: Double; const AMessage: string);
begin
  CheckNearEnough(AExpected, AActual, 0.0001, AMessage);
end;

procedure TtiTestCase.CheckNearEnough(const AExpected, AActual, AFEPS: Double);
begin
  CheckNearEnough(AExpected, AActual, AFEPS, '');
end;

procedure TtiTestCase.CheckEquals(const AValue: Integer; const AField: TtiFieldInteger);
begin
  CheckEquals(AValue, AField.AsInteger);
end;

procedure TtiTestCase.CheckEquals(const AValue: string; const AField: TtiFieldString);
begin
  CheckEquals(AValue, AField.AsString);
end;

procedure TtiTestCase.CheckEquals(const AValue: Real; const AField: TtiFieldFloat);
begin
  CheckNearEnough(AValue, AField.AsFloat);
end;

procedure TtiTestCase.CheckEquals(const AValue: TDateTime; const AField: TtiFieldDateTime);
begin
  CheckNearEnough(AValue, AField.AsDateTime);
end;

procedure TtiTestCase.CheckEquals(const AExpected, AActual: TtiFieldFloat);
begin
  CheckNearEnough(AExpected.AsFloat, AActual.AsFloat, 'Failed on %s', [AExpected.FieldName]);
end;

procedure TtiTestCase.CheckEquals(const AExpected, AActual: TtiFieldInteger);
begin
  CheckEquals(AExpected.AsInteger, AActual.AsInteger, Format('Failed on %s', [AExpected.FieldName]));
end;

procedure TtiTestCase.CheckEquals(const AExpected, AActual: TtiFieldDateTime);
begin
  CheckNearEnough(AExpected.AsDateTime, AActual.AsDateTime, 'Failed on %s', [AExpected.FieldName]);
end;

procedure TtiTestCase.CheckEquals(const AExpected, AActual: TtiFieldDate);
begin
  CheckNearEnough(AExpected.AsDateTime, AActual.AsDateTime, 'Failed on %s', [AExpected.FieldName]);
end;

procedure TtiTestCase.CheckEquals(const AExpected, AActual: TtiFieldBoolean);
begin
  CheckEquals(AExpected.AsBoolean, AActual.AsBoolean, Format('Failed on %s', [AExpected.FieldName]));
end;

procedure TtiTestCase.CheckEquals(const AExpected, AActual: TtiFieldString);
begin
  CheckEquals(AExpected.AsString, AActual.AsString, Format('Failed on %s', [AExpected.FieldName]));
end;

procedure TtiTestCase.CheckEquals(const AValue: Boolean; const AField: TtiFieldBoolean);
begin
  CheckEquals(AValue, AField.AsBoolean);
end;

procedure TtiTestCase.CheckNearEnough(const AExpected, AActual, AFEPS: Double; const AString: string);
begin
  Check(tiIsNearEnough(AExpected, AActual, AFEPS),
        NotEqualsErrorMessage(FloatToStr(AExpected), FloatToStr(AActual),
          Format('(feps: %g) %s', [AFEPS, AString])));
end;

procedure TtiTestCase.CheckNearEnough(const AExpected, AActual: Double; const AMessage: string; pArgs: array of const);
begin
  CheckNearEnough(AExpected, AActual, Format(AMessage, pArgs));
end;

procedure TtiTestCase.CheckNearestMillisecond(const AExpected: TDateTime;
  const AActual: TDateTime; const AMessage: string);
begin
  Check(tiCompareDateTimeToMilliSecond(AExpected, AActual) = EqualsValue,
        NotEqualsErrorMessage(
            FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AExpected),
            FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AActual), AMessage));
end;

procedure TtiTestCase.CheckNearestMillisecond(const AExpected: TDateTime;
  const AActual: TDateTime; const AMessage: string; AArgs: array of const);
begin
  CheckNearestMillisecond(AExpected, AActual, Format(AMessage, AArgs));
end;

procedure TtiTestCase.CheckDateTimeNearEnough(const ADateTime1: TDateTime;
  const ADateTime2: TDateTime; const AProximity: TDateTime;
  const AMessage: string);
var
  LMessage: string;

  function _FormatDateTime(const ADateTime: TDateTime): string;
  begin
    if DateOf(ADateTime) <> 0 then
      Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ADateTime)
    else
      Result := FormatDateTime('hh:nn:ss.zzz', ADateTime);
  end;

begin
  LMessage := 'Date times are not near enough: ' +
      _FormatDateTime(ADateTime1) + ' <> ' +
      _FormatDateTime(ADateTime2) + ', ' +
      'Proximity = ' + _FormatDateTime(AProximity);
  if AMessage <> '' then
    LMessage := LMessage + #13#10 + AMessage;
  Check(tiIsDateTimeNearEnough(ADateTime1, ADateTime2, AProximity), LMessage);
end;

procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldString);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, CTIErrorInvalidObject);
  Assert(AObj2.TestValid, CTIErrorInvalidObject);
  Assert(AField1.TestValid, CTIErrorInvalidObject);
  Assert(AField2.TestValid, CTIErrorInvalidObject);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsString := AField2.AsString + '1';
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldFloat);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, CTIErrorInvalidObject);
  Assert(AObj2.TestValid, CTIErrorInvalidObject);
  Assert(AField1.TestValid, CTIErrorInvalidObject);
  Assert(AField2.TestValid, CTIErrorInvalidObject);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsFloat := AField2.AsFloat + 1;
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldInteger);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, CTIErrorInvalidObject);
  Assert(AObj2.TestValid, CTIErrorInvalidObject);
  Assert(AField1.TestValid, CTIErrorInvalidObject);
  Assert(AField2.TestValid, CTIErrorInvalidObject);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsInteger := AField2.AsInteger + 1;
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldBoolean);
begin
  Assert(AObj1.TestValid, CTIErrorInvalidObject);
  Assert(AObj2.TestValid, CTIErrorInvalidObject);
  Assert(AField1.TestValid, CTIErrorInvalidObject);
  Assert(AField2.TestValid, CTIErrorInvalidObject);

  Check(AObj1.Equals(AObj2));
  AField1.AsBoolean := not AField2.AsBoolean;
  Check(not AObj1.Equals(AObj2));
  AField1.AsBoolean := AField2.AsBoolean;
  Check(AObj1.Equals(AObj2));
end;

procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject;
  const APropName: String);
var
  LSavedStr: string;
begin
//  CheckEquals(AObj1.PropValue[APropName], AObj2.PropValue[APropName]);
  case AObj1.PropType(APropName) of
  tiTKInteger:  begin
                  Assert(False, 'Under construction');
                end;
  tiTKFloat:    begin
                  Assert(False, 'Under construction');
                end;
  tiTKString:   begin
                  Check(AObj1.Equals(AObj2), 'Expected equality');
                  LSavedStr:= AObj2.PropValue[APropName];
                  AObj2.PropValue[APropName]:= AObj2.PropValue[APropName] + 'A';
                  Check(not AObj1.Equals(AObj2), 'Expected inequality');
                  AObj2.PropValue[APropName]:=LSavedStr;
                  Check(AObj1.Equals(AObj2), 'Expected equality');
                end;
  tiTKDateTime: begin
                  Assert(False, 'Under construction');
                end;
  tiTKBoolean:  begin
                  Assert(False, 'Under construction');
                end;
  //tiTKBinary:
  else
    raise EtiOPFDUnitException.CreateFmt('Invalid type for property "%s.%s"',
      [AObj1.ClassName, APropName]);
  end;

end;

procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldDateTime);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, CTIErrorInvalidObject);
  Assert(AObj2.TestValid, CTIErrorInvalidObject);
  Assert(AField1.TestValid, CTIErrorInvalidObject);
  Assert(AField2.TestValid, CTIErrorInvalidObject);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsDateTime := AField2.AsDateTime + 1;
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.CheckTIObjectEqualsWithMessage(
  const AObj1: TtiObject;
  const AObj2: TtiObject;
  const AField1: TtiFieldString;
  const AField2: TtiFieldString);
var
  LMessage: string;
  LSave: string;
  LDifferenceMessageActual: string;
  LDifferenceMessageExpected: string;
begin
  LMessage := AObj1.ClassName + '.' + Copy(AField1.ClassName, 2, Length(AField1.ClassName));
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
  LSave := AField1.AsString ;
  LDifferenceMessageActual:= '';
  AField1.AsString := AField1.AsString + 'x';
  LDifferenceMessageExpected:= MakeDifferenceMessage(AField1, AField2);
  Check(not AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals(LDifferenceMessageExpected, LDifferenceMessageActual);
  AField1.AsString := LSave;
  LDifferenceMessageActual:= '';
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
end;

procedure TtiTestCase.CheckTIObjectEqualsWithMessage(
  const AObj1: TtiObject;
  const AObj2: TtiObject;
  const AField1: TtiFieldFloat;
  const AField2: TtiFieldFloat);
var
  LMessage: string;
  LSave: real;
  LDifferenceMessageActual: string;
  LDifferenceMessageExpected: string;
begin
  LMessage := AObj1.ClassName + '.' + Copy(AField1.ClassName, 2, Length(AField1.ClassName));
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
  LSave := AField1.AsFloat;
  LDifferenceMessageActual:= '';
  AField1.AsFloat:= AField1.AsFloat + 1;
  LDifferenceMessageExpected:= MakeDifferenceMessage(AField1, AField2);
  Check(not AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals(LDifferenceMessageExpected, LDifferenceMessageActual);
  AField1.AsFloat := LSave;
  LDifferenceMessageActual:= '';
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
end;

procedure TtiTestCase.CheckTIObjectEqualsWithMessage(
  const AObj1: TtiObject;
  const AObj2: TtiObject;
  const AField1: TtiFieldInteger;
  const AField2: TtiFieldInteger);
var
  LMessage: string;
  LSave: integer;
  LDifferenceMessageActual: string;
  LDifferenceMessageExpected: string;
begin
  LMessage := AObj1.ClassName + '.' + Copy(AField1.ClassName, 2, Length(AField1.ClassName));
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
  LSave := AField1.AsInteger;
  LDifferenceMessageActual:= '';
  AField1.AsInteger := AField1.AsInteger + 1;
  LDifferenceMessageExpected:= MakeDifferenceMessage(AField1, AField2);
  Check(not AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals(LDifferenceMessageExpected, LDifferenceMessageActual);
  AField1.AsInteger := LSave;
  LDifferenceMessageActual:= '';
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
end;

procedure TtiTestCase.CheckTIObjectEqualsWithMessage(
  const AObj1: TtiObject;
  const AObj2: TtiObject;
  const AField1: TtiFieldDateTime;
  const AField2: TtiFieldDateTime);
var
  LMessage: string;
  LSave: TDateTime;
  LDifferenceMessageActual: string;
  LDifferenceMessageExpected: string;
begin
  LMessage := AObj1.ClassName + '.' + Copy(AField1.ClassName, 2, Length(AField1.ClassName));
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
  LSave := AField1.AsDateTime;
  LDifferenceMessageActual:= '';
  AField1.AsDateTime:= AField1.AsDateTime + 1;
  LDifferenceMessageExpected:= MakeDifferenceMessage(AField1, AField2);
  Check(not AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals(LDifferenceMessageExpected, LDifferenceMessageActual);
  AField1.AsDateTime := LSave;
  LDifferenceMessageActual:= '';
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
end;

procedure TtiTestCase.CheckTIObjectEqualsWithMessage(
  const AObj1: TtiObject;
  const AObj2: TtiObject;
  const AField1: TtiFieldBoolean;
  const AField2: TtiFieldBoolean);
var
  LMessage: string;
  LSave: Boolean;
  LDifferenceMessageActual: string;
  LDifferenceMessageExpected: string;
begin
  LMessage := AObj1.ClassName + '.' + Copy(AField1.ClassName, 2, Length(AField1.ClassName));
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
  LSave := AField1.AsBoolean;
  LDifferenceMessageActual:= '';
  AField1.AsBoolean:= not AField1.AsBoolean;
  LDifferenceMessageExpected:= MakeDifferenceMessage(AField1, AField2);
  Check(not AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals(LDifferenceMessageExpected, LDifferenceMessageActual);
  AField1.AsBoolean := LSave;
  LDifferenceMessageActual:= '';
  Check(AObj1.Equals(AObj2, LDifferenceMessageActual), LMessage);
  CheckEquals('', LDifferenceMessageActual);
end;

function TtiTestCase.MakeDifferenceMessage(const AField1,
  AField2: TtiFieldAbs): string;
begin
  result:=
    AField1.FieldName + ': "' +
    AField1.AsString + '" -> "' +
    AField2.AsString + '"';
end;

procedure TtiTestCase.TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldInteger);
var
  LFieldName: string;
  LSaved: Integer;
begin
  Assert(AObj.TestValid, CTIErrorInvalidObject);
  Assert(AField.TestValid, CTIErrorInvalidObject);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsInteger;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsInteger := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldString);
var
  LFieldName: string;
  LSaved: string;
begin
  Assert(AObj.TestValid, CTIErrorInvalidObject);
  Assert(AField.TestValid, CTIErrorInvalidObject);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsString;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsString := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldFloat);
var
  LFieldName: string;
  LSaved: real;
begin
  Assert(AObj.TestValid, CTIErrorInvalidObject);
  Assert(AField.TestValid, CTIErrorInvalidObject);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsFloat;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsFloat := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

{$IFDEF OID_AS_INT64}
procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject;
  var AField1, AField2: TtiOID);
begin
  Assert(AObj1.TestValid, CTIErrorInvalidObject);
  Assert(AObj2.TestValid, CTIErrorInvalidObject);
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1 := (AField2 * 10) + 1;
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field');
  AField1 := AField2;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;
{$ELSE}
procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject;
  const AField1, AField2: TtiOID);
begin
  Assert(AObj1.TestValid, CTIErrorInvalidObject);
  Assert(AObj2.TestValid, CTIErrorInvalidObject);
  Assert(AField1.TestValid, CTIErrorInvalidObject);
  Assert(AField2.TestValid, CTIErrorInvalidObject);
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsString := AField2.AsString + '1';
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field');
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;
{$ENDIF}

procedure TtiTestCase.TestTIObjectEquals(const AObj1, AObj2: TtiObject;
  const AField1, AField2: TtiFieldDate);
var
  lFieldName: string;
begin
  Assert(AObj1.TestValid, CTIErrorInvalidObject);
  Assert(AObj2.TestValid, CTIErrorInvalidObject);
  Assert(AField1.TestValid, CTIErrorInvalidObject);
  Assert(AField2.TestValid, CTIErrorInvalidObject);
  lFieldName := AField1.FieldName;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
  AField1.AsDateTime := AField2.AsDateTime + 1;
  Check(not AObj1.Equals(AObj2), 'Equals returned TRUE when it should have returned FALSE after changing field ' + lFieldName);
  AField1.AsString := AField2.AsString;
  Check(AObj1.Equals(AObj2), 'Equals returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldBoolean);
var
  LFieldName: string;
  LSaved: Boolean;
begin
  Assert(AObj.TestValid, CTIErrorInvalidObject);
  Assert(AField.TestValid, CTIErrorInvalidObject);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsBoolean;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsBoolean := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

procedure TtiTestCase.TestTIObjectIsValid(const AObj: TtiObject; const AField: TtiFieldDateTime);
var
  LFieldName: string;
  LSaved: TDateTime;
begin
  Assert(AObj.TestValid, CTIErrorInvalidObject);
  Assert(AField.TestValid, CTIErrorInvalidObject);
  LFieldName := AField.FieldName;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
  LSaved := AField.AsDateTime;
  AField.IsNull := True;
  Check(not AObj.IsValid, 'IsValid returned TRUE when it should have returned FALSE after changing field ' + LFieldName);
  AField.AsDateTime := LSaved;
  Check(AObj.IsValid, 'IsValid returned FALSE when it should have returned True');
end;

//procedure TtiTestCase.CheckDeletionFromDatabase(const AListClass: TtiObjectClass);
//var
//  LList: TtiObjectList;
//begin
//  LList:= AListClass.Create as TtiObjectList;
//  try
//    LList.Read;
//    CheckEquals(1, LList.Count);
//    LList.Items[0].Deleted:= True;
//    LList.Items[0].Save;
//    CheckObjectState(posDeleted, LList.Items[0]);
//  finally
//    LList.Free;
//  end;
//
//  LList:= AListClass.Create as TtiObjectList;
//  try
//    LList.Read;
//    CheckEquals(0, LList.Count);
//  finally
//    LList.Free;
//  end;
//end;

procedure TtiTestCase.CheckEquals(const AExpected: string; AActual: integer);
begin
  CheckEquals(StrToInt(AExpected), AActual);
end;

procedure TtiTestCase.CheckEquals(const AValue: TStream; const AField: TStream);
var
  LMessage: string;
begin
  Check(True); // To Force OnCheckCalled to be called
  if not tiTestStreamsIdentical(AValue, AField, LMessage) then
    Fail(LMessage);
end;

procedure TtiTestCase.CheckEquals(const AValue: String; const AField: TtiFieldInteger);
begin
  CheckEquals(AValue, AField.AsString);
end;

procedure TtiTestCase.CheckNearEnough(const AExpected: TDateTime; const AField: TtiFieldDateTime);
begin
  CheckNearEnough(AExpected, AField.AsDateTime, AField.FieldName);
end;

procedure TtiTestCase.CheckNearEnough(const AExpected: Real; const AField: TtiFieldFloat);
begin
  CheckNearEnough(AExpected, AField.AsFloat, AField.FieldName);
end;

procedure TtiTestCase.CheckNearEnough(const AExpected, AActual: TtiFieldFloat);
begin
  CheckNearEnough(AExpected.AsFloat, AActual.AsFloat);
end;

procedure TtiTestCase.CheckTIObjectEqualsMethod(const AData1, AData2: TtiObject; const APropName, ANewValue: string);
var
  LSaved: string;
begin
  Assert(AData1.TestValid, CTIErrorInvalidObject);
  Assert(AData2.TestValid, CTIErrorInvalidObject);
  CheckEquals(True, AData1.Equals(AData2));
  LSaved:= AData1.PropValue[APropName];
  AData1.PropValue[APropName]:= ANewValue;
  CheckEquals(False, AData1.Equals(AData2));
  AData1.PropValue[APropName]:= LSaved;
  CheckEquals(True, AData1.Equals(AData2));
end;

procedure TtiTestCase.CheckTIObjectEqualsMethod(const AData1, AData2: TtiObject; const APropName: string; const ANewValue: Real);
var
  LSaved: Real;
begin
  Assert(AData1.TestValid, CTIErrorInvalidObject);
  Assert(AData2.TestValid, CTIErrorInvalidObject);
  CheckEquals(True, AData1.Equals(AData2));
  LSaved:= AData1.PropValue[APropName];
  AData1.PropValue[APropName]:= ANewValue;
  CheckEquals(False, AData1.Equals(AData2));
  AData1.PropValue[APropName]:= LSaved;
  CheckEquals(True, AData1.Equals(AData2));
end;

procedure TtiTestCase.CheckTIObjectIsValidMethod(const AData: TtiObject;
  const APropName: string; const AInvalidValue: String;
  const AErrorProperty: string);
var
  LSaved: String;
  LErrors: TtiObjectErrors;
  LErrorProperty: string;
begin
  Assert(AData.TestValid, CTIErrorInvalidObject);
  if AErrorProperty = '' then
    LErrorProperty:= APropName
  else
    LErrorProperty:= AErrorProperty;
  LErrors:= TtiObjectErrors.Create;
  try
    CheckEquals(True, AData.IsValid);
    LSaved:= AData.PropValue[APropName];
    AData.PropValue[APropName]:= AInvalidValue;
    LErrors.Clear;
    CheckEquals(False, AData.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(LErrorProperty, LErrors.Items[0].ErrorProperty);
    CheckNotEquals('', LErrors.Items[0].ErrorMessage);
    AData.PropValue[APropName]:= LSaved;
    LErrors.Clear;
    CheckEquals(True, AData.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);
  finally
    LErrors.Free;
  end;
end;

procedure TtiTestCase.CheckTIObjectIsValidMethod(const AData: TtiObject;
  const APropName: string; const AInvalidValue: Int64; const AErrorProperty: string = '');
var
  LSaved: Int64;
  LErrors: TtiObjectErrors;
  LErrorProperty: string;
begin
  Assert(AData.TestValid, CTIErrorInvalidObject);
  if AErrorProperty = '' then
    LErrorProperty:= APropName
  else
    LErrorProperty:= AErrorProperty;
  LErrors:= TtiObjectErrors.Create;
  try
    CheckEquals(True, AData.IsValid);
    LSaved:= AData.PropValue[APropName];
    AData.PropValue[APropName]:= AInvalidValue;
    LErrors.Clear;
    CheckEquals(False, AData.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(LErrorProperty, LErrors.Items[0].ErrorProperty);
    CheckNotEquals('', LErrors.Items[0].ErrorMessage);
    AData.PropValue[APropName]:= LSaved;
    LErrors.Clear;
    CheckEquals(True, AData.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);
  finally
    LErrors.Free;
  end;
end;

procedure TtiTestCase.CheckTIObjectIsValidMethod(const AData: TtiObject;
  const APropName: string; const AInvalidValue: Real; const AErrorProperty: string = '');
var
  LSaved: real;
  LErrors: TtiObjectErrors;
  LErrorProperty: string;
begin
  Assert(AData.TestValid, CTIErrorInvalidObject);
  if AErrorProperty = '' then
    LErrorProperty:= APropName
  else
    LErrorProperty:= AErrorProperty;
  LErrors:= TtiObjectErrors.Create;
  try
    CheckEquals(True, AData.IsValid);
    LSaved:= AData.PropValue[APropName];
    AData.PropValue[APropName]:= AInvalidValue;
    LErrors.Clear;
    CheckEquals(False, AData.IsValid(LErrors));
    CheckEquals(1, LErrors.Count);
    CheckEquals(LErrorProperty, LErrors.Items[0].ErrorProperty);
    CheckNotEquals('', LErrors.Items[0].ErrorMessage);
    AData.PropValue[APropName]:= LSaved;
    LErrors.Clear;
    CheckEquals(True, AData.IsValid(LErrors));
    CheckEquals(0, LErrors.Count);
  finally
    LErrors.Free;
  end;
end;

{ TtiPerformanceCounter }

constructor TtiPerformanceCounter.Create;
begin
  inherited;
  FInitialized:= false;
  FRunning:= false;
end;

procedure TtiPerformanceCounter.Initialize;
var
  LStart: TtiPerformanceCounterResultType;
  LStop: TtiPerformanceCounterResultType;
  LTime: TtiPerformanceCounterResultType;
  i: Cardinal;
  LDummy: TtiPerformanceCounterResultType;
  LLoopCount: TtiPerformanceCounterResultType;
begin
  LTime:= 0;
  LLoopCount:= 100;
  while LTime <= 500 do
  begin
    LLoopCount:= LLoopCount * 10;
    LDummy:= 1;
    LStart:= GetPerformanceValue;
    for i := 1 to LLoopCount do
      Inc(LDummy, LDummy);
    LStop:= GetPerformanceValue;
    LTime:= LStop-LStart;
  end;
  FPerformanceFactor:= LLoopCount div LTime div 10000000;
end;

procedure TtiPerformanceCounter.Start;
begin
  Assert(not FRunning, 'Currently running');
  Initialize;
  FStart:= GetPerformanceValue;
  FRunning:= true;
end;

procedure TtiPerformanceCounter.Stop;
begin
  Assert(FRunning, 'Not running');
  FStop:= GetPerformanceValue;
  FRunning:= false;
end;

function TtiPerformanceCounter.GetPerformanceValue: TtiPerformanceCounterResultType;
begin
  result:= tiGetTickCount;
end;

function TtiPerformanceCounter.GetTickCount: TtiPerformanceCounterResultType;
begin
  Assert(not FRunning, 'Still running');
  result:= (FStop - FStart) * FPerformanceFactor;
end;

procedure TtiTestCase.CheckEquals(const AField: TtiFieldDateTime;
  const AValue: TDateTime);
begin
  CheckNearEnough(AField.AsDateTime, AValue);
end;

procedure TtiTestCase.CheckEquals(const AField: TtiFieldInteger;
  const AValue: Integer);
begin
  CheckEquals(AField.AsInteger, AValue);
end;

procedure TtiTestCase.CheckEquals(const AField: TtiFieldFloat;
  const AValue: Real);
begin
  CheckEquals(AField.AsFloat, AValue);
end;

procedure TtiTestCase.CheckEquals(const AField: TtiFieldBoolean;
  const AValue: Boolean);
begin
  CheckEquals(AField.AsBoolean, AValue);
end;

procedure TtiTestCase.CheckEquals(const AField: TtiFieldString;
  const AValue: string);
begin
  CheckEquals(AField.AsString, AValue);
end;

{ TtiTestCaseWithDatabaseConnection }


{ TtiTestCaseWithTestSetupData }

procedure TtiTestCase.CheckEquals(const AExpected: string; const AOID: TtiOID; const AMessage: string = '');
begin
  {$IFNDEF OID_AS_INT64}
  CheckEquals(AExpected, AOID.AsString, AMessage);
  {$ELSE}
  CheckEquals(StrToInt(AExpected), AOID, AMessage);
  {$ENDIF}
end;

{$IFNDEF OID_AS_INT64}
procedure TtiTestCase.CheckEquals(const AExpected, AActual: TtiOID);
begin
  Check(AExpected.Equals(AActual),
    'Expected "' + AExpected.AsString + '" but found "' + AActual.AsString + '"');
end;
{$ENDIF}

procedure TtiTestCase.CheckExceptionMessage(const AMessage: string;
  const AArgs: array of const; const AException: Exception);
begin
  CheckExceptionMessage(Format(AMessage, AArgs), AException);
end;

procedure TtiTestCase.CheckExceptionMessage(const AMessage: string;
  const AException: Exception);
begin
  Check(Pos(UpperCase(AMessage), UpperCase(AException.Message)) <> 0,
         'Expected "' + AMessage +'" in exception message but found "' +
         AException.message + '"');
end;

{ TtiTestSetup }

constructor TtiTestSetup.Create(const ATestCase: TtiTestCase);
begin
  Assert(Assigned(ATestCase), CTIErrorInvalidObject);
  inherited Create;
  FTestCase := ATestCase;
end;

function TtiTestSetup.tvToBoolean(const AValue: string; const AInc: integer = 0): Boolean;
begin
  result := ( tvToInt(AValue, AInc) mod 2 ) = 0 ;
end;

function TtiTestSetup.tvToDateTime(const AValue: string; const AInc: integer = 0): TDateTime;
begin
  result := Now + tvToInt(AValue, AInc);
end;

function TtiTestSetup.tvToFloat(const AValue: string; const AInc: integer = 0): Real;
var
  lInt : Integer ;
begin
  lInt := tvToInt(AValue, AInc);
  Result := lInt + Frac(lInt / 10) + Frac(lInt / 100);
end;

function TtiTestSetup.tvToInt(const AValue: string; const AInc: integer = 0): Int64;
begin
  Result := StrToInt64(AValue) + AInc;
end;

function TtiTestSetup.tvToIntWithLimit(const AValue: string;
  const ALimit: integer): Int64;
begin
  if ALimit = 0 then
    Result := StrToInt64(AValue)
  else
    Result := StrToInt64(tiPad0(AValue, ALimit));
end;

function TtiTestSetup.tvToStr(const AValue: string;
  const AInc: integer): string;
begin
  result:= IntToStr(tvToInt(AValue, AInc));
end;

procedure TtiTestSetup.tvToStream(
  const AStream: TStream;
  const AValue: string);
var
  L: string;
begin
  L:= tiReplicate(AValue, 1000);
  tiStringToStream(L, AStream);
end;

function TtiTestSetup.tvToDate(const AValue: string; const AInc: integer = 0): TDateTime;
begin
  result := Date + tvToInt(AValue, AInc);
end;

procedure TtiTestSetup.UnderConstruction(const AMessage: string = '');
begin
{$IFNDEF IGNORE_UNDER_CONSTRUCTION}
  if AMessage = '' then
    TC.Fail('Under construction', CallerAddr)
  else
    TC.Fail(AMessage, CallerAddr);
{$ENDIF}
end;

initialization
  // Slow to create every test, so recycle one instance
  ULongString:= tiCreateStringOfSize(3000);

end.



