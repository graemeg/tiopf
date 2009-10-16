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
  {$IFDEF FPC}
  ,fpcunit
  ,testregistry
  ,testdecorator
  {$ELSE}
  ,TestFramework
  ,TestExtensions
  {$ENDIF}
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
    function GetLongString: string;
  protected
    procedure   SetUpOnce; {$IFDEF FPC}virtual;{$ELSE}override;{$ENDIF}
    procedure   SetUp; override;
    procedure   TearDown; override;
    procedure   TearDownOnce; {$IFDEF FPC}virtual;{$ELSE}override;{$ENDIF}
    function    TempFileName(const AFilename: string = ''): string;
    property    LongString : string read GetLongString;
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
    function  AreStreamContentsSame(const pStream1, pStream2 : TStream; var AMessage : string): boolean;
    procedure CheckStreamContentsSame(const pStream1, pStream2 : TStream);
    procedure CheckFormattedMessage(const AFormat: string; const AArgs: array of const; const AActual: string; const AMessage: string = '');
    procedure CheckObjectState(const AObjectState : TPerObjectState; const AData : TtiObject; const AMessage: string = '');
    procedure CheckNearEnough(AExpected, AActual: Extended; const AMessage: string = ''); overload;
    procedure CheckNearEnough(const AExpected, AActual: Double); overload;
    procedure CheckNearEnough(const AExpected, AActual: Double; const AMessage: string); overload;
    procedure CheckNearEnough(const AExpected, AActual: Double; const AMessage: string; pArgs: array of const); overload;
    procedure CheckNearEnough(const AExpected, AActual, AFEPS: Double); overload;
    procedure CheckNearEnough(const AExpected, AActual, AFEPS: Double; const AString: string); overload;
    procedure CheckNearEnough(const AExpected: TDateTime; const AField: TtiFieldDateTime); overload;
    procedure CheckNearEnough(const AExpected: Real; const AField: TtiFieldFloat); overload;
    procedure CheckNearEnough(const AExpected, AActual: TtiFieldFloat); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const APropName: String); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldString); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldInteger); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldFloat); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldDateTime); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldDate); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldBoolean); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiOID); overload;
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
  ,StrUtils
  ,TypInfo
  {$IFDEF MSWINDOWS}
  ,Windows
  ,tiConsoleApp
  {$ENDIF}
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

procedure TtiTestCase.CheckStreamContentsSame(const pStream1, pStream2: TStream);
var
  lResult : boolean;
  lMessage : string;
begin
  Check(True); // To Force OnCheckCalled to be called
  lMessage := '';
  lResult := AreStreamContentsSame(pStream1, pStream2, lMessage);
  Check(lResult,  lMessage);
end;


function TtiTestCase.AreStreamContentsSame(const pStream1, pStream2: TStream; var AMessage : string): boolean;
var
   LByte1, LByte2 : byte;
begin
   result := true;

   if pStream1.Size <> pStream2.Size then
   begin
     result := false;
     AMessage := 'Streams have different sizes ('+inttostr(pStream1.Size)+'/'+
                 inttostr(pStream2.Size)+')';
     exit; //==>
   end;

   pStream1.Position := 0;
   pStream2.Position := 0;
   while (pStream1.Size - pStream1.Position > 0) do
   begin
     pStream1.Read(LByte1, 1);
     pStream2.Read(LByte2, 1);
     if LByte1 <> LByte2 then
     begin
       result := false;
       AMessage := 'Streams Differ at position '+
                   inttostr(pStream1.Position)+' of '+
                   inttostr(pStream1.Size)+
                   ':'+inttostr(LByte1)+'/'+inttostr(LByte2);
       exit; //==>
     end;
   end;
end;

procedure TtiTestCase.Check(const ACondition: Boolean; AMessage: string; const AArgs: array of const);
begin
  Check(ACondition, Format(AMessage, AArgs));
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

procedure TtiTestCase.CheckFormattedMessage(const AFormat: string;
  const AArgs: array of const; const AActual: string;
  const AMessage: string = '');
begin
  CheckEquals(Format(AFormat, AArgs), AActual, AMessage);
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

initialization
  // Slow to create every test, so recycle one instance
  ULongString:= tiCreateStringOfSize(3000);

end.



