unit tiTestFramework;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiOPFTestManager
  ,tiObject
  ,tiQuery
  ,tiExcept
  ,tiOPFManager
  ,tiPersistenceLayers
  ,tiDBConnectionPool
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
  CErrorPublishedPropertyCount = 'Count of published properties did not return ' +
    'the expected value. The number of published properties has been changed ' +
    'without the unit tests being updated';

  cTableNameTestGroup               = 'Test_Group';

  cTableNameTIOPFTestChild_A        = 'Test_Child_a';
  cTableNameTIOPFTestChild_B        = 'Test_Child_b';
  cTableNameTIOPFTestParent         = 'Test_Parent';

  cTableNameCreateTable             = 'Test_Create_Table';
  cTableNameTIOPFTestChildGrouped_A = 'Test_Child_Grouped_a';
  cTableNameTIOPFTestChildGrouped_B = 'Test_Child_Grouped_b';
  cTableNameTIOPFTestParentGrouped  = 'Test_Parent_Grouped'; // The objects's parent table
  cTableNameTIOPFTestParentGroup    = 'Test_Parent_Group';  // The collection table

  cFieldAs_Index                = 1;

  cTIQueryTableName             = 'TIQueryTable';
  cTIQueryTableNameInt64        = 'TIQueryTableInt64';
  cTIQueryColName               = 'TIQueryField';

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
    procedure   SetUp; override;
    procedure   TearDown; override;
    function    TempFileName(const AFilename: string = ''): string;
    property    LongString : string read GetLongString;
    function    tstIntToStr(pInt:Integer):string;
    function    tstIntToBool(pInt:Integer): boolean;
    function    tstIntToFloat(pInt:Integer): Extended;
    function    tstIntToDateTime(pInt:Integer): TDateTime;
    procedure   tstIntToStream(pInt:Integer; const AStream : TStream);
    procedure   LoadConfiguration(const iniFile :TCustomIniFile;
                                  const Section :string); {$IFNDEF FPC}override;{$ENDIF}
  public
    {$IFDEF FPC}
    constructor Create; override;
    function    GetName: string; virtual;
    property    Name: string read GetName;
      // DUnit compatibility interface
      {$I DUnitCompatableInterface.inc}
    {$ELSE}
      constructor Create {$IFNDEF DUNIT2}(AMethodName: string){$ENDIF}; override;
    {$ENDIF}
    destructor Destroy; override;
    class function TempDirectory: string;
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
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldString); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldInteger); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldFloat); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldDateTime); overload;
    procedure TestTIObjectEquals(const AObj1, AObj2: TtiObject; const AField1, AField2: TtiFieldBoolean); overload;
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
//    {: Check a delete visitor is working by creating an instance of TtiObjectClass (which must be a list), then calling it's read method.
//       There must be one object in the list. This object will be marked as deleted, then saved and the list will be re-read. The
//       list must be empty on the second read.}
//    procedure CheckDeletionFromDatabase(const AListClass: TtiObjectClass);
    procedure CheckINIFileEntry(const AExpected: TDateTime; const AINIFileName, AINISection, AINIIdent: string);


  end;

  TtiTestCaseClass    = class of TtiTestCase;

  TtiTestCaseWithTestSetupData = class(TtiTestCase)
  private
    FtiOPFTestSetupData: TtiOPFTestSetupData;
  protected
    procedure   SetUpOnce; override;
  public
    class function PersistenceLayerName: string; virtual; abstract;
    property    PerFrameworkSetup : TtiOPFTestSetupData read FtiOPFTestSetupData;
  end;

  TtiTestCaseWithPersistenceLayer = class(TtiTestCaseWithTestSetupData)
  private
    function    GetPersistenceLayerName: string;
    function    GetPersistenceLayer: TtiPersistenceLayer;
  protected
    procedure   CheckObjectState(AObjectState: TPerObjectState; const AData : TtiObject);
    procedure   CheckExceptionMessage(const AMessage : string; const AException : Exception);

    procedure   CreateDBIfNotExists;
    procedure   WriteTimingResult(const pAction, APersistenceLayerName : string; AValue : Extended);

    property    PersistenceLayer: TtiPersistenceLayer read GetPersistenceLayer;
  public

    function    GetName: string; override;
  end;

  TtiTestCaseWithPersistenceLayerClass    = class of TtiTestCaseWithPersistenceLayer;


  TtiTestCaseWithDatabaseConnection = class(TtiTestCaseWithPersistenceLayer)
  private
    FDBConnectionPool: TtiDBConnectionPool;
    function    GetDatabaseName: string;
    function    GetUserName: string;
    function    GetPassword: string;
  protected
    procedure SetupOnce; override;
    procedure TearDownOnce; override;

    function PersistenceLayerSupportsMultiUser: boolean;
    function PersistenceLayerSupportsSQL: boolean;

    property DBConnectionPool: TtiDBConnectionPool read FDBConnectionPool;

    procedure   SetupTestTables; overload;
    procedure   DeleteTestTables; overload;
    procedure   SetupTestTables(const AManager: TtiOPFManager); overload;
    procedure   DeleteTestTables(const AManager: TtiOPFManager); overload;
    procedure   EmptyTestTables;
    procedure   DropTestTable;
    procedure   DropTable(const ATableName: string);

    procedure   CreateTableTestBin( const ADatabase: TtiDatabase = nil);
    procedure   CreateTableTestGroup(const ADatabase: TtiDatabase = nil);
    procedure   InsertIntoTestGroup(const ADatabase: TtiDatabase; AValue : integer);
    procedure   DropTableTestBin(   const ADatabase: TtiDatabase = nil);
    procedure   DropTableTestGroup( const ADatabase: TtiDatabase = nil);
    procedure   CreateTable(const   ATable : TtiDBMetaDataTable);

    procedure   DropNextOIDTable;
    procedure   CreateNextOIDIntTable;
    procedure   CreateNextOIDStrTable;
    procedure   InsertRow(const ATableName: string; AParams: TtiQueryParams);


    // ToDo: These overloaded method are confusing. Fix.
    procedure   CreateTableInteger(   const ADatabaseName, APersistenceLayerName : string);overload;
//    procedure   CreateTableInt64(     const ADatabaseName, APersistenceLayerName : string);overload;
    procedure   CreateTableString(    const ADatabaseName, APersistenceLayerName : string);overload;
    procedure   CreateTableFloat(     const ADatabaseName, APersistenceLayerName : string);overload;
    procedure   CreateTableBoolean(   const ADatabaseName, APersistenceLayerName : string);overload;
    procedure   CreateTableDateTime(  const ADatabaseName, APersistenceLayerName : string);overload;
    procedure   CreateTableLongString(const ADatabaseName, APersistenceLayerName : string);overload;
    procedure   CreateTableStream(    const ADatabaseName, APersistenceLayerName : string);overload;
    procedure   CreateTableInteger(   const ADatabase : TtiDatabase);overload;
//    procedure   CreateTableInt64(     const ADatabase : TtiDatabase);overload;
    procedure   CreateTableString(    const ADatabase : TtiDatabase);overload;
    procedure   CreateTableFloat(     const ADatabase : TtiDatabase);overload;
    procedure   CreateTableBoolean(   const ADatabase : TtiDatabase);overload;
    procedure   CreateTableDateTime(  const ADatabase : TtiDatabase);overload;
    procedure   CreateTableLongString(const ADatabase : TtiDatabase);overload;
    procedure   CreateTableStream(    const ADatabase : TtiDatabase);overload;


  public
    property    DatabaseName     : string read GetDatabaseName;
    property    UserName         : string read GetUserName;
    property    Password         : string read GetPassword;
  end;

  TtiTestCaseWithTables = class(TtiTestCaseWithDatabaseConnection)
  protected
    procedure SetUp; override;
    procedure TearDown; override;

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
  ,tiConstants
  ,tiINI
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

constructor TtiTestCase.Create{$IFNDEF DUNIT2ORFPC}(AMethodName: string){$ENDIF};
begin
  inherited;
end;


{$IFDEF FPC}
// DUnit compatibility interface
function TtiTestCase.GetName: string;
begin
  Result := TestName;
end;
{$ENDIF}


procedure TtiTestCase.SetUp;
begin
  inherited;
  // ToDo: Must look into this - sometimes TempDirectory contains a file that's locked
  FTempDirectory := TempDirectory;
  if not DirectoryExists(FTempDirectory) then
    try tiForceDirectories(FTempDirectory) except end;
end;

procedure TtiTestCase.TearDown;
begin
  inherited;
  // ToDo: Must look into this - sometimes TempDirectory contains a file that's locked
  if DirectoryExists(FTempDirectory) then
    try tiForceRemoveDir(FTempDirectory) except end;
  uTempPath := '';
  FTempDirectory := '';
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


{$IFDEF FPC}
  // DUnit compatibility interface
  {$I DUnitCompatableInterface.inc}
{$ENDIF}


procedure TtiTestCaseWithDatabaseConnection.DropTableTestGroup(const ADatabase : TtiDatabase = nil);
var
  LDatabase: TtiDatabase;
begin
  if ADatabase = nil then
    try
      LDatabase:= DBConnectionPool.Lock;
      try
        LDatabase.DropTable(cTableNameTestGroup);
      finally
        DBConnectionPool.UnLock(LDatabase);
      end;
    except end
  else
    try ADatabase.DropTable(cTableNameTestGroup) except end;
end;


procedure TtiTestCaseWithDatabaseConnection.DropTestTable;
var
  LDatabase: TtiDatabase;
begin
  try
      LDatabase:= DBConnectionPool.Lock;
      try
        LDatabase.DropTable(cTIQueryTableName);
      finally
        DBConnectionPool.UnLock(LDatabase);
      end;
  except
    // Swallow
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.DeleteTestTables;
begin
  DropTableTestGroup;
  DropTableTestBin;
  DropTable('test_item');
  DropTable(cTableNameTIOPFTestChild_A);
  DropTable(cTableNameTIOPFTestChild_B);
  DropTable(cTableNameTIOPFTestParent);
  DropTable(cTableNameTIOPFTestChildGrouped_A);
  DropTable(cTableNameTIOPFTestChildGrouped_B);
  DropTable(cTableNameTIOPFTestParentGrouped);
  DropTable(cTableNameTIOPFTestParentGroup);

end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableTestGroup(const ADatabase : TtiDatabase = nil) ;
var
  lTable : TtiDBMetaDataTable;
begin
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTableNameTestGroup;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Group_Str_Field',   qfkString, 10);
    lTable.AddField('Group_Int_Field',   qfkInteger);
    lTable.AddField('Group_Float_Field', qfkFloat);
    lTable.AddField('Group_Date_Field',  qfkDateTime);
    lTable.AddField('Group_Bool_Field',  qfkLogical);
    lTable.AddField('Group_Notes_Field', qfkLongString);
    if ADatabase = nil then
      CreateTable(lTable)
    else
      ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.InsertIntoTestGroup(const ADatabase: TtiDatabase; AValue : integer);
var
  lParams : TtiQueryParams;
  e: extended;
begin
  e := 1000;  // removes compiler warning
  Check(LongString <> '', 'FLongString not assigned');
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString('OID', IntToStr(AValue));
    lParams.SetValueAsString('Group_Str_Field', tiPad0(IntToStr(AValue), 10));
    lParams.SetValueAsInteger('Group_Int_Field', AValue);
    lParams.SetValueAsFloat('Group_Float_Field', StrToInt(tiReplicate(IntToStr(AValue), 4)) / e);
    lParams.SetValueAsDateTime('Group_Date_Field', EncodeDate(1900, 1, AValue)) ;
    lParams.SetValueAsBoolean('Group_Bool_Field', ((AValue mod 2) = 0)) ;
    lParams.SetValueAsString('Group_Notes_Field', LongString) ;
    ADatabase.InsertRow(cTableNameTestGroup, lParams);
    // Each test takes place inside a transaction.
    // If you want the test data to be setup inside it's own
    // transaction, then uncomment these lines.
    // FDatabase.Commit;
    // FDatabase.StartTransaction;
  finally
    lParams.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.InsertRow(const ATableName: string;
  AParams: TtiQueryParams);
var
  LDatabase: TtiDatabase;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    LDatabase.InsertRow(ATableName, AParams);
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableTestBin(const ADatabase : TtiDatabase = nil);
var
  lTable : TtiDBMetaDataTable;
begin
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := 'Test_Bin';
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Item_Binary_Field', qfkBinary );
    if ADatabase = nil then
      CreateTable(lTable)
    else
      ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.DropTable(const ATableName: string);
var
  LDatabase: TtiDatabase;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    try
      LDatabase.DropTable(ATableName);
    except end;
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.DropTableTestBin(const ADatabase : TtiDatabase = nil);
begin
  if ADatabase = nil then
    DropTable('test_bin')
  else
    try ADatabase.DropTable('test_bin'  ) except end;
end;


procedure TtiTestCaseWithPersistenceLayer.CreateDBIfNotExists;
var
  LPersistenceLayer  : TtiPersistenceLayer;
  LDatabaseClass : TtiDatabaseClass;
begin
  if PerFrameworkSetup.CanCreateDatabase then
  begin
    LPersistenceLayer := gTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(PersistenceLayerName);
    Assert(LPersistenceLayer <> nil, 'Unable to find registered persistence layer <' + PersistenceLayerName +'>');
    LDatabaseClass := LPersistenceLayer.DatabaseClass;
    if not LDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password) then
    begin
      LDatabaseClass.CreateDatabase(
        PerFrameworkSetup.DBName,
        PerFrameworkSetup.Username,
        PerFrameworkSetup.Password);
      if not LDatabaseClass.DatabaseExists(
        PerFrameworkSetup.DBName,
        PerFrameworkSetup.Username,
        PerFrameworkSetup.Password) then
        EtiOPFDUnitException.Create('Unable to create database <' + PerFrameworkSetup.DBName + '>');
    end;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.EmptyTestTables;
var
  LDatabase: TtiDatabase;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    LDatabase.DeleteRow('test_item',    nil);
    LDatabase.DeleteRow(cTableNameTestGroup,   nil);
    LDatabase.DeleteRow('test_bin',     nil);
    LDatabase.DeleteRow(cTableNameTIOPFTestChild_A, nil);
    LDatabase.DeleteRow(cTableNameTIOPFTestChild_B, nil);
    LDatabase.DeleteRow('test_parent',  nil);
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;




procedure TtiTestCaseWithDatabaseConnection.SetupTestTables(const AManager: TtiOPFManager);
var
  lTable : TtiDBMetaDataTable;
  LDatabase: TtiDatabase;
begin
  DeleteTestTables(AManager);
  LDatabase:= AManager.DefaultDBConnectionPool.Lock;
  try
    CreateTableTestGroup(LDatabase);
  finally
    AManager.DefaultDBConnectionPool.UnLock(LDatabase);
  end;
  lTable := TtiDBMetaDataTable.Create;
  try

    lTable.Clear;
    lTable.Name := 'Test_Item';
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    lTable.AddField('OID_Group',          qfkString, 36); // Should be Not Null & FK
    lTable.AddField('Item_Str_Field',     qfkString, 10);
    lTable.AddField('Item_Int_Field',     qfkInteger);
    lTable.AddField('Item_Float_Field',   qfkFloat);
    lTable.AddField('Item_Bool_Field',    qfkLogical);
    lTable.AddField('Item_Date_Field',    qfkDateTime);
    lTable.AddField('Item_Notes_Field',   qfkLongString);
    AManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParent;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Owner_OID',          qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Parent_Str_Field',   qfkString, 10);
    AManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChild_A;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    AManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChild_B;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    AManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParentGrouped;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Owner_OID',          qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Parent_Str_Field',   qfkString, 10);
    AManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChildGrouped_A;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    AManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChildGrouped_B;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    AManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParentGroup;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    AManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName);

  finally
    lTable.Free;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.SetupTestTables;
var
  lTable : TtiDBMetaDataTable;
begin
  DeleteTestTables;
  CreateTableTestGroup;
  CreateTableTestBin;
  lTable := TtiDBMetaDataTable.Create;
  try

    lTable.Clear;
    lTable.Name := 'Test_Item';
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    lTable.AddField('OID_Group',          qfkString, 36); // Should be Not Null & FK
    lTable.AddField('Item_Str_Field',     qfkString, 10);
    lTable.AddField('Item_Int_Field',     qfkInteger);
    lTable.AddField('Item_Float_Field',   qfkFloat);
    lTable.AddField('Item_Bool_Field',    qfkLogical);
    lTable.AddField('Item_Date_Field',    qfkDateTime);
    lTable.AddField('Item_Notes_Field',   qfkLongString);
    CreateTable(lTable);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParent;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Owner_OID',          qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Parent_Str_Field',   qfkString, 10);
    CreateTable(lTable);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChild_A;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    CreateTable(lTable);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChild_B;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    CreateTable(lTable);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParentGrouped;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Owner_OID',          qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Parent_Str_Field',   qfkString, 10);
    CreateTable(lTable);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChildGrouped_A;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    CreateTable(lTable);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChildGrouped_B;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    CreateTable(lTable);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParentGroup;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    CreateTable(lTable);

  finally
    lTable.Free;
  end;
end;


function TtiTestCaseWithDatabaseConnection.GetDatabaseName: string;
begin
  Assert(PerFrameworkSetup.TestValid, CTIErrorInvalidObject);
  result := PerFrameworkSetup.DBName;
end;


procedure TtiTestCaseWithDatabaseConnection.DeleteTestTables(const AManager: TtiOPFManager);
var
  LDatabase: TtiDatabase;
begin
  LDatabase:= AManager.DefaultDBConnectionPool.Lock;
  try
    DropTableTestGroup(LDatabase);
  finally
    AManager.DefaultDBConnectionPool.UnLock(LDatabase);
  end;

  try AManager.DropTable('test_item',
                           PerFrameworkSetup.DBName,
                           PerFrameworkSetup.PersistenceLayerName)  except end;

  try AManager.DropTable(cTableNameTIOPFTestChild_A, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName)  except end;
  try AManager.DropTable(cTableNameTIOPFTestChild_B, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName)  except end;
  try AManager.DropTable(cTableNameTIOPFTestParent, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName)  except end;

  try AManager.DropTable(cTableNameTIOPFTestChildGrouped_A, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName)  except end;
  try AManager.DropTable(cTableNameTIOPFTestChildGrouped_B, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName)  except end;
  try AManager.DropTable(cTableNameTIOPFTestParentGrouped, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName)  except end;
  try AManager.DropTable(cTableNameTIOPFTestParentGroup, PerFrameworkSetup.DBName, PerFrameworkSetup.PersistenceLayerName)  except end;
end;

procedure TtiTestCaseWithDatabaseConnection.DropNextOIDTable;
begin
  DropTable('Next_OID');
end;


procedure TtiTestCaseWithDatabaseConnection.CreateNextOIDIntTable;
var
  lTable : TtiDBMetaDataTable;
  lParams : TtiQueryParams;
begin

  DropNextOIDTable;

  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := 'Next_OID';
    lTable.AddField('OID', qfkInteger);
    CreateTable(lTable);
  finally
    lTable.Free;
  end;

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsInteger('OID', 1000000);
    InsertRow('Next_OID', lParams);
  finally
    lParams.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateNextOIDStrTable;
var
  lTable : TtiDBMetaDataTable;
  lParams : TtiQueryParams;
begin

  DropNextOIDTable;

  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := 'Next_OID';
    lTable.AddField('OID', qfkString, 10);
    CreateTable(lTable);
  finally
    lTable.Free;
  end;

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString('OID', '0');
    InsertRow('Next_OID', lParams);
  finally
    lParams.Free;
  end;

end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableBoolean(const ADatabaseName, APersistenceLayerName : string);
var
  LPersistenceLayer: TtiPersistenceLayer;
  LDatabase: TtiDatabase;
begin
  LPersistenceLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(APersistenceLayerName);
  LDatabase:= LPersistenceLayer.DBConnectionPools.Lock(ADatabaseName);
  try
    CreateTableBoolean(LDatabase);
  finally
    LPersistenceLayer.DBConnectionPools.UnLock(ADatabaseName, LDatabase);
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTable(
  const ATable: TtiDBMetaDataTable);
var
  LDatabase: TtiDatabase;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    LDatabase.CreateTable(ATable);
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableBoolean(const ADatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTIQueryTableName;
    lTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    lTable.AddField(cTIQueryColName, qfkLogical);
    ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableDateTime(const ADatabaseName, APersistenceLayerName : string);
var
  LPersistenceLayer: TtiPersistenceLayer;
  LDatabase: TtiDatabase;
begin
  LPersistenceLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(APersistenceLayerName);
  LDatabase:= LPersistenceLayer.DBConnectionPools.Lock(ADatabaseName);
  try
    CreateTableDateTime(LDatabase);
  finally
    LPersistenceLayer.DBConnectionPools.UnLock(ADatabaseName, LDatabase);
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableDateTime(const ADatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTIQueryTableName;
    lTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    lTable.AddField(cTIQueryColName, qfkDateTime);
    ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableFloat(const ADatabaseName, APersistenceLayerName : string);
var
  LPersistenceLayer: TtiPersistenceLayer;
  LDatabase: TtiDatabase;
begin
  LPersistenceLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(APersistenceLayerName);
  LDatabase:= LPersistenceLayer.DBConnectionPools.Lock(ADatabaseName);
  try
    CreateTableFloat(LDatabase);
  finally
    LPersistenceLayer.DBConnectionPools.UnLock(ADatabaseName, LDatabase);
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableFloat(const ADatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTIQueryTableName;
    lTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    lTable.AddField(cTIQueryColName, qfkFloat);
    ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableInteger(const ADatabaseName, APersistenceLayerName : string);
var
  lTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTIQueryTableName;
    lTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    lTable.AddField(cTIQueryColName, qfkInteger);
    CreateTable(LTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableInteger(const ADatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTIQueryTableName;
    lTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    lTable.AddField(cTIQueryColName, qfkInteger);
    ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableLongString(const ADatabaseName, APersistenceLayerName : string);
var
  LPersistenceLayer: TtiPersistenceLayer;
  LDatabase: TtiDatabase;
begin
  LPersistenceLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(APersistenceLayerName);
  LDatabase:= LPersistenceLayer.DBConnectionPools.Lock(ADatabaseName);
  try
    CreateTableLongString(LDatabase);
  finally
    LPersistenceLayer.DBConnectionPools.UnLock(ADatabaseName, LDatabase);
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableLongString(const ADatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTIQueryTableName;
    lTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    lTable.AddField(cTIQueryColName, qfkLongString);
    ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableStream(const ADatabaseName, APersistenceLayerName : string);
var
  LPersistenceLayer: TtiPersistenceLayer;
  LDatabase: TtiDatabase;
begin
  LPersistenceLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(APersistenceLayerName);
  LDatabase:= LPersistenceLayer.DBConnectionPools.Lock(ADatabaseName);
  try
    CreateTableStream(LDatabase);
  finally
    LPersistenceLayer.DBConnectionPools.UnLock(ADatabaseName, LDatabase);
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableStream(const ADatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTIQueryTableName;
    lTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    lTable.AddField(cTIQueryColName, qfkBinary);
    ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableString(const ADatabaseName, APersistenceLayerName : string);
var
  LPersistenceLayer: TtiPersistenceLayer;
  LDatabase: TtiDatabase;
begin
  LPersistenceLayer:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(APersistenceLayerName);
  LDatabase:= LPersistenceLayer.DBConnectionPools.Lock(ADatabaseName);
  try
    CreateTableString(LDatabase);
  finally
    LPersistenceLayer.DBConnectionPools.UnLock(ADatabaseName, LDatabase);
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.CreateTableString(const ADatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := cTIQueryTableName;
    lTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    lTable.AddField(cTIQueryColName, qfkString, 255);
    ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TtiTestCaseWithPersistenceLayer.WriteTimingResult(const pAction, APersistenceLayerName : string; AValue : Extended);
var
  lFileName : string;
  lINI : TINIFile;
begin
  lFileName := ExtractFilePath(ParamStr(0)) + PathDelim + 'DataAccessTimingResults.txt';
  lINI := TINIFile.Create(lFileName);
  try
    lINI.WriteString(cCompilerName + ' ' + APersistenceLayerName,
                     pAction, Trim(Format('%12.2f', [AValue])));
  finally
    lINI.Free;
  end;
end;


procedure TtiTestCaseWithPersistenceLayer.CheckObjectState(AObjectState: TPerObjectState; const AData: TtiObject);
begin
  Assert(AData.TestValid(TtiObject), CTIErrorInvalidObject);
  Check(AData.ObjectState = AObjectState,
        'ObjectState. Expected ' +
        AData.ClassName + '.' + ObjectStateToString(AObjectState) +
        ' but is ' +
        AData.ObjectStateAsString);
end;


procedure TtiTestCaseWithPersistenceLayer.CheckExceptionMessage(const AMessage: string;
  const AException: Exception);
begin
  Check(Pos(UpperCase(AMessage), UpperCase(AException.Message)) <> 0,
         'Expected "' + AMessage +'" in exception message but found "' +
         AException.message + '"');
end;


function TtiTestCaseWithDatabaseConnection.GetPassword: string;
begin
  Assert(PerFrameworkSetup.TestValid, CTIErrorInvalidObject);
  result := PerFrameworkSetup.Password;
end;


function TtiTestCaseWithDatabaseConnection.GetUserName: string;
begin
  Assert(PerFrameworkSetup.TestValid, CTIErrorInvalidObject);
  result := PerFrameworkSetup.Username;
end;


function TtiTestCaseWithPersistenceLayer.GetName: string;
begin
  result := '[' + PersistenceLayerName + '] ' + inherited GetName;
end;


function TtiTestCaseWithPersistenceLayer.GetPersistenceLayerName: string;
begin
  Assert(FtiOPFTestSetupData.TestValid, CTIErrorInvalidObject);
  result:= FtiOPFTestSetupData.PersistenceLayerName;
end;


procedure TtiTestCase.CheckFormattedMessage(const AFormat: string;
  const AArgs: array of const; const AActual: string;
  const AMessage: string = '');
begin
  CheckEquals(Format(AFormat, AArgs), AActual, AMessage);
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

procedure TtiTestCase.CheckLessThan(const AMustBeLessThan, AValue: Int64);
const
  CCheckLessThanMessage =
    'Must be less than "%d" but was "%d"';
begin
  Check(AValue<AMustBeLessThan, CCheckLessThanMessage, [AMustBeLessThan, AValue]);
end;

procedure TtiTestCase.CheckNearEnough(AExpected, AActual: Extended; const AMessage: string = '');
begin
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

destructor TtiTestCase.Destroy;
begin
  FreeAndNil(FPerformanceCounter);
  inherited;
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
  if not tiIsNearEnough(AExpected, AActual, AFEPS) then
    FailNotEquals(FloatToStr(AExpected), FloatToStr(AActual), Format('(feps: %g) %s', [AFEPS, AString]), {$IFDEF FPC} nil {$ELSE} CallerAddr {$ENDIF});
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

function TtiTestCaseWithDatabaseConnection.PersistenceLayerSupportsMultiUser: boolean;
var
  LDefaults: TtiPersistenceLayerDefaults;
begin
  LDefaults:= TtiPersistenceLayerDefaults.Create;
  try
    PersistenceLayer.AssignPersistenceLayerDefaults(LDefaults);
    Result:= LDefaults.CanSupportMultiUser;
  finally
    LDefaults.Free;
  end;
end;

function TtiTestCaseWithDatabaseConnection.PersistenceLayerSupportsSQL: boolean;
var
  LDefaults: TtiPersistenceLayerDefaults;
begin
  LDefaults:= TtiPersistenceLayerDefaults.Create;
  try
    PersistenceLayer.AssignPersistenceLayerDefaults(LDefaults);
    Result:= LDefaults.CanSupportSQL;
  finally
    LDefaults.Free;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.SetupOnce;
begin
  inherited;
  PersistenceLayer.DBConnectionPools.Connect(
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password,
    '');
  FDBConnectionPool:= PersistenceLayer.DBConnectionPools.Find(PerFrameworkSetup.DBName);
end;

procedure TtiTestCaseWithDatabaseConnection.TearDownOnce;
begin
  PersistenceLayer.DBConnectionPools.Disconnect(
    PerFrameworkSetup.DBName);
  inherited;
end;

{ TtiTestCaseWithTestSetupData }

function TtiTestCaseWithPersistenceLayer.GetPersistenceLayer: TtiPersistenceLayer;
begin
  result:= GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(PersistenceLayerName);
end;

procedure TtiTestCaseWithTestSetupData.SetUpOnce;
begin
  inherited;
  FtiOPFTestSetupData:= gTIOPFTestManager.FindByPersistenceLayerName(PersistenceLayerName);
  Assert(FtiOPFTestSetupData <> nil, 'FtiOPFTestSetupData not assigned');
end;

{ TtiTestCaseWithTables }

procedure TtiTestCaseWithTables.SetUp;
begin
  inherited;
  SetupTestTables;
end;

procedure TtiTestCaseWithTables.TearDown;
begin
  DeleteTestTables;
  inherited;
end;

initialization
  // Slow to create every test, so recycle one instance
  ULongString:= tiCreateStringOfSize(3000);

end.



