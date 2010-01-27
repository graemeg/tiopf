{ Adds persistence layer and database access for testing of tiOPF.

  Not sure if these are the best unit & class names.}

unit tiOPFTestCase;

{$I tiDefines.inc}

interface
uses
   tiTestFramework
  ,tiOPFTestManager
  ,tiObject
  ,tiQuery
  ,tiExcept
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

  EtiOPFDUnitException = class(EtiOPFException);

  {: Adds the class function PersistenceLayerName, which must be overridden in
     the concrete classes. SetUpOnce uses PersistenceLayerName to find the
     appropriate tiOPFTestSetupData object in the GTIOPFTestManager}
  TtiTestCaseWithTestSetupData = class(TtiTestCase)
  private
    FTestSetupData: TtiOPFTestSetupData;
  protected
    procedure   SetUpOnce; override;
  public
    class function PersistenceLayerName: string; virtual; abstract;
    property    TestSetupData : TtiOPFTestSetupData read FTestSetupData;
  end;

  {: Adds a reference to the appropriate persistence layer}
  TtiTestCaseWithPersistenceLayer = class(TtiTestCaseWithTestSetupData)
  private
    function    GetPersistenceLayer: TtiPersistenceLayer;
    procedure   CreateDBIfNotExists;

  protected
    procedure   SetUpOnce; override;

    // ToDo: Move CheckObjectState upstairs
    procedure   CheckObjectState(AObjectState: TPerObjectState; const AData : TtiObject);
    procedure   WriteTimingResult(const pAction, APersistenceLayerName : string; AValue : Extended);

    function    PersistenceLayerSupportsMultiUser: boolean;
    function    PersistenceLayerSupportsSQL: boolean;
    property    PersistenceLayer: TtiPersistenceLayer read GetPersistenceLayer;
  public
    function    GetName: string; override;
  end;

  TtiTestCaseWithPersistenceLayerClass    = class of TtiTestCaseWithPersistenceLayer;

  {: Adds a reference to the appropriate DBConnectionPool}
  TtiTestCaseWithDatabaseConnection = class(TtiTestCaseWithPersistenceLayer)
  private
    FDBConnectionPool: TtiDBConnectionPool;
    FCreatedTables: TStringList;
    function    GetDatabaseName: string;
    function    GetUserName: string;
    function    GetPassword: string;

  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure TearDownOnce; override;

    property    DatabaseName     : string read GetDatabaseName;
    property    UserName         : string read GetUserName;
    property    Password         : string read GetPassword;

    property DBConnectionPool: TtiDBConnectionPool read FDBConnectionPool;

    procedure   CreateTable(const ATable : TtiDBMetaDataTable; const ADatabase: TtiDatabase = nil);
    procedure   InsertRow(const ATableName: string; AParams: TtiQueryParams; const ADatabase: TtiDatabase = nil);
    procedure   DropTable(const ATableName: string; const ADatabase: TtiDatabase = nil);
    procedure   DropCreatedTables;

    procedure   CreateTestTables; overload;
    procedure   EmptyTestTables;

    procedure   CreateTableTestBin( const ADatabase: TtiDatabase = nil);
    procedure   CreateTableTestGroup(const ADatabase: TtiDatabase = nil);
    procedure   InsertIntoTestGroup(const AValue : integer; const ADatabase: TtiDatabase = nil);

    procedure   CreateTableInteger(   const ADatabase : TtiDatabase = nil);
    procedure   CreateTableString(    const ADatabase : TtiDatabase = nil);
    procedure   CreateTableFloat(     const ADatabase : TtiDatabase = nil);
    procedure   CreateTableBoolean(   const ADatabase : TtiDatabase = nil);
    procedure   CreateTableDateTime(  const ADatabase : TtiDatabase = nil);
    procedure   CreateTableLongString(const ADatabase : TtiDatabase = nil);
    procedure   CreateTableStream(    const ADatabase : TtiDatabase = nil);

  end;

const
  cDUnitTestFloatPrecision = 0.000001;

implementation
uses
   tiBaseObject
  ,tiOPFManager
  ,tiUtils
  ,tiConstants
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
  LOutput: AnsiString;

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

procedure TtiTestCaseWithDatabaseConnection.CreateTableTestGroup(const ADatabase : TtiDatabase = nil) ;
var
  LTable : TtiDBMetaDataTable;
begin
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := cTableNameTestGroup;
    LTable.AddInstance('OID',               qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Group_Str_Field',   qfkString, 10);
    LTable.AddInstance('Group_Int_Field',   qfkInteger);
    LTable.AddInstance('Group_Float_Field', qfkFloat);
    LTable.AddInstance('Group_Date_Field',  qfkDateTime);
    LTable.AddInstance('Group_Bool_Field',  qfkLogical);
    LTable.AddInstance('Group_Notes_Field', qfkLongString);
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.InsertIntoTestGroup(
  const AValue : integer; const ADatabase: TtiDatabase = nil);
var
  LParams : TtiQueryParams;
  LExtended: Extended;
begin
  Check(LongString <> '', 'FLongString not assigned');
  LExtended:= 1000; // Removes compiler warning at SetValueAsFloat
  LParams := TtiQueryParams.Create;
  try
    LParams.SetValueAsString('OID', IntToStr(AValue));
    LParams.SetValueAsString('Group_Str_Field', tiPad0(IntToStr(AValue), 10));
    LParams.SetValueAsInteger('Group_Int_Field', AValue);
    LParams.SetValueAsFloat('Group_Float_Field', StrToInt(tiReplicate(IntToStr(AValue), 4)) / LExtended);
    LParams.SetValueAsDateTime('Group_Date_Field', EncodeDate(1900, 1, AValue)) ;
    LParams.SetValueAsBoolean('Group_Bool_Field', ((AValue mod 2) = 0)) ;
    LParams.SetValueAsString('Group_Notes_Field', LongString) ;
    InsertRow(cTableNameTestGroup, LParams, ADatabase);
  finally
    lParams.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.InsertRow(const ATableName: string;
  AParams: TtiQueryParams; const ADatabase: TtiDatabase = nil);
var
  LDatabase: TtiDatabase;
begin
  if ADatabase = nil then
  begin
    LDatabase:= DBConnectionPool.Lock;
    try
      LDatabase.InsertRow(ATableName, AParams);
    finally
      DBConnectionPool.UnLock(LDatabase);
    end;
  end else
    ADatabase.InsertRow(ATableName, AParams);
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableTestBin(const ADatabase : TtiDatabase = nil);
var
  LTable : TtiDBMetaDataTable;
begin
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := 'Test_Bin';
    LTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    LTable.AddField('Item_Binary_Field', qfkBinary );
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
  end;
end;


procedure TtiTestCaseWithDatabaseConnection.DropCreatedTables;
var
  LDatabase: TtiDatabase;
  i: integer;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    for i := FCreatedTables.Count - 1 downto 0 do
      DropTable(FCreatedTables.Strings[i], LDatabase);
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
  Assert(FCreatedTables.Count = 0);
end;

procedure TtiTestCaseWithDatabaseConnection.DropTable(const ATableName: string; const ADatabase: TtiDatabase = nil);
var
  LDatabase: TtiDatabase;
  LIndex: integer;
begin
  if ADatabase = nil then
  begin
    LDatabase:= DBConnectionPool.Lock;
    try
      try
        LDatabase.DropTable(ATableName);
      except end;
    finally
      DBConnectionPool.UnLock(LDatabase);
    end;
  end else
    ADatabase.DropTable(ATableName);
  LIndex:= FCreatedTables.IndexOf(LowerCase(ATableName));
  if LIndex <> -1 then
  begin
    FCreatedTables.Delete(LIndex);
    FCreatedTables.Capacity:= FCreatedTables.Count; // To suppress DUnit2 leak report
  end;
end;

procedure TtiTestCaseWithPersistenceLayer.CreateDBIfNotExists;
var
  LPersistenceLayer  : TtiPersistenceLayer;
  LDatabaseClass : TtiDatabaseClass;
begin
  // Some database require a DBA to create (Oracle for example),
  // so don't run this setup code
  if TestSetupData.CanCreateDatabase then
  begin
    LPersistenceLayer := GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(PersistenceLayerName);
    Assert(LPersistenceLayer <> nil, 'Unable to find registered persistence layer <' + PersistenceLayerName +'>');
    LDatabaseClass := LPersistenceLayer.DatabaseClass;
    if not LDatabaseClass.DatabaseExists(
      TestSetupData.DBName,
      TestSetupData.Username,
      TestSetupData.Password) then
    begin
      LDatabaseClass.CreateDatabase(
        TestSetupData.DBName,
        TestSetupData.Username,
        TestSetupData.Password);
      if not LDatabaseClass.DatabaseExists(
        TestSetupData.DBName,
        TestSetupData.Username,
        TestSetupData.Password) then
        EtiOPFDUnitException.Create('Unable to create database <' + TestSetupData.DBName + '>');
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

procedure TtiTestCaseWithDatabaseConnection.CreateTestTables;
var
  LTable : TtiDBMetaDataTable;
begin
  CreateTableTestGroup;
  CreateTableTestBin;
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Clear;
    LTable.Name := 'Test_Item';
    LTable.AddInstance('OID',                qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('OID_Group',          qfkString, 36); // Should be Not Null & FK
    LTable.AddInstance('Item_Str_Field',     qfkString, 10);
    LTable.AddInstance('Item_Int_Field',     qfkInteger);
    LTable.AddInstance('Item_Float_Field',   qfkFloat);
    LTable.AddInstance('Item_Bool_Field',    qfkLogical);
    LTable.AddInstance('Item_Date_Field',    qfkDateTime);
    LTable.AddInstance('Item_Notes_Field',   qfkLongString);
    CreateTable(LTable);

    LTable.Clear;
    LTable.Name :=  cTableNameTIOPFTestParent;
    LTable.AddInstance('OID',                qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Owner_OID',          qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Parent_Str_Field',   qfkString, 10);
    CreateTable(LTable);

    LTable.Clear;
    LTable.Name :=  cTableNameTIOPFTestChild_A;
    LTable.AddInstance('OID',               qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Child_Int_Field',   qfkInteger);
    LTable.AddInstance('Child_Float_Field', qfkFloat);
    CreateTable(LTable);

    LTable.Clear;
    LTable.Name :=  cTableNameTIOPFTestChild_B;
    LTable.AddInstance('OID',               qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Child_Int_Field',   qfkInteger);
    LTable.AddInstance('Child_Float_Field', qfkFloat);
    CreateTable(LTable);

    LTable.Clear;
    LTable.Name :=  cTableNameTIOPFTestParentGrouped;
    LTable.AddInstance('OID',                qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Owner_OID',          qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Parent_Str_Field',   qfkString, 10);
    CreateTable(LTable);

    LTable.Clear;
    LTable.Name :=  cTableNameTIOPFTestChildGrouped_A;
    LTable.AddInstance('OID',               qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Child_Int_Field',   qfkInteger);
    LTable.AddInstance('Child_Float_Field', qfkFloat);
    CreateTable(LTable);

    LTable.Clear;
    LTable.Name :=  cTableNameTIOPFTestChildGrouped_B;
    LTable.AddInstance('OID',               qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance('Child_Int_Field',   qfkInteger);
    LTable.AddInstance('Child_Float_Field', qfkFloat);
    CreateTable(LTable);

    LTable.Clear;
    LTable.Name :=  cTableNameTIOPFTestParentGroup;
    LTable.AddInstance('OID',                qfkString, 36); // Should be Not Null & PK
    CreateTable(LTable);
  finally
    LTable.Free;
  end;
end;

function TtiTestCaseWithDatabaseConnection.GetDatabaseName: string;
begin
  Assert(TestSetupData.TestValid, CTIErrorInvalidObject);
  result := TestSetupData.DBName;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTable(
  const ATable: TtiDBMetaDataTable;
  const ADatabase: TtiDatabase = nil);
var
  LDatabase: TtiDatabase;
begin
  if ADatabase = nil then
  begin
    LDatabase:= DBConnectionPool.Lock;
    try
      LDatabase.CreateTable(ATable);
    finally
      DBConnectionPool.UnLock(LDatabase);
    end;
  end else
    ADatabase.CreateTable(ATable);
  FCreatedTables.Add(LowerCase(ATable.Name));
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableBoolean(const ADatabase : TtiDatabase = nil);
var
  LTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := cTIQueryTableName;
    LTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    LTable.AddField(cTIQueryColName, qfkLogical);
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableDateTime(const ADatabase : TtiDatabase);
var
  LTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := cTIQueryTableName;
    LTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    LTable.AddField(cTIQueryColName, qfkDateTime);
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableFloat(const ADatabase : TtiDatabase);
var
  LTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := cTIQueryTableName;
    LTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    LTable.AddField(cTIQueryColName, qfkFloat);
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableInteger(const ADatabase : TtiDatabase = nil);
var
  LTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := cTIQueryTableName;
    LTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    LTable.AddField(cTIQueryColName, qfkInteger);
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableLongString(const ADatabase : TtiDatabase);
var
  LTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := cTIQueryTableName;
    LTable.AddInstance('OID', qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance(cTIQueryColName, qfkLongString);
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableStream(const ADatabase : TtiDatabase);
var
  LTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := cTIQueryTableName;
    LTable.AddField('OID', qfkString, 36); // Should be Not Null & PK
    LTable.AddField(cTIQueryColName, qfkBinary);
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
  end;
end;

procedure TtiTestCaseWithDatabaseConnection.CreateTableString(const ADatabase : TtiDatabase);
var
  LTable : TtiDBMetaDataTable;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := cTIQueryTableName;
    LTable.AddInstance('OID', qfkString, 36); // Should be Not Null & PK
    LTable.AddInstance(cTIQueryColName, qfkString, 255);
    CreateTable(LTable, ADatabase);
  finally
    LTable.Free;
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


function TtiTestCaseWithDatabaseConnection.GetPassword: string;
begin
  Assert(TestSetupData.TestValid, CTIErrorInvalidObject);
  result := TestSetupData.Password;
end;


function TtiTestCaseWithDatabaseConnection.GetUserName: string;
begin
  Assert(TestSetupData.TestValid, CTIErrorInvalidObject);
  result := TestSetupData.Username;
end;


function TtiTestCaseWithPersistenceLayer.GetName: string;
begin
  result := '[' + PersistenceLayerName + '] ' + inherited GetName;
end;

{ TtiTestCaseWithDatabaseConnection }

function TtiTestCaseWithPersistenceLayer.PersistenceLayerSupportsMultiUser: boolean;
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

function TtiTestCaseWithPersistenceLayer.PersistenceLayerSupportsSQL: boolean;
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

procedure TtiTestCaseWithPersistenceLayer.SetUpOnce;
begin
  inherited;
  CreateDBIfNotExists;
end;

procedure TtiTestCaseWithDatabaseConnection.SetUp;
begin
  inherited;
  PersistenceLayer.DBConnectionPools.Connect(
    DatabaseName,
    DatabaseName,
    Username,
    Password,
    '');
  FDBConnectionPool:= PersistenceLayer.DBConnectionPools.Find(TestSetupData.DBName);
end;

procedure TtiTestCaseWithDatabaseConnection.SetupOnce;
begin
  inherited;
  FCreatedTables:= TStringList.Create;
end;

procedure TtiTestCaseWithDatabaseConnection.TearDown;
begin
  DropCreatedTables;
  PersistenceLayer.DBConnectionPools.Disconnect(
    TestSetupData.DBName);
  inherited;
end;

procedure TtiTestCaseWithDatabaseConnection.TearDownOnce;
begin
  FCreatedTables.Free;
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
  FTestSetupData:= GTIOPFTestManager.FindByPersistenceLayerName(PersistenceLayerName);
  Assert(FTestSetupData <> nil, 'FTestSetupData not assigned');
end;

initialization
  // Slow to create every test, so recycle one instance
  ULongString:= tiCreateStringOfSize(3000);

end.









