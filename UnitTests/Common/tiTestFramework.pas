unit tiTestFramework;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  fpcunit
  ,testregistry
  ,testdecorator
  {$ELSE}
  TestFramework
  ,TestExtensions
  {$ENDIF}
  ,tiOPFTestManager
  ,tiObject
  ,tiQuery
  ,Classes
  ,SysUtils
 ;

const
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
  cTimingRepeatCount            = 400;

type
  // Abstract test case for testing non persistence related classes
  TtiTestCase = class(TTestCase)
  private
    function GetLongString: string;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
    function    TempDirectory: string;
    function    TempFileName(const AFilename: string = ''): string;
    property    LongString : string    read GetLongString;
    function    tstIntToStr(pInt:Integer):string;
    function    tstIntToBool(pInt:Integer): boolean;
    function    tstIntToFloat(pInt:Integer): Extended;
    function    tstIntToDateTime(pInt:Integer): TDateTime;
    procedure   tstIntToStream(pInt:Integer; const AStream : TStream);
    function    AreStreamContentsSame(const pStream1, pStream2 : TStream; var AMessage : string): boolean;
    procedure   CheckStreamContentsSame(const pStream1, pStream2 : TStream);
    procedure   CheckNearEnough(AExpected, AActual: Extended; const AMessage: string);
  public
    {$IFDEF FPC}
    constructor Create; override;
    function  GetName: string; virtual;
    property  Name: string read GetName;
    {$ELSE}
    constructor Create(AMethodName: string); override;
    {$ENDIF}
    // DUnit interface was added after FPC 2.0.2
    {$IFDEF FPC}
      {$I DUnitCompatableIntf.inc}
    {$ENDIF}
  end;


  TtiTestCaseClass    = class of TtiTestCase;

  TtiDUnitSetupTask   = (sutPerLayer, sutDBConnection, sutTables);
  TtiDUnitSetupTasks  = set of TtiDUnitSetupTask;


  // Abstract test case for testing persistence related classes
  TtiOPFTestCase = class(TtiTestCase)
  private
    FSetupTasks: TtiDUnitSetupTasks;
    FtiOPFTestSetupData: TtiOPFTestSetupData;
    procedure   SetSetupTasks(const AValue: TtiDUnitSetupTasks);
    function    GetDatabaseName: string;
    function    GetUserName: string;
    function    GetPassword: string;
    function    GetPerLayerName: string;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
    procedure   CheckObjectState(AObjectState: TPerObjectState; const AData : TtiObject);
    procedure   CheckExceptionMessage(const AMessage : string; const AException : Exception);
    procedure   CreateDBIfNotExists; virtual;
    procedure   SetupTestTables;
    procedure   DeleteTestTables;
    procedure   EmptyTestTables;

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

    procedure   DropTestTable;

    procedure   CreateTableTestBin( const ADatabase: TtiDatabase = nil);
    procedure   CreateTableTestGroup(const ADatabase: TtiDatabase = nil);
    procedure   InsertIntoTestGroup(const ADatabase: TtiDatabase; AValue : integer);
    procedure   DropTableTestBin(   const ADatabase: TtiDatabase = nil);
    procedure   DropTableTestGroup( const ADatabase: TtiDatabase = nil);

    procedure   DropNextOIDTable;
    procedure   CreateNextOIDIntTable;
    procedure   CreateNextOIDStrTable;

    procedure   WriteTimingResult(const pAction, APersistenceLayerName : string; AValue : Extended);

  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: string); override;
    {$ENDIF}
    property    PerFrameworkSetup : TtiOPFTestSetupData read FtiOPFTestSetupData write FtiOPFTestSetupData;
    property    PerLayerName     : string read GetPerLayerName;
    property    DatabaseName     : string read GetDatabaseName;
    property    UserName         : string read GetUserName;
    property    Password         : string read GetPassword;
    property    SetupTasks       : TtiDUnitSetupTasks read FSetupTasks write SetSetupTasks;
    function    GetName: string; override;
  end;


  TtiOPFTestSetupDecorator = class(TTestSetup)
  protected
    function  PerLayerID: ShortString;
  public
    function  GetName: string; {$IFNDEF FPC}override{$ELSE}virtual{$ENDIF};
    {$IFDEF FPC}
    property  Name: string read GetName;
    {$ENDIF}
  end;


implementation
uses
   tiOPFManager
  ,tiDUnitUtils
  ,tiUtils
  ,tiPersistenceLayers
  ,tiConstants
  ,tiDUnitDependencies
  ,tiExcept
  ,INIFiles
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
 ;

var
  UTempPath : string;
  ULongString: string;

function TtiTestCase.TempDirectory: string;
var
  pcTemp : array[0..MAX_PATH] of char;
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
  lResult := AreStreamContentsSame(pStream1, pStream2, lMessage);
  if not lResult then
    Fail(lMessage);
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


constructor TtiTestCase.Create{$IFNDEF FPC}(AMethodName: string){$ENDIF};
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
  if not DirectoryExists(TempDirectory) then
    try tiForceDirectories(TempDirectory) except end;
end;

procedure TtiTestCase.TearDown;
//var
//  LSL: TStringList;
begin
  inherited;
  // ToDo: Must look into this - sometimes TempDirectory contains a file that's locked
  if DirectoryExists(TempDirectory) then
    try tiForceRemoveDir(TempDirectory) except end;
// To check for files left in the temp directory
//  LSL:= TStringList.Create;
//  try
//    tiFilesToStringList(tiGetTempDir, '*', LSL, False);
//    CheckEquals(FFileCount, LSL.Count, 'Files left in  ' + tiGetTempDir);
//
//    tiDirectoryTreeToStringList(tiGetTempDir, LSL, false);
//    CheckEquals(FDirCount, LSL.Count, 'Directories left in ' + tiGetTempDir);
//  finally
//    LSL.Free;
//  end;
end;

function TtiTestCase.tstIntToBool(pInt: Integer): boolean;
begin
  Result := (pInt mod 2) = 0;
end;


function TtiTestCase.tstIntToDateTime(pInt: Integer): TDateTime;
begin
  Result := pInt / 1000;
end;


function TtiTestCase.tstIntToFloat(pInt: Integer): Extended;
begin
  Result := pInt / 1000;
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
  // DUnit interface was added after FPC 2.0.2
  {$I DUnitCompatableImpl.inc}
{$ENDIF}


procedure TtiOPFTestCase.DropTableTestGroup(const ADatabase : TtiDatabase = nil);
begin
  if ADatabase = nil then
    try gTIOPFManager.DropTable(cTableNameTestGroup,
                             PerFrameworkSetup.DBName,
                             PerFrameworkSetup.PerLayerName)  except end
  else
    try ADatabase.DropTable(cTableNameTestGroup) except end;
end;


procedure TtiOPFTestCase.DeleteTestTables;
begin
  DropTableTestGroup;
  DropTableTestBin;
  try gTIOPFManager.DropTable('test_item',
                           PerFrameworkSetup.DBName,
                           PerFrameworkSetup.PerLayerName)  except end;

  try gTIOPFManager.DropTable(cTableNameTIOPFTestChild_A, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)  except end;
  try gTIOPFManager.DropTable(cTableNameTIOPFTestChild_B, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)  except end;
  try gTIOPFManager.DropTable(cTableNameTIOPFTestParent, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)  except end;

  try gTIOPFManager.DropTable(cTableNameTIOPFTestChildGrouped_A, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)  except end;
  try gTIOPFManager.DropTable(cTableNameTIOPFTestChildGrouped_B, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)  except end;
  try gTIOPFManager.DropTable(cTableNameTIOPFTestParentGrouped, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)  except end;
  try gTIOPFManager.DropTable(cTableNameTIOPFTestParentGroup, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)  except end;

end;


procedure TtiOPFTestCase.CreateTableTestGroup(const ADatabase : TtiDatabase = nil) ;
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
      gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)
    else
      ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.InsertIntoTestGroup(const ADatabase: TtiDatabase; AValue : integer);
var
  lParams : TtiQueryParams;
begin
  Check(LongString <> '', 'FLongString not assigned');
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString('OID', IntToStr(AValue));
    lParams.SetValueAsString('Group_Str_Field', tiPad0(IntToStr(AValue), 10));
    lParams.SetValueAsInteger('Group_Int_Field', AValue);
    lParams.SetValueAsFloat('Group_Float_Field', StrToInt(tiReplicate(IntToStr(AValue), 4)) / 1000);
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


procedure TtiOPFTestCase.CreateTableTestBin(const ADatabase : TtiDatabase = nil);
var
  lTable : TtiDBMetaDataTable;
begin
  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := 'Test_Bin';
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Item_Binary_Field', qfkBinary );
    if ADatabase = nil then
      gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName)
    else
      ADatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.DropTableTestBin(const ADatabase : TtiDatabase = nil);
begin
  if ADatabase = nil then
    try gTIOPFManager.DropTable('test_bin',
                             PerFrameworkSetup.DBName,
                             PerFrameworkSetup.PerLayerName)  except end
  else
    try ADatabase.DropTable('test_bin'  ) except end;
end;


{ TtiOPFTestSetupData }

function TtiOPFTestSetupDecorator.GetName: string;
begin
  result := 'SetUp [' + PerLayerID + '] connection for ' +
    {$IFNDEF FPC}
    Test.Name
    {$ELSE}
    Test.TestName
    {$ENDIF}
   ;
end;


constructor TtiOPFTestCase.Create{$IFNDEF FPC}(AMethodName: string){$ENDIF};
begin
  inherited;
  // Must assign SetupTasks in the concrete's create method
  // SetupTasks := [sutPerLayer, sutDBConnection, sutTables];
  // Must set PerLayerName;
end;


procedure TtiOPFTestCase.CreateDBIfNotExists;
var
  lRegPerLayer  : TtiPersistenceLayer;
  lDatabaseClass : TtiDatabaseClass;
begin
  lRegPerLayer := gTIOPFManager.PersistenceLayers.FindByPerLayerName(PerLayerName);
  Assert(lRegPerLayer <> nil, 'Unable to find registered persistence layer <' + PerLayerName +'>');
  lDatabaseClass := lRegPerLayer.tiDatabaseClass;
  if not lDatabaseClass.DatabaseExists(
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password) then
  begin
    lDatabaseClass.CreateDatabase(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password);
    if not lDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password) then
      EtiOPFDUnitException.Create('Unable to create database <' + PerFrameworkSetup.DBName + '>');
  end;
end;


procedure TtiOPFTestCase.EmptyTestTables;
begin
  gTIOPFManager.DeleteRow('test_item',    nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  gTIOPFManager.DeleteRow(cTableNameTestGroup,   nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  gTIOPFManager.DeleteRow('test_bin',     nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  gTIOPFManager.DeleteRow(cTableNameTIOPFTestChild_A, nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  gTIOPFManager.DeleteRow(cTableNameTIOPFTestChild_B, nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  gTIOPFManager.DeleteRow('test_parent',  nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
end;


procedure TtiOPFTestCase.SetSetupTasks(const AValue: TtiDUnitSetupTasks);
begin
  FSetupTasks := AValue;
  if (sutDBConnection in AValue) and
     (not (sutPerLayer in AValue)) then
    FSetupTasks := FSetupTasks + [sutPerLayer];
  if (sutTables in FSetupTasks) and
     (not (sutDBConnection in FSetupTasks)) then
    FSetupTasks := FSetupTasks + [sutDBConnection];
end;


procedure TtiOPFTestCase.SetUp;
begin
  inherited;
  Assert(PerFrameworkSetup <> nil, 'PerFrameworkSetup not assigned');
  // Load persistence layer
  if sutPerLayer in SetupTasks then
  begin
    if not gTIOPFManager.PersistenceLayers.IsLoaded(PerFrameworkSetup.PerLayerName) then
    begin
      {$IFDEF STATIC_PERLAYER_LINKING}
        raise EtiOPFException.CreateFmt('Persistence layer not loaded <%s>', [PerFrameworkSetup.PerLayerName]);
      {$ELSE}
        gTIOPFManager.LoadPersistenceLayer(PerFrameworkSetup.PerLayerName)
      {$ENDIF}
    end;
    if PerFrameworkSetup.CanCreateDatabase then
      CreateDBIfNotExists;
  end;

  // Establish a database connection
  if sutDBConnection in SetupTasks then
    gTIOPFManager.ConnectDatabase(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password,
      '',
      PerFrameworkSetup.PerLayerName);

  // SetUp test tables
  if sutTables in SetupTasks then
    SetupTestTables;
end;


procedure TtiOPFTestCase.SetupTestTables;
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
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParent;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Owner_OID',          qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Parent_Str_Field',   qfkString, 10);
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChild_A;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChild_B;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParentGrouped;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Owner_OID',          qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Parent_Str_Field',   qfkString, 10);
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChildGrouped_A;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestChildGrouped_B;
    lTable.AddField('OID',               qfkString, 36); // Should be Not Null & PK
    lTable.AddField('Child_Int_Field',   qfkInteger);
    lTable.AddField('Child_Float_Field', qfkFloat);
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);

    lTable.Clear;
    lTable.Name :=  cTableNameTIOPFTestParentGroup;
    lTable.AddField('OID',                qfkString, 36); // Should be Not Null & PK
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);

  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.TearDown;
begin
  if sutTables in SetupTasks then
    DeleteTestTables;
  if sutDBConnection in SetupTasks then
    gTIOPFManager.DisconnectDatabase(PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  if (sutPerLayer in SetupTasks) then
  begin
    {$IFNDEF STATIC_PERLAYER_LINKING}
      gTIOPFManager.UnloadPersistenceLayer(PerFrameworkSetup.PerLayerName);
    {$ENDIF}
  end;
  inherited;
end;


function TtiOPFTestCase.GetDatabaseName: string;
begin
  Assert(PerFrameworkSetup.TestValid, cTIInvalidObjectError);
  result := PerFrameworkSetup.DBName;
end;


procedure TtiOPFTestCase.DropNextOIDTable;
begin
  try
    gTIOPFManager.DropTable('Next_OID', PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  except
  end;
end;


procedure TtiOPFTestCase.CreateNextOIDIntTable;
var
  lTable : TtiDBMetaDataTable;
  lParams : TtiQueryParams;
begin

  DropNextOIDTable;

  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := 'Next_OID';
    lTable.AddField('OID', qfkInteger);
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  finally
    lTable.Free;
  end;

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsInteger('OID', 1000000);
    gTIOPFManager.InsertRow('Next_OID', lParams, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  finally
    lParams.Free;
  end;
end;


procedure TtiOPFTestCase.CreateNextOIDStrTable;
var
  lTable : TtiDBMetaDataTable;
  lParams : TtiQueryParams;
begin

  DropNextOIDTable;

  lTable := TtiDBMetaDataTable.Create;
  try
    lTable.Name := 'Next_OID';
    lTable.AddField('OID', qfkString, 10);
    gTIOPFManager.CreateTable(lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  finally
    lTable.Free;
  end;

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString('OID', '0');
    gTIOPFManager.InsertRow('Next_OID', lParams, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName);
  finally
    lParams.Free;
  end;

end;


procedure TtiOPFTestCase.CreateTableBoolean(const ADatabaseName, APersistenceLayerName : string);
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
    gTIOPFManager.CreateTable(lTable, ADatabaseName, APersistenceLayerName);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.CreateTableBoolean(const ADatabase : TtiDatabase);
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


procedure TtiOPFTestCase.CreateTableDateTime(const ADatabaseName, APersistenceLayerName : string);
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
    gTIOPFManager.CreateTable(lTable, ADatabaseName, APersistenceLayerName);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.CreateTableDateTime(const ADatabase : TtiDatabase);
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


procedure TtiOPFTestCase.CreateTableFloat(const ADatabaseName, APersistenceLayerName : string);
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
    gTIOPFManager.CreateTable(lTable, ADatabaseName, APersistenceLayerName);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.CreateTableFloat(const ADatabase : TtiDatabase);
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


procedure TtiOPFTestCase.CreateTableInteger(const ADatabaseName, APersistenceLayerName : string);
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
    gTIOPFManager.CreateTable(lTable, ADatabaseName, APersistenceLayerName);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.CreateTableInteger(const ADatabase : TtiDatabase);
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


procedure TtiOPFTestCase.CreateTableLongString(const ADatabaseName, APersistenceLayerName : string);
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
    gTIOPFManager.CreateTable(lTable, ADatabaseName, APersistenceLayerName);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.CreateTableLongString(const ADatabase : TtiDatabase);
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


procedure TtiOPFTestCase.CreateTableStream(const ADatabaseName, APersistenceLayerName : string);
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
    gTIOPFManager.CreateTable(lTable, ADatabaseName, APersistenceLayerName);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.CreateTableStream(const ADatabase : TtiDatabase);
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


procedure TtiOPFTestCase.CreateTableString(const ADatabaseName, APersistenceLayerName : string);
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
    gTIOPFManager.CreateTable(lTable, ADatabaseName, APersistenceLayerName);
  finally
    lTable.Free;
  end;
end;


procedure TtiOPFTestCase.CreateTableString(const ADatabase : TtiDatabase);
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


procedure TtiOPFTestCase.DropTestTable;
begin
  try gTIOPFManager.DropTable(cTIQueryTableName, DatabaseName, PerLayerName) except end;
end;


procedure TtiOPFTestCase.WriteTimingResult(const pAction, APersistenceLayerName : string; AValue : Extended);
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


procedure TtiOPFTestCase.CheckObjectState(AObjectState: TPerObjectState; const AData: TtiObject);
begin
  Assert(AData.TestValid(TtiObject), cTIInvalidObjectError);
  Check(AData.ObjectState = AObjectState,
        'ObjectState. Expected ' +
        AData.ClassName + '.' + ObjectStateToString(AObjectState) +
        ' but is ' +
        AData.ObjectStateAsString);
end;


procedure TtiOPFTestCase.CheckExceptionMessage(const AMessage: string;
  const AException: Exception);
begin
  Check(Pos(UpperCase(AMessage), UpperCase(AException.Message)) <> 0,
         'Expected "' + AMessage +'" in exception message but found "' +
         AException.message + '"');
end;


function TtiOPFTestCase.GetPassword: string;
begin
  Assert(PerFrameworkSetup.TestValid, cTIInvalidObjectError);
  result := PerFrameworkSetup.Password;
end;


function TtiOPFTestCase.GetUserName: string;
begin
  Assert(PerFrameworkSetup.TestValid, cTIInvalidObjectError);
  result := PerFrameworkSetup.Username;
end;


function TtiOPFTestCase.GetName: string;
begin
  if PerFrameworkSetup <> nil then
    result := '[' + PerLayerName + '] ' + inherited GetName
  else
    Result := inherited GetName;
end;


function TtiOPFTestSetupDecorator.PerLayerID: ShortString;
begin
//  Result:= gTIOPFTestManager.SelectedOPFTestSetupDataAsString;
end;


function TtiOPFTestCase.GetPerLayerName: string;
begin
  Assert(FtiOPFTestSetupData.TestValid, cErrorTIPerObjAbsTestValid);
  result:= FtiOPFTestSetupData.PerLayerName;
end;


procedure TtiTestCase.CheckNearEnough(AExpected, AActual: Extended; const AMessage: string);
begin
  CheckEquals(AExpected, AActual, 0.000001, AMessage);
end;


function TtiTestCase.GetLongString: string;
begin
  if ULongString = '' then
    ULongString:= tiCreateStringOfSize(3000);
  result:= ULongString;
end;


end.

