unit tiQuery_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework,
  tiOPFTestCase,
  tiDBConnectionPool,
  tiQuery,
  SysUtils,
  Classes;

type

  TTestTIPersistenceLayers = class(TtiTestCaseWithPersistenceLayer)
  protected
    procedure DoThreadedDBConnectionPool(
      const ADBConnectionPool: TtiDBConnectionPool;
      const AThreadCount : integer);
  published
    procedure   ConnectDatabase; virtual;
    procedure   tiOPFManager_ConnectDatabase; virtual;

    procedure   Database_Connect; virtual;
    procedure   DBConnectionPoolConnectDisconnect; virtual;
    procedure   NonThreadedDBConnectionPool; virtual;
    procedure   ThreadedDBConnectionPool; virtual;

    procedure   CreateTIQuery_LayerName; virtual;
    procedure   CreateTIQuery_DatabaseClass; virtual;
    procedure   CreateTIDatabase;

    // ToDo: There are many tests on TtiPersistenceLayerList that must be tested here
  end;

  TtiOPFManager_ExecInsertSQLCheckMethod = procedure(const AQuery: TtiQuery) of object;

  // Test TtiDatabase connectivity
  TTestTIDatabase = class(TtiTestCaseWithDatabaseConnection)
  protected

    procedure CheckFieldMetaData(const pDBMetaDataTable : TtiDBMetaDataTable;
                                   const AFieldName : string;
                                   pKind : TtiQueryFieldKind;
                                   pWidth : integer = 0);

    procedure DatabaseExists; virtual; abstract;
    procedure CreateDatabase; virtual; abstract;
    procedure tiOPFManager_DoExecInsertSQLCheck(const ACheckMethod: TtiOPFManager_ExecInsertSQLCheckMethod);
    procedure tiOPFManager_DoExecInsertSQLCheckInteger(const AQuery: TtiQuery);
    procedure tiOPFManager_DoExecInsertSQLCheckString(const AQuery: TtiQuery);
    procedure tiOPFManager_DoExecInsertSQLCheckFloat(const AQuery: TtiQuery);
    procedure tiOPFManager_DoExecInsertSQLCheckBoolean(const AQuery: TtiQuery);
    procedure tiOPFManager_DoExecInsertSQLCheckDateTime(const AQuery: TtiQuery);
    procedure tiOPFManager_DoExecInsertSQLCheckLongString(const AQuery: TtiQuery);

  published
    procedure tiOPFManager_ExecInsertSQLInteger;
    procedure tiOPFManager_ExecInsertSQLString;
    procedure tiOPFManager_ExecInsertSQLFloat;
    procedure tiOPFManager_ExecInsertSQLBoolean;
    procedure tiOPFManager_ExecInsertSQLDateTime;
    procedure tiOPFManager_ExecInsertSQLLongString;

    procedure CreateTIQuery;
    procedure CreateAndAttachTIQuery;
    procedure Transaction_InTransaction; virtual;
    procedure Transaction_Commit; virtual;
    procedure Transaction_RollBack; virtual;
    procedure CreateTableDropTable; virtual;
    procedure CreateTableDropTable_Timing; virtual;
    procedure ReadMetaData; virtual;
  end;


  // Test query access
  TTestTIQueryAbs = class(TtiTestCaseWithDatabaseConnection)
  private
    FQuery   : TtiQuery;
    FDatabase: TtiDatabase;
  protected
    property  Query   : TtiQuery    read FQuery;
    property  Database: TtiDatabase read FDatabase;

    procedure SetUp; override;
    procedure TearDown; override;

    // Helper methods used in the concretes
    procedure PopulateTableString(  const AValue : String   );
    procedure PopulateTableInteger( const AValue : Integer  );
    procedure PopulateTableReal(    const AValue : Extended     );
    procedure PopulateTableBoolean( const AValue : Boolean  );
    procedure PopulateTableDateTime(const AValue : TDateTime);
    procedure PopulateTableStream(  const AValue : TStream);
    procedure DoFieldAsStringLong(pStrLen: integer);

    // Implement (or hide) these in the concrete
    // Implemented for SQL persistence layers,
    // Hidden for Non SQL Persistence layers
    procedure GetSetSQL; virtual; abstract;
    procedure QueryType; virtual; abstract;
    procedure ParamName; virtual; abstract;
    procedure ParamCount; virtual; abstract;
    procedure ParamsAsString; virtual; abstract;
    procedure ParamAsString; virtual; abstract;
    procedure ParamAsInteger; virtual; abstract;
    procedure ParamAsFloat; virtual; abstract;
    procedure ParamAsBoolean; virtual; abstract;
    procedure ParamAsDateTime; virtual; abstract;
    procedure ParamAsStream; virtual; abstract;
    procedure ParamAsMacro; virtual; abstract;
    procedure ParamIsNull; virtual; abstract;
    procedure OpenCloseActive; virtual; abstract;
    procedure ExecSQL; virtual; abstract;
    procedure RowsAffected; virtual; abstract;

  published

    procedure ConfirmSetupWorks; virtual;
    procedure ConfirmDBConnectionWorks; virtual;
    procedure TypeKindToQueryFieldKind;
    procedure QueryToDatabaseConnection; // AttachDatabase, DetachDatabase;
    procedure FieldAsInteger;
    procedure FieldAsString;
    procedure FieldAsStringLong1;
    procedure FieldAsStringLong10;
    procedure FieldAsStringLong100;
    procedure FieldAsStringLong255;
    procedure FieldAsStringLong256;
    procedure FieldAsStringLong257;
    procedure FieldAsStringLong511;
    procedure FieldAsStringLong512;
    procedure FieldAsStringLong513;
    procedure FieldAsStringLong1023;
    procedure FieldAsStringLong1024;
    procedure FieldAsStringLong1025;
    procedure FieldAsStringLong1999;
    procedure FieldAsStringLong2000;
    procedure FieldAsStringLong2001;
    procedure FieldAsStringLong3999;
    procedure FieldAsStringLong4000;
    procedure FieldAsStringLong4001;
    procedure FieldAsStringLong5000;
    procedure FieldAsStringLong10000;
    procedure FieldAsFloat;
    procedure FieldAsBoolean;
    procedure FieldAsDateTime;
    procedure FieldAsStream;
    procedure FieldIsNull; virtual;
    procedure FieldByNameVSFieldByIndex; virtual;
    procedure FieldCount; virtual;
    procedure FieldName ; virtual;
    procedure FieldIndex; virtual;
    procedure FieldKind ; virtual;
    procedure FieldSize ; virtual;
    procedure EOF; virtual;
    procedure Next; virtual;
    procedure InsertDeleteUpdate_Timing;
  end;

  // Test the meta data objects. No database access required.
  TTestTIMetaData = class(TtiTestCase)
  published
    procedure DBMetaDataAdd;
    procedure DBMetaDataFindByTableName;
    procedure MetaDataTableAdd;
    procedure MetaDataTableAddField;
    procedure MetaDataTableFindByFieldName;
    procedure MetaDataTableMaxFieldNameWidth;
    procedure MetaDataFieldClone;
    procedure MetaDataTableClone;
  end;


  TTestTIQueryParams = class(TtiTestCase)
  published
    procedure TestCheckStreamContentsSame;
    procedure AsString;
    procedure AsVariant_String;
    procedure AsInteger;
    procedure AsVariant_Integer;
    procedure AsFloat;
    procedure AsVariant_Float;
    procedure AsDateTime;
    procedure AsVariant_DateTime;
    procedure AsBoolean;
    procedure AsVariant_Boolean;
    procedure AsStream;
    procedure ParamByName;
    procedure ParamIsNull;
  end;

procedure RegisterTests;

const
  cTIQueryTestName  = 'Dynamically loaded persistence layer';
  // Number of threads and iterations for DBPool testing
  // Set a high number for thorough testing (eg, 100)
  // Set a low number for quick testing (eg, 5)
  CThreadCount     = 5;
  cuIterations      = 5;
  cRepeatCount      = 5;
  // Seconds. Run the timing test for this number of seconds and count the
  // number of iterations. Higher value gives more accurate values.
  // Lower value gives faster tests.
  // Results are written to DataAccessTimingResults.txt
  CTimingTestPeriod = 2;

implementation
uses
  {$IFDEF MSWINDOWS}
   Forms
  ,Windows,
  {$ENDIF}
  Contnrs
  ,TypInfo
  ,SyncObjs
  ,DateUtils
  ,tiUtils
  ,tiConstants
  ,tiPersistenceLayers
  ,tiOPFManager
  ,tiOPFTestManager
  ,tiTestDependencies
  ,tiThread
  ,tiStreams
  ,tiRTTI
 ;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIMetaData);
  tiRegisterNonPersistentTest(TTestTIQueryParams);
end;


procedure TTestTIPersistenceLayers.tiOPFManager_ConnectDatabase;
var
  LM: TtiOPFManager;
begin
  LM:= TtiOPFManager.Create;
  try
     LM.PersistenceLayers.__RegisterPersistenceLayer(TestSetupData.PersistenceLayerClass);
     LM.ConnectDatabase(
       TestSetupData.DBName,
       TestSetupData.Username,
       TestSetupData.Password,
       TestSetupData.QueryParams,
       TestSetupData.PersistenceLayerName);
     try
       CheckEquals(TestSetupData.PersistenceLayerName, LM.DefaultPersistenceLayerName, 'PersistenceLayerName');
       CheckNotNull(LM.DefaultDBConnectionPool, 'DefaultDBConnectionPool');
       CheckEquals(TestSetupData.DBName, LM.DefaultDBConnectionName, 'DatabaseName');
     finally
       LM.DisconnectDatabase(TestSetupData.DBName, TestSetupData.PersistenceLayerName);
     end;
     CheckNull(LM.DefaultDBConnectionPool, 'DefaultDBConnectionPool');
  finally
    LM.Free;
  end;
end;

procedure TTestTIPersistenceLayers.Database_Connect;
var
  LDatabase: TtiDatabase;
begin
  LDatabase:= PersistenceLayer.DatabaseClass.Create;
  try
    LDatabase.Connect(
      TestSetupData.DBName,
      TestSetupData.UserName,
      TestSetupData.Password,
      TestSetupData.QueryParams,
      '');
    Check(LDatabase.Connected, 'Connect failed');
    LDatabase.Connected := false;
    Check(not LDatabase.Connected, 'Connected := false failed');
  finally
    LDatabase.Free;
  end;
end;


procedure TTestTIPersistenceLayers.DBConnectionPoolConnectDisconnect;
const
  CAlias = 'TestAliasName';
begin
  PersistenceLayer.DBConnectionPools.Connect(
    CAlias,
    TestSetupData.DBName,
    TestSetupData.Username,
    TestSetupData.Password,
    TestSetupData.QueryParams);
  CheckEquals(1, PersistenceLayer.DBConnectionPools.Count);
  PersistenceLayer.DBConnectionPools.Disconnect(CAlias);
  CheckEquals(0, PersistenceLayer.DBConnectionPools.Count);
end;

procedure TTestTIDatabase.CreateAndAttachTIQuery;
var
  LQuery: TtiQuery;
  LDatabase: TtiDatabase;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    LQuery:= LDatabase.CreateAndAttachTIQuery;
    try
      CheckIs(LQuery, LDatabase.TIQueryClass);
      CheckNotNull(LQuery.Database);
      CheckSame(LDatabase, LQuery.Database);
    finally
      LQuery.Free;
    end;
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TTestTIDatabase.CreateTableDropTable;
  procedure _CreateTableDropTable(const AFieldName : string; AFieldKind : TtiQueryFieldKind; AFieldWidth : integer);
  var
    LDatabase: TtiDatabase;
    LTable : TtiDBMetaDataTable;
    LDBMetaData : TtiDBMetaData;
    LDBMetaDataTable : TtiDBMetaDataTable;
  begin

    LDBMetaData := TtiDBMetaData.Create;
    try
      LTable := TtiDBMetaDataTable.Create;
      try
        LTable.Name := cTableNameCreateTable;
        LTable.AddInstance(AFieldName, AFieldKind, AFieldWidth);
        CreateTable(LTable);
      finally
        LTable.Free;
      end;

      try

        LDatabase:= DBConnectionPool.Lock;
        try
          LDatabase.ReadMetaDataTables(LDBMetaData);
          LDBMetaDataTable := LDBMetaData.FindByTableName(cTableNameCreateTable);
          Check(LDBMetaDataTable <> nil, 'Unable to find metadata for <test_create_table> on field  <' + AFieldName + '>');
          Check(SameText(LDBMetaDataTable.Name, cTableNameCreateTable), 'Wrong table found when searching for <test_create_table> on field <' + AFieldName + '>');
          LDatabase.ReadMetaDataFields(LDBMetaDataTable);
        finally
          DBConnectionPool.UnLock(LDatabase);
        end;

        CheckFieldMetaData(LDBMetaDataTable, AFieldName, AFieldKind, AFieldWidth);
      finally
        DropTable(cTableNameCreateTable);
      end;

      LDatabase:= DBConnectionPool.Lock;
      try
        LDBMetaData.Clear;
        LDatabase.ReadMetaDataTables(LDBMetaData);
        LDBMetaDataTable := LDBMetaData.FindByTableName(cTableNameCreateTable);
        Check(LDBMetaDataTable = nil, 'Drop table <test_create_table> failed on field <' + AFieldName + '>');
      finally
        DBConnectionPool.UnLock(LDatabase);
      end;
    finally
      LDBMetaData.Free;
    end;
  end;
begin
  _CreateTableDropTable( 'Str_Field',   qfkString,    10);
  _CreateTableDropTable( 'Int_Field',   qfkInteger,    0);
  _CreateTableDropTable( 'Float_Field', qfkFloat,      0);
  _CreateTableDropTable( 'Date_Field',  qfkDateTime,   0);
  _CreateTableDropTable( 'Bool_Field' , qfkLogical,    0);
  _CreateTableDropTable( 'Notes_Field', qfkLongString, 0);
end;

procedure TTestTIQueryAbs.EOF;
begin
  CreateTableTestGroup(Database);
  Database.StartTransaction;
  try
    FQuery.SelectRow('Test_Group', nil);
    Check(FQuery.EOF, 'FQuery.EOF = true failed.');
    FQuery.Close;
  finally
    Database.Commit;
  end;

  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    FQuery.SelectRow('Test_Group', nil);
    Check(not FQuery.EOF, 'FQuery.EOF = false failed.');
    FQuery.Close;
  finally
    Database.Rollback;
  end;
end;


procedure TTestTIQueryAbs.FieldAsBoolean;
begin
  CreateTableBoolean(Database);
  PopulateTableBoolean(True);
  Database.StartTransaction;
  try
    FQuery.SelectRow(cTIQueryTableName, nil);
    CheckEquals(True, FQuery.FieldAsBoolean[ cTIQueryColName ], 'FieldAsBoolean');
    CheckEquals(True, FQuery.FieldAsBooleanByIndex[ cFieldAs_Index ], 'FieldAsBoolean');
    FQuery.Close;
  finally
    Database.RollBack;
  end;
end;


procedure TTestTIQueryAbs.FieldAsDateTime;
var
  lNow : TDateTime;
begin
  lNow := Now;
  CreateTableDateTime(Database);
  PopulateTableDateTime(lNow);
  Database.StartTransaction;
  try
    FQuery.SelectRow(cTIQueryTableName, nil);
    CheckEquals(lNow, FQuery.FieldAsDateTime[ cTIQueryColName ], cdtOneSecond, 'FieldAsDateTime');
    CheckEquals(lNow, FQuery.FieldAsDateTimeByIndex[ cFieldAs_Index ], cdtOneSecond, 'FieldAsDateTime');
    FQuery.Close;
  finally
    Database.Rollback;
  end;
end;


procedure TTestTIQueryAbs.FieldAsFloat;
const
  cPrecision = 6;
begin
  CreateTableFloat(Database);
  PopulateTableReal(1234.5678);
  Database.StartTransaction;
  try
    FQuery.SelectRow(cTIQueryTableName, nil);
    CheckEquals(1234.5678, FQuery.FieldAsFloat[ cTIQueryColName ], 0.00001, 'Failed on 1');
    CheckEquals(1234.5678, FQuery.FieldAsFloatByIndex[ cFieldAs_Index ], 0.00001, 'Failed on 2');
    FQuery.Close;
  finally
    Database.Rollback;
  end;
end;


procedure TTestTIQueryAbs.FieldAsInteger;
begin
  CreateTableInteger(Database);
  PopulateTableInteger(1);
  Database.StartTransaction;
  try
    FQuery.SelectRow(cTIQueryTableName, nil);
    CheckEquals(1, FQuery.FieldAsInteger[ cTIQueryColName ], 'FieldAsInteger');
    CheckEquals(1, FQuery.FieldAsIntegerByIndex[ cFieldAs_Index ], 'FieldAsIntegerByIndex');
    FQuery.Close;
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQueryAbs.FieldAsString;
const
  cString = 'abcdefghij';
begin
  CreateTableString(Database);
  PopulateTableString(cString);
  Database.StartTransaction;
  try
    FQuery.SelectRow(cTIQueryTableName, nil);
    CheckEquals(cString, FQuery.FieldAsString[ cTIQueryColName ], 'FieldAsString');
    CheckEquals(cString, FQuery.FieldAsStringByIndex[ cFieldAs_Index ], 'FieldAsStringByIndex');
    FQuery.Close;
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQueryAbs.FieldCount;
var
  lFieldCount : integer;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    FQuery.SelectRow('Test_Group', nil);
    lFieldCount := FQuery.FieldCount;
    CheckEquals(7, lFieldCount);
    FQuery.Close;
  finally
    Database.Rollback;
  end;
end;


procedure TTestTIQueryAbs.FieldIndex;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    FQuery.SelectRow('Test_Group', nil);
    Check(FQuery.FieldIndex('OID'              ) = 0, 'FQuery.FieldIndex = 0 failed');
    Check(FQuery.FieldIndex('Group_Str_Field'  ) = 1, 'FQuery.FieldIndex = 1 failed');
    Check(FQuery.FieldIndex('Group_Int_Field'  ) = 2, 'FQuery.FieldIndex = 2 failed');
    Check(FQuery.FieldIndex('Group_Float_Field') = 3, 'FQuery.FieldIndex = 3 failed');
    Check(FQuery.FieldIndex('Group_Date_Field' ) = 4, 'FQuery.FieldIndex = 4 failed');
    Check(FQuery.FieldIndex('Group_Bool_Field' ) = 5, 'FQuery.FieldIndex = 5 failed');
    Check(FQuery.FieldIndex('Group_Notes_Field') = 6, 'FQuery.FieldIndex = 6 failed');
    FQuery.Close;
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQueryAbs.FieldIsNull;
var
  lParams : TtiQueryParams;
begin
  CreateTableTestGroup(Database);
  Database.StartTransaction;
  try
    lParams := TtiQueryParams.Create;
    try
      lParams.SetValueAsString('OID', '1');
      Database.InsertRow('Test_Group', lParams);
      FQuery.SelectRow('Test_Group', nil);
      Check(FQuery.FieldIsNull[ 'Group_Str_Field' ], 'FieldIsNull  = true failed');
      Check(FQuery.FieldIsNullByIndex[ cFieldAs_Index ], 'FieldIsNull  = true failed');
      FQuery.Close;
    finally
      lParams.Free;
    end;
  finally
    Database.Commit;
  end;

  Database.DeleteRow('Test_Group', nil);
  Database.StartTransaction;
  try
    lParams := TtiQueryParams.Create;
    try
      lParams.SetValueAsString('OID', '2');
      lParams.SetValueAsString('Group_Str_Field', '2');
      Database.InsertRow('Test_Group', lParams);
      FQuery.SelectRow('Test_Group', nil);
      Check(not FQuery.FieldIsNull[ 'Group_Str_Field' ], 'FieldIsNull = false failed');
      Check(not FQuery.FieldIsNullByIndex[ cFieldAs_Index ], 'FieldIsNull = false failed');
      FQuery.Close;
    finally
      lParams.Free;
    end;
  finally
    Database.Commit;
  end;

end;


procedure TTestTIQueryAbs.FieldKind;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    FQuery.SelectRow('Test_Group', nil);
    Check(FQuery.FieldKind(FQuery.FieldIndex('Group_Str_Field'  ))  = qfkString,     'FQuery.FieldKind = qfkString failed');
    Check(FQuery.FieldKind(FQuery.FieldIndex('Group_Int_Field'  ))  = qfkInteger,    'FQuery.FieldKind = qfkInteger failed');
    Check(FQuery.FieldKind(FQuery.FieldIndex('Group_Float_Field'))  = qfkFloat,      'FQuery.FieldKind = qfkFloat failed');
    Check(FQuery.FieldKind(FQuery.FieldIndex('Group_Date_Field' ))  = qfkDateTime,   'FQuery.FieldKind = qfkDateTime failed');
    if FQuery.HasNativeLogicalType then
      Check(FQuery.FieldKind(FQuery.FieldIndex('Group_Bool_Field' ))  = qfkLogical,    'FQuery.FieldKind = qfkLogical failed');
    Check(FQuery.FieldKind(FQuery.FieldIndex('Group_Notes_Field' )) = qfkLongString, 'FQuery.FieldKind = qfkLongString failed');
    FQuery.Close;
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQueryAbs.FieldName;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    FQuery.SelectRow('Test_Group', nil);
    Check(SameText(FQuery.FieldName(0), 'OID'              ),  'FQuery.FieldName(0) failed');
    Check(SameText(FQuery.FieldName(1), 'Group_Str_Field'  ),  'FQuery.FieldName(1) failed');
    Check(SameText(FQuery.FieldName(2), 'Group_Int_Field'  ),  'FQuery.FieldName(2) failed');
    Check(SameText(FQuery.FieldName(3), 'Group_Float_Field'),  'FQuery.FieldName(3) failed');
    Check(SameText(FQuery.FieldName(4), 'Group_Date_Field' ),  'FQuery.FieldName(4) failed');
    Check(SameText(FQuery.FieldName(5), 'Group_Bool_Field' ),  'FQuery.FieldName(5) failed');
    Check(SameText(FQuery.FieldName(6), 'Group_Notes_Field' ), 'FQuery.FieldName(6) failed');
    FQuery.Close;
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQueryAbs.FieldSize;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  Database.StartTransaction;
  try
    FQuery.SelectRow('Test_Group', nil);
    CheckEquals(10, FQuery.FieldSize(FQuery.FieldIndex('Group_Str_Field'   )), 'Group_Str_Field'  );
    CheckEquals(0,  FQuery.FieldSize(FQuery.FieldIndex('Group_Int_Field'   )), 'Group_Int_Field'  );
    CheckEquals(0,  FQuery.FieldSize(FQuery.FieldIndex('Group_Float_Field' )), 'Group_Float_Field');
    CheckEquals(0,  FQuery.FieldSize(FQuery.FieldIndex('Group_Date_Field'  )), 'Group_Date_Field' );
    CheckEquals(0,  FQuery.FieldSize(FQuery.FieldIndex('Group_Notes_Field' )), 'Group_Notes_Field');
    // Nasty, but I can't think of a better solution right now...
    if (PersistenceLayerName = 'DOA') or (PersistenceLayerName = 'Sqldb_IB')  or (PersistenceLayerName = 'Zeos_FB') then
      CheckEquals(1,  FQuery.FieldSize(FQuery.FieldIndex('Group_Bool_Field'  )), 'Group_Bool_Field' )
    else
      CheckEquals(0,  FQuery.FieldSize(FQuery.FieldIndex('Group_Bool_Field'  )), 'Group_Bool_Field' );

    FQuery.Close;
// qfkBinary,
// qfkMacro,
// qfkLongString
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQueryAbs.Next;
begin
  CreateTableTestGroup(Database);
  InsertIntoTestGroup(1, Database);
  InsertIntoTestGroup(2, Database);
  InsertIntoTestGroup(3, Database);
  InsertIntoTestGroup(4, Database);
  InsertIntoTestGroup(5, Database);
  Database.StartTransaction;
  try
    FQuery.SelectRow('Test_Group', nil);
    Check(not FQuery.EOF, '0 FQuery.EOF = true failed.');
    FQuery.Next;
    Check(not FQuery.EOF, '1 FQuery.EOF = true failed.');
    FQuery.Next;
    Check(not FQuery.EOF, '2 FQuery.EOF = true failed.');
    FQuery.Next;
    Check(not FQuery.EOF, '3 FQuery.EOF = true failed.');
    FQuery.Next;
    Check(not FQuery.EOF, '4 FQuery.EOF = true failed.');
    FQuery.Next;
    Check(FQuery.EOF, '5 FQuery.EOF = true failed.');
    FQuery.Close;
  finally
    Database.Commit;
  end;
end;


procedure TTestTIQueryAbs.QueryToDatabaseConnection;
begin
  FQuery.AttachDatabase(Database);
  Check(FQuery.Database <> nil, 'FQuery.AttachDatabase failed');
  FQuery.DetachDatabase;
  Check(FQuery.Database = nil, 'FQuery.DetachDatabase failed');
end;


procedure TTestTIDatabase.ReadMetaData;
var
  LDBMetaData : TtiDBMetaData;
  LDBMetaDataTable : TtiDBMetaDataTable;
  LDatabase : TtiDatabase;
begin
  CreateTestTables;
  LDBMetaData := TtiDBMetaData.Create;
  try
    LDatabase:= DBConnectionPool.Lock;
    try
      LDatabase.ReadMetaDataTables(LDBMetaData);
      LDBMetaDataTable := LDBMetaData.FindByTableName('Test_Item');
      Check(LDBMetaDataTable <> nil, 'Unable to find metadata for Test_Item');
      Check(SameText(LDBMetaDataTable.Name, 'Test_Item'), 'Wrong table found when searching for <test_item>');
      LDatabase.ReadMetaDataFields(LDBMetaDataTable);

      // So, we will just search for the field.
      // Currently, there is no field information like type or size returned.
      // This should be added.
      CheckFieldMetaData(LDBMetaDataTable, 'OID',              qfkInteger);
      CheckFieldMetaData(LDBMetaDataTable, 'OID_GROUP',        qfkInteger);
      CheckFieldMetaData(LDBMetaDataTable, 'ITEM_INT_FIELD',   qfkInteger);
      CheckFieldMetaData(LDBMetaDataTable, 'ITEM_FLOAT_FIELD', qfkFloat);
      CheckFieldMetaData(LDBMetaDataTable, 'ITEM_STR_FIELD',   qfkString, 10);
      CheckFieldMetaData(LDBMetaDataTable, 'ITEM_Bool_FIELD',  qfkLogical);
      CheckFieldMetaData(LDBMetaDataTable, 'ITEM_Date_FIELD',  qfkDateTime);
      CheckFieldMetaData(LDBMetaDataTable, 'ITEM_Notes_FIELD', qfkLongString);

      LDBMetaDataTable := LDBMetaData.FindByTableName('Test_Group');
      Check(LDBMetaDataTable <> nil, 'Unable to find metadata for test_group');
      Check(SameText(LDBMetaDataTable.Name, 'Test_Group'), 'Wrong table found when searching for <test_group>');
      LDatabase.ReadMetaDataFields(LDBMetaDataTable);

      CheckFieldMetaData(LDBMetaDataTable, 'OID',               qfkInteger);
      CheckFieldMetaData(LDBMetaDataTable, 'GROUP_INT_FIELD',   qfkInteger);
      CheckFieldMetaData(LDBMetaDataTable, 'GROUP_FLOAT_FIELD', qfkFloat  );
      CheckFieldMetaData(LDBMetaDataTable, 'GROUP_STR_FIELD',   qfkString, 10);
      CheckFieldMetaData(LDBMetaDataTable, 'GROUP_Bool_FIELD',  qfkLogical);
      CheckFieldMetaData(LDBMetaDataTable, 'GROUP_Date_FIELD',  qfkDateTime);
      CheckFieldMetaData(LDBMetaDataTable, 'GROUP_Notes_FIELD', qfkLongString);
    finally
      DBConnectionPool.UnLock(LDatabase);
    end;
  finally
    LDBMetaData.Free;
  end;
end;


procedure TTestTIDatabase.tiOPFManager_DoExecInsertSQLCheck(const ACheckMethod: TtiOPFManager_ExecInsertSQLCheckMethod);
var
  LDatabase: TtiDatabase;
  LQuery: TtiQuery;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    LDatabase.StartTransaction;
    LQuery:= LDatabase.CreateAndAttachTIQuery;
    try
      LQuery.SelectRow(cTIQueryTableName, nil);
      Check(not LQuery.EOF);
      CheckEquals(2, LQuery.FieldCount);
      CheckEquals('OID', UpperCase(LQuery.FieldName(0)));
      CheckEquals(UpperCase(cTIQueryColName), UpperCase(LQuery.FieldName(1)));
      ACheckMethod(LQuery);
      LQuery.Next;
      Check(LQuery.EOF);
    finally
      LQuery.Free;
    end;
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TTestTIDatabase.tiOPFManager_DoExecInsertSQLCheckInteger(const AQuery: TtiQuery);
begin
  CheckEquals(100, AQuery.FieldAsInteger[cTIQueryColName]);
end;

procedure TTestTIDatabase.tiOPFManager_DoExecInsertSQLCheckBoolean(
  const AQuery: TtiQuery);
begin
  CheckEquals(True, AQuery.FieldAsBoolean[cTIQueryColName]);
end;

procedure TTestTIDatabase.tiOPFManager_DoExecInsertSQLCheckDateTime(
  const AQuery: TtiQuery);
begin
  CheckEquals(EncodeDateTime(2010, 12, 31, 12, 34, 56, 0), AQuery.FieldAsDateTime[cTIQueryColName]);
end;

procedure TTestTIDatabase.tiOPFManager_DoExecInsertSQLCheckFloat(
  const AQuery: TtiQuery);
begin
  CheckNearEnough(123.456, AQuery.FieldAsFloat[cTIQueryColName]);
end;

procedure TTestTIDatabase.tiOPFManager_DoExecInsertSQLCheckLongString(
  const AQuery: TtiQuery);
begin
  CheckEquals(LongString, AQuery.FieldAsString[cTIQueryColName]);
end;

procedure TTestTIDatabase.tiOPFManager_DoExecInsertSQLCheckString(
  const AQuery: TtiQuery);
begin
  CheckEquals('test', AQuery.FieldAsString[cTIQueryColName]);
end;

procedure TTestTIDatabase.tiOPFManager_ExecInsertSQLBoolean;
begin
  CreateTableBoolean;
  GTIOPFManager.ExecInsertSQL(
    cTIQueryTableName,
    [cTIQueryColName],
    [True],
    TestSetupData.DBName,
    TestSetupData.PersistenceLayerName);
  tiOPFManager_DoExecInsertSQLCheck(tiOPFManager_DoExecInsertSQLCheckBoolean);
end;

procedure TTestTIDatabase.tiOPFManager_ExecInsertSQLDateTime;
begin
  CreateTableDateTime;
  // The TVarRec container used to pass parameter values does not support
  // TDateTime directly, but wrapping it in a variant works.
  GTIOPFManager.ExecInsertSQL(
    cTIQueryTableName,
    [cTIQueryColName],
    [Variant(EncodeDateTime(2010, 12, 31, 12, 34, 56, 0))],
    TestSetupData.DBName,
    TestSetupData.PersistenceLayerName);
  tiOPFManager_DoExecInsertSQLCheck(tiOPFManager_DoExecInsertSQLCheckDateTime);
end;

procedure TTestTIDatabase.tiOPFManager_ExecInsertSQLFloat;
begin
  CreateTableFloat;
  GTIOPFManager.ExecInsertSQL(
    cTIQueryTableName,
    [cTIQueryColName],
    [123.456],
    TestSetupData.DBName,
    TestSetupData.PersistenceLayerName);
  tiOPFManager_DoExecInsertSQLCheck(tiOPFManager_DoExecInsertSQLCheckFloat);
end;

procedure TTestTIDatabase.tiOPFManager_ExecInsertSQLInteger;
begin
  CreateTableInteger;
  GTIOPFManager.ExecInsertSQL(
    cTIQueryTableName,
    [cTIQueryColName],
    [100],
    TestSetupData.DBName,
    TestSetupData.PersistenceLayerName);
  tiOPFManager_DoExecInsertSQLCheck(tiOPFManager_DoExecInsertSQLCheckInteger);
end;

procedure TTestTIDatabase.tiOPFManager_ExecInsertSQLLongString;
begin
  CreateTableLongString;
  GTIOPFManager.ExecInsertSQL(
    cTIQueryTableName,
    [cTIQueryColName],
    [LongString],
    TestSetupData.DBName,
    TestSetupData.PersistenceLayerName);
  tiOPFManager_DoExecInsertSQLCheck(tiOPFManager_DoExecInsertSQLCheckLongString);
end;

procedure TTestTIDatabase.tiOPFManager_ExecInsertSQLString;
begin
  CreateTableString;
  GTIOPFManager.ExecInsertSQL(
    cTIQueryTableName,
    [cTIQueryColName],
    ['test'],
    TestSetupData.DBName,
    TestSetupData.PersistenceLayerName);
  tiOPFManager_DoExecInsertSQLCheck(tiOPFManager_DoExecInsertSQLCheckString);
end;

procedure TTestTIQueryAbs.SetUp;
begin
  inherited;
  FDatabase:= DBConnectionPool.Lock;
  FQuery:= FDatabase.CreateAndAttachTIQuery;
end;


procedure TTestTIQueryAbs.TearDown;
begin
  DBConnectionPool.UnLock(FDatabase);
  FQuery.Free;
  inherited;
end;

procedure TTestTIDatabase.Transaction_InTransaction;
var
  LDatabase: TtiDatabase;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    LDatabase.StartTransaction;
    Check(LDatabase.InTransaction, 'Database not in a transaction');
    LDatabase.Commit;
    Check(not LDatabase.InTransaction, 'Database in a transaction when it should not be');
    LDatabase.StartTransaction;
    Check(LDatabase.InTransaction, 'Database not in a transaction');
    LDatabase.RollBack;
    Check(not LDatabase.InTransaction, 'Database in a transaction when it should not be');
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TTestTIDatabase.CheckFieldMetaData(
  const pDBMetaDataTable: TtiDBMetaDataTable; const AFieldName: string;
  pKind: TtiQueryFieldKind; pWidth: integer);
var
  lField : TtiDBMetaDataField;
begin
  lField := pDBMetaDataTable.FindByFieldName(AFieldName);
  Check(lField <> nil,   'Field <' + AFieldName + '> not found');
// ToDo: The meta data system wont read in field kind or size yet. Requires work.
//      Check(lField.Kind = pKind, 'Field <' + AFieldName + '> field kind of wrong type');
//      if pWidth <> 0 then
//        Check(lField.Width = pWidth, 'Field <' + AFieldName + '> field width of wrong size');
end;

procedure TTestTIQueryAbs.TypeKindToQueryFieldKind;
begin
  Check(tiTypeKindToQueryFieldKind(tiTKInteger) = qfkInteger,  'Failed on tiTKInteger ');
  Check(tiTypeKindToQueryFieldKind(tiTKFloat  ) = qfkFloat   , 'Failed on tiTKFloat   ');
  Check(tiTypeKindToQueryFieldKind(tiTKString ) = qfkString  , 'Failed on tiTKString  ');
  Check(tiTypeKindToQueryFieldKind(tiTKDateTime) = qfkDateTime, 'Failed on tiTKDateTime');
  Check(tiTypeKindToQueryFieldKind(tiTKBoolean) = qfkLogical , 'Failed on tiTKBoolean ');
end;

type

  TThrdDBConnectionPoolTest = class(TtiThread)
  private
    FCycles: integer;
    FDone: Boolean;
    FCritSect: TCriticalSection;
    FDBConnectionPool: TtiDBConnectionPool;
    function GetDone: Boolean;
    procedure SetDone(const Value: Boolean);
  public
    constructor CreateExt(const ADBConnectionPool: TtiDBConnectionPool;
                          const ACycles: integer);
    destructor  Destroy; override;
    procedure   Execute; override;
    property    Done: Boolean read GetDone write SetDone;
  end;

  constructor TThrdDBConnectionPoolTest.CreateExt(
    const ADBConnectionPool: TtiDBConnectionPool;
    const ACycles : integer);
  begin
    Create(true);
    FCritSect:= TCriticalSection.Create;
    FreeOnTerminate := false;
    FCycles := ACycles;
    FDBConnectionPool:= ADBConnectionPool;
    Done := False;
  end;

  destructor TThrdDBConnectionPoolTest.Destroy;
begin
  FCritSect.Free;
  inherited;
end;

  procedure TThrdDBConnectionPoolTest.Execute;
  var
    i : integer;
    LDatabase : TtiDatabase;
  begin
    for i := 1 to FCycles do
    begin
      LDatabase := FDBConnectionPool.Lock;
      Sleep(100);
      FDBConnectionPool.UnLock(LDatabase);
    end;
    Done := True;
  end;

function TThrdDBConnectionPoolTest.GetDone: Boolean;
begin
  FCritSect.Enter;
  try
    result:= FDone;
  finally
    FCritSect.Leave;
  end;
end;

procedure TThrdDBConnectionPoolTest.SetDone(const Value: Boolean);
begin
  FCritSect.Enter;
  try
    FDone:= Value;
  finally
    FCritSect.Leave;
  end;
end;

procedure TTestTIPersistenceLayers.DoThreadedDBConnectionPool(
  const ADBConnectionPool: TtiDBConnectionPool; const AThreadCount : integer);
  procedure _CreateThreads(
    const ADBConnectionPool: TtiDBConnectionPool;
    const AList : TList; const AThreadCount, AIterations : integer);
  var
    i : integer;
  begin
    for i := 1 to AThreadCount do
      AList.Add(TThrdDBConnectionPoolTest.CreateExt(
                   ADBConnectionPool,
                   AIterations));
  end;

  procedure _StartThreads(AList : TList);
  var
    i : integer;
  begin
    for i := 0 to AList.Count - 1 do
      TThread(AList.Items[i]).Start;
  end;

  procedure _WaitForThreads(AList : TList);
  var
    i : integer;
    LAllFinished : boolean;
  begin
    LAllFinished := false;
    while not LAllFinished do
    begin
      LAllFinished := true;
      for i := 0 to AList.Count - 1 do
      begin
        LAllFinished := LAllFinished and TThrdDBConnectionPoolTest(AList.Items[i]).Done;
      end;
      Sleep(100);
    end;
  end;
var
  LList : TObjectList;
begin
  Check(True); // To Force OnCheckCalled to be called
  LList := TObjectList.Create;
  try
    _CreateThreads(ADBConnectionPool, LList, AThreadCount, cuIterations);
    _StartThreads(LList);
    _WaitForThreads(LList);
  finally
    LList.Free;
  end;
end;

procedure TTestTIPersistenceLayers.NonThreadedDBConnectionPool;
var
  i : integer;
  LDatabase : TtiDatabase;
  lDBConnectionName : string;
const
  CAlias = 'TestAliasName';
begin
  PersistenceLayer.DBConnectionPools.Connect(
    CAlias,
    TestSetupData.DBName,
    TestSetupData.Username,
    TestSetupData.Password,
    TestSetupData.QueryParams);
  try
    lDBConnectionName := TestSetupData.DBName;
    for i := 1 to 1 do
    begin
      LDatabase := PersistenceLayer.DBConnectionPools.Lock(CAlias);
      CheckNotNull(LDatabase);
      PersistenceLayer.DBConnectionPools.UnLock(CAlias, LDatabase);
    end;
  finally
    PersistenceLayer.DBConnectionPools.Disconnect(CAlias);
  end;
end;

procedure TTestTIPersistenceLayers.ThreadedDBConnectionPool;
const
  CAlias = 'TestAliasName';
begin
  if PersistenceLayerSupportsMultiUser then
  begin
    PersistenceLayer.DBConnectionPools.Connect(
      CAlias,
      TestSetupData.DBName,
      TestSetupData.Username,
      TestSetupData.Password,
      TestSetupData.QueryParams);
    try
      DoThreadedDBConnectionPool(PersistenceLayer.DefaultDBConnectionPool, CThreadCount)
    finally
      PersistenceLayer.DBConnectionPools.Disconnect(CAlias);
    end;
  end else
    Check(True);
end;

{ TTestTIDatabase }

procedure TTestTIDatabase.Transaction_Commit;
var
  LQuery : TtiQuery;
  LDatabase: TtiDatabase;
begin
  CreateTableTestGroup;
  try
    InsertIntoTestGroup(1);
    LDatabase:= DBConnectionPool.Lock;
    try
      LQuery := LDatabase.CreateAndAttachTIQuery;
      try
        Check(not LDatabase.InTransaction);
        LDatabase.StartTransaction;
        Check(LDatabase.InTransaction);
        LQuery.SelectRow(cTableNameTestGroup, nil);
        Check(not LQuery.EOF, 'Transaction not committed');
        LQuery.Next;
        Check(LQuery.EOF, 'Wrong number of records');
        LDatabase.Commit;
        Check(not LDatabase.InTransaction);
      finally
        LQuery.Free;
      end;
    finally
      DBConnectionPool.UnLock(LDatabase);
    end;
  finally
    DropTable(cTableNameTestGroup);
  end;
end;

procedure TTestTIDatabase.Transaction_RollBack;
var
  LDatabase: TtiDatabase;
  LQuery : TtiQuery;
  LParams: TtiQueryParams;
  LEOF : boolean;
begin
  try
    CreateTableTestGroup;
    LDatabase:= DBConnectionPool.Lock;
    try
      LQuery := nil;
      LParams:= nil;
      try
        LQuery := LDatabase.CreateAndAttachTIQuery;
        LParams:= TtiQueryParams.Create;
        LParams.SetValueAsInteger('oid', 1);
        LDatabase.StartTransaction;
        LQuery.InsertRow(cTableNameTestGroup, LParams);
        LDatabase.Rollback;

        LDatabase.StartTransaction;
        LQuery.SelectRow(cTableNameTestGroup, nil);
        LEOF := LQuery.EOF;
        Check(LEOF, 'Transaction not rolled back');
        LQuery.Close;

        LQuery.InsertRow(cTableNameTestGroup, LParams);
        LDatabase.Commit;

        LDatabase.StartTransaction;
        LQuery.SelectRow(cTableNameTestGroup, nil);
        LEOF := LQuery.EOF;
        LDatabase.Commit;

        LDatabase.StartTransaction;
        Check(not LEOF, 'Transaction not committed');

      finally
        LQuery.Free;
        LParams.Free;
      end;
    finally
      DBConnectionPool.UnLock(LDatabase);
    end;
  finally
    DropTable(cTableNameTestGroup);
  end;
end;

procedure TTestTIDatabase.CreateTableDropTable_Timing;
var
  LDatabase: TtiDatabase;
  LTable1         : TtiDBMetaDataTable;
  LTable2         : TtiDBMetaDataTable;
  LCreateTableTime : DWord;
  LDropTableTime  : DWord;
  LMetaDataTime   : DWord;
  LBulkTestStart:   DWord;
  LSingleTestStart: DWord;
  LCount               : integer;
begin
  Check(True); // To Force OnCheckCalled to be called
  LCreateTableTime := 0;
  LDropTableTime  := 0;
  LMetaDataTime   := 0;
  DropTable(cTableNameCreateTable);
  LDatabase:= DBConnectionPool.Lock;
  try
    LTable1 := TtiDBMetaDataTable.Create;
    try
      LTable1.Name := cTableNameCreateTable;
      LTable2 := TtiDBMetaDataTable.Create;
      try
        LTable2.Name := cTableNameCreateTable;
        LTable1.AddInstance('test', qfkString, 10);
        LCount:= 0;
        LBulkTestStart:= tiGetTickCount;
        while (tiGetTickCount - LBulkTestStart) < (CTimingTestPeriod*1000) do
        begin
          Inc(LCount);
          // Create Table
          LSingleTestStart := tiGetTickCount;
          LDatabase.CreateTable(LTable1);
          Inc(LCreateTableTime, tiGetTickCount - LSingleTestStart);

          // Meta Data
          LTable2.Clear;
          LSingleTestStart := tiGetTickCount;
          LDatabase.ReadMetaDataFields(LTable2);
          Inc(LMetaDataTime, tiGetTickCount - LSingleTestStart);

          // Drop Table
          LSingleTestStart := tiGetTickCount;
          LDatabase.DropTable(cTableNameCreateTable);
          Inc(LDropTableTime, tiGetTickCount - LSingleTestStart);

        end;
        WriteTimingResult('TableTestIterationCount',     TestSetupData.PersistenceLayerName, LCount);
        WriteTimingResult('TotalTestTime',  TestSetupData.PersistenceLayerName, CTimingTestPeriod);
        WriteTimingResult('CreateTable',    TestSetupData.PersistenceLayerName, tiSafeDiv(LCreateTableTime, LCount));
        WriteTimingResult('DropTableTable', TestSetupData.PersistenceLayerName, tiSafeDiv(LDropTableTime, LCount));
        WriteTimingResult('ReadMetaData',   TestSetupData.PersistenceLayerName, tiSafeDiv(LMetaDataTime, LCount));
      finally
        LTable2.Free;
      end;
    finally
      LTable1.Free;
    end;

  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;

procedure TTestTIDatabase.CreateTIQuery;
var
  LQuery: TtiQuery;
  LDatabase: TtiDatabase;
begin
  LDatabase:= DBConnectionPool.Lock;
  try
    LQuery:= LDatabase.CreateTIQuery;
    try
      CheckIs(LQuery, LDatabase.TIQueryClass);
      CheckNull(LQuery.Database);
    finally
      LQuery.Free;
    end;
  finally
    DBConnectionPool.UnLock(LDatabase);
  end;
end;

{ TTestTIMetaData }

procedure TTestTIMetaData.DBMetaDataAdd;
var
  lList : TtiDBMetaData;
  lData : TtiDBMetaDataTable;
begin
  lList := TtiDBMetaData.Create;
  try
    CheckEquals(0, lList.Count, 'Count');
    lData := TtiDBMetaDataTable.Create;
    lList.Add(lData);
    CheckEquals(1, lList.Count, 'Count');
    CheckSame(lData, lList.Items[0]);
    lList.Clear;
    CheckEquals(0, lList.Count, 'Count');
  finally
    lList.Free;
  end;
end;

procedure TTestTIMetaData.DBMetaDataFindByTableName;
var
  lList : TtiDBMetaData;
  lData : TtiDBMetaDataTable;
begin
  lList := TtiDBMetaData.Create;
  try
    lData := TtiDBMetaDataTable.Create;
    lData.Name := 'test1';
    lList.Add(lData);
    lData := TtiDBMetaDataTable.Create;
    lData.Name := 'test2';
    lList.Add(lData);
    lData := TtiDBMetaDataTable.Create;
    lData.Name := 'test3';
    lList.Add(lData);

    lData := lList.FindByTableName('test2');
    CheckNotNull(lData);
    CheckEquals('test2', lData.Name);

    lData := lList.FindByTableName('TEST3');
    CheckNotNull(lData);
    CheckEquals('test3', lData.Name);

  finally
    lList.Free;
  end;
end;

procedure TTestTIMetaData.MetaDataFieldClone;
var
  lField1 : TtiDBMetaDataField;
  lField2 : TtiDBMetaDataField;
begin
  lField1 := TtiDBMetaDataField.Create;
  try
    lField1.Name := 'test';
    lField1.Width := 10;
    lField1.Kind := qfkString;
    lField2 := lField1.Clone;
    try
      CheckEquals(lField1.Name, lField2.Name, 'lTable2.Name');
      CheckEquals(lField1.Width, lField2.Width, 'lTable2.Width');
      CheckEquals(lField1.KindAsStr, lField2.KindAsStr, 'lTable2.KindAsStr');
    finally
      lField2.Free;
    end;
  finally
    lField1.Free;
  end;
end;

procedure TTestTIMetaData.MetaDataTableAdd;
var
  lList : TtiDBMetaDataTable;
  lData : TtiDBMetaDataField;
begin
  lList := TtiDBMetaDataTable.Create;
  try
    CheckEquals(0, lList.Count, 'Count');
    lData := TtiDBMetaDataField.Create;
    lList.Add(lData);
    CheckEquals(1, lList.Count, 'Count');
    CheckSame(lData, lList.Items[0]);
    lList.Clear;
    CheckEquals(0, lList.Count, 'Count');
  finally
    lList.Free;
  end;
end;

procedure TTestTIMetaData.MetaDataTableAddField;
var
  lList : TtiDBMetaDataTable;
begin
  lList := TtiDBMetaDataTable.Create;
  try
    CheckEquals(0, lList.Count, 'Count');
    lList.AddInstance('test1', qfkString, 10);
    CheckEquals(1, lList.Count, 'Count');
    lList.AddInstance('test2', qfkInteger);
    CheckEquals(2, lList.Count, 'Count');
    lList.AddInstance('test3', qfkFloat);
    CheckEquals(3, lList.Count, 'Count');
    lList.AddInstance('test4', qfkDateTime);
    CheckEquals(4, lList.Count, 'Count');
    lList.AddInstance('test5', qfkLogical);
    CheckEquals(5, lList.Count, 'Count');
    lList.Clear;
    CheckEquals(0, lList.Count, 'Count');
  finally
    lList.Free;
  end;
end;

procedure TTestTIMetaData.MetaDataTableClone;
var
  lTable1 : TtiDBMetaDataTable;
  lTable2 : TtiDBMetaDataTable;
  lData : TtiDBMetaDataField;
begin
  lTable1 := TtiDBMetaDataTable.Create;
  try
    lTable1.Name := 'test';
    lData := TtiDBMetaDataField.Create;
    lData.Name := 'test';
    lData.Width := 10;
    lData.Kind := qfkString;
    lTable1.Add(lData);

    lTable2 := lTable1.Clone;
    try
      CheckEquals('test', lTable2.Name, 'Name');
      CheckEquals(lTable1.Count, lTable2.Count, 'Count');
      CheckEquals(lTable1.Items[0].Name, lTable2.Items[0].Name, 'lTable2.Items[0].Name');
      CheckEquals(lTable1.Items[0].Width, lTable2.Items[0].Width, 'lTable2.Items[0].Width');
      CheckEquals(lTable1.Items[0].KindAsStr, lTable2.Items[0].KindAsStr, 'lTable2.Items[0].KindAsStr');
    finally
      lTable2.Free;
    end;

  finally
    lTable1.Free;
  end;
end;

procedure TTestTIMetaData.MetaDataTableFindByFieldName;
var
  lList : TtiDBMetaDataTable;
  lData : TtiDBMetaDataField;
begin
  lList := TtiDBMetaDataTable.Create;
  try
    lData := TtiDBMetaDataField.Create;
    lData.Name := 'test1';
    lList.Add(lData);
    lData := TtiDBMetaDataField.Create;
    lData.Name := 'test2';
    lList.Add(lData);
    lData := TtiDBMetaDataField.Create;
    lData.Name := 'test3';
    lList.Add(lData);

    lData := lList.FindByFieldName('test2');
    CheckNotNull(lData);
    CheckEquals('test2', lData.Name);

    lData := lList.FindByFieldName('TEST3');
    CheckNotNull(lData);
    CheckEquals('test3', lData.Name);

  finally
    lList.Free;
  end;
end;

procedure TTestTIMetaData.MetaDataTableMaxFieldNameWidth;
var
  lList : TtiDBMetaDataTable;
  lData : TtiDBMetaDataField;
begin
  lList := TtiDBMetaDataTable.Create;
  try
    lData := TtiDBMetaDataField.Create;
    lData.Name := 'a';
    lList.Add(lData);
    CheckEquals(1, lList.MaxFieldNameWidth);
    lData := TtiDBMetaDataField.Create;
    lData.Name := 'ab';
    lList.Add(lData);
    CheckEquals(2, lList.MaxFieldNameWidth);
    lData := TtiDBMetaDataField.Create;
    lData.Name := 'abc';
    lList.Add(lData);
    CheckEquals(3, lList.MaxFieldNameWidth);
    lData := TtiDBMetaDataField.Create;
    lData.Name := 'def';
    lList.Add(lData);
    CheckEquals(3, lList.MaxFieldNameWidth);

  finally
    lList.Free;
  end;
end;

procedure TTestTIQueryAbs.ConfirmSetupWorks;
begin
  CheckNotNull(PersistenceLayer, 'RegPerlayerNotAssigned');
  CheckEquals(TestSetupData.PersistenceLayerName, PersistenceLayer.PersistenceLayerName, 'Wrong RegPerLayer');
  Check(not Database.InTransaction, 'Database InTransaction when it should not be');
end;

procedure TTestTIQueryAbs.ConfirmDBConnectionWorks;
begin
  Database.StartTransaction;
  try
    Check(Database.InTransaction, 'Database not InTransaction when it should be');
    // Do nothing;
  finally
    Database.Commit;
  end;
end;

procedure TTestTIQueryAbs.PopulateTableString(const AValue : String);
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString(cTIQueryColName, AValue);
    Database.InsertRow(cTIQueryTableName, lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryAbs.FieldAsStringLong1;
begin
  DoFieldAsStringLong(    1);
end;

procedure TTestTIQueryAbs.FieldAsStringLong10;
begin
  DoFieldAsStringLong(   10);
end;

procedure TTestTIQueryAbs.FieldAsStringLong100;
begin
  DoFieldAsStringLong(  100);
end;

procedure TTestTIQueryAbs.FieldAsStringLong255;
begin
  DoFieldAsStringLong(  255);
end;

procedure TTestTIQueryAbs.FieldAsStringLong256;
begin
  DoFieldAsStringLong(  256);
end;

procedure TTestTIQueryAbs.FieldAsStringLong257;
begin
  DoFieldAsStringLong(  257);
end;

procedure TTestTIQueryAbs.FieldAsStringLong511;
begin
  DoFieldAsStringLong(  511);
end;

procedure TTestTIQueryAbs.FieldAsStringLong512;
begin
  DoFieldAsStringLong(  512);
end;

procedure TTestTIQueryAbs.FieldAsStringLong513;
begin
  DoFieldAsStringLong(  513);
end;

procedure TTestTIQueryAbs.FieldAsStringLong1023;
begin
  DoFieldAsStringLong( 1023);
end;

procedure TTestTIQueryAbs.FieldAsStringLong1024;
begin
  DoFieldAsStringLong( 1024);
end;

procedure TTestTIQueryAbs.FieldAsStringLong1025;
begin
  DoFieldAsStringLong( 1025);
end;

procedure TTestTIQueryAbs.FieldAsStringLong5000;
begin
  DoFieldAsStringLong( 5000);
end;

procedure TTestTIQueryAbs.FieldAsStringLong10000;
begin
  DoFieldAsStringLong(10000);
end;

procedure TTestTIQueryAbs.DoFieldAsStringLong(pStrLen : integer);
var
  lValue : string;
  lTarget : string;
begin
  lTarget := tiCreateStringOfSize(pStrLen);
  CreateTableLongString(Database);
  PopulateTableString(lTarget);
  Database.StartTransaction;
  try
    FQuery.SelectRow(cTIQueryTableName, nil);
    lValue := FQuery.FieldAsString[ cTIQueryColName ];

    CheckEquals(Length(lTarget), Length(lValue), 'FieldAsString: ' + IntToStr(pStrLen));
    CheckEquals(lTarget, lValue, 'FieldAsString: ' + IntToStr(pStrLen));

    lValue := FQuery.FieldAsStringByIndex[ cFieldAs_Index ];
    CheckEquals(lTarget, lValue, 'FieldAsStringByIndex: ' + IntToStr(pStrLen));

    FQuery.Close;
  finally
    Database.Commit;
  end;
end;

procedure TTestTIQueryAbs.PopulateTableReal(const AValue: Extended);
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsFloat(cTIQueryColName, AValue);
    Database.InsertRow(cTIQueryTableName, lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryAbs.PopulateTableInteger(const AValue: Integer);
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsInteger(cTIQueryColName, AValue);
    Database.InsertRow(cTIQueryTableName, lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryAbs.PopulateTableDateTime(const AValue: TDateTime);
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsDateTime(cTIQueryColName, AValue);
    Database.InsertRow(cTIQueryTableName, lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryAbs.PopulateTableBoolean(const AValue: Boolean);
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsBoolean(cTIQueryColName, AValue);
    Database.InsertRow(cTIQueryTableName, lParams);
  finally
    lParams.Free;
  end;
end;

{ TTestTIQueryParams }

procedure TTestTIQueryParams.AsBoolean;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsBoolean('param1', True);
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamBoolean, 'Wrong class');
    CheckEquals(True, lParams.GetValueAsBoolean('param1'), 'GetValueAsBoolean failed');
{$IFDEF BOOLEAN_NUM_1}
    CheckEquals('1', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
{$ELSE}
    CheckEquals('T', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
{$ENDIF}

    lParams.SetValueAsString('param1', 'False');
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamBoolean, 'Wrong class');
    CheckEquals(False, lParams.GetValueAsBoolean('param1'), 'GetValueAsBoolean failed');
{$IFDEF BOOLEAN_NUM_1}
    CheckEquals('0', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
{$ELSE}
    CheckEquals('F', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
{$ENDIF}
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsDateTime;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
  lDate : TDateTime;
begin
  lDate := EncodeDate(2004, 01, 01);
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsDateTime('param1', lDate);
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamDateTime, 'Wrong class');
    CheckEquals(lDate, lParams.GetValueAsDateTime('param1'), 0.0001, 'GetValueAsDateTime failed');
    CheckEquals(tiDateTimeAsXMLString(lDate), lParams.GetValueAsString('param1'), 'GetValueAsString failed');

    lDate := EncodeDate(2004, 02, 02);
    lParams.SetValueAsString('param1', tiDateTimeAsXMLString(lDate));
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamDateTime, 'Wrong class');
    CheckEquals(lDate, lParams.GetValueAsDateTime('param1'), 0.0001, 'GetValueAsDateTime failed');
    CheckEquals(tiDateTimeAsXMLString(lDate), lParams.GetValueAsString('param1'), 'GetValueAsString failed');

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsFloat;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsFloat('param1', 123.456);
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamFloat, 'Wrong class');
    CheckEquals(123.456, lParams.GetValueAsFloat('param1'), 0.0001, 'GetValueAsFloat failed');
    CheckEquals('123.456', lParams.GetValueAsString('param1'), 'GetValueAsString failed');

    lParams.SetValueAsString('param1', '456.789');
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamFloat, 'Wrong class');
    CheckEquals(456.789, lParams.GetValueAsFloat('param1'), 0.0001,  'GetValueAsFloat failed');
    CheckEquals('456.789', lParams.GetValueAsString('param1'), 'GetValueAsString failed');

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsInteger;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsInteger('param1', 123);
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamInteger, 'Wrong class');
    CheckEquals(123, lParams.GetValueAsInteger('param1'), 'GetValueAsInteger failed');
    CheckEquals('123', lParams.GetValueAsString('param1'), 'GetValueAsString failed');

    lParams.SetValueAsString('param1', '456');
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamInteger, 'Wrong class');
    CheckEquals(456, lParams.GetValueAsInteger('param1'), 'GetValueAsInteger failed');
    CheckEquals('456', lParams.GetValueAsString('param1'), 'GetValueAsString failed');

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsStream;
var
  lParams : TtiQueryParams;
  lParam  : TtiQueryParamAbs;
  lStream1 : TStringStream;
  lStream2 : TStream;
  ls      : string;
begin
  lStream1 := TStringStream.Create(LongString);
  try
      lParams := TtiQueryParams.Create;
      try
        lParams.SetValueAsStream('param1', lStream1);
        CheckEquals(1, lParams.Count, 'Count');
        lParam := lParams.Items[0];
        CheckNotNull(lParam, 'NotNull failed');
        CheckIs(lParam, TtiQueryParamStream, 'Wrong class');
        lStream2 := lParams.GetValueAsStream('param1');
        CheckStreamContentsSame(lStream1, lStream2);
        ls := lParams.GetValueAsString('param1');
        // Because, it's MIME encoded when it's read out as a string
        // (for persisting to a DB that does not have a native BIN field type)
        ls := MimeDecodeString(ls);
        CheckEquals(Length(LongString), Length(ls), ' length of strings #1');
        CheckEquals(LongString, ls, 'GetValueAsString failed');

        lStream1.WriteString('a');
        lParams.SetValueAsStream('param1', lStream1);
        CheckEquals(1, lParams.Count, 'Count');
        lParam := lParams.Items[0];
        CheckNotNull(lParams, 'NotNull failed');
        CheckIs(lParam, TtiQueryParamStream, 'Wrong class');
        lStream2 := lParams.GetValueAsStream('param1');
        CheckStreamContentsSame(lStream1, lStream2);
        ls := lParams.GetValueAsString('param1');
        // Because, it's MIME encoded when it's read out as a string
        // (for persisting to a DB that does not have a native BIN field type)
        ls := MimeDecodeString(ls);
        CheckEquals(lStream1.Size, Length(ls), ' length of strings #2');
        CheckEquals(LongString+'a', ls, 'GetValueAsString failed');
      finally
        lParams.Free;
      end;
  finally
    lStream1.Free;
  end;
end;

procedure TTestTIQueryParams.AsString;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString('param1', 'test');
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamString, 'Wrong class');
    CheckEquals('test', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsVariant_Boolean;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsVariant('param1', True);
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamBoolean, 'Wrong class');
    CheckEquals(True, lParams.GetValueAsBoolean('param1'), 'GetValueAsBoolean failed');
{$IFDEF BOOLEAN_NUM_1}
    CheckEquals('1', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
{$ELSE}
    CheckEquals('T', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
{$ENDIF}

    lParams.SetValueAsVariant('param1', 'False');
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamBoolean, 'Wrong class');
    CheckEquals(False, lParams.GetValueAsBoolean('param1'), 'GetValueAsBoolean failed');
{$IFDEF BOOLEAN_NUM_1}
    CheckEquals('0', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
{$ELSE}
    CheckEquals('F', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
{$ENDIF}
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsVariant_DateTime;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
  lDate : TDateTime;
begin
  lDate := EncodeDate(2004, 01, 01);
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsVariant('param1', lDate);
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamDateTime, 'Wrong class');
    CheckEquals(lDate, lParams.GetValueAsDateTime('param1'), 0.0001, 'GetValueAsDateTime failed');

    lDate := EncodeDate(2004, 02, 02);
    lParams.SetValueAsVariant('param1', tiDateTimeAsXMLString(lDate));
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamDateTime, 'Wrong class');
    CheckEquals(lDate, lParams.GetValueAsDateTime('param1'), 0.0001, 'GetValueAsDateTime failed');
    CheckEquals(tiDateTimeAsXMLString(lDate), lParams.GetValueAsString('param1'), 'GetValueAsString failed');
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsVariant_Float;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsVariant('param1', 123.456);
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamFloat, 'Wrong class');
    CheckEquals(123.456, lParams.GetValueAsFloat('param1'), 0.0001, 'GetValueAsFloat failed');
    CheckEquals('123.456', lParams.GetValueAsString('param1'), 'GetValueAsString failed');

    lParams.SetValueAsVariant('param1', '456.789');
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamFloat, 'Wrong class');
    CheckEquals(456.789, lParams.GetValueAsFloat('param1'), 0.0001,  'GetValueAsFloat failed');
    CheckEquals('456.789', lParams.GetValueAsString('param1'), 'GetValueAsString failed');

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsVariant_Integer;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsVariant('param1', 123);
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamInteger, 'Wrong class');
    CheckEquals(123, lParams.GetValueAsInteger('param1'), 'GetValueAsInteger failed');
    CheckEquals('123', lParams.GetValueAsString('param1'), 'GetValueAsString failed');

    lParams.SetValueAsVariant('param1', '456');
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamInteger, 'Wrong class');
    CheckEquals(456, lParams.GetValueAsInteger('param1'), 'GetValueAsInteger failed');
    CheckEquals('456', lParams.GetValueAsString('param1'), 'GetValueAsString failed');

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsVariant_String;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsVariant('param1', 'test');
    CheckEquals(1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamString, 'Wrong class');
    CheckEquals('test', lParams.GetValueAsString('param1'), 'GetValueAsString failed');
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.ParamByName;
var
  lParams : TtiQueryParams;
  lParam : TtiQueryParamAbs;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString( 'paramS', 'test');
    CheckEquals(1, lParams.Count, 'Count');
    lParams.SetValueAsInteger('paramI', 123);
    CheckEquals(2, lParams.Count, 'Count');
    lParams.SetValueAsFloat(  'paramF', 123.456);
    CheckEquals(3, lParams.Count, 'Count');
    lParams.SetValueAsDateTime('paramD', Date);
    CheckEquals(4, lParams.Count, 'Count');
    lParams.SetValueAsBoolean('paramB', True);
    CheckEquals(5, lParams.Count, 'Count');

    lParam := lParams.FindParamByName('paramS');
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamString, 'Wrong class');

    lParam := lParams.FindParamByName('paramI');
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamInteger, 'Wrong class');

    lParam := lParams.FindParamByName('paramF');
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamFloat, 'Wrong class');

    lParam := lParams.FindParamByName('paramD');
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamDateTime, 'Wrong class');

    lParam := lParams.FindParamByName('paramB');
    CheckNotNull(lParams, 'NotNull failed');
    CheckIs(lParam, TtiQueryParamBoolean, 'Wrong class');

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.ParamIsNull;
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString( 'paramS', 'test');
    lParams.SetValueAsInteger('paramI', 123);
    lParams.SetValueAsFloat(  'paramF', 123.456);
    lParams.SetValueAsDateTime('paramD', Date);
    lParams.SetValueAsBoolean('paramB', True);

    CheckFalse(lParams.ParamIsNull['paramS'], 'String param null');
    CheckFalse(lParams.ParamIsNull['paramI'], 'Integer param null');
    CheckFalse(lParams.ParamIsNull['paramF'], 'Float param null');
    CheckFalse(lParams.ParamIsNull['paramD'], 'Date param null');
    CheckFalse(lParams.ParamIsNull['paramB'], 'Boolean param null');

    lParams.ParamIsNull[ 'paramS' ]:= true;
    lParams.ParamIsNull[ 'paramI' ]:= true;
    lParams.ParamIsNull[ 'paramF' ]:= true;
    lParams.ParamIsNull[ 'paramD' ]:= true;
    lParams.ParamIsNull[ 'paramB' ]:= true;

    Check(lParams.ParamIsNull['paramS'], 'String param not null');
    Check(lParams.ParamIsNull['paramI'], 'Integer param not null');
    Check(lParams.ParamIsNull['paramF'], 'Float param not null');
    Check(lParams.ParamIsNull['paramD'], 'Date param not null');
    Check(lParams.ParamIsNull['paramB'], 'Boolean param not null');

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.TestCheckStreamContentsSame;
var
  lStreamFrom : TStringStream;
  lStreamTo  : TStream;
  lResult    : boolean;
  lMessage   : string;
begin
  lStreamFrom := TStringStream.Create(LongString);
  try
    lStreamTo := TStringStream.Create(LongString);
    try
      lResult := AreStreamContentsSame(lStreamFrom, lStreamTo, lMessage);
      Check(lResult, '#1 Returned FALSE but should have returned TRUE');
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;

  lStreamFrom := TStringStream.Create(LongString);
  try
    lStreamTo := TStringStream.Create(LongString + 'a');
    try
      lResult := AreStreamContentsSame(lStreamFrom, lStreamTo, lMessage);
      Check(not lResult, '#2 Returned TRUE but should have returned FALSE <' + lMessage + '>');
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;

  lStreamFrom := TStringStream.Create(LongString);
  try
    lStreamTo := TStringStream.Create(Copy(LongString, 1, Length(LongString) - 1) + ';');
    try
      lResult := AreStreamContentsSame(lStreamFrom, lStreamTo, lMessage);
      Check(not lResult, '#3 Returned TRUE but should have returned FALSE <' + lMessage + '>');
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;
end;

procedure TTestTIQueryAbs.FieldAsStream;
var
  LStream1 : TStringStream;
  LStream2 : TMemoryStream;
begin
  CreateTableStream(Database);
  LStream1 := TStringStream.Create(tiCreateStringOfSize(1000));
  try
    PopulateTableStream(LStream1);
    Database.StartTransaction;
    try
      FQuery.SelectRow(cTIQueryTableName, nil);
      LStream2 := TMemoryStream.Create;
      try
        FQuery.AssignFieldAsStream(cTIQueryColName, LStream2);
        CheckStreamContentsSame(LStream1, LStream2);
        LStream2.Size := 0;
        FQuery.AssignFieldAsStreamByIndex(cFieldAs_Index, LStream2);
        CheckStreamContentsSame(LStream1, LStream2);
      finally
        LStream2.Free;
      end;
      FQuery.Close;
    finally
      Database.Commit;
    end;
  finally
    LStream1.Free;
  end;
end;

procedure TTestTIQueryAbs.PopulateTableStream(const AValue: TStream);
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsStream(cTIQueryColName, AValue);
    Database.InsertRow(cTIQueryTableName, lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryAbs.InsertDeleteUpdate_Timing;
var
  LParams : TtiQueryParams;
  LInsertTime : DWord;
  LUpdateTime : DWord;
  LDeleteTime : DWord;
  LSingleTestStart     : DWord;
  LCount : integer;
  LBulkTestStart: DWord;
begin
  Check(True); // To Force OnCheckCalled to be called
  LInsertTime := 0;
  LUpdateTime := 0;
  LDeleteTime := 0;
  CreateTableString(Database);

  LParams := TtiQueryParams.Create;
  try
    LParams.SetValueAsString(cTIQueryColName, 'test');

    LCount:= 0;
    LBulkTestStart:= tiGetTickCount;
    while (tiGetTickCount - LBulkTestStart) < (CTimingTestPeriod*1000) do
    begin
      Inc(LCount);

      LSingleTestStart := tiGetTickCount;
      Database.StartTransaction;
      FQuery.InsertRow(cTIQueryTableName, LParams);
      Database.Commit;
      Inc(LInsertTime, tiGetTickCount - LSingleTestStart);
      FQuery.Close;

      LSingleTestStart := tiGetTickCount;
      Database.StartTransaction;
      FQuery.UpdateRow(cTIQueryTableName, LParams, LParams);
      Database.Commit;
      Inc(LUpdateTime, tiGetTickCount - LSingleTestStart);
      FQuery.Close;

      LSingleTestStart := tiGetTickCount;
      Database.StartTransaction;
      FQuery.DeleteRow(cTIQueryTableName, LParams);
      Database.Commit;
      Inc(LDeleteTime, tiGetTickCount - LSingleTestStart);
      FQuery.Close;
    end;

  finally
    LParams.Free;
  end;

  WriteTimingResult('RowTestIterationCount',  TestSetupData.PersistenceLayerName, LCount);
  WriteTimingResult('TotalTestTime', TestSetupData.PersistenceLayerName, CTimingTestPeriod);
  WriteTimingResult('InsertRow',     TestSetupData.PersistenceLayerName, tiSafeDiv(LInsertTime, LCount));
  WriteTimingResult('UpdateRow',     TestSetupData.PersistenceLayerName, tiSafeDiv(LUpdateTime, LCount));
  WriteTimingResult('DeleteRow',     TestSetupData.PersistenceLayerName, tiSafeDiv(LDeleteTime, LCount));
end;

{$IFDEF TESTINT64}
procedure TTestTIQueryAbs.PopulateTableInt64(const AValue: Int64);
var
  lParams : TtiQueryParams;
begin
  lParams := TtiQueryParams.Create;
  try
    FDatabase.DeleteRow(cTIQueryTableNameInt64, nil);
    lParams.SetValueAsInteger(cTIQueryColName, AValue);
    FDatabase.InsertRow(cTIQueryTableNameInt64, lParams);
  finally
    lParams.Free;
  end;
end;
{$ENDIF}

procedure TTestTIQueryAbs.FieldByNameVSFieldByIndex;
const
  cString = 'abcdefghij';
  cCount  = 1000000;
  cImprovement = 100;
var
  lStart : DWord;
  i : Integer;
  lByName : DWOrd;
  lByIndex : DWord;
begin
  CreateTableString(Database);
  PopulateTableString(cString);
  Database.StartTransaction;
  try
    FQuery.SelectRow(cTIQueryTableName, nil);
    lStart := tiGetTickCount;
    for i := 1 to cCount do
      FQuery.FieldAsString[ cTIQueryColName ];
    lByName := tiGetTickCount - lStart;

    lStart := tiGetTickCount;
    for i := 1 to cCount do
      FQuery.FieldAsStringByIndex[ cFieldAs_Index ];
    lByIndex := tiGetTickCount - lStart;

    FQuery.Close;
  finally
    Database.Commit;
  end;
  Check(lByIndex < lByName, 'It got slower. ByIndex: ' +
         IntToStr(lByIndex) + ' ByName: ' + IntToStr(lByName));
end;

procedure TTestTIQueryAbs.FieldAsStringLong1999;
begin
  DoFieldAsStringLong(1999);
end;

procedure TTestTIQueryAbs.FieldAsStringLong2000;
begin
  DoFieldAsStringLong(2000);
end;

procedure TTestTIQueryAbs.FieldAsStringLong2001;
begin
  DoFieldAsStringLong(2001);
end;

procedure TTestTIQueryAbs.FieldAsStringLong3999;
begin
  DoFieldAsStringLong(3999);
end;

procedure TTestTIQueryAbs.FieldAsStringLong4000;
begin
  DoFieldAsStringLong(4000);
end;

procedure TTestTIQueryAbs.FieldAsStringLong4001;
begin
  DoFieldAsStringLong(4001);
end;

procedure TTestTIPersistenceLayers.ConnectDatabase;
var
  LPersistenceLayer: TtiPersistenceLayer;
const
  CDatabaseAlias = 'TestDatabaseAlias';
begin
  // just a safety precaution.
  if not Assigned(TestSetupData) then
    raise Exception.Create('TestSetupData is not assigned')
  else if not Assigned(TestSetupData.PersistenceLayerClass) then
    raise Exception.Create('TestSetupData.PersistenceLayerClass is not assigned');

  LPersistenceLayer:= TestSetupData.PersistenceLayerClass.Create;
  try
    LPersistenceLayer.DBConnectionPools.Connect(
      CDatabaseAlias,
      TestSetupData.DBName,
      TestSetupData.Username,
      TestSetupData.Password,
      TestSetupData.QueryParams);
    try
      CheckNotNull(LPersistenceLayer.DefaultDBConnectionPool, 'DefaultDBConnectionPool');
      CheckEquals(CDatabaseAlias, LPersistenceLayer.DefaultDBConnectionName, 'DefaultDBConnectionName');
      CheckEquals(1, LPersistenceLayer.DBConnectionPools.Count, 'lRegPerLayer.DBConnectionPools.Count');
      CheckEquals(1, LPersistenceLayer.DefaultDBConnectionPool.Count, 'lRegPerLayer.DefaultDBConnectionPool.Count');
    finally
      LPersistenceLayer.DBConnectionPools.Disconnect(CDatabaseAlias);
    end;
    CheckNull(LPersistenceLayer.DefaultDBConnectionPool, 'DefaultDBConnectionPool');
    CheckEquals('', LPersistenceLayer.DefaultDBConnectionName, 'DefaultDBConnectionName');
  finally
    LPersistenceLayer.Free;
  end;
end;

procedure TTestTIPersistenceLayers.CreateTIQuery_LayerName;
var
  LPersistenceLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  lQuery : TtiQuery;
begin
  LPersistenceLayerName := PersistenceLayerName;
  LPersistenceLayer := GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(LPersistenceLayerName);
  try
    CheckNotNull(lQuery, 'Failed creating TtiQuery for <' + LPersistenceLayerName + '>');
    CheckIs(lQuery, LPersistenceLayer.QueryClass, 'Query wrong class');
  finally
    lQuery.Free;
  end;
end;

procedure TTestTIPersistenceLayers.CreateTIQuery_DatabaseClass;
var
  LPersistenceLayerName   : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  LQuery : TtiQuery;
begin
  LPersistenceLayerName := PersistenceLayerName;
  LPersistenceLayer := GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
  LQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(LPersistenceLayer.DatabaseClass);
  try
    CheckNotNull(LQuery, 'Failed creating TtiQuery for <' + LPersistenceLayerName + '>');
    CheckIs(LQuery, LPersistenceLayer.QueryClass, 'Query wrong class');
  finally
    LQuery.Free;
  end;
end;

procedure TTestTIPersistenceLayers.CreateTIDatabase;
var
  LPersistenceLayerName : string;
  LPersistenceLayer : TtiPersistenceLayer ;
  LDatabase    : TtiDatabase;
begin
  LPersistenceLayerName := PersistenceLayerName;
  LPersistenceLayer := GTIOPFManager.PersistenceLayers.FindByPersistenceLayerName(LPersistenceLayerName);
  LDatabase := GTIOPFManager.PersistenceLayers.CreateTIDatabase(LPersistenceLayerName);
  try
    CheckNotNull(LDatabase, 'Failed creating TtiDatabase for <' + LPersistenceLayerName + '>');
    CheckIs(LDatabase, LPersistenceLayer.DatabaseClass, 'Database wrong class');
  finally
    LDatabase.Free;
  end;
end;


end.
