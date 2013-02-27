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

// Uncomment to run performance FieldByNameVSFieldByIndex
//{$DEFINE PERFORMANCE_TESTS}

unit tiQueryAbs_TST;

interface
uses
   tiPersistAbs_TST
  ,tiQuery
  ,SysUtils
  ,Classes
  ,tiDBConnectionPool
  ,tiDBConnectionSetupAbs_TST
  ,TestFramework
  ,tiRegPerLayer
  ;

type

  // Test the meta data objects. No database access required.
  TTestTIMetaData = class( TTestCase )
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

  TTestTIQueryParams = class( TtiPerTestCase )
  protected
    procedure   Setup ; override ;
    procedure   TearDown ; override ;
  published
    procedure TestCheckStreamContentsSame ;
    procedure AsString   ;
    procedure AsInteger  ;
    procedure AsFloat    ;
    procedure AsDateTime ;
    procedure AsBoolean  ;
    procedure AsStream ;
    procedure ParamByName ;
    procedure ParamIsNull ;
  end ;

  //
  TTestTIDBAbs = class( TtiPerTestCase )
  protected
    procedure CheckFieldMetaData( const pDBMetaDataTable : TtiDBMetaDataTable ;
                                   const pFieldName : string ;
                                   pKind : TtiQueryFieldKind ;
                                   pWidth : integer = 0 ) ;
  public
    constructor Create(MethodName: string); override ;
  end ;

  // Test TtiDatabase connectivity
  TTestTIDatabaseAbs = class( TTestTIDBAbs )
  private
  protected
    FRegPerLayer : TtiRegPerLayer;
    FDatabase : TtiDatabase ;
    FDatabaseClass : TtiDatabaseClass ;
    procedure Setup ; override ;
    procedure TearDown ; override ;
    procedure DatabaseExists ; virtual ; abstract ;
    procedure CreateDatabase ; virtual ; abstract ;
  published
    procedure Connect ;
    procedure Transaction_InTransaction ;
    procedure Transaction_Commit ;
    procedure Transaction_RollBack ; virtual ;
    procedure CreateTableDropTable ;
    procedure CreateTableDropTable_Timing ;
  end;

  // Test persistence layer loading and unloading
  TTestTIDatabaseConnectionAbs = class( TTestTIDBAbs )
  protected
    procedure DoThreadedDBConnectionPool( pThreadCount : integer ) ;
  published
    procedure LoadDatabaseLayer ;
    procedure NonThreadedDBConnectionPool ;
    procedure ThreadedDBConnectionPool ; virtual ;
    procedure ReadMetaData ;
  end;

  // Test query access
  TTestTIQueryAbs = class( TtiPerTestCase )
  private
    FRegPerLayer : TtiRegPerLayer ;
    FPooledDB : TPooledDB ;
    FDatabase : TtiDatabase ;
    FQuery    : TtiQuery ;
  protected
    // Some field vars used in the concrete
    property  PooledDB : TPooledDB   read FPooledDB   write FPooledDB ;
    property  Database : TtiDatabase read FDatabase   write FDatabase ;
    property  Query    : TtiQuery    read FQuery      write FQuery ;

    procedure Setup ; override ;
    procedure TearDown ; override ;

    // Helper methods used in the concretes
    procedure DoAttachAndConnect ;
    procedure DoDetachAndDisConnect ;
    procedure DoReAttach;
    procedure PopulateTableString(   const pValue : String    ) ;
    procedure PopulateTableInteger(  const pValue : Integer   ) ;
{$IFDEF TESTINT64}
    procedure PopulateTableInt64(    const pValue : Int64     ) ;
{$ENDIF}
    procedure PopulateTableReal(     const pValue : Real      ) ;
    procedure PopulateTableBoolean(  const pValue : Boolean   ) ;
    procedure PopulateTableDateTime( const pValue : TDateTime ) ;
    procedure PopulateTableStream(   const pValue : TStream ) ;
    procedure DropTestTable ;
    procedure DoFieldAsStringLong(pStrLen: integer);

    // Implement (or hide) these in the concrete
    procedure GetSetSQL ; virtual ; abstract ;
    procedure QueryType ; virtual ; abstract ;
    procedure ParamName ; virtual ; abstract ;
    procedure ParamCount ; virtual ; abstract ;
    procedure ParamsAsString ; virtual ; abstract ;
    procedure ParamAsString ; virtual ; abstract ;
    procedure ParamAsInteger ; virtual ; abstract ;
    procedure ParamAsFloat ; virtual ; abstract ;
    procedure ParamAsBoolean ; virtual ; abstract ;
    procedure ParamAsDateTime ; virtual ; abstract ;
    procedure ParamAsStream ; virtual ; abstract ;
    procedure ParamAsMacro ; virtual ; abstract ;
    procedure ParamIsNull ; virtual ; abstract ;
    procedure OpenCloseActive ; virtual ; abstract ;
    procedure ExecSQL ; virtual ; abstract ;

  public
    constructor Create(MethodName: string); override ;

  published

    procedure ConfirmSetupWorks ; virtual ;
    procedure ConfirmDBConnectionWorks ; virtual ;

    procedure TypeKindToQueryFieldKind;

    // TtiQuery to TtiDatabase connection
    procedure QueryToDatabaseConnection ; // AttachDatabase, DetachDatabase ;

    // Data (field) access
    procedure FieldAsInteger ;
{$IFDEF TESTINT64}
    procedure FieldAsInt64 ;
{$ENDIF}
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
    procedure FieldAsStringLong5000;
    procedure FieldAsStringLong10000;
    procedure FieldAsFloat;
    procedure FieldAsBoolean;
    procedure FieldAsDateTime;
    procedure FieldAsStream;
    procedure FieldIsNull; virtual ;

    {$IFDEF PERFORMANCE_TESTS}
    procedure FieldByNameVSFieldByIndex;
    {$ENDIF}

    // Meta data access methods
    procedure FieldCount ; virtual ;
    procedure FieldName  ; virtual ;
    procedure FieldIndex ; virtual ;
    procedure FieldKind  ; virtual ;
    procedure FieldSize  ; virtual ;

    // Traversing result set
    procedure EOF ; virtual ;
    procedure Next ; virtual ;

    procedure InsertDeleteUpdate_Timing ;

  end ;

  TThrdDBConnectionPoolTest = class( TThread )
  private
    FCycles : integer ;
    FPerFrameworkSetup : TPerFrameworkSetup ;
    FDone : boolean ;
  public
    constructor CreateExt( const pPerFrameworkSetup : TPerFrameworkSetup ;
                           pCycles : integer ) ;
    procedure   Execute ; override ;
    property    Done : boolean read FDone write FDone ;
  end ;

procedure RegisterTests ;

const
  cTIQueryTestName  = 'Dynamically loaded persistence layer' ;
  // Number of threads and iterations for DBPool testing
  // Set a high number for thorough testing (eg, 100)
  // Set a low number for quick testing (eg, 5)
  cuThreadCount     =  5 ;
  cuIterations      =  5 ;

implementation
uses
   tiPersist
  ,Windows
  ,Contnrs
  ,tiLog
  ,tiPtnVisPerObj
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,Forms
  ,cTIPersist
  ,TypInfo
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiDUnitUtils
  ,tiDialogs // for debugging
  ,tiDUnitDependencies
  ,tiXML
  ,tiRJMime
  ;

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
  begin
    RegisterTest( TTestTIMetaData.Suite ) ;
    RegisterTest( TTestTIQueryParams.Suite ) ;
  end;
end ;

procedure TTestTIDatabaseAbs.Connect;
begin
  FDatabase.Connect( PerFrameworkSetup.DBName,
                     PerFrameworkSetup.UserName,
                     PerFrameworkSetup.Password,
                     '' ) ;
  Check( FDatabase.Connected, 'Connect failed' ) ;
  FDatabase.Connected := false ;
  Check( not FDatabase.Connected, 'Connected := false failed' ) ;
end;

procedure TTestTIDatabaseAbs.CreateTableDropTable ;
  procedure _CreateTableDropTable(const pFieldName : string ; pFieldKind : TtiQueryFieldKind ; pFieldWidth : integer ) ;
  var
    lTable : TtiDBMetaDataTable ;
    lDBMetaData : TtiDBMetaData ;
    lDBMetaDataTable : TtiDBMetaDataTable ;
  begin
    lTable := TtiDBMetaDataTable.Create ;
    try
      lTable.Name := cTableNameCreateTable ;
      lTable.AddField( pFieldName, pFieldKind, pFieldWidth ) ;
      FDatabase.CreateTable( lTable ) ;
    finally
      lTable.Free ;
    end ;

    lDBMetaData := TtiDBMetaData.Create ;
    try
      FDatabase.ReadMetaDataTables( lDBMetaData ) ;
      lDBMetaDataTable := lDBMetaData.FindByTableName( cTableNameCreateTable ) ;
      Check( lDBMetaDataTable <> nil, 'Unable to find metadata for <test_create_table> on field  <' + pFieldName + '>' ) ;
      Check( SameText( lDBMetaDataTable.Name, cTableNameCreateTable ), 'Wrong table found when searching for <test_create_table> on field <' + pFieldName + '>' ) ;
      FDatabase.ReadMetaDataFields( lDBMetaDataTable ) ;

      CheckFieldMetaData( lDBMetaDataTable, pFieldName, pFieldKind, pFieldWidth ) ;

      FDatabase.DropTable( cTableNameCreateTable ) ;
      lDBMetaData.Clear ;
      FDatabase.ReadMetaDataTables( lDBMetaData ) ;
      lDBMetaDataTable := lDBMetaData.FindByTableName( cTableNameCreateTable ) ;
      Check( lDBMetaDataTable = nil, 'Drop table <test_create_table> failed on field <' + pFieldName + '>' ) ;
    finally
      lDBMetaData.Free ;
    end ;
  end;
begin
  FDatabase.Connect( PerFrameworkSetup.DBName,
                     PerFrameworkSetup.UserName,
                     PerFrameworkSetup.Password,
                     '' ) ;
  try
    _CreateTableDropTable(  'Str_Field',   qfkString,    10 ) ;
    _CreateTableDropTable(  'Int_Field',   qfkInteger,    0 ) ;
    _CreateTableDropTable(  'Float_Field', qfkFloat,      0 ) ;
    _CreateTableDropTable(  'Date_Field',  qfkDateTime,   0 ) ;
    _CreateTableDropTable(  'Bool_Field' , qfkLogical,    0 ) ;
    _CreateTableDropTable(  'Notes_Field', qfkLongString, 0 ) ;
  finally
    FDatabase.Connected := false ;
  end ;
end;

procedure TTestTIQueryAbs.DoAttachAndConnect;
begin
  FQuery.AttachDatabase( FDatabase ) ;
  FDatabase.StartTransaction ;
end;

procedure TTestTIQueryAbs.DoDetachAndDisConnect;
begin
  if FDatabase.InTransaction then
    FDatabase.Commit ;
  FQuery.DetachDatabase ;
end;

procedure TTestTIQueryAbs.DoReAttach;
begin
  DoDetachAndDisConnect;
  DoAttachAndConnect;
end;


procedure TTestTIQueryAbs.EOF;
begin
  CreateTableTestGroup(Database) ;
  DoAttachAndConnect ;
  try
    FQuery.SelectRow( 'test_group', nil ) ;
    Check( FQuery.EOF, 'FQuery.EOF = true failed.' ) ;
    FQuery.Close ;
    InsertIntoTestGroup( Database, 1 ) ;
    FQuery.SelectRow( 'test_group', nil ) ;
    Check( not FQuery.EOF, 'FQuery.EOF = false failed.' ) ;
    FQuery.Close ;
  finally
    DoDetachAndDisconnect ;
  end ;
end;
                         
procedure TTestTIQueryAbs.FieldAsBoolean;
begin
  CreateTableBoolean(FDatabase) ;
  try
    DoAttachAndConnect ;
    try
      PopulateTableBoolean(True);
      DoReAttach;
      FQuery.SelectRow( cTIQueryTableName, nil ) ;
      CheckEquals( FQuery.FieldAsBoolean[ cTIQueryColName ], True, 'FieldAsBoolean' ) ;
      CheckEquals( FQuery.FieldAsBooleanByIndex[ cFieldAs_Index ], True, 'FieldAsBoolean' ) ;
      FQuery.Close ;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
    DropTestTable ;
  end ;
end;

procedure TTestTIQueryAbs.FieldAsDateTime;
var
  lNow : TDateTime ;
begin
  lNow := Now ;
  CreateTableDateTime(FDatabase) ;
  try
    DoAttachAndConnect ;
    try
      PopulateTableDateTime(lNow);
      DoReAttach;
      FQuery.SelectRow( cTIQueryTableName, nil ) ;
      CheckEquals( FQuery.FieldAsDateTime[ cTIQueryColName ], lNow, cdtOneSecond, 'FieldAsDateTime' ) ;
      CheckEquals( FQuery.FieldAsDateTimeByIndex[ cFieldAs_Index ], lNow, cdtOneSecond, 'FieldAsDateTime' ) ;
      FQuery.Close ;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
    DropTestTable ;
  end ;
end;

procedure TTestTIQueryAbs.FieldAsFloat;
const
  cPrecision = 6 ;
begin
  CreateTableFloat(FDatabase) ;
  try
    DoAttachAndConnect ;
    try
      PopulateTableReal(1234.5678);
      DoReAttach;
      FQuery.SelectRow( cTIQueryTableName, nil ) ;
      CheckEquals( FQuery.FieldAsFloat[ cTIQueryColName ], 1234.5678, 0.00001, 'FieldAsFloat' ) ;
      CheckEquals( FQuery.FieldAsFloatByIndex[ cFieldAs_Index ], 1234.5678, 0.00001, 'FieldAsFloat' ) ;
      FQuery.Close ;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
    DropTestTable ;
  end ;
end;

procedure TTestTIQueryAbs.FieldAsInteger;
begin
  CreateTableInteger(FDatabase) ;
  try
    DoAttachAndConnect ;
    try
      PopulateTableInteger(1);
      DoReAttach;
      FQuery.SelectRow( cTIQueryTableName, nil ) ;
      CheckEquals( FQuery.FieldAsInteger[ cTIQueryColName ], 1, 'FieldAsInteger' ) ;
      CheckEquals( FQuery.FieldAsIntegerByIndex[ cFieldAs_Index ], 1, 'FieldAsIntegerByIndex' ) ;
      FQuery.Close ;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
    DropTestTable ;
  end ;
end;

{$IFDEF TESTINT64}
procedure TTestTIQueryAbs.FieldAsInt64;
begin
//  CreateTableInt64(FDatabase) ;
  try
    DoAttachAndConnect ;
    try
      PopulateTableInt64(1);
      DoReAttach;
      FQuery.SelectRow( cTIQueryTableNameInt64, nil ) ;
      CheckEquals( FQuery.FieldAsInteger[ cTIQueryColName ], 1, 'FieldAsInt64' ) ;
      CheckEquals( FQuery.FieldAsIntegerByIndex[ cFieldAs_Index ], 1, 'FieldAsInt64ByIndex' ) ;
      FQuery.Close ;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
//    DropTestTable ;
  end ;
end;
{$ENDIF}

procedure TTestTIQueryAbs.FieldAsString;
const
  cString = 'abcdefghij' ;
begin
  CreateTableString(FDatabase) ;
  try
    DoAttachAndConnect ;
    try
      PopulateTableString(cString);
      DoReAttach;
      FQuery.SelectRow( cTIQueryTableName, nil ) ;
      CheckEquals( FQuery.FieldAsString[ cTIQueryColName ], cString, 'FieldAsString') ;
      CheckEquals( FQuery.FieldAsStringByIndex[ cFieldAs_Index ], cString, 'FieldAsStringByIndex') ;
      FQuery.Close ;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
    DropTestTable ;
  end ;
end;

procedure TTestTIQueryAbs.FieldCount;
var
  lFieldCount : integer ;
begin
  CreateTableTestGroup(Database);
  DoAttachAndConnect ;
  try
    FDatabase.DeleteRow( 'test_group', nil ) ;
    InsertIntoTestGroup( Database, 1 ) ;
    FQuery.SelectRow( 'test_group', nil ) ;
    lFieldCount := FQuery.FieldCount ;
    CheckEquals( 7, lFieldCount ) ;
    FQuery.Close ;
  finally
    DoDetachAndDisconnect ;
  end ;
end;

procedure TTestTIQueryAbs.FieldIndex;
begin
  CreateTableTestGroup(Database) ;
  DoAttachAndConnect ;
  try
    InsertIntoTestGroup( Database, 1 ) ;
    FQuery.SelectRow( 'test_Group', nil ) ;
    Check( FQuery.FieldIndex( 'oid'               ) = 0, 'FQuery.FieldIndex = 0 failed' ) ;
    Check( FQuery.FieldIndex( 'Group_Str_Field'   ) = 1, 'FQuery.FieldIndex = 1 failed' ) ;
    Check( FQuery.FieldIndex( 'Group_Int_Field'   ) = 2, 'FQuery.FieldIndex = 2 failed' ) ;
    Check( FQuery.FieldIndex( 'Group_Float_Field' ) = 3, 'FQuery.FieldIndex = 3 failed' ) ;
    Check( FQuery.FieldIndex( 'Group_Date_Field'  ) = 4, 'FQuery.FieldIndex = 4 failed' ) ;
    Check( FQuery.FieldIndex( 'Group_Bool_Field'  ) = 5, 'FQuery.FieldIndex = 5 failed' ) ;
    Check( FQuery.FieldIndex( 'Group_Notes_Field' ) = 6, 'FQuery.FieldIndex = 6 failed' ) ;
    FQuery.Close ;
  finally
    DoDetachAndDisconnect ;
  end ;
end;

procedure TTestTIQueryAbs.FieldIsNull;
var
  lParams : TtiQueryParams ;
begin
  CreateTableTestGroup(Database) ;
  DoAttachAndConnect ;
  try
    lParams := TtiQueryParams.Create ;
    try
      lParams.SetValueAsString( 'OID', '1' );
      FDatabase.InsertRow( 'Test_Group', lParams ) ;
      FQuery.SelectRow( 'test_group', nil ) ;
      Check( FQuery.FieldIsNull[ 'group_str_field' ], 'FieldIsNull  = true failed' ) ;
      Check( FQuery.FieldIsNullByIndex[ cFieldAs_Index ], 'FieldIsNull  = true failed' ) ;
      FQuery.Close ;

      FDatabase.DeleteRow( 'test_group', nil ) ;
      lParams.SetValueAsString( 'OID', '2' ) ;
      lParams.SetValueAsString( 'Group_Str_Field', '2') ;
      FDatabase.InsertRow( 'Test_Group', lParams ) ;
      FQuery.SelectRow( 'test_group', nil ) ;
      Check( not FQuery.FieldIsNull[ 'group_str_field' ], 'FieldIsNull = false failed' ) ;
      Check( not FQuery.FieldIsNullByIndex[ cFieldAs_Index ], 'FieldIsNull = false failed' ) ;
      FQuery.Close ;
    finally
      lParams.Free ;
    end ;
  finally
    DoDetachAndDisconnect ;
  end ;
end;

procedure TTestTIQueryAbs.FieldKind;
begin
  CreateTableTestGroup(Database) ;
  DoAttachAndConnect ;
  try
    InsertIntoTestGroup( Database, 1 ) ;
    FQuery.SelectRow( 'test_group', nil ) ;
    Check( FQuery.FieldKind( FQuery.FieldIndex( 'Group_Str_Field'   ))  = qfkString,     'FQuery.FieldKind = qfkString failed' ) ;
    Check( FQuery.FieldKind( FQuery.FieldIndex( 'Group_Int_Field'   ))  = qfkInteger,    'FQuery.FieldKind = qfkInteger failed' ) ;
    Check( FQuery.FieldKind( FQuery.FieldIndex( 'Group_Float_Field' ))  = qfkFloat,      'FQuery.FieldKind = qfkFloat failed' ) ;
    Check( FQuery.FieldKind( FQuery.FieldIndex( 'Group_Date_Field'  ))  = qfkDateTime,   'FQuery.FieldKind = qfkDateTime failed' ) ;
    if FQuery.HasNativeLogicalType then
      Check( FQuery.FieldKind( FQuery.FieldIndex( 'Group_Bool_Field'  ))  = qfkLogical,    'FQuery.FieldKind = qfkLogical failed' ) ;
    Check( FQuery.FieldKind( FQuery.FieldIndex( 'Group_Notes_Field'  )) = qfkLongString, 'FQuery.FieldKind = qfkLongString failed' ) ;
    FQuery.Close ;
  finally
    DoDetachAndDisconnect ;
  end ;
end;

procedure TTestTIQueryAbs.FieldName;
begin
  CreateTableTestGroup(Database) ;
  DoAttachAndConnect ;
  try
    InsertIntoTestGroup( Database, 1 ) ;
    FQuery.SelectRow( 'test_group', nil ) ;
    Check( SameText( FQuery.FieldName( 0 ), 'oid'               ),  'FQuery.FieldName( 0 ) failed' ) ;
    Check( SameText( FQuery.FieldName( 1 ), 'Group_Str_Field'   ),  'FQuery.FieldName( 1 ) failed' ) ;
    Check( SameText( FQuery.FieldName( 2 ), 'Group_Int_Field'   ),  'FQuery.FieldName( 2 ) failed' ) ;
    Check( SameText( FQuery.FieldName( 3 ), 'Group_Float_Field' ),  'FQuery.FieldName( 3 ) failed' ) ;
    Check( SameText( FQuery.FieldName( 4 ), 'Group_Date_Field'  ),  'FQuery.FieldName( 4 ) failed' ) ;
    Check( SameText( FQuery.FieldName( 5 ), 'Group_Bool_Field'  ),  'FQuery.FieldName( 5 ) failed' ) ;
    Check( SameText( FQuery.FieldName( 6 ), 'Group_Notes_Field'  ), 'FQuery.FieldName( 6 ) failed' ) ;
    FQuery.Close ;
  finally
    DoDetachAndDisconnect ;
  end ;
end;

procedure TTestTIQueryAbs.FieldSize;
begin
  CreateTableTestGroup(Database) ;
  DoAttachAndConnect ;
  try
    InsertIntoTestGroup( Database, 1 ) ;
    FQuery.SelectRow( 'Test_Group', nil ) ;
    CheckEquals( 10, FQuery.FieldSize( FQuery.FieldIndex( 'Group_Str_Field'    )), 'Group_Str_Field'   ) ;
    CheckEquals( 0,  FQuery.FieldSize( FQuery.FieldIndex( 'Group_Int_Field'    )), 'Group_Int_Field'   ) ;
    CheckEquals( 0,  FQuery.FieldSize( FQuery.FieldIndex( 'Group_Float_Field'  )), 'Group_Float_Field' ) ;
    CheckEquals( 0,  FQuery.FieldSize( FQuery.FieldIndex( 'Group_Date_Field'   )), 'Group_Date_Field'  ) ;
    CheckEquals( 0,  FQuery.FieldSize( FQuery.FieldIndex( 'Group_Notes_Field'  )), 'Group_Notes_Field' ) ;
    // Nasty, but I can't think of a better solution right now...
    if PerLayerName <> 'DOA' then
      CheckEquals( 0,  FQuery.FieldSize( FQuery.FieldIndex( 'Group_Bool_Field'   )), 'Group_Bool_Field'  )
    else
      CheckEquals( 1,  FQuery.FieldSize( FQuery.FieldIndex( 'Group_Bool_Field'   )), 'Group_Bool_Field'  );

    FQuery.Close ;
// qfkBinary,
// qfkMacro,
// qfkLongString
  finally
    DoDetachAndDisconnect ;
  end ;
end;

procedure TTestTIQueryAbs.Next;
begin
  CreateTableTestGroup(Database) ;
  DoAttachAndConnect ;
  try
    InsertIntoTestGroup( Database, 1 ) ;
    InsertIntoTestGroup( Database, 2 ) ;
    InsertIntoTestGroup( Database, 3 ) ;
    InsertIntoTestGroup( Database, 4 ) ;
    InsertIntoTestGroup( Database, 5 ) ;
    FQuery.SelectRow( 'test_group', nil ) ;
    Check( not FQuery.EOF, '0 FQuery.EOF = true failed.' ) ;
    FQuery.Next ;
    Check( not FQuery.EOF, '1 FQuery.EOF = true failed.' ) ;
    FQuery.Next ;
    Check( not FQuery.EOF, '2 FQuery.EOF = true failed.' ) ;
    FQuery.Next ;
    Check( not FQuery.EOF, '3 FQuery.EOF = true failed.' ) ;
    FQuery.Next ;
    Check( not FQuery.EOF, '4 FQuery.EOF = true failed.' ) ;
    FQuery.Next ;
    Check( FQuery.EOF, '5 FQuery.EOF = true failed.' ) ;
    FQuery.Close ;
  finally
    DoDetachAndDisconnect ;
  end ;
end;

procedure TTestTIQueryAbs.QueryToDatabaseConnection;
begin
  FQuery.AttachDatabase( FDatabase ) ;
  Check( FQuery.Database <> nil, 'FQuery.AttachDatabase failed' ) ;
  FQuery.DetachDatabase;
  Check( FQuery.Database = nil, 'FQuery.DetachDatabase failed' ) ;
end;

procedure TTestTIDatabaseConnectionAbs.ReadMetaData;
var
  lDBMetaData : TtiDBMetaData ;
  lDBMetaDataTable : TtiDBMetaDataTable ;
  lPooledDB : TPooledDB ;
  lDatabase : TtiDatabase ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  gTIPerMgr.LoadDatabaseLayer(
                     PerFrameworkSetup.PerLayerName,
                     PerFrameworkSetup.DBName,
                     PerFrameworkSetup.UserName,
                     PerFrameworkSetup.Password ) ;
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerFrameworkSetup.PerLayerName);
  CheckNotNull(lRegPerLayer, 'Unable to find RegPerLayer' ) ;
    try
    SetupTestTables ;
    try
      lDBMetaData := TtiDBMetaData.Create ;
      try
        lPooledDB :=lRegPerLayer.DBConnectionPools.Lock( PerFrameworkSetup.DBName );
        try
          lDatabase := lPooledDB.Database ;
          lDatabase.ReadMetaDataTables( lDBMetaData ) ;
          lDBMetaDataTable := lDBMetaData.FindByTableName( 'test_item' ) ;
          Check( lDBMetaDataTable <> nil, 'Unable to find metadata for test_item' ) ;
          Check( SameText( lDBMetaDataTable.Name, 'Test_Item' ), 'Wrong table found when searching for <test_item>' ) ;
          lDatabase.ReadMetaDataFields( lDBMetaDataTable ) ;

          // So, we will just search for the field.
          // Currently, there is no field information like type or size returned.
          // This should be added.
          CheckFieldMetaData( lDBMetaDataTable, 'OID',              qfkInteger ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'OID_GROUP',        qfkInteger ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'ITEM_INT_FIELD',   qfkInteger ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'ITEM_FLOAT_FIELD', qfkFloat ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'ITEM_STR_FIELD',   qfkString, 10 ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'ITEM_Bool_FIELD',  qfkLogical ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'ITEM_Date_FIELD',  qfkDateTime ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'ITEM_Notes_FIELD', qfkLongString ) ;

          lDBMetaDataTable := lDBMetaData.FindByTableName( 'test_group' ) ;
          Check( lDBMetaDataTable <> nil, 'Unable to find metadata for test_group' ) ;
          Check( SameText( lDBMetaDataTable.Name, 'Test_Group' ), 'Wrong table found when searching for <test_group>' ) ;
          lDatabase.ReadMetaDataFields( lDBMetaDataTable ) ;

          CheckFieldMetaData( lDBMetaDataTable, 'OID',               qfkInteger ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'GROUP_INT_FIELD',   qfkInteger ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'GROUP_FLOAT_FIELD', qfkFloat   ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'GROUP_STR_FIELD',   qfkString, 10 ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'GROUP_Bool_FIELD',  qfkLogical ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'GROUP_Date_FIELD',  qfkDateTime ) ;
          CheckFieldMetaData( lDBMetaDataTable, 'GROUP_Notes_FIELD', qfkLongString ) ;
        finally
          lRegPerLayer.DBConnectionPools.UnLock( PerFrameworkSetup.DBName, lPooledDB ) ;
        end ;
      finally
        lDBMetaData.Free ;
      end ;
    finally
      DeleteTestTables ;
    end ;
  finally
    gTIPerMgr.UnLoadDatabaseLayer( PerFrameworkSetup.PerLayerName,
                                   PerFrameworkSetup.DBName );
  end;
end;

procedure TTestTIQueryAbs.Setup;
begin
  inherited;
  FRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerFrameworkSetup.PerLayerName);
  CheckNotNull(FRegPerLayer, 'Unable to find RegPerLayer <' + PerFrameworkSetup.PerLayerName);
  FPooledDB := FRegPerLayer.DBConnectionPools.Lock( PerFrameworkSetup.DBName );
  FDatabase := FPooledDB.Database ;
  Assert( not FDatabase.InTransaction, 'Database in transaction after <gTIPerMgr.DefaultPerLayer.DBConnectionPools.Lock> called' ) ;
  DropTestTable ;
  DropTableTestGroup(FDatabase) ;
  FQuery    := FRegPerLayer.tiQueryClass.Create ;
end;

procedure TTestTIQueryAbs.TearDown;
begin
  DropTestTable ;
  CheckNotNull(FRegPerLayer, 'Unable to find RegPerLayer <' + PerFrameworkSetup.PerLayerName);
  FRegPerLayer.DBConnectionPools.UnLock( PerFrameworkSetup.DBName, FPooledDB ) ;
  FQuery.Free ;
  inherited;
end;

procedure TTestTIDatabaseAbs.Transaction_InTransaction;
begin
  FDatabase.Connect( PerFrameworkSetup.DBName,
                     PerFrameworkSetup.UserName,
                     PerFrameworkSetup.Password,
                     '' ) ;
  FDatabase.StartTransaction ;
  Check( FDatabase.InTransaction, 'Database not in a transaction' ) ;
  FDatabase.Commit ;
  Check( not FDatabase.InTransaction, 'Database in a transaction when it should not be' ) ;
  FDatabase.StartTransaction ;
  Check( FDatabase.InTransaction, 'Database not in a transaction' ) ;
  FDatabase.RollBack ;
  Check( not FDatabase.InTransaction, 'Database in a transaction when it should not be' ) ;
  FDatabase.Connected := false ;
end;

procedure TTestTIDBAbs.CheckFieldMetaData(
  const pDBMetaDataTable: TtiDBMetaDataTable; const pFieldName: string;
  pKind: TtiQueryFieldKind; pWidth: integer);
var
  lField : TtiDBMetaDataField ;
begin
  lField := pDBMetaDataTable.FindByFieldName( pFieldName ) ;
  Check( lField <> nil,   'Field <' + pFieldName + '> not found' ) ;
// ToDo: The meta data system wont read in field kind or size yet. Requires work.
//      Check( lField.Kind = pKind, 'Field <' + pFieldName + '> field kind of wrong type' ) ;
//      if pWidth <> 0 then
//        Check( lField.Width = pWidth, 'Field <' + pFieldName + '> field width of wrong size' ) ;
end;

procedure TTestTIQueryAbs.TypeKindToQueryFieldKind;
begin
  Check( tiTypeKindToQueryFieldKind(tiTKInteger ) = qfkInteger,  'Failed on tiTKInteger ' ) ;
  Check( tiTypeKindToQueryFieldKind(tiTKFloat   ) = qfkFloat   , 'Failed on tiTKFloat   ' ) ;
  Check( tiTypeKindToQueryFieldKind(tiTKString  ) = qfkString  , 'Failed on tiTKString  ' ) ;
  Check( tiTypeKindToQueryFieldKind(tiTKDateTime) = qfkDateTime, 'Failed on tiTKDateTime' ) ;
  Check( tiTypeKindToQueryFieldKind(tiTKBoolean ) = qfkLogical , 'Failed on tiTKBoolean ' ) ;
end;

procedure TTestTIDatabaseConnectionAbs.DoThreadedDBConnectionPool(pThreadCount: integer);
  procedure _CreateThreads( pList : TList ;
                            pThreadCount, pIterations : integer ) ;
  var
    i : integer ;
  begin
    for i := 1 to pThreadCount do
    begin
      LogArray([ 'Creating thread', GetCurrentThreadID ]);
      pList.Add( TThrdDBConnectionPoolTest.CreateExt(
                   PerFrameworkSetup,
                   pIterations )) ;
    end ;
  end ;

  procedure _StartThreads( pList : TList ) ;
  var
    i : integer ;
  begin
    for i := 0 to pList.Count - 1 do
    begin
      LogArray([ 'Starting thread', GetCurrentThreadID ]);
      TThread( pList.Items[i] ).Resume ;
    end ;
  end ;

  procedure _WaitForThreads( pList : TList ) ;
  var
    i : integer ;
    lAllFinished : boolean ;
  begin
    lAllFinished := false ;
    while not lAllFinished do
    begin
      LogArray([ 'Waiting for threads' ]);
      lAllFinished := true ;
      for i := 0 to pList.Count - 1 do
        lAllFinished := lAllFinished and TThrdDBConnectionPoolTest( pList.Items[i] ).Done ;
      Sleep( 100 ) ;
      Application.ProcessMessages ;
    end ;
  end ;
var
  lList : TObjectList ;
begin
  gTIPerMgr.LoadDatabaseLayer(
    PerFrameworkSetup.PerLayerName,
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password ) ;
  try
    lList := TObjectList.Create ;
    try
      _CreateThreads( lList, pThreadCount, cuIterations ) ;
      _StartThreads( lList ) ;
      _WaitForThreads( lList ) ;
    finally
      lList.Free ;
    end;
  finally
    gTIPerMgr.UnLoadDatabaseLayer(
      PerFrameworkSetup.PerLayerName,
      PerFrameworkSetup.DBName ) ;
  end;
end;

procedure TTestTIDatabaseConnectionAbs.NonThreadedDBConnectionPool;
var
  i : integer ;
  lConnection : TPooledDB ;
  lDBConnectionName : string ;
  lRegPerLayer : TtiRegPerLayer ;
begin
  gTIPerMgr.LoadDatabaseLayer(
    PerFrameworkSetup.PerLayerName,
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password ) ;
  try
    lDBConnectionName := PerFrameworkSetup.DBName ;
    for i := 1 to 10 do
    begin
      lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerFrameworkSetup.PerLayerName);
      CheckNotNull(lRegPerLayer, 'Unable to find RegPerLayer <' + PerFrameworkSetup.PerLayerName);
      lConnection := lRegPerLayer.DBConnectionPools.Lock( lDBConnectionName ) ;
      Sleep( 100 );
      lRegPerLayer.DBConnectionPools.UnLock( lDBConnectionName, lConnection ) ;
      Sleep( 100 );
      LogFmt( 'Thread: %d, Cylce: %d',
              [GetCurrentThreadID, i]);
    end ;
  finally
    gTIPerMgr.UnLoadDatabaseLayer(
      PerFrameworkSetup.PerLayerName,
      PerFrameworkSetup.DBName ) ;
  end;
end;

procedure TTestTIDatabaseConnectionAbs.ThreadedDBConnectionPool;
begin
  DoThreadedDBConnectionPool( cuThreadCount ) ;
end;

constructor TThrdDBConnectionPoolTest.CreateExt( const pPerFrameworkSetup : TPerFrameworkSetup ;
                                                 pCycles : integer ) ;
begin
  Create( true ) ;
  FreeOnTerminate := false ;
  FCycles := pCycles ;
  FPerFrameworkSetup := pPerFrameworkSetup ;
  FDone := false ;
end;

procedure TThrdDBConnectionPoolTest.Execute;
var
  i : integer ;
  lRegPerLayer : TtiRegPerLayer ;
  lConnection : TPooledDB ;
begin
  try
    for i := 1 to FCycles do
    begin
      lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(FPerFrameworkSetup.PerLayerName ) ;
      Assert(lRegPerLayer <> nil, 'Unable to find RegPerLayer <' + FPerFrameworkSetup.PerLayerName ) ;
      lConnection := lRegPerLayer.DBConnectionPools.Lock( FPerFrameworkSetup.DBName ) ;
      Sleep( 100 ) ;
      lRegPerLayer.DBConnectionPools.UnLock( FPerFrameworkSetup.DBName, lConnection ) ;
      LogFmt( 'Thread: %d, Cylce: %d',
              [GetCurrentThreadID, i]);
    end ;
    FDone := true ;
  except
    on e:exception do
      LogError( e.message ) ;
  end
end;

constructor TTestTIQueryAbs.Create(MethodName: string);
begin
  inherited;
  SetupTasks := [sutPerLayer, sutDBConnection ];
end;

{ TTestTIDatabaseAbs }

constructor TTestTIDBAbs.Create(MethodName: string);
begin
  inherited;
  SetupTasks := [sutPerLayer];
end;

procedure TTestTIDatabaseAbs.Setup;
begin
  inherited;
  FRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerFrameworkSetup.PerLayerName);
  CheckNotNull(FRegPerLayer, 'Unable to find RegPerLayer <' + PerFrameworkSetup.PerLayerName);
  FDatabaseClass := FRegPerLayer.tiDatabaseClass;
  FDatabase := FDatabaseClass.Create ;
end;

procedure TTestTIDatabaseAbs.TearDown;
begin
  if FDatabase.Connected then
    FDatabase.Connected := false ;
  FreeAndNil(FDatabase);
  FDatabaseClass := nil ;
  inherited;
end;

procedure TTestTIDatabaseConnectionAbs.LoadDatabaseLayer;
var
  lRegPerLayer : TtiRegPerLayer ;
begin
  gTIPerMgr.LoadDatabaseLayer(
    PerFrameworkSetup.PerLayerName,
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password ) ;
  try
    lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerFrameworkSetup.PerLayerName);
    CheckNotNull( lRegPerLayer, 'Unable to find RegPerLayer' ) ;
    CheckEquals(    PerFrameworkSetup.PerLayerName, lRegPerLayer.PerLayerName, 'PerLayerName' ) ;
    CheckNotNull( lRegPerLayer.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
    CheckEquals( PerFrameworkSetup.DBName, lRegPerLayer.DefaultDBConnectionPool.DBConnectParams.DatabaseName, 'DatabaseName' ) ;
  finally
    gTIPerMgr.UnLoadDatabaseLayer(
      PerFrameworkSetup.PerLayerName,
      PerFrameworkSetup.DBName ) ;
  end;
  CheckNull( lRegPerLayer.DefaultDBConnectionPool, 'DefaultDBConnectionPool' ) ;
end;

procedure TTestTIDatabaseAbs.Transaction_Commit;
var
  lQuery : TtiQuery ;
begin
  FDatabase.Connect( PerFrameworkSetup.DBName,
                     PerFrameworkSetup.UserName,
                     PerFrameworkSetup.Password,
                     '' ) ;
  try FDatabase.DropTable(cTableNameTestGroup) except end ;

  CreateTableTestGroup(FDatabase);

  FDatabase.StartTransaction ;
  try
    InsertIntoTestGroup(FDatabase, 1);
    FDatabase.Commit ;
    lQuery := FRegPerLayer.tiQueryClass.Create;
    try
      lQuery.AttachDatabase(FDatabase);
      FDatabase.StartTransaction ;
      lQuery.SelectRow(cTableNameTestGroup, nil);
      Check( not lQuery.EOF, 'Transaction not committed' ) ;
      lQuery.Next ;
      Check( lQuery.EOF, 'Wrong number of records' ) ;
      FDatabase.Commit ;
    finally
      lQuery.Free;
    end;
  finally
    FDatabase.DropTable(cTableNameTestGroup);
  end;
end;

procedure TTestTIDatabaseAbs.Transaction_RollBack;
var
  lQuery : TtiQuery ;
  lEOF : boolean ;
begin
  FDatabase.Connect( PerFrameworkSetup.DBName,
                     PerFrameworkSetup.UserName,
                     PerFrameworkSetup.Password,
                     '' ) ;
  try FDatabase.DropTable(cTableNameTestGroup) except end ;
  CreateTableTestGroup(FDatabase);

  FDatabase.StartTransaction ;
  try
    InsertIntoTestGroup(FDatabase, 1);
    FDatabase.RollBack ;
    lQuery := FRegPerLayer.tiQueryClass.Create;
    try
      lQuery.AttachDatabase(FDatabase);
      FDatabase.StartTransaction ;
      lQuery.SelectRow(cTableNameTestGroup, nil);
      lEOF := lQuery.EOF ;
      Check( lEOF, 'Transaction not rolled back' ) ;
      FDatabase.Commit ;
    finally
      lQuery.Free;
    end;
  finally
    FDatabase.DropTable(cTableNameTestGroup);
  end;
end;

procedure TTestTIDatabaseAbs.CreateTableDropTable_Timing;
var
  lTable1          : TtiDBMetaDataTable ;
  lTable2          : TtiDBMetaDataTable ;
  lCreateTableTime : DWord ;
  lDropTableTime   : DWord ;
  lMetaDataTime    : DWord ;
  lStart           : DWord ;
  i                : integer ;
begin
  lCreateTableTime := 0 ;
  lDropTableTime   := 0 ;
  lMetaDataTime    := 0 ;
  FDatabase.Connect( PerFrameworkSetup.DBName,
                     PerFrameworkSetup.UserName,
                     PerFrameworkSetup.Password,
                     '' ) ;
  try
    lTable1 := TtiDBMetaDataTable.Create ;
    try
      lTable1.Name := cTableNameCreateTable ;
      lTable2 := TtiDBMetaDataTable.Create ;
      try
        lTable2.Name := cTableNameCreateTable ;
        lTable1.AddField( 'test', qfkString, 10 ) ;
        for i := 1 to cTimingRepeatCount do
        begin
          // Create Table
          lStart := GetTickCount ;
          FDatabase.CreateTable( lTable1 ) ;
          Inc( lCreateTableTime, GetTickCount - lStart ) ;

          // Meta Data
          lTable2.Clear ;
          lStart := GetTickCount ;
          FDatabase.ReadMetaDataFields(lTable2);
          Inc( lMetaDataTime, GetTickCount - lStart ) ;

          // Drop Table
          lStart := GetTickCount ;
          FDatabase.DropTable(cTableNameCreateTable);
          Inc( lDropTableTime, GetTickCount - lStart ) ;

        end;
        WriteTimingResult('CreateTable',    PerFrameworkSetup.PerLayerName, lCreateTableTime / cTimingRepeatCount);
        WriteTimingResult('DropTableTable', PerFrameworkSetup.PerLayerName, lDropTableTime / cTimingRepeatCount);
        WriteTimingResult('ReadMetaData',   PerFrameworkSetup.PerLayerName, lMetaDataTime / cTimingRepeatCount);
      finally
        lTable2.Free;
      end;
    finally
      lTable1.Free ;
    end ;

  finally
    FDatabase.Connected := false ;
  end ;
end;

{ TTestTIMetaData }

procedure TTestTIMetaData.DBMetaDataAdd;
var
  lList : TtiDBMetaData ;
  lData : TtiDBMetaDataTable ;
begin
  lList := TtiDBMetaData.Create ;
  try
    CheckEquals( 0, lList.Count, 'Count' ) ;
    lData := TtiDBMetaDataTable.Create ;
    lList.Add( lData ) ;
    CheckEquals( 1, lList.Count, 'Count' ) ;
    CheckSame( lData, lList.Items[0] ) ;
    lList.Clear ;
    CheckEquals( 0, lList.Count, 'Count' ) ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestTIMetaData.DBMetaDataFindByTableName;
var
  lList : TtiDBMetaData ;
  lData : TtiDBMetaDataTable ;
begin
  lList := TtiDBMetaData.Create ;
  try
    lData := TtiDBMetaDataTable.Create ;
    lData.Name := 'test1' ;
    lList.Add( lData ) ;
    lData := TtiDBMetaDataTable.Create ;
    lData.Name := 'test2' ;
    lList.Add( lData ) ;
    lData := TtiDBMetaDataTable.Create ;
    lData.Name := 'test3' ;
    lList.Add( lData ) ;

    lData := lList.FindByTableName('test2');
    CheckNotNull(lData);
    CheckEquals('test2', lData.Name ) ;

    lData := lList.FindByTableName('TEST3');
    CheckNotNull(lData);
    CheckEquals('test3', lData.Name ) ;

  finally
    lList.Free ;
  end ;
end;

procedure TTestTIMetaData.MetaDataFieldClone;
var
  lField1 : TtiDBMetaDataField ;
  lField2 : TtiDBMetaDataField ;
begin
  lField1 := TtiDBMetaDataField.Create ;
  try
    lField1.Name := 'test';
    lField1.Width := 10 ;
    lField1.Kind  := qfkString ;
    lField2 := lField1.Clone ;
    try
      CheckEquals( lField1.Name, lField2.Name, 'lTable2.Name' ) ;
      CheckEquals( lField1.Width, lField2.Width, 'lTable2.Width' ) ;
      CheckEquals( lField1.KindAsStr, lField2.KindAsStr, 'lTable2.KindAsStr' ) ;
    finally
      lField2.Free ;
    end;
  finally
    lField1.Free ;
  end ;
end;

procedure TTestTIMetaData.MetaDataTableAdd;
var
  lList : TtiDBMetaDataTable ;
  lData : TtiDBMetaDataField ;
begin
  lList := TtiDBMetaDataTable.Create ;
  try
    CheckEquals( 0, lList.Count, 'Count' ) ;
    lData := TtiDBMetaDataField.Create ;
    lList.Add( lData ) ;
    CheckEquals( 1, lList.Count, 'Count' ) ;
    CheckSame( lData, lList.Items[0] ) ;
    lList.Clear ;
    CheckEquals( 0, lList.Count, 'Count' ) ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestTIMetaData.MetaDataTableAddField;
var
  lList : TtiDBMetaDataTable ;
begin
  lList := TtiDBMetaDataTable.Create ;
  try
    CheckEquals( 0, lList.Count, 'Count' ) ;
    lList.AddField( 'test1', qfkString, 10 ) ;
    CheckEquals( 1, lList.Count, 'Count' ) ;
    lList.AddField( 'test2', qfkInteger ) ;
    CheckEquals( 2, lList.Count, 'Count' ) ;
    lList.AddField( 'test3', qfkFloat ) ;
    CheckEquals( 3, lList.Count, 'Count' ) ;
    lList.AddField( 'test4', qfkDateTime ) ;
    CheckEquals( 4, lList.Count, 'Count' ) ;
    lList.AddField( 'test5', qfkLogical ) ;
    CheckEquals( 5, lList.Count, 'Count' ) ;
    lList.Clear ;
    CheckEquals( 0, lList.Count, 'Count' ) ;
  finally
    lList.Free ;
  end ;
end;

procedure TTestTIMetaData.MetaDataTableClone;
var
  lTable1 : TtiDBMetaDataTable ;
  lTable2 : TtiDBMetaDataTable ;
  lData : TtiDBMetaDataField ;
begin
  lTable1 := TtiDBMetaDataTable.Create ;
  try
    lTable1.Name := 'test' ;
    lData := TtiDBMetaDataField.Create ;
    lData.Name := 'test';
    lData.Width := 10 ;
    lData.Kind  := qfkString ;
    lTable1.Add( lData ) ;

    lTable2 := lTable1.Clone ;
    try
      CheckEquals( 'test', lTable2.Name, 'Name' ) ;
      CheckEquals( lTable1.Count, lTable2.Count, 'Count' ) ;
      CheckEquals( lTable1.Items[0].Name, lTable2.Items[0].Name, 'lTable2.Items[0].Name' ) ;
      CheckEquals( lTable1.Items[0].Width, lTable2.Items[0].Width, 'lTable2.Items[0].Width' ) ;
      CheckEquals( lTable1.Items[0].KindAsStr, lTable2.Items[0].KindAsStr, 'lTable2.Items[0].KindAsStr' ) ;
    finally
      lTable2.Free ;
    end;

  finally
    lTable1.Free ;
  end ;
end;

procedure TTestTIMetaData.MetaDataTableFindByFieldName;
var
  lList : TtiDBMetaDataTable ;
  lData : TtiDBMetaDataField ;
begin
  lList := TtiDBMetaDataTable.Create ;
  try
    lData := TtiDBMetaDataField.Create ;
    lData.Name := 'test1' ;
    lList.Add( lData ) ;
    lData := TtiDBMetaDataField.Create ;
    lData.Name := 'test2' ;
    lList.Add( lData ) ;
    lData := TtiDBMetaDataField.Create ;
    lData.Name := 'test3' ;
    lList.Add( lData ) ;

    lData := lList.FindByFieldName('test2');
    CheckNotNull(lData);
    CheckEquals('test2', lData.Name ) ;

    lData := lList.FindByFieldName('TEST3');
    CheckNotNull(lData);
    CheckEquals('test3', lData.Name ) ;

  finally
    lList.Free ;
  end ;
end;

procedure TTestTIMetaData.MetaDataTableMaxFieldNameWidth;
var
  lList : TtiDBMetaDataTable ;
  lData : TtiDBMetaDataField ;
begin
  lList := TtiDBMetaDataTable.Create ;
  try
    lData := TtiDBMetaDataField.Create ;
    lData.Name := 'a' ;
    lList.Add( lData ) ;
    CheckEquals( 1, lList.MaxFieldNameWidth ) ;
    lData := TtiDBMetaDataField.Create ;
    lData.Name := 'ab' ;
    lList.Add( lData ) ;
    CheckEquals( 2, lList.MaxFieldNameWidth ) ;
    lData := TtiDBMetaDataField.Create ;
    lData.Name := 'abc' ;
    lList.Add( lData ) ;
    CheckEquals( 3, lList.MaxFieldNameWidth ) ;
    lData := TtiDBMetaDataField.Create ;
    lData.Name := 'def' ;
    lList.Add( lData ) ;
    CheckEquals( 3, lList.MaxFieldNameWidth ) ;

  finally
    lList.Free ;
  end ;
end;

procedure TTestTIQueryAbs.ConfirmSetupWorks;
begin
  CheckNotNull(FRegPerLayer, 'RegPerlayerNotAssigned');
  CheckEquals(PerFrameworkSetup.PerLayerName, FRegPerLayer.PerLayerName, 'Wrong RegPerLayer');
  Check( not FDatabase.InTransaction, 'Database InTransaction when it should not be' ) ;
end;

procedure TTestTIQueryAbs.ConfirmDBConnectionWorks;
begin
  DoAttachAndConnect ;
  try
    Check( FDatabase.InTransaction, 'Database not InTransaction when it should be' ) ;
    // Do nothing ;
  finally
    DoDetachAndDisconnect ;
  end ;
end;

procedure TTestTIQueryAbs.PopulateTableString( const pValue : String ) ;
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsString(cTIQueryColName, pValue) ;
    FDatabase.InsertRow( cTIQueryTableName, lParams ) ;
  finally
    lParams.Free ;
  end ;
end;

procedure TTestTIQueryAbs.FieldAsStringLong1;
begin
  DoFieldAsStringLong(     1 ) ;
end;

procedure TTestTIQueryAbs.FieldAsStringLong10;
begin
  DoFieldAsStringLong(    10 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong100;
begin
  DoFieldAsStringLong(   100 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong255;
begin
  DoFieldAsStringLong(   255 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong256;
begin
  DoFieldAsStringLong(   256 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong257;
begin
  DoFieldAsStringLong(   257 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong511;
begin
  DoFieldAsStringLong(   511 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong512;
begin
  DoFieldAsStringLong(   512 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong513;
begin
  DoFieldAsStringLong(   513 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong1023;
begin
  DoFieldAsStringLong(  1023 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong1024;
begin
  DoFieldAsStringLong(  1024 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong1025;
begin
  DoFieldAsStringLong(  1025 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong5000;
begin
  DoFieldAsStringLong(  5000 ) ;
end ;

procedure TTestTIQueryAbs.FieldAsStringLong10000;
begin
  DoFieldAsStringLong( 10000 ) ;
end ;

procedure TTestTIQueryAbs.DoFieldAsStringLong(pStrLen : integer);
var
  lValue : string ;
  lTarget : string ;
begin
  lTarget := tiCreateStringOfSize(pStrLen);
  CreateTableLongString(Database) ;
  try
    DoAttachAndConnect ;
    try
      PopulateTableString(lTarget);
      DoReAttach;
      FQuery.SelectRow( cTIQueryTableName, nil ) ;
      lValue := FQuery.FieldAsString[ cTIQueryColName ] ;
      CheckEquals( lTarget, lValue, 'FieldAsString: ' + IntToStr( pStrLen )) ;

      lValue := FQuery.FieldAsStringByIndex[ cFieldAs_Index ] ;
      CheckEquals( lTarget, lValue, 'FieldAsStringByIndex: ' + IntToStr( pStrLen )) ;

      FQuery.Close ;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
    DropTestTable ;
  end ;
end;

procedure TTestTIQueryAbs.DropTestTable;
begin
  Assert(FDatabase <> nil, 'FDatabase not assigned' ) ;
  try FDatabase.DropTable( cTIQueryTableName ) except end ;
end;

procedure TTestTIQueryAbs.PopulateTableReal(const pValue: Real);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsFloat(cTIQueryColName, pValue) ;
    FDatabase.InsertRow( cTIQueryTableName, lParams ) ;
  finally
    lParams.Free ;
  end ;
end;

procedure TTestTIQueryAbs.PopulateTableInteger(const pValue: Integer);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsInteger(cTIQueryColName, pValue) ;
    FDatabase.InsertRow( cTIQueryTableName, lParams ) ;
  finally
    lParams.Free ;
  end ;
end;

procedure TTestTIQueryAbs.PopulateTableDateTime(const pValue: TDateTime);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsDateTime(cTIQueryColName, pValue) ;
    FDatabase.InsertRow( cTIQueryTableName, lParams ) ;
  finally
    lParams.Free ;
  end ;
end;

procedure TTestTIQueryAbs.PopulateTableBoolean(const pValue: Boolean);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsBoolean(cTIQueryColName, pValue) ;
    FDatabase.InsertRow( cTIQueryTableName, lParams ) ;
  finally
    lParams.Free ;
  end ;
end;

{ TTestTIQueryParams }

procedure TTestTIQueryParams.AsBoolean;
var
  lParams : TtiQueryParams ;
  lParam : TtiQueryParamAbs ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsBoolean('param1', True);
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamBoolean, 'Wrong class' ) ;
    CheckEquals( True, lParams.GetValueAsBoolean('param1'), 'GetValueAsBoolean failed') ;
    CheckEquals( 'TRUE', lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;

    lParams.SetValueAsString('param1', 'False');
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamBoolean, 'Wrong class' ) ;
    CheckEquals( False, lParams.GetValueAsBoolean('param1'), 'GetValueAsBoolean failed') ;
    CheckEquals( 'FALSE', lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsDateTime;
var
  lParams : TtiQueryParams ;
  lParam : TtiQueryParamAbs ;
  lDate : TDateTime ;
begin
  lDate := EncodeDate( 2004, 01, 01 ) ;
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsDateTime('param1', lDate);
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamDateTime, 'Wrong class' ) ;
    CheckEquals( lDate, lParams.GetValueAsDateTime('param1'), 0.0001, 'GetValueAsDateTime failed') ;
    CheckEquals( tiDateTimeAsXMLString(lDate), lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;

    lDate := EncodeDate( 2004, 02, 02 ) ;
    lParams.SetValueAsString('param1', tiDateTimeAsXMLString(lDate));
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamDateTime, 'Wrong class' ) ;
    CheckEquals( lDate, lParams.GetValueAsDateTime('param1'), 0.0001, 'GetValueAsDateTime failed') ;
    CheckEquals( tiDateTimeAsXMLString(lDate), lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsFloat;
var
  lParams : TtiQueryParams ;
  lParam : TtiQueryParamAbs ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsFloat('param1', 123.456);
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamFloat, 'Wrong class' ) ;
    CheckEquals( 123.456, lParams.GetValueAsFloat('param1'), 0.0001, 'GetValueAsFloat failed') ;
    CheckEquals( '123.456', lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;

    lParams.SetValueAsString('param1', '456.789');
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamFloat, 'Wrong class' ) ;
    CheckEquals( 456.789, lParams.GetValueAsFloat('param1'), 0.0001,  'GetValueAsFloat failed') ;
    CheckEquals( '456.789', lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsInteger;
var
  lParams : TtiQueryParams ;
  lParam : TtiQueryParamAbs ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsInteger('param1', 123);
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamInteger, 'Wrong class' ) ;
    CheckEquals( 123, lParams.GetValueAsInteger('param1'), 'GetValueAsInteger failed') ;
    CheckEquals( '123', lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;

    lParams.SetValueAsString('param1', '456');
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamInteger, 'Wrong class' ) ;
    CheckEquals( 456, lParams.GetValueAsInteger('param1'), 'GetValueAsInteger failed') ;
    CheckEquals( '456', lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.AsStream;
var
  lParams  : TtiQueryParams ;
  lParam   : TtiQueryParamAbs ;
  lStream1 : TStringStream ;
  lStream2 : TStream ;
  ls       : string ;
begin
  lStream1 := TStringStream.Create(LongString) ;
  try
      lParams := TtiQueryParams.Create ;
      try
        lParams.SetValueAsStream('param1', lStream1);
        CheckEquals( 1, lParams.Count, 'Count');
        lParam := lParams.Items[0];
        CheckNotNull(lParams, 'NotNull failed' ) ;
        CheckIs(lParam, TtiQueryParamStream, 'Wrong class' ) ;
        lStream2 := lParams.GetValueAsStream('param1');
        CheckStreamContentsSame(lStream1, lStream2);
        ls := lParams.GetValueAsString('param1') ;
        // Because, it's MIME encoded when it's read out as a string
        // (for persisting to a DB that does not have a native BIN field type)
        ls := MimeDecodeString(ls);
        CheckEquals( LongString, ls, 'GetValueAsString failed') ;

        lStream1.WriteString('a');
        lParams.SetValueAsStream('param1', lStream1);
        CheckEquals( 1, lParams.Count, 'Count');
        lParam := lParams.Items[0];
        CheckNotNull(lParams, 'NotNull failed' ) ;
        CheckIs(lParam, TtiQueryParamStream, 'Wrong class' ) ;
        lStream2 := lParams.GetValueAsStream('param1');
        CheckStreamContentsSame(lStream1, lStream2);
        ls := lParams.GetValueAsString('param1') ;
        // Because, it's MIME encoded when it's read out as a string
        // (for persisting to a DB that does not have a native BIN field type)
        ls := MimeDecodeString(ls);
        CheckEquals( LongString+'a', ls, 'GetValueAsString failed') ;

      finally
        lParams.Free;
      end;
  finally
    lStream1.Free;
  end;
end;

procedure TTestTIQueryParams.AsString;
var
  lParams : TtiQueryParams ;
  lParam : TtiQueryParamAbs ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsString('param1', 'test');
    CheckEquals( 1, lParams.Count, 'Count');
    lParam := lParams.Items[0];
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamString, 'Wrong class' ) ;
    CheckEquals( 'test', lParams.GetValueAsString('param1'), 'GetValueAsString failed') ;
  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.ParamByName;
var
  lParams : TtiQueryParams ;
  lParam : TtiQueryParamAbs ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsString(  'paramS', 'test');
    CheckEquals( 1, lParams.Count, 'Count');
    lParams.SetValueAsInteger( 'paramI', 123);
    CheckEquals( 2, lParams.Count, 'Count');
    lParams.SetValueAsFloat(   'paramF', 123.456);
    CheckEquals( 3, lParams.Count, 'Count');
    lParams.SetValueAsDateTime('paramD', Date);
    CheckEquals( 4, lParams.Count, 'Count');
    lParams.SetValueAsBoolean( 'paramB', True);
    CheckEquals( 5, lParams.Count, 'Count');

    lParam := lParams.FindParamByName('paramS');
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamString, 'Wrong class' ) ;

    lParam := lParams.FindParamByName('paramI');
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamInteger, 'Wrong class' ) ;

    lParam := lParams.FindParamByName('paramF');
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamFloat, 'Wrong class' ) ;

    lParam := lParams.FindParamByName('paramD');
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamDateTime, 'Wrong class' ) ;

    lParam := lParams.FindParamByName('paramB');
    CheckNotNull(lParams, 'NotNull failed' ) ;
    CheckIs(lParam, TtiQueryParamBoolean, 'Wrong class' ) ;

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.ParamIsNull;
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsString(  'paramS', 'test');
    lParams.SetValueAsInteger( 'paramI', 123);
    lParams.SetValueAsFloat(   'paramF', 123.456);
    lParams.SetValueAsDateTime('paramD', Date);
    lParams.SetValueAsBoolean( 'paramB', True);

    Check(lParams.ParamIsNull['parmaS'], 'String param null');
    Check(lParams.ParamIsNull['parmaI'], 'Integer param null');
    Check(lParams.ParamIsNull['parmaF'], 'Float param null');
    Check(lParams.ParamIsNull['parmaD'], 'Date param null');
    Check(lParams.ParamIsNull['parmaB'], 'Boolean param null');

    lParams.ParamIsNull[ 'paramS' ] := true ;
    lParams.ParamIsNull[ 'paramI' ] := true ;
    lParams.ParamIsNull[ 'paramF' ] := true ;
    lParams.ParamIsNull[ 'paramD' ] := true ;
    lParams.ParamIsNull[ 'paramB' ] := true ;

    Check(lParams.ParamIsNull['parmaS'], 'String param not null');
    Check(lParams.ParamIsNull['parmaI'], 'Integer param not null');
    Check(lParams.ParamIsNull['parmaF'], 'Float param not null');
    Check(lParams.ParamIsNull['parmaD'], 'Date param not null');
    Check(lParams.ParamIsNull['parmaB'], 'Boolean param not null');

  finally
    lParams.Free;
  end;
end;

procedure TTestTIQueryParams.Setup;
begin
  // Do nothing
end;

procedure TTestTIQueryParams.TearDown;
begin
  // Do nothing
end;

procedure TTestTIQueryParams.TestCheckStreamContentsSame;
var
  lStreamFrom : TStringStream ;
  lStreamTo   : TStream ;
  lResult     : boolean ;
  lMessage    : string ;
begin
  lStreamFrom := TStringStream.Create(LongString);
  try
    lStreamTo := TStringStream.Create(LongString);
    try
      lResult := AreStreamContentsSame( lStreamFrom, lStreamTo, lMessage ) ;
      Check( lResult, 'Returned FALSE but should have returned TRUE' );
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;

  lStreamFrom := TStringStream.Create(LongString);
  try
    lStreamTo := TStringStream.Create(LongString + 'a' );
    try
      lResult := AreStreamContentsSame( lStreamFrom, lStreamTo, lMessage );
      Check( not lResult, 'Returned TRUE but should have returned FALSE <' + lMessage + '>');
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;

  lStreamFrom := TStringStream.Create(LongString);
  try
    lStreamTo := TStringStream.Create(Copy( LongString, 1, Length( LongString ) - 1 ) + 'a' );
    try
      lResult := AreStreamContentsSame( lStreamFrom, lStreamTo, lMessage );
      Check( not lResult, 'Returned TRUE but should have returned FALSE <' + lMessage + '>');
    finally
      lStreamTo.Free;
    end;
  finally
    lStreamFrom.Free;
  end;

end;

procedure TTestTIQueryAbs.FieldAsStream;
var
  lStream1 : TStringStream ;
  lStream2 : TMemoryStream ;
begin
  CreateTableStream(FDatabase) ;
  try
    DoAttachAndConnect ;
    try
      lStream1 := TStringStream.Create(tiCreateStringOfSize(1000)) ;
      try
        PopulateTableStream(lStream1);
        DoReAttach;
        FQuery.SelectRow( cTIQueryTableName, nil ) ;
        lStream2 := TMemoryStream.Create;
        try
          FQuery.AssignFieldAsStream(cTIQueryColName, lStream2);
          CheckStreamContentsSame(lStream1, lStream2);
          lStream2.Size := 0 ;
          FQuery.AssignFieldAsStreamByIndex(cFieldAs_Index, lStream2);
          CheckStreamContentsSame(lStream1, lStream2);
        finally
          lStream2.Free;
        end;
        FQuery.Close ;
      finally
        lStream1.Free;
      end;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
    DropTestTable ;
  end ;
end;

procedure TTestTIQueryAbs.PopulateTableStream(const pValue: TStream);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsStream(cTIQueryColName, pValue) ;
    FDatabase.InsertRow( cTIQueryTableName, lParams ) ;
  finally
    lParams.Free ;
  end ;
end;

procedure TTestTIQueryAbs.InsertDeleteUpdate_Timing;
var
  lParams : TtiQueryParams ;
  lInsertTime : DWord ;
  lUpdateTime : DWord ;
  lDeleteTime : DWord ;
  lStart      : DWord ;
  i : integer ;
begin
  lInsertTime := 0 ;
  lUpdateTime := 0 ;
  lDeleteTime := 0 ;
  CreateTableString(FDatabase) ;
  try
    FQuery.AttachDatabase( FDatabase ) ;
    try

      lParams := TtiQueryParams.Create ;
      try
        lParams.SetValueAsString(cTIQueryColName, 'test') ;

        for i := 1 to cTimingRepeatCount do
        begin
          lStart := GetTickCount ;
          FDatabase.StartTransaction;
          FQuery.InsertRow( cTIQueryTableName, lParams ) ;
          FDatabase.Commit ;
          Inc(lInsertTime, GetTickCount - lStart);
          FQuery.Close ;

          lStart := GetTickCount ;
          FDatabase.StartTransaction;
          FQuery.UpdateRow( cTIQueryTableName, lParams, lParams ) ;
          FDatabase.Commit ;
          Inc(lUpdateTime, GetTickCount - lStart);
          FQuery.Close ;

          lStart := GetTickCount ;
          FDatabase.StartTransaction;
          FQuery.DeleteRow( cTIQueryTableName, lParams ) ;
          FDatabase.Commit ;
          Inc(lDeleteTime, GetTickCount - lStart);
          FQuery.Close ;
        end ;

      finally
        lParams.Free ;
      end ;
    finally
      FQuery.DetachDatabase ;
    end;
  finally
    DropTestTable ;
  end ;
  WriteTimingResult('InsertRow', PerFrameworkSetup.PerLayerName, lInsertTime / cTimingRepeatCount);
  WriteTimingResult('UpdateRow', PerFrameworkSetup.PerLayerName, lUpdateTime / cTimingRepeatCount);
  WriteTimingResult('DeleteRow', PerFrameworkSetup.PerLayerName, lDeleteTime / cTimingRepeatCount);
end;

{$IFDEF TESTINT64}
procedure TTestTIQueryAbs.PopulateTableInt64(const pValue: Int64);
var
  lParams : TtiQueryParams ;
begin
  lParams := TtiQueryParams.Create ;
  try
    FDatabase.DeleteRow( cTIQueryTableNameInt64, nil ) ;
    lParams.SetValueAsInteger(cTIQueryColName, pValue) ;
    FDatabase.InsertRow( cTIQueryTableNameInt64, lParams ) ;
  finally
    lParams.Free ;
  end ;
end;
{$ENDIF}

{$IFDEF PERFORMANCE_TESTS}
procedure TTestTIQueryAbs.FieldByNameVSFieldByIndex;
const
  cString = 'abcdefghij' ;
  cCount  = 1000000 ;
  cImprovement = 100 ;
var
  lStart : DWord ;
  i : Integer ;
  lByName  : DWOrd ;
  lByIndex : DWord;
  lRatio   : real;
begin
  CreateTableString(FDatabase) ;
  try
    DoAttachAndConnect ;
    try
      PopulateTableString(cString);
      FQuery.SelectRow( cTIQueryTableName, nil ) ;
      lStart := GetTickCount ;
      for i := 1 to cCount do
        FQuery.FieldAsString[ cTIQueryColName ];
      lByName := GetTickCount - lStart;

      lStart := GetTickCount ;
      for i := 1 to cCount do
        FQuery.FieldAsStringByIndex[ cFieldAs_Index ];
      lByIndex := GetTickCount - lStart;

      FQuery.Close ;
    finally
      DoDetachAndDisconnect ;
    end;
  finally
    DropTestTable ;
  end ;
  Check( lByIndex < lByName, 'It got slower. ByIndex: ' +
         IntToStr(lByIndex) + ' ByName: ' + IntToStr(lByName));
  lRatio := 10000 / (lByIndex * 10000 div lByName);
  Check( lRatio > cImprovement, 'Not fast enough: ' + tiFloatToStr( lRatio, 2 ) + ' x faster (should be ' + IntToStr(cImprovement) + ')' ) ;

end;
{$ENDIF}

end.
