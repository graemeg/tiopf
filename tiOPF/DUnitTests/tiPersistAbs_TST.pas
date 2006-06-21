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

unit tiPersistAbs_TST;

interface
uses
  TestFrameWork
  ,TestExtensions
  ,tiPtnVisPerObj
  ,tiDBConnectionSetupAbs_TST
  ,tiQuery
  ,Classes
  ,SysUtils
  ;

const
  cFieldAs_Index = 1 ; // Index of field created in CreateTable???() methods


type

  TtiDUnitSetupTask = ( sutPerLayer, sutDBConnection, sutTables ) ;
  TtiDUnitSetupTasks = set of TtiDUnitSetupTask ;

  // Abstract test case for testing non persistence related classes
  TtiTestCase = class( TTestCase )
  private
    FLongString : string ;
  protected
    function    tstIntToStr(pInt:Integer):string;
    function    tstIntToBool(pInt:Integer):boolean;
    function    tstIntToFloat(pInt:Integer):real;
    function    tstIntToDateTime(pInt:Integer):TDateTime;
    procedure   tstIntToStream(pInt:Integer; const pStream : TStream);
    property    LongString : string    read FLongString write FLongString ;
    function    AreStreamContentsSame( const pStream1, pStream2 : TStream ; var pMessage : string ) : boolean ;
    procedure   CheckStreamContentsSame( const pStream1, pStream2 : TStream ) ;
  public
    constructor Create(MethodName : string); override ;
  end ;

  // Abstract test case for testing persistence related classes
  TtiPerTestCase = class( TtiTestCase )
  private
    FSetupTasks: TtiDUnitSetupTasks;
    procedure   SetSetupTasks(const Value: TtiDUnitSetupTasks);
    function    GetPerLayerName: string;
    function    GetDatabaseName: string;
    function    GetUserName: string;
    function    GetPassword: string;
  protected
    procedure   Setup ; override ;
    procedure   TearDown ; override ;
    procedure   CheckObjectState(pObjectState: TPerObjectState ; const pData : TPerObjAbs );
    procedure   CheckExceptionMessage(const pMessage : string ; const pException : Exception ) ;
    procedure   CreateDBIfNotExists ; virtual ;
    procedure   SetupTestTables ;
    procedure   DeleteTestTables ;
    procedure   EmptyTestTables ;

    // ToDo: These overloaded method are confusing. Fix.
    procedure   CreateTableInteger(    const pDatabaseName, pPerLayerName : string );overload;
//    procedure   CreateTableInt64(      const pDatabaseName, pPerLayerName : string );overload;
    procedure   CreateTableString(     const pDatabaseName, pPerLayerName : string );overload;
    procedure   CreateTableFloat(      const pDatabaseName, pPerLayerName : string );overload;
    procedure   CreateTableBoolean(    const pDatabaseName, pPerLayerName : string );overload;
    procedure   CreateTableDateTime(   const pDatabaseName, pPerLayerName : string );overload;
    procedure   CreateTableLongString( const pDatabaseName, pPerLayerName : string );overload;
    procedure   CreateTableStream(     const pDatabaseName, pPerLayerName : string );overload;
    procedure   CreateTableInteger(    const pDatabase : TtiDatabase );overload;
//    procedure   CreateTableInt64(      const pDatabase : TtiDatabase );overload;
    procedure   CreateTableString(     const pDatabase : TtiDatabase );overload;
    procedure   CreateTableFloat(      const pDatabase : TtiDatabase );overload;
    procedure   CreateTableBoolean(    const pDatabase : TtiDatabase );overload;
    procedure   CreateTableDateTime(   const pDatabase : TtiDatabase );overload;
    procedure   CreateTableLongString( const pDatabase : TtiDatabase );overload;
    procedure   CreateTableStream(     const pDatabase : TtiDatabase );overload;

    procedure   DropTestTable ;

    procedure   CreateTableTestBin(  const pDatabase: TtiDatabase = nil);
    procedure   CreateTableTestGroup(const pDatabase: TtiDatabase = nil);
    procedure   InsertIntoTestGroup( const pDatabase: TtiDatabase ; pValue : integer ) ;
    procedure   DropTableTestBin(    const pDatabase: TtiDatabase = nil);
    procedure   DropTableTestGroup(  const pDatabase: TtiDatabase = nil);

    procedure   DropNextOIDTable;
    procedure   CreateNextOIDIntTable;
    procedure   CreateNextOIDStrTable;

    procedure   WriteTimingResult(const pAction, pPerLayerName : string ; pValue : real);

  public
    constructor Create(MethodName : string); override ;
    function    PerFrameworkSetup : TPerFrameworkSetup;
    property    PerLayerName      : string read GetPerLayerName ;
    property    DatabaseName      : string read GetDatabaseName ;
    property    UserName          : string read GetUserName;
    property    Password          : string read GetPassword;
    property    SetupTasks        : TtiDUnitSetupTasks read FSetupTasks write SetSetupTasks ;
    function    GetName: string; override ;
  end ;

  TtiTestCaseClass = class of TtiPerTestCase ;

  TTestDBConnectionSetup = class( TTestSetup )
  private
  protected
    procedure Setup ; override ;
    procedure TearDown ; override ;
    function  PerLayerID : ShortString ; virtual ; abstract ;
  public
    function  GetName : String ; override ;
  end ;

const
  cTableNameTestGroup               = 'Test_Group' ;

  cTableNameTIOPFTestChild_A        = 'Test_Child_a' ;
  cTableNameTIOPFTestChild_B        = 'Test_Child_b' ;
  cTableNameTIOPFTestParent         = 'Test_Parent';

  cTableNameCreateTable             = 'Test_Create_Table' ;
  cTableNameTIOPFTestChildGrouped_A = 'Test_Child_Grouped_a' ;
  cTableNameTIOPFTestChildGrouped_B = 'Test_Child_Grouped_b' ;
  cTableNameTIOPFTestParentGrouped  = 'Test_Parent_Grouped'; // The objects's parent table
  cTableNameTIOPFTestParentGroup    = 'Test_Parent_Group';  // The collection table

//procedure DropTableTestGroup(   const pDatabase : TtiDatabase = nil ) ;
//procedure CreateTableTestGroup( const pDatabase : TtiDatabase = nil ) ;
//procedure CreateTableTestBin(   const pDatabase : TtiDatabase = nil ) ;
//procedure DropTableTestBin(     const pDatabase : TtiDatabase = nil ) ;

  cTIQueryTableName = 'TIQueryTable' ;
  cTIQueryTableNameInt64 = 'TIQueryTableInt64' ;
  cTIQueryColName   = 'TIQueryField' ;
  cTimingRepeatCount = 400 ;


implementation
uses
  tiPersist
  ,tiRegPerLayer
  ,cTIPersist
  ,Windows // for sleep
  ,tiLog
  ,Forms
  ,tiPtnVisPerObj_Cli
  ,tiUtils
  ,tiDUnitDependencies
  ,tiDUnitUtils
  ,INIFiles
  ;

var
  uPerFrameworkSetup : TPerFrameworkSetup;

procedure TtiPerTestCase.DropTableTestGroup( const pDatabase : TtiDatabase = nil ) ;
begin
  if pDatabase = nil then
    try gTIPerMgr.DropTable( cTableNameTestGroup,
                             PerFrameworkSetup.DBName,
                             PerFrameworkSetup.PerLayerName )  except end
  else
    try pDatabase.DropTable( cTableNameTestGroup ) except end ;
end;

procedure TtiPerTestCase.DeleteTestTables;
begin
  DropTableTestGroup;
  DropTableTestBin;
  try gTIPerMgr.DropTable( 'test_item',
                           PerFrameworkSetup.DBName,
                           PerFrameworkSetup.PerLayerName )  except end ;

  try gTIPerMgr.DropTable( cTableNameTIOPFTestChild_A, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName )  except end ;
  try gTIPerMgr.DropTable( cTableNameTIOPFTestChild_B, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName )  except end ;
  try gTIPerMgr.DropTable( cTableNameTIOPFTestParent, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName )  except end ;

  try gTIPerMgr.DropTable( cTableNameTIOPFTestChildGrouped_A, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName )  except end ;
  try gTIPerMgr.DropTable( cTableNameTIOPFTestChildGrouped_B, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName )  except end ;
  try gTIPerMgr.DropTable( cTableNameTIOPFTestParentGrouped, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName )  except end ;
  try gTIPerMgr.DropTable( cTableNameTIOPFTestParentGroup, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName )  except end ;

end;

procedure TtiPerTestCase.CreateTableTestGroup( const pDatabase : TtiDatabase = nil )  ;
var
  lTable : TtiDBMetaDataTable ;
begin
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTableNameTestGroup ;
    lTable.AddField( 'OID',               qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Group_Str_Field',   qfkString, 10 ) ;
    lTable.AddField( 'Group_Int_Field',   qfkInteger ) ;
    lTable.AddField( 'Group_Float_Field', qfkFloat ) ;
    lTable.AddField( 'Group_Date_Field',  qfkDateTime ) ;
    lTable.AddField( 'Group_Bool_Field',  qfkLogical ) ;
    lTable.AddField( 'Group_Notes_Field', qfkLongString ) ;
    if pDatabase = nil then
      gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName )
    else
      pDatabase.CreateTable(lTable);
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.InsertIntoTestGroup(const pDatabase: TtiDatabase ; pValue : integer);
var
  lParams : TtiQueryParams ;
begin
  Check( FLongString <> '', 'FLongString not assigned' );
  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsString('OID', IntToStr(pValue)) ;
    lParams.SetValueAsString('Group_Str_Field', tiPad0( IntToStr( pValue ), 10 )) ;
    lParams.SetValueAsInteger('Group_Int_Field', pValue) ;
    lParams.SetValueAsFloat('Group_Float_Field', StrToInt( tiReplicate( IntToStr( pValue ), 4 )) / 1000) ;
    lParams.SetValueAsDateTime('Group_Date_Field', EncodeDate( 1900, 1, pValue ))  ;
    lParams.SetValueAsBoolean('Group_Bool_Field', (( pValue mod 2 ) = 0 ))  ;
    lParams.SetValueAsString('Group_Notes_Field', FLongString)  ;
    pDatabase.InsertRow( cTableNameTestGroup, lParams ) ;
    // Each test takes place inside a transaction.
    // If you want the test data to be setup inside it's own
    // transaction, then uncomment these lines.
    // FDatabase.Commit ;
    // FDatabase.StartTransaction ;
  finally
    lParams.Free ;
  end ;
end ;

procedure TtiPerTestCase.CreateTableTestBin( const pDatabase : TtiDatabase = nil ) ;
var
  lTable : TtiDBMetaDataTable ;
begin
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := 'Test_Bin' ;
    lTable.AddField( 'OID',               qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Item_Binary_Field', qfkBinary  ) ;
    if pDatabase = nil then
      gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) 
    else
      pDatabase.CreateTable(lTable);
  finally
    lTable.Free;
  end;
end;

procedure TtiPerTestCase.DropTableTestBin( const pDatabase : TtiDatabase = nil ) ;
begin
  if pDatabase = nil then
    try gTIPerMgr.DropTable( 'test_bin',
                             PerFrameworkSetup.DBName,
                             PerFrameworkSetup.PerLayerName )  except end
  else
    try pDatabase.DropTable( 'test_bin'   ) except end;
end;

{ TPerFrameworkSetup }

function TTestDBConnectionSetup.GetName: String ;
begin
  result := 'Setup [' + PerLayerID + '] connection for ' + Test.Name ;
end;

{ TtiTestCase }

constructor TtiPerTestCase.Create(MethodName: string);
begin
  inherited;
  // Must assign SetupTasks in the concrete's create method
  //SetupTasks := [sutPerLayer, sutDBConnection, sutTables];
end;

procedure TtiPerTestCase.CreateDBIfNotExists;
var
  lRegPerLayer   : TtiRegPerLayer ;
  lDatabaseClass : TtiDatabaseClass ;
begin
  lRegPerLayer := gTIPerMgr.RegPerLayers.FindByPerLayerName(PerLayerName);
  Assert(lRegPerLayer <> nil, 'Unable to find registered persistence layer <' + PerLayerName +'>' ) ;
  lDatabaseClass := lRegPerLayer.tiDatabaseClass ;
  if not lDatabaseClass.DatabaseExists(
    PerFrameworkSetup.DBName,
    PerFrameworkSetup.Username,
    PerFrameworkSetup.Password ) then
  begin
    lDatabaseClass.CreateDatabase(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;
    if not lDatabaseClass.DatabaseExists(
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) then
      tiFmtException( 'Unable to create database <' +
                      PerFrameworkSetup.DBName + '>' ) ;

  end ;
end;

procedure TtiPerTestCase.EmptyTestTables;
begin
  gTIPerMgr.DeleteRow( 'test_item',    nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  gTIPerMgr.DeleteRow( cTableNameTestGroup,   nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  gTIPerMgr.DeleteRow( 'test_bin',     nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  gTIPerMgr.DeleteRow( cTableNameTIOPFTestChild_A, nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  gTIPerMgr.DeleteRow( cTableNameTIOPFTestChild_B, nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  gTIPerMgr.DeleteRow( 'test_parent',  nil, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
end;

function TtiPerTestCase.PerFrameworkSetup: TPerFrameworkSetup;
begin
  result := uPerFrameworkSetup ;
end;

function TtiPerTestCase.GetPerLayerName: string;
begin
  Assert( PerFrameworkSetup.TestValid, cTIInvalidObjectError );
  result := PerFrameworkSetup.PerLayerName ;
end;

procedure TtiPerTestCase.SetSetupTasks(const Value: TtiDUnitSetupTasks);
begin
  FSetupTasks := Value;
  if ( sutDBConnection in Value ) and
     ( not ( sutPerLayer in Value )) then
    FSetupTasks := FSetupTasks + [sutPerLayer] ;
  if ( sutTables in FSetupTasks ) and
     ( not ( sutDBConnection in FSetupTasks )) then
    FSetupTasks := FSetupTasks + [sutDBConnection] ;
end;

procedure TtiPerTestCase.Setup;
begin
  inherited ;
  Assert( PerFrameworkSetup <> nil,
          'Request for unregistered framework setup <FPerLayerID>' ) ;
  // Load persistence layer
  if sutPerLayer in SetupTasks then
  begin
    if not gTIPerMgr.RegPerLayers.IsLoaded( PerFrameworkSetup.PerLayerName ) then
      gTIPerMgr.LoadPersistenceLayer( PerFrameworkSetup.PerLayerName ) ;
    if PerFrameworkSetup.CanCreateDatabase then
      CreateDBIfNotExists ;
  end ;

  // Establish a database connection
  if sutDBConnection in SetupTasks then
    gTIPerMgr.LoadDatabaseLayer(
      PerFrameworkSetup.PerLayerName,
      PerFrameworkSetup.DBName,
      PerFrameworkSetup.Username,
      PerFrameworkSetup.Password ) ;

  // Setup test tables
  if sutTables in SetupTasks then
    SetupTestTables ;

  Application.ProcessMessages ;
end;

procedure TtiPerTestCase.SetupTestTables;
var
  lTable : TtiDBMetaDataTable ;
begin
  DeleteTestTables ;
  CreateTableTestGroup ;
  CreateTableTestBin;
  lTable := TtiDBMetaDataTable.Create ;
  try

    lTable.Clear ;
    lTable.Name := 'Test_Item' ;
    lTable.AddField( 'OID',                qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'OID_Group',          qfkString, 36 ) ; // Should be Not Null & FK
    lTable.AddField( 'Item_Str_Field',     qfkString, 10 ) ;
    lTable.AddField( 'Item_Int_Field',     qfkInteger ) ;
    lTable.AddField( 'Item_Float_Field',   qfkFloat ) ;
    lTable.AddField( 'Item_Bool_Field',    qfkLogical ) ;
    lTable.AddField( 'Item_Date_Field',    qfkDateTime ) ;
    lTable.AddField( 'Item_Notes_Field',   qfkLongString ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;

    lTable.Clear ;
    lTable.Name :=  cTableNameTIOPFTestParent ;
    lTable.AddField( 'OID',                qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Owner_OID',          qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Parent_Str_Field',   qfkString, 10 ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;

    lTable.Clear ;
    lTable.Name :=  cTableNameTIOPFTestChild_A;
    lTable.AddField( 'OID',               qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Child_Int_Field',   qfkInteger ) ;
    lTable.AddField( 'Child_Float_Field', qfkFloat ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;

    lTable.Clear ;
    lTable.Name :=  cTableNameTIOPFTestChild_B;
    lTable.AddField( 'OID',               qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Child_Int_Field',   qfkInteger ) ;
    lTable.AddField( 'Child_Float_Field', qfkFloat ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;

    lTable.Clear ;
    lTable.Name :=  cTableNameTIOPFTestParentGrouped ;
    lTable.AddField( 'OID',                qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Owner_OID',          qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Parent_Str_Field',   qfkString, 10 ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;

    lTable.Clear ;
    lTable.Name :=  cTableNameTIOPFTestChildGrouped_A;
    lTable.AddField( 'OID',               qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Child_Int_Field',   qfkInteger ) ;
    lTable.AddField( 'Child_Float_Field', qfkFloat ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;

    lTable.Clear ;
    lTable.Name :=  cTableNameTIOPFTestChildGrouped_B;
    lTable.AddField( 'OID',               qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( 'Child_Int_Field',   qfkInteger ) ;
    lTable.AddField( 'Child_Float_Field', qfkFloat ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;

    lTable.Clear ;
    lTable.Name :=  cTableNameTIOPFTestParentGroup ;
    lTable.AddField( 'OID',                qfkString, 36 ) ; // Should be Not Null & PK
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;

  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.TearDown;
begin
  if sutTables in SetupTasks then
    DeleteTestTables ;
  if sutDBConnection in SetupTasks then
    gTIPerMgr.UnLoadDatabaseLayer(PerFrameworkSetup.PerLayerName,
                                  PerFrameworkSetup.DBName) ;
  if ( sutPerLayer in SetupTasks ) and
     (gPerFrameworkSetupFactory.TestPersistenceLayers = tplOneAtTheTime ) then
    gTIPerMgr.UnLoadPersistenceFramework(PerFrameworkSetup.PerLayerName) ;
  Application.ProcessMessages ;
end;

procedure TTestDBConnectionSetup.Setup;
begin
  inherited;
  // Wish I could work out how to set a property on the instance of TestCase
  // that this decorator owns, but I have burnt hours on it and cant
  // crack the problem. Looks like there are several instances of TestCase
  // floating around, and if you do this:
  // TtiTestCase(Test).PerFrameworkSetup := Bla
  // then you get a different instance to the one that gets run -
  // hence PerFrameworkSetup is nill and of no use. Got any ideas?
  uPerFrameworkSetup := gPerFrameworkSetupFactory.FindByClassID( PerLayerID ) ;
end;

procedure TTestDBConnectionSetup.TearDown;
begin
  uPerFrameworkSetup := nil ;
  inherited;
end;

function TtiPerTestCase.GetDatabaseName: string;
begin
  Assert( PerFrameworkSetup.TestValid, cTIInvalidObjectError );
  result := PerFrameworkSetup.DBName ;
end;

procedure TtiPerTestCase.DropNextOIDTable ;
begin
  try
    gTIPerMgr.DropTable( 'Next_OID', PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName );
  except
  end ;
end ;

procedure TtiPerTestCase.CreateNextOIDIntTable ;
var
  lTable  : TtiDBMetaDataTable ;
  lParams : TtiQueryParams ;
begin

  DropNextOIDTable ;

  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := 'Next_OID' ;
    lTable.AddField( 'OID', qfkInteger ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  finally
    lTable.Free ;
  end ;

  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsInteger( 'OID', 1000000) ;
    gTIPerMgr.InsertRow( 'Next_OID', lParams, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  finally
    lParams.Free ;
  end ;

end ;

procedure TtiPerTestCase.CreateNextOIDStrTable ;
var
  lTable  : TtiDBMetaDataTable ;
  lParams : TtiQueryParams ;
begin

  DropNextOIDTable ;

  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := 'Next_OID' ;
    lTable.AddField( 'OID', qfkString, 10 ) ;
    gTIPerMgr.CreateTable( lTable, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  finally
    lTable.Free ;
  end ;

  lParams := TtiQueryParams.Create ;
  try
    lParams.SetValueAsString( 'OID', '0' );
    gTIPerMgr.InsertRow( 'Next_OID', lParams, PerFrameworkSetup.DBName, PerFrameworkSetup.PerLayerName ) ;
  finally
    lParams.Free ;
  end ;

end ;

procedure TtiTestCase.CheckStreamContentsSame(const pStream1, pStream2: TStream);
var
  lResult : boolean ;
  lMessage : string ;
begin
  lResult := AreStreamContentsSame(pStream1, pStream2, lMessage);
  if not lResult then
    Fail(lMessage);
end;

function TtiTestCase.AreStreamContentsSame(const pStream1, pStream2: TStream; var pMessage : string ): boolean;
var
   LByte1, LByte2 : byte;
begin
   result := true;

   if pStream1.Size <> pStream2.Size then
   begin
     result := false;
     pMessage := 'Streams have different sizes ('+inttostr(pStream1.Size)+'/'+
                 inttostr(pStream2.Size)+')';
     exit; // =============================>
   end;

   pStream1.Position := 0 ;
   pStream2.Position := 0 ;
   while (pStream1.Size - pStream1.Position > 0) do
   begin
     pStream1.Read(LByte1, 1);
     pStream2.Read(LByte2, 1);
     if LByte1 <> LByte2 then
     begin
       result := false;
       pMessage := 'Streams Differ at position '+
                   inttostr(pStream1.Position)+' of '+
                   inttostr(pStream1.Size)+
                   ':'+inttostr(LByte1)+'/'+inttostr(LByte2);
       exit; // =============================>
     end;
   end;
end ;

procedure TtiPerTestCase.CreateTableBoolean(const pDatabaseName, pPerLayerName : string);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkLogical ) ;
    gTIPerMgr.CreateTable( lTable, pDatabaseName, pPerLayerName ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableBoolean(const pDatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkLogical ) ;
    pDatabase.CreateTable( lTable ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableDateTime(const pDatabaseName, pPerLayerName : string);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkDateTime ) ;
    gTIPerMgr.CreateTable( lTable, pDatabaseName, pPerLayerName ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableDateTime(const pDatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkDateTime ) ;
    pDatabase.CreateTable( lTable ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableFloat(const pDatabaseName, pPerLayerName : string);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkFloat ) ;
    gTIPerMgr.CreateTable( lTable, pDatabaseName, pPerLayerName ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableFloat(const pDatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkFloat ) ;
    pDatabase.CreateTable( lTable ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableInteger(const pDatabaseName, pPerLayerName : string);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkInteger ) ;
    gTIPerMgr.CreateTable( lTable, pDatabaseName, pPerLayerName ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableInteger(const pDatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkInteger ) ;
    pDatabase.CreateTable( lTable ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableLongString(const pDatabaseName, pPerLayerName : string);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkLongString ) ;
    gTIPerMgr.CreateTable( lTable, pDatabaseName, pPerLayerName ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableLongString(const pDatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkLongString ) ;
    pDatabase.CreateTable( lTable ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableStream(const pDatabaseName, pPerLayerName : string);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkBinary ) ;
    gTIPerMgr.CreateTable( lTable, pDatabaseName, pPerLayerName ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableStream(const pDatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkBinary ) ;
    pDatabase.CreateTable( lTable) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableString(const pDatabaseName, pPerLayerName : string);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkString, 255 ) ;
    gTIPerMgr.CreateTable( lTable, pDatabaseName, pPerLayerName ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.CreateTableString(const pDatabase : TtiDatabase);
var
  lTable : TtiDBMetaDataTable ;
begin
  // If the structure of the table created is changed, also change the
  // constant: cFieldAs_Index
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkString, 255 ) ;
    pDatabase.CreateTable( lTable ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiPerTestCase.DropTestTable;
begin
  try gTIPerMgr.DropTable( cTIQueryTableName, DatabaseName, PerLayerName ) except end ;
end;

procedure TtiPerTestCase.WriteTimingResult(const pAction, pPerLayerName : string ; pValue : real);
var
  lFileName : string ;
  lINI : TINIFile ;
begin
  lFileName := ExtractFilePath(ParamStr(0)) + '\DUnitTIOPFTimingResults.txt' ;
  lINI := TINIFile.Create(lFileName);
  try
    lINI.WriteString(pAction, pPerLayerName, Format('%12.2f', [pValue]));
  finally
    lINI.Free;
  end;
end;

{ These routines happen to be a clone of CreateTableInteger... for now.
  This means that our Int64 tests cannot be conclusive until we can
  auto-create Int64 fields.

  NOT USED FOR NOW

procedure TtiTestCase.CreateTableInt64(const pDatabaseName,  pPerLayerName: string);
var
  lTable : TtiDBMetaDataTable ;
begin
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkInteger ) ;
    gTIPerMgr.CreateTable( lTable, pDatabaseName, pPerLayerName ) ;
  finally
    lTable.Free ;
  end ;
end;

procedure TtiTestCase.CreateTableInt64(const pDatabase: TtiDatabase);
var
  lTable : TtiDBMetaDataTable ;
begin
  lTable := TtiDBMetaDataTable.Create ;
  try
    lTable.Name := cTIQueryTableName ;
    lTable.AddField( 'OID', qfkString, 36 ) ; // Should be Not Null & PK
    lTable.AddField( cTIQueryColName, qfkInteger ) ;
    pDatabase.CreateTable( lTable ) ;
  finally
    lTable.Free ;
  end ;
end;
 }

procedure TtiPerTestCase.CheckObjectState(pObjectState: TPerObjectState; const pData: TPerObjAbs);
begin
  Assert( pData.TestValid(TPerObjAbs), cTIInvalidObjectError);
  Check(pData.ObjectState = pObjectState,
        'ObjectState. Expected ' +
        pData.ClassName + '.' + ObjectStateToString(pObjectState) +
        ' but is ' +
        pData.ObjectStateAsString ) ;
end;

procedure TtiPerTestCase.CheckExceptionMessage(const pMessage: string;
  const pException: Exception);
begin
  Check( Pos( UpperCase(pMessage), UpperCase(pException.Message)) <> 0,
         'Expected "' + pMessage +'" in exception message but found "' +
         pException.message + '"' ) ;
end;

function TtiPerTestCase.GetPassword: string;
begin
  Assert( PerFrameworkSetup.TestValid, cTIInvalidObjectError );
  result := PerFrameworkSetup.Password ;
end;

function TtiPerTestCase.GetUserName: string;
begin
  Assert( PerFrameworkSetup.TestValid, cTIInvalidObjectError );
  result := PerFrameworkSetup.Username ;
end;

constructor TtiTestCase.Create(MethodName: string);
begin
  inherited;
  FLongString := tiCreateStringOfSize(3000);
end;

function TtiTestCase.tstIntToBool(pInt: Integer): boolean;
begin
  Result := ( pInt mod 2 ) = 0 ;
end;

function TtiTestCase.tstIntToDateTime(pInt: Integer): TDateTime;
begin
  Result := pInt / 1000 ;
end;

function TtiTestCase.tstIntToFloat(pInt: Integer): real;
begin
  Result := pInt / 1000 ;
end;

function TtiTestCase.tstIntToStr(pInt: Integer): string;
begin
  Result := IntToStr(pInt);
end;

procedure TtiTestCase.tstIntToStream(pInt: Integer;const pStream: TStream);
begin
  tiStringToStream(tstIntToStr(pInt), pStream);
end;


function TtiPerTestCase.GetName: string;
begin
  if PerFrameworkSetup <> nil then
    result := '[' +
      PerLayerName + '] ' +
      Inherited GetName
  else
    result := Inherited GetName;
end;

end.
