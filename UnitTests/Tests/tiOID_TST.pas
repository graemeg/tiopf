unit tiOID_TST;

{$I tiDefines.inc}

interface

uses
  Classes,
  tiTestFramework,
  tiOPFTestCase,
  tiOID;


type

  // Persistence TtiOID tests
  TTestTIOIDPersistent = class(TtiTestCaseWithDatabaseConnection)
  protected
    FOIDGeneratorClass: TtiOIDGeneratorClass;
    FOIDList:           TStringList;
    FRepeatCount: integer;
    FNumThreads: integer;
    procedure TestThenAddOIDAsString(const AOID: string);
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CreateNextOIDIntTable;
    procedure CreateNextOIDStrTable;
  published

    // These must be run for each persistence layer & oid class
    // if the OID generator touches the database. Integer OIDs touch the
    // database. GUID oid generators dont. The down side of the test hierarchy
    // is that GUID tests will be run for each persistence layer.
    procedure TtiNextOIDGeneratorAssignNextOIDSingleUser; virtual;
    procedure TtiNextOIDGeneratorAssignNextOIDThreaded; virtual;
    procedure TtiNextOIDGeneratorAssignNextOIDMultiUser; virtual;

    // These must be run for each persistence layer & each OID class
    procedure TtiOIDAssignToTIQueryParam;
    procedure TtiOIDAssignToTIQuery;
    procedure TtiOIDAssignToTIQueryFieldName;
    procedure TtiOIDAssignFromTIQuery;
    procedure TtiOIDAssignFromTIQueryFieldName;
    procedure TtiOIDEqualsQueryField;
    procedure TtiObjectCreateNew;
  end;

  TTestTIOIDPersistentGUID = class(TTestTIOIDPersistent)
  protected
    procedure SetUp; override;
  published
    procedure TtiNextOIDGeneratorAssignNextOIDSingleUser; override;
    procedure TtiNextOIDGeneratorAssignNextOIDThreaded; override;
    procedure TtiNextOIDGeneratorAssignNextOIDMultiUser; override;
  end;

  TTestTIOIDPersistentInteger = class(TTestTIOIDPersistent)
  protected
    procedure SetUp; override;
  end;

  TTestTIOIDNonPersistent = class(TtiTestCase)
  private
    FOIDClass: TtiOIDClass;
  published
    procedure NextOIDGeneratorClass; virtual; abstract;
    procedure AsString; virtual; abstract;
    procedure AsVariant; virtual; abstract;
    procedure IsNullAndSetToNull; virtual; abstract;
    procedure Assign; virtual; abstract;
    procedure NullOIDAsString; virtual; abstract;
    procedure Compare;
    procedure tiObject_Equals;
    procedure Clone;
  end;

  { base test class for Integer and Int64 tests }
  TTestTIOIDIntBase = class(TTestTIOIDNonPersistent)
  published
    procedure Assign; override;
    procedure AsString; override;
    procedure IsNullAndSetToNull; override;
  end;

  TTestTIOIDInteger = class(TTestTIOIDIntBase)
  protected
    procedure SetUp; override;
  published
    procedure NextOIDGeneratorClass; override;
    procedure AsVariant; override;
    procedure NullOIDAsString; override;
  end;

  TTestTIOIDInt64 = class(TTestTIOIDIntBase)
  protected
    procedure SetUp; override;
  published
    procedure NextOIDGeneratorClass; override;
    procedure AsString; override;
    procedure AsVariant; override;
    procedure NullOIDAsString; override;
  end;

  TTestTIOIDString = class(TTestTIOIDNonPersistent)
  protected
    procedure SetUp; override;
  published
    procedure NextOIDGeneratorClass; override;
    procedure AsString; override;
    procedure AsVariant; override;
    procedure IsNullAndSetToNull; override;
    procedure Assign; override;
    procedure NullOIDAsString; override;
  end;


  TTestTIOIDGUID = class(TTestTIOIDNonPersistent)
  private
    FGuidAsString: string;
  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure NextOIDGeneratorClass; override;
    procedure AsString; override;
    procedure AsVariant; override;
    procedure IsNullAndSetToNull; override;
    procedure Assign; override;
    procedure NullOIDAsString; override;
  end;

procedure RegisterTests;

implementation

uses
  tiOPFManager,
  tiObject,
  SysUtils,
  tiOIDGUID,    // Pull in the GUID OID framework
  tiOIDInteger, // Pull in the integer OID framework
  tiOIDString,  // Pull in the string OID framework
  tiOIDInt64, // Pull in the Int64 OID framework
  tiQuery,
  tiThread,
  {$IFDEF MSWINDOWS}
  tiWin32,
  {$ENDIF}
  tiOPFTestManager,
  tiTestDependencies,
  tiUtils;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIOIDInteger);
  tiRegisterNonPersistentTest(TTestTIOIDInt64);
  tiRegisterNonPersistentTest(TTestTIOIDString);
  tiRegisterNonPersistentTest(TTestTIOIDGUID);
end;


{ TTestTIOID }

procedure TTestTIOIDNonPersistent.Clone;
var
  lOID1: TtiOID;
  lOID2: TtiOID;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID1.AsVariant := 1;
    lOID2 := lOID1.Clone;
    try
      CheckIs(lOID2, FOIDClass, 'Failed on OID.Clone.ClassType');
      CheckEquals(lOID1.AsString, lOID2.AsString, 'Failed on lOID1.AsVariant = lOID2.AsVariant');
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOIDNonPersistent.Compare;
var
  lOID1:    TtiOID;
  lOID2:    TtiOID;
  lCompare: integer;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := 2;
      lOID2.AsVariant := 2;
      lCompare        := lOID1.Compare(lOID2);
      CheckEquals(0, lCompare, 'Failed on compare = 0');

      lOID1.AsVariant := 2;
      lOID2.AsVariant := 1;
      lCompare        := lOID1.Compare(lOID2);
      CheckEquals(1, lCompare, 'Failed on compare = 1');

      lOID1.AsVariant := 1;
      lOID2.AsVariant := 2;
      lCompare        := lOID1.Compare(lOID2);
      CheckEquals(-1, lCompare, 'Failed on compare = -1');
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOIDNonPersistent.tiObject_Equals;
var
  lOID1:   TtiOID;
  lOID2:   TtiOID;
  lEquals: boolean;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := 1;
      lOID2.AsVariant := 1;
      lEquals         := lOID1.Equals(lOID2);
      Check(lEquals, 'Failed on Equals');

      lOID1.AsVariant := 1;
      lOID2.AsVariant := 2;
      lEquals         := lOID1.Equals(lOID2);
      Check(not lEquals, 'Failed on not Equals');

    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


{ TTestTIOIDInteger }

procedure TTestTIOIDInteger.AsVariant;
var
  lOID: TtiOID;
  lInt: integer;
  lStr: string;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsVariant := '1';
    CheckEquals('1', lOID.AsString, 'Failed on AsString');
    lStr := lOID.AsVariant;
    CheckEquals('1', lStr, 'Failed on AsVariant #1');
    lInt := lOID.AsVariant;
    CheckEquals(1, lInt, 'Failed on AsVariant #2');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDInteger.NextOIDGeneratorClass;
var
  LOIDGenerator: TtiOIDGenerator;
begin
  LOIDGenerator := TtiOIDGeneratorInteger.Create;
  try
    Check(LOIDGenerator.OIDClass = TOIDInteger);
  finally
    LOIDGenerator.Free;
  end;
end;

procedure TTestTIOIDInteger.NullOIDAsString;
var
  LOID: TtiOID;
begin
  LOID := TOIDInteger.Create;
  try
    CheckEquals('0', LOID.NullOIDAsString);
  finally
    LOID.Free;
  end;
end;

procedure TTestTIOIDInteger.SetUp;
begin
  inherited Setup;
  FOIDClass := TOIDInteger;
end;


{ TTestTIOIDGUID }

procedure TTestTIOIDGUID.Assign;
var
  lOID1: TtiOID;
  lOID2: TtiOID;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := FGuidAsString;
      lOID2.Assign(lOID1);
      CheckEquals(lOID1.AsString, lOID2.AsString);
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOIDGUID.AsString;
var
  lOID: TtiOID;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsString := FGuidAsString;
    CheckEquals(FGuidAsString, lOID.AsString, 'Failed on 1');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDGUID.AsVariant;
var
  lOID: TtiOID;
  lStr: string;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsVariant := FGuidAsString;
    CheckEquals(FGuidAsString, lOID.AsString, 'Failed on 1');
    lStr := lOID.AsVariant;
    CheckEquals(FGuidAsString, lStr, 'Failed on 2');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDGUID.NextOIDGeneratorClass;
var
  LOIDGenerator: TtiOIDGenerator;
begin
  LOIDGenerator := TtiOIDGeneratorGUID.Create;
  try
    Check(LOIDGenerator.OIDClass = TOIDGUID);
  finally
    LOIDGenerator.Free;
  end;
end;

procedure TTestTIOIDGUID.IsNullAndSetToNull;
var
  lOID: TtiOID;
begin
  lOID := FOIDClass.Create;
  try
    Check(lOID.IsNull, 'Failed on 1');
    lOID.AsVariant := FGuidAsString;
    Check(not lOID.IsNull, 'Failed on 2');
    lOID.SetToNull;
    Check(lOID.IsNull, 'Failed on 3');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDGUID.NullOIDAsString;
var
  LOID: TtiOID;
begin
  LOID := TOIDGUID.Create;
  try
    CheckEquals('', LOID.NullOIDAsString);
  finally
    LOID.Free;
  end;
end;

procedure TTestTIOIDGUID.SetUp;
begin
  inherited;
  FOIDClass     := TOIDGUID;
  FGuidAsString := tiCreateGUIDString;
end;

procedure TTestTIOIDGUID.SetupOnce;
begin
  inherited;
  {$IFDEF MSWINDOWS}
  tiWin32CoInitialize; // An attempt to suppress a leak identified by DUnit2
  tiWin32CoUnInitialize;
  {$ENDIF}
end;

procedure TTestTIOIDGUID.TearDown;
begin
  inherited;
  FGuidAsString := '';
  {$IFDEF MSWINDOWS}
  tiWin32CoUnInitialize;
  {$ENDIF}
end;

{ TTestTIOIDString }

procedure TTestTIOIDString.Assign;
var
  lOID1: TtiOID;
  lOID2: TtiOID;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := 'test';
      lOID2.Assign(lOID1);
      CheckEquals(lOID1.AsString, lOID2.AsString);
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;


procedure TTestTIOIDString.AsString;
var
  lOID: TtiOID;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsString := 'test';
    CheckEquals('test', lOID.AsString, 'Failed on 1');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDString.AsVariant;
var
  lOID: TtiOID;
  lStr: string;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsVariant := 'test';
    CheckEquals('test', lOID.AsString, 'Failed on 1');
    lStr := lOID.AsVariant;
    CheckEquals('test', lStr, 'Failed on 2');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDString.NextOIDGeneratorClass;
var
  LOIDGenerator: TtiOIDGenerator;
begin
  LOIDGenerator := TtiOIDGeneratorString.Create;
  try
    Check(LOIDGenerator.OIDClass = TOIDString);
  finally
    LOIDGenerator.Free;
  end;
end;

procedure TTestTIOIDString.IsNullAndSetToNull;
var
  lOID: TtiOID;
begin
  lOID := FOIDClass.Create;
  try
    Check(lOID.IsNull, 'Failed on 1');
    lOID.AsVariant := 'test';
    Check(not lOID.IsNull, 'Failed on 2');
    lOID.SetToNull;
    Check(lOID.IsNull, 'Failed on 3');
  finally
    lOID.Free;
  end;
end;


procedure TTestTIOIDString.NullOIDAsString;
var
  LOID: TtiOID;
begin
  LOID := TOIDString.Create;
  try
    CheckEquals('', LOID.NullOIDAsString);
  finally
    LOID.Free;
  end;
end;

procedure TTestTIOIDString.SetUp;
begin
  inherited;
  FOIDClass := TOIDString;
end;

procedure TTestTIOIDPersistent.Setup;
begin
  inherited Setup;
  // Number of times to repeat NextOID test
  // Set a high number for thorough testing (eg, 100000)
  // Set a low number for quick testing (eg, 100)
  FRepeatCount:= 100;
  // 3 is the minimum for testing, 10 will give a more indepth test
  FNumThreads:= 10;
  FOIDList := TStringList.Create;
end;

procedure TTestTIOIDPersistent.TearDown;
begin
  FOIDList.Free;
  inherited TearDown;
end;

procedure TTestTIOIDPersistent.TestThenAddOIDAsString(const AOID: string);
begin
  if FOIDList.IndexOf(AOID) <> -1 then
    raise Exception.CreateFmt('Duplicate OID "%s"', [AOID])
  else
    FOIDList.Add(AOID);
end;

type
  TtiOIDGeneratorThread = class(TtiThread)
  private
    FOIDGeneratorClass: TtiOIDGeneratorClass;
    FOIDList: TStringList;
    FRepeateCount: integer;
    FIndex: integer;
    FDBConnectionName: string;
    FPersistenceLayerName: string;
  public
    constructor Create(const AOIDGeneratorClass: TtiOIDGeneratorClass; const AIndex: integer;
      const ARepeatCount: integer; const ADatabaseAliasName: string; const APersistenceLayerName: string);
    destructor Destroy; override;
    procedure Execute; override;
    property OIDList: TStringList read FOIDList;
    property Index: integer read FIndex;
  end;

constructor TtiOIDGeneratorThread.Create(const AOIDGeneratorClass: TtiOIDGeneratorClass;
  const AIndex: integer; const ARepeatCount: integer; const ADatabaseAliasName: string;
  const APersistenceLayerName: string);
begin
  inherited Create(True);
  FOIDGeneratorClass := AOIDGeneratorClass;
  FIndex          := AIndex;
  FOIDList        := TStringList.Create;
  FreeOnTerminate := False;
  FRepeateCount   := ARepeatCount;
  FDBConnectionName := ADatabaseAliasName;
  FPersistenceLayerName := APersistenceLayerName;
end;

destructor TtiOIDGeneratorThread.Destroy;
begin
  FOIDList.Free;
  inherited;
end;

procedure TtiOIDGeneratorThread.Execute;
var
  LNextOIDGenerator: TtiOIDGenerator;
  LOID: TtiOID;
  i:    integer;
begin
  Assert(Assigned(FOIDGeneratorClass), 'FOIDGeneratorClass not assigned');
  LNextOIDGenerator := FOIDGeneratorClass.Create;
  try
    for i := 1 to FRepeateCount do
    begin
      LOID := FOIDGeneratorClass.OIDClass.Create;
      try
        LNextOIDGenerator.AssignNextOID(LOID, FDBConnectionName, FPersistenceLayerName);
        FOIDList.Add(LOID.AsString);
      finally
        LOID.Free;
      end;
    end;
  finally
    LNextOIDGenerator.Free;
  end;
end;


procedure TTestTIOIDPersistent.TtiNextOIDGeneratorAssignNextOIDMultiUser;

  procedure _CheckForDuplicates(const ATestCase: TtiTestCase; const AThread: TtiOIDGeneratorThread);
  var
    i:    integer;
    LOID: string;
  begin
    for i := 0 to AThread.OIDList.Count - 1 do
    begin
      LOID := AThread.OIDList.Strings[i];
      if FOIDList.IndexOf(LOID) <> -1 then
        Check(False, 'Duplicate OID "%s". Thread "%d", Index "%d"',
          [LOID, AThread.Index, i])
      else
        FOIDList.Add(LOID);
    end;
  end;

var
  LList:       TtiThreadList;
  i:           integer;
begin
  if PersistenceLayerSupportsMultiUser then
  begin
    LList       := TtiThreadList.Create;
    try
      for i := 0 to FNumThreads - 1 do
        LList.Add(TtiOIDGeneratorThread.Create(FOIDGeneratorClass, i, FRepeatCount, DatabaseName, PersistenceLayerName));
      LList.StartAll;
      LList.WaitForAll;
      for i := 0 to LList.Count - 1 do
        _CheckForDuplicates(Self, LList.Items[i] as TtiOIDGeneratorThread);
    finally
      LList.Free;
    end;
  end;
  Check(True); // Suppress DUnit2 warning
end;

procedure TTestTIOIDPersistent.TtiNextOIDGeneratorAssignNextOIDSingleUser;
var
  LNextOIDGenerator: TtiOIDGenerator;
  LOID: TtiOID;
  i:    integer;
begin
  SetAllowedLeakArray([32, 56]); // CoInitialize

  Assert(Assigned(FOIDGeneratorClass), 'FOIDGeneratorClass not assigned');
  LNextOIDGenerator := FOIDGeneratorClass.Create;
  try
    for i := 10 to FRepeatCount do
    begin
      LOID := FOIDGeneratorClass.OIDClass.Create;
      try
        CheckEquals(LOID.NullOIDAsString, LOID.AsString);
        LNextOIDGenerator.AssignNextOID(LOID, DatabaseName, PersistenceLayerName);
        CheckNotEquals(LOID.NullOIDAsString, LOID.AsString);
        TestThenAddOIDAsString(LOID.AsString);
      finally
        LOID.Free;
      end;
    end;
    FOIDList.Clear;
  finally
    LNextOIDGenerator.Free;
  end;
end;

procedure TTestTIOIDPersistent.TtiNextOIDGeneratorAssignNextOIDThreaded;
var
  LThread: TtiOIDGeneratorThread;
begin
  if PersistenceLayerSupportsMultiUser then
  begin
    SetAllowedLeakArray([32, 56]); // CoInitialize
    LThread:= TtiOIDGeneratorThread.Create(FOIDGeneratorClass, 0, FRepeatCount, DatabaseName, PersistenceLayerName);
    try
      LThread.Start;
      LThread.WaitFor;
    finally
      LThread.Free;
    end;
  end;
  Check(True); // Suppress DUnit2 warning
end;

procedure TTestTIOIDPersistent.TtiOIDAssignToTIQueryParam;
var
  LOID:         TtiOID;
  LQueryParams: TtiQueryParams;
begin
  LOID         := nil;
  LQueryParams := nil;
  try
    LOID := FOIDGeneratorClass.OIDClass.Create;
    LQueryParams := TtiQueryParams.Create;
    CheckEquals(LOID.NullOIDAsString, LOID.AsString);
    LOID.AssignToTIQueryParam('oid', LQueryParams);
    CheckEquals(1, LQueryParams.Count);
    CheckNotNull(LQueryParams.FindParamByName('oid'));
    CheckEquals(LOID.NullOIDAsString, LQueryParams.Items[0].ValueAsString);
  finally
    LOID.Free;
    LQueryParams.Free;
  end;
end;

procedure TTestTIOIDPersistent.TtiOIDAssignToTIQuery;
var
  LOID:      TtiOID;
  LQuery:    TtiQuery;
  LDatabase: TtiDatabase;
  LActual:   string;
const
  CExpected = '123';
begin
  // Only run this test against SQL databases
  if not PersistenceLayerSupportsSQL then
  begin
    Check(True);
    Exit; //==>
  end;

  LOID := FOIDGeneratorClass.OIDClass.Create;
  try
    CheckEquals(LOID.NullOIDAsString, LOID.AsString);
    LDatabase := PersistenceLayer.DBConnectionPools.Lock(DatabaseName);
    try
      LDatabase.StartTransaction;
      try
        LQuery := LDatabase.CreateTIQuery;
        try
          LQuery.AttachDatabase(LDatabase);
          LQuery.SQLText := 'update Next_OID set OID = :OID';
          LOID.AsString  := CExpected;
          LOID.AssignToTIQuery(LQuery);
          LQuery.ExecSQL;
          LQuery.SQLText := 'select OID from Next_OID';
          LQuery.Active  := True;
          LActual        := IntToStr(LQuery.FieldAsInteger['OID']);
          CheckEquals(CExpected, LActual);
        finally
          LQuery.Free;
        end;
      finally
        LDatabase.Rollback;
      end;
    finally
      PersistenceLayer.DBConnectionPools.UnLock(DatabaseName, LDatabase);
    end;
  finally
    LOID.Free;
  end;
end;

procedure TTestTIOIDPersistent.TtiOIDAssignFromTIQuery;
var
  LOID:      TtiOID;
  LQuery:    TtiQuery;
  LDatabase: TtiDatabase;
begin
  LOID := FOIDGeneratorClass.OIDClass.Create;
  try
    CheckEquals(LOID.NullOIDAsString, LOID.AsString);
    LDatabase := PersistenceLayer.DBConnectionPools.Lock(DatabaseName);
    try
      LDatabase.StartTransaction;
      try
        LQuery := LDatabase.CreateTIQuery;
        try
          LQuery.AttachDatabase(LDatabase);
          LQuery.SelectRow('Next_OID', nil);
          LOID.AssignFromTIQuery(LQuery);
          Check(not LQuery.EOF);
          CheckNotEquals(LOID.NullOIDAsString, LOID.AsString);
          CheckEquals(LQuery.FieldAsString['OID'], LOID.AsString);
        finally
          LQuery.Free;
        end;
      finally
        LDatabase.Rollback;
      end;
    finally
      PersistenceLayer.DBConnectionPools.UnLock(DatabaseName, LDatabase);
    end;
  finally
    LOID.Free;
  end;
end;

procedure TTestTIOIDPersistent.TtiOIDAssignToTIQueryFieldName;
var
  LOID:      TtiOID;
  LQuery:    TtiQuery;
  LDatabase: TtiDatabase;
  LActual:   string;
const
  CExpected = '123';
begin
  // Only run this test against SQL databases
  if not PersistenceLayerSupportsSQL then
  begin
    Check(True);
    Exit; //==>
  end;

  LOID := FOIDGeneratorClass.OIDClass.Create;
  try
    CheckEquals(LOID.NullOIDAsString, LOID.AsString);
    LDatabase := PersistenceLayer.DBConnectionPools.Lock(DatabaseName);
    try
      LDatabase.StartTransaction;
      try
        LQuery := LDatabase.CreateTIQuery;
        try
          LQuery.AttachDatabase(LDatabase);
          LQuery.SQLText := 'update Next_OID set OID = :newoid';
          LOID.AsString  := CExpected;
          LOID.AssignToTIQuery('newoid', LQuery);
          LQuery.ExecSQL;
          LQuery.SQLText := 'select OID from Next_OID';
          LQuery.Active  := True;
          LActual        := IntToStr(LQuery.FieldAsInteger['OID']);
          CheckEquals(CExpected, LActual);
        finally
          LQuery.Free;
        end;
      finally
        LDatabase.Rollback;
      end;
    finally
      PersistenceLayer.DBConnectionPools.UnLock(DatabaseName, LDatabase);
    end;
  finally
    LOID.Free;
  end;
end;

procedure TTestTIOIDPersistent.TtiOIDAssignFromTIQueryFieldName;
var
  LOID:      TtiOID;
  LQuery:    TtiQuery;
  LDatabase: TtiDatabase;
begin
  LOID := FOIDGeneratorClass.OIDClass.Create;
  try
    CheckEquals(LOID.NullOIDAsString, LOID.AsString);
    LDatabase := PersistenceLayer.DBConnectionPools.Lock(DatabaseName);
    try
      LDatabase.StartTransaction;
      try
        LQuery := LDatabase.CreateTIQuery;
        try
          LQuery.AttachDatabase(LDatabase);
          LQuery.SelectRow('Next_OID', nil);
          LOID.AssignFromTIQuery('OID', LQuery);
          Check(not LQuery.EOF);
          CheckNotEquals(LOID.NullOIDAsString, LOID.AsString);
          CheckEquals(LQuery.FieldAsString['OID'], LOID.AsString);
        finally
          LQuery.Free;
        end;
      finally
        LDatabase.Rollback;
      end;
    finally
      PersistenceLayer.DBConnectionPools.UnLock(DatabaseName, LDatabase);
    end;
  finally
    LOID.Free;
  end;
end;

procedure TTestTIOIDPersistent.TtiOIDEqualsQueryField;
var
  LOID:      TtiOID;
  LQuery:    TtiQuery;
  LDatabase: TtiDatabase;
begin
  LOID := FOIDGeneratorClass.OIDClass.Create;
  try
    LDatabase := PersistenceLayer.DBConnectionPools.Lock(DatabaseName);
    try
      LDatabase.StartTransaction;
      try
        LQuery := LDatabase.CreateTIQuery;
        try
          LQuery.AttachDatabase(LDatabase);
          LQuery.SelectRow('Next_OID', nil);
          LOID.AssignFromTIQuery(LQuery);
          Check(LOID.EqualsQueryField('OID', LQuery));
          LOID.AsString := '-1';
          Check(not LOID.EqualsQueryField('OID', LQuery));
        finally
          LQuery.Free;
        end;
      finally
        LDatabase.RollBack;
      end;
    finally
      PersistenceLayer.DBConnectionPools.UnLock(DatabaseName, LDatabase);
    end;
  finally
    LOID.Free;
  end;
end;

procedure TTestTIOIDPersistent.TtiObjectCreateNew;
var
  LObject: TtiObject;
  LDefaultPersistenceLayerName: string;
  LDefaultDBConnectionName: string;
begin
  SetAllowedLeakArray([32, 56, 88]); // CoInitialize
  LDefaultPersistenceLayerName:= GTIOPFManager.DefaultPersistenceLayerName;
  LDefaultDBConnectionName:= GTIOPFManager.DefaultDBConnectionName;

  GTIOPFManager.DefaultPersistenceLayerName:= PersistenceLayerName;
  GTIOPFManager.DefaultDBConnectionName:= DatabaseName;
  try
    GTIOPFManager.DefaultOIDGenerator := FOIDGeneratorClass.Create;
    try

      LObject := TtiObject.Create;
      try
        CheckEquals(LObject.OID.NullOIDAsString, LObject.OID.AsString);
      finally
        LObject.Free;
      end;

      LObject := TtiObject.CreateNew(nil, DatabaseName, PersistenceLayerName);
      try
        CheckNotEquals(LObject.OID.NullOIDAsString, LObject.OID.AsString);
      finally
        LObject.Free;
      end;

    finally
      GTIOPFManager.DefaultOIDGenerator := GTIOPFTestManager.DefaultOIDGeneratorClass.Create;
    end;
  finally
    GTIOPFManager.DefaultPersistenceLayerName:= LDefaultPersistenceLayerName;
    GTIOPFManager.DefaultDBConnectionName:= LDefaultDBConnectionName;
  end;
end;

{ TTestTIOIDPersistentInteger }

procedure TTestTIOIDPersistentInteger.Setup;
begin
  inherited;
  FOIDGeneratorClass := TtiOIDGeneratorInteger;
  CreateNextOIDIntTable;
end;

{ TTestTIOIDPersistentGUID }

procedure TTestTIOIDPersistentGUID.Setup;
begin
  inherited Setup;
  CreateNextOIDStrTable; // Required here for testing persistence of GUID OID
  FOIDGeneratorClass    := TtiOIDGeneratorGUID;
end;

procedure TTestTIOIDPersistent.CreateNextOIDIntTable;
var
  LTable : TtiDBMetaDataTable;
  lParams : TtiQueryParams;
begin
  { TODO : For some reason the Next_OID table hangs around then this method fails. Why?  - graeme }
  DropTable('Next_OID');

  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := 'Next_OID';
    LTable.AddInstance('OID', qfkInteger);
    CreateTable(LTable);
  finally
    LTable.Free;
  end;

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsInteger('OID', 1000000);
    InsertRow('Next_OID', lParams);
  finally
    lParams.Free;
  end;
end;


procedure TTestTIOIDPersistent.CreateNextOIDStrTable;
var
  LTable : TtiDBMetaDataTable;
  lParams : TtiQueryParams;
begin
  { TODO : For some reason the Next_OID table hangs around then this method fails. Why?  - graeme }
  DropTable('Next_OID');

  LTable := TtiDBMetaDataTable.Create;
  try
    LTable.Name := 'Next_OID';
    LTable.AddInstance('OID', qfkString, 10);
    CreateTable(LTable);
  finally
    LTable.Free;
  end;

  lParams := TtiQueryParams.Create;
  try
    lParams.SetValueAsString('OID', '0');
    InsertRow('Next_OID', lParams);
  finally
    lParams.Free;
  end;
end;

procedure TTestTIOIDPersistentGUID.TtiNextOIDGeneratorAssignNextOIDMultiUser;
begin
  AllowedMemoryLeakSize := 32;  //Sneaky way to get 4 values
  SetAllowedLeakArray([208, 265, 376]);
  inherited TtiNextOIDGeneratorAssignNextOIDMultiUser;
end;

procedure TTestTIOIDPersistentGUID.TtiNextOIDGeneratorAssignNextOIDSingleUser;
begin
  SetAllowedLeakArray([32, 208, 265]);
  inherited;
end;

procedure TTestTIOIDPersistentGUID.TtiNextOIDGeneratorAssignNextOIDThreaded;
begin
  SetAllowedLeakArray([32, 208, 265]);
  inherited TtiNextOIDGeneratorAssignNextOIDThreaded;
end;

{ TTestTIOIDInt64 }

procedure TTestTIOIDInt64.SetUp;
begin
  inherited SetUp;
  FOIDClass := TOIDInt64;
end;

procedure TTestTIOIDInt64.NextOIDGeneratorClass;
var
  LOIDGenerator: TtiOIDGenerator;
begin
  LOIDGenerator := TtiOIDGeneratorInt64.Create;
  try
    Check(LOIDGenerator.OIDClass = TOIDInt64);
  finally
    LOIDGenerator.Free;
  end;
end;

procedure TTestTIOIDInt64.AsString;
const
  cHighInt64 = '9223372036854775807';
var
  lOID: TtiOID;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsString := '1';
    CheckEquals('1', lOID.AsString, 'Failed on 1');
    lOID.AsString := cHighInt64;
    CheckEquals(cHighInt64, lOID.AsString, 'Failed on 2');
  finally
    lOID.Free;
  end;
end;

procedure TTestTIOIDInt64.AsVariant;
var
  lOID: TtiOID;
  lInt: Int64;
  lStr: string;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsVariant := '1';
    CheckEquals('1', lOID.AsString, 'Failed on AsString');
    lStr := lOID.AsVariant;
    CheckEquals('1', lStr, 'Failed on AsVariant #1');
    lInt := lOID.AsVariant;
    CheckEquals(1, lInt, 'Failed on AsVariant #2');
  finally
    lOID.Free;
  end;
end;

procedure TTestTIOIDInt64.NullOIDAsString;
var
  LOID: TtiOID;
begin
  LOID := TOIDInt64.Create;
  try
    CheckEquals('0', LOID.NullOIDAsString);
  finally
    LOID.Free;
  end;
end;

{ TTestTIOIDIntBase }

procedure TTestTIOIDIntBase.Assign;
var
  lOID1: TtiOID;
  lOID2: TtiOID;
begin
  lOID1 := FOIDClass.Create;
  try
    lOID2 := FOIDClass.Create;
    try
      lOID1.AsVariant := 1;
      lOID2.Assign(lOID1);
      CheckEquals(lOID1.AsString, lOID2.AsString);
    finally
      lOID2.Free;
    end;
  finally
    lOID1.Free;
  end;
end;

procedure TTestTIOIDIntBase.AsString;
var
  lOID: TtiOID;
begin
  lOID := FOIDClass.Create;
  try
    lOID.AsString := '1';
    CheckEquals('1', lOID.AsString, 'Failed on AsString');
  finally
    lOID.Free;
  end;
end;

procedure TTestTIOIDIntBase.IsNullAndSetToNull;
var
  lOID: TtiOID;
begin
  lOID := FOIDClass.Create;
  try
    Check(lOID.IsNull, 'Failed on IsNull');
    lOID.AsVariant := 1;
    Check(not lOID.IsNull, 'Failed on not IsNull');
    lOID.SetToNull;
    Check(lOID.IsNull, 'Failed on SetToNull');
  finally
    lOID.Free;
  end;
end;

end.
