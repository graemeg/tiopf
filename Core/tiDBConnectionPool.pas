unit tiDBConnectionPool;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiObject
  ,tiQuery
  ,tiPool
  ,Classes
  ,Contnrs
  ,SysUtils
  ,SyncObjs
 ;

const
  cErrorAttemptToAddDuplicateDBConnectionPool = 'Attempt to register a duplicate database connection: "%s"';
  cErrorUnableToFindDBConnectionPool = 'Attempt to lock a database connection for a database that has not been registered: "%s"';

type

  TtiDBConnectionPools   = class;
  TtiDBConnectionPool   = class;

  TtiDBConnectionPoolDataAbs = class(TtiBaseObject)
  private
    FDBConnectionPool: TtiDBConnectionPool;
  public
    property  DBConnectionPool : TtiDBConnectionPool read FDBConnectionPool write FDBConnectionPool;
    procedure InitDBConnectionPool; virtual;
  end;

  TtiDBConnectionPoolDataClass = class of TtiDBConnectionPoolDataAbs;

  // The database connection pool
  TtiDBConnectionPool = class(TtiPool)
  private
    FDBConnectionPools: TtiDBConnectionPools;
    FDBConnectionParams : TtiDBConnectionParams;
    FDatabaseAlias: string;
    procedure Init;

  protected
    function    PooledItemClass: TtiPooledItemClass; override;
    procedure   AfterAddPooledItem(const APooledItem: TtiPooledItem); override;
    property    DBConnectionPools: TtiDBConnectionPools read FDBConnectionPools;
  public
    constructor Create(const ADBConnectionPools: TtiDBConnectionPools;
      const ADatabaseAlias: string; const ADBConnectionParams: TtiDBConnectionParams);
    destructor  Destroy; override;
    property    DBConnectParams : TtiDBConnectionParams read FDBConnectionParams;
    property    DatabaseAlias: string read FDatabaseAlias;
    function    Lock : TtiDatabase; reintroduce;
    procedure   UnLock(const ADatabase: TtiDatabase); reintroduce;
    function    DetailsAsString : string;

  end;

  // Maintains a list of TDBConnectionPool(s) so a single app can connect to
  // multiple databases.
  TtiDBConnectionPools = class(TtiBaseObject)
  private
    FPersistenceLayer: TtiBaseObject;
    FList : TObjectList;
    FCritSect: TCriticalSection;
    function GetItems(i: integer): TtiDBConnectionPool;
  public
    Constructor Create(const APersistenceLayer: TtiBaseObject);
    Destructor  Destroy; override;
    function    Lock(      const ADatabaseAlias : string): TtiDatabase; reintroduce;
    procedure   UnLock(      const ADatabaseAlias : string; const ADatabase : TtiDatabase); reintroduce;
    procedure   Connect(     const ADatabaseAlias, ADatabaseName, AUserName, psUserPassword : string; const AParams : string);
    procedure   AddInstance( const ADatabaseAlias, ADatabaseName, AUserName, psUserPassword : string; const AParams : string);
    function    Find(        const ADatabaseAlias : string): TtiDBConnectionPool; reintroduce;
    procedure   Disconnect(  const ADatabaseAlias : string);
    procedure   DisconnectAll;

    function    DetailsAsString : string;
    procedure   Clear;
    function    IsConnected(const ADatabaseAlias : string): boolean;
    function    Count : integer;
    property    Items[i:integer]:TtiDBConnectionPool read GetItems;

    property    PersistenceLayer: TtiBaseObject read FPersistenceLayer;
  end;


implementation
uses
   tiLog
  ,tiUtils
  ,tiOPFManager
  ,tiPersistenceLayers
  ,tiConstants
  ,tiExcept
 ;

const

  cusParamDelim   =   '|';

  cusConnectionNames = 'ConnectionNames';
  cusFileName        = 'DBParams.DCD';

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TDBConnectParams
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBConnectionPool.AfterAddPooledItem(const APooledItem: TtiPooledItem);
var
  LPersistenceLayer: TtiPersistenceLayer;
  LDatabase: TtiDatabase;
begin
  LPersistenceLayer := (DBConnectionPools.PersistenceLayer as TtiPersistenceLayer);
  LDatabase:= LPersistenceLayer.tiDatabaseClass.Create;
  APooledItem.Data:= LDatabase;
  LDatabase.Connect(
    DBConnectParams.DatabaseName,
    DBConnectParams.UserName,
    DBConnectParams.UserPassword,
    DBConnectParams.Params);
end;

constructor TtiDBConnectionPool.Create(
  const ADBConnectionPools: TtiDBConnectionPools;
  const ADatabaseAlias: string;
  const ADBConnectionParams: TtiDBConnectionParams);
begin
  inherited Create;
  FDBConnectionPools:= ADBConnectionPools;
  FDatabaseAlias:= ADatabaseAlias;
  FDBConnectionParams:= ADBConnectionParams;
  Init;
end;

destructor TtiDBConnectionPool.Destroy;
begin
  inherited;
end;

function TtiDBConnectionPool.Lock: TtiDatabase;
begin
  result := TtiDatabase(inherited Lock);
end;

type
  TtiPooledDatabase = class(TtiPooledItem)
  public
    function MustRemoveItemFromPool(AListCount: Integer): boolean; override;
  end;

  function TtiPooledDatabase.MustRemoveItemFromPool(AListCount: Integer): Boolean;
  begin
    Assert(Data.TestValid(TtiDatabase), cTIInvalidObjectError);
    result :=
      (Inherited MustRemoveItemFromPool(AListCount)) or
      ((Data as TtiDatabase).ErrorInLastCall);
  end;

function TtiDBConnectionPool.PooledItemClass: TtiPooledItemClass;
begin
  result:= TtiPooledDatabase;
end;

procedure TtiDBConnectionPool.UnLock(const ADatabase: TtiDatabase);
begin
  Assert(ADatabase.TestValid, cTIInvalidObjectError);
  Assert(not ADatabase.InTransaction, 'Database in transaction immediately before being unlocked in DBConnectionPool.');
  inherited UnLock(ADatabase);
end;

procedure TtiDBConnectionPoolDataAbs.InitDBConnectionPool;
begin
  // Do nothing, implement in the concrete
end;

procedure TtiDBConnectionPool.Init;
var
  lRegPerLayer : TtiPersistenceLayer;
  lDBConnectionPoolData : TtiDBConnectionPoolDataAbs;
begin
  lRegPerLayer := (DBConnectionPools.PersistenceLayer as TtiPersistenceLayer);
  lDBConnectionPoolData := lRegPerLayer.tiDBConnectionPoolDataClass.Create;
  try
    lDBConnectionPoolData.DBConnectionPool := self;
    lDBConnectionPoolData.InitDBConnectionPool;
  finally
    lDBConnectionPoolData.Free;
  end;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TDBConnectionPools
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDBConnectionPools.Create(const APersistenceLayer: TtiBaseObject);
begin
  inherited Create;
  Assert(APersistenceLayer.TestValid(TtiPersistenceLayer, True), cTIInvalidObjectError);
  FPersistenceLayer:= APersistenceLayer;
  FList := TObjectList.Create;
  FCritSect:= TCriticalSection.Create;
end;

destructor TtiDBConnectionPools.Destroy;
begin
  FList.Free;
  FCritSect.Free;
  inherited;
end;

procedure TtiDBConnectionPools.Connect(const ADatabaseAlias, ADatabaseName,
  AUserName, psUserPassword, AParams: string);
var
  lDBConnectionPool : TtiDBConnectionPool;
  LDBConnectionParams: TtiDBConnectionParams;
  LDatabase: TtiDatabase;
begin
  LDBConnectionParams.DatabaseName:= ADatabaseName;
  LDBConnectionParams.UserName:= AUserName;
  LDBConnectionParams.Params:= AParams;

  FCritSect.Enter;
  try
    lDBConnectionPool := Find(ADatabaseAlias);
    if lDBConnectionPool <> nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorAttemptToAddDuplicateDBConnectionPool, [ADatabaseName + '/' + AUserName]);
    Log('Creating database connection pool for %s/%s', [ADatabaseName, AUserName], lsConnectionPool);
    lDBConnectionPool := TtiDBConnectionPool.Create(Self, ADatabaseAlias, LDBConnectionParams);
    try
      LDatabase:= LDBConnectionPool.Lock;
      LDBConnectionPool.Unlock(LDatabase);
      FList.Add(lDBConnectionPool);
    except
      on e:exception do
      begin
        lDBConnectionPool.Free;
        Raise;
      end;
    end;
  finally
    FCritSect.Leave;
  end;
end;

function TtiDBConnectionPools.Find(const ADatabaseAlias: string): TtiDBConnectionPool;
var
  i : integer;
begin
  // ToDo: No thread protection here, because find may be called from inside a
  //       block that is already protected
  for i := 0 to FList.Count - 1 do
    if SameText(Items[i].DatabaseAlias, ADatabaseAlias) then
    begin
      result := Items[i];
      Exit; //==>
    end;
  result := nil;
end;

function TtiDBConnectionPools.Lock(const ADatabaseAlias: string): TtiDatabase;
var
  LDBConnectionPool : TtiDBConnectionPool;
begin
  result:= nil;
  FCritSect.Enter;
  try
    LDBConnectionPool := Find(ADatabaseAlias);
    if LDBConnectionPool = nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [ADatabaseAlias]);
    result := LDBConnectionPool.Lock;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiDBConnectionPools.UnLock(const ADatabaseAlias: string;
  const ADatabase : TtiDatabase);
var
  LDBConnectionPool : TtiDBConnectionPool;
begin
  FCritSect.Enter;
  try
    LDBConnectionPool := Find(ADatabaseAlias);
    Assert(LDBConnectionPool.TestValid(TtiDBConnectionPool, True), cErrorTIPerObjAbsTestValid);
    if LDBConnectionPool = nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [ADatabaseAlias]);
    LDBConnectionPool.UnLock(ADatabase);
  finally
    FCritSect.Leave;
  end;
end;


function TtiDBConnectionPools.DetailsAsString: string;
var
  i : integer;
begin
  FCritSect.Enter;
  try
    result := '';
    // This is not thread safe!
    for i := 0 to FList.Count - 1 do
    begin
      result := tiAddTrailingValue(result, CrLf(2), true);
      result := result + TtiDBConnectionPool(FList.Items[i]).DetailsAsString;
    end;
  finally
    FCritSect.Leave;
  end;
end;

function TtiDBConnectionPool.DetailsAsString: string;
var
  LPersistenceLayer : TtiPersistenceLayer;
begin
  Assert(DBConnectionPools.TestValid(TtiDBConnectionPools), cTIInvalidObjectError);
  Assert(DBConnectionPools.PersistenceLayer.TestValid(TtiPersistenceLayer), cTIInvalidObjectError);
  LPersistenceLayer := DBConnectionPools.PersistenceLayer as TtiPersistenceLayer;
  result :=
    'Persistence layer:   ' + LPersistenceLayer.PerLayerName + Cr +
    'Database name:       ' + DBConnectParams.DatabaseName + Cr +
    'User name:           ' + DBConnectParams.UserName     + Cr +
    'Password:            ' + tiReplicate('X', Length(DBConnectParams.UserPassword)) + Cr +
    'Number in pool:      ' + IntToStr(Count);
end;

procedure TtiDBConnectionPools.AddInstance(const ADatabaseAlias, ADatabaseName,
  AUserName, psUserPassword, AParams: string);
var
  lDBConnectionPool : TtiDBConnectionPool;
  LDBConnectionParams: TtiDBConnectionParams;
begin
  LDBConnectionParams.DatabaseName:= ADatabaseName;
  LDBConnectionParams.UserName:= AUserName;
  LDBConnectionParams.Params:= AParams;
  FCritSect.Enter;
  try
    lDBConnectionPool := Find(ADatabaseAlias);
    if lDBConnectionPool <> nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorAttemptToAddDuplicateDBConnectionPool, [ADatabaseName + '/' + AUserName]);
    lDBConnectionPool := TtiDBConnectionPool.Create(Self, ADatabaseAlias, LDBConnectionParams);
    FList.Add(lDBConnectionPool);
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiDBConnectionPools.Clear;
begin
  FCritSect.Enter;
  try
  FList.Clear;
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiDBConnectionPools.Disconnect(const ADatabaseAlias: string);
var
  lDBConnectionPool : TtiDBConnectionPool;
begin
  FCritSect.Enter;
  try
    lDBConnectionPool := Find(ADatabaseAlias);
    if lDBConnectionPool =  nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [ADatabaseAlias]);
    FList.Extract(lDBConnectionPool);
    lDBConnectionPool.Free;
  finally
    FCritSect.Leave;
  end;
end;

function TtiDBConnectionPools.IsConnected(const ADatabaseAlias: string): boolean;
begin
  result := (Find(ADatabaseAlias) <> nil);
end;

function TtiDBConnectionPools.Count: integer;
begin
  FCritSect.Enter;
  try
  result := FList.Count;
  finally
    FCritSect.Leave;
  end;
end;

function TtiDBConnectionPools.GetItems(i: integer): TtiDBConnectionPool;
begin
  result := TtiDBConnectionPool(FList.Items[i])
end;

procedure TtiDBConnectionPools.DisconnectAll;
var
  i : integer;
begin
  for i := Count - 1 downto 0 do
    Disconnect(Items[i].DBConnectParams.DatabaseName);
end;

end.



