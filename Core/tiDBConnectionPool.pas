unit tiDBConnectionPool;

{$I tiDefines.inc}

interface
uses
  tiBaseObject,
  tiQuery,
  tiPool,
  Classes
{$IFDEF IOS}
  ,System.Generics.Defaults
  ,Generics.Collections
{$ELSE}
  ,Contnrs
{$ENDIF IOS}
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
  end;

  // The database connection pool
  TtiDBConnectionPool = class(TtiPool)
  private
    FDBConnectionPools: TtiDBConnectionPools;
    FDBConnectionParams : TtiDBConnectionParams;
    FDatabaseAlias: string;

  protected
    function    PooledItemClass: TtiPooledItemClass; override;
    procedure   AfterAddPooledItem(const APooledItem: TtiPooledItem); override;
    property    DBConnectionPools: TtiDBConnectionPools read FDBConnectionPools;
    procedure   InitToMinPoolSize;
  public
    constructor Create(const ADBConnectionPools: TtiDBConnectionPools;
      const ADatabaseAlias: string;
      const ADBConnectionParams: TtiDBConnectionParams;
      const AMinPoolSize, AMaxPoolSize, APoolTimeOut: integer);
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
{$IFDEF IOS}
    FList : TObjectList<TtiDBConnectionPool>;
{$ELSE}
    FList : TObjectList;
{$ENDIF IOS}
    FCritSect: TCriticalSection;
    function GetItems(i: integer): TtiDBConnectionPool;
    procedure GetPoolSizeDefaults(var AMinPoolSize, AMaxPoolSize, APoolTimeOut: integer);
  public
    Constructor Create(const APersistenceLayer: TtiBaseObject);
    Destructor  Destroy; override;
    function    Lock(      const ADatabaseAlias : string): TtiDatabase; reintroduce;
    procedure   UnLock(      const ADatabaseAlias : string; const ADatabase : TtiDatabase); reintroduce;
    procedure   Connect(     const ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams : string); overload;
    procedure   Connect(     const ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams, AQueryOptions : string; const AMinPoolSize, AMaxPoolSize, APoolTimeOut: integer); overload;
    procedure   AddInstance(
      const ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams : string); overload;
    procedure   AddInstance(
      const ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams : string;
      const AMinPoolSize, AMaxPoolSize, APoolTimeOut: integer); overload;
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
  tiLog,
  tiUtils,
  tiConstants,
  tiExcept,
  tiPersistenceLayers;

const

  cusParamDelim   =   '|';

  cusConnectionNames = 'ConnectionNames';
  cusFileName        = 'DBParams.DCD';
  CDefaultMinPoolSize = 0;
  CDefaultMaxPoolSizeMultiUser = 9999;
  CDefaultMaxPoolSizeSingleUser = 1;

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
  LDatabase:= LPersistenceLayer.DatabaseClass.Create;
  APooledItem.Data:= LDatabase;
  LDatabase.Connect(
    DBConnectParams.DatabaseName,
    DBConnectParams.UserName,
    DBConnectParams.Password,
    DBConnectParams.DBParams,
    DBConnectParams.QueryOptions);
end;

constructor TtiDBConnectionPool.Create(
  const ADBConnectionPools: TtiDBConnectionPools;
  const ADatabaseAlias: string;
  const ADBConnectionParams: TtiDBConnectionParams;
  const AMinPoolSize, AMaxPoolSize, APoolTimeOut: integer);
begin
  Assert(ADBConnectionPools.TestValid, CTIErrorInvalidObject);
  inherited Create(AMinPoolSize, AMaxPoolSize, APoolTimeOut);
  FDBConnectionPools:= ADBConnectionPools;
  FDatabaseAlias:= ADatabaseAlias;
  FDBConnectionParams:= ADBConnectionParams;
  InitToMinPoolSize;
end;

procedure TtiDBConnectionPool.InitToMinPoolSize;
var
  LList: TList;
  i: integer;
begin
  LList:= TList.Create;
  try
    for i := 1 to MinPoolSize do
      LList.Add(Lock);
    for i := 0 to LList.Count-1 do
      UnLock(LList.Items[i]);
  finally
    LList.Free;
  end;
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
    function MustRemoveItemFromPool: boolean; override;
  end;

  function TtiPooledDatabase.MustRemoveItemFromPool: Boolean;
  begin
    Assert(Data.TestValid(TtiDatabase), CTIErrorInvalidObject);
    result :=
      (Inherited MustRemoveItemFromPool) or
      ((Data as TtiDatabase).ErrorInLastCall);
  end;

function TtiDBConnectionPool.PooledItemClass: TtiPooledItemClass;
begin
  result:= TtiPooledDatabase;
end;

procedure TtiDBConnectionPool.UnLock(const ADatabase: TtiDatabase);
begin
  Assert(ADatabase.TestValid, CTIErrorInvalidObject);
  //Assert(not ADatabase.InTransaction, 'Database in transaction immediately before being unlocked in DBConnectionPool.');
  if ADatabase.InTransaction then
    ADatabase.Rollback;
  inherited UnLock(ADatabase);
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TDBConnectionPools
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDBConnectionPools.Create(const APersistenceLayer: TtiBaseObject);
begin
  inherited Create;
  Assert(APersistenceLayer.TestValid(TtiPersistenceLayer, True), CTIErrorInvalidObject);
  FPersistenceLayer:= APersistenceLayer;
{$IFDEF IOS}
 FList := TObjectList<TtiDBConnectionPool>.Create;
{$ELSE}
 FList := TObjectList.Create;
{$ENDIF IOS}
  FCritSect:= TCriticalSection.Create;
end;

destructor TtiDBConnectionPools.Destroy;
begin
  FList.Free;
  FCritSect.Free;
  inherited;
end;


procedure TtiDBConnectionPools.Connect(
  const ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams, AQueryOptions: string;
  const AMinPoolSize, AMaxPoolSize, APoolTimeOut: integer);
var
  lDBConnectionPool : TtiDBConnectionPool;
  LDBConnectionParams: TtiDBConnectionParams;
  LDatabase: TtiDatabase;
  LMinPoolSize: integer;
  LMaxPoolSize: integer;
  LPoolTimeOut: integer;
begin
  LMinPoolSize:= AMinPoolSize;
  LMaxPoolSize:= AMaxPoolSize;
  LPoolTimeOut:= APoolTimeOut;
  GetPoolSizeDefaults(LMinPoolSize, LMaxPoolSize, LPoolTimeOut);

  LDBConnectionParams.DatabaseName:= ADatabaseName;
  LDBConnectionParams.UserName:= AUserName;
  LDBConnectionParams.Password:= APassword;
  LDBConnectionParams.DBParams:= AParams;
  LDBConnectionParams.QueryOptions:= AQueryOptions;

  FCritSect.Enter;
  try
    lDBConnectionPool := Find(ADatabaseAlias);
    if lDBConnectionPool <> nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorAttemptToAddDuplicateDBConnectionPool, [ADatabaseName + '/' + AUserName]);
    Log('Creating database connection pool for %s/%s', [ADatabaseName, AUserName], lsConnectionPool);
    lDBConnectionPool := TtiDBConnectionPool.Create(Self, ADatabaseAlias, LDBConnectionParams, LMinPoolSize, LMaxPoolSize, LPoolTimeOut);
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

procedure TtiDBConnectionPools.Connect(const ADatabaseAlias, ADatabaseName,
  AUserName, APassword, AParams: string);
begin
  Connect(
    ADatabaseAlias,
    ADatabaseName,
    AUserName,
    APassword,
    AParams,
    '',
    0,
    0,
    0);
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
  Log('About to lock: %s', [ADatabaseAlias], lsConnectionPool);
  FCritSect.Enter;
  try
    Log('Finding pool: %s', [ADatabaseAlias], lsConnectionPool);
    LDBConnectionPool := Find(ADatabaseAlias);
    if LDBConnectionPool = nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [ADatabaseAlias]);
    Log('Locking: %s', [ADatabaseAlias], lsConnectionPool);
    result := LDBConnectionPool.Lock;
  finally
    Log('Completing lock: %s', [ADatabaseAlias], lsConnectionPool);
    FCritSect.Leave;
    Log('Lock complete: %s', [ADatabaseAlias], lsConnectionPool);
  end;
end;

procedure TtiDBConnectionPools.UnLock(const ADatabaseAlias: string;
  const ADatabase : TtiDatabase);
var
  LDBConnectionPool : TtiDBConnectionPool;
begin
  Log('About to unlock: %s', [ADatabaseAlias], lsConnectionPool);
  FCritSect.Enter;
  try
    Log('Finding pool: %s', [ADatabaseAlias], lsConnectionPool);
    LDBConnectionPool := Find(ADatabaseAlias);
    Assert(LDBConnectionPool.TestValid(TtiDBConnectionPool, True), CTIErrorInvalidObject);
    if LDBConnectionPool = nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [ADatabaseAlias]);
    Log('Unlocking: %s', [ADatabaseAlias], lsConnectionPool);
    LDBConnectionPool.UnLock(ADatabase);
  finally
    Log('Completing unlock: %s', [ADatabaseAlias], lsConnectionPool);
    FCritSect.Leave;
    Log('Unlock complete: %s', [ADatabaseAlias], lsConnectionPool);
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
      result := tiAddTrailingValue(result, tiLineEnd(2), true);
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
  Assert(DBConnectionPools.TestValid(TtiDBConnectionPools), CTIErrorInvalidObject);
  Assert(DBConnectionPools.PersistenceLayer.TestValid(TtiPersistenceLayer), CTIErrorInvalidObject);
  LPersistenceLayer := DBConnectionPools.PersistenceLayer as TtiPersistenceLayer;
  result :=
    'Persistence layer:   ' + LPersistenceLayer.PersistenceLayerName + tiLineEnd +
    'Database name:       ' + DBConnectParams.DatabaseName + tiLineEnd +
    'User name:           ' + DBConnectParams.UserName     + tiLineEnd +
    'Password:            ' + CPasswordMasked + tiLineEnd +
    'Number in pool:      ' + IntToStr(Count);
end;

procedure TtiDBConnectionPools.AddInstance(
  const ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams : string;
  const AMinPoolSize, AMaxPoolSize, APoolTimeOut: integer);
var
  lDBConnectionPool : TtiDBConnectionPool;
  LDBConnectionParams: TtiDBConnectionParams;
  LMinPoolSize: integer;
  LMaxPoolSize: integer;
  LPoolTimeOut: integer;
begin
  LMinPoolSize:= AMinPoolSize;
  LMaxPoolSize:= AMaxPoolSize;
  LPoolTimeOut:= APoolTimeOut;
  GetPoolSizeDefaults(LMinPoolSize, LMaxPoolSize, LPoolTimeOut);

  LDBConnectionParams.DatabaseName:= ADatabaseName;
  LDBConnectionParams.UserName:= AUserName;
  LDBConnectionParams.Password:= APassword;
  LDBConnectionParams.DBParams:= AParams;
  FCritSect.Enter;
  try
    lDBConnectionPool := Find(ADatabaseAlias);
    if lDBConnectionPool <> nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorAttemptToAddDuplicateDBConnectionPool, [ADatabaseName + '/' + AUserName]);
    lDBConnectionPool := TtiDBConnectionPool.Create(Self, ADatabaseAlias, LDBConnectionParams, LMinPoolSize, LMaxPoolSize, LPoolTimeOut);
    FList.Add(lDBConnectionPool);
  finally
    FCritSect.Leave;
  end;
end;

procedure TtiDBConnectionPools.AddInstance(const ADatabaseAlias, ADatabaseName,
  AUserName, APassword, AParams: string);
begin
  AddInstance(ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams, 0, 0, 0);
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
  LDBConnectionPool : TtiDBConnectionPool;
  LWasDefault: boolean;
begin
  FCritSect.Enter;
  try
    LDBConnectionPool := Find(ADatabaseAlias);
    if LDBConnectionPool =  nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [ADatabaseAlias]);
    LWasDefault:= (PersistenceLayer as TtiPersistenceLayer).DefaultDBConnectionPool = LDBConnectionPool;
    FList.Extract(LDBConnectionPool);
    LDBConnectionPool.Free;
    FList.Capacity:= FList.Count; // To suppress leak warnings
    if LWasDefault and (Count > 0) then
      (PersistenceLayer as TtiPersistenceLayer).DefaultDBConnectionName:= Items[0].DatabaseAlias;
    if LWasDefault and (Count = 0) then
      (PersistenceLayer as TtiPersistenceLayer).DefaultDBConnectionName:= '';
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

procedure TtiDBConnectionPools.GetPoolSizeDefaults(
  var AMinPoolSize, AMaxPoolSize, APoolTimeOut: integer);
var
  LDefaults: TtiPersistenceLayerDefaults;
begin
  LDefaults:= TtiPersistenceLayerDefaults.Create;
  try
    (PersistenceLayer as TtiPersistenceLayer).AssignPersistenceLayerDefaults(LDefaults);
    if AMinPoolSize = 0 then
      AMinPoolSize:= CDefaultMinPoolSize;
    if AMaxPoolSize = 0 then
    begin
      if LDefaults.CanSupportMultiUser then
        AMaxPoolSize:= CDefaultMaxPoolSizeMultiUser
      else
        AMaxPoolSize:= CDefaultMaxPoolSizeSingleUser;
    end;
    if APoolTimeOut = 0 then
      APoolTimeOut:= 1;
  finally
    LDefaults.Free;
  end;
end;

procedure TtiDBConnectionPools.DisconnectAll;
var
  i : integer;
begin
  for i := Count - 1 downto 0 do
    Disconnect(Items[i].DatabaseAlias);
end;

end.
