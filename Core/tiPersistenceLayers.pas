unit tiPersistenceLayers;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ,Classes
  ,tiDBConnectionPool
  ,tiQuery
  ,tiVisitorDB
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,tiObject
  ,tiOID
 ;

const
  cErrorUnableToFindPerLayerToUnload = 'Unable to determine which persistence layer to unload.';
  cErrorAttemtpToLoadPerLayerThatsNotLoaded = 'Attempt to unload persistence layer <%s> that''s not currently loaded.';

type

  TtiPerLayerLoadingStyle = (pllsStaticLinking, pllsDynamicLoading);
  TtiPersistenceLayers = class;
  TtiPersistenceLayer  = class;

  TtiPersistenceLayers = class(TtiObjectList)
  private
    FDefaultPerLayer : TtiPersistenceLayer;
    FLayerLoadingStyle: TtiPerLayerLoadingStyle;
    function    PackageIDToPackageName(const pPackageID: string): TFileName;
    function    GetDefaultPerLayer: TtiPersistenceLayer;
    procedure   SetDefaultPerLayer(const Value: TtiPersistenceLayer);
    function    GetDefaultPerLayerName: string;
    procedure   SetDefaultPerLayerName(const Value: string);
  protected
    function    GetItems(i: integer): TtiPersistenceLayer; reintroduce;
    procedure   SetItems(i: integer; const Value: TtiPersistenceLayer); reintroduce;
    function    GetOwner: TtiPersistenceLayer; reintroduce;
    procedure   SetOwner(const Value: TtiPersistenceLayer); reintroduce;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Items[i:integer] : TtiPersistenceLayer read GetItems write SetItems;
    procedure   Add(pObject : TtiPersistenceLayer); reintroduce;

    // These manage the loading and unloading of the packages
    function    LoadPersistenceLayer(const pPerLayerName : string) : TtiPersistenceLayer;
    procedure   UnLoadPersistenceLayer(const pPerLayerName : string);
    function    IsLoaded(const pPerLayerName : string) : boolean;

    function    FindByPerLayerName(const pLayerName : string) : TtiPersistenceLayer;
    function    FindByTIDatabaseClass(const pTIDatabaseClass : TtiDatabaseClass) : TtiPersistenceLayer;
    property    DefaultPerLayer     : TtiPersistenceLayer read GetDefaultPerLayer     write SetDefaultPerLayer;
    property    DefaultPerLayerName : string         read GetDefaultPerLayerName write SetDefaultPerLayerName;
    property    LoadingStyle: TtiPerLayerLoadingStyle read FLayerLoadingStyle write FLayerLoadingStyle;

    // Do not call these your self. They are called in the initialization section
    // of tiQueryXXX.pas that contains the concrete classes.
    procedure   __RegisterPersistenceLayer(const pLayerName: string;
                                          pDBConnectionPoolDataClass : TtiDBConnectionPoolDataClass;
                                          ptiQueryClass : TtiQueryClass;
                                          ptiDatabaseClass : TtiDatabaseClass);
    procedure   __UnRegisterPersistenceLayer(const pLayerName: string);

    function    CreateTIQuery(const pLayerName : string {= '' })            : TtiQuery; overload;
    function    CreateTIQuery(const pTIDatabaseClass : TtiDatabaseClass) : TtiQuery; overload;
    function    CreateTIDatabase(const pLayerName : string {= '' })     : TtiDatabase;
    function    CreateTIDBConnectionPoolData(const pLayerName : string {= ''}) : TtiDBConnectionPoolDataAbs;
    function    LockDatabase(const pDBConnectionName : string; pPerLayerName : string) : TtiDatabase;
    procedure   UnLockDatabase(const pDatabase : TtiDatabase;const pDBConnectionName : string; pPerLayerName : string);

    {$IFDEF FPC}
    {$I tiPersistenceLayersIntf.inc}
    {$ENDIF}
  end;

  TtiPersistenceLayer = class(TtiObject)
  private
    FTiQueryClass: TtiQueryClass;
    FTiDatabaseClass: TtiDatabaseClass;
    FTiDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
    FPerLayerName: string;
    FModuleID: HModule;
    FDBConnectionPools: TDBConnectionPools;
    FDefaultDBConnectionPool : TDBConnectionPool;
    FNextOIDMgr: TNextOIDMgr;
    FDynamicallyLoaded: boolean;
    function  GetDefaultDBConnectionPool: TDBConnectionPool;
    function  GetDefaultDBConnectionName: string;
    procedure SetDefaultDBConnectionName(const Value: string);
  protected
    function    GetOwner: TtiPersistenceLayers; reintroduce;
    procedure   SetOwner(const Value: TtiPersistenceLayers); reintroduce;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Owner       : TtiPersistenceLayers            read GetOwner      write SetOwner;

    // Properties that are set when the persistence layer is loaded (set in
    // initialization section of TtiQueryXXX.pas that contains the concrete)
    property  tiDBConnectionPoolDataClass : TtiDBConnectionPoolDataClass read FTiDBConnectionPoolDataClass write FTiDBConnectionPoolDataClass;
    property  tiQueryClass                : TtiQueryClass read FTiQueryClass        write FTiQueryClass;
    property  tiDatabaseClass             : TtiDatabaseClass read FTiDatabaseClass  write FTiDatabaseClass;
    property  PerLayerName                : string read FPerLayerName write FPerLayerName;
    property  DynamicallyLoaded           : boolean read FDynamicallyLoaded write FDynamicallyLoaded;

    property  ModuleID                    : HModule read FModuleID write FModuleID;
    property  DefaultDBConnectionName     : string read GetDefaultDBConnectionName write SetDefaultDBConnectionName;
    property  DefaultDBConnectionPool     : TDBConnectionPool read GetDefaultDBConnectionPool;
    property  DBConnectionPools           : TDBConnectionPools read FDBConnectionPools;
    property  NextOIDMgr                  : TNextOIDMgr read FNextOIDMgr;

    function  DatabaseExists(const pDatabaseName, pUserName, pPassword : string) : boolean;
    procedure CreateDatabase(const pDatabaseName, pUserName, pPassword : string);
    function  TestConnectToDatabase(const pDatabaseName, pUserName, pPassword, pParams : string) : boolean;
  end;


implementation
uses
   tiUtils
  ,tiLog
  ,tiConstants
  ,tiOPFManager
  ,tiExcept
 ;

constructor TtiPersistenceLayer.Create;
begin
  inherited;
  FTiDBConnectionPoolDataClass := TtiDBConnectionPoolDataAbs;
  ModuleID := 0;
  FDBConnectionPools:= TDBConnectionPools.Create;
  FDBConnectionPools.Owner := Self;
  FNextOIDMgr := TNextOIDMgr.Create;
  {$IFNDEF OID_AS_INT64}
  FNextOIDMgr.Owner := Self;
  {$ENDIF}
  FDynamicallyLoaded := false;
end;

procedure TtiPersistenceLayers.Add(pObject: TtiPersistenceLayer);
begin
  inherited Add(pObject);
end;

function TtiPersistenceLayers.FindByPerLayerName(const pLayerName: string): TtiPersistenceLayer;
var
  i : integer;
begin
  result := nil;
  if (pLayerName = '') and
     (Count = 1) then
  begin
    result := Items[0];
    Exit; //==>
  end;

  for i := 0 to Count - 1 do
    if SameText(Items[i].PerLayerName, pLayerName) then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;

function TtiPersistenceLayers.GetItems(i: integer): TtiPersistenceLayer;
begin
  result := TtiPersistenceLayer(inherited GetItems(i));
end;

function TtiPersistenceLayers.GetOwner: TtiPersistenceLayer;
begin
  result := TtiPersistenceLayer(GetOwner);
end;

procedure TtiPersistenceLayers.__RegisterPersistenceLayer(
  const pLayerName: string;
  pDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
  ptiQueryClass: TtiQueryClass; ptiDatabaseClass: TtiDatabaseClass);
var
  lData : TtiPersistenceLayer;
begin
  if IsLoaded(pLayerName) then
    Exit; //==>
  lData := TtiPersistenceLayer.Create;
  lData.PerLayerName := pLayerName;
  lData.tiDBConnectionPoolDataClass := pDBConnectionPoolDataClass;
  lData.tiQueryClass          := ptiQueryClass   ;
  lData.tiDatabaseClass       := ptiDatabaseClass;
  Add(lData);
end;

procedure TtiPersistenceLayers.SetItems(i: integer; const Value: TtiPersistenceLayer);
begin
  inherited SetItems(i, Value);
end;

procedure TtiPersistenceLayers.SetOwner(const Value: TtiPersistenceLayer);
begin
  inherited SetOwner(Value);
end;

function TtiPersistenceLayers.CreateTIDatabase(const pLayerName : string {= ''}) : TtiDatabase;
var
  lData : TtiPersistenceLayer;
begin
  lData := FindByPerLayerName(pLayerName);
  if lData = nil then
    raise Exception.Create('Request for unregistered persistence layer <' + pLayerName + '>');
  result := lData.tiDatabaseClass.Create;
end;

function TtiPersistenceLayers.CreateTIQuery(const pLayerName : string {= ''}): TtiQuery;
var
  lData : TtiPersistenceLayer;
begin
  lData := FindByPerLayerName(pLayerName);
  if lData = nil then
    raise Exception.Create('Request for unregistered persistence layer <' + pLayerName + '>');
  result := lData.tiQueryClass.Create;
end;

function TtiPersistenceLayers.CreateTIDBConnectionPoolData(const pLayerName : string {= ''}) : TtiDBConnectionPoolDataAbs;
var
  lData : TtiPersistenceLayer;
begin
  lData := FindByPerLayerName(pLayerName);
  if lData = nil then
    raise Exception.Create('Request for unregistered persistence layer <' + pLayerName + '>');
  result := lData.tiDBConnectionPoolDataClass.Create;
end;

procedure TtiPersistenceLayer.CreateDatabase(const pDatabaseName, pUserName,
  pPassword: string);
begin
  Assert(FTiDatabaseClass<>nil, 'FTiDatabaseClass not assigned');
  FTiDatabaseClass.CreateDatabase(pDatabaseName, pUserName, pPassword);
end;

function TtiPersistenceLayer.DatabaseExists(const pDatabaseName, pUserName, pPassword: string): boolean;
begin
  Assert(FTiDatabaseClass<>nil, 'FTiDatabaseClass not assigned');
  result := FTiDatabaseClass.DatabaseExists(pDatabaseName, pUserName, pPassword);
end;

destructor TtiPersistenceLayer.Destroy;
begin
  FDBConnectionPools.Free;
  FNextOIDMgr.Free;
  inherited;
end;

function TtiPersistenceLayer.GetDefaultDBConnectionName: string;
var
  lDBConnectionPool : TDBConnectionPool;
begin
  lDBConnectionPool := DefaultDBConnectionPool;
  if lDBConnectionPool = nil then
  begin
    result := '';
    Exit; //==>
  end;
  result := lDBConnectionPool.DBConnectParams.ConnectionName;
end;

function TtiPersistenceLayer.GetDefaultDBConnectionPool: TDBConnectionPool;
begin
  Assert(FDefaultDBConnectionPool.TestValid(TDBConnectionPool, true), cTIInvalidObjectError);
  if FDefaultDBConnectionPool <> nil then
  begin
    result := FDefaultDBConnectionPool;
    Exit; //==>
  end;

  if DBConnectionPools.Count = 0 then
  begin
    result := nil;
    Exit; //==>
  end;

  result := DBConnectionPools.Items[0];
  Assert(Result.TestValid(TDBConnectionPool), cTIInvalidObjectError);
end;

function TtiPersistenceLayer.GetOwner: TtiPersistenceLayers;
begin
  result := TtiPersistenceLayers(inherited GetOwner);
end;

procedure TtiPersistenceLayer.SetDefaultDBConnectionName(const Value: string);
begin
  FDefaultDBConnectionPool := FDBConnectionPools.Find(Value);
  Assert(FDefaultDBConnectionPool.TestValid(TDBConnectionPool, true), cTIInvalidObjectError);
end;

procedure TtiPersistenceLayer.SetOwner(const Value: TtiPersistenceLayers);
begin
  inherited SetOwner(Value);
end;

destructor TtiPersistenceLayers.Destroy;
var
  i : integer;
begin
  for i := Count - 1 downto 0 do
//    if Items[i].ModuleID <> 0 then
      UnLoadPersistenceLayer(Items[i].PerLayerName);
//      UnLoadPackage(Items[i].ModuleID);
  inherited;
end;

procedure TtiPersistenceLayers.__UnRegisterPersistenceLayer(const pLayerName: string);
var
  lData : TtiPersistenceLayer;
begin
  lData := FindByPerLayerName(pLayerName);
  if lData = nil then
    Exit; //==>
  if gTIOPFManager.DefaultPerLayer = lData then
    gTIOPFManager.DefaultPerLayer := nil;
  Remove(lData);
end;

function TtiPersistenceLayers.IsLoaded(const pPerLayerName: string): boolean;
begin
  result := (FindByPerLayerName(pPerLayerName) <> nil);
end;

function TtiPersistenceLayers.CreateTIQuery(
  const pTIDatabaseClass: TtiDatabaseClass): TtiQuery;
var
  lData : TtiPersistenceLayer;
begin
  lData := FindByTIDatabaseClass(pTIDatabaseClass);
  if lData = nil then
    raise Exception.Create('Unable to find persistence layer for database class <' + pTIDatabaseClass.ClassName + '>');
  result := lData.tiQueryClass.Create;
end;

function TtiPersistenceLayers.FindByTIDatabaseClass(
  const pTIDatabaseClass: TtiDatabaseClass): TtiPersistenceLayer;
var
  i : integer;
begin
  Assert(pTIDatabaseClass <> nil, 'pTIDatabaseClass <> nil');
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].TIDatabaseClass = pTIDatabaseClass then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;

function TtiPersistenceLayers.LoadPersistenceLayer(const pPerLayerName: string) : TtiPersistenceLayer;
var
  lPackageName : TFileName;
  lPackageModule : HModule;
  lMessage : string;
begin
  result := FindByPerLayerName(pPerLayerName);
  if result <> nil then
    Exit; //==>

  lPackageName := PackageIDToPackageName(pPerLayerName);
  Log('Loading %s', [lPackageName], lsConnectionPool);

  try
    lPackageModule := LoadPackage(ExtractFileName(lPackageName));
    result   := FindByPerLayerName(pPerLayerName);
    if result = nil then
      raise exception.Create('Unable to locate package in memory after it was loaded.' + Cr +
                              'Check that this application was build with the runtime package tiPersistCore');
    result.DynamicallyLoaded := true;
    result.ModuleID := lPackageModule;
  except
    on e:exception do
    begin
      lMessage := 'Unable to initialize persistence layer <' +
                  pPerLayerName + '> Package name <' +
                  lPackageName + '>' + Cr(2) +
                  'Error message: ' + e.message;
      raise Exception.Create(lMessage);
    end;
  end;
end;

function TtiPersistenceLayers.PackageIDToPackageName(const pPackageID : string) : TFileName;
begin
  result :=
    tiAddTrailingSlash(tiGetEXEPath) +
    cTIPersistPackageRootName +
    pPackageID +
    cPackageSuffix +
    '.bpl';
end;

{
function TtiPersistenceLayers.PackageNameToPackageID(const pPackageName : TFileName) : string;
begin
  result := tiExtractFileNameOnly(pPackageName);
  result := tiStrTran(result, cPackageSuffix, '');
  result := tiStrTran(result,
                       cTIPersistPackageRootName,
                       '');
end;
}

procedure TtiPersistenceLayers.UnLoadPersistenceLayer(const pPerLayerName: string);
var
  lPackageID     : string;
  lRegPerLayer  : TtiPersistenceLayer;
begin
  if pPerLayerName <> '' then
    lPackageID := pPerLayerName
  else if Count = 1 then
    lPackageID   := Items[0].PerLayerName
  else
    raise EtiOPFProgrammerException.Create(cErrorUnableToFindPerLayerToUnload);

  if not IsLoaded(pPerLayerName) then
    raise EtiOPFProgrammerException.CreateFmt(cErrorAttemtpToLoadPerLayerThatsNotLoaded, [lPackageID]);

  lRegPerLayer := FindByPerLayerName(pPerLayerName);
  Assert(lRegPerLayer.TestValid, cTIInvalidObjectError);

  lRegPerLayer.DBConnectionPools.DisConnectAll;
  Log('Unloading persistence layer <' + lRegPerLayer.PerLayerName + '>', lsConnectionPool);

  if lRegPerLayer.ModuleID <> 0 then
    UnLoadPackage(lRegPerLayer.ModuleID) // lRegPerLayer has now been destroyed
  else
    Remove(lRegPerLayer);

  if Count > 0 then
    DefaultPerLayer:= Items[0]
  else
    DefaultPerLayer:= nil;
end;

function TtiPersistenceLayers.LockDatabase(const pDBConnectionName: string;pPerLayerName: string): TtiDatabase;
var
  lRegPerLayer : TtiPersistenceLayer;
  lDBConnectionName : string;
  lPooledDB : TPooledDB;
begin
  if pPerLayerName <> '' then
    lRegPerLayer := FindByPerLayerName(pPerLayerName)
  else
    lRegPerLayer := DefaultPerLayer;

  Assert(lRegPerLayer.TestValid(TtiPersistenceLayer), cTIInvalidObjectError);

  if pDBConnectionName <> '' then
    lDBConnectionName := pDBConnectionName
  else
    lDBConnectionName := lRegPerLayer.DefaultDBConnectionName;

  lPooledDB := lRegPerLayer.DBConnectionPools.Lock(lDBConnectionName);
  result    := lPooledDB.Database;
end;

procedure TtiPersistenceLayers.UnLockDatabase(const pDatabase: TtiDatabase;
  const pDBConnectionName: string; pPerLayerName: string);
var
  lDBConnectionName : string;
  lRegPerLayer : TtiPersistenceLayer;
begin
  if pPerLayerName <> '' then
    lRegPerLayer := FindByPerLayerName(pPerLayerName)
  else
    lRegPerLayer := DefaultPerLayer;

  Assert(lRegPerLayer.TestValid(TtiPersistenceLayer), cTIInvalidObjectError);

  if pDBConnectionName <> '' then
    lDBConnectionName := pDBConnectionName
  else
    lDBConnectionName := lRegPerLayer.DefaultDBConnectionName;

  lRegPerLayer.DBConnectionPools.UnLockByData(lDBConnectionName, pDatabase);
end;

function TtiPersistenceLayers.GetDefaultPerLayer: TtiPersistenceLayer;
begin
  Assert(FDefaultPerLayer.TestValid(TtiPersistenceLayer, true), cTIInvalidObjectError);
  if FDefaultPerLayer <> nil then
  begin
    result := FDefaultPerLayer;
    Exit; //==>
  end;
  if Count = 0 then
  begin
    result := nil;
    Exit; //==>
  end;
  FDefaultPerLayer := Items[0];
  Assert(FDefaultPerLayer.TestValid(TtiPersistenceLayer), cTIInvalidObjectError);
  result := FDefaultPerLayer;
end;

procedure TtiPersistenceLayers.SetDefaultPerLayer(const Value: TtiPersistenceLayer);
begin
  FDefaultPerLayer := Value;
end;

function TtiPersistenceLayers.GetDefaultPerLayerName: string;
begin
  if DefaultPerLayer <> nil then
    result := DefaultPerLayer.PerLayerName
  else
    result := '';
end;

procedure TtiPersistenceLayers.SetDefaultPerLayerName(const Value: string);
begin
  FDefaultPerLayer := FindByPerLayerName(Value);
end;

function TtiPersistenceLayer.TestConnectToDatabase(const pDatabaseName,
  pUserName, pPassword, pParams: string): boolean;
begin
  Assert(FTiDatabaseClass<>nil, 'FTiDatabaseClass not assigned');
  result := FTiDatabaseClass.TestConnectTo(pDatabaseName, pUserName, pPassword,
                                           pParams);
end;

constructor TtiPersistenceLayers.Create;
begin
  inherited;
  FLayerLoadingStyle := pllsDynamicLoading;
end;

{$IFDEF FPC}
{$I tiPersistenceLayersImpl.inc}
{$ENDIF}

end.
