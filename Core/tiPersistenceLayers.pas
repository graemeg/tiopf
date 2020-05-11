unit tiPersistenceLayers;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiObject
  ,tiDBConnectionPool
  ,tiQuery
  ,SysUtils
  ,Classes
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
;

const
  cErrorUnableToFindPerLayerToUnload = 'Unable to determine which persistence layer to unload.';
  cErrorAttemtpToLoadPerLayerThatsNotLoaded = 'Attempt to unload persistence layer <%s> that''s not currently loaded.';
  CDefaultDatabaseName = 'Demo';
  CDefaultDatabaseDirectory = '..' + PathDelim + '_Data' + PathDelim;

type

  TtiPerLayerLoadingStyle = (pllsStaticLinking, pllsDynamicLoading);
  TtiPersistenceLayers = class;
  TtiPersistenceLayer  = class;
  TtiPersistenceLayerClass = class of TtiPersistenceLayer;
  TtiPersistenceLayerDefaults = class;

  TtiPersistenceLayers = class(TtiObjectList)
  private
    FDefaultPerLayer : TtiPersistenceLayer;
    FLayerLoadingStyle: TtiPerLayerLoadingStyle;
    function    PackageIDToPackageName(const APackageID: string): TFileName;
    function    GetDefaultPerLayer: TtiPersistenceLayer;
    procedure   SetDefaultPerLayer(const AValue: TtiPersistenceLayer);
    function    GetDefaultPersistenceLayerName: string;
    procedure   SetDefaultPersistenceLayerName(const AValue: string);
  protected
    function    GetItems(i: integer): TtiPersistenceLayer; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiPersistenceLayer); reintroduce;
    function    GetOwner: TtiPersistenceLayer; reintroduce;
    procedure   SetOwner(const AValue: TtiPersistenceLayer); reintroduce;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Items[i:integer]: TtiPersistenceLayer read GetItems write SetItems; default;
    procedure   Add(AObject : TtiPersistenceLayer); reintroduce;

    // These manage the loading and unloading of the packages
    function    LoadPersistenceLayer(const APersistenceLayerName : string): TtiPersistenceLayer;
    procedure   UnLoadPersistenceLayer(const APersistenceLayerName : string);
    function    IsLoaded(const APersistenceLayerName : string): boolean;
    function    IsDefault(const APersistenceLayerName : string): boolean;

    function    FindByPersistenceLayerName(const ALayerName : string): TtiPersistenceLayer;
    function    FindByTIDatabaseClass(const ADatabaseClass : TtiDatabaseClass): TtiPersistenceLayer;
    property    DefaultPerLayer    : TtiPersistenceLayer read GetDefaultPerLayer     write SetDefaultPerLayer;
    property    DefaultPersistenceLayerName : string         read GetDefaultPersistenceLayerName write SetDefaultPersistenceLayerName;
    property    LoadingStyle: TtiPerLayerLoadingStyle read FLayerLoadingStyle write FLayerLoadingStyle;

    // Do not call these your self. They are called in the initialization section
    // of tiQueryXXX.pas that contains the concrete classes.
    procedure   __RegisterPersistenceLayer(const APersistenceLayerClass: TtiPersistenceLayerClass);
    procedure   __UnRegisterPersistenceLayer(const ALayerName: string);

    function    CreateTIQuery(const ALayerName : string {= '' })           : TtiQuery; overload;
    function    CreateTIQuery(const ADatabaseClass : TtiDatabaseClass): TtiQuery; overload;
    function    CreateTIDatabase(const ALayerName : string {= '' })    : TtiDatabase;
    function    LockDatabase(const ADBConnectionName : string; APersistenceLayerName : string): TtiDatabase;
    procedure   UnLockDatabase(const ADatabase : TtiDatabase;const ADBConnectionName : string; APersistenceLayerName : string);

    {$IFDEF FPC}
    {$I tiPersistenceLayersIntf.inc}
    {$ENDIF}
  end;

  { TtiPersistenceLayer }

  TtiPersistenceLayer = class(TtiObject)
  private
    FModuleID: HModule;
    FDBConnectionPools: TtiDBConnectionPools;
    FDefaultDBConnectionPool : TtiDBConnectionPool;
    FDynamicallyLoaded: boolean;
    function  GetDefaultDBConnectionPool: TtiDBConnectionPool;
    function  GetDefaultDBConnectionName: string;
    procedure SetDefaultDBConnectionName(const AValue: string);
  protected
    function    GetOwner: TtiPersistenceLayers; reintroduce;
    procedure   SetOwner(const AValue: TtiPersistenceLayers); reintroduce;
    function    GetCaption: string; override;

    // These must be overridden in the concrete classes
    function GetDatabaseClass: TtiDatabaseClass; virtual; abstract;
    function GetPersistenceLayerName: string; virtual; abstract;
    function GetQueryClass: TtiQueryClass; virtual; abstract;

  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Owner      : TtiPersistenceLayers            read GetOwner      write SetOwner;

    property  QueryClass                 : TtiQueryClass read GetQueryClass;
    property  DatabaseClass              : TtiDatabaseClass read GetDatabaseClass;
    property  PersistenceLayerName       : string read GetPersistenceLayerName;

    property  DynamicallyLoaded          : boolean read FDynamicallyLoaded write FDynamicallyLoaded;
    property  ModuleID                   : HModule read FModuleID write FModuleID;
    // ToDo: DefaultDBConnectionName will actuallly return the connection Alias. Rename property
    property  DefaultDBConnectionName    : string read GetDefaultDBConnectionName write SetDefaultDBConnectionName;
    property  DefaultDBConnectionPool    : TtiDBConnectionPool read GetDefaultDBConnectionPool;
    property  DBConnectionPools          : TtiDBConnectionPools read FDBConnectionPools;

    function  DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string=''): boolean;
    procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string='');
    procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string='');
    function  TestConnectToDatabase(const ADatabaseName, AUserName, APassword, AParams : string): boolean;
    // Must be overridden in the concreate class
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); virtual; abstract;
  end;

  { TtiPersistenceLayerDefaults }

  TtiPersistenceLayerDefaults = class(TtiBaseObject)
  private
    FCanCreateDatabae: Boolean;
    FCanDropDatabae: Boolean;
    FDatabaseName: string;
    FPassword: string;
    FCanSupportMultiUser: Boolean;
    FUserName: string;
    FPersistenceLayerName: string;
    FCanSupportSQL: Boolean;
    FIsDatabaseNameFilePath: boolean;
    FParams: string;
  public
    property PersistenceLayerName: string read FPersistenceLayerName write FPersistenceLayerName;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property IsDatabaseNameFilePath: boolean read FIsDatabaseNameFilePath write FIsDatabaseNameFilePath;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Params: string read FParams write FParams;
    property CanCreateDatabase: Boolean read FCanCreateDatabae write FCanCreateDatabae;
    property CanDropDatabase: Boolean read FCanDropDatabae write FCanDropDatabae;
    property CanSupportMultiUser: Boolean read FCanSupportMultiUser write FCanSupportMultiUser;
    property CanSupportSQL: Boolean read FCanSupportSQL write FCanSupportSQL;
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
  ModuleID := 0;
  FDBConnectionPools:= TtiDBConnectionPools.Create(Self);
  FDynamicallyLoaded := false;
end;

procedure TtiPersistenceLayers.Add(AObject: TtiPersistenceLayer);
begin
  inherited Add(AObject);
end;

function TtiPersistenceLayers.FindByPersistenceLayerName(const ALayerName: string): TtiPersistenceLayer;
var
  i : integer;
begin
  result := nil;
  if (ALayerName = '') and
     (Count = 1) then
  begin
    result := Items[0];
    Exit; //==>
  end;

  for i := 0 to Count - 1 do
    if SameText(Items[i].PersistenceLayerName, ALayerName) then
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
  const APersistenceLayerClass: TtiPersistenceLayerClass);
var
  LData : TtiPersistenceLayer;
begin
  Assert(APersistenceLayerClass <> nil, 'APersistenceLayerClass not assigned');
  LData := APersistenceLayerClass.Create;
  if IsLoaded(LData.PersistenceLayerName) then
    LData.Free
  else
    Add(LData);
end;

procedure TtiPersistenceLayers.SetItems(i: integer; const AValue: TtiPersistenceLayer);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiPersistenceLayers.SetOwner(const AValue: TtiPersistenceLayer);
begin
  inherited SetOwner(AValue);
end;

function TtiPersistenceLayers.CreateTIDatabase(const ALayerName : string {= ''}): TtiDatabase;
var
  LPersistenceLayer : TtiPersistenceLayer;
begin
  LPersistenceLayer := FindByPersistenceLayerName(ALayerName);
  if LPersistenceLayer = nil then
    raise Exception.Create('Request for unregistered persistence layer <' + ALayerName + '>');
  result := LPersistenceLayer.DatabaseClass.Create;
end;

function TtiPersistenceLayers.CreateTIQuery(const ALayerName : string {= ''}): TtiQuery;
var
  LPersistenceLayer : TtiPersistenceLayer;
begin
  LPersistenceLayer := FindByPersistenceLayerName(ALayerName);
  if LPersistenceLayer = nil then
    raise Exception.Create('Request for unregistered persistence layer <' + ALayerName + '>');
  result := LPersistenceLayer.QueryClass.Create;
end;

procedure TtiPersistenceLayer.CreateDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
begin
  Assert(DatabaseClass<>nil, 'DatabaseClass not assigned');
  DatabaseClass.CreateDatabase(ADatabaseName, AUserName, APassword, AParams);
end;

procedure TtiPersistenceLayer.DropDatabase(const ADatabaseName, AUserName,
  APassword: string; const AParams: string);
begin
  Assert(DatabaseClass<>nil, 'DatabaseClass not assigned');
  DatabaseClass.DropDatabase(ADatabaseName, AUserName, APassword, AParams);
end;

function TtiPersistenceLayer.DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string): boolean;
begin
  Assert(DatabaseClass<>nil, 'DatabaseClass not assigned');
  result := DatabaseClass.DatabaseExists(ADatabaseName, AUserName, APassword, AParams);
end;

destructor TtiPersistenceLayer.Destroy;
begin
  FDBConnectionPools.Free;
  inherited;
end;

function TtiPersistenceLayer.GetDefaultDBConnectionName: string;
var
  lDBConnectionPool : TtiDBConnectionPool;
begin
  lDBConnectionPool := DefaultDBConnectionPool;
  if lDBConnectionPool = nil then
  begin
    result := '';
    Exit; //==>
  end;
  result := lDBConnectionPool.DatabaseAlias;
end;

function TtiPersistenceLayer.GetDefaultDBConnectionPool: TtiDBConnectionPool;
begin
  Assert(FDefaultDBConnectionPool.TestValid(TtiDBConnectionPool, true), CTIErrorInvalidObject);
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
  Assert(Result.TestValid(TtiDBConnectionPool), CTIErrorInvalidObject);
end;

function TtiPersistenceLayer.GetOwner: TtiPersistenceLayers;
begin
  result := TtiPersistenceLayers(inherited GetOwner);
end;

procedure TtiPersistenceLayer.SetDefaultDBConnectionName(const AValue: string);
begin
  FDefaultDBConnectionPool := FDBConnectionPools.Find(AValue);
  Assert(FDefaultDBConnectionPool.TestValid(TtiDBConnectionPool, true), CTIErrorInvalidObject);
end;

procedure TtiPersistenceLayer.SetOwner(const AValue: TtiPersistenceLayers);
begin
  inherited SetOwner(AValue);
end;

function TtiPersistenceLayer.GetCaption: string;
begin
  Result := PersistenceLayerName;
end;

destructor TtiPersistenceLayers.Destroy;
var
  i : integer;
begin
  for i := Count - 1 downto 0 do
    UnLoadPersistenceLayer(Items[i].PersistenceLayerName);
  inherited;
end;

procedure TtiPersistenceLayers.__UnRegisterPersistenceLayer(const ALayerName: string);
var
  lData : TtiPersistenceLayer;
begin
  lData := FindByPersistenceLayerName(ALayerName);
  if lData = nil then
    Exit; //==>
  if GTIOPFManager.DefaultPerLayer = lData then
    GTIOPFManager.DefaultPerLayer := nil;
  Remove(lData);
end;

function TtiPersistenceLayers.IsDefault(const APersistenceLayerName: string): boolean;
begin
  result := SameText(DefaultPersistenceLayerName, APersistenceLayerName);
end;

function TtiPersistenceLayers.IsLoaded(const APersistenceLayerName: string): boolean;
begin
  result := (FindByPersistenceLayerName(APersistenceLayerName) <> nil);
end;

function TtiPersistenceLayers.CreateTIQuery(
  const ADatabaseClass: TtiDatabaseClass): TtiQuery;
var
  LPersistenceLayer : TtiPersistenceLayer;
begin
  LPersistenceLayer := FindByTIDatabaseClass(ADatabaseClass);
  if LPersistenceLayer = nil then
    raise Exception.Create('Unable to find persistence layer for database class <' + ADatabaseClass.ClassName + '>');
  result := LPersistenceLayer.QueryClass.Create;
end;

function TtiPersistenceLayers.FindByTIDatabaseClass(
  const ADatabaseClass: TtiDatabaseClass): TtiPersistenceLayer;
var
  i : integer;
begin
  Assert(ADatabaseClass <> nil, 'ADatabaseClass <> nil');
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].DatabaseClass = ADatabaseClass then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;

function TtiPersistenceLayers.LoadPersistenceLayer(const APersistenceLayerName: string): TtiPersistenceLayer;
var
  lPackageName : TFileName;
  lPackageModule : HModule;
  lMessage : string;
begin
  result := FindByPersistenceLayerName(APersistenceLayerName);
  if result <> nil then
    Exit; //==>

  lPackageName := PackageIDToPackageName(APersistenceLayerName);
  Log('Loading %s', [lPackageName], lsConnectionPool);

  try
    lPackageModule := LoadPackage(ExtractFileName(lPackageName));
    result  := FindByPersistenceLayerName(APersistenceLayerName);
    if result = nil then
      raise exception.Create('Unable to locate package in memory after it was loaded.' + tiLineEnd +
                              'Check that this application was build with the runtime package tiPersistCore');
    result.DynamicallyLoaded := true;
    result.ModuleID := lPackageModule;
  except
    on e:exception do
    begin
      lMessage := 'Unable to initialize persistence layer <' +
                  APersistenceLayerName + '> Package name <' +
                  lPackageName + '>' + tiLineEnd(2) +
                  'Error message: ' + e.message;
      raise Exception.Create(lMessage);
    end;
  end;
end;

function TtiPersistenceLayers.PackageIDToPackageName(const APackageID : string): TFileName;
begin
  result :=
    tiAddTrailingSlash(tiGetEXEPath) +
    cTIPersistPackageRootName +
    APackageID +
    cPackageSuffix +
    cPackageExtension;
end;

{
function TtiPersistenceLayers.PackageNameToPackageID(const pPackageName : TFileName): string;
begin
  result := tiExtractFileNameOnly(pPackageName);
  result := tiStrTran(result, cPackageSuffix, '');
  result := tiStrTran(result,
                       cTIPersistPackageRootName,
                       '');
end;
}

procedure TtiPersistenceLayers.UnLoadPersistenceLayer(const APersistenceLayerName: string);
var
  LPackageID    : string;
  LPersistenceLayer : TtiPersistenceLayer;
begin
  if APersistenceLayerName <> '' then
    LPackageID := APersistenceLayerName
  else if Count = 1 then
    LPackageID  := Items[0].PersistenceLayerName
  else
    raise EtiOPFProgrammerException.Create(cErrorUnableToFindPerLayerToUnload);

  if not IsLoaded(APersistenceLayerName) then
    raise EtiOPFProgrammerException.CreateFmt(cErrorAttemtpToLoadPerLayerThatsNotLoaded, [LPackageID]);

  LPersistenceLayer := FindByPersistenceLayerName(APersistenceLayerName);
  Assert(LPersistenceLayer.TestValid, CTIErrorInvalidObject);

  LPersistenceLayer.DBConnectionPools.DisConnectAll;
  Log('Unloading persistence layer <' + LPersistenceLayer.PersistenceLayerName + '>', lsConnectionPool);

  if LPersistenceLayer.ModuleID <> 0 then
    UnLoadPackage(LPersistenceLayer.ModuleID) // lRegPerLayer has now been destroyed
  else
    Remove(LPersistenceLayer);

  if Count > 0 then
    DefaultPerLayer:= Items[0]
  else
    DefaultPerLayer:= nil;
end;

function TtiPersistenceLayers.LockDatabase(const ADBConnectionName: string;APersistenceLayerName: string): TtiDatabase;
var
  lRegPerLayer : TtiPersistenceLayer;
  lDBConnectionName : string;
begin
  if APersistenceLayerName <> '' then
    lRegPerLayer := FindByPersistenceLayerName(APersistenceLayerName)
  else
    lRegPerLayer := DefaultPerLayer;

  Assert(lRegPerLayer.TestValid(TtiPersistenceLayer), CTIErrorInvalidObject);

  if ADBConnectionName <> '' then
    lDBConnectionName := ADBConnectionName
  else
    lDBConnectionName := lRegPerLayer.DefaultDBConnectionName;

  Result := lRegPerLayer.DBConnectionPools.Lock(lDBConnectionName);

end;

procedure TtiPersistenceLayers.UnLockDatabase(const ADatabase: TtiDatabase;
  const ADBConnectionName: string; APersistenceLayerName: string);
var
  lDBConnectionName : string;
  lRegPerLayer : TtiPersistenceLayer;
begin
  if APersistenceLayerName <> '' then
    lRegPerLayer := FindByPersistenceLayerName(APersistenceLayerName)
  else
    lRegPerLayer := DefaultPerLayer;

  Assert(lRegPerLayer.TestValid(TtiPersistenceLayer), CTIErrorInvalidObject);

  if ADBConnectionName <> '' then
    lDBConnectionName := ADBConnectionName
  else
    lDBConnectionName := lRegPerLayer.DefaultDBConnectionName;

  lRegPerLayer.DBConnectionPools.UnLock(lDBConnectionName, ADatabase);
end;

function TtiPersistenceLayers.GetDefaultPerLayer: TtiPersistenceLayer;
begin
  Assert(FDefaultPerLayer.TestValid(TtiPersistenceLayer, true), CTIErrorInvalidObject);
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
  Assert(FDefaultPerLayer.TestValid(TtiPersistenceLayer), CTIErrorInvalidObject);
  result := FDefaultPerLayer;
end;

procedure TtiPersistenceLayers.SetDefaultPerLayer(const AValue: TtiPersistenceLayer);
begin
  FDefaultPerLayer := AValue;
end;

function TtiPersistenceLayers.GetDefaultPersistenceLayerName: string;
begin
  if DefaultPerLayer <> nil then
    result := DefaultPerLayer.PersistenceLayerName
  else
    result := '';
end;

procedure TtiPersistenceLayers.SetDefaultPersistenceLayerName(const AValue: string);
begin
  FDefaultPerLayer := FindByPersistenceLayerName(AValue);
end;

function TtiPersistenceLayer.TestConnectToDatabase(const ADatabaseName,
  AUserName, APassword, AParams: string): boolean;
begin
  Assert(DatabaseClass<>nil, 'DatabaseClass not assigned');
  result := DatabaseClass.TestConnectTo(ADatabaseName, AUserName, APassword,
                                           AParams);
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









