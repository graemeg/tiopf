unit tiOPFManager;

{$I tiDefines.inc}

interface
uses
   tiDBConnectionPool
  ,tiQuery
  ,tiObject
  ,tiPersistenceLayers
  ,tiVisitor
  ,tiVisitorDB
  ,tiAutoMap
  ,tiOID
  ,tiThread
  ,SysUtils
  ,Classes
  ,SyncObjs   // This unit must always appear after the Windows unit!
  ,Contnrs
 ;

const
  cErrorUnableToFindPerLayer = 'Unable to find persistence layer <%s>';
  cErrorCallToTIPerMgrWhileShuttingDown = 'Call to tiPerMgr while shutting down';
  cErrorAttemptToLoadPersistenceLayerMoreThanOnce = 'Attempt to load persistence layer <%s> more than once.';
  cErrorUnableToFindDefaultPerLayer = 'Unable to find default persistence layer';
  cErrorUnableToFindDefaultDatabase = 'Unable to find default database';


type

  // ToDo: TtiOPFManager should descend from TtiBaseObject, not TtiObject
  TtiOPFManager = class(TtiObject)
  private
    FPersistenceLayers : TtiPersistenceLayers;
    FDefaultPackageName: string;
    FVisitorManager: TtiObjectVisitorManager;
    FClassDBMappingMgr: TtiClassDBMappingMgr;
    FTerminated: boolean;
    FCriticalSection: TCriticalSection;
    FActiveThreadList: TtiActiveThreadList;
    FApplicationData: TObjectList;
    FApplicationStartTime: TDateTime;
    {$IFNDEF OID_AS_INT64}
      FDefaultOIDClassName: string;
      FOIDFactory: TOIDFactory;
      procedure SetDefaultOIDClassName(const AValue: string);
    {$ENDIF}

    function  GetDefaultDBConnectionName: string;
    procedure SetDefaultDBConnectionName(const AValue: string);
    function  GetDefaultDBConnectionPool: TtiDBConnectionPool;
    function  GetDefaultPerLayerName: string;
    function  GetClassDBMappingMgr: TtiClassDBMappingMgr;
    function  GetDefaultPerLayer: TtiPersistenceLayer;
    procedure SetDefaultPerLayer(const AValue: TtiPersistenceLayer);
    procedure SetDefaultPerLayerName(const AValue: string);
    function  GetApplicationData: TList;
  public
    constructor Create; override;
    destructor  Destroy; override;

    {: Load a persistence layer }
    procedure   LoadPersistenceLayer(Const APersistenceLayerName : string);
    {: Unload a persistence layer and all its database connections.
       If no parameter is passed, the default persistence layer will be unloaded.}
    procedure   UnLoadPersistenceLayer(const APersistenceLayerName : string = '');

    procedure   ConnectDatabase(           const ADatabaseAlias : string;
                                           const ADatabaseName : string;
                                           const AUserName    : string;
                                           const APassword    : string;
                                           const AParams      : string;
                                           const APersistenceLayerName : string); overload;

    procedure   ConnectDatabase(           const ADatabaseName : string;
                                           const AUserName    : string;
                                           const APassword    : string;
                                           const AParams      : string;
                                           const APersistenceLayerName : string); overload;

    procedure   ConnectDatabase(           const ADatabaseName : string;
                                           const AUserName    : string;
                                           const APassword    : string;
                                           const AParams      : string); overload;

    procedure   ConnectDatabase(           const ADatabaseName : string;
                                           const AUserName    : string;
                                           const APassword    : string); overload;

    // ToDo: Require an Out param that returns an exception message if there was onep
    function    TestThenConnectDatabase(   const ADatabaseAlias : string;
                                           const ADatabaseName : string;
                                           const AUserName    : string;
                                           const APassword    : string;
                                           const AParams      : string;
                                           const APersistenceLayerName : string): boolean; overload;

    function   TestThenConnectDatabase(    const ADatabaseName : string;
                                           const AUserName    : string;
                                           const APassword    : string;
                                           const AParams      : string;
                                           const APackageID   : string): boolean; overload;

    function   TestThenConnectDatabase(    const ADatabaseName : string;
                                           const AUserName    : string;
                                           const APassword    : string;
                                           const AParams      : string): boolean; overload;

    function    TestThenConnectDatabase(   const ADatabaseName : string;
                                           const AUserName    : string;
                                           const APassword    : string): boolean; overload;

    procedure   ConnectDatabaseWithRetry(  const ADatabaseName:  string;
                                           const AUserName:      string;
                                           const APassword:      string;
                                           const ARetryCount:    Word;
                                           const ARetryInterval: Word);

    procedure   DisconnectDatabase(        const ADatabaseName : string;
                                           const APackageID   : string); overload;
    procedure   DisconnectDatabase(        const ADatabaseName : string); overload;
    procedure   DisconnectDatabase; overload;

    // These register visitors
    procedure   RegisterVisitor(const AGroupName : string; const AClassRef  : TtiVisitorClass);
    procedure   RegReadPKVisitor(const AClassRef : TtiVisitorClass);
    procedure   RegReadThisVisitor(const AClassRef : TtiVisitorClass);
    procedure   RegReadVisitor(  const AClassRef : TtiVisitorClass);
    procedure   RegSaveVisitor(  const AClassRef : TtiVisitorClass);

    // ToDo: Remove these methods as they are implemented on TtiObject
    procedure   ReadPK(  const AVisited         : TtiVisited;
                          const ADBConnectionName : string = '';
                          const APersistenceLayerName    : string = ''); reintroduce;
    procedure   ReadThis(const AVisited : TtiVisited;
                          const ADBConnectionName : string = '';
                          const APersistenceLayerName    : string = ''); reintroduce;
    procedure   Read(    const AVisited : TtiVisited;
                          const ADBConnectionName : string = '';
                          const APersistenceLayerName    : string = ''); reintroduce;
    procedure   Save(    const AVisited : TtiVisited;
                          const ADBConnectionName : string = '';
                          const APersistenceLayerName    : string = ''); reintroduce;

    procedure   ExecSQL( const pSQL : string;
                          const ADBConnectionName : string = '';
                          const APersistenceLayerName    : string = '');

    // These execute database independant commands
    procedure   CreateDatabase(const ADatabaseName : string;
                                const AUserName    : string;
                                const pUserPassword : string;
                                const APackageID   : string = '');
    procedure   DropTable(  const ATableName       : TTableName;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = ''); overload;
    procedure   DropTable(  const ATableMetaData : TtiDBMetaDataTable;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = ''); overload;
    procedure   CreateTable(const ATableMetaData   : TtiDBMetaDataTable;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = '');
    procedure   DeleteRow(  const ATableName       : string;
                             const AWhere           : TtiQueryParams;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = ''); virtual;
    procedure   InsertRow(  const ATableName       : string;
                             const AParams          : TtiQueryParams;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = ''); virtual;
    procedure   UpdateRow(  const ATableName       : string;
                             const AWhere           : TtiQueryParams;
                             const AParams          : TtiQueryParams;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = ''); virtual;
    function    TableExists(const ATableName       : string;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = ''): boolean; virtual;

    procedure   ReadMetaDataTables(pDBMetaData : TtiDBMetaData;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = '');
    procedure   ReadMetaDataFields(pDBMetaDataTable : TtiDBMetaDataTable;
                             const ADBConnectionName : string = '';
                             const APersistenceLayerName    : string = '');
    procedure   Terminate;
    function    TerminateThreads(const Timeout : Integer=0): Boolean;
    property    Terminated : boolean read FTerminated write FTerminated;
    property    ActiveThreadList : TtiActiveThreadList read FActiveThreadList;
    property    ApplicationData : TList read GetApplicationData;
    property    ApplicationStartTime : TDateTime read FApplicationStartTime;

    property    DefaultPerLayer        : TtiPersistenceLayer    read GetDefaultPerLayer write SetDefaultPerLayer;
    property    DefaultPerLayerName    : string            read GetDefaultPerLayerName write SetDefaultPerLayerName;
    property    DefaultDBConnectionPool : TtiDBConnectionPool read GetDefaultDBConnectionPool;
    property    DefaultDBConnectionName : string            read GetDefaultDBConnectionName write SetDefaultDBConnectionName;

    property    PersistenceLayers      : TtiPersistenceLayers read FPersistenceLayers;
    property    VisitorManager         : TtiObjectVisitorManager read FVisitorManager;

    // ToDo: How to relate the ClassDBMappingMgr to a persistence layer -
    //       The code exists inside the ClassDBMappingMgr but is is stubbed out as it
    //       loads before a persistence layer is available hence is not working.
    property    ClassDBMappingMgr     : TtiClassDBMappingMgr read GetClassDBMappingMgr;

    {$IFNDEF OID_AS_INT64}
    property    DefaultOIDClassName    : string read FDefaultOIDClassName write SetDefaultOIDClassName;
    property    OIDFactory            : TOIDFactory read FOIDFactory;
    {$ENDIF}
  end;


function  GTIOPFManager: TtiOPFManager;
function  ShuttingDown: Boolean;
procedure FreeAndNilTIPerMgr;


const
  cuStandardTask_ReadPK   = 'StandardTask_ReadPK'  ;
  cuStandardTask_ReadThis = 'StandardTask_ReadThis';
  cuStandardTask_Read     = 'StandardTask_Read'    ;
  cuStandardTask_Save     = 'StandardTask_Save'    ;


implementation
uses
   tiUtils
  ,tiConstants
  ,tiExcept
  ,tiLog

  {$IFDEF LINK_ADOACCESS}    ,tiQueryADOAccess    {$ENDIF}
  {$IFDEF LINK_ADOSQLSERVER} ,tiQueryADOSQLServer {$ENDIF}
  {$IFDEF LINK_BDEPARADOX}   ,tiQueryBDEParadox   {$ENDIF}
  {$IFDEF LINK_CRSDAC}       ,tiQueryCrSdac       {$ENDIF}
  {$IFDEF LINK_CSV}          ,tiQueryCSV          {$ENDIF}
  {$IFDEF LINK_DOA}          ,tiQueryDOA          {$ENDIF}
  {$IFDEF LINK_FBL}          ,tiQueryFBL          {$ENDIF}
  {$IFDEF LINK_IBO}          ,tiQueryIBO          {$ENDIF}
  {$IFDEF LINK_IBX}          ,tiQueryIBX          {$ENDIF}
  {$IFDEF LINK_REMOTE}       ,tiQueryRemote       {$ENDIF}
  {$IFDEF LINK_SQLDB_IB}     ,tiQuerySqldbIB      {$ENDIF}
  {$IFDEF LINK_TAB}          ,tiQueryTAB          {$ENDIF}
  {$IFDEF LINK_XML}          ,tiQueryXML          {$ENDIF}
  {$IFDEF LINK_XMLLIGHT}     ,tiQueryXMLLight     {$ENDIF}
  {$IFDEF LINK_ZEOS_FB10}    ,tiQueryZeosFB10     {$ENDIF}
  {$IFDEF LINK_ZEOS_FB15}    ,tiQueryZeosFB15     {$ENDIF}
  {$IFDEF LINK_ZEOS_MYSQL41} ,tiQueryZeosMySQL41  {$ENDIF}
  {$IFDEF LINK_ZEOS_MYSQL50} ,tiQueryZeosMySQL50  {$ENDIF}
  {$IFDEF LINK_DBISAM4}      ,tiQueryDBISAM4      {$ENDIF}
  {$IFDEF LINK_ASQLITE3}     ,tiQueryAsqlite3     {$ENDIF}
  {$IFNDEF FPC}
  ,Forms
  {$ENDIF}
 ;


var
  UTIOPFManager: TtiOPFManager;
  UShuttingDown: Boolean;


function GTIOPFManager : TtiOPFManager;
begin
  if uTIOPFManager = nil then
  begin
    if ShuttingDown then
      raise Exception.Create(cErrorCallToTIPerMgrWhileShuttingDown);
    uTIOPFManager := TtiOPFManager.Create;
  end;
  result := uTIOPFManager;
end;


procedure FreeAndNilTIPerMgr;
begin
  FreeAndNil(uTIOPFManager);
end;


function ShuttingDown: Boolean;
begin
  Result := uShuttingDown;
end;


function TtiOPFManager.GetDefaultDBConnectionName: string;
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer;
  if lRegPerLayer <> nil then
    result := lRegPerLayer.DefaultDBConnectionName
  else
    result := '';
end;


procedure TtiOPFManager.SetDefaultDBConnectionName(const AValue: string);
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer;
  if lRegPerLayer <> nil then
    lRegPerLayer.DefaultDBConnectionName := AValue;
end;


procedure TtiOPFManager.ConnectDatabase(const ADatabaseAlias, ADatabaseName,
  AUserName, APassword, AParams, APersistenceLayerName: string);
var
  LPersistenceLayer: TtiPersistenceLayer;
begin

  if APersistenceLayerName = '' then
    LPersistenceLayer:= DefaultPerLayer
  else
    LPersistenceLayer := FPersistenceLayers.FindByPerLayerName(APersistenceLayerName);

  if LPersistenceLayer = nil then
    raise EtiOPFInternalException.CreateFmt(cErrorUnableToFindPerLayer,[APersistenceLayerName]);

  LPersistenceLayer.DBConnectionPools.Connect(
    ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams);

  if LPersistenceLayer.DefaultDBConnectionName = '' then
     LPersistenceLayer.DefaultDBConnectionName := ADatabaseName;
end;

constructor TtiOPFManager.Create;
begin
  inherited;
  FCriticalSection := TCriticalSection.Create;
  FPersistenceLayers := TtiPersistenceLayers.Create;
  FVisitorManager := TtiObjectVisitorManager.Create(Self);

  FDefaultPackageName := '';
  FTerminated := false;

  {$IFNDEF OID_AS_INT64}
  FOIDFactory := TOIDFactory.Create;
  {$ENDIF}

  FActiveThreadList := TtiActiveThreadList.Create;
  FApplicationData := TObjectList.Create(true);
  FApplicationStartTime := Now;
end;


destructor TtiOPFManager.Destroy;
begin
  Terminate;
  FActiveThreadList.Free;
  FVisitorManager.Free;
  {$IFNDEF OID_AS_INT64}
    FOIDFactory.Free;
  {$ENDIF}
  FClassDBMappingMgr.Free;
  FPersistenceLayers.Free;
  FApplicationData.Free;
  FCriticalSection.Free;
  inherited;
end;


procedure TtiOPFManager.LoadPersistenceLayer(const APersistenceLayerName : string);
begin
  // ToDo: Terminated must be related to each loaded persistence layer. This
  //       would make it possible to terminate a single layer at the time.
  FTerminated := false;
  PersistenceLayers.LoadPersistenceLayer(APersistenceLayerName);
end;


function TtiOPFManager.GetDefaultDBConnectionPool: TtiDBConnectionPool;
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer;
  if lRegPerLayer <> nil then
    result := lRegPerLayer.DefaultDBConnectionPool
  else
    result := nil
end;


function TtiOPFManager.GetDefaultPerLayerName: string;
var
  LPersistenceLayer : TtiPersistenceLayer;
begin
  LPersistenceLayer := DefaultPerLayer;
  if LPersistenceLayer <> nil then
    result := LPersistenceLayer.PersistenceLayerName
  else
    result := '';
end;


procedure TtiOPFManager.RegReadPKVisitor(const AClassRef: TtiVisitorClass);
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.RegisterVisitor(cuStandardTask_ReadPK, AClassRef);
end;


procedure TtiOPFManager.RegReadVisitor(const AClassRef: TtiVisitorClass);
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.RegisterVisitor(cuStandardTask_Read, AClassRef);
end;


procedure TtiOPFManager.RegSaveVisitor(const AClassRef: TtiVisitorClass);
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.RegisterVisitor(cuStandardTask_Save, AClassRef);
end;


procedure TtiOPFManager.Read(const AVisited         : TtiVisited;
                        const ADBConnectionName : string = '';
                        const APersistenceLayerName    : string = '');
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.Execute(cuStandardTask_Read,
    AVisited,
    ADBConnectionName,
    APersistenceLayerName);
end;


procedure TtiOPFManager.ReadPK(const AVisited         : TtiVisited;
                          const ADBConnectionName : string = '';
                          const APersistenceLayerName    : string = '');
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.Execute(cuStandardTask_ReadPK,
    AVisited,
    ADBConnectionName,
    APersistenceLayerName);
end;


procedure TtiOPFManager.Save(const AVisited         : TtiVisited;
                        const ADBConnectionName : string = '';
                        const APersistenceLayerName    : string = '');
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.Execute(cuStandardTask_Save,
    AVisited,
    ADBConnectionName,
    APersistenceLayerName);
end;


procedure TtiOPFManager.ConnectDatabase(
  const ADatabaseName : string;
  const AUserName    : string;
  const APassword : string;
  const AParams      : string;
  const APersistenceLayerName   : string);
begin
  ConnectDatabase(ADatabaseName, ADatabaseName, AUserName, APassword, AParams, APersistenceLayerName);
end;

function TtiOPFManager.GetClassDBMappingMgr: TtiClassDBMappingMgr;
begin
  if FClassDBMappingMgr = nil then
  begin
    FClassDBMappingMgr := TtiClassDBMappingMgr.Create;
    FClassDBMappingMgr.Owner := self;
    // Register the visitors that work with the persistence mapping classes
    VisitorManager.RegisterVisitor(cuStandardTask_ReadPK,   TVisAutoCollectionPKRead);
    VisitorManager.RegisterVisitor(cuStandardTask_ReadThis, TVisAutoReadThis);
    VisitorManager.RegisterVisitor(cuStandardTask_Read,     TVisAutoReadThis);
    VisitorManager.RegisterVisitor(cuStandardTask_Read,     TVisAutoCollectionRead);
    VisitorManager.RegisterVisitor(cuStandardTask_Save,     TVisAutoDelete);
    VisitorManager.RegisterVisitor(cuStandardTask_Save,     TVisAutoUpdate);
    VisitorManager.RegisterVisitor(cuStandardTask_Save,     TVisAutoCreate);
  end;
  result := FClassDBMappingMgr;
end;

function TtiOPFManager.TerminateThreads(const Timeout : Integer=0): Boolean;
var
  LStart: Cardinal;
  ACheckFor : Cardinal;
begin
  Result := false;
  ACheckFor := Timeout*1000;
  LStart := tiGetTickCount;
  ActiveThreadList.Terminate;
  while ActiveThreadList.Count >0 do
  begin
    Sleep(10);
    {$IFNDEF FPC}
    Application.ProcessMessages;
    {$ENDIF}
    if (ACheckFor>0) and ((tiGetTickCount - LStart) > ACheckFor) then Exit;
  end;
  Result := (ActiveThreadList.Count =0);
end;

procedure TtiOPFManager.Terminate;
begin
  FCriticalSection.Enter;
  try
    FTerminated := true;
  finally
    FCriticalSection.Leave;
  end;
  FActiveThreadList.Terminate;
end;

procedure TtiOPFManager.ExecSQL(const pSQL             : string;
                            const ADBConnectionName : string = '';
                            const APersistenceLayerName    : string = '');
var
  LDBConnectionName : string;
  LDatabase: TtiDatabase;
begin
Assert(APersistenceLayerName = '', 'Not implemented whe pPreLayerName <> ''');
  Assert(DefaultPerLayer <> nil, 'DefaultPerLayer not assigned');
  if ADBConnectionName = '' then
    LDBConnectionName := DefaultDBConnectionName
  else
    LDBConnectionName := ADBConnectionName;
  LDatabase:= DefaultPerLayer.DBConnectionPools.Lock(LDBConnectionName);
  try
    LDatabase.ExecSQL(pSQL);
  finally
    DefaultPerLayer.DBConnectionPools.UnLock(LDBConnectionName, LDatabase);
  end;
end;


procedure TtiOPFManager.DisconnectDatabase(
  const ADatabaseName : string;
  const APackageID   : string);
var
  lRegPerLayer  : TtiPersistenceLayer;
begin
  Assert(ADatabaseName <> '', 'ADatabaseName not assigned');
  Assert(APackageID <> '', 'APackageID not assigned');

  lRegPerLayer := FPersistenceLayers.FindByPerLayerName(APackageID);
  if lRegPerLayer = nil then
    raise EtiOPFInternalException.CreateFmt(cErrorUnableToFindPerLayer,[APackageID]);

  if lRegPerLayer.NextOIDMgr.FindByDatabaseName(ADatabaseName) <> nil then
    lRegPerLayer.NextOIDMgr.UnloadNextOIDGenerator(ADatabaseName);

  if (SameText(lRegPerLayer.DefaultDBConnectionName, ADatabaseName)) then
    lRegPerLayer.DefaultDBConnectionName := '';
  lRegPerLayer.DBConnectionPools.DisConnect(ADatabaseName);

end;


procedure TtiOPFManager.CreateTable(const ATableMetaData: TtiDBMetaDataTable;
                                const ADBConnectionName : string = '';
                                const APersistenceLayerName    : string = '');
var
  lDB : TtiDatabase;
begin
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
    lDB.CreateTable(ATableMetaData);
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


procedure TtiOPFManager.DropTable(const ATableName: TTableName;
                              const ADBConnectionName: string = '';
                              const APersistenceLayerName : string = '');
var
  lDB : TtiDatabase;
begin
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
    lDB.DropTable(ATableName);
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


procedure TtiOPFManager.DropTable(const ATableMetaData   : TtiDBMetaDataTable;
                              const ADBConnectionName : string = '';
                              const APersistenceLayerName    : string = '');
var
  lDB : TtiDatabase;
begin
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
    lDB.DropTable(ATableMetaData);
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


procedure TtiOPFManager.DeleteRow(const ATableName       : string;
                              const AWhere           : TtiQueryParams;
                              const ADBConnectionName : string = '';
                              const APersistenceLayerName    : string = '');
var
  lDB : TtiDatabase;
begin
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
    lDB.DeleteRow(ATableName, AWhere);
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


procedure TtiOPFManager.InsertRow(const ATableName       : string;
                              const AParams          : TtiQueryParams;
                              const ADBConnectionName : string = '';
                              const APersistenceLayerName    : string = '');
var
  lDB : TtiDatabase;
begin
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
    lDB.InsertRow(ATableName, AParams);
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


procedure TtiOPFManager.UpdateRow(const ATableName : string;
                              const AWhere    : TtiQueryParams;
                              const AParams   : TtiQueryParams;
                              const ADBConnectionName : string = '';
                              const APersistenceLayerName    : string = '');
var
  lDB : TtiDatabase;
begin
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
   lDB.UpdateRow(ATableName, AWhere, AParams);
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


procedure TtiOPFManager.RegReadThisVisitor(const AClassRef: TtiVisitorClass);
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.RegisterVisitor(cuStandardTask_ReadThis, AClassRef);
end;


procedure TtiOPFManager.ReadThis(const AVisited         : TtiVisited;
                            const ADBConnectionName : string = '';
                            const APersistenceLayerName    : string = '');
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.Execute(cuStandardTask_ReadThis,
    AVisited,
    ADBConnectionName,
    APersistenceLayerName);
end;


function TtiOPFManager.GetDefaultPerLayer: TtiPersistenceLayer;
begin
  result := FPersistenceLayers.DefaultPerLayer;
end;

function TtiOPFManager.TableExists(const ATableName: string;
                               const ADBConnectionName: string = '';
                               const APersistenceLayerName : string = ''): boolean;
var
  lDBMetaData : TtiDBMetaData;
  lDB : TtiDatabase;
begin
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
    lDBMetaData := TtiDBMetaData.Create;
    try
      lDB.ReadMetaDataTables(lDBMetaData);
      result := lDBMetaData.FindByTableName(ATableName) <> nil;
    finally
      lDBMetaData.Free;
    end;
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


procedure TtiOPFManager.UnLoadPersistenceLayer(const APersistenceLayerName: string);
begin
  PersistenceLayers.UnLoadPersistenceLayer(APersistenceLayerName);
end;


procedure TtiOPFManager.SetDefaultPerLayer(const AValue: TtiPersistenceLayer);
begin
  FPersistenceLayers.DefaultPerLayer := AValue;
end;


procedure TtiOPFManager.SetDefaultPerLayerName(const AValue: string);
begin
  FPersistenceLayers.DefaultPerLayerName := AValue;
end;

procedure TtiOPFManager.ReadMetaDataFields(
  pDBMetaDataTable: TtiDBMetaDataTable; const ADBConnectionName,
  APersistenceLayerName: string);
var
  lDB : TtiDatabase;
begin
  Assert(pDBMetaDataTable.TestValid(TtiDBMetaDataTable), CTIErrorInvalidObject);
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
    lDB.ReadMetaDataFields(pDBMetaDataTable);
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


procedure TtiOPFManager.ReadMetaDataTables(pDBMetaData: TtiDBMetaData;
  const ADBConnectionName, APersistenceLayerName: string);
var
  lDB : TtiDatabase;
begin
  Assert(pDBMetaData.TestValid(TtiDBMetaData), CTIErrorInvalidObject);
  lDB := PersistenceLayers.LockDatabase(ADBConnectionName, APersistenceLayerName);
  try
    lDB.ReadMetaDataTables(pDBMetaData);
  finally
    PersistenceLayers.UnLockDatabase(lDB, ADBConnectionName, APersistenceLayerName);
  end;
end;


{$IFNDEF OID_AS_INT64}
  procedure TtiOPFManager.SetDefaultOIDClassName(const AValue: string);
  begin
    FDefaultOIDClassName := AValue;
  end;
{$ENDIF}


function TtiOPFManager.GetApplicationData: TList;
begin
  result := FApplicationData;
end;


function TtiOPFManager.TestThenConnectDatabase(
  const ADatabaseName : string;
  const AUserName    : string;
  const APassword : string;
  const AParams      : string;
  const APackageID   : string): boolean;
begin
  result:= TestThenConnectDatabase(
    ADatabaseName, ADatabaseName, AUserName,
    APassword, AParams, APackageID);
end;


procedure TtiOPFManager.CreateDatabase(const ADatabaseName, AUserName,
  pUserPassword, APackageID: string);
var
  LPersistenceLayer : TtiPersistenceLayer;
begin
  LPersistenceLayer := PersistenceLayers.FindByPerLayerName(APackageID);
  if LPersistenceLayer = nil then
    raise EtiOPFInternalException.CreateFmt(cErrorUnableToFindPerLayer,[APackageID]);
  LPersistenceLayer.DatabaseClass.CreateDatabase(ADatabaseName, AUserName, pUserPassword);
end;


procedure TtiOPFManager.RegisterVisitor(const AGroupName: string; const AClassRef: TtiVisitorClass);
begin
  Assert(FVisitorManager.TestValid, CTIErrorInvalidObject);
  FVisitorManager.RegisterVisitor(AGroupName, AClassRef);
end;


procedure TtiOPFManager.ConnectDatabase(const ADatabaseName, AUserName, APassword, AParams: string);
begin
  ConnectDatabase(ADatabaseName, ADatabaseName, AUserName, APassword, AParams, '');
end;


procedure TtiOPFManager.ConnectDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  ConnectDatabase(ADatabaseName, AUserName, APassword, '');
end;


procedure TtiOPFManager.ConnectDatabaseWithRetry(const ADatabaseName, AUserName,
  APassword: string; const ARetryCount, ARetryInterval: Word);

  procedure LogDatabaseConnectionAttempt(const ADatabaseName: string;
    const AUserName: string; const ARetryCount: Integer);
  begin
    Log('Attempt %d to connect to database %s as %s',
      [ARetryCount, ADatabaseName, AUserName], lsUserInfo);
  end;

var
  LRetryCount: Word;
begin
  // ToDo: Must add more detail describing why a connection could not be established.
  //       For example, incorrect username/password does not warrent a retry
  //       Database unavailable does.
  LRetryCount:= 1;
  LogDatabaseConnectionAttempt(ADatabaseName, AUserName, LRetryCount);
  while (LRetryCount <= ARetryCount) and
    (not gTIOPFManager.TestThenConnectDatabase(
      ADatabaseName, AUsername, APassword, '')) do
  begin
    Sleep(ARetryInterval * 1000);
    Inc(LRetryCount);
    if LRetryCount <= ARetryCount then
      LogDatabaseConnectionAttempt(ADatabaseName, AUserName, LRetryCount);
  end;

  if LRetryCount > ARetryCount then
    raise EtiOPFDBExceptionCanNotConnect.Create(
      GTIOPFManager.DefaultPerLayerName,
      ADatabaseName,
      AUserName,
      CPasswordMasked,
      Format(CTIOPFExcMsgCanNotConnectToDatabaseAfterRetry,
             [ARetryCount, ARetryInterval]));

  Log('Connecting to database successful' + CrLf +
      gTIOPFManager.DefaultDBConnectionPool.DetailsAsString, lsUserInfo);

end;

procedure TtiOPFManager.DisconnectDatabase(const ADatabaseName: string);
var
  LPersistenceLayer: TtiPersistenceLayer;
begin
  LPersistenceLayer := DefaultPerLayer;
  if LPersistenceLayer = nil then
    raise EtiOPFInternalException.Create(cErrorUnableToFindDefaultPerLayer);
  DisconnectDatabase(ADatabaseName, LPersistenceLayer.PersistenceLayerName);
end;


function TtiOPFManager.TestThenConnectDatabase(const ADatabaseName,
  AUserName, APassword, AParams: string): boolean;
begin
  Result := TestThenConnectDatabase(ADatabaseName, AUserName, APassword, AParams, '');
end;


function TtiOPFManager.TestThenConnectDatabase(const ADatabaseName,
  AUserName, APassword: string): boolean;
begin
  Result := TestThenConnectDatabase(ADatabaseName, AUserName, APassword, '');
end;


function TtiOPFManager.TestThenConnectDatabase(const ADatabaseAlias,
  ADatabaseName, AUserName, APassword, AParams,
  APersistenceLayerName: string): boolean;
var
  LPersistenceLayer  : TtiPersistenceLayer;
begin

  if APersistenceLayerName = '' then
    LPersistenceLayer:= DefaultPerLayer
  else
    LPersistenceLayer := FPersistenceLayers.FindByPerLayerName(APersistenceLayerName);

  if LPersistenceLayer = nil then
    raise EtiOPFInternalException.CreateFmt(cErrorUnableToFindPerLayer,[APersistenceLayerName]);

  if LPersistenceLayer.DBConnectionPools.IsConnected(ADatabaseAlias) then
    result := true  // Assume OK if already connect (Warning: Could be a different user)
  else
    if LPersistenceLayer.DatabaseClass.TestConnectTo(ADatabaseName, AUserName, APassword, AParams) then
    begin
      LPersistenceLayer.DBConnectionPools.Connect(ADatabaseAlias, ADatabaseName, AUserName, APassword, AParams);
      result := true;
    end else
      Result := false;
end;

procedure TtiOPFManager.DisconnectDatabase;
var
  LPersistenceLayer: TtiPersistenceLayer;
  LDatabaseName: string;
begin
  LPersistenceLayer := DefaultPerLayer;
  Assert(LPersistenceLayer <> nil, cErrorUnableToFindDefaultPerLayer);
  LDatabaseName := LPersistenceLayer.DefaultDBConnectionName;
  Assert(LDatabaseName <> '', cErrorUnableToFindDefaultDatabase);
  DisconnectDatabase(LDatabaseName, LPersistenceLayer.PersistenceLayerName);
end;


initialization
  uShuttingDown := False;

  // This works if there is only one persistence layer linked in via a compiler
  // directive, but not if there are more.
  // Have added this code to solve the problem of forcing the correct default per layer
  // when the remote layer is pulled in from code, and a SQL layer is required as well.
  {$IFDEF LINK_ADOACCESS}     gTIOPFManager.DefaultPerLayerName := cTIPersistADOAccess;   {$ENDIF}
  {$IFDEF LINK_ADOSQLSERVER}  gTIOPFManager.DefaultPerLayerName := cTIPersistADOSQLServer;{$ENDIF}
  {$IFDEF LINK_BDEPARADOX}    gTIOPFManager.DefaultPerLayerName := cTIPersistBDEParadox;  {$ENDIF}
  {$IFDEF LINK_CSV}           gTIOPFManager.DefaultPerLayerName := cTIPersistCSV;         {$ENDIF}
  {$IFDEF LINK_DOA}           gTIOPFManager.DefaultPerLayerName := cTIPersistDOA;         {$ENDIF}
  {$IFDEF LINK_FBL}           gTIOPFManager.DefaultPerLayerName := cTIPersistFBL;         {$ENDIF}
  {$IFDEF LINK_IBO}           gTIOPFManager.DefaultPerLayerName := cTIPersistIBO;         {$ENDIF}
  {$IFDEF LINK_IBX}           gTIOPFManager.DefaultPerLayerName := cTIPersistIBX;         {$ENDIF}
  {$IFDEF LINK_REMOTE}        gTIOPFManager.DefaultPerLayerName := cTIPersistRemote;      {$ENDIF}
  {$IFDEF LINK_SQLDB_IB}      gTIOPFManager.DefaultPerLayerName := cTIPersistSqldbIB;     {$ENDIF}
  {$IFDEF LINK_TAB}           gTIOPFManager.DefaultPerLayerName := cTIPersistTAB;         {$ENDIF}
  {$IFDEF LINK_XML}           gTIOPFManager.DefaultPerLayerName := cTIPersistXML;         {$ENDIF}
  {$IFDEF LINK_XMLLIGHT}      gTIOPFManager.DefaultPerLayerName := cTIPersistXMLLight;    {$ENDIF}
  {$IFDEF LINK_ZEOS_FB10}     gTIOPFManager.DefaultPerLayerName := cTIPersistZeosFB10;    {$ENDIF}
  {$IFDEF LINK_ZEOS_FB15}     gTIOPFManager.DefaultPerLayerName := cTIPersistZeosFB15;    {$ENDIF}
  {$IFDEF LINK_ZEOS_MySQLl50} gTIOPFManager.DefaultPerLayerName := cTIPersistZeosMySQL50; {$ENDIF}
  {$IFDEF LINK_DBISAM4}       gTIOPFManager.DefaultPerLayerName := cTIPersistDBISAM4;     {$ENDIF}

finalization
  uShuttingDown := True;
  FreeAndNilTIPerMgr;

end.
