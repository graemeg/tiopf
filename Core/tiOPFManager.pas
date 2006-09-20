unit tiOPFManager;

{$I tiDefines.inc}

interface
uses
   SysUtils
  ,Classes
  ,tiDBConnectionPool
  ,tiQuery
  ,tiObject
  ,tiPersistenceLayers
  ,tiVisitor
  ,tiClassToDBMap_BOM
  ,tiClassToDBMap_Srv
  ,tiOID
  ,tiThread
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
  TtiOPFManager = class(TtiObjectList)
  private
    FPersistenceLayers : TtiPersistenceLayers;
    FDefaultPackageName: string;
    FVisitorManager: TtiVisitorManager;
    FClassDBMappingMgr: TtiClassDBMappingMgr;
    FTerminated: boolean;
    FCriticalSection: TCriticalSection;
    FTerminateOnFailedDBConnection: boolean;
    FActiveThreadList: TtiActiveThreadList;
    FApplicationData: TObjectList;
    FApplicationStartTime: TDateTime;
    {$IFNDEF OID_AS_INT64}
      FDefaultOIDClassName: string;
      FOIDFactory: TOIDFactory;
      procedure SetDefaultOIDClassName(const Value: string);
    {$ENDIF}

    function  GetDefaultDBConnectionName: string;
    procedure SetDefaultDBConnectionName(const Value: string);
    function  GetDefaultDBConnectionPool: TDBConnectionPool;
    function  GetDefaultPerLayerName: string;
    function  GetClassDBMappingMgr: TtiClassDBMappingMgr;
    function  GetDefaultPerLayer: TtiPersistenceLayer;
    procedure SetDefaultPerLayer(const Value: TtiPersistenceLayer);
    procedure SetDefaultPerLayerName(const Value: string);
    function  GetApplicationData: TList;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;

    {: Load a persistence layer }
    procedure   LoadPersistenceLayer( Const pPackageID : string ) ;
    {: Unload a persistence layer and all its database connections.
       If no parameter is passed, the default persistence layer will be unloaded.}
    procedure   UnLoadPersistenceLayer( const pPackageID : string = '' ) ;

    procedure   ConnectDatabase(           const ADatabaseName : string;
                                           const AUserName     : string;
                                           const APassword     : string;
                                           const AParams       : string;
                                           const APackageID    : string); overload ;

    procedure   ConnectDatabase(           const ADatabaseName : string;
                                           const AUserName     : string;
                                           const APassword     : string;
                                           const AParams       : string); overload ;

    procedure   ConnectDatabase(           const ADatabaseName : string;
                                           const AUserName     : string;
                                           const APassword     : string); overload ;

    function   TestThenConnectDatabase(    const ADatabaseName : string;
                                           const AUserName     : string;
                                           const APassword     : string;
                                           const AParams       : string;
                                           const APackageID    : string): boolean; overload ;

    function   TestThenConnectDatabase(    const ADatabaseName : string;
                                           const AUserName     : string;
                                           const APassword     : string;
                                           const AParams       : string): boolean; overload ;

    function    TestThenConnectDatabase(   const ADatabaseName : string;
                                           const AUserName     : string;
                                           const APassword     : string): boolean; overload ;

    procedure   DisconnectDatabase(        const ADatabaseName : string;
                                           const APackageID    : string); overload ;
    procedure   DisconnectDatabase(        const ADatabaseName : string ); overload ;
    procedure   DisconnectDatabase ; overload ;

    // These register visitors
    procedure   RegisterVisitor( const psGroupName : string ; const pClassRef   : TVisClassRef ) ;
    procedure   RegReadPKVisitor( const pClassRef : TVisClassRef ) ;
    procedure   RegReadThisVisitor( const pClassRef : TVisClassRef ) ;
    procedure   RegReadVisitor(   const pClassRef : TVisClassRef ) ;
    procedure   RegSaveVisitor(   const pClassRef : TVisClassRef ) ;

    // These call visitors
    function    ReadPK(   const pVisited          : TtiVisited ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ): string; reintroduce;
    function    ReadThis( const pVisited : TtiVisited ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ): string; reintroduce;
    function    Read(     const pVisited : TtiVisited ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ): string; reintroduce;
    function    Save(     const pVisited : TtiVisited ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = ''): string; reintroduce;
    procedure   ExecSQL(  const pSQL : string ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' );

    // These execute database independant commands
    function    CreateDatabase( const pDatabaseName : string ;
                                const pUserName     : string ;
                                const pUserPassword : string ;
                                const pPackageID    : string = '' ): string ;
    procedure   DropTable(   const pTableName        : TTableName ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '' ) ; overload ;
    procedure   DropTable(   const pTableMetaData : TtiDBMetaDataTable ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '' ) ; overload ;
    procedure   CreateTable( const pTableMetaData    : TtiDBMetaDataTable ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '') ;
    procedure   DeleteRow(   const pTableName        : string ;
                             const pWhere            : TtiQueryParams ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '' ) ; virtual ;
    procedure   InsertRow(   const pTableName        : string ;
                             const pParams           : TtiQueryParams ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '' ) ; virtual ;
    procedure   UpdateRow(   const pTableName        : string ;
                             const pWhere            : TtiQueryParams ;
                             const pParams           : TtiQueryParams ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '' ) ; virtual ;
    function    TableExists( const pTableName        : string ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '' ) : boolean ; virtual ;

    procedure   ReadMetaDataTables( pDBMetaData : TtiDBMetaData ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '' ) ;
    procedure   ReadMetaDataFields( pDBMetaDataTable : TtiDBMetaDataTable ;
                             const pDBConnectionName : string = '' ;
                             const pPerLayerName     : string = '' ) ;
    procedure   Terminate ;
    function    TerminateThreads(const Timeout : Integer=0) : Boolean;
    property    Terminated : boolean read FTerminated write FTerminated ;
    property    TerminateOnFailedDBConnection : boolean read FTerminateOnFailedDBConnection write FTerminateOnFailedDBConnection ;
    property    ActiveThreadList : TtiActiveThreadList read FActiveThreadList ;
    property    ApplicationData : TList read GetApplicationData ;
    property    ApplicationStartTime : TDateTime read FApplicationStartTime ;

  published
    property    DefaultPerLayer         : TtiPersistenceLayer    read GetDefaultPerLayer write SetDefaultPerLayer ;
    property    DefaultPerLayerName     : string            read GetDefaultPerLayerName write SetDefaultPerLayerName ;
    property    DefaultDBConnectionPool : TDBConnectionPool read GetDefaultDBConnectionPool ;
    property    DefaultDBConnectionName : string            read GetDefaultDBConnectionName write SetDefaultDBConnectionName ;

    property    PersistenceLayers       : TtiPersistenceLayers read FPersistenceLayers ;
    property    VisMgr                  : TtiVisitorManager read FVisitorManager ; // Don't use VisMgr. It will be removed
    property    VisitorManager          : TtiVisitorManager read FVisitorManager ;

    // ToDo: How to relate the ClassDBMappingMgr to a persistence layer -
    //       The code exists inside the ClassDBMappingMgr but is is stubbed out as it
    //       loads before a persistence layer is available hence is not working.
    property    ClassDBMappingMgr      : TtiClassDBMappingMgr read GetClassDBMappingMgr ;

    {$IFNDEF OID_AS_INT64}
    property    DefaultOIDClassName     : string read FDefaultOIDClassName write SetDefaultOIDClassName ;
    property    OIDFactory             : TOIDFactory read FOIDFactory ;
    {$ENDIF}
  end;


function  gTIOPFManager: TtiOPFManager;
function  ShuttingDown: Boolean;
procedure FreeAndNilTIPerMgr;


const
  cuStandardTask_ReadPK   = 'StandardTask_ReadPK'   ;
  cuStandardTask_ReadThis = 'StandardTask_ReadThis' ;
  cuStandardTask_Read     = 'StandardTask_Read'     ;
  cuStandardTask_Save     = 'StandardTask_Save'     ;


implementation
uses
   tiCommandLineParams
//  ,INIFiles
  ,tiUtils
  ,tiLog
  ,tiConstants
  ,tiRegINI
  ,tiExcept

  {$IFDEF LINK_XML}          ,tiQueryXML          {$ENDIF}
  {$IFDEF LINK_IBX}          ,tiQueryIBX          {$ENDIF}
  {$IFDEF LINK_BDEPARADOX}   ,tiQueryBDEParadox   {$ENDIF}
  {$IFDEF LINK_ADOACCESS}    ,tiQueryADOAccess    {$ENDIF}
  {$IFDEF LINK_ADOSQLSERVER} ,tiQueryADOSQLServer {$ENDIF}
  {$IFDEF LINK_CSV}          ,tiQueryCSV          {$ENDIF}
  {$IFDEF LINK_TAB}          ,tiQueryTAB          {$ENDIF}
  {$IFDEF LINK_XMLLIGHT}     ,tiQueryXMLLight     {$ENDIF}
  {$IFDEF LINK_DOA}          ,tiQueryDOA          {$ENDIF}
  {$IFDEF LINK_REMOTE}       ,tiQueryRemote       {$ENDIF}
  {$IFDEF LINK_SQLDB_IB}     ,tiQuerySqldbIB      {$ENDIF}
  {$IFDEF LINK_FBL}          ,tiQueryFBL          {$ENDIF}

  ,Forms
  ;


var
  uTIOPFManager : TtiOPFManager ;
  uShuttingDown: Boolean;


function gTIOPFManager : TtiOPFManager ;
begin
  if uTIOPFManager = nil then
  begin
    if ShuttingDown then
      raise Exception.Create(cErrorCallToTIPerMgrWhileShuttingDown);
    uTIOPFManager := TtiOPFManager.Create ;
  end;
  result := uTIOPFManager ;
end ;


procedure FreeAndNilTIPerMgr ;
begin
  FreeAndNil(uTIOPFManager);
end;


function ShuttingDown: Boolean ;
begin
  Result := uShuttingDown;
end;


function TtiOPFManager.GetDefaultDBConnectionName: string;
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer <> nil then
    result := lRegPerLayer.DefaultDBConnectionName
  else
    result := '' ;
end;


procedure TtiOPFManager.SetDefaultDBConnectionName( const Value: string);
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer <> nil then
    lRegPerLayer.DefaultDBConnectionName := Value ;
end;


constructor TtiOPFManager.Create;
begin
  inherited ;
  FCriticalSection := TCriticalSection.Create ;
  FPersistenceLayers := TtiPersistenceLayers.Create ;
  FVisitorManager := TtiVisitorManager.Create ;

  FDefaultPackageName := '' ;
  FTerminated := false ;

  {$IFNDEF OID_AS_INT64}
  FOIDFactory := TOIDFactory.Create ;
  {$ENDIF}

  FTerminateOnFailedDBConnection := true ;
  FActiveThreadList := TtiActiveThreadList.Create ;
  FApplicationData  := TObjectList.Create(true) ;
  FApplicationStartTime := Now ;
end;


destructor TtiOPFManager.Destroy;
begin
  Terminate;
  FVisitorManager.Free ;
  {$IFNDEF OID_AS_INT64}
    FOIDFactory.Free ;
  {$ENDIF}
  FClassDBMappingMgr.Free;
  FPersistenceLayers.Free ;
  FActiveThreadList.Free;
  FApplicationData.Free;
  FCriticalSection.Free;
  inherited;
end;


procedure TtiOPFManager.LoadPersistenceLayer(const pPackageID : string );
begin
  // ToDo: Terminated must be related to each loaded persistence layer. This
  //       would make it possible to terminate a single layer at the time.
  FTerminated := false ;
  PersistenceLayers.LoadPersistenceLayer(pPackageID);
end;


function TtiOPFManager.GetDefaultDBConnectionPool: TDBConnectionPool;
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer <> nil then
    result := lRegPerLayer.DefaultDBConnectionPool
  else
    result := nil
end;


function TtiOPFManager.GetDefaultPerLayerName: string;
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer <> nil then
    result := lRegPerLayer.PerLayerName
  else
    result := ''
end;


procedure TtiOPFManager.RegReadPKVisitor(const pClassRef: TVisClassRef);
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  FVisitorManager.RegisterVisitor( cuStandardTask_ReadPK, pClassRef ) ;
end;


procedure TtiOPFManager.RegReadVisitor(const pClassRef: TVisClassRef);
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  FVisitorManager.RegisterVisitor( cuStandardTask_Read, pClassRef ) ;
end;


procedure TtiOPFManager.RegSaveVisitor(const pClassRef: TVisClassRef);
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  FVisitorManager.RegisterVisitor( cuStandardTask_Save, pClassRef ) ;
end;


function TtiOPFManager.Read(const pVisited          : TtiVisited;
                        const pDBConnectionName : string = '';
                        const pPerLayerName     : string = ''): string;
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  result :=
      FVisitorManager.Execute( cuStandardTask_Read,
                       pVisited,
                       pDBConnectionName,
                       pPerLayerName ) ;
end;


function TtiOPFManager.ReadPK(const pVisited          : TtiVisited;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ): string;
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  result :=
      FVisitorManager.Execute( cuStandardTask_ReadPK,
                       pVisited,
                       pDBConnectionName,
                       pPerLayerName ) ;
end;


function TtiOPFManager.Save(const pVisited          : TtiVisited;
                        const pDBConnectionName : string = '' ;
                        const pPerLayerName     : string = '' ): string;
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  result :=
      FVisitorManager.Execute( cuStandardTask_Save,
                       pVisited,
                       pDBConnectionName,
                       pPerLayerName ) ;
end;


procedure TtiOPFManager.ConnectDatabase(
  const ADatabaseName : string;
  const AUserName     : string;
  const APassword : string;
  const AParams       : string;
  const APackageID    : string);
var
  lRegPerLayer   : TtiPersistenceLayer ;
begin
  Assert(APackageID <> '', 'pPackageID not assigned');
  Assert(ADatabaseName <> '', 'pDatabaseName not assigned');

  lRegPerLayer := FPersistenceLayers.FindByPerLayerName( APackageID ) ;

  if lRegPerLayer = nil then
    raise EtiOPFInternalException.CreateFmt(cErrorUnableToFindPerLayer,[APackageID]);

  lRegPerLayer.DBConnectionPools.Connect( ADatabaseName, AUserName,
                                          APassword, AParams );

  if lRegPerLayer.DefaultDBConnectionName = '' then
     lRegPerLayer.DefaultDBConnectionName := ADatabaseName ;

end;


function TtiOPFManager.GetClassDBMappingMgr: TtiClassDBMappingMgr;
begin
  if FClassDBMappingMgr = nil then
  begin
    FClassDBMappingMgr := TtiClassDBMappingMgr.Create ;
    FClassDBMappingMgr.Owner := self ;
    // Register the visitors that work with the persistence mapping classes
    VisitorManager.RegisterVisitor( cuStandardTask_ReadPK,   TVisAutoCollectionPKRead ) ;
    VisitorManager.RegisterVisitor( cuStandardTask_ReadThis, TVisAutoReadThis ) ;
//    VisMgr.RegisterVisitor( cuStandardTask_ReadThis, TVisAutoCollectionRead ) ;
    VisitorManager.RegisterVisitor( cuStandardTask_Read,     TVisAutoReadThis ) ;
    VisitorManager.RegisterVisitor( cuStandardTask_Read,     TVisAutoCollectionRead ) ;
    VisitorManager.RegisterVisitor( cuStandardTask_Save,     TVisAutoDelete ) ;
    VisitorManager.RegisterVisitor( cuStandardTask_Save,     TVisAutoUpdate ) ;
    VisitorManager.RegisterVisitor( cuStandardTask_Save,     TVisAutoCreate ) ;
  end ;
  result := FClassDBMappingMgr ;
end;

function TtiOPFManager.TerminateThreads(const Timeout : Integer=0) : Boolean;
var
  LStart: Cardinal;
  ACheckFor : Cardinal;
begin
  Result := false;
  ACheckFor := Timeout*1000;
  LStart := tiGetTickCount;
  ActiveThreadList.Terminate;
  while ActiveThreadList.RunningThreadCount >0 do
  begin
    Sleep(10);
    Application.ProcessMessages;
    if (ACheckFor>0) and ((tiGetTickCount - LStart) > ACheckFor) then Exit;
  end;
  Result := (ActiveThreadList.RunningThreadCount =0);
end;

procedure TtiOPFManager.Terminate;
begin
  FCriticalSection.Enter ;
  try
    FTerminated := true ;
  finally
    FCriticalSection.Leave ;
  end ;
  FActiveThreadList.Terminate;
{
  // This little gem is here to force the application to wait until all threads
  // have finished running before blowing away the persistence layers below
  // those threads (if they are performing database access). The problem is,
  // if the reason the user wants to shut down the application is because a
  // query has run a-muck, then the shut down will not go any futher than
  // this.

  // We require a descendant of TThread which knows how to run a query, and
  // terminate it self if we want to throw the query away. TThreadProgress
  // should use this same class as its starting point.

  // We would not allow the visitor manager to run a threaded query where the
  // thread is not of this type.
  while FVisMgr.ThreadCount > 0 do
  begin
    Sleep( 100 ) ;
    Application.ProcessMessages ;
  end ;
}
end;


procedure TtiOPFManager.ExecSQL(const pSQL              : string;
                            const pDBConnectionName : string = '' ;
                            const pPerLayerName     : string = '' );
var
  lDBConnectionName : string ;
  lPooledDB : TPooledDB ;
begin
Assert( pPerLayerName = '', 'Not implemented whe pPreLayerName <> ''' ) ;
  Assert( DefaultPerLayer <> nil, 'DefaultPerLayer not assigned' ) ;
  if pDBConnectionName = '' then
    lDBConnectionName := DefaultDBConnectionName
  else
    lDBConnectionName := pDBConnectionName ;
  lPooledDB := DefaultPerLayer.DBConnectionPools.Lock( lDBConnectionName ) ;
  try
    lPooledDB.Database.ExecSQL( pSQL ) ;
  finally
    DefaultPerLayer.DBConnectionPools.UnLock( lDBConnectionName, lPooledDB ) ;
  end ;
end;


procedure TtiOPFManager.DisconnectDatabase(
  const ADatabaseName : string;
  const APackageID    : string);
var
  lRegPerLayer   : TtiPersistenceLayer ;
begin
  Assert(ADatabaseName <> '', 'ADatabaseName not assigned');
  Assert(APackageID <> '', 'APackageID not assigned');

  lRegPerLayer := FPersistenceLayers.FindByPerLayerName( APackageID ) ;
  if lRegPerLayer = nil then
    raise EtiOPFInternalException.CreateFmt(cErrorUnableToFindPerLayer,[APackageID]);

  if lRegPerLayer.NextOIDMgr.FindByDatabaseName(ADatabaseName) <> nil then
    lRegPerLayer.NextOIDMgr.UnloadNextOIDGenerator( ADatabaseName ) ;

  if ( SameText( lRegPerLayer.DefaultDBConnectionName, ADatabaseName )) then
    lRegPerLayer.DefaultDBConnectionName := '' ;
  lRegPerLayer.DBConnectionPools.DisConnect( ADatabaseName ) ;

end;


procedure TtiOPFManager.CreateTable(const pTableMetaData: TtiDBMetaDataTable;
                                const pDBConnectionName : string = '';
                                const pPerLayerName     : string = '');
var
  lDB : TtiDatabase ;
begin
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.CreateTable(pTableMetaData) ;
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


procedure TtiOPFManager.DropTable(const pTableName: TTableName;
                              const pDBConnectionName: string = '';
                              const pPerLayerName : string = '');
var
  lDB : TtiDatabase ;
begin
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.DropTable(pTableName);
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


procedure TtiOPFManager.DropTable(const pTableMetaData    : TtiDBMetaDataTable;
                              const pDBConnectionName : string = '' ;
                              const pPerLayerName     : string = '');
var
  lDB : TtiDatabase ;
begin
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.DropTable(pTableMetaData);
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


procedure TtiOPFManager.DeleteRow(const pTableName        : string;
                              const pWhere            : TtiQueryParams ;
                              const pDBConnectionName : string = '';
                              const pPerLayerName     : string = '');
var
  lDB : TtiDatabase ;
begin
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.DeleteRow( pTableName, pWhere ) ;
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


procedure TtiOPFManager.InsertRow(const pTableName        : string ;
                              const pParams           : TtiQueryParams ;
                              const pDBConnectionName : string = '' ;
                              const pPerLayerName     : string = '' );
var
  lDB : TtiDatabase ;
begin
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.InsertRow( pTableName, pParams ) ;
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


procedure TtiOPFManager.UpdateRow(const pTableName : string;
                              const pWhere     : TtiQueryParams ;
                              const pParams    : TtiQueryParams ;
                              const pDBConnectionName : string = '' ;
                              const pPerLayerName     : string = '' );
var
  lDB : TtiDatabase ;
begin
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
   lDB.UpdateRow( pTableName, pWhere, pParams ) ;
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


procedure TtiOPFManager.RegReadThisVisitor(const pClassRef: TVisClassRef);
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  FVisitorManager.RegisterVisitor( cuStandardTask_ReadThis, pClassRef ) ;
end;


function TtiOPFManager.ReadThis(const pVisited          : TtiVisited;
                            const pDBConnectionName : string = '' ;
                            const pPerLayerName     : string = '' ): string;
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  result :=
      FVisitorManager.Execute( cuStandardTask_ReadThis,
                       pVisited,
                       pDBConnectionName,
                       pPerLayerName ) ;
end;


function TtiOPFManager.GetDefaultPerLayer: TtiPersistenceLayer;
begin
  result := FPersistenceLayers.DefaultPerLayer ;
end;


function TtiOPFManager.TableExists(const pTableName: string;
                               const pDBConnectionName: string = '' ;
                               const pPerLayerName : string = '' ): boolean;
var
  lDBMetaData : TtiDBMetaData ;
  lDB : TtiDatabase ;
begin
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDBMetaData := TtiDBMetaData.Create ;
    try
      lDB.ReadMetaDataTables(lDBMetaData);
      result := lDBMetaData.FindByTableName(pTableName) <> nil;
    finally
      lDBMetaData.Free;
    end;
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


procedure TtiOPFManager.UnLoadPersistenceLayer(const pPackageID: string);
begin
  PersistenceLayers.UnLoadPersistenceLayer(pPackageID);
end;


procedure TtiOPFManager.SetDefaultPerLayer(const Value: TtiPersistenceLayer);
begin
  FPersistenceLayers.DefaultPerLayer := Value ;
end;


procedure TtiOPFManager.SetDefaultPerLayerName(const Value: string);
begin
  FPersistenceLayers.DefaultPerLayerName := Value ;
end;

procedure TtiOPFManager.ReadMetaDataFields(
  pDBMetaDataTable: TtiDBMetaDataTable; const pDBConnectionName,
  pPerLayerName: string);
var
  lDB : TtiDatabase ;
begin
  Assert(pDBMetaDataTable.TestValid(TtiDBMetaDataTable), cTIInvalidObjectError);
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.ReadMetaDataFields(pDBMetaDataTable);
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


procedure TtiOPFManager.ReadMetaDataTables(pDBMetaData: TtiDBMetaData;
  const pDBConnectionName, pPerLayerName: string);
var
  lDB : TtiDatabase ;
begin
  Assert(pDBMetaData.TestValid(TtiDBMetaData), cTIInvalidObjectError);
  lDB := PersistenceLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.ReadMetaDataTables(pDBMetaData);
  finally
    PersistenceLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;


{$IFNDEF OID_AS_INT64}
  procedure TtiOPFManager.SetDefaultOIDClassName(const Value: string);
  begin
    FDefaultOIDClassName := Value;
  end;
{$ENDIF}


function TtiOPFManager.GetApplicationData: TList;
begin
  result := FApplicationData;
end;


function TtiOPFManager.TestThenConnectDatabase(
  const ADatabaseName : string;
  const AUserName     : string;
  const APassword : string;
  const AParams       : string;
  const APackageID    : string): boolean;
var
  lRegPerLayer : TtiPersistenceLayer ;
begin
  Assert( APackageID <> '', 'APackageID not assigned' ) ;
  lRegPerLayer := FPersistenceLayers.FindByPerLayerName( APackageID ) ;
  if lRegPerLayer = nil then
    raise EtiOPFInternalException.CreateFmt(cErrorUnableToFindPerLayer,[APackageID]);
  if lRegPerLayer.tiDatabaseClass.TestConnectTo(ADatabaseName, AUserName, APassword, AParams) then
  begin
    lRegPerLayer.DBConnectionPools.Connect(ADatabaseName, AUserName, APassword, AParams);
    result := true ;
  end else
    Result := false;
end;


function TtiOPFManager.CreateDatabase(const pDatabaseName, pUserName,
  pUserPassword, pPackageID: string): string;
var
  lRegPerLayer : TtiPersistenceLayer;
begin
  Result := '';   { Graeme: What is this function supposed to return? }
  lRegPerLayer := PersistenceLayers.FindByPerLayerName( pPackageID ) ;
  if lRegPerLayer = nil then
    raise EtiOPFInternalException.CreateFmt(cErrorUnableToFindPerLayer,[pPackageID]);
  lRegPerLayer.tiDatabaseClass.CreateDatabase( pDatabaseName, pUserName, pUserPassword);
end;


procedure TtiOPFManager.RegisterVisitor(const psGroupName: string; const pClassRef: TVisClassRef);
begin
  Assert( FVisitorManager.TestValid, cErrorTIPerObjAbsTestValid );
  FVisitorManager.RegisterVisitor( psGroupName, pClassRef ) ;
end;


procedure TtiOPFManager.ConnectDatabase(const ADatabaseName, AUserName, APassword, AParams: string);
var
  lRegPerLayer: TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer = nil then
    raise EtiOPFInternalException.Create(cErrorUnableToFindDefaultPerLayer);
  ConnectDatabase(ADatabaseName, AUserName, APassword, AParams, lRegPerLayer.PerLayerName);
end;


procedure TtiOPFManager.ConnectDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  ConnectDatabase(ADatabaseName, AUserName, APassword, '');
end;


procedure TtiOPFManager.DisconnectDatabase(const ADatabaseName: string);
var
  lRegPerLayer: TtiPersistenceLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer = nil then
    raise EtiOPFInternalException.Create(cErrorUnableToFindDefaultPerLayer);
  DisconnectDatabase(ADatabaseName, lRegPerLayer.PerLayerName);
end;


function TtiOPFManager.TestThenConnectDatabase(const ADatabaseName,
  AUserName, APassword, AParams: string): boolean;
var
  lPerLayer: TtiPersistenceLayer;
begin
  lPerLayer := DefaultPerLayer ;
  Assert(lPerLayer <> nil, cErrorUnableToFindDefaultPerLayer);
  Result := TestThenConnectDatabase(ADatabaseName, AUserName, APassword, AParams, lPerLayer.PerLayerName);
end;


function TtiOPFManager.TestThenConnectDatabase(const ADatabaseName,
  AUserName, APassword: string): boolean;
begin
  Result := TestThenConnectDatabase(ADatabaseName, AUserName, APassword, '');
end;


procedure TtiOPFManager.DisconnectDatabase;
var
  lRegPerLayer: TtiPersistenceLayer;
  lDatabaseName: string;
begin
  lRegPerLayer := DefaultPerLayer ;
  Assert(lRegPerLayer <> nil, cErrorUnableToFindDefaultPerLayer);
  lDatabaseName := lRegPerLayer.DefaultDBConnectionName;
  Assert(lDatabaseName <> '', cErrorUnableToFindDefaultDatabase);
  DisconnectDatabase(lDatabaseName, lRegPerLayer.PerLayerName);
end;


initialization
  uShuttingDown := False ;

  // This works if there is only one persistence layer linked in via a compiler
  // directive, but not if there are more.
  // Have added this code to solve the problem of forcing the correct default per layer
  // when the remote layer is pulled in from code, and a SQL layer is required as well.
  // Note: When adding another persistence layer, compiler directives must also be added
  //       to tiDefines.pas
  {$IFDEF LINK_XML}         gTIOPFManager.DefaultPerLayerName := cTIPersistXML;         {$ENDIF}
  {$IFDEF LINK_IBX}         gTIOPFManager.DefaultPerLayerName := cTIPersistIBX;         {$ENDIF}
  {$IFDEF LINK_BDEPARADOX}  gTIOPFManager.DefaultPerLayerName := cTIPersistBDEParadox;  {$ENDIF}
  {$IFDEF LINK_ADOACCESS}   gTIOPFManager.DefaultPerLayerName := cTIPersistADOAccess;   {$ENDIF}
  {$IFDEF LINK_ADOSQLSERVER}gTIOPFManager.DefaultPerLayerName := cTIPersistADOSQLServer;{$ENDIF}
  {$IFDEF LINK_CSV}         gTIOPFManager.DefaultPerLayerName := cTIPersistCSV;         {$ENDIF}
  {$IFDEF LINK_TAB}         gTIOPFManager.DefaultPerLayerName := cTIPersistTAB;         {$ENDIF}
  {$IFDEF LINK_XMLLIGHT}    gTIOPFManager.DefaultPerLayerName := cTIPersistXMLLight;    {$ENDIF}
  {$IFDEF LINK_DOA}         gTIOPFManager.DefaultPerLayerName := cTIPersistDOA;         {$ENDIF}
  {$IFDEF LINK_REMOTE}      gTIOPFManager.DefaultPerLayerName := cTIPersistRemote;      {$ENDIF}
  {$IFDEF LINK_SQLDB_IB}
          gTIOPFManager.DefaultPerLayerName := cTIPersistSqldbIB;
          gTIOPFManager.PersistenceLayers.LoadingStyle := pllsStaticLinking;
  {$ENDIF}
  {$IFDEF LINK_FBL}
          gTIOPFManager.DefaultPerLayerName := cTIPersistFBL;
          gTIOPFManager.PersistenceLayers.LoadingStyle := pllsStaticLinking;
  {$ENDIF}
  
finalization
  uShuttingDown := True;
  FreeAndNilTIPerMgr;

end.

