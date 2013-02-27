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

  Revision history:
    November 2000, Peter Hinrichsen, Made open source

  Purpose:

  Classes:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiPersist;

interface
uses
  SysUtils
  ,Classes
  ,tiDBConnectionPool
  ,tiQuery
  ,tiPtnVisSQL
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,tiPtnVisPerObj
  ,tiRegPerLayer
  ,tiPtnVisMgr
  ,tiPtnVis
  ,tiClassToDBMap_BOM
  ,tiClassToDBMap_Srv
  ,tiPerObjOIDAbs
  ,SyncObjs
  ,tiThread
  ,Contnrs
  ;

const
  cErrorUnableToFindPerLayer = 'Unable to find persistence layer <%s>';

type

  TtiPerMgr = class( TPerObjList )
  private
    FRegPerLayers : TtiRegPerLayers;
    FDefaultPackageName: string;
    FVisMgr: TtiVisMgr;
    FClassDBMappingMgr : TtiClassDBMappingMgr ;
    FTerminated: boolean;
    FCriticalSection : TCriticalSection ;
    FTerminateOnFailedDBConnection: boolean;
    FActiveThreadList : TtiActiveThreadList ;
    FApplicationData : TObjectList ;
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
    function  GetDefaultPerLayer: TtiRegPerLayer;
    procedure SetDefaultPerLayer(const Value: TtiRegPerLayer);
    procedure SetDefaultPerLayerName(const Value: string);
    function GetApplicationData: TList;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;

    // Load just a persistence layer
    procedure   LoadPersistenceLayer( Const pPackageID : string ) ;
    // Unload a persistence layer and all its database connections
    procedure   UnLoadPersistenceLayer( const pPackageID : string = '' ) ;

    // Load a persistence layer and database connections with a single call
    procedure   LoadPersistenceFramework( const pPackageID : string = '' ;
                                          const pDatabaseName : string = '' ;
                                          const pUserName     : string = '' ;
                                          const pUserPassword : string = '' ) ;

    // Do not use UnLoadPersistenceFramework, use UnLoadPersistenceLayer
    procedure   UnLoadPersistenceFramework( const pPackageID : string = '' ) ;

    // Load just a database layer and associate it with the specified p
    // persistence layer
    //
    // Call LoadDatabaseLayer when the persistence layer is determined at
    // compile time, but you must still load a database.
    procedure   LoadDatabaseLayer(        const pPackageID    : string ;
                                          const pDatabaseName : string = '' ;
                                          const pUserName     : string = '' ;
                                          const pUserPassword : string = '' ;
                                          const pParams       : string = '' ) ;

    function    TestThenConnectToDatabase( const pDatabaseName : string ;
                                           const pUserName     : string ;
                                           const pUserPassword : string ;
                                           const pPackageID    : string = '' ;
                                           const pParams       : string = '' ): string ;

    procedure   UnLoadDatabaseLayer(      const pPackageID    : string ;
                                          const pDatabaseName : string = '' ) ;

    function    ReadConfigValue( const pCommandLineParamID,
                                 pRegistryKey: string): string;

    // These register visitors
    procedure   RegReadPKVisitor( const pClassRef : TVisClassRef ) ;
    procedure   RegReadThisVisitor( const pClassRef : TVisClassRef ) ;
    procedure   RegReadVisitor(   const pClassRef : TVisClassRef ) ;
    procedure   RegSaveVisitor(   const pClassRef : TVisClassRef ) ;

    // These call visitors
    function    ReadPK(   const pVisited          : TVisitedAbs ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ) : string ;
    function    ReadThis( const pVisited : TVisitedAbs ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ) : string ;
    function    Read(     const pVisited : TVisitedAbs ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ) : string ;
    function    Save(     const pVisited : TVisitedAbs ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '') : string ;
    procedure   ExecSQL(  const pSQL : string ;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ) ;

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
    property    Terminated : boolean read FTerminated write FTerminated ;
    property    TerminateOnFailedDBConnection : boolean read FTerminateOnFailedDBConnection write FTerminateOnFailedDBConnection ;
    property    ActiveThreadList : TtiActiveThreadList read FActiveThreadList ;
    property    ApplicationData : TList read GetApplicationData ;
    property    ApplicationStartTime : TDateTime read FApplicationStartTime ;

  published
    property    DefaultPerLayer         : TtiRegPerLayer    read GetDefaultPerLayer write SetDefaultPerLayer ;
    property    DefaultPerLayerName     : string            read GetDefaultPerLayerName write SetDefaultPerLayerName ;
    property    DefaultDBConnectionPool : TDBConnectionPool read GetDefaultDBConnectionPool ;
    property    DefaultDBConnectionName : string            read GetDefaultDBConnectionName write SetDefaultDBConnectionName ;

    property    RegPerLayers           : TtiRegPerLayers read FRegPerLayers ;
    property    VisMgr                 : TtiVisMgr read FVisMgr ;
    // ToDo: How to relate the ClassDBMappingMgr to a persistence layer -
    //       The code exists inside the ClassDBMappingMgr but is is stubbed out as it
    //       loads before a persistence layer is available hence is not working.
    property    ClassDBMappingMgr      : TtiClassDBMappingMgr read GetClassDBMappingMgr ;

    {$IFNDEF OID_AS_INT64}
    property    DefaultOIDClassName     : string read FDefaultOIDClassName write SetDefaultOIDClassName ;
    property    OIDFactory             : TOIDFactory read FOIDFactory ;
    {$ENDIF}
  end ;

function gTIPerMgr : TtiPerMgr ;
procedure FreeAndNilTIPerMgr ;

const
  cuStandardTask_ReadPK   = 'StandardTask_ReadPK'   ;
  cuStandardTask_ReadThis = 'StandardTask_ReadThis' ;
  cuStandardTask_Read     = 'StandardTask_Read'     ;
  cuStandardTask_Save     = 'StandardTask_Save'     ;

implementation
uses
  tiCommandLineParams
  ,INIFiles
  ,tiUtils
  ,tiLog
  ,ctiPersist
  {$IFDEF MSWINDOWS}
  ,Forms
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QForms
  {$ENDIF LINUX}
  ,tiDialogs
  ,tiRegINI
  ;

var
  uTIPerMgr : TtiPerMgr ;

function gTIPerMgr : TtiPerMgr ;
begin
  if uTIPerMgr = nil then
    uTIPerMgr := TtiPerMgr.Create ;
  result := uTIPerMgr ;
end ;

procedure FreeAndNilTIPerMgr ;
begin
  FreeAndNil(uTIPerMgr);
end;

// -----------------------------------------------------------------------------
procedure TtiPerMgr.LoadPersistenceFramework(const pPackageID : string = '' ;
                                             const pDatabaseName : string = '' ;
                                             const pUserName     : string = '' ;
                                             const pUserPassword : string = '' ) ;
var
  lPackageID     : string ;
begin

  if pPackageID <> '' then
    lPackageID := pPackageID
  else
    lPackageID   := ReadConfigValue( 'pl', 'PersistenceLayerName' ) ;

//  if lPackageID = '' then
//    lPackageID := GetPersistenceLayerFromUser ;

  if lPackageID = '' then
  begin
    // You should not get here.
    LogError( 'Unable to load persistence layer <' + lPackageID + '>' ) ;
    LogError( 'About to call HALT from ' + ClassName + '.LoadPersistenceFramework');
    tiAppError( 'Unable to load persistence layer <' + lPackageID + '>'  ) ;
    Halt ;
  end;

  if RegPerLayers.IsLoaded( lPackageID ) then
    tiFmtException( 'Attempt to load persistence layer <' +
                    lPackageID + '> more than once.',
                    ClassName, 'LoadPersistenceFramework' ) ;

  // ToDo: Terminated must be related to each loaded persistence layer. This
  //       would make it possible to terminate a single layer at the time.
  FTerminated := false ;

  try
    LoadPersistenceLayer( lPackageID ) ;
    LoadDatabaseLayer(
      lPackageID,
      pDatabaseName,
      pUserName,
      pUserPassword ) ;
  except
    on e:exception do
    begin
      tiAppError( e.Message ) ;
      LogError( e.Message ) ;
      LogError( 'About to call HALT from ' + ClassName + '.LoadPersistenceFramework');
      Halt ;
    end ;
  end ;
end ;

function TtiPerMgr.ReadConfigValue( const pCommandLineParamID : string ;
                                    const pRegistryKey : string ) : string ;
  function _ReadFromINIFile( const pRegistryKey : string ) : string ;
  var
    lINI : TINIFile ;
    lFileName : TFileName ;
  begin
    lFileName := gCommandLineParams.GetParam( 'Config' ) ;
    if lFileName = '' then
      Exit ; //==>
    lFileName := tiSwapExt( lFileName, 'INI' ) ;
    if ExtractFilePath(lFileName) = '' then
      lFileName := tiAddTrailingSlash(tiGetEXEPath) + lFileName ;
    if not FileExists( lFileName ) then
      tiFmtException( 'Can not find configuration file <' +
                      lFileName + '>',
                      ClassName, 'ReadConfigValue' ) ;
    lINI := TINIFile.Create( lFileName ) ;
    try
      result := lINI.ReadString( 'tiPersist', pRegistryKey, '' ) ;
    finally
      lINI.Free ;
    end ;
  end ;
begin

  result := gCommandLineParams.GetParam( pCommandLineParamID ) ;
  {$IFDEF MSWINDOWS}
  if result = '' then
    result := gReg.ReadString( 'tiPersist', pRegistryKey, '' ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if result = '' then
    result := gINI.ReadString( 'tiPersist', pRegistryKey, '' ) ;
  {$ENDIF LINUX}
  if result = '' then
    result := _ReadFromINIFile( pRegistryKey ) ;

end;

{
function TtiPerMgr.GetPersistenceLayerFromUser: string;
  procedure _RemoveCorePackage( psl : TStringList ) ;
  var
    i : integer ;
  begin
    for i := psl.Count-1 downto 0 do
      if tiWildCardMatch( psl.Strings[i], '*tiPersistCore.bpl' ) then
        psl.Delete(i) ;
  end ;
  procedure _PackageNameToPackageID( psl : TStringList ) ;
  var
    i : integer ;
  begin
    for i := 0 to psl.Count - 1 do
      psl.Strings[i] := PackageNameToPackageID( psl.Strings[i] ) ;
  end ;
var
  liSelection : integer ;
  lsl : TStringList ;
begin
  result := '' ;
  lsl := TStringList.Create ;
  try
    // Get a list of possible packages matching the wild card cTIPersistWildCard
    tiFilesToStringList( tiGetEXEPath,
                         cPackagePrefix + cTIPersistPackageRootName + '*' + cPackageExtension,
                         lsl, false ) ;
    // Should improve on this logic...
    // eg, chech the Win32, System directories first...
    // What is there are other drives? Check the NT path first...
    if lsl.Count = 0 then
      tiFilesToStringList( 'C:',
                         cPackagePrefix + cTIPersistPackageRootName + '*' + cPackageExtension,
                           lsl, true ) ;

    _RemoveCorePackage( lsl ) ;
    _PackageNameToPackageID( lsl ) ;

    // Show a dialog to ask the user to select from this list
    liSelection :=
      tiInputDialogCombo( 'Please select a persistence layer',
                          'Persistence layer',
                          -1,
                          lsl ) ;
    // Return the users selection
    if liSelection <> -1 then
      result := lsl.Strings[ liSelection ]
    else
      Application.Terminate ;
  finally
    lsl.Free ;
  end;
end;
}

procedure TtiPerMgr.UnLoadPersistenceFramework( const pPackageID : string = '' ) ;
begin
  UnLoadPersistenceLayer(pPackageID);
end ;

function TtiPerMgr.GetDefaultDBConnectionName: string;
var
  lRegPerLayer : TtiRegPerLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer <> nil then
    result := lRegPerLayer.DefaultDBConnectionName
  else
    result := '' ;
end;

procedure TtiPerMgr.SetDefaultDBConnectionName( const Value: string);
var
  lRegPerLayer : TtiRegPerLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer <> nil then
    lRegPerLayer.DefaultDBConnectionName := Value ;
end;

constructor TtiPerMgr.Create;
begin
  inherited ;
  FCriticalSection := TCriticalSection.Create ;
  FRegPerLayers := TtiRegPerLayers.Create ;
  FVisMgr := TtiVisMgr.Create ;

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

destructor TtiPerMgr.Destroy;
begin
  Terminate;
  FVisMgr.Free ;
  {$IFNDEF OID_AS_INT64}
    FOIDFactory.Free ;
  {$ENDIF}
  FClassDBMappingMgr.Free;
  FRegPerLayers.Free ;
  FActiveThreadList.Free;
  FApplicationData.Free;
  FCriticalSection.Free;
  inherited;
end;

procedure TtiPerMgr.LoadPersistenceLayer(const pPackageID : string );
begin
  // ToDo: Terminated must be related to each loaded persistence layer. This
  //       would make it possible to terminate a single layer at the time.
  FTerminated := false ;
  RegPerLayers.LoadPersistenceLayer(pPackageID);
end;

function TtiPerMgr.GetDefaultDBConnectionPool: TDBConnectionPool;
var
  lRegPerLayer : TtiRegPerLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer <> nil then
    result := lRegPerLayer.DefaultDBConnectionPool
  else
    result := nil
end;

function TtiPerMgr.GetDefaultPerLayerName: string;
var
  lRegPerLayer : TtiRegPerLayer;
begin
  lRegPerLayer := DefaultPerLayer ;
  if lRegPerLayer <> nil then
    result := lRegPerLayer.PerLayerName
  else
    result := ''
end;

procedure TtiPerMgr.RegReadPKVisitor(const pClassRef: TVisClassRef);
begin
  FVisMgr.RegisterVisitor( cuStandardTask_ReadPK, pClassRef ) ;
end;

procedure TtiPerMgr.RegReadVisitor(const pClassRef: TVisClassRef);
begin
  FVisMgr.RegisterVisitor( cuStandardTask_Read, pClassRef ) ;
end;

procedure TtiPerMgr.RegSaveVisitor(const pClassRef: TVisClassRef);
begin
  FVisMgr.RegisterVisitor( cuStandardTask_Save, pClassRef ) ;
end;

function TtiPerMgr.Read(const pVisited          : TVisitedAbs;
                        const pDBConnectionName : string = '';
                        const pPerLayerName     : string = ''): string;
begin
  result :=
      FVisMgr.Execute( cuStandardTask_Read,
                       pVisited,
                       pDBConnectionName,
                       pPerLayerName ) ;
end;

function TtiPerMgr.ReadPK(const pVisited          : TVisitedAbs;
                          const pDBConnectionName : string = '' ;
                          const pPerLayerName     : string = '' ): string;
begin
  result :=
      FVisMgr.Execute( cuStandardTask_ReadPK,
                       pVisited,
                       pDBConnectionName,
                       pPerLayerName ) ;
end;

function TtiPerMgr.Save(const pVisited          : TVisitedAbs;
                        const pDBConnectionName : string = '' ;
                        const pPerLayerName     : string = '' ): string;
begin
  result :=
      FVisMgr.Execute( cuStandardTask_Save,
                       pVisited,
                       pDBConnectionName,
                       pPerLayerName ) ;
end;

procedure TtiPerMgr.LoadDatabaseLayer(
  const pPackageID : string ;
  const pDatabaseName, pUserName, pUserPassword, pParams: string);
var
  lsDatabaseName : string ;
  lsUserName     : string ;
  lsUserPassword : string ;
  lRegPerLayer   : TtiRegPerLayer ;
begin

  if pDatabaseName <> '' then
    lsDatabaseName := pDatabaseName
  else
    lsDatabaseName := ReadConfigValue( 'd', 'DatabaseName' ) ;

  if pUserName <> '' then
    lsUserName := pUserName
  else
    lsUserName     := ReadConfigValue( 'u', 'UserName' ) ;

  if pUserPassword <> '' then
    lsUserPassword := pUserPassword
  else
    lsUserPassword := ReadConfigValue( 'p', 'UserPassword' ) ;

  // There was no database on the command line, so there is nothing more to do
  if ( SameText( lsDatabaseName, EmptyStr )) then
    Exit ; //==>

  lRegPerLayer := FRegPerLayers.FindByPerLayerName( pPackageID ) ;
  Assert( lRegPerLayer <> nil, 'Unable to locate registered persistence layer <' +
                               pPackageID + '>' ) ;
  // There was a database and ( UserName or UserPassword ) on the command line
  // or in an INI file, or the registry, or entered by the user...
  if ( not SameText( lsUserName,     EmptyStr )) and
     ( not SameText( lsUserPassword, EmptyStr )) then
    lRegPerLayer.DBConnectionPools.Connect( lsDatabaseName, lsUserName,
                                            lsUserPassword, pParams )
  else
    lRegPerLayer.DBConnectionPools.Connect( lsDatabaseName ) ;

  if lRegPerLayer.DefaultDBConnectionName = '' then
     lRegPerLayer.DefaultDBConnectionName := lsDatabaseName ;

end;

function TtiPerMgr.GetClassDBMappingMgr: TtiClassDBMappingMgr;
begin
  if FClassDBMappingMgr = nil then
  begin
    FClassDBMappingMgr := TtiClassDBMappingMgr.Create ;
    FClassDBMappingMgr.Owner := self ;
    // Register the visitors that work with the persistence mapping classes
    VisMgr.RegisterVisitor( cuStandardTask_ReadPK,   TVisAutoCollectionPKRead ) ;
    VisMgr.RegisterVisitor( cuStandardTask_ReadThis, TVisAutoReadThis ) ;
//    VisMgr.RegisterVisitor( cuStandardTask_ReadThis, TVisAutoCollectionRead ) ;
    VisMgr.RegisterVisitor( cuStandardTask_Read,     TVisAutoReadThis ) ;
    VisMgr.RegisterVisitor( cuStandardTask_Read,     TVisAutoCollectionRead ) ;
    VisMgr.RegisterVisitor( cuStandardTask_Save,     TVisAutoDelete ) ;
    VisMgr.RegisterVisitor( cuStandardTask_Save,     TVisAutoUpdate ) ;
    VisMgr.RegisterVisitor( cuStandardTask_Save,     TVisAutoCreate ) ;
  end ;
  result := FClassDBMappingMgr ;
end;

procedure TtiPerMgr.Terminate;
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

procedure TtiPerMgr.ExecSQL(const pSQL              : string;
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

procedure TtiPerMgr.UnLoadDatabaseLayer( const pPackageID    : string ;
                                         const pDatabaseName: string = '' );
var
  lsDatabaseName : string ;
  lRegPerLayer   : TtiRegPerLayer ;
begin

  lRegPerLayer := FRegPerLayers.FindByPerLayerName( pPackageID ) ;
  Assert( lRegPerLayer <> nil, 'Unable to find persistence layer <' + pPackageID + '>' ) ;

  if pDatabaseName <> '' then
    lsDatabaseName := pDatabaseName
  else
    lsDatabaseName := lRegPerLayer.DefaultDBConnectionName ;

  Assert( lsDatabaseName <> '', 'Unable to determine database name.' ) ;

  if lRegPerLayer.NextOIDMgr.FindByDatabaseName(lsDatabaseName) <> nil then
    lRegPerLayer.NextOIDMgr.UnloadNextOIDGenerator( lsDatabaseName ) ;

  Log( 'Unloading database connection pool for <' + lsDatabaseName + '>' ) ;
  if ( SameText( lRegPerLayer.DefaultDBConnectionName, lsDatabaseName )) then
    lRegPerLayer.DefaultDBConnectionName := '' ;
  lRegPerLayer.DBConnectionPools.DisConnect( lsDatabaseName ) ;

end;

procedure TtiPerMgr.CreateTable(const pTableMetaData: TtiDBMetaDataTable;
                                const pDBConnectionName : string = '';
                                const pPerLayerName     : string = '');
var
  lDB : TtiDatabase ;
begin
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.CreateTable(pTableMetaData) ;
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

procedure TtiPerMgr.DropTable(const pTableName: TTableName;
                              const pDBConnectionName: string = '';
                              const pPerLayerName : string = '');
var
  lDB : TtiDatabase ;
begin
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.DropTable(pTableName);
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

procedure TtiPerMgr.DropTable(const pTableMetaData    : TtiDBMetaDataTable;
                              const pDBConnectionName : string = '' ;
                              const pPerLayerName     : string = '');
var
  lDB : TtiDatabase ;
begin
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.DropTable(pTableMetaData);
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

procedure TtiPerMgr.DeleteRow(const pTableName        : string;
                              const pWhere            : TtiQueryParams ;
                              const pDBConnectionName : string = '';
                              const pPerLayerName     : string = '');
var
  lDB : TtiDatabase ;
begin
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.DeleteRow( pTableName, pWhere ) ;
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

procedure TtiPerMgr.InsertRow(const pTableName        : string ;
                              const pParams           : TtiQueryParams ;
                              const pDBConnectionName : string = '' ;
                              const pPerLayerName     : string = '' );
var
  lDB : TtiDatabase ;
begin
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.InsertRow( pTableName, pParams ) ;
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

procedure TtiPerMgr.UpdateRow(const pTableName : string;
                              const pWhere     : TtiQueryParams ;
                              const pParams    : TtiQueryParams ;
                              const pDBConnectionName : string = '' ;
                              const pPerLayerName     : string = '' );
var
  lDB : TtiDatabase ;
begin
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
   lDB.UpdateRow( pTableName, pWhere, pParams ) ;
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

procedure TtiPerMgr.RegReadThisVisitor(const pClassRef: TVisClassRef);
begin
  FVisMgr.RegisterVisitor( cuStandardTask_ReadThis, pClassRef ) ;
end;

function TtiPerMgr.ReadThis(const pVisited          : TVisitedAbs;
                            const pDBConnectionName : string = '' ;
                            const pPerLayerName     : string = '' ): string;
begin
  result :=
      FVisMgr.Execute( cuStandardTask_ReadThis,
                       pVisited,
                       pDBConnectionName,
                       pPerLayerName ) ;
end;

function TtiPerMgr.GetDefaultPerLayer: TtiRegPerLayer;
begin
  result := FRegPerLayers.DefaultPerLayer ;
end;

function TtiPerMgr.TableExists(const pTableName: string;
                               const pDBConnectionName: string = '' ;
                               const pPerLayerName : string = '' ): boolean;
var
  lDBMetaData : TtiDBMetaData ;
  lDB : TtiDatabase ;
begin
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDBMetaData := TtiDBMetaData.Create ;
    try
      lDB.ReadMetaDataTables(lDBMetaData);
      result := lDBMetaData.FindByTableName(pTableName) <> nil;
    finally
      lDBMetaData.Free;
    end;
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

procedure TtiPerMgr.UnLoadPersistenceLayer(const pPackageID: string);
begin
  RegPerLayers.UnLoadPersistenceLayer(pPackageID);
end;

procedure TtiPerMgr.SetDefaultPerLayer(const Value: TtiRegPerLayer);
begin
  FRegPerLayers.DefaultPerLayer := Value ;
end;

procedure TtiPerMgr.SetDefaultPerLayerName(const Value: string);
begin
  FRegPerLayers.DefaultPerLayerName := Value ;
end;
procedure TtiPerMgr.ReadMetaDataFields(
  pDBMetaDataTable: TtiDBMetaDataTable; const pDBConnectionName,
  pPerLayerName: string);
var
  lDB : TtiDatabase ;
begin
  Assert(pDBMetaDataTable.TestValid(TtiDBMetaDataTable), cTIInvalidObjectError);
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.ReadMetaDataFields(pDBMetaDataTable);
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

procedure TtiPerMgr.ReadMetaDataTables(pDBMetaData: TtiDBMetaData;
  const pDBConnectionName, pPerLayerName: string);
var
  lDB : TtiDatabase ;
begin
  Assert(pDBMetaData.TestValid(TtiDBMetaData), cTIInvalidObjectError);
  lDB := RegPerLayers.LockDatabase( pDBConnectionName, pPerLayerName ) ;
  try
    lDB.ReadMetaDataTables(pDBMetaData);
  finally
    RegPerLayers.UnLockDatabase( lDB, pDBConnectionName, pPerLayerName ) ;
  end ;
end;

{$IFNDEF OID_AS_INT64}
  procedure TtiPerMgr.SetDefaultOIDClassName(const Value: string);
  begin
    FDefaultOIDClassName := Value;
  end;
{$ENDIF}

function TtiPerMgr.GetApplicationData: TList;
begin
  result := FApplicationData;
end;

function TtiPerMgr.TestThenConnectToDatabase( const pDatabaseName : string ;
                                     const pUserName     : string ;
                                     const pUserPassword : string ;
                                     const pPackageID    : string = '' ;
                                     const pParams       : string = '' ): string;
  function _ExtractUserNamePassword(pValue: string; pIndex : integer): string;
  var
    lCount : integer ;
  begin
    lCount := tiNumToken(pValue, cDatabaseNameDelim) ;
    if lCount < pIndex then
      result := tiToken(pValue, cDatabaseNameDelim, Count)
    else
      result := tiToken(pValue, cDatabaseNameDelim, pIndex);
  end ;
var
  lPackageID   : string ;
  lRegPerLayer : TtiRegPerLayer ;
  i : integer ;
  lDatabaseName : string ;
  lUserName     : string ;
  lPassword     : string ;
begin
  if pPackageID = '' then
    lPackageID := DefaultPerLayerName
  else
    lPackageID := pPackageID ;

  Assert( lPackageID <> '', 'Unable to determine which persistence layer to use' ) ;

  lRegPerLayer := FRegPerLayers.FindByPerLayerName( lPackageID ) ;
  Assert( lRegPerLayer <> nil, 'Unable to locate registered persistence layer <' +
                               lPackageID + '>' ) ;

  result := '' ;
  for i := 1 to tiNumToken(pDatabaseName, cDatabaseNameDelim) do
  begin
    lDatabaseName := tiToken(pDatabaseName, cDatabaseNameDelim, i);
    lUserName     := _ExtractUsernamePassword(pUserName, i);
    lPassword     := _ExtractUsernamePassword(pUserName, i);
    if lRegPerLayer.tiDatabaseClass.TestConnectTo(lDatabaseName, lUserName, lPassword, pParams) then
    begin
      // ToDo: Should pass this params value
      lRegPerLayer.DBConnectionPools.Connect(lDatabaseName, lUserName, lPassword, pParams);
      result := lDatabaseName ;
      Exit ; //==>
    end;
  end ;
end;

function TtiPerMgr.CreateDatabase(const pDatabaseName, pUserName,
  pUserPassword, pPackageID: string): string;
var
  lRegPerLayer : TtiRegPerLayer;
begin
  lRegPerLayer := RegPerLayers.FindByPerLayerName( pPackageID ) ;
  if lRegPerLayer = nil then
    tiFmtException(Format(cErrorUnableToFindPerLayer,[pPackageID]),
                   ClassName, 'CreateDatabase');

  lRegPerLayer.tiDatabaseClass.CreateDatabase( pDatabaseName, pUserName, pUserPassword);
end;

initialization

finalization
  FreeAndNilTIPerMgr ;

end.
