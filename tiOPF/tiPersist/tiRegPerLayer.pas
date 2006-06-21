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

unit tiRegPerLayer;

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
  ,tiPerObjOIDAbs
  ;
type

  TtiRegPerLayers = class ;
  TtiRegPerLayer  = class ;

  TtiRegPerLayers = class( TPerObjList )
  private
    FDefaultPerLayer : TtiRegPerLayer ;
    function    PackageIDToPackageName(const pPackageID: string): TFileName;
    function    GetDefaultPerLayer: TtiRegPerLayer;
    procedure   SetDefaultPerLayer(const Value: TtiRegPerLayer);
    function    GetDefaultPerLayerName: string;
    procedure   SetDefaultPerLayerName(const Value: string);
  protected
    function    GetItems(i: integer): TtiRegPerLayer ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiRegPerLayer); reintroduce ;
    function    GetOwner: TtiRegPerLayer; reintroduce ;
    procedure   SetOwner(const Value: TtiRegPerLayer); reintroduce ;
  public
    destructor  Destroy ; override ;
    property    Items[i:integer] : TtiRegPerLayer read GetItems write SetItems ;
    procedure   Add( pObject : TtiRegPerLayer   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;

    // These manage the loading and unloading of the packages
    function    LoadPersistenceLayer(const pPerLayerName : string) : TtiRegPerLayer;
    procedure   UnLoadPersistenceLayer(const pPerLayerName : string);
    function    IsLoaded( const pPerLayerName : string ) : boolean ;

    function    FindByPerLayerName( const pLayerName : string ) : TtiRegPerLayer ;
    function    FindByTIDatabaseClass( const pTIDatabaseClass : TtiDatabaseClass ) : TtiRegPerLayer ;
    property    DefaultPerLayer     : TtiRegPerLayer read GetDefaultPerLayer     write SetDefaultPerLayer ;
    property    DefaultPerLayerName : string         read GetDefaultPerLayerName write SetDefaultPerLayerName ;

    // Do not call these your self. They are called in the initialization section
    // of tiQueryXXX.pas that contains the concrete classes.
    procedure   __RegisterPersistenceLayer( const pLayerName: string;
                                          pDBConnectionPoolDataClass : TtiDBConnectionPoolDataClass ;
                                          ptiQueryClass : TtiQueryClass ;
                                          ptiDatabaseClass : TtiDatabaseClass);
    procedure   __UnRegisterPersistenceLayer( const pLayerName: string);

    function    CreateTIQuery( const pLayerName : string {= '' })            : TtiQuery ; overload ;
    function    CreateTIQuery( const pTIDatabaseClass : TtiDatabaseClass ) : TtiQuery ; overload ;
    function    CreateTIDatabase( const pLayerName : string {= '' })     : TtiDatabase ;
    function    CreateTIDBConnectionPoolData( const pLayerName : string {= ''} ) : TtiDBConnectionPoolDataAbs ;
    function    LockDatabase( const pDBConnectionName : string ; pPerLayerName : string ) : TtiDatabase ;
    procedure   UnLockDatabase( const pDatabase : TtiDatabase ;const pDBConnectionName : string ; pPerLayerName : string );

  end;

  TtiRegPerLayer = class( TPerObjAbs )
  private
    FTiQueryClass: TtiQueryClass;
    FTiDatabaseClass: TtiDatabaseClass;
    FTiDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
    FPerLayerName: string;
    FModuleID: HModule;
    FDBConnectionPools: TDBConnectionPools;
    FDefaultDBConnectionPool : TDBConnectionPool ;
    FNextOIDMgr: TNextOIDMgr;
    FDynamicallyLoaded: boolean;
    function  GetDefaultDBConnectionPool: TDBConnectionPool;
    function  GetDefaultDBConnectionName: string;
    procedure SetDefaultDBConnectionName(const Value: string);
  protected
    function    GetOwner: TtiRegPerLayers; reintroduce ;
    procedure   SetOwner(const Value: TtiRegPerLayers); reintroduce ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Owner       : TtiRegPerLayers            read GetOwner      write SetOwner ;

    // Properties that are set when the persistence layer is loaded (set in
    // initialization section of TtiQueryXXX.pas that contains the concrete)
    property  tiDBConnectionPoolDataClass : TtiDBConnectionPoolDataClass read FTiDBConnectionPoolDataClass write FTiDBConnectionPoolDataClass ;
    property  tiQueryClass                : TtiQueryClass read FTiQueryClass        write FTiQueryClass ;
    property  tiDatabaseClass             : TtiDatabaseClass read FTiDatabaseClass  write FTiDatabaseClass ;
    property  PerLayerName                : string read FPerLayerName write FPerLayerName ;
    property  DynamicallyLoaded           : boolean read FDynamicallyLoaded write FDynamicallyLoaded ;

    property  ModuleID                    : HModule read FModuleID write FModuleID ;
    property  DefaultDBConnectionName     : string read GetDefaultDBConnectionName write SetDefaultDBConnectionName ;
    property  DefaultDBConnectionPool     : TDBConnectionPool read GetDefaultDBConnectionPool ;
    property  DBConnectionPools           : TDBConnectionPools read FDBConnectionPools ;
    property  NextOIDMgr                  : TNextOIDMgr read FNextOIDMgr ;

    function  DatabaseExists( const pDatabaseName, pUserName, pPassword : string ) : boolean ;
    procedure CreateDatabase( const pDatabaseName, pUserName, pPassword : string );
    function  TestConnectToDatabase( const pDatabaseName, pUserName, pPassword, pParams : string ) : boolean ;
  end ;


implementation
uses
  tiUtils
  ,tiDialogs
  ,tiLog
  ,ctiPersist
  ,tiPersist
;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiRegPerLayer
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiRegPerLayer.Create;
begin
  inherited ;
  FTiDBConnectionPoolDataClass := TtiDBConnectionPoolDataAbs ;
  ModuleID := 0 ;
  FDBConnectionPools:= TDBConnectionPools.Create;
  FDBConnectionPools.Owner := Self ;
  FNextOIDMgr := TNextOIDMgr.create ;
  {$IFNDEF OID_AS_INT64}
  FNextOIDMgr.Owner := Self ;
  {$ENDIF}
  FDynamicallyLoaded := false ;
end;

{ TtiRegPerLayers }

procedure TtiRegPerLayers.Add(pObject: TtiRegPerLayer;
  pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiRegPerLayers.FindByPerLayerName( const pLayerName: string): TtiRegPerLayer;
var
  i : integer ;
begin

  result := nil ;
  if ( pLayerName = '' ) and
     ( Count = 1 ) then
  begin
    result := Items[0] ;
    Exit ; //==>
  end ;

  for i := 0 to Count - 1 do
    if SameText( Items[i].PerLayerName, pLayerName ) then
    begin
      result := Items[i] ;
      Exit ; //==>
    end;
end;

function TtiRegPerLayers.GetItems(i: integer): TtiRegPerLayer;
begin
  result := TtiRegPerLayer( inherited GetItems( i )) ;
end;

function TtiRegPerLayers.GetOwner: TtiRegPerLayer;
begin
  result := TtiRegPerLayer( GetOwner ) ;
end;

procedure TtiRegPerLayers.__RegisterPersistenceLayer(
  const pLayerName: string;
  pDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
  ptiQueryClass: TtiQueryClass; ptiDatabaseClass: TtiDatabaseClass);
var
  lData : TtiRegPerLayer ;
begin
  if IsLoaded(pLayerName) then
    Exit ; //==>
  lData := TtiRegPerLayer.Create ;
  lData.PerLayerName := pLayerName ;
  lData.tiDBConnectionPoolDataClass := pDBConnectionPoolDataClass ;
  lData.tiQueryClass          := ptiQueryClass    ;
  lData.tiDatabaseClass       := ptiDatabaseClass ;
  Add( lData ) ;
end ;

procedure TtiRegPerLayers.SetItems(i: integer; const Value: TtiRegPerLayer);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiRegPerLayers.SetOwner(const Value: TtiRegPerLayer);
begin
  inherited SetOwner( Value ) ;
end;

function TtiRegPerLayers.CreateTIDatabase( const pLayerName : string {= ''} ) : TtiDatabase;
var
  lData : TtiRegPerLayer ;
begin
  lData := FindByPerLayerName( pLayerName ) ;
  if lData = nil then
  begin
    tiAppError( 'Request for unregistered persistence layer <' + pLayerName + '>' ) ;
    LogError( 'Request for unregistered persistence layer <' + pLayerName + '>'  ) ;
    LogError( 'About to call HALT from ' + ClassName + '.CreateTIDatabase');
    Halt ;
  end ;
  result := lData.tiDatabaseClass.Create ;
end;

function TtiRegPerLayers.CreateTIQuery( const pLayerName : string {= ''} ): TtiQuery;
var
  lData : TtiRegPerLayer ;
begin
  lData := FindByPerLayerName( pLayerName ) ;
  if lData = nil then
  begin
    tiAppError( 'Request for unregistered persistence layer <' + pLayerName + '>' ) ;
    LogError( 'Request for unregistered persistence layer <' + pLayerName + '>' ) ;
    LogError( 'About to call HALT from ' + ClassName + '.CreateTIQuery');
    Halt ;
  end ;
  result := lData.tiQueryClass.Create ;
end;

function TtiRegPerLayers.CreateTIDBConnectionPoolData( const pLayerName : string {= ''} ) : TtiDBConnectionPoolDataAbs;
var
  lData : TtiRegPerLayer ;
begin

  lData := FindByPerLayerName( pLayerName ) ;
  if lData = nil then
  begin
    tiAppError( 'Request for unregistered persistence layer <' + pLayerName + '>' ) ;
    LogError( 'Request for unregistered persistence layer <' + pLayerName + '>' ) ;
    LogError( 'About to call HALT from ' + ClassName + '.CreateTIDBConnectionPoolData');
    Halt ;
  end ;
  result := lData.tiDBConnectionPoolDataClass.Create ;

end;

procedure TtiRegPerLayer.CreateDatabase(const pDatabaseName, pUserName,
  pPassword: string);
begin
  Assert(FTiDatabaseClass<>nil, 'FTiDatabaseClass not assigned' ) ;
  FTiDatabaseClass.CreateDatabase(pDatabaseName, pUserName, pPassword ) ;
end;

function TtiRegPerLayer.DatabaseExists(const pDatabaseName, pUserName, pPassword: string): boolean;
begin
  Assert(FTiDatabaseClass<>nil, 'FTiDatabaseClass not assigned' ) ;
  result := FTiDatabaseClass.DatabaseExists(pDatabaseName, pUserName, pPassword ) ;
end;

destructor TtiRegPerLayer.Destroy;
begin
  FDBConnectionPools.Free;
  FNextOIDMgr.Free;
  inherited;
end;

function TtiRegPerLayer.GetDefaultDBConnectionName: string;
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  lDBConnectionPool := DefaultDBConnectionPool ;
  if lDBConnectionPool = nil then
  begin
    result := '' ;
    Exit ; //==>
  end;
  result := lDBConnectionPool.DBConnectParams.ConnectionName ;
end;

function TtiRegPerLayer.GetDefaultDBConnectionPool: TDBConnectionPool;
begin
  Assert( FDefaultDBConnectionPool.TestValid(TDBConnectionPool, true), cTIInvalidObjectError );
  if FDefaultDBConnectionPool <> nil then
  begin
    result := FDefaultDBConnectionPool ;
    Exit ; //==>
  end;

  if DBConnectionPools.Count = 0 then
  begin
    result := nil ;
    Exit ; //==>
  end;

  result := DBConnectionPools.Items[0];
  Assert( Result.TestValid(TDBConnectionPool), cTIInvalidObjectError );

end;

function TtiRegPerLayer.GetOwner: TtiRegPerLayers;
begin
  result := TtiRegPerLayers( inherited GetOwner ) ;
end;

procedure TtiRegPerLayer.SetDefaultDBConnectionName(const Value: string);
begin
  FDefaultDBConnectionPool := FDBConnectionPools.Find(Value);
  Assert( FDefaultDBConnectionPool.TestValid(TDBConnectionPool, true), cTIInvalidObjectError );
end;

procedure TtiRegPerLayer.SetOwner(const Value: TtiRegPerLayers);
begin
  inherited SetOwner( Value ) ;
end;

destructor TtiRegPerLayers.Destroy;
var
  i : integer ;
begin
  for i := Count - 1 downto 0 do
//    if Items[i].ModuleID <> 0 then
      UnLoadPersistenceLayer(Items[i].PerLayerName);
//      UnLoadPackage( Items[i].ModuleID ) ;
  inherited;
end;

procedure TtiRegPerLayers.__UnRegisterPersistenceLayer( const pLayerName: string);
var
  lData : TtiRegPerLayer ;
begin
  lData := FindByPerLayerName( pLayerName ) ;
  if lData = nil then
    Exit ; //==>
  if gTIPerMgr.DefaultPerLayer = lData then
    gTIPermgr.DefaultPerLayer := nil ;
  Remove( lData ) ;
end;

function TtiRegPerLayers.IsLoaded(const pPerLayerName: string): boolean;
begin
  result := ( FindByPerLayerName( pPerLayerName ) <> nil ) ;
end;

function TtiRegPerLayers.CreateTIQuery(
  const pTIDatabaseClass: TtiDatabaseClass): TtiQuery;
var
  lData : TtiRegPerLayer ;
begin
  lData := FindByTIDatabaseClass( pTIDatabaseClass ) ;
  if lData = nil then
  begin
    tiAppError( 'Unable to find persistence layer for database class <' + pTIDatabaseClass.ClassName + '>' ) ;
    LogError( 'Unable to find persistence layer for database class <' + pTIDatabaseClass.ClassName + '>' ) ;
    LogError( 'About to call HALT from ' + ClassName + '.CreateTIQuery');
    Halt ;
  end ;
  result := lData.tiQueryClass.Create ;
end;

function TtiRegPerLayers.FindByTIDatabaseClass(
  const pTIDatabaseClass: TtiDatabaseClass): TtiRegPerLayer;
var
  i : integer ;
begin
  Assert(pTIDatabaseClass <> nil, 'pTIDatabaseClass <> nil' );
  result := nil ;
  for i := 0 to Count - 1 do
    if Items[i].TIDatabaseClass = pTIDatabaseClass then
    begin
      result := Items[i] ;
      Exit ; //==>
    end;
end;

function TtiRegPerLayers.LoadPersistenceLayer(const pPerLayerName: string) : TtiRegPerLayer;
var
  lPackageName : TFileName ;
  lPackageModule : HModule ;
  lMessage : string ;
begin
  result := FindByPerLayerName(pPerLayerName);
  if result <> nil then
    Exit ; //==>

  lPackageName := PackageIDToPackageName( pPerLayerName ) ;
  LogFmt( 'Loading %s', [lPackageName] ) ;

  try
    lPackageModule := LoadPackage( ExtractFileName( lPackageName )) ;
    result   := FindByPerLayerName( pPerLayerName );
    if result = nil then
      raise exception.create( 'Unable to locate package in memory after it was loaded.' + Cr +
                              'Check that this application was build with the runtime package tiPersistCore' ) ;
    result.DynamicallyLoaded := true ;
    result.ModuleID := lPackageModule ;
  except
    on e:exception do
    begin
      lMessage := 'Unable to initialize persistence layer <' +
                  pPerLayerName + '> Package name <' +
                  lPackageName + '>' + Cr( 2 ) +
                  'Error message: ' + e.message ;
      tiAppError( lMessage ) ;
      LogError( lMessage ) ;
      LogError( 'About to call HALT from ' + ClassName + '.LoadPersistenceLayer');
      Halt ;
    end ;
  end ;
end;

function TtiRegPerLayers.PackageIDToPackageName( const pPackageID : string ) : TFileName ;
begin
  result :=
    tiAddTrailingSlash( tiGetEXEPath ) +
    cTIPersistPackageRootName +
    pPackageID +
    cPackageSuffix +
    '.bpl' ;
end ;

{
function TtiRegPerLayers.PackageNameToPackageID( const pPackageName : TFileName ) : string ;
begin
  result := tiExtractFileNameOnly( pPackageName ) ;
  result := tiStrTran( result, cPackageSuffix, '' ) ;
  result := tiStrTran( result,
                       cTIPersistPackageRootName,
                       '' ) ;
end ;
}

procedure TtiRegPerLayers.UnLoadPersistenceLayer(const pPerLayerName: string);
var
  lPackageID     : string ;
  lRegPerLayer  : TtiRegPerLayer ;
begin
  if pPerLayerName <> '' then
    lPackageID := pPerLayerName
  else if Count = 1 then
    lPackageID   := Items[0].PerLayerName
  else
    tiFmtException( 'Unable to determine which persistence layer to unload.' ) ;

  if not IsLoaded( pPerLayerName ) then
  begin
    tiFmtException( 'Attempt to unload persistence layer <' +
                    lPackageID + '> and it is not currently loaded.',
                    ClassName, 'UnLoadPersistenceFramework' ) ;
  end ;

  // gTIPerMgr.Terminate ; // Not sure about this...

  lRegPerLayer := FindByPerLayerName( pPerLayerName ) ;
  Assert( lRegPerLayer.TestValid, cTIInvalidObjectError );

  lRegPerLayer.DBConnectionPools.DisConnectAll;
  Log( 'Unloading persistence layer <' + lRegPerLayer.PerLayerName + '>' ) ;

  try
    UnLoadPackage( lRegPerLayer.ModuleID ) ; // lRegPerLayer has now been destroyed
  except end ;

end;

function TtiRegPerLayers.LockDatabase(const pDBConnectionName: string;pPerLayerName: string): TtiDatabase;
var
  lRegPerLayer : TtiRegPerLayer ;
  lDBConnectionName : string ;
  lPooledDB : TPooledDB ;
begin
  if pPerLayerName <> '' then
    lRegPerLayer := FindByPerLayerName(pPerLayerName)
  else
    lRegPerLayer := DefaultPerLayer ;

  Assert( lRegPerLayer.TestValid(TtiRegPerLayer), cTIInvalidObjectError );

  if pDBConnectionName <> '' then
    lDBConnectionName := pDBConnectionName
  else
    lDBConnectionName := lRegPerLayer.DefaultDBConnectionName ;

  lPooledDB := lRegPerLayer.DBConnectionPools.Lock( lDBConnectionName ) ;
  result := lPooledDB.Database ;

end;

procedure TtiRegPerLayers.UnLockDatabase(const pDatabase: TtiDatabase;
  const pDBConnectionName: string; pPerLayerName: string);
var
  lDBConnectionName : string ;
  lRegPerLayer : TtiRegPerLayer;
begin
  if pPerLayerName <> '' then
    lRegPerLayer := FindByPerLayerName(pPerLayerName)
  else
    lRegPerLayer := DefaultPerLayer ;

  Assert( lRegPerLayer.TestValid(TtiRegPerLayer), cTIInvalidObjectError );

  if pDBConnectionName <> '' then
    lDBConnectionName := pDBConnectionName
  else
    lDBConnectionName := lRegPerLayer.DefaultDBConnectionName ;

  lRegPerLayer.DBConnectionPools.UnLockByData( lDBConnectionName, pDatabase ) ;

end;

function TtiRegPerLayers.GetDefaultPerLayer: TtiRegPerLayer;
begin
  Assert( FDefaultPerLayer.TestValid(TtiRegPerLayer, true), cTIInvalidObjectError );
  if FDefaultPerLayer <> nil then
  begin
    result := FDefaultPerLayer ;
    Exit ; //==>
  end;
  if Count = 0 then
  begin
    result := nil ;
    Exit ; //==>
  end;
  FDefaultPerLayer := Items[0];
  Assert( FDefaultPerLayer.TestValid(TtiRegPerLayer), cTIInvalidObjectError );
  result := FDefaultPerLayer ;
end;

procedure TtiRegPerLayers.SetDefaultPerLayer(const Value: TtiRegPerLayer);
begin
  FDefaultPerLayer := Value ;
end;

function TtiRegPerLayers.GetDefaultPerLayerName: string;
begin
  if DefaultPerLayer <> nil then
    result := DefaultPerLayer.PerLayerName
  else
    result := '' ;
end;

procedure TtiRegPerLayers.SetDefaultPerLayerName(const Value: string);
begin
  FDefaultPerLayer := FindByPerLayerName(Value);
end;

function TtiRegPerLayer.TestConnectToDatabase(const pDatabaseName,
  pUserName, pPassword, pParams: string): boolean;
begin
  Assert(FTiDatabaseClass<>nil, 'FTiDatabaseClass not assigned' ) ;
  result := FTiDatabaseClass.TestConnectTo(pDatabaseName, pUserName, pPassword,
                                           pParams ) ;
end;

end.
