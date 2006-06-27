unit tiDBConnectionPool;

{$I tiDefines.inc}

interface
uses
   tiObject
  ,tiBaseObject
  ,tiQuery
  ,tiPool
  ,tiCommandLineParams
  ,Classes
  ,Contnrs
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,Forms
  ,Controls
  ,StdCtrls
  ,Buttons
  ,SysUtils
  ;

const
  cErrorAttemptToAddDuplicateDBConnectionPool = 'Attempt to add duplicate database connection pool <%s>';
  cErrorUnableToFindDBConnectionPool = 'Unable to find database connection pool <%s>';

type

  TDBConnectParams    = class ;
  TDBConnectionPool   = class ;

  // A holder for database connection info: DatabaseName, UserName, Password.
  TDBConnectParams = class( TtiBaseObject )
  private
    FsUserName       : string;
    FsDatabaseName   : string;
    FsUserPassword   : string;
    FbCancelled      : boolean;
    FsConnectionName : string;
    FsHostName       : string;
    FiTraceLevel     : integer;
    FParams: string;
    function  GetAsString: string;
    procedure SetAsString(const Value: string);
  published
    property ConnectionName : string read FsConnectionName write FsConnectionName ;
    property DatabaseName : string   read FsDatabaseName   write FsDatabaseName ;
    property UserName     : string   read FsUserName       write FsUserName     ;
    property UserPassword : string   read FsUserPassword   write FsUserPassword ;
    property HostName     : string   read FsHostName       write FsHostName     ;
    property Params       : string   read FParams          Write FParams ;
  public
    constructor Create ;
    constructor CreateExt( const psConnectionDetails : string ) ;
    property    Cancelled    : boolean read FbCancelled    write FbCancelled ;
    property    AsString     : string  read GetAsString    write SetAsString ;
    property    TraceLevel   : integer read FiTraceLevel   write FiTraceLevel ;
    function    UserPasswordMask : string ;
    function    Validate     : boolean ;
    procedure   Assign( pData : TDBConnectParams ) ; reintroduce ;
  end ;

  // A List of TDBConnectParams
  TDBConnectParamsList = class( TObjectList )
    function FindByConnectionName( const psConnectionName : string ) : TDBConnectParams ;
  end ;

  // Form to promt user for DatabaseName, UserName and UserPassword
  TFormPromptForLogon = class( TForm )
  private
    FDBConnectParams: TDBConnectParams;
    FlblDatabaseName : TLabel ;
    FlblUserName     : TLabel ;
    FlblUserPassword : TLabel ;
    FeDatabaseName   : TEdit ;
    FeUserName       : TEdit ;
    FeUserPassword   : TEdit ;
    FbbCancel        : TBitBtn ;
    FbbLogon         : TBitBtn ;
    procedure DoCancelClick( sender : TObject ) ;
    procedure DoLogonClick(  sender : TObject ) ;
    procedure SetDBConnectParams(const Value: TDBConnectParams);
  public
    constructor CreateNew( pOwner : TComponent ; Dummy : integer = 0 ) ; override ;
    property    DBConnectParams : TDBConnectParams read FDBConnectParams write SetDBConnectParams ;
  end ;

  // A pooled database connection
  TPooledDB = class( TPooledItem )
  private
    function GetDatabase: TtiDatabase;
    procedure SetDatabase(const Value: TtiDatabase);
  public
    constructor Create( pOwner : TtiPool ) ; override ;
    destructor  Destroy ; override ;
    property    Database : TtiDatabase read GetDatabase write SetDatabase ;
    procedure   Connect( pDBConnectParams : TDBConnectParams ) ;
    function    Owner : TDBConnectionPool ; reintroduce ;
    function    MustRemoveItemFromPool(pListCount: Integer) : boolean ; override ;
  end ;

  TPooledDBEvent = procedure ( pPooledDB : TPooledDB ) of object ;

  TtiDBConnectionPoolDataAbs = class( TtiBaseObject )
  private
    FDBConnectionPool: TDBConnectionPool;
  public
    property  DBConnectionPool : TDBConnectionPool read FDBConnectionPool write FDBConnectionPool ;
    procedure InitDBConnectionPool ; virtual ;
  end ;

  TtiDBConnectionPoolDataClass = class of TtiDBConnectionPoolDataAbs ;

  // The database connection pool
  TDBConnectionPool = class( TtiPool )
  private
    FDBConnectParams : TDBConnectParams ;
    FbInitCalled : boolean ;
    FMetaData : TtiDBMetaData ;
    procedure DoOnAddPooledItem( pPooledItem : TPooledItem ) ;
    procedure AddConnection ;
    procedure Init ;
    function  GetMetaData: TtiDBMetaData;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    DBConnectParams : TDBConnectParams read FDBConnectParams write FDBConnectParams ;
    function    Lock : TPooledDB ; reintroduce ;
    procedure   UnLock( pDBConnection : TPooledDB ) ; reintroduce ;
    // This will connect using the parameters passed. If connection is not successful,
    // a dialog will show asking the user to enter details.
    procedure   Connect( const psDatabaseName, psUserName, psUserPassword, pParams : string ) ; overload ;
    property    MetaData : TtiDBMetaData read GetMetaData ;
    function    DetailsAsString : string ;

  end ;

  // Maintains a list of TDBConnectionPool(s) so a single app can connect to
  // multiple databases.
  TDBConnectionPools = class( TtiObject )
  private
    FList : TObjectList ;
    function GetItems(i: integer): TDBConnectionPool;
  public
    Constructor Create ; override ;
    Destructor  Destroy ; override ;
    function    Lock(       const psDBConnectionName : string ) : TPooledDB ;
    // ToDo: Unlock should be able to work out which database pool from the
    // PDBConnection parameter. No need for the psConnectionName param.
    procedure   UnLock(       const psDBConnectionName : string ; pDBConnection : TPooledDB ) ;
    procedure   UnLockByData( const psDBConnectionName : string ; pDBConnection : TtiDatabase ) ;
    procedure   Connect(      const psDatabaseName, psUserName, psUserPassword : string ; const pParams : string ) ;
    procedure   DisConnect(   const psConnectionName : string ) ;
    procedure   DisConnectAll ;
    function    Find(         const psDBConnectionName : string ) : TDBConnectionPool ; reintroduce ;
    function    DetailsAsString : string ;
    procedure   Clear ;
    function    IsConnected( const pDBConnectionName : string ) : boolean ;
    function    Count : integer ;
    property    Items[i:integer]:TDBConnectionPool read GetItems ;
  end ;


implementation
uses
  tiLog
  ,tiUtils
  ,tiEncrypt
  ,tiOPFManager
  ,tiPersistenceLayers
  ,tiConstants
  ,tiExcept
  ,Math
  ;

const

  cusParamDelim   =   '|' ;

  cusConnectionNames = 'ConnectionNames' ;
  cusFileName        = 'DBParams.DCD' ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TPooledDB
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TPooledDB.Create( pOwner : TtiPool );
begin
  inherited create( pOwner ) ;
end;

destructor TPooledDB.destroy;
begin
  inherited destroy ;
end;

procedure TPooledDB.Connect(pDBConnectParams: TDBConnectParams);
begin
  Database.TraceLevel := pDBConnectParams.TraceLevel;
  Database.Connect( pDBConnectParams.DatabaseName,
                    pDBConnectParams.UserName,
                    pDBConnectParams.UserPassword,
                    pDBConnectParams.Params ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TDBConnectParams
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TDBConnectParams.Create;
begin
  inherited ;
  FsUserPassword := ''    ;
  FsUserName     := ''    ;
  FsDatabaseName := ''    ;
  FsUserPassword := ''    ;
  FiTraceLevel   := 0     ;
  FbCancelled    := false ;
end;

constructor TDBConnectParams.CreateExt(const psConnectionDetails : string);
begin
  Create ;
  AsString := psConnectionDetails ;
end;

procedure TDBConnectParams.Assign(pData: TDBConnectParams);
begin
  ConnectionName := pData.ConnectionName ;
  HostName       := pData.HostName       ;
  DatabaseName   := pData.DatabaseName   ;
  UserName       := pData.UserName       ;
  UserPassword   := pData.UserPassword   ;
  TraceLevel     := pData.TraceLevel     ;
  Cancelled      := pData.Cancelled      ;
end;

function TDBConnectParams.UserPasswordMask: string;
begin
  result := StringOfChar( '*', length( UserPassword )) ;
end;

function TDBConnectParams.Validate: boolean;
begin
  result := ( DatabaseName <> '' ) and
            ( UserName <> '' ) and
            ( UserPassword <> '' ){ and
            ( HostName <> '' )} ;
            // No host name here as we are just validating for user input at logon.
end;

procedure TDBConnectionPool.AddConnection;
begin
  Init ;
  AddItem ;
end;

procedure TDBConnectionPool.Connect(const psDatabaseName,
                                          psUserName,
                                          psUserPassword,
                                          pParams: string ) ;
begin
  try
    DBConnectParams.ConnectionName := psDatabaseName ;
    DBConnectParams.DatabaseName   := psDatabaseName ;
    DBConnectParams.UserName       := psUserName ;
    DBConnectParams.UserPassword   := psUserPassword ;
    DBConnectParams.Params         := pParams;
    AddConnection
  except
    on e:exception do
      if gTIOPFManager.TerminateOnFailedDBConnection then
      begin
        gTIOPFManager.Terminate ;
        raise ;
      end else
        raise;
  end ;
end ;

constructor TDBConnectionPool.Create;
begin
  inherited ;
  PooledItemClass  := TPooledDB ;
  OnAddPooledItem  := {$IFDEF FPC}@{$ENDIF}DoOnAddPooledItem ;
  FDBConnectParams := TDBConnectParams.Create ;
  FbInitCalled  := false ;
end ;

destructor TDBConnectionPool.Destroy;
begin
  FDBConnectParams.Free ;
//  FDBConnectionPoolData.Free ;
  FMetaData.Free ;
  inherited;
end;

procedure TDBConnectionPool.DoOnAddPooledItem(pPooledItem: TPooledItem);
var
  lRegPerLayer : TtiPersistenceLayer ;
begin
  lRegPerLayer := ( Owner.Owner as TtiPersistenceLayer ) ;
  TPooledDB( pPooledItem ).Database := lRegPerLayer.tiDatabaseClass.Create ;
  TPooledDB( pPooledItem ).Connect( DBConnectParams ) ;
end;

function TDBConnectionPool.Lock: TPooledDB;
begin
  result := TPooledDB( inherited Lock ) ;
end;

procedure TDBConnectionPool.UnLock(pDBConnection: TPooledDB);
begin
  Assert( pDBConnection.TestValid(TPooledDB), cErrorTIPerObjAbsTestValid );
  Assert( not pDBConnection.Database.InTransaction, 'Database in transaction immediately before being unlocked in DBConnectionPool.' ) ;
  inherited UnLock( pDBConnection ) ;
end;

constructor TFormPromptForLogon.CreateNew(pOwner: TComponent ; Dummy : integer = 0 );
begin
  inherited CreateNew( pOwner, Dummy );
  FormStyle   := fsStayOnTop ;
  BorderStyle := bsDialog ;
  BorderIcons := [] ;
  Position    := poScreenCenter ;
  Width       := 177 ;
  Height      := 207 ;
  Caption     := ' Database logon' ;

  FeDatabaseName := TEdit.Create( self ) ;
  with FeDatabaseName do begin
    parent   := self ;
    Left     := 20   ;
    Top      := 28   ;
    Width    := 137  ;
    Height   := 21   ;
    TabOrder := 0    ;
  end ;

  FeUserName := TEdit.Create( self ) ;
  with FeUserName do begin
    Parent   := self ;
    Left     := 20   ;
    Top      := 68   ;
    Width    := 137  ;
    Height   := 21   ;
    TabOrder := 1    ;
  end ;

  FeUserPassword := TEdit.Create( self ) ;
  with FeUserPassword do begin
    Parent := self ;
    Left   := 20 ;
    Top    := 112;
    Width  := 137;
    Height := 21;
    PasswordChar := '*';
    {$IFDEF LINUX}
    EchoMode := emPassword;
    {$ENDIF LINUX}
    TabOrder := 2;
  end;

  FlblDatabaseName := TLabel.Create( self ) ;
  with FlblDatabaseName do begin
    parent       := self ;
    Left         := 8  ;
    Top          := 12 ;
    Width        := 75 ;
    Height       := 13 ;
    Caption      := '&Database name' ;
    FocusControl := FeDatabaseName ;
  end ;

  FlblUserName := TLabel.Create( self ) ;
  with FlblUserName do begin
    parent       := self ;
    Left         :=  8 ;
    Top          := 52 ;
    Width        := 51 ;
    Height       := 13 ;
    Caption      := '&User name' ;
    FocusControl := FeUserName ;
  end ;

  FlblUserPassword := TLabel.Create( self ) ;
  with FlblUserPassword do begin
    parent       := self ;
    Left         := 8             ;
    Top          := 96            ;
    Width        := 46            ;
    Height       := 13            ;
    Caption      := '&Password'   ;
    FocusControl := FeUserPassword ;
  end ;

  FbbLogon := TBitBtn.Create( self ) ;
  with FbbLogon do begin
    Parent   := self ;
    Left     := 8;
    Top      := 148;
    Width    := 75;
    Height   := 25;
    Kind     := bkOK;
    Caption  := '&Logon';
    Default  := True;
    ModalResult := mrNone ;
    TabOrder := 3;
    OnClick  := {$IFDEF FPC}@{$ENDIF}DoLogonClick;
  end ;

  FbbCancel := TBitBtn.Create( self ) ;
  with FbbCancel do begin
    Parent   := self;
    Left     := 88;
    Top      := 148;
    Width    := 75;
    Height   := 25;
    Kind     := bkCancel ;
    Cancel   := True;
    Caption  := '&Cancel';
    ModalResult := mrNone ;
    TabOrder := 4;
    OnClick  := {$IFDEF FPC}@{$ENDIF}DoCancelClick ;
  end;

end;

procedure TFormPromptForLogon.DoCancelClick(sender: TObject);
begin
  FDBConnectParams.Cancelled := true ;
  ModalResult := mrCancel ;
  Close ;
end;

procedure TFormPromptForLogon.DoLogonClick(sender: TObject);
begin
  if FeDatabaseName.Text = '' then begin
    FeDatabaseName.SetFocus ;
    raise exception.Create( 'Please enter a database name' ) ;
  end ;
  FDBConnectParams.DatabaseName := FeDatabaseName.Text ;

  if FeUserName.Text = '' then begin
    FeUserName.SetFocus ;
    raise exception.Create( 'Please enter a user name' ) ;
  end ;
  FDBConnectParams.UserName     := FeUserName.Text     ;

  if FeUserPassword.Text = '' then begin
    FeUserPassword.SetFocus ;
    raise exception.Create( 'Please enter a password' ) ;
  end ;
  FDBConnectParams.UserPassword := FeUserPassword.Text ;

  ModalResult := mrOK ;
  Close ;
end;

procedure TFormPromptForLogon.SetDBConnectParams( const Value: TDBConnectParams);
begin
  FDBConnectParams    := Value;
  FeDatabaseName.Text := FDBConnectParams.DatabaseName ;
  FeUserName.Text     := FDBConnectParams.UserName     ;
  FeUserPassword.Text := FDBConnectParams.UserPassword ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TDBConnectParamsList
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TDBConnectParamsList.FindByConnectionName(
  const psConnectionName: string): TDBConnectParams;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( TDBConnectParams( Items[i] ).ConnectionName,
                psConnectionName ) then begin
      result := TDBConnectParams( Items[i] ) ;
      break ; //==>
    end ;
end;

function TDBConnectParams.GetAsString: string;
begin
  result := ConnectionName + cusParamDelim +
            HostName       + cusParamDelim +
            DatabaseName   + cusParamDelim +
            UserName       + cusParamDelim +
            UserPassword   ;
end;

procedure TDBConnectParams.SetAsString(const Value: string);
begin
  ConnectionName := tiToken( Value, cusParamDelim, 1 ) ;
  HostName       := tiToken( Value, cusParamDelim, 2 ) ;
  DatabaseName   := tiToken( Value, cusParamDelim, 3 ) ;
  UserName       := tiToken( Value, cusParamDelim, 4 ) ;
  UserPassword   := tiToken( Value, cusParamDelim, 5 ) ;
end;

procedure TtiDBConnectionPoolDataAbs.InitDBConnectionPool;
begin
  // Do nothing, implement in the concrete
end;

procedure TDBConnectionPool.Init;
var
  lRegPerLayer : TtiPersistenceLayer ;
  lDBConnectionPoolData : TtiDBConnectionPoolDataAbs ;
begin
  if FbInitCalled then
    Exit ; //==>
  FbInitCalled := true ;

  lRegPerLayer := ( Owner.Owner as TtiPersistenceLayer ) ;
  lDBConnectionPoolData := lRegPerLayer.tiDBConnectionPoolDataClass.Create;
  try
    lDBConnectionPoolData.DBConnectionPool := self ;
    lDBConnectionPoolData.InitDBConnectionPool ;
  finally
    lDBConnectionPoolData.Free;
  end;
end ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TDBConnectionPools
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TDBConnectionPools.Create;
begin
  inherited ;
  FList := TObjectList.Create ;
end;

destructor TDBConnectionPools.Destroy;
begin
  FList.Free ;
  inherited;
end;

procedure TDBConnectionPools.Connect(const psDatabaseName,
  psUserName, psUserPassword, pParams: string);
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  lDBConnectionPool := Find( psDatabaseName ) ;
  if lDBConnectionPool <> nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorAttemptToAddDuplicateDBConnectionPool, [psDatabaseName + '/' + psUserName]);
  Log( 'Creating database connection pool for %s/%s', [psDatabaseName, psUserName], lsConnectionPool);
  lDBConnectionPool := TDBConnectionPool.Create ;
  try
    lDBConnectionPool.Owner := self ;
    lDBConnectionPool.Connect( psDatabaseName, psUserName, psUserPassword, pParams ) ;
    FList.Add( lDBConnectionPool ) ;
  except
    on e:exception do
    begin
      lDBConnectionPool.Free ;
      Raise ;
    end ;
  end ;
end;

function TDBConnectionPools.Find( const psDBConnectionName: string): TDBConnectionPool;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FList.Count - 1 do
    if SameText( TDBConnectionPool( FList.Items[i] ).DBConnectParams.ConnectionName,
                 psDBConnectionName ) then
    begin
      result := TDBConnectionPool( FList.Items[i] ) ;
      Exit ; //==>
    end ;
end;

function TDBConnectionPools.Lock( const psDBConnectionName: string): TPooledDB;
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  // Try to find a pool of DB connections
  lDBConnectionPool := Find( psDBConnectionName ) ;
  // Some error checking
  if lDBConnectionPool = nil then
    EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [psDBConnectionName]);
  // Success. So try and lock a connection.
  result := lDBConnectionPool.Lock ;
end;

procedure TDBConnectionPools.UnLock(const psDBConnectionName: string;
                                      pDBConnection: TPooledDB);
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  lDBConnectionPool := Find( psDBConnectionName ) ;
  Assert( lDBConnectionPool.TestValid(TDBConnectionPool, True), cErrorTIPerObjAbsTestValid );
  if lDBConnectionPool = nil then
    EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [psDBConnectionName]);
  lDBConnectionPool.UnLock( pDBConnection ) ;
end;

function TPooledDB.Owner: TDBConnectionPool;
begin
  result := TDBConnectionPool( Inherited Owner ) ;
end;

function TDBConnectionPool.GetMetaData: TtiDBMetaData;
begin
  if FMetaData = nil then
  begin
      FMetaData := TtiDBMetaData.Create ;
      FMetaData.Owner := Self ;
      FMetaData.Read ;
  end ;
  result := FMetaData ;
end;

function TDBConnectionPools.DetailsAsString: string;
var
  i : integer ;
begin
  result := '' ;
  // This is not thread safe!
  for i := 0 to FList.Count - 1 do
  begin
    result := tiAddTrailingValue( result, CrLf( 2 ), true ) ;
    result := result + TDBConnectionPool( FList.Items[i] ).DetailsAsString ;
  end ;
end;

function TDBConnectionPool.DetailsAsString: string;
var
  lDBConnectionPools : TDBConnectionPools ;
  lRegPerLayer : TtiPersistenceLayer ;
begin
  Assert( Owner.TestValid(TDBConnectionPools), cTIInvalidObjectError );
  lDBConnectionPools := Owner as TDBConnectionPools ;
  Assert( lDBConnectionPools.Owner.TestValid(TtiPersistenceLayer), cTIInvalidObjectError );
  lRegPerLayer := lDBConnectionPools.Owner as TtiPersistenceLayer ;
  result :=
    'Persistence layer:   ' + lRegPerLayer.PerLayerName + Cr +
    'Database name:       ' + DBConnectParams.DatabaseName + Cr +
    'User name:           ' + DBConnectParams.UserName     + Cr +
    'Password:            ' + tiReplicate( 'X', Length( DBConnectParams.UserPassword )) + Cr +
    'Number in pool:      ' + IntToStr( Count ) ;
end;

procedure TDBConnectionPools.Clear;
begin
  FList.Clear ;
end;

procedure TDBConnectionPools.DisConnect(const psConnectionName: string);
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  lDBConnectionPool := Find( psConnectionName ) ;
  if lDBConnectionPool =  nil then
    EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [psConnectionName]);
  FList.Extract( lDBConnectionPool ) ;
  lDBConnectionPool.Free ;
end;

function TDBConnectionPools.IsConnected( const pDBConnectionName: string): boolean;
begin
  result := ( Find( pDBConnectionName ) <> nil ) ;
end;

function TDBConnectionPools.Count: integer;
begin
  result := FList.Count ;
end;

function TDBConnectionPools.GetItems(i: integer): TDBConnectionPool;
begin
  result := TDBConnectionPool(FList.Items[i])
end;

function TPooledDB.GetDatabase: TtiDatabase;
begin
  Assert( Data.TestValid(TtiDatabase), cErrorTIPerObjAbsTestValid );
  result := Data as TtiDatabase ;
end;

procedure TPooledDB.SetDatabase(const Value: TtiDatabase);
begin
  Data := Value ;
end;

procedure TDBConnectionPools.UnLockByData(const psDBConnectionName: string; pDBConnection: TtiDatabase);
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  lDBConnectionPool := Find( psDBConnectionName ) ;
  if lDBConnectionPool = nil then
    EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindDBConnectionPool, [psDBConnectionName]);
  lDBConnectionPool.UnLockByData( pDBConnection ) ;
end;

procedure TDBConnectionPools.DisConnectAll;
var
  i : integer ;
begin
  for i := Count - 1 downto 0 do
    DisConnect(Items[i].DBConnectParams.DatabaseName);
end;

function TPooledDB.MustRemoveItemFromPool(pListCount: Integer): Boolean;
begin
  Assert( Database.TestValid(TtiDatabase), cTIInvalidObjectError );
  result :=
    ( Inherited MustRemoveItemFromPool(pListCount)) or
    ( Database.ErrorInLastCall ) ;
end;

end.


