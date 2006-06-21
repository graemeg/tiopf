{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Originally developed by TechInsite Pty. Ltd.
  23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
  PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
  Phone: +61 3 9419 6456
  Fax:   +61 3 9419 1682
  Web:   www.techinsite.com.au       

  This code is made available on the TechInsite web site as open source.
  You may use this code in any way you like, except to sell it to other
  developers. Please be sure to leave the file header and list of
  contributors unchanged.

  If you make any changes or enhancements, which you think will benefit other
  developers and will not break any existing code, please forward your changes
  (well commented) to TechInsite and I will make them available in the next
  version.

  Revision history:
    Sept 1999, PWH, Created
    November 2000, Peter Hinrichsen, Made open source

  Purpose:
      Maintain a pool of database connections.
      Manage the locking and unlocking of these connections.

  Classes:
    TDBConnectParams               - A holder for database connection info:
                                     DatabaseName, UserName, Password.
    TDBConnectParamsList           - A list of DBConnectParams
    TFormPromptForLogon            - Form to promt user for DatabaseName,
                                     UserName and UserPassword
    TDBConnectParamsBulderAbs      - Abstract DBConnectParamsBuilder
    TDBConenctParamsBuilderPrompt  - DBConnectParamsBuilder for managing
                                     user logon through logon form
    TDBConenctParamsBuilderEncrypt - DBConnectParamsBuilder for managing
                                     automatic logon through encrypted file
    TPooledDB                      - A pooled database connection
    TDBConnectionPool              - The database connection pool
    TDBConnectionPoolMgr           - Maintains a list of TDBConnectionPool(s)


  ToDo:
    1. DBConnection pool to detect if a query has changed in the database and
       refresh itself if necessary.
    2. Add management routines to see which databases are connected to, and
       how many connections, etc.
    3. Add code to check a database connection is still active (ie not terminated
       by db server, or network connection) before allowing a lock.
    4. Add hook for logging of a user's activity.
    5. Add a hook for passing a user's name without the TtiQueries having to
       access the global user object.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiDBConnectionPool;

interface
uses
  Classes
  ,Contnrs
  {$IFDEF MSWINDOWS}
  ,Windows
  ,Forms
  ,Controls
  ,StdCtrls
  ,Buttons
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QForms
  ,QControls
  ,QStdCtrls
  ,QButtons
  ,Libc
  {$ENDIF LINUX}
  ,SysUtils
  ,tiQuery
  ,tiPoolAbs
  ,tiCommandLineParams
  ,tiPtnVisPerObj
  ;


type

  TDBConnectParams    = class ;
  TDBConnectionPool   = class ;

  // A holder for database connection info: DatabaseName, UserName, Password.
  // ---------------------------------------------------------------------------
  TDBConnectParams = class( TPersistent )
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
    constructor create ;
    constructor CreateExt( const psConnectionDetails : string ) ;
    property    Cancelled    : boolean read FbCancelled    write FbCancelled ;
    property    AsString     : string  read GetAsString    write SetAsString ;
    property    TraceLevel   : integer read FiTraceLevel   write FiTraceLevel ;
    function    UserPasswordMask : string ;
    function    Validate     : boolean ;
    procedure   Assign( pData : TDBConnectParams ) ; reintroduce ;
  end ;

  // A List of TDBConnectParams
  // ---------------------------------------------------------------------------
  TDBConnectParamsList = class( TObjectList )
    function FindByConnectionName( const psConnectionName : string ) : TDBConnectParams ;
  end ;

  // ---------------------------------------------------------------------------
  TDBConnectParamsListSerialiser = class( TObject )
  private
    FFileName: TFileName;
    FEncryptionType: string;
    procedure ReadToStringList(pSL: TStringList);
    procedure SaveFromStringList(pSL: TStringList);
  public
    constructor create ;
    property    FileName : TFileName read FFileName write FFileName ;
    procedure   Read( pDBConnectParamsList : TDBConnectParamsList ) ;
    procedure   Write( pDBConnectParamsList : TDBConnectParamsList ) ;
    property    EncryptionType : string read FEncryptionType write FEncryptionType ;
  end ;

  // Form to promt user for DatabaseName, UserName and UserPassword
  // ---------------------------------------------------------------------------
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
    constructor CreateNew( Owner : TComponent ; Dummy : integer = 0 ) ; override ;
    property    DBConnectParams : TDBConnectParams read FDBConnectParams write SetDBConnectParams ;
  end ;

  // Abstract DBConnectParamsBuilder
  // ---------------------------------------------------------------------------
  TDBConnectParamsBulderAbs = class( TObject )
  private
    FDBConnectionPool: TDBConnectionPool;
  protected
    procedure Execute ; virtual ; abstract ;
    property  DBConnectionPool : TDBConnectionPool read FDBConnectionPool write FDBConnectionPool ;
  end ;

  // DBConnectParamsBuilder for managing user logon through logon form
  // ---------------------------------------------------------------------------
  TDBConenctParamsBuilderPrompt = class( TDBConnectParamsBulderAbs )
  private
    procedure   PromptForLogon ;
  public
    procedure   Execute ; override ;
  end ;

  // ToDo: Have a good think about the location of this class as it is pulling
  //       Encrypt into the core package, which we probably don't want.
  // DBConnectParamsBuilder for managing automatic logon through encrypted file
  // ---------------------------------------------------------------------------
  TDBConenctParamsBuilderEncrypt = class( TDBConnectParamsBulderAbs )
  private
    FsConnectionName: string;
  public
    procedure   Execute ; override ;
    property    ConnectionName : string read FsConnectionName write FsConnectionName ;
  end ;

  // A pooled database connection
  // ---------------------------------------------------------------------------
  TPooledDB = class( TPooledItem )
  private
    function GetDatabase: TtiDatabase;
    procedure SetDatabase(const Value: TtiDatabase);
  public
    constructor create( pOwner : TtiPool ) ; override ;
    destructor  destroy ; override ;
    property    Database : TtiDatabase read GetDatabase write SetDatabase ;
    procedure   Connect( pDBConnectParams : TDBConnectParams ) ;
    function    Owner : TDBConnectionPool ; reintroduce ;
    function    MustRemoveItemFromPool(pListCount: Integer) : boolean ; override ;
  end ;

  TPooledDBEvent = procedure ( pPooledDB : TPooledDB ) of object ;

  // ---------------------------------------------------------------------------
  TtiDBConnectionPoolDataAbs = class( TPersistent )
  private
    FDBConnectionPool: TDBConnectionPool;
  public
    property  DBConnectionPool : TDBConnectionPool read FDBConnectionPool write FDBConnectionPool ;
    procedure InitDBConnectionPool ; virtual ;
  end ;

  TtiDBConnectionPoolDataClass = class of TtiDBConnectionPoolDataAbs ;

  // The database connection pool
  // ---------------------------------------------------------------------------
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
    // This will connect using the connection string passed. The encrypted
    // database connection detail file DBParams.DCD will be searched for the
    // username and password.
    procedure   Connect( const psConnectionName : string ) ; overload ;
    // This will connect using the parameters passed. If connection is not successful,
    // a dialog will show asking the user to enter details.
    procedure   Connect( const psDatabaseName, psUserName, psUserPassword, pParams : string ; pInteractive : boolean = false ) ; overload ;
    property    MetaData : TtiDBMetaData read GetMetaData ;
    function    DetailsAsString : string ;

  end ;

  // Maintains a list of TDBConnectionPool(s) so a single app can connect to
  // multiple databases.
  // ---------------------------------------------------------------------------
  TDBConnectionPools = class( TPerObjAbs )
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
    procedure   Connect(      const psConnectionName : string ) ; overload ;
    procedure   Connect(      const psDatabaseName, psUserName, psUserPassword : string ; const pParams : string ; pInteractive : boolean = false ) ; overload ;
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
  ,tiEncryptAbs
  ,tiEncryptSimple
  ,tiPersist
  ,Math
  ,tiDialogs
  ,tiRegPerLayer
  ,cTIPersist
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
constructor TPooledDB.create( pOwner : TtiPool );
begin
  inherited create( pOwner ) ;
end;

//------------------------------------------------------------------------------
destructor TPooledDB.destroy;
begin
  inherited destroy ;
end;

//------------------------------------------------------------------------------
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
constructor TDBConnectParams.create;
begin
  inherited ;
  FsUserPassword := ''    ;
  FsUserName     := ''    ;
  FsDatabaseName := ''    ;
  FsUserPassword := ''    ;
  FiTraceLevel   := 0     ;
  FbCancelled    := false ;
end;

//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
function TDBConnectParams.Validate: boolean;
begin
  result := ( DatabaseName <> '' ) and
            ( UserName <> '' ) and
            ( UserPassword <> '' ){ and
            ( HostName <> '' )} ;
            // No host name here as we are just validating for user input at logon.
end;

// -----------------------------------------------------------------------------
procedure TDBConnectionPool.Connect(const psConnectionName: string) ;
var
  lBldr : TDBConenctParamsBuilderEncrypt ;
begin
  Clear ;
  lBldr := TDBConenctParamsBuilderEncrypt.Create ;
  try
    lBldr.ConnectionName := psConnectionName ;
    lBldr.DBConnectionPool := self ;
    lBldr.Execute ;
  finally
    lBldr.Free;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TDBConnectionPool.AddConnection;
begin
  Init ;
  AddItem ;
end;

procedure TDBConnectionPool.Connect(const psDatabaseName,
                                          psUserName,
                                          psUserPassword,
                                          pParams: string ;
                                          pInteractive: boolean = false ) ;
var
  lBldr : TDBConenctParamsBuilderPrompt ;
begin
  try
    DBConnectParams.ConnectionName := psDatabaseName ;
    DBConnectParams.DatabaseName   := psDatabaseName ;
    DBConnectParams.UserName       := psUserName ;
    DBConnectParams.UserPassword   := psUserPassword ;
    DBConnectParams.Params         := pParams;
    if not pInteractive then
      AddConnection
    else
    begin
      Clear ;
      lBldr := TDBConenctParamsBuilderPrompt.Create ;
      try
        lBldr.DBConnectionPool := self ;
        lBldr.Execute ;
      finally
        lBldr.Free;
      end ;
    end;
  except
    on e:exception do
      if gTIPerMgr.TerminateOnFailedDBConnection then
      begin
        gLog.TerminateGUILogs;
        gTIPerMgr.Terminate ;
        tiAppError( e.message ) ;
        LogError( e, false ) ;
        LogError( 'About to call HALT from ' + ClassName + '.SetConnected', false);
        // Or Halt ?
        //Application.Terminate ;
        Halt;
      end else
        raise;
  end ;
end ;

constructor TDBConnectionPool.Create;
begin
  inherited ;
  PooledItemClass  := TPooledDB ;
  OnAddPooledItem  := DoOnAddPooledItem ;
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
  lRegPerLayer : TtiRegPerLayer ;
begin
  lRegPerLayer := ( Owner.Owner as TtiRegPerLayer ) ;
  TPooledDB( pPooledItem ).Database := lRegPerLayer.tiDatabaseClass.Create ;
  TPooledDB( pPooledItem ).Connect( DBConnectParams ) ;
end;

function TDBConnectionPool.Lock: TPooledDB;
begin
  result := TPooledDB( inherited Lock ) ;
  Assert( not result.Database.InTransaction, 'Database in transaction immediately after being locked in DBConnectionPool.' ) ;
end;

procedure TDBConnectionPool.UnLock(pDBConnection: TPooledDB);
begin
  Assert( not pDBConnection.Database.InTransaction, 'Database in transaction immediately before being unlocked in DBConnectionPool.' ) ;
  inherited UnLock( pDBConnection ) ;
end;

{ TDBConenctParamsBuilderPrompt }

procedure TDBConenctParamsBuilderPrompt.Execute ;
var
  liTryCount : integer ;
begin
  Log( 'Running TDBConenctParamsBuilderPrompt.Execute', lsConnectionPool ) ;

  if not DBConnectionPool.DBConnectParams.Validate then
    PromptForLogon ;

  try
    DBConnectionPool.AddConnection ;
    Exit ; //==>
  except
    // Swallow the exception, and popup a dialog asking the user to enter database, username and password.
    // ToDo: Will no longer want this. Fix.
    on e:exception do
      Log( 'Attempt to connect caused error: ' + e.message, lsConnectionPool ) ;
  end ;

  liTryCount := 0 ;
  while ( not DBConnectionPool.DBConnectParams.Cancelled ) and
        ( liTryCount <= 3 )  do begin
    PromptForLogon ;
    try
      DBConnectionPool.AddConnection ;
      Inc( liTryCount ) ;
      Break ; //==>
    except
      // Swallow the exception, ask the user again
      on e:exception do
        Log( 'Attempt to connect caused error: ' + e.message, lsConnectionPool ) ;
    end ;
  end ;

  if DBConnectionPool.DBConnectParams.Cancelled then
  begin
    LogError( 'About to call HALT from ' + ClassName + '.Execute' ) ;
    Halt ;
  end;

end;

procedure TDBConenctParamsBuilderPrompt.PromptForLogon ;
var
  lForm : TFormPromptForLogon ;
begin
  lForm := TFormPromptForLogon.CreateNew( nil ) ;
  try
    lForm.DBConnectParams := DBConnectionPool.DBConnectParams ;
    lForm.ShowModal ;
  finally
    lForm.Free ;
  end ;
end ;


{ TDBConenctParamsBuilderEncrypt }

procedure TDBConenctParamsBuilderEncrypt.Execute  ;
var
  lDBConnectParamsList : TDBConnectParamsList ;
  lSerialiser          : TDBConnectParamsListSerialiser ;
  lDBConnectParams     : TDBConnectParams ;
begin

  Log( 'Running TDBConenctParamsBuilderEncrypt.Execute', lsConnectionPool ) ;

  // Create a parameter list object
  lDBConnectParamsList := TDBConnectParamsList.Create ;

  try
    // Populate the parameter list object
    lSerialiser          := TDBConnectParamsListSerialiser.Create ;
    try
      lSerialiser.Read( lDBConnectParamsList ) ;
    finally
      lSerialiser.Free;
    end;

    // Find the appropriate DBConnectParams
    lDBConnectParams := lDBConnectParamsList.FindByConnectionName( ConnectionName ) ;

    if lDBConnectParams = nil then
    begin
      // ToDo: Fix this with standard exception handling.
      tiAppError( 'Unable to find database connection details for <' +
                  ConnectionName + '>' ) ;
      LogError( 'Unable to find database connection details for <' +
                  ConnectionName + '>' ) ;
      LogError( 'About to call HALT from ' + ClassName + '.Execute' ) ;
      Halt ;
    end ;

    // Assign the DBConnectParams values to DBConnectionPool
    DBConnectionPool.DBConnectParams.Assign( lDBConnectParams ) ;

  finally
    lDBConnectParamsList.Free ;
  end ;

  DBConnectionPool.AddConnection ;

end;

{ TFormPromptForLogon }

constructor TFormPromptForLogon.CreateNew(Owner: TComponent ; Dummy : integer = 0 );
begin
  inherited CreateNew( Owner, Dummy );
  FormStyle   := fsStayOnTop ;
  {$IFDEF MSWINDOWS}
  BorderStyle := bsDialog ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  BorderStyle := fbsDialog ;
  {$ENDIF LINUX}
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
    {$IFDEF MSWINDOWS}
    PasswordChar := '*';
    {$ENDIF MSWINDOWS}
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
    OnClick  := DoLogonClick;
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
    OnClick  := DoCancelClick ;
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
    raise exception.create( 'Please enter a database name' ) ;
  end ;
  FDBConnectParams.DatabaseName := FeDatabaseName.Text ;

  if FeUserName.Text = '' then begin
    FeUserName.SetFocus ;
    raise exception.create( 'Please enter a user name' ) ;
  end ;
  FDBConnectParams.UserName     := FeUserName.Text     ;

  if FeUserPassword.Text = '' then begin
    FeUserPassword.SetFocus ;
    raise exception.create( 'Please enter a password' ) ;
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

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TDBConnectParamsListSerialiser
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TDBConnectParamsListSerialiser.create;
begin
  inherited ;
  FFileName := tiAddTrailingSlash( tiGetEXEPath ) + cusFileName ;
  FEncryptionType := cgsEncryptionSimple ;
//  FEncryptionType := cgsEncryptionNone ;
end;

procedure TDBConnectParamsListSerialiser.Read( pDBConnectParamsList : TDBConnectParamsList ) ;
var
  lsl : TStringList ;
  i : integer ;
  ls : string ;
begin
  pDBConnectParamsList.Clear ;
  lsl := TStringList.Create ;
  try
    ReadToStringList( lsl ) ;
    for i := 0 to lsl.Count - 1 do
    begin
      ls := lsl.Strings[i] ;
      if Trim(ls)<>''then
        pDBConnectParamsList.Add(TDBConnectParams.CreateExt(ls)) ;
    end;
  finally
    lsl.Free ;
  end ;
end;

procedure TDBConnectParamsListSerialiser.ReadToStringList( pSL : TStringList ) ;
var
  lEncrypt : TtiEncryptAbs ;
  lString : string ;
begin
  lString := tiFileToString( FileName ) ;
  lEncrypt := gEncryptFactory.CreateInstance( EncryptionType ) ;
  try
    pSL.Text := lEncrypt.DecryptString( lString ) ;
  finally
    lEncrypt.Free ;
  end ;
end ;

procedure TDBConnectParamsListSerialiser.SaveFromStringList( pSL : TStringList ) ;
var
  lEncrypt : TtiEncryptAbs ;
  lString : string ;
begin
  lEncrypt := gEncryptFactory.CreateInstance( EncryptionType ) ;
  try
    lString := lEncrypt.EncryptString( Trim(pSL.Text)) ;
    tiStringToFile( lString, FileName ) ;
  finally
    lEncrypt.Free ;
  end ;
end ;


procedure TDBConnectParamsListSerialiser.Write( pDBConnectParamsList : TDBConnectParamsList ) ;
var
  lsl : TStringList ;
  i   : integer ;
begin
  lsl := TStringList.Create ;
  try
    // Write out the database connection details
    for i := 0 to pDBConnectParamsList.Count - 1 do
      lsl.Add( TDBConnectParams( pDBConnectParamsList.Items[i] ).AsString ) ;
    SaveFromStringList( lsl ) ;
  finally
    lsl.free ;
  end;
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

{ TtiDBConnectionPoolDataAbs }

procedure TtiDBConnectionPoolDataAbs.InitDBConnectionPool;
begin
  // Do nothing, implement in the concrete
end;

procedure TDBConnectionPool.Init;
var
  lRegPerLayer : TtiRegPerLayer ;
  lDBConnectionPoolData : TtiDBConnectionPoolDataAbs ;
begin
  if FbInitCalled then
    Exit ; //==>
  FbInitCalled := true ;

  lRegPerLayer := ( Owner.Owner as TtiRegPerLayer ) ;
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

// -----------------------------------------------------------------------------
destructor TDBConnectionPools.Destroy;
begin
  FList.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TDBConnectionPools.Connect(const psConnectionName: string);
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  lDBConnectionPool := Find( psConnectionName ) ;
  if lDBConnectionPool <> nil then
    tiFmtException( 'Attempt to add duplicate database connection pool <%s>',
                    [psConnectionName],
                    ClassName,
                    'Connect' ) ;

  lDBConnectionPool := TDBConnectionPool.Create ;
  try
    lDBConnectionPool.Owner := Self ;
    lDBConnectionPool.Connect( psConnectionName ) ;
    FList.Add( lDBConnectionPool ) ;
  except
    on e:exception do
    begin
      tiFmtException( e, 'Unable to create new database connection pool <' +
                      psConnectionName + '>', ClassName, 'Connect' ) ;
      lDBConnectionPool.Free ;
    end ;
  end ;

end;

// -----------------------------------------------------------------------------
procedure TDBConnectionPools.Connect(const psDatabaseName,
  psUserName, psUserPassword, pParams: string ; pInteractive : boolean = false );
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  lDBConnectionPool := Find( psDatabaseName ) ;
  if lDBConnectionPool <> nil then
    tiFmtException( 'Attempt to add duplicate database connection pool %s/%s',
                    [psDatabaseName, psUserName],
                    ClassName,
                    'Connect' ) ;
  LogFmt( 'Creating database connection pool for %s/%s',
       [psDatabaseName, psUserName], lsConnectionPool);
  lDBConnectionPool := TDBConnectionPool.Create ;
  try
    lDBConnectionPool.Owner := self ;
    lDBConnectionPool.Connect( psDatabaseName, psUserName, psUserPassword, pParams, pInteractive ) ;
    FList.Add( lDBConnectionPool ) ;
  except
    on e:exception do
    begin
      lDBConnectionPool.Free ;
      Raise ;
    end ;
  end ;
end;

// -----------------------------------------------------------------------------
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

// -----------------------------------------------------------------------------
function TDBConnectionPools.Lock( const psDBConnectionName: string): TPooledDB;
var
  lDBConnectionPool : TDBConnectionPool ;
begin

  // Try to find a pool of DB connections
  lDBConnectionPool := Find( psDBConnectionName ) ;

  // Not found? then try to create one
  if lDBConnectionPool = nil then
  begin
    Connect( psDBConnectionName ) ;
    lDBConnectionPool := Find( psDBConnectionName ) ;
  end ;

  // Some error checking
  if lDBConnectionPool = nil then
    tiFmtException( 'Unable to find database connection pool <%s>',
                    [psDBConnectionName],
                    ClassName,
                    'Lock' ) ;

  // Success. So try and lock a connection.
  result := lDBConnectionPool.Lock ;

end;

// -----------------------------------------------------------------------------
procedure TDBConnectionPools.UnLock(const psDBConnectionName: string;
                                      pDBConnection: TPooledDB);
var
  lDBConnectionPool : TDBConnectionPool ;
begin
  lDBConnectionPool := Find( psDBConnectionName ) ;
  if lDBConnectionPool = nil then
    tiFmtException( 'Unable to find database connection pool <%s>',
                    [psDBConnectionName],
                    ClassName,
                    'UnLock' ) ;
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
  lRegPerLayer : TtiRegPerLayer ;
begin
  Assert( Owner.TestValid(TDBConnectionPools), cTIInvalidObjectError );
  lDBConnectionPools := Owner as TDBConnectionPools ;
  Assert( lDBConnectionPools.Owner.TestValid(TtiRegPerLayer), cTIInvalidObjectError );
  lRegPerLayer := lDBConnectionPools.Owner as TtiRegPerLayer ;
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
    tiFmtException( 'Attempt to remove unconnected database connection pool <%s>',
                    [psConnectionName],
                    ClassName,
                    'DisConnect' ) ;

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
    tiFmtException( 'Unable to find database connection pool <%s>',
                    [psDBConnectionName],
                    ClassName,
                    'UnLock' ) ;
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

initialization
  Classes.RegisterClass( TtiDBConnectionPoolDataAbs ) ;

end.


