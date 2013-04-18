{
  This unit implements the base classes for the Free Pascal SQLDB persistence
  layer.

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldb;

{$I tiDefines.inc}

{ For debug purposes only }
{.$Define LOGSQLDB}


interface

uses
  Classes,
  SysUtils,
  DB,
  sqldb,
  tiQuery,
  tiQueryDataset,
  tiPersistenceLayers;

type

  TtiPersistenceLayerSqldDB = class(TtiPersistenceLayer)
    function GetQueryClass: TtiQueryClass; override;
  end;


  TtiDatabaseSQLDB = class(TtiDatabaseSQL)
  private
    FDatabase: TSQLConnection;
    FTransaction: TSQLTransaction;
  protected
    procedure SetConnected(AValue: Boolean); override;
    function GetConnected: Boolean; override;
    class function CreateSQLConnection: TSQLConnection; virtual; abstract;
    function HasNativeLogicalType: Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''): Boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string = ''); override;
    property SQLConnection: TSQLConnection read FDatabase write FDatabase;
    procedure StartTransaction; override;
    function InTransaction: Boolean; override;
    procedure Commit; override;
    procedure RollBack; override;
    function Test: Boolean; override;
    function TIQueryClass: TtiQueryClass; override;
  end;


  TtiQuerySQLDB = class(TtiQueryDataset)
  private
    FSQLQuery: TSQLQuery;
    FbActive: Boolean;
    procedure Prepare;
  protected
    procedure CheckPrepared; override;
    procedure SetActive(const AValue: Boolean); override;
    function GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    function GetFieldAsBoolean(const AName: string): Boolean; override;
    procedure SetParamAsBoolean(const AName: string; const AValue: Boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ExecSQL: integer; override;
    procedure AttachDatabase(ADatabase: TtiDatabase); override;
    procedure DetachDatabase; override;
    procedure Reset; override;
    function HasNativeLogicalType: Boolean; override;
  end;


implementation

uses
  tiUtils,
  tiLog,
  TypInfo,
  tiConstants,
  tiExcept,
  Variants;


{ TtiQuerySQLDB }

constructor TtiQuerySQLDB.Create;
begin
  inherited;
  FSQLQuery  := TSQLQuery.Create(nil);
  Dataset := FSQLQuery;
  Params  := FSQLQuery.Params;
  FSupportsRowsAffected := True;
end;

destructor TtiQuerySQLDB.Destroy;
begin
  Params  := nil;
  Dataset := nil;
  FSQLQuery.Free;
  inherited;
end;

function TtiQuerySQLDB.ExecSQL: integer;
begin
  Log(ClassName + ': [Prepare] ' + tiNormalizeStr(self.SQLText), lsSQL);
  Prepare;
  LogParams;
  FSQLQuery.ExecSQL;
  Result := FSQLQuery.RowsAffected;
end;

function TtiQuerySQLDB.GetFieldAsBoolean(const AName: string): Boolean;
var
  lBoolStr: string;
begin
  lBoolStr := FSQLQuery.FieldByName(AName).AsString;
  result := (lBoolStr = 'T') or
    (lBoolStr = 'TRUE') or
    (lBoolStr = 'Y') or
    (lBoolStr = 'YES') or
    (lBoolStr = '1');
end;

function TtiQuerySQLDB.GetSQL: TStrings;
begin
  Result := FSQLQuery.SQL;
end;

procedure TtiQuerySQLDB.SetActive(const AValue: Boolean);
begin
{$ifdef LOGSQLDB}
  log('>>> TtiQuerySQLDB.SetActive');
{$endif}
  Assert(Database.TestValid(TtiDatabase), CTIErrorInvalidObject);
  if AValue then
  begin
    {$ifdef LOGSQLDB}
    Log('Open Query');
    {$endif}
    FSQLQuery.Open;
    FbActive := True;
  end
  else
  begin
    {$ifdef LOGSQLDB}
    Log('Closing Query');
    {$endif}
    FSQLQuery.Close;
    FbActive := False;
  end;
{$ifdef LOGSQLDB}
  log('<<< TtiQuerySQLDB.SetActive');
{$endif}
end;

procedure TtiQuerySQLDB.SetParamAsBoolean(const AName: string;
  const AValue: Boolean);
begin
{$IFDEF BOOLEAN_CHAR_1}
  if AValue then
    FSQLQuery.Params.ParamByName(AName).AsString := 'T'
  else
    FSQLQuery.Params.ParamByName(AName).AsString := 'F'
{$ELSE}
  {$IFDEF BOOLEAN_NUM_1}
    if AValue then
      FSQLQuery.Params.ParamByName(AName).AsInteger := 1
    else
      FSQLQuery.Params.ParamByName(AName).AsInteger := 0;
  {$ELSE}
    if AValue then
      FSQLQuery.Params.ParamByName(AName).AsString := 'TRUE'
    else
      FSQLQuery.Params.ParamByName(AName).AsString := 'FALSE';
  {$ENDIF}
{$ENDIF BOOLEAN_CHAR_1}
end;

procedure TtiQuerySQLDB.SetSQL(const AValue: TStrings);
begin
{$ifdef LOGSQLDB}
  log('>>>> SetSQL: ' + AValue.Text);
{$endif}
  FSQLQuery.SQL.Assign(AValue);
{$ifdef LOGSQLDB}
  log('<<<< SetSQL');
{$endif}
end;

procedure TtiQuerySQLDB.CheckPrepared;
begin
{$ifdef LOGSQLDB}
  Log('>>> TtiQuerySQLDB.CheckPrepared');
{$endif}
  inherited CheckPrepared;
  Prepare;
{$ifdef LOGSQLDB}
  Log('<<< TtiQuerySQLDB.CheckPrepared');
{$endif}
end;

procedure TtiQuerySQLDB.Prepare;
begin
{$ifdef LOGSQLDB}
  Log('>>> TtiQuerySQLDB.Prepare');
{$endif}
  if FSQLQuery.Prepared then
    Exit; //==>
  FSQLQuery.Prepare;
{$ifdef LOGSQLDB}
  Log('<<< TtiQuerySQLDB.Prepare');
{$endif}
end;

procedure TtiQuerySQLDB.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  if (Database is TtiDatabaseSQLDB) then
  begin
    FSQLQuery.Database    := TtiDatabaseSQLDB(Database).SQLConnection;
    FSQLQuery.Transaction := TtiDatabaseSQLDB(Database).SQLConnection.Transaction;
  end;
end;

procedure TtiQuerySQLDB.DetachDatabase;
begin
  inherited DetachDatabase;
  if FSQLQuery.Active then
    FSQLQuery.Close;
  FSQLQuery.Transaction := nil;
  FSQLQuery.Database    := nil;
end;

procedure TtiQuerySQLDB.Reset;
begin
  Active := False;
  FSQLQuery.SQL.Clear;
end;

function TtiQuerySQLDB.HasNativeLogicalType: Boolean;
begin
  if not Assigned(Database) then
    Result := True
  else
    Result := TtiDatabaseSQLDB(Database).HasNativeLogicalType;
end;


{ TtiDatabaseSQLDB }

constructor TtiDatabaseSQLDB.Create;
begin
  inherited Create;
  FDatabase           := CreateSQLConnection;
  FDatabase.LoginPrompt := False;
  FTransaction        := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FDatabase;
  FDatabase.Transaction := FTransaction;
  FTransaction.Active := False;
end;

destructor TtiDatabaseSQLDB.Destroy;
begin
  try
    FTransaction.Active   := False;
    FDatabase.Connected   := False;
    FDatabase.Transaction := nil;
    FTransaction.Database := nil;
    FTransaction.Free;
    FDatabase.Free;
  except
    {$ifdef logsqldb}
    on e: Exception do
      LogError(e.message);
    {$endif}
  end;
  inherited;
end;

procedure TtiDatabaseSQLDB.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');

  Log(ClassName + ': [Commit Trans]', lsSQL);
  //  FTransaction.CommitRetaining;
  FTransaction.Commit;
end;

function TtiDatabaseSQLDB.InTransaction: Boolean;
begin
  //  Result := False;
  Result := FTransaction.Active;
  //  Result := (FTransaction.Handle <> NIL);
end;

procedure TtiDatabaseSQLDB.RollBack;
begin
  Log(ClassName + ': [RollBack Trans]', lsSQL);
  //  FTransaction.RollbackRetaining;
  FTransaction.RollBack;
end;

procedure TtiDatabaseSQLDB.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');

  Log(ClassName + ': [Start Trans]', lsSQL);
  FTransaction.StartTransaction;
end;

function TtiDatabaseSQLDB.GetConnected: Boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseSQLDB.SetConnected(AValue: Boolean);
var
  lMessage: string;
begin
  try
    if (not AValue) then
    begin
      {$ifdef LOGSQLDB}
      Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
      {$endif}
      FDatabase.Connected := False;
      Exit; //==>
    end;

    if tiNumToken(DatabaseName, ':') > 1 then
    begin
      // Assumes tiOPF's "databasehost:databasename" format.
      FDatabase.HostName      := tiToken(DatabaseName, ':', 1);
      { we can't use tiToken(x,y,2) because Windows paths contain a : after the drive letter }
      FDatabase.DatabaseName  := Copy(DatabaseName, Length(FDatabase.HostName)+2, Length(DatabaseName));
      //writeln('*************');
      //writeln('DEBUG:  TtiDatabaseSQLDB.SetConnected ', FDatabase.HostName);
      //writeln('DEBUG:  TtiDatabaseSQLDB.SetConnected ', FDatabase.DatabaseName);
      //writeln('-------------');
    end
    else
      FDatabase.DatabaseName := DatabaseName;

    FDatabase.Params.Assign(Params);
    FDatabase.UserName     := Username;
    FDatabase.Password     := Password;

    { Assign some well known extra parameters if they exist. }
    if Params.Values['ROLE'] <> '' then
      FDatabase.Role := Params.Values['ROLE'];
    { charset is a db neutral property we defined for tiOPF. }
    if Params.Values['CHARSET'] <> '' then
      FDatabase.CharSet := Params.Values['CHARSET'];
    { lc_ctype is native to Interface/Firebird databases. }
    if Params.Values['LC_CTYPE'] <> '' then
      FDatabase.CharSet := Params.Values['LC_CTYPE'];

    FDatabase.Connected    := True;
  except
    on e: EDatabaseError do
    begin
      lMessage := 'Error attempting to connect to database.' + tiLineEnd+ e.Message;
      raise EtiOPFDBExceptionUserNamePassword.Create('Sqldb', DatabaseName, UserName, Password, lMessage);
    end;

    on e: Exception do
    begin
      lMessage := 'Error attempting to connect to database.' + tiLineEnd + e.Message;
      raise EtiOPFDBException.Create('Sqldb', DatabaseName, UserName, Password, lMessage)
    end;
  end;
end;

class procedure TtiDatabaseSQLDB.CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string);
var
  DB: TSQLConnection;
begin
  if (ADatabaseName <> '') or (AUserName <> '') or (APassword <> '') then
  begin
    DB := CreateSQLConnection;
    try
      DB.DatabaseName := ADatabasename;
      DB.UserName := AUsername;
      DB.Password := APassword;
      DB.CreateDB;
    finally
      DB.Free;
    end;
  end;
end;

class procedure TtiDatabaseSQLDB.DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string);
var
  DB: TSQLConnection;
begin
  if (ADatabaseName <> '') or (AUserName <> '') or (APassword <> '') then
  begin
    DB := CreateSQLConnection;
    try
      DB.DatabaseName := ADatabasename;
      DB.UserName := AUsername;
      DB.Password := APassword;
      DB.DropDB;
    finally
      DB.Free;
    end;
  end;
end;

class function TtiDatabaseSQLDB.DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string): Boolean;
var
  DB: TSQLConnection;
begin
  Result := False;
  if (ADatabaseName <> '') or (AUserName <> '') or (APassword <> '') then
  begin
    DB := CreateSQLConnection;
    try
      DB.DatabaseName := ADatabaseName;
      DB.UserName := AUserName;
      DB.Password := APassword;
      try
        DB.Connected := True;
        Result := True;
      except
        on e: Exception do
          Result := False;
      end;
      DB.Connected := False;
    finally
      DB.Free;
    end;
  end;
end;

function TtiDatabaseSQLDB.Test: Boolean;
begin
  Result := Connected;
end;

function TtiDatabaseSQLDB.TIQueryClass: TtiQueryClass;
begin
  Result := TtiQuerySQLDB;
end;

function TtiDatabaseSQLDB.HasNativeLogicalType: Boolean;
begin
  Result := True;
end;

function TtiPersistenceLayerSqldDB.GetQueryClass: TtiQueryClass;
begin
  Result := TtiQuerySqldb;
end;


end.

