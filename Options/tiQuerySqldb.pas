{
  This unit implements the base classes for the Free Pascal SQLDB persistence
  layer.

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldb;

{$mode objfpc}{$H+}

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
  tiObject,
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
    function FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
    class function CreateSQLConnection: TSQLConnection; virtual; abstract;
    function HasNativeLogicalType: Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: string): Boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string); override;
    property SQLConnection: TSQLConnection read FDatabase write FDatabase;
    procedure StartTransaction; override;
    function InTransaction: Boolean; override;
    procedure Commit; override;
    procedure RollBack; override;
    procedure ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    function Test: Boolean; override;
    function TIQueryClass: TtiQueryClass; override;
  end;


  TtiQuerySQLDB = class(TtiQueryDataset)
  private
    FIBSQL: TSQLQuery;
    FbActive: Boolean;
    procedure Prepare;
  protected
    procedure CheckPrepared; override;
    procedure SetActive(const AValue: Boolean); override;
    function GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ExecSQL; override;
    procedure AttachDatabase(ADatabase: TtiDatabase); override;
    procedure DetachDatabase; override;
    procedure Reset; override;
    function HasNativeLogicalType: Boolean; override;
  end;


implementation

uses
  tiUtils,
{$ifdef LOGSQLDB}
  tiLog,
{$endif}
  TypInfo,
  tiOPFManager,
  tiConstants,
  tiExcept,
  Variants;


{ TtiQuerySQLDB }

constructor TtiQuerySQLDB.Create;
begin
  inherited;
  FIBSQL  := TSQLQuery.Create(nil);
  Dataset := FIBSQL;
  Params  := FIBSQL.Params;
end;

destructor TtiQuerySQLDB.Destroy;
begin
  Params  := nil;
  Dataset := nil;
  FIBSQL.Free;
  inherited;
end;

procedure TtiQuerySQLDB.ExecSQL;
begin
{$ifdef LOGSQLDB}
  Log('>>> TtiQuerySQLDB.ExecSQL: ' + FIBSQL.SQL.Text);
{$endif}
  Prepare;
  FIBSQL.ExecSQL;
{$ifdef LOGSQLDB}
  Log('<<< TtiQuerySQLDB.ExecSQL');
{$endif}
end;

function TtiQuerySQLDB.GetSQL: TStrings;
begin
  Result := FIBSQL.SQL;
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
    FIBSQL.Open;
    FbActive := True;
  end
  else
  begin
    {$ifdef LOGSQLDB}
    Log('Closing Query');
    {$endif}
    FIBSQL.Close;
    FbActive := False;
  end;
{$ifdef LOGSQLDB}
  log('<<< TtiQuerySQLDB.SetActive');
{$endif}
end;

procedure TtiQuerySQLDB.SetSQL(const AValue: TStrings);
begin
{$ifdef LOGSQLDB}
  log('>>>> SetSQL: ' + AValue.Text);
{$endif}
  FIBSQL.SQL.Assign(AValue);
{$ifdef LOGSQLDB}
  log('<<<< SetSQL');
{$endif}
end;

procedure TtiQuerySQLDB.CheckPrepared;
begin
{$ifdef LOGSQLDB}
  Log('>>> TtiQuerySQLDB.CheckPrepared');
{$endif}
  if not FIBSQL.Prepared then
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
  if FIBSQL.Prepared then
    Exit; //==>
  FIBSQL.Prepare;
{$ifdef LOGSQLDB}
  Log('<<< TtiQuerySQLDB.Prepare');
{$endif}
end;

procedure TtiQuerySQLDB.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  if (Database is TtiDatabaseSQLDB) then
  begin
    FIBSQL.Database    := TtiDatabaseSQLDB(Database).SQLConnection;
    FIBSQL.Transaction := TtiDatabaseSQLDB(Database).SQLConnection.Transaction;
  end;
end;

procedure TtiQuerySQLDB.DetachDatabase;
begin
  inherited DetachDatabase;
  if FIBSQL.Active then
    FIBSQL.Close;
  FIBSQL.Transaction := nil;
  FIBSQL.Database    := nil;
end;

procedure TtiQuerySQLDB.Reset;
begin
  Active := False;
  FIBSQL.SQL.Clear;
end;

function TtiQuerySQLDB.HasNativeLogicalType: Boolean;
begin
  if not assigned(Database) then
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
  //  FTransaction.RollbackRetaining;
  FTransaction.RollBack;
end;

procedure TtiDatabaseSQLDB.StartTransaction;
begin
{$ifdef LOGSQLDB}
  Log('>>>> Start transaction...');
{$endif}
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');

  FTransaction.StartTransaction;
{$ifdef LOGSQLDB}
  Log('<<<< Start transaction...');
{$endif}
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

    FDatabase.DatabaseName := DatabaseName;
    FDatabase.UserName     := Username;
    FDatabase.Password     := Password;
    FDatabase.Params.Values['user_name'] := UserName;
    FDatabase.Params.Values['password'] := Password;
    FDatabase.Connected    := True;
  except
    // ToDo: Must come up with a better solution that this:
    //       Try several times before raising an exception.
    //       Rather than calling 'Halt', just terminate this database connection,
    //       unless this is the first connection.
    on e: EDatabaseError do
    begin
      // Invalid username / password error
      //      if (EIBError(E).IBErrorCode = 335544472) then
      //        raise EtiOPFDBExceptionUserNamePassword.Create(cTIPersistIBX, DatabaseName, UserName, Password)
      //      else
      //      begin
      lMessage :=
        'Error attempting to connect to database.' + Cr + e.Message;
      raise EtiOPFDBExceptionUserNamePassword.Create(
        cTIPersistSqldbIB, DatabaseName, UserName, Password, lMessage);
      //      end;
    end
    else
      raise EtiOPFDBException.Create(cTIPersistSqldbIB, DatabaseName, UserName, Password)
  end;
end;

procedure TtiDatabaseSQLDB.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lQuery: TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery    := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      { SQL Views are now also included }
      lQuery.SQLText :=
        'SELECT RDB$RELATION_NAME as Table_Name ' +
        '  FROM RDB$RELATIONS ' +
        'WHERE ((RDB$SYSTEM_FLAG = 0) OR (RDB$SYSTEM_FLAG IS NULL)) ' +
//        '  AND (RDB$VIEW_SOURCE IS NULL) ' +
        'ORDER BY RDB$RELATION_NAME ';
      lQuery.Open;
      while not lQuery.EOF do
      begin
        lTable      := TtiDBMetaDataTable.Create;
        lTable.Name := Trim(lQuery.FieldAsString['table_name']);
        lTable.ObjectState := posPK;
        lMetaData.Add(lTable);
        lQuery.Next;
      end;
      lQuery.DetachDatabase;
      lMetaData.ObjectState := posClean;
    finally
      Commit;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseSQLDB.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTableName: string;
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType: integer;
  lFieldLength: integer;
const
  cIBField_LONG      = 8;
  cIBField_DOUBLE    = 27;
  cIBField_TIMESTAMP = 35;
  cIBField_DATE      = 12;
  cIBField_TIME      = 13;
  cIBField_VARYING   = 37;
  cIBField_BLOB      = 261;
  cIBField_TEXT      = 14;
  {  cIBField_SHORT     = 7;
  cIBField_QUAD      = 9;
  cIBField_FLOAT     = 10;
  cIBField_CSTRING   = 40;
  cIBField_BLOB_ID   = 45;}
begin
  lTable     := (AData as TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery     := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      lQuery.SQLText :=
        '  select ' +
        '    r.rdb$field_name     as field_name ' +
        '    ,rdb$field_type      as field_type ' +
        '    ,rdb$field_sub_type  as field_sub_type ' +
        '    ,rdb$field_length    as field_length ' +
        '  from ' +
        '    rdb$relation_fields r ' +
        '    ,rdb$fields f ' +
        '  where ' +
        '      r.rdb$relation_name = ''' + lTableName + '''' +
        '  and f.rdb$field_name = r.rdb$field_source ';

      lQuery.Open;
      while not lQuery.EOF do
      begin
        lField       := TtiDBMetaDataField.Create;
        lField.Name  := Trim(lQuery.FieldAsString['field_name']);
        lFieldType   := lQuery.FieldAsInteger['field_type'];
        lFieldLength := lQuery.FieldAsInteger['field_length'];

        lField.Width := 0;

        case lFieldType of
          cIBField_LONG: lField.Kind := qfkInteger;
          cIBField_DOUBLE: lField.Kind := qfkFloat;
          cIBField_TIMESTAMP,
          cIBField_DATE,
          cIBField_TIME: lField.Kind := qfkDateTime;
          cIBField_VARYING,
          cIBField_TEXT:
          begin
            lField.Kind  := qfkString;
            lField.Width := lFieldLength;
          end;
          cIBField_BLOB:
          begin
            Assert(not lQuery.FieldIsNull['field_sub_type'], 'field_sub_type is null');
            if lQuery.FieldAsInteger['field_sub_type'] = 1 then
              lField.Kind := qfkLongString
            else
              raise EtiOPFInternalException.Create(
                'Invalid field_sub_type <' + IntToStr(lQuery.FieldAsInteger['field_sub_type']) + '>');
          end;
          else
            raise EtiOPFInternalException.Create(
              'Invalid Interbase FieldType <' + IntToStr(lFieldType) + '>');
        end;
        lField.ObjectState := posClean;
        lTable.Add(lField);
        lQuery.Next;
      end;
    finally
      Commit;
    end;
    lQuery.DetachDatabase;
    lTable.ObjectState := posClean;
  finally
    lQuery.Free;
  end;
end;

function TtiDatabaseSQLDB.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := AFieldMetaData.Name;
  case AFieldMetaData.Kind of
    qfkString: Result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger: Result    := 'Integer';
    //    qfkFloat: result := 'Decimal(10, 5)';
    qfkFloat: Result      := 'DOUBLE PRECISION';
    // Just for new version of IB (6.x)
    // DATE holds only DATE without TIME...
    qfkDateTime: Result   := 'TIMESTAMP';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical: Result    := 'Char(1) default ''F'' check(' +
        lFieldName + ' in (''T'', ''F''))';
    {$ELSE}
    qfkLogical: Result    := 'VarChar(5) default ''FALSE'' check(' +
        lFieldName + ' in (''TRUE'', ''FALSE'')) ';
    {$ENDIF}
    qfkBinary: Result     := 'Blob sub_type 0';
    qfkLongString: Result := 'Blob sub_type 1';
    else
      raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

class procedure TtiDatabaseSQLDB.CreateDatabase(const ADatabaseName, AUserName, APassword: string);
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

class procedure TtiDatabaseSQLDB.DropDatabase(const ADatabaseName, AUserName, APassword: string);
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

class function TtiDatabaseSQLDB.DatabaseExists(const ADatabaseName, AUserName, APassword: string): Boolean;
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
  Result := False;
  Assert(False, 'Under construction');
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

