{
  This unit implements the base classes for the Free Pascal SQLDB persistence
  layer.

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldb;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  db,
  sqldb,
  tiQuery,
  tiObject,
  tiPersistenceLayers;

Type
  { TtiPersistenceLayerSqldDB }
  TtiPersistenceLayerSqldDB = class(TtiPersistenceLayer)
    function GetQueryClass: TtiQueryClass; override;
  end;

  { TtiDatabaseSQLDB }

  TtiDatabaseSQLDB = class(TtiDatabaseSQL)
  private
    FDatabase: TSQLConnection;
    FTransaction: TSQLTransaction;
  protected
    procedure   SetConnected(AValue: boolean); override;
    function    GetConnected: boolean; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
    Class Function CreateSQLConnection : TSQLConnection; virtual; abstract;
    function    HasNativeLogicalType: boolean; virtual;
  public
    constructor Create; override;
    destructor  Destroy; override;
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword: string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string); override;
    property    SQLConnection: TSQLConnection Read FDatabase Write FDatabase;
    procedure   StartTransaction; override;
    function    InTransaction: boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    procedure   ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    function    Test: boolean; override;
    function    TIQueryClass: TtiQueryClass; override;
  end;

  { TtiQuerySQLDB }

  TtiQuerySQLDB = class(TtiQuerySQL)
  private
    FIBSQL: TSQLQuery;
    FbActive: boolean;
    function  IBFieldKindToTIFieldKind(pDataType: TFieldType): TtiQueryFieldKind;
    Procedure CheckPrepared;
    procedure Prepare;
  protected
    function  GetFieldAsString(const AName: string): string; override;
    function  GetFieldAsFloat(const AName: string): extended; override;
    function  GetFieldAsBoolean(const AName: string): boolean; override;
    function  GetFieldAsInteger(const AName: string): int64; override;
    function  GetFieldAsDateTime(const AName: string): TDateTime; override;

    function  GetFieldAsStringByIndex(AIndex: integer): string; override;
    function  GetFieldAsFloatByIndex(AIndex: integer): extended; override;
    function  GetFieldAsBooleanByIndex(AIndex: integer): boolean; override;
    function  GetFieldAsIntegerByIndex(AIndex: integer): int64; override;
    function  GetFieldAsDateTimeByIndex(AIndex: integer): TDateTime; override;
    function  GetFieldIsNullByIndex(AIndex: integer): boolean; override;

    function  GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    function  GetActive: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    function  GetEOF: boolean; override;
    function  GetParamAsString(const AName: string): string; override;
    function  GetParamAsBoolean(const AName: string): boolean; override;
    function  GetParamAsFloat(const AName: string): extended; override;
    function  GetParamAsInteger(const AName: string): int64; override;
    function  GetParamAsDateTime(const AName: string): TDateTime; override;
    function  GetParamAsTextBLOB(const AName: string): string; override;
    function  GetParamIsNull(const AName: string): Boolean; override;
    procedure SetParamAsString(const AName, AValue: string); override;
    procedure SetParamAsBoolean(const AName: string; const AValue: boolean); override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended); override;
    procedure SetParamAsInteger(const AName: string; const AValue: int64); override;
    procedure SetParamAsDateTime(const AName: string; const AValue: TDateTime); override;
    procedure SetParamAsTextBLOB(const AName, AValue: string); override;
    procedure SetParamIsNull(const AName: string; const AValue: Boolean); override;
    function  GetFieldIsNull(const AName: string): boolean; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open; override;
    procedure   Close; override;
    procedure   Next; override;
    procedure   ExecSQL; override;

    function    ParamCount: integer; override;
    function    ParamName(AIndex: integer): string; override;

    procedure   AssignParamFromStream(const AName: string; const AStream: TStream); override;
    procedure   AssignParamToStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStream(const AName: string; const AStream: TStream); override;
    procedure   AssignFieldAsStreamByIndex(AIndex: integer; const AValue: TStream); override;
    procedure   AssignParams(const AParams: TtiQueryParams; const AWhere: TtiQueryParams = nil); override;

    procedure   AttachDatabase(ADatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

    function    FieldCount: integer; override;
    function    FieldName(AIndex: integer): string; override;
    function    FieldIndex(const AName: string): integer; override;
    function    FieldKind(AIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex: integer): integer; override;
    function    HasNativeLogicalType: boolean; override;
  end;


implementation

{$define LOGSQLDB}
uses
  tiUtils
{$ifdef LOGSQLDB}
  ,tiLog
{$endif}
  ,TypInfo
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,Variants
 ;

{ TtiQuerySQLDB }

constructor TtiQuerySQLDB.Create;
begin
  inherited;
  FIBSQL := TSQLQuery.Create(nil);
end;

destructor TtiQuerySQLDB.Destroy;
begin
  FIBSQL.Free;
  inherited;
end;

procedure TtiQuerySQLDB.Close;
begin
{$ifdef LOGSQLDB}Log('>>> TtiQuerySQLDB.Close');{$endif}
  Active := False;
{$ifdef LOGSQLDB}Log('<<< TtiQuerySQLDB.Close');{$endif}
end;

procedure TtiQuerySQLDB.ExecSQL;
begin
{$ifdef LOGSQLDB}Log('>>> TtiQuerySQLDB.ExecSQL: '+FIBSQL.SQL.Text);{$endif}
  Prepare;
  FIBSQL.ExecSQL;
{$ifdef LOGSQLDB}Log('<<< TtiQuerySQLDB.ExecSQL');{$endif}
end;

function TtiQuerySQLDB.GetFieldAsBoolean(const AName: string): boolean;
var
  lsValue: string;
begin
  lsValue := Trim(upperCase(FIBSQL.FieldByName(UpperCase(AName)).AsString));
  Result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;

function TtiQuerySQLDB.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).AsDateTime;
end;

function TtiQuerySQLDB.GetFieldAsFloat(const AName: string): extended;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).AsFloat;
end;

function TtiQuerySQLDB.GetFieldAsInteger(const AName: string): int64;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).AsInteger;
end;

function TtiQuerySQLDB.GetFieldAsString(const AName: string): string;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).AsString;
end;

function TtiQuerySQLDB.GetFieldAsStringByIndex(AIndex: integer): string;
begin
  Result := FIBSQL.Fields[AIndex].AsString;
end;

function TtiQuerySQLDB.GetFieldAsFloatByIndex(AIndex: integer): extended;
begin
  Result := FIBSQL.Fields[AIndex].AsFloat;
end;

function TtiQuerySQLDB.GetFieldAsBooleanByIndex(AIndex: integer): boolean;
var
  lsValue: string;
begin
  lsValue := Trim(FIBSQL.Fields[AIndex].AsString);
  Result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;

function TtiQuerySQLDB.GetFieldAsIntegerByIndex(AIndex: integer): int64;
begin
  Result := FIBSQL.Fields[AIndex].AsInteger;
end;

function TtiQuerySQLDB.GetFieldAsDateTimeByIndex(AIndex: integer): TDateTime;
begin
  Result := FIBSQL.Fields[AIndex].AsDateTime;
end;

function TtiQuerySQLDB.GetFieldIsNullByIndex(AIndex: integer): boolean;
begin
  Result := FIBSQL.Fields[AIndex].IsNull;
end;

function TtiQuerySQLDB.GetActive: boolean;
begin
{$ifdef LOGSQLDB}Log('>>> TtiQuerySQLDB.GetActive');{$endif}
  Result := FIBSQL.Active; //FbActive;
{$ifdef LOGSQLDB}Log('<<< TtiQuerySQLDB.GetActive');{$endif}
end;

function TtiQuerySQLDB.GetEOF: boolean;
begin
{$ifdef LOGSQLDB}Log('>>> TtiQuerySQLDB.GetEOF');{$endif}
  Result := FIBSQL.EOF;
{$ifdef LOGSQLDB}Log('<<< TtiQuerySQLDB.GetEOF');{$endif}
end;

function TtiQuerySQLDB.GetParamAsBoolean(const AName: string): boolean;
var
  lValue: string;
begin
  lValue := FIBSQL.Params.ParamByName(UpperCase(AName)).AsString;
{$IFDEF BOOLEAN_CHAR_1}
  Result := SameText(lValue, 'T');
{$ELSE}
  Result := SameText(lValue, 'TRUE');
{$ENDIF}// BOOLEAN_CHAR_1
end;

function TtiQuerySQLDB.GetParamAsDateTime(const AName: string): TDateTime;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsDateTime;
end;

function TtiQuerySQLDB.GetParamAsTextBLOB(const AName: string): string;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsString;
end;

function TtiQuerySQLDB.GetParamAsFloat(const AName: string): extended;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsFloat;
end;

function TtiQuerySQLDB.GetParamAsInteger(const AName: string): int64;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsInteger;
end;

function TtiQuerySQLDB.GetParamAsString(const AName: string): string;
begin
{$ifdef LOGSQLDB}Log('>>> TtiQuerySQLDB.GetParamAsString ('+AName+')');{$endif}
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsString;
{$ifdef LOGSQLDB}Log('<<< TtiQuerySQLDB.GetParamAsString ('+Aname+')');{$endif}
end;

function TtiQuerySQLDB.GetSQL: TStrings;
begin
  Result := FIBSQL.SQL;
end;

procedure TtiQuerySQLDB.Next;
begin
  FIBSQL.Next;
end;

procedure TtiQuerySQLDB.Open;
begin
{$ifdef LOGSQLDB}
  Log('>>> Open');
  log('ParamCount: ' + IntToStr(ParamCount));
{$endif}
  CheckPrepared;
  FIBSQL.Open;
{$ifdef LOGSQLDB}Log('<<< Open');{$endif}
end;

function TtiQuerySQLDB.ParamCount: integer;
begin
  Result := FIBSQL.Params.Count;
end;

function TtiQuerySQLDB.ParamName(AIndex: integer): string;
begin
  Result := FIBSQL.Params[AIndex].Name;
end;

procedure TtiQuerySQLDB.SetActive(const AValue: boolean);
begin
{$ifdef LOGSQLDB}log('>>> TtiQuerySQLDB.SetActive');{$endif}
  Assert(Database.TestValid(TtiDatabase), CTIErrorInvalidObject);
  if AValue then
  begin
    {$ifdef LOGSQLDB}Log('Exec Query');{$endif}
    FIBSQL.ExecSQL;
    FbActive := True;
  end
  else
  begin
    {$ifdef LOGSQLDB}Log('Closing Query');{$endif}
    FIBSQL.Close;
    FbActive := False;
  end;
{$ifdef LOGSQLDB}log('<<< TtiQuerySQLDB.SetActive');{$endif}
end;

procedure TtiQuerySQLDB.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  CheckPrepared;
{$IFDEF BOOLEAN_CHAR_1}
  if AValue then
    FIBSQL.Params.ParamByName(UpperCase(AName)).AsString := 'T'
  else
    FIBSQL.Params.ParamByName(UpperCase(AName)).AsString := 'F';
{$ELSE}
  if AValue then
    FIBSQL.Params.ParamByName(UpperCase(AName)).AsString := 'TRUE'
  else
    FIBSQL.Params.ParamByName(UpperCase(AName)).AsString := 'FALSE';
{$ENDIF}// BOOLEAN_CHAR_1
end;

procedure TtiQuerySQLDB.SetParamAsDateTime(const AName: string;
  const AValue: TDateTime);
begin
  CheckPrepared;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsDateTime := AValue;
end;

procedure TtiQuerySQLDB.SetParamAsTextBLOB(const AName, AValue: string);
begin
{$ifdef LOGSQLDB}log('>>> TtiQuerySQLDB.SetParamAsTextBLOB');{$endif}
  CheckPrepared;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsString := AValue;
{$ifdef LOGSQLDB}log('<<< TtiQuerySQLDB.SetParamAsTextBLOB');{$endif}
end;

procedure TtiQuerySQLDB.SetParamAsFloat(const AName: string; const AValue: extended);
begin
{$ifdef LOGSQLDB}log('>>> TtiQuerySQLDB.SetParamAsFloat');{$endif}
  CheckPrepared;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsFloat := AValue;
{$ifdef LOGSQLDB}log('<<< TtiQuerySQLDB.SetParamAsFloat');{$endif}
end;

procedure TtiQuerySQLDB.SetParamAsInteger(const AName: string; const AValue: int64);
begin
{$ifdef LOGSQLDB}log('>>> TtiQuerySQLDB.SetParamAsInteger');{$endif}
  CheckPrepared;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsInteger := AValue;
{$ifdef LOGSQLDB}log('<<< TtiQuerySQLDB.SetParamAsInteger');{$endif}
end;

procedure TtiQuerySQLDB.SetParamAsString(const AName, AValue: string);
begin
{$ifdef LOGSQLDB}log('>>> TtiQuerySQLDB.SetParamAsString');{$endif}
  CheckPrepared;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsString := AValue;
{$ifdef LOGSQLDB}log('<<< TtiQuerySQLDB.SetParamAsString');{$endif}
end;

procedure TtiQuerySQLDB.SetSQL(const AValue: TStrings);
begin
{$ifdef LOGSQLDB}log('>>>> SetSQL: '+AValue.Text);{$endif}
  FIBSQL.SQL.Assign(AValue);
{$ifdef LOGSQLDB}log('<<<< SetSQL');{$endif}
end;

procedure TtiQuerySQLDB.CheckPrepared;

begin
{$ifdef LOGSQLDB}Log('>>> TtiQuerySQLDB.CheckPrepared');{$endif}
  If Not FIBSQL.Prepared then
    Prepare;
{$ifdef LOGSQLDB}Log('<<< TtiQuerySQLDB.CheckPrepared');{$endif}
end;

procedure TtiQuerySQLDB.Prepare;
begin
{$ifdef LOGSQLDB}Log('>>> TtiQuerySQLDB.Prepare');{$endif}
  if FIBSQL.Prepared then
    Exit; //==>
  FIBSQL.Prepare;
{$ifdef LOGSQLDB}Log('<<< TtiQuerySQLDB.Prepare');{$endif}
end;

procedure TtiQuerySQLDB.AssignParamFromStream(const AName: string;
  const AStream: TStream);
begin
  Assert(AStream <> NIL, 'Stream not assigned');
  AStream.Position := 0;
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(AName)).LoadFromStream(AStream, ftBlob);
end;

procedure TtiQuerySQLDB.AssignParamToStream(const AName: string;
  const AStream: TStream);
var
  lBinData:          olevariant;
  lDataPtr:          Pointer;
  lHigh, lLow, lLen: integer;
  lParameter:        TParam;
begin
  Assert(AStream <> NIL, 'Stream not assigned');
  lParameter := FIBSQL.Params.ParamByName(UpperCase(AName));
  lLow      := VarArrayLowBound(lParameter.Value, 1);
  lHigh     := VarArrayHighBound(lParameter.Value, 1);
  lLen      := lHigh - lLow + 1;
  lBinData  := VarArrayCreate([0, lLen], varByte);
  lBinData  := lParameter.Value;
  lDataPtr  := VarArrayLock(lBinData);
  try
    AStream.WriteBuffer(lDataPtr^, lLen);
  finally
    VarArrayUnlock(lBinData);
  end;

{$Note  Please try this option of saving to a Stream as well }
{
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  (FIBSQL.Params.ParamByName(UpperCase(AName)) as TBlobField).SaveToStream(AStream);
  AStream.Position := 0;
}
end;

procedure TtiQuerySQLDB.AssignFieldAsStream(const AName: string;
  const AStream: TStream);
begin
  {$Note Look at AssignParamToStream if this doesn't work}
  Assert(AStream <> NIL, 'Stream not assigned');
  AStream.Position := 0;
  (FIBSQL.FieldByName(UpperCase(AName)) as TBlobField).SaveToStream(AStream);
end;

procedure TtiQuerySQLDB.AssignFieldAsStreamByIndex(AIndex: integer;
  const AValue: TStream);
begin
  Assert(AValue <> NIL, 'Stream not assigned');
  AValue.Position := 0;
  TBlobField(FIBSQL.Fields[AIndex]).SaveToStream(AValue);
end;


procedure TtiQuerySQLDB.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  if (Database is TtiDatabaseSQLDB) then
  begin
    FIBSQL.Database := TtiDatabaseSQLDB(Database).SQLConnection;
    FIBSQL.Transaction := TtiDatabaseSQLDB(Database).SQLConnection.Transaction;
  end;
end;

procedure TtiQuerySQLDB.DetachDatabase;
begin
  inherited DetachDatabase;
  if FIBSQL.Active then
    FIBSQL.Close;
  FIBSQL.Transaction := nil;
  FIBSQL.Database   := nil;
end;

function TtiQuerySQLDB.FieldCount: integer;
begin
  Result := FIBSQL.FieldCount;
end;

function TtiQuerySQLDB.FieldName(AIndex: integer): string;
begin
  Result := FIBSQL.Fields[AIndex].Name;
end;

procedure TtiQuerySQLDB.Reset;
begin
  Active := False;
  FIBSQL.SQL.Clear;
end;

function TtiQuerySQLDB.FieldIndex(const AName: string): integer;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).Index;
end;

// -----------------------------------------------------------------------------
// This code is cloned in TtiQueryBDEAbs - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
// -----------------------------------------------------------------------------
function TtiQuerySQLDB.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lDataType: TFieldType;
begin
  lDataType := FIBSQL.Fields[AIndex].DataType;

{ These are the available field types for a TDataSet descendant
  TFieldType = (ftUnknown, ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
    ftBytes, ftVarBytes, ftAutoInc, ftBlob, ftMemo, ftGraphic, ftFmtMemo,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
    ftWideString, ftLargeint, ftADT, ftArray, ftReference,
    ftDataSet, ftOraBlob, ftOraClob, ftVariant, ftInterface,
    ftIDispatch, ftGuid, ftTimeStamp, ftFMTBcd);
}
  // These are the available TtiQueryFieldKind(s)
  //  ,
  //  qfkInteger,
  //  qfkFloat,
  //  qfkDateTime,
  //  qfkLogical,
  //  qfkBinary,
  //  qfkMacro,
  //  qfkLongString

  case lDataType of
    ftString, ftWideString: Result := qfkString;
    ftSmallint, ftInteger, ftWord, ftLargeint: Result := qfkInteger;
    ftBoolean: Result        := qfkLogical;
    ftFloat, ftCurrency, ftBCD: Result := qfkFloat;
    ftDate, ftTime, ftDateTime: Result := qfkDateTime;
    ftBlob, ftGraphic, ftVarBytes: Result := qfkBinary;
    ftMemo, ftFmtMemo: Result := qfkLongString;
    else
      raise Exception.Create('Invalid FIBSQL.Fields[ AIndex ].DataType <' +
        GetEnumName(TypeInfo(TFieldType), Ord(lDataType)));
  end;
  //    ftUnknown,
  //    ftBytes, ftVarBytes, ftAutoInc,
  //    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftFixedChar,
  //    ftADT, ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob,
  //    ftVariant, ftInterface, ftIDispatch, ftGuid
end;

{ -----------------------------------------------------------------------------
 This code is cloned in TtiQueryBDEAbs - Looks like we need to abstract more
 and introduce a TDataSet version of the TtiQuery
 -----------------------------------------------------------------------------}
function TtiQuerySQLDB.IBFieldKindToTIFieldKind(pDataType: TFieldType): TtiQueryFieldKind;
var
  s: string;
begin
  case pDataType of
    ftString,
    ftWideString:     Result := qfkString;

    ftSmallint,
    ftInteger,
    ftWord,
    ftLargeint:       Result := qfkInteger;

    ftBoolean:        Result := qfkLogical;

    ftFloat,
    ftCurrency,
    ftBCD:            Result := qfkFloat;

    ftDate,
    ftTime,
    ftDateTime:       Result := qfkDateTime;

    ftBlob,
    ftGraphic,
    ftVarBytes:       Result := qfkBinary;

    ftMemo,
    ftFmtMemo:        Result := qfkLongString;
  else
    begin
    s := GetEnumName(TypeInfo(TFieldType), Ord(pDataType));
    raise Exception.Create('Invalid FIBSQL.Fields[ AIndex ].DataType <' +
       s +
//      GetEnumName(TypeInfo(TFieldType), Ord(pDataType)) +
      '>');
    end;
  end;
end;

function TtiQuerySQLDB.FieldSize(AIndex: integer): integer;
begin
  if FieldKind(AIndex) in [qfkInteger, qfkFloat, qfkDateTime,
    qfkLogical, qfkLongString] then
    Result := 0
  else
    Result := FIBSQL.Fields[AIndex].Size;
end;

function TtiQuerySQLDB.GetParamIsNull(const AName: string): boolean;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).IsNull;
end;

procedure TtiQuerySQLDB.SetParamIsNull(const AName: string; const AValue: boolean);
begin
  if AValue then
  begin
    Prepare;
    FIBSQL.Params.ParamByName(UpperCase(AName)).Value := Null;
  end;
end;

function TtiQuerySQLDB.GetFieldIsNull(const AName: string): boolean;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).IsNull;
end;

procedure TtiQuerySQLDB.AssignParams(const AParams: TtiQueryParams; const AWhere: TtiQueryParams = nil);
begin
  if AParams = NIL then
    Exit;
  Prepare;
  inherited;
end;

function TtiQuerySQLDB.HasNativeLogicalType: boolean;
begin
  If not assigned(Database) then
    Result:=True
  else
    Result:=TtiDatabaseSQLDB(Database).HasNativeLogicalType;
end;

{ TtiDatabaseSQLDB }

constructor TtiDatabaseSQLDB.Create;
begin
  inherited Create;
  FDatabase              := CreateSQLConnection;
  FDatabase.LoginPrompt  := False;
  FTransaction         := TSQLTransaction.Create(nil);
  FTransaction.DataBase := FDatabase;
  FDatabase.Transaction  := FTransaction;
  FTransaction.Active := False;
end;

destructor TtiDatabaseSQLDB.Destroy;
begin
  try
    FTransaction.Active  := False;
    FDatabase.Connected    := False;
    FDatabase.Transaction  := nil;
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

function TtiDatabaseSQLDB.InTransaction: boolean;
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
{$ifdef LOGSQLDB}Log('>>>> Start transaction...');{$endif}
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');

  FTransaction.StartTransaction;
{$ifdef LOGSQLDB}Log('<<<< Start transaction...');{$endif}
end;

function TtiDatabaseSQLDB.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseSQLDB.SetConnected(AValue: boolean);
var
  lMessage: string;
begin
  try
    if (not AValue) then
    begin
      {$ifdef LOGSQLDB}Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);{$endif}
      FDatabase.Connected := False;
      Exit; //==>
    end;

    FDatabase.DatabaseName := DatabaseName;
    FDatabase.UserName := Username;
    FDatabase.Password := Password;
    FDatabase.Params.Values['user_name'] := UserName;
    FDatabase.Params.Values['password']  := Password;
    FDatabase.Connected := True;
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
  lQuery:    TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable:    TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery   := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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
        lTable     := TtiDBMetaDataTable.Create;
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
  lTableName:   string;
  lQuery:       TtiQuery;
  lTable:       TtiDBMetaDataTable;
  lField:       TtiDBMetaDataField;
  lFieldType:   integer;
  lFieldLength: integer;
const
  cIBField_LONG      = 8;
  cIBField_DOUBLE    = 27;
  cIBField_TIMESTAMP = 35;
  cIBField_DATE      = 12;
  cIBField_TIME      = 13;
  cIBField_VARYING   = 37;
  cIBField_BLOB      = 261;

  cIBField_SHORT     = 7;
  cIBField_QUAD      = 9;
  cIBField_FLOAT     = 10;
  cIBField_TEXT      = 14;
  cIBField_CSTRING   = 40;
  cIBField_BLOB_ID   = 45;
begin
  lTable    := (AData as TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery    := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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
        lField      := TtiDBMetaDataField.Create;
        lField.Name := Trim(lQuery.FieldAsString['field_name']);
        lFieldType  := lQuery.FieldAsInteger['field_type'];
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
            lField.Kind := qfkString;
            lField.Width := lFieldLength;
          end;
          cIBField_BLOB:
          begin
            Assert(
              not lQuery.FieldIsNull['field_sub_type'], 'field_sub_type is null');
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

function TtiDatabaseSQLDB.FieldMetaDataToSQLCreate(
  const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := AFieldMetaData.Name;
  case AFieldMetaData.Kind of
    qfkString: Result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger: Result := 'Integer';
    //    qfkFloat: result := 'Decimal(10, 5)';
    qfkFloat: Result  := 'DOUBLE PRECISION';
    // Just for new version of IB (6.x)
    // DATE holds only DATE without TIME...
    qfkDateTime:  Result := 'TIMESTAMP';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical: Result := 'Char(1) default ''F'' check(' +
        lFieldName + ' in (''T'', ''F''))';
    {$ELSE}
    qfkLogical: Result := 'VarChar(5) default ''FALSE'' check(' +
        lFieldName + ' in (''TRUE'', ''FALSE'')) ';
    {$ENDIF}
    qfkBinary: Result := 'Blob sub_type 0';
    qfkLongString: Result := 'Blob sub_type 1';
    else
      raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

class procedure TtiDatabaseSQLDB.CreateDatabase(
  const ADatabaseName, AUserName, APassword: string);
var
  DB : TSQLConnection;
begin
  if (ADatabaseName <> '') or (AUserName <> '') or (APassword <> '') then
    begin
    DB:=CreateSQLConnection;
    try
      with DB do
        begin
        DatabaseName:=aDatabasename;
        UserName:=AUsername;
        Password:=APassword;
        CreateDB;
        end;
    finally
      DB.Free;
    end;
    end;
end;

class function TtiDatabaseSQLDB.DatabaseExists(
  const ADatabaseName, AUserName, APassword: string): boolean;
var
  DB : TSQLConnection;
begin
  Result:=False;
  if (ADatabaseName <> '') or (AUserName <> '') or (APassword <> '') then
    begin
    DB:=CreateSQLConnection;
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

function TtiDatabaseSQLDB.Test: boolean;
begin
  Result := False;
  Assert(False, 'Under construction');
end;

function TtiDatabaseSQLDB.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQuerySQLDB;
end;

function TtiDatabaseSQLDB.HasNativeLogicalType: boolean;
begin
  Result:=True;
end;

function TtiPersistenceLayerSqldDB.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQuerySqldb;
end;

end.

