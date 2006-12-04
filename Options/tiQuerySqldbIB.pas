{
  This persistence layer uses standard Free Pascal sqlDB (interbase) components.

  The connection string format is the same as the standard Interbase/Firebird
  persistence layers.

  eg:
    gTIOPFManager.ConnectDatabase('192.168.0.20:E:\Databases\Test.fdb',
        'sysdba', 'masterkey', '');

  TODO:
    * Refactor this so the other database drivers can also be used. sqlDB
      works very similar to the dbExpress drivers from Delphi.
    * BUG: I still get PrepareStatement errors at random.

  Initial Author:  Graeme Geldenhuys (graemeg@gmail.com) - Feb 2006
}

unit tiQuerySqldbIB;

{$I tiDefines.inc}

interface

uses
  Classes
  ,db
  ,sqldb
  ,IBConnection
  ,tiDBConnectionPool
  ,tiObject
  ,tiQuery
 ;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseSqldIB = class(TtiDatabaseSQL)
  private
    FDatabase: TIBConnection;
    FIBTransaction: TSQLTransaction;
  protected
    procedure   SetConnected(AValue: boolean); override;
    function    GetConnected: boolean; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string); override;
    property    IBDatabase: TIBConnection Read FDatabase Write FDatabase;
    procedure   StartTransaction; override;
    function    InTransaction: boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    procedure   ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    function    Test: boolean; override;
  end;

  // ---------------------------------------------------------------------------
  TtiQuerySqldbIB = class(TtiQuerySQL)
  private
    FIBSQL: TSQLQuery;
    FbActive: boolean;
    function    IBFieldKindToTIFieldKind(pDataType: TFieldType): TtiQueryFieldKind;
    procedure   Prepare;
  protected
    function    GetFieldAsString(const AName: string): string; override;
    function    GetFieldAsFloat(const AName: string): extended; override;
    function    GetFieldAsBoolean(const AName: string): boolean; override;
    function    GetFieldAsInteger(const AName: string): int64; override;
    function    GetFieldAsDateTime(const AName: string): TDateTime; override;
    function    GetFieldAsStringByIndex(AIndex: integer): string; override;
    function    GetFieldAsFloatByIndex(AIndex: integer): extended; override;
    function    GetFieldAsBooleanByIndex(AIndex: integer): boolean; override;
    function    GetFieldAsIntegerByIndex(AIndex: integer): int64; override;
    function    GetFieldAsDateTimeByIndex(AIndex: integer): TDateTime; override;
    function    GetFieldIsNullByIndex(AIndex: integer): boolean; override;
    function    GetSQL: TStrings; override;
    procedure   SetSQL(const AValue: TStrings); override;
    function    GetActive: boolean; override;
    procedure   SetActive(const AValue: boolean); override;
    function    GetEOF: boolean; override;
    function    GetParamAsString(const AName: string): string; override;
    function    GetParamAsBoolean(const AName: string): boolean; override;
    function    GetParamAsFloat(const AName: string): extended; override;
    function    GetParamAsInteger(const AName: string): int64; override;
    procedure   SetParamAsString(const AName, AValue: string); override;
    procedure   SetParamAsBoolean(const AName: string; const AValue: boolean); override;
    procedure   SetParamAsFloat(const AName: string; const AValue: extended); override;
    procedure   SetParamAsInteger(const AName: string; const AValue: int64); override;
    function    GetParamAsDateTime(const AName: string): TDateTime; override;
    procedure   SetParamAsDateTime(const AName: string; const AValue: TDateTime); override;
    function    GetParamIsNull(const AName: string): boolean; override;
    procedure   SetParamIsNull(const AName: string; const AValue: boolean); override;
    function    GetFieldIsNull(const AName: string): boolean; override;
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

uses
  SysUtils
  ,tiConstants
  ,tiExcept
  ,tiLog
  ,tiOPFManager
  ,tiUtils
  ,TypInfo
  ,Variants
 ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * TtiQuerySqldbIB
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
constructor TtiQuerySqldbIB.Create;
begin
  inherited;
  FIBSQL := TSQLQuery.Create(nil);
end;

// -----------------------------------------------------------------------------
destructor TtiQuerySqldbIB.Destroy;
begin
  FIBSQL.Free;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.Close;
begin
  Active := False;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.ExecSQL;
begin
  Prepare;
  FIBSQL.ExecSQL;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsBoolean(const AName: string): boolean;
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

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).AsDateTime;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsFloat(const AName: string): extended;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).AsFloat;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsInteger(const AName: string): int64;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).AsInteger;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsString(const AName: string): string;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).AsString;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsStringByIndex(AIndex: integer): string;
begin
  Result := FIBSQL.Fields[AIndex].AsString;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsFloatByIndex(AIndex: integer): extended;
begin
  Result := FIBSQL.Fields[AIndex].AsFloat;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsBooleanByIndex(AIndex: integer): boolean;
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

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsIntegerByIndex(AIndex: integer): int64;
begin
  Result := FIBSQL.Fields[AIndex].AsInteger;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsDateTimeByIndex(AIndex: integer): TDateTime;
begin
  Result := FIBSQL.Fields[AIndex].AsDateTime;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldIsNullByIndex(AIndex: integer): boolean;
begin
  Result := FIBSQL.Fields[AIndex].IsNull;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetActive: boolean;
begin
  Log('>>> TtiQuerySqldbIB.GetActive');
  Result := FIBSQL.Active; //FbActive;
  Log('<<< TtiQuerySqldbIB.GetActive');
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetEOF: boolean;
begin
  Result := FIBSQL.EOF;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsBoolean(const AName: string): boolean;
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

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsDateTime(const AName: string): TDateTime;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsDateTime;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsFloat(const AName: string): extended;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsFloat;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsInteger(const AName: string): int64;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsInteger;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsString(const AName: string): string;
begin
  Log('>>> TtiQuerySqldbIB.GetParamAsString');
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).AsString;
  Log('<<< TtiQuerySqldbIB.GetParamAsString');
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetSQL: TStrings;
begin
  Result := FIBSQL.SQL;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.Next;
begin
  FIBSQL.Next;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.Open;
begin
  Log('>>> Open');
  log('ParamCount: ' + IntToStr(ParamCount));
  FIBSQL.Open;
  Log('<<< Open');
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.ParamCount: integer;
begin
  Result := FIBSQL.Params.Count;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.ParamName(AIndex: integer): string;
begin
  Result := FIBSQL.Params[AIndex].Name;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetActive(const AValue: boolean);
begin
  log('>>> TtiQuerySqldbIB.SetActive');
  Assert(Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  if AValue then
  begin
    Log('Exec Query');
    FIBSQL.ExecSQL;
    FbActive := True;
  end
  else
  begin
    Log('Closing Query');
    FIBSQL.Close;
    FbActive := False;
  end;
  log('<<< TtiQuerySqldbIB.SetActive');
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  Prepare;
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

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsDateTime(const AName: string;
  const AValue: TDateTime);
begin
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsDateTime := AValue;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsFloat(const AName: string; const AValue: extended);
begin
  log('>>> TtiQuerySqldbIB.SetParamAsFloat');
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsFloat := AValue;
  log('<<< TtiQuerySqldbIB.SetParamAsFloat');
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsInteger(const AName: string; const AValue: int64);
begin
  log('>>> TtiQuerySqldbIB.SetParamAsInteger');
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsInteger := AValue;
  log('<<< TtiQuerySqldbIB.SetParamAsInteger');
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsString(const AName, AValue: string);
begin
  log('>>> TtiQuerySqldbIB.SetParamAsString');
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(AName)).AsString := AValue;
  log('<<< TtiQuerySqldbIB.SetParamAsString');
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetSQL(const AValue: TStrings);
begin
  log('>>>> SetSQL');
  FIBSQL.SQL.Assign(AValue);
  log('<<<< SetSQL');
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.Prepare;
begin
  if FIBSQL.Prepared then
    Exit; //==>
  FIBSQL.Prepare;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AssignParamFromStream(const AName: string;
  const AStream: TStream);
begin
  Assert(AStream <> NIL, 'Stream not assigned');
  AStream.Position := 0;
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(AName)).LoadFromStream(AStream, ftBlob);
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AssignParamToStream(const AName: string;
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

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AssignFieldAsStream(const AName: string;
  const AStream: TStream);
begin
  {$Note Look at AssignParamToStream if this doesn't work}
  Assert(AStream <> NIL, 'Stream not assigned');
  AStream.Position := 0;
  (FIBSQL.FieldByName(UpperCase(AName)) as TBlobField).SaveToStream(AStream);
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AssignFieldAsStreamByIndex(AIndex: integer;
  const AValue: TStream);
begin
  Assert(AValue <> NIL, 'Stream not assigned');
  AValue.Position := 0;
  TBlobField(FIBSQL.Fields[AIndex]).SaveToStream(AValue);
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  if (Database is TtiDatabaseSqldIB) then
  begin
    FIBSQL.Database := TtiDatabaseSqldIB(Database).IBDatabase;
    FIBSQL.Transaction := TtiDatabaseSqldIB(Database).IBDatabase.Transaction;
  end;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.DetachDatabase;
begin
  inherited DetachDatabase;
  if FIBSQL.Active then
    FIBSQL.Close;
  FIBSQL.Transaction := nil;
  FIBSQL.Database   := nil;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.FieldCount: integer;
begin
  Result := FIBSQL.FieldCount;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.FieldName(AIndex: integer): string;
begin
  Result := FIBSQL.Fields[AIndex].Name;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.Reset;
begin
  Active := False;
  FIBSQL.SQL.Clear;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.FieldIndex(const AName: string): integer;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).Index;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * TtiDatabaseSqldIB
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

constructor TtiDatabaseSqldIB.Create;
begin
  inherited Create;
  FDatabase              := TIBConnection.Create(nil);
  FDatabase.LoginPrompt  := False;
  FDatabase.Dialect      := 3;
  FIBTransaction         := TSQLTransaction.Create(nil);
  FIBTransaction.DataBase := FDatabase;
  FDatabase.Transaction  := FIBTransaction;
  FIBTransaction.Active := False;
end;

// -----------------------------------------------------------------------------
destructor TtiDatabaseSqldIB.Destroy;
begin
  try
    FIBTransaction.Active  := False;
    FDatabase.Connected    := False;
    FDatabase.Transaction  := nil;
    FIBTransaction.Database := nil;
    FIBTransaction.Free;
    FDatabase.Free;
  except
    on e: Exception do
      LogError(e.message);
  end;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseSqldIB.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');

//  FIBTransaction.CommitRetaining;
  FIBTransaction.Commit;
end;

// -----------------------------------------------------------------------------
function TtiDatabaseSqldIB.InTransaction: boolean;
begin
//  Result := False;
  Result := FIBTransaction.Active;
//  Result := (FIBTransaction.Handle <> NIL);
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseSqldIB.RollBack;
begin
//  FIBTransaction.RollbackRetaining;
  FIBTransaction.RollBack;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseSqldIB.StartTransaction;
begin
  Log('>>>> Start transaction...');
  if InTransaction then
    raise EtiOPFInternalException.Create(
      'Attempt to start a transaction but transaction already exists.');

  FIBTransaction.StartTransaction;
  Log('<<<< Start transaction...');
end;

// -----------------------------------------------------------------------------
// This code is cloned in TtiQueryBDEAbs - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.FieldKind(AIndex: integer): TtiQueryFieldKind;
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
function TtiQuerySqldbIB.IBFieldKindToTIFieldKind(pDataType: TFieldType): TtiQueryFieldKind;
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


function TtiQuerySqldbIB.FieldSize(AIndex: integer): integer;
begin
  if FieldKind(AIndex) in [qfkInteger, qfkFloat, qfkDateTime,
    qfkLogical, qfkLongString] then
    Result := 0
  else
    Result := FIBSQL.Fields[AIndex].Size;
end;


function TtiQuerySqldbIB.GetParamIsNull(const AName: string): boolean;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(AName)).IsNull;
end;


procedure TtiQuerySqldbIB.SetParamIsNull(const AName: string; const AValue: boolean);
begin
  if AValue then
  begin
    Prepare;
    FIBSQL.Params.ParamByName(UpperCase(AName)).Value := Null;
  end;
end;


function TtiDatabaseSqldIB.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;


procedure TtiDatabaseSqldIB.SetConnected(AValue: boolean);
var
  lMessage: string;
begin
  try
    if (not AValue) then
    begin
      Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
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


function TtiQuerySqldbIB.GetFieldIsNull(const AName: string): boolean;
begin
  Result := FIBSQL.FieldByName(UpperCase(AName)).IsNull;
end;


procedure TtiDatabaseSqldIB.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lQuery:    TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable:    TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery   := gTIOPFManager.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      lQuery.SQLText :=
        'SELECT RDB$RELATION_NAME as Table_Name ' +
        'FROM RDB$RELATIONS              ' +
        'WHERE ((RDB$SYSTEM_FLAG = 0) OR ' +
        '(RDB$SYSTEM_FLAG IS NULL)) AND  ' +
        '(RDB$VIEW_SOURCE IS NULL)       ' +
        'ORDER BY RDB$RELATION_NAME      ';
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


procedure TtiDatabaseSqldIB.ReadMetaDataFields(AData: TtiDBMetaDataTable);
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
  lQuery    := gTIOPFManager.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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


function TtiDatabaseSqldIB.FieldMetaDataToSQLCreate(
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
    qfkDateTime: if IBDatabase.Dialect <> 1 then
        Result := 'TIMESTAMP'
      else
        Result := 'Date';
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


procedure TtiQuerySqldbIB.AssignParams(const AParams, AWhere: TtiQueryParams);
begin
  if AParams = NIL then
    Exit;
  Prepare;
  inherited;
end;


class procedure TtiDatabaseSqldIB.CreateDatabase(
  const ADatabaseName, AUserName, APassword: string);
begin
  if (ADatabaseName <> '') or (AUserName <> '') or (APassword <> '') then;
  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
end;


class function TtiDatabaseSqldIB.DatabaseExists(
  const ADatabaseName, AUserName, APassword: string): boolean;
var
  lDatabase: TtiDatabaseSqldIB;
begin
  lDatabase := TtiDatabaseSqldIB.Create;
  try
    lDatabase.FDatabase.DatabaseName := ADatabaseName;
    lDatabase.FDatabase.UserName := AUserName;
    lDatabase.FDatabase.Password := APassword;
    lDatabase.FDatabase.Params.Values['user_name']:= AUserName;
    lDatabase.FDatabase.Params.Values['password']:= APassword;
    try
      lDatabase.FDatabase.Connected := True;
      Result := True;
    except
      on e: Exception do
        Result := False;
    end;
    lDatabase.FDatabase.Connected := False;
  finally
    lDatabase.Free;
  end;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.HasNativeLogicalType: boolean;
begin
  Result := False;
end;

// -----------------------------------------------------------------------------
function TtiDatabaseSqldIB.Test: boolean;
begin
  Result := False;
  Assert(False, 'Under construction');
end;

initialization

  gTIOPFManager.RegPerLayers.__RegisterPersistenceLayer(
        cTIPersistSqldbIB,
        TtiDBConnectionPoolDataAbs,
        TtiQuerySqldbIB,
        TtiDatabaseSqldIB);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.RegPerLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbIB);

end.
