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
    procedure   SetConnected(pbValue: boolean); override;
    function    GetConnected: boolean; override;
    function    FieldMetaDataToSQLCreate(const pFieldMetaData: TtiDBMetaDataField): string; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    class function DatabaseExists(const pDatabaseName, pUserName, pPassword: string): boolean; override;
    class procedure CreateDatabase(const pDatabaseName, pUserName, pPassword: string); override;
    property    IBDatabase: TIBConnection Read FDatabase Write FDatabase;
    procedure   StartTransaction; override;
    function    InTransaction: boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    procedure   ReadMetaDataTables(pData: TtiDBMetaData); override;
    procedure   ReadMetaDataFields(pData: TtiDBMetaDataTable); override;
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
    function    GetFieldAsString(const psName: string): string; override;
    function    GetFieldAsFloat(const psName: string): extended; override;
    function    GetFieldAsBoolean(const psName: string): boolean; override;
    function    GetFieldAsInteger(const psName: string): int64; override;
    function    GetFieldAsDateTime(const psName: string): TDateTime; override;
    function    GetFieldAsStringByIndex(pIndex: integer): string; override;
    function    GetFieldAsFloatByIndex(pIndex: integer): extended; override;
    function    GetFieldAsBooleanByIndex(pIndex: integer): boolean; override;
    function    GetFieldAsIntegerByIndex(pIndex: integer): int64; override;
    function    GetFieldAsDateTimeByIndex(pIndex: integer): TDateTime; override;
    function    GetFieldIsNullByIndex(pIndex: integer): boolean; override;
    function    GetSQL: TStrings; override;
    procedure   SetSQL(const Value: TStrings); override;
    function    GetActive: boolean; override;
    procedure   SetActive(const Value: boolean); override;
    function    GetEOF: boolean; override;
    function    GetParamAsString(const psName: string): string; override;
    function    GetParamAsBoolean(const psName: string): boolean; override;
    function    GetParamAsFloat(const psName: string): extended; override;
    function    GetParamAsInteger(const psName: string): int64; override;
    procedure   SetParamAsString(const psName, Value: string); override;
    procedure   SetParamAsBoolean(const psName: string; const Value: boolean); override;
    procedure   SetParamAsFloat(const psName: string; const Value: extended); override;
    procedure   SetParamAsInteger(const psName: string; const Value: int64); override;
    function    GetParamAsDateTime(const psName: string): TDateTime; override;
    procedure   SetParamAsDateTime(const psName: string; const Value: TDateTime); override;
    function    GetParamIsNull(const psName: string): boolean; override;
    procedure   SetParamIsNull(const psName: string; const Value: boolean); override;
    function    GetFieldIsNull(const psName: string): boolean; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open; override;
    procedure   Close; override;
    procedure   Next; override;
    procedure   ExecSQL; override;
    function    ParamCount: integer; override;
    function    ParamName(pIndex: integer): string; override;
    procedure   AssignParamFromStream(const pName: string; const pStream: TStream); override;
    procedure   AssignParamToStream(const pName: string; const pStream: TStream); override;
    procedure   AssignFieldAsStream(const pName: string; const pStream: TStream); override;
    procedure   AssignFieldAsStreamByIndex(pIndex: integer; const pValue: TStream); override;
    procedure   AssignParams(const pParams: TtiQueryParams; const pWhere: TtiQueryParams = nil); override;
    procedure   AttachDatabase(pDatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;
    function    FieldCount: integer; override;
    function    FieldName(pIndex: integer): string; override;
    function    FieldIndex(const psName: string): integer; override;
    function    FieldKind(pIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(pIndex: integer): integer; override;
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
function TtiQuerySqldbIB.GetFieldAsBoolean(const psName: string): boolean;
var
  lsValue: string;
begin
  lsValue := Trim(upperCase(FIBSQL.FieldByName(UpperCase(psName)).AsString));
  Result  := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  Result := FIBSQL.FieldByName(UpperCase(psName)).AsDateTime;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsFloat(const psName: string): extended;
begin
  Result := FIBSQL.FieldByName(UpperCase(psName)).AsFloat;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsInteger(const psName: string): int64;
begin
  Result := FIBSQL.FieldByName(UpperCase(psName)).AsInteger;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsString(const psName: string): string;
begin
  Result := FIBSQL.FieldByName(UpperCase(psName)).AsString;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsStringByIndex(pIndex: integer): string;
begin
  Result := FIBSQL.Fields[pIndex].AsString;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsFloatByIndex(pIndex: integer): extended;
begin
  Result := FIBSQL.Fields[pIndex].AsFloat;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsBooleanByIndex(pIndex: integer): boolean;
var
  lsValue: string;
begin
  lsValue := Trim(FIBSQL.Fields[pIndex].AsString);
  Result  := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsIntegerByIndex(pIndex: integer): int64;
begin
  Result := FIBSQL.Fields[pIndex].AsInteger;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldAsDateTimeByIndex(pIndex: integer): TDateTime;
begin
  Result := FIBSQL.Fields[pIndex].AsDateTime;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetFieldIsNullByIndex(pIndex: integer): boolean;
begin
  Result := FIBSQL.Fields[pIndex].IsNull;
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
function TtiQuerySqldbIB.GetParamAsBoolean(const psName: string): boolean;
var
  lValue: string;
begin
  lValue := FIBSQL.Params.ParamByName(UpperCase(psName)).AsString;
{$IFDEF BOOLEAN_CHAR_1}
  Result := SameText(lValue, 'T');
{$ELSE}
  Result := SameText(lValue, 'TRUE');
{$ENDIF}// BOOLEAN_CHAR_1
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsDateTime(const psName: string): TDateTime;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(psName)).AsDateTime;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsFloat(const psName: string): extended;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(psName)).AsFloat;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsInteger(const psName: string): int64;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(psName)).AsInteger;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.GetParamAsString(const psName: string): string;
begin
  Log('>>> TtiQuerySqldbIB.GetParamAsString');
  Result := FIBSQL.Params.ParamByName(UpperCase(psName)).AsString;
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
function TtiQuerySqldbIB.ParamName(pIndex: integer): string;
begin
  Result := FIBSQL.Params[pIndex].Name;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetActive(const Value: boolean);
begin
  log('>>> TtiQuerySqldbIB.SetActive');
  Assert(Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  if Value then
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
procedure TtiQuerySqldbIB.SetParamAsBoolean(const psName: string; const Value: boolean);
begin
  Prepare;
{$IFDEF BOOLEAN_CHAR_1}
  if Value then
    FIBSQL.Params.ParamByName(UpperCase(psName)).AsString := 'T'
  else
    FIBSQL.Params.ParamByName(UpperCase(psName)).AsString := 'F';
{$ELSE}
  if Value then
    FIBSQL.Params.ParamByName(UpperCase(psName)).AsString := 'TRUE'
  else
    FIBSQL.Params.ParamByName(UpperCase(psName)).AsString := 'FALSE';
{$ENDIF}// BOOLEAN_CHAR_1
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsDateTime(const psName: string;
  const Value: TDateTime);
begin
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(psName)).AsDateTime := Value;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsFloat(const psName: string; const Value: extended);
begin
  log('>>> TtiQuerySqldbIB.SetParamAsFloat');
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(psName)).AsFloat := Value;
  log('<<< TtiQuerySqldbIB.SetParamAsFloat');
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsInteger(const psName: string; const Value: int64);
begin
  log('>>> TtiQuerySqldbIB.SetParamAsInteger');
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(psName)).AsInteger := Value;
  log('<<< TtiQuerySqldbIB.SetParamAsInteger');
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetParamAsString(const psName, Value: string);
begin
  log('>>> TtiQuerySqldbIB.SetParamAsString');
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(psName)).AsString := Value;
  log('<<< TtiQuerySqldbIB.SetParamAsString');
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.SetSQL(const Value: TStrings);
begin
  log('>>>> SetSQL');
  FIBSQL.SQL.Assign(Value);
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
procedure TtiQuerySqldbIB.AssignParamFromStream(const pName: string;
  const pStream: TStream);
begin
  Assert(pStream <> NIL, 'Stream not assigned');
  pStream.Position := 0;
  Prepare;
  FIBSQL.Params.ParamByName(UpperCase(pName)).LoadFromStream(pStream, ftBlob);
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AssignParamToStream(const pName: string;
  const pStream: TStream);
var
  lBinData:          olevariant;
  lDataPtr:          Pointer;
  lHigh, lLow, lLen: integer;
  lParameter:        TParam;
begin
  Assert(pStream <> NIL, 'Stream not assigned');
  lParameter := FIBSQL.Params.ParamByName(UpperCase(pName));
  lLow       := VarArrayLowBound(lParameter.Value, 1);
  lHigh      := VarArrayHighBound(lParameter.Value, 1);
  lLen       := lHigh - lLow + 1;
  lBinData   := VarArrayCreate([0, lLen], varByte);
  lBinData   := lParameter.Value;
  lDataPtr   := VarArrayLock(lBinData);
  try
    pStream.WriteBuffer(lDataPtr^, lLen);
  finally
    VarArrayUnlock(lBinData);
  end;

{$Note  Please try this option of saving to a Stream as well }
{
  Assert(pStream <> nil, 'Stream not assigned');
  pStream.Position := 0;
  (FIBSQL.Params.ParamByName(UpperCase(pName)) as TBlobField).SaveToStream(pStream);
  pStream.Position := 0;
}
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AssignFieldAsStream(const pName: string;
  const pStream: TStream);
begin
  {$Note Look at AssignParamToStream if this doesn't work}
  Assert(pStream <> NIL, 'Stream not assigned');
  pStream.Position := 0;
  (FIBSQL.FieldByName(UpperCase(pName)) as TBlobField).SaveToStream(pStream);
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AssignFieldAsStreamByIndex(pIndex: integer;
  const pValue: TStream);
begin
  Assert(pValue <> NIL, 'Stream not assigned');
  pValue.Position := 0;
  TBlobField(FIBSQL.Fields[pIndex]).SaveToStream(pValue);
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.AttachDatabase(pDatabase: TtiDatabase);
begin
  inherited AttachDatabase(pDatabase);
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
  FIBSQL.Database    := nil;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.FieldCount: integer;
begin
  Result := FIBSQL.FieldCount;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.FieldName(pIndex: integer): string;
begin
  Result := FIBSQL.Fields[pIndex].Name;
end;

// -----------------------------------------------------------------------------
procedure TtiQuerySqldbIB.Reset;
begin
  Active := False;
  FIBSQL.SQL.Clear;
end;

// -----------------------------------------------------------------------------
function TtiQuerySqldbIB.FieldIndex(const psName: string): integer;
begin
  Result := FIBSQL.FieldByName(UpperCase(psName)).Index;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * TtiDatabaseSqldIB
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

constructor TtiDatabaseSqldIB.Create;
begin
  inherited Create;
  FDatabase               := TIBConnection.Create(nil);
  FDatabase.LoginPrompt   := False;
  FDatabase.Dialect       := 3;
  FIBTransaction          := TSQLTransaction.Create(nil);
  FIBTransaction.DataBase := FDatabase;
  FDatabase.Transaction   := FIBTransaction;
  FIBTransaction.Active := False;
end;

// -----------------------------------------------------------------------------
destructor TtiDatabaseSqldIB.Destroy;
begin
  try
    FIBTransaction.Active   := False;
    FDatabase.Connected     := False;
    FDatabase.Transaction   := nil;
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
function TtiQuerySqldbIB.FieldKind(pIndex: integer): TtiQueryFieldKind;
var
  lDataType: TFieldType;
begin
  lDataType := FIBSQL.Fields[pIndex].DataType;

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
    ftBoolean: Result         := qfkLogical;
    ftFloat, ftCurrency, ftBCD: Result := qfkFloat;
    ftDate, ftTime, ftDateTime: Result := qfkDateTime;
    ftBlob, ftGraphic, ftVarBytes: Result := qfkBinary;
    ftMemo, ftFmtMemo: Result := qfkLongString;
    else
      raise Exception.Create('Invalid FIBSQL.Fields[ pIndex ].DataType <' +
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
    raise Exception.Create('Invalid FIBSQL.Fields[ pIndex ].DataType <' +
      GetEnumName(TypeInfo(TFieldType), Ord(pDataType)) +
      '>');
  end;
end;


function TtiQuerySqldbIB.FieldSize(pIndex: integer): integer;
begin
  if FieldKind(pIndex) in [qfkInteger, qfkFloat, qfkDateTime,
    qfkLogical, qfkLongString] then
    Result := 0
  else
    Result := FIBSQL.Fields[pIndex].Size;
end;


function TtiQuerySqldbIB.GetParamIsNull(const psName: string): boolean;
begin
  Result := FIBSQL.Params.ParamByName(UpperCase(psName)).IsNull;
end;


procedure TtiQuerySqldbIB.SetParamIsNull(const psName: string; const Value: boolean);
begin
  if Value then
  begin
    Prepare;
    FIBSQL.Params.ParamByName(UpperCase(psName)).Value := Null;
  end;
end;


function TtiDatabaseSqldIB.GetConnected: boolean;
begin
  Result := FDatabase.Connected;
end;


procedure TtiDatabaseSqldIB.SetConnected(pbValue: boolean);
var
  lMessage: string;
begin
  try
    if (not pbValue) then
    begin
      Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
      FDatabase.Connected := False;
      Exit; //==>
    end;

    FDatabase.DatabaseName := DatabaseName;
    FDatabase.UserName := Username;
    FDatabase.Password := Password;
    FDatabase.Params.Values['user_name']  := UserName;
    FDatabase.Params.Values['password']   := Password;
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
      //        raise EtiOPFDBExceptionUserNamePassword.Create( cTIPersistIBX, DatabaseName, UserName, Password )
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


function TtiQuerySqldbIB.GetFieldIsNull(const psName: string): boolean;
begin
  Result := FIBSQL.FieldByName(UpperCase(psName)).IsNull;
end;


procedure TtiDatabaseSqldIB.ReadMetaDataTables(pData: TtiDBMetaData);
var
  lQuery:    TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable:    TtiDBMetaDataTable;
begin
  lMetaData := (pData as TtiDBMetaData);
  lQuery    := gTIOPFManager.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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


procedure TtiDatabaseSqldIB.ReadMetaDataFields(pData: TtiDBMetaDataTable);
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
  lTable     := (pData as TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery     := gTIOPFManager.RegPerLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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
  const pFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := pFieldMetaData.Name;
  case pFieldMetaData.Kind of
    qfkString: Result := 'VarChar( ' + IntToStr(pFieldMetaData.Width) + ' )';
    qfkInteger: Result := 'Integer';
    //    qfkFloat: result := 'Decimal( 10, 5 )';
    qfkFloat: Result   := 'DOUBLE PRECISION';
    // Just for new version of IB (6.x)
    // DATE holds only DATE without TIME...
    qfkDateTime: if IBDatabase.Dialect <> 1 then
        Result := 'TIMESTAMP'
      else
        Result := 'Date';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical: Result := 'Char( 1 ) default ''F'' check( ' +
        lFieldName + ' in ( ''T'', ''F'' ))';
    {$ELSE}
    qfkLogical: Result := 'VarChar( 5 ) default ''FALSE'' check( ' +
        lFieldName + ' in ( ''TRUE'', ''FALSE'' )) ';
    {$ENDIF}
    qfkBinary: Result := 'Blob sub_type 0';
    qfkLongString: Result := 'Blob sub_type 1';
    else
      raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;


procedure TtiQuerySqldbIB.AssignParams(const pParams, pWhere: TtiQueryParams);
begin
  if pParams = NIL then
    Exit;
  Prepare;
  inherited;
end;


class procedure TtiDatabaseSqldIB.CreateDatabase(
  const pDatabaseName, pUserName, pPassword: string);
begin
  if (pDatabaseName <> '') or (pUserName <> '') or (pPassword <> '') then;
  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
end;


class function TtiDatabaseSqldIB.DatabaseExists(
  const pDatabaseName, pUserName, pPassword: string): boolean;
var
  lDatabase: TtiDatabaseSqldIB;
begin
  lDatabase := TtiDatabaseSqldIB.Create;
  try
    lDatabase.FDatabase.DatabaseName := pDatabaseName;
    lDatabase.FDatabase.UserName := pUserName;
    lDatabase.FDatabase.Password := pPassword;
    lDatabase.FDatabase.Params.Values['user_name'] := pUserName;
    lDatabase.FDatabase.Params.Values['password'] := pPassword;
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