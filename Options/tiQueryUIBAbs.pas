{
  TODO: Refactor this unit so it descends from tiQueryDataset classes.
}
unit tiQueryUIBAbs;

{$I tiDefines.inc}

interface
uses
  tiQuery
  ,SysUtils
  ,Classes
  ,UIB
  ,UIBase
  ,UIBLib
  ,tiAutoMap
  ,tiObject
  ,tiPersistenceLayers
  ,tiDBConnectionPool
  ;

type
  EtiOPFDBException = Class(Exception)
    constructor Create(const APerLayerName, ADatabaseName, AUserName, APassword : string; const AMessage : string = ''); Virtual;
  end;

  EtiOPFDBExceptionUserNamePassword = Class(EtiOPFDBException)
    constructor Create(const APerLayerName, ADatabaseName, AUserName, APassword : string; const AMessage : string = ''); override;
  end;

  // ---------------------------------------------------------------------------
  TtiDatabaseUIBAbs = Class(TtiDatabaseSQL)
  private
    FDatabase : TUIBDatabase;
    FTransaction : TUIBTransaction;
    FLayerName : string;
  protected
    procedure SetConnected(AValue : boolean); override;
    function GetConnected : boolean; override;
    function FieldMetaDataToSQLCreate(const AFieldMetaData :  TtiDBMetaDataField) : string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property UIBDatabase : TUIBDatabase Read FDatabase Write FDatabase;
    procedure StartTransaction; override;
    function InTransaction : boolean; override;
    procedure Commit; override;
    procedure RollBack; override;
    procedure ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    function Test : boolean; override;
    property LayerName : string Read FLayerName Write FLayerName;
  end;


  TtiQueryUIBAbs = Class(TtiQuerySQL)
  private
    FActive : boolean;
    FWasPrepared : boolean;
    FQuery : TUIBQuery;
    FSQLParams : TSQLParams;
    function IBFieldKindToTIFieldKind(pData : TUIBSQLVar) : TtiQueryFieldKind;
  protected
    { these two methods come from tiQueryDataset }
    function ParamsAsStringList: TStringList;
    procedure LogParams;
//    function GetParamAsVariant(const AName : string) : Variant; override;
//    procedure SetParamAsVariant(const AName : string; const Value : Variant); override;
//    function GetFieldAsVariant(const AName : string) : Variant; override;
    function GetFieldAsBoolean(const AName : string) : boolean; override;
    function GetFieldAsDateTime(const AName : string) : TDateTime; override;
    function GetFieldAsFloat(const AName : string) : extended; override;
    function GetFieldAsInteger(const AName : string) : Int64; override;
    function GetFieldAsString(const AName : string) : string; override;
    function GetFieldIsNull(const AName : string) : boolean; override;

    function GetActive : boolean; override;
    function GetEOF : boolean; override;
    function GetRecordCount: Integer; override;
    function GetRowsAffected: Integer; override;
    function GetSQL : TStrings; override;
    procedure SetActive(const AValue : boolean); override;
    procedure SetSQL(const AValue : TStrings); override;
    procedure InternalPrepare; Virtual;

    function GetParamAsBoolean(const AName : string) : boolean; override;
    function GetParamAsDateTime(const AName : string) : TDateTime; override;
    function GetParamAsFloat(const AName : string) : extended; override;
    function GetParamAsInteger(const AName : string) : Int64; override;
    function GetParamAsString(const AName : string) : string; override;
    function GetParamAsTextBLOB(const AName : string) : string; override;
    function GetParamIsNull(const AName : string) : boolean; override;

    procedure SetParamAsBoolean(const AName : string; const AValue : boolean); override;
    procedure SetParamAsDateTime(const AName : string; const AValue : TDateTime); override;
    procedure SetParamAsFloat(const AName : string; const AValue : extended); override;
    procedure SetParamAsInteger(const AName : string; const AValue : Int64); override;
    procedure SetParamAsString(const AName, AValue : string); override;
    procedure SetParamAsTextBLOB(const AName, AValue : string); override;
    procedure SetParamIsNull(const AName : string; const AValue : boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
    procedure Next; override;
    function ExecSQL : Integer; override;

    function ParamCount : Integer; override;
    function ParamName(AIndex : integer) : string; override;

    procedure AssignParamFromStream(const AName : string; const AStream : TStream); override;
    procedure AssignParamToStream(const AName : string; const AStream : TStream); override;
    procedure AssignFieldAsStream(const AName : string; const AStream : TStream); override;

    procedure AttachDatabase(ADatabase : TtiDatabase); override;
    procedure DetachDatabase; override;
    procedure Reset; override;

    function FieldCount : Integer; override;
    function FieldName(AIndex : integer) : string; override;
    function FieldIndex(const AName : string) : Integer; override;
    function FieldKind(AIndex : integer) : TtiQueryFieldKind; override;
    function FieldSize(AIndex : integer) : Integer; override;
    function HasNativeLogicalType : boolean; override;

  end;

Implementation

Uses
  DB
  , Math
  , tiUtils
  , tiLog
  , TypInfo
  , tiOPFManager
  , tiExcept
  , tiConstants
  , Variants
  ;

Type
  TSQLType = Record
    SqlType : Integer;
    TypeName : string;
  end;

  TSQLTypes = Array[0..13] Of TSQLType;

  TPrivTypes = Record
    PrivFlag : Integer;
    PrivString : string;
  end;

const
  priv_UNKNOWN = 1;
  priv_SELECT = 2;
  priv_INSERT = 4;
  priv_UPDATE = 8;
  priv_DELETE = 16;
  priv_EXECUTE = 32;
  priv_REFERENCES = 64;

  PrivTypes : Array[0..5] Of TPrivTypes = (
    (PrivFlag : priv_DELETE; PrivString : 'DELETE'), {do not localize}
    (PrivFlag : priv_EXECUTE; PrivString : 'EXECUTE'), {do not localize}
    (PrivFlag : priv_INSERT; PrivString : 'INSERT'), {do not localize}
    (PrivFlag : priv_SELECT; PrivString : 'SELECT'), {do not localize}
    (PrivFlag : priv_UPDATE; PrivString : 'UPDATE'), {do not localize}
    (PrivFlag : priv_REFERENCES; PrivString : 'REFERENCES')); {do not localize}

  ColumnTypes : TSQLTypes = (
    (SqlType : blr_short; TypeName : 'SMALLINT'), {do not localize}
    (SqlType : blr_long; TypeName : 'INTEGER'), {do not localize}
    (SqlType : blr_quad; TypeName : 'QUAD'), {do not localize}
    (SqlType : blr_float; TypeName : 'FLOAT'), {do not localize}
    (SqlType : blr_text; TypeName : 'CHAR'), {do not localize}
    (SqlType : blr_double; TypeName : 'DOUBLE PRECISION'), {do not localize}
    (SqlType : blr_varying; TypeName : 'VARCHAR'), {do not localize}
    (SqlType : blr_cstring; TypeName : 'CSTRING'), {do not localize}
    (SqlType : blr_blob_id; TypeName : 'BLOB_ID'), {do not localize}
    (SqlType : blr_blob; TypeName : 'BLOB'), {do not localize}
    (SqlType : blr_sql_time; TypeName : 'TIME'), {do not localize}
    (SqlType : blr_sql_date; TypeName : 'DATE'), {do not localize}
    (SqlType : blr_timestamp; TypeName : 'TIMESTAMP'), {do not localize}
    (SqlType : blr_int64; TypeName : 'INT64')); {do not localize}

  SubTypes : Array[0..8] Of string = (
    'UNKNOWN', {do not localize}
    'TEXT', {do not localize}
    'BLR', {do not localize}
    'ACL', {do not localize}
    'RANGES', {do not localize}
    'SUMMARY', {do not localize}
    'FORMAT', {do not localize}
    'TRANSACTION_DESCRIPTION', {do not localize}
    'EXTERNAL_FILE_DESCRIPTION'); {do not localize}

  TriggerTypes : Array[0..6] Of string = (
    '', {do not localize}
    'BEFORE INSERT', {do not localize}
    'AFTER INSERT', {do not localize}
    'BEFORE UPDATE', {do not localize}
    'AFTER UPDATE', {do not localize}
    'BEFORE DELETE', {do not localize}
    'AFTER DELETE'); {do not localize}

  IntegralSubtypes : Array[0..2] Of string = (
    'UNKNOWN', {do not localize}
    'NUMERIC', {do not localize}
    'DECIMAL'); {do not localize}

  ODS_VERSION6 = 6; { on-disk structure as of v3.0 }
  ODS_VERSION7 = 7; { new on disk structure for fixing index bug }
  ODS_VERSION8 = 8; { new btree structure to support pc semantics }
  ODS_VERSION9 = 9; { btree leaf pages are always propogated up }
  ODS_VERSION10 = 10; { V6.0 features. SQL delimited idetifier,
                                        SQLDATE, and 64-bit exact numeric
                                        type }

  { flags for RDB$FILE_FLAGS }
  FILE_shadow = 1;
  FILE_inactive = 2;
  FILE_manual = 4;
  FILE_cache = 8;
  FILE_conditional = 16;

  { flags for RDB$LOG_FILES }
  LOG_serial = 1;
  LOG_default = 2;
  LOG_raw = 4;
  LOG_overflow = 8;



  MAX_INTSUBTYPES = 2;
  MAXSUBTYPES = 8; { Top of subtypes array }

{ Object types used in RDB$DEPENDENCIES and RDB$USER_PRIVILEGES }

  obj_relation = 0;
  obj_view = 1;
  obj_trigger = 2;
  obj_computed = 3;
  obj_validation = 4;
  obj_procedure = 5;
  obj_expression_index = 6;
  obj_exception = 7;
  obj_user = 8;
  obj_field = 9;
  obj_index = 10;
  obj_count = 11;
  obj_user_group = 12;
  obj_sql_role = 13;

  CollationSQL =
    'SELECT CST.RDB$CHARACTER_SET_NAME, COL.RDB$COLLATION_NAME, CST.RDB$DEFAULT_COLLATE_NAME ' + {do not localize}
    'FROM RDB$COLLATIONS COL JOIN RDB$CHARACTER_SETS CST ON ' + {do not localize}
    '  COL.RDB$CHARACTER_SET_ID = CST.RDB$CHARACTER_SET_ID ' + {do not localize}
    'WHERE ' + {do not localize}
    '  COL.RDB$COLLATION_ID = :COLLATION AND ' + {do not localize}
    '  CST.RDB$CHARACTER_SET_ID = :CHAR_SET_ID ' + {do not localize}
    'ORDER BY COL.RDB$COLLATION_NAME, CST.RDB$CHARACTER_SET_NAME'; {do not localize}

  NonCollationSQL =
    'SELECT CST.RDB$CHARACTER_SET_NAME ' + {do not localize}
    'FROM RDB$CHARACTER_SETS CST ' + {do not localize}
    'WHERE CST.RDB$CHARACTER_SET_ID = :CHARSETID ' + {do not localize}
    'ORDER BY CST.RDB$CHARACTER_SET_NAME'; {do not localize}

  PrecisionSQL =
    'SELECT * FROM RDB$FIELDS ' + {do not localize}
    'WHERE RDB$FIELD_NAME = :FIELDNAME'; {do not localize}

  ArraySQL =
    'SELECT * FROM RDB$FIELD_DIMENSIONS FDIM ' + {do not localize}
    'WHERE ' + {do not localize}
    '  FDIM.RDB$FIELD_NAME = :FIELDNAME ' + {do not localize}
    'ORDER BY FDIM.RDB$DIMENSION'; {do not localize}

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryUIB
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{
********************************* TtiQueryUIB **********************************
}

constructor TtiQueryUIBAbs.Create;
begin
  Inherited;
  FSQLParams := TSQLParams.Create(csUTF8);
  FSupportsRowsAffected := True;
  FWasPrepared := false;
  FQuery := TUIBQuery.Create(Nil);
  FQuery.FetchBlobs := True;
  FQuery.OnError := etmStayIn;
  FActive := false;
end;

destructor TtiQueryUIBAbs.Destroy;
begin
  FQuery.Free;
  FSQLParams.Free;
  Inherited;
end;

procedure TtiQueryUIBAbs.Close;
begin
  Active := false;
end;

function TtiQueryUIBAbs.ExecSQL : Integer;
begin
  Log(ClassName + ': [Prepare] ' + tiNormalizeStr(self.SQLText), lsSQL);
  InternalPrepare;
  LogParams;
  FQuery.Execute;
  Result := FQuery.RowsAffected;
end;

function TtiQueryUIBAbs.GetFieldAsBoolean(const AName : string) : boolean;
var
  lsValue: string;
begin
//  Result := FQuery.Fields.ByNameAsBoolean[UpperCase(AName)];
  lsValue := Trim(UpperCase(GetFieldAsString(AName))); //++IPK 2012-05-04
  result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;

function TtiQueryUIBAbs.GetFieldAsDateTime(const AName : string) : TDateTime;
begin
  result := FQuery.Fields.ByNameAsDateTime[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetFieldAsFloat(const AName : string) : extended;
begin
  result := FQuery.Fields.ByNameAsDouble[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetFieldAsInteger(const AName : string) : Int64;
begin
  Result := FQuery.Fields.ByNameAsInt64[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetFieldAsString(const AName : string) : string;
begin
  result := FQuery.Fields.ByNameAsString[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetActive : boolean;
begin
  result := FActive;
end;

function TtiQueryUIBAbs.GetRecordCount: Integer;
begin
  result := FQuery.RecordCount;
end;

function TtiQueryUIBAbs.GetRowsAffected: Integer;
begin
  result := FQuery.RowsAffected;
end;

function TtiQueryUIBAbs.GetEOF : boolean;
begin
  result := FQuery.EOF;
end;

function TtiQueryUIBAbs.GetParamAsBoolean(const AName : string) : boolean;
Var
  lValue : string;
begin
  lValue := FQuery.Params.ByNameAsString[UpperCase(AName)];
{$IFDEF BOOLEAN_CHAR_1}
  result := SameText(lValue, 'T');
{$ELSE}
  result := SameText(lValue, 'TRUE');
{$ENDIF} // BOOLEAN_CHAR_1
end;

function TtiQueryUIBAbs.GetParamAsDateTime(const AName : string) : TDateTime;
begin
  result := FQuery.Params.ByNameAsDateTime[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetParamAsFloat(const AName : string) : extended;
begin
  result := FQuery.Params.ByNameAsDouble[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetParamAsInteger(const AName : string) : Int64;
begin
  result := FQuery.Params.ByNameAsInt64[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetParamAsString(const AName : string) : string;
begin
  if FQuery.Params.ByNameIsBlob[UpperCase(AName)] then
    result := '[BlobData]'
  else
    Result := FQuery.Params.ByNameAsString[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetParamAsTextBLOB(const AName : string) : string;
begin
  Result := FQuery.Params.ByNameAsString[UpperCase(AName)];
//  Result := FQuery.Params.
 // Result := FQuery.Params.ByNameAsWideString[UpperCase(AName)];
end;

function TtiQueryUIBAbs.GetSQL : TStrings;
begin
  result := FQuery.SQL;
end;

procedure TtiQueryUIBAbs.Next;
begin
  FQuery.Next;
end;

procedure TtiQueryUIBAbs.Open;
begin
  Log(ClassName + ': ' + tiNormalizeStr(self.SQLText), lsSQL);
  LogParams;
  Active := true;
end;

function TtiQueryUIBAbs.ParamCount : Integer;
begin
  Result := FQuery.Params.ParamCount;
end;

function TtiQueryUIBAbs.ParamName(AIndex : integer) : string;
begin
  result := FQuery.Params.FieldName[AIndex];
end;

procedure TtiQueryUIBAbs.SetActive(const AValue : boolean);
begin
 // Assert(Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  If AValue Then
  begin
    try
      FQuery.Open;
      FActive := True;
    Except
        On e : exception Do
          Raise EtiOPFInternalException.Create(E.Message);
    end;
  end
  else
  begin
    FQuery.Close;
    FActive := False;
    FWasPrepared := False;
  end;
end;

procedure TtiQueryUIBAbs.SetParamAsBoolean(const AName : string; const AValue : boolean);
begin
  InternalPrepare;
{$IFDEF BOOLEAN_CHAR_1}
  if AValue then
    FQuery.Params.ByNameAsString[UpperCase(AName)] := 'T'
  else
    FQuery.Params.ByNameAsString[UpperCase(AName)] := 'F';
{$ELSE}
  {$IFDEF BOOLEAN_NUM_1}
    if AValue then
      FQuery.Params.ByNameAsInt64[UpperCase(AName)] := 1
    else
      FQuery.Params.ByNameAsInt64[UpperCase(AName)] := 0;
  {$ELSE}
    if AValue then
      FQuery.Params.ByNameAsString[UpperCase(AName)] := 'TRUE'
    else
      FQuery.Params.ByNameAsString[UpperCase(AName)] := 'FALSE';
  {$ENDIF}
{$ENDIF} // BOOLEAN_CHAR_1
end;

procedure TtiQueryUIBAbs.SetParamAsDateTime(const AName : string; const AValue :
  TDateTime);
begin
  InternalPrepare;
  FQuery.Params.ByNameAsDateTime[UpperCase(AName)] := AValue;
end;

procedure TtiQueryUIBAbs.SetParamAsFloat(const AName : string; const AValue : extended);
begin
  InternalPrepare;
  FQuery.Params.ByNameAsDouble[UpperCase(AName)] := AValue;
end;

procedure TtiQueryUIBAbs.SetParamAsInteger(const AName : string; const AValue :
  Int64);
begin
  InternalPrepare;
  FQuery.Params.ByNameAsInt64[UpperCase(AName)] := AValue;
end;

procedure TtiQueryUIBAbs.SetParamAsString(const AName, AValue : string);
Var
  sValue : string;
  iIndex : Integer;
begin
  InternalPrepare;
  iIndex := FQuery.Params.GetFieldIndex(UpperCase(AName));
  If FQuery.Params.SQLType[iIndex] = SQL_BLOB Then
  begin
    sValue := AValue;
    FQuery.ParamsSetBlob(iIndex, sValue);
  end
  else
  begin
    FQuery.Params.ByNameAsString[UpperCase(AName)] := AValue;
  end;
end;

procedure TtiQueryUIBAbs.SetParamAsTextBLOB(const AName, AValue : string);
Var
  sValue : string;
begin
  InternalPrepare;
  sValue := AValue;
  FQuery.ParamsSetBlob(UpperCase(AName), sValue);
end;

procedure TtiQueryUIBAbs.SetSQL(const AValue : TStrings);
begin
  FQuery.SQL.Assign(AValue);
end;

procedure TtiQueryUIBAbs.InternalPrepare;
Var
  lStHandle : ISCStmtHandle;
begin
  If Not FWasPrepared Then
  begin
    FSQLParams.Clear;
    FSQLParams.Parse(FQuery.SQL.Text);
    FQuery.Prepare;
    lStHandle := FQuery.StHandle;
    FQuery.DataBase.Lib.DSQLDescribeBind(lStHandle, FQuery.DataBase.SqlDialect, FSQLParams);
    FWasPrepared := True;
  end;
end;

procedure TtiQueryUIBAbs.AssignParamFromStream(const AName : string; const AStream : TStream);
begin
  Assert(AStream <> Nil, 'Stream not assigned');
  InternalPrepare;
  AStream.Position := 0;
  FQuery.ParamsSetBlob(UpperCase(AName), AStream);
end;

procedure TtiQueryUIBAbs.AssignParamToStream(const AName : string; const AStream : TStream);
begin
  Assert(AStream <> Nil, 'Stream not assigned');
  FQuery.ParamsSetBlob(AName, AStream);
//  Assert(false, 'not realized');
end;

procedure TtiQueryUIBAbs.AssignFieldAsStream(const AName : string; const AStream : TStream);
Var
  iIndex : integer;
begin
  Assert(AStream <> Nil, 'Stream not assigned');
  AStream.Position := 0;
  iIndex := FQuery.Fields.GetFieldIndex(UpperCase(AName));
  If FQuery.Fields.IsBlob[iIndex] Then
  begin
    FQuery.ReadBlob(UpperCase(AName), AStream);
  end
  else
  begin
    Raise Exception.CreateFmt('Field is not a BLOB error in method AssignFieldAsStream of class %s', [ClassName]);
//    tiFmtException('Field is not a BLOB', Classname, 'AssignFieldAsStream');
  end;
end;

procedure TtiQueryUIBAbs.AttachDatabase(ADatabase : TtiDatabase);
begin
  Inherited AttachDatabase(ADatabase);
  FQuery.Database := TtiDatabaseUIBAbs(ADatabase).UIBDatabase;
  FQuery.Transaction := FQuery.Database.Transactions[0];
end;

procedure TtiQueryUIBAbs.DetachDatabase;
begin
  Inherited DetachDatabase;
  If FActive Then
  begin
    FQuery.Close;
    FActive := False;
  end;
  FQuery.Transaction := Nil;
  FQuery.Database := Nil;
end;

function TtiQueryUIBAbs.FieldCount : Integer;
begin
  {  if FQuery.EOF
    then result := 0
    else }result := FQuery.Fields.FieldCount;
end;

function TtiQueryUIBAbs.FieldName(AIndex : integer) : string;
begin
  Result := FQuery.Fields.AliasName[AIndex];
end;

procedure TtiQueryUIBAbs.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
end;

function TtiQueryUIBAbs.FieldIndex(const AName : string) : Integer;
begin
  result := FQuery.Fields.GetFieldIndex(UpperCase(AName));
end;

function TtiQueryUIBAbs.FieldKind(AIndex : integer) : TtiQueryFieldKind;
Var
  lValue : string;
begin
  FQuery.Fields.CheckRange(AIndex);
  Result := IBFieldKindToTiFieldKind(FQuery.Fields.Data.SQLVar[AIndex]);
  If (result = qfkString) Then
  begin
    lValue := FQuery.Fields.AsString[AIndex];
    // ToDo: How to detect a logical field in a SQL database
    //       where logicals are represented as VarChar or Char?
    //       In the IBX layer we are currently looking for a value of
    //       'TRUE' or 'FALSE', but this is not reliable as it will not
    //       detect a null. Also, other ways of representing booleans
    //       might be used like 'T', 'F', '0', '1', etc...
{$IFDEF BOOLEAN_CHAR_1}
    If SameText(lValue, 'T') Or
      SameText(lValue, 'F') Then
      result := qfkLogical;
{$ELSE}
    If SameText(lValue, 'TRUE') Or
      SameText(lValue, 'TRUE ') Or
      SameText(lValue, 'FALSE') Then
{$ENDIF} // BOOLEAN_CHAR_1
  end;
end;


(*function TtiQueryUIBAbs.GetFieldAsVariant(const AName : string) : Variant;
begin
//  assert(false, Format('No longer in use %s.GetFieldAsVariant', [self.ClassName]));
  try
    Case FieldKind(FieldIndex(AName)) Of
  //      qfkBinary : result := FIBSQL.FieldByName(UpperCase(AName)).AsString;
      qfkLongString : result := FQuery.Fields.ByNameAsString[UpperCase(AName)];
      qfkInteger : result := FQuery.Fields.ByNameAsInteger[UpperCase(AName)];
    else
      result := FQuery.Fields.ByNameAsVariant[UpperCase(AName)];
    end;
  Except
    On e : exception Do
      tiFmtException(e,
        ClassName,
        'GetFieldAsVariant',
        'Error reading field <' + AName + '>');
  end;
end;
  *)

(*function TtiQueryUIBAbs.GetParamAsVariant(const AName : string) : Variant;
begin
//  assert(false, Format('No longer in use %s.GetParamAsVariant', [self.ClassName]));
  Result := FQuery.Params.ByNameAsVariant[UpperCase(AName)];
end;
  *)
function TtiQueryUIBAbs.IBFieldKindToTIFieldKind(pData : TUIBSQLVar) : TtiQueryFieldKind;
begin
  Case (pData.SqlType And (Not 1)) Of
    SQL_TEXT, SQL_VARYING : result := qfkString;
    SQL_LONG, SQL_SHORT, SQL_INT64, SQL_QUAD : result := qfkInteger;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT : result := qfkFloat;
    SQL_BLOB : Case pData.SqlSubType Of
        isc_blob_untyped : result := qfkBinary;
        isc_blob_text : result := qfkLongString;
      else
        result := qfkString; // Just to shup up the compiler
        raise EtiOPFInternalException.Create('Invalid Interbase/FireBird/IBX sqlsubtype');
      end;
    SQL_TIMESTAMP, SQL_TYPE_TIME, SQL_TYPE_DATE : result := qfkDateTime;
  else
    result := qfkString; // Just to shup up the compiler
    Raise EtiOPFInternalException.Create('Invalid Interbase/Firebird/IBX sqltype');
  end;
end;

function TtiQueryUIBAbs.FieldSize(AIndex : integer) : Integer;
begin
  If FieldKind(AIndex) In [qfkInteger, qfkFloat, qfkDateTime,
    qfkLogical, qfkLongString] Then
    result := 0
  else
    result := FQuery.Fields.SqlLen[AIndex];
end;

function TtiQueryUIBAbs.GetParamIsNull(const AName : string) : boolean;
begin
  result := FQuery.Params.ByNameIsNull[UpperCase(AName)];
end;

procedure TtiQueryUIBAbs.SetParamIsNull(const AName : string; const AValue : boolean);
begin
  FQuery.Params.ByNameIsNull[UpperCase(AName)] := AValue;
end;

function TtiQueryUIBAbs.GetFieldIsNull(const AName : string) : boolean;
begin
  result := FQuery.Fields.ByNameIsNull[UpperCase(AName)];
end;

function TtiQueryUIBAbs.HasNativeLogicalType : boolean;
begin
  Result := False;
end;

function TtiQueryUIBAbs.ParamsAsStringList: TStringList;
var
  i: integer;
  s: string;
begin
  result := TStringList.Create;
  try
    for i := 0 to ParamCount-1 do
    begin
      if ParamIsNull[ ParamName(i)] then      // Display the fact
        s := 'Null'
      else
        s := ParamAsString[ParamName(i)];
      result.Add(ParamName(i) + '=' + s);
    end;
  except
    on e: exception do
      LogError(e, true);
  end;
end;

procedure TtiQueryUIBAbs.LogParams;
const
  cLogLine = '%s: [Param %d] %s = %s';
var
  sl: TStringList;
  i: integer;
begin
  sl := ParamsAsStringList;
  try
    for i := 0 to sl.Count-1 do
      Log(Format(cLogLine, [ClassName, i+1, sl.Names[i], sl.ValueFromIndex[i]]), lsSQL);
  finally
    sl.Free;
  end;
end;

(*procedure TtiQueryUIBAbs.SetParamAsVariant(const AName : string; const Value :
  Variant);
Var
  lFieldKind : TtiQueryFieldKind;
  lValue : Variant;
  lsValue : string;
  iIndex : Word;
begin
  InternalPrepare;
  lValue := Value;
  try
    If VarIsNull(lValue) Then
    begin
      FQuery.Params.ByNameIsNull[UpperCase(AName)] := true;
    end
    else
    begin
      iIndex := FSQLParams.GetFieldIndex(UpperCase(AName));
      FSQLParams.CheckRange(iIndex);
      lFieldKind := IBFieldKindToTIFieldKind(FSQLParams.Data.SQLVar[iIndex]);
      Case lFieldKind Of
        qfkDateTime : FQuery.Params.ByNameAsDateTime[UpperCase(AName)] := lValue;
        qfkFloat : FQuery.Params.ByNameAsDouble[UpperCase(AName)] := lValue;
        qfkInteger : FQuery.Params.ByNameAsInteger[UpperCase(AName)] := lValue;
        qfkString :
          begin
            lsValue := VarToStr(lValue);
              // VarToStr on a boolean will return an integer so we must
              // check for booleans and convert to a string.
            If tiIsVariantOfType(Value, varBoolean) Then
            begin
{$IFDEF BOOLEAN_CHAR_1}
              If Value Then
                FQuery.Params.ByNameAsString[UpperCase(AName)] := 'T'
              else
                FQuery.Params.ByNameAsString[UpperCase(AName)] := 'F';
{$ELSE}
              If Value Then
                FQuery.Params.ByNameAsString[UpperCase(AName)] := 'TRUE'
              else
                FQuery.Params.ByNameAsString[UpperCase(AName)] := 'FALSE';
{$ENDIF} // BOOLEAN_CHAR_1
            end
            else
              FQuery.Params.ByNameAsString[UpperCase(AName)] := lsValue;
          end;
      else
        tiFmtException('Unsupported variant field type', ClassName, 'SetParamAsVariant');
      end
    end;
  Except
    On e : exception Do
      tiFmtException(e, ClassName, 'SetParamAsVariant', 'ParameterName: ' + AName);
  end;
end;
  *)
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseUIB
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{
******************************** TtiDatabaseUIB ********************************
}

constructor TtiDatabaseUIBAbs.Create;
begin
  Inherited Create;
  FDatabase := TUIBDatabase.Create(Nil);
  FDatabase.SQLDialect := 3;
  FTransaction := TUIBTransaction.Create(Nil);
  FTransaction.Database := FDatabase;
  FTransaction.AutoStart := False;
  FTransaction.AutoStop := False;
end;

destructor TtiDatabaseUIBAbs.Destroy;
begin
  try
    FDatabase.Connected := false;
    FTransaction.Database := Nil;
    FTransaction.Free;
    FDatabase.Free;
  Except
    On e : exception Do
      LogError(e.message);
  end;
  Inherited;
end;

procedure TtiDatabaseUIBAbs.Commit;
begin
  If Not InTransaction Then
    Raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  Log(ClassName + ': [Commit Trans]', lsSQL);
  FTransaction.Commit;
end;

function TtiDatabaseUIBAbs.InTransaction : boolean;
begin
  result := FTransaction.InTransaction;
end;

procedure TtiDatabaseUIBAbs.RollBack;
begin
  Log(ClassName + ': [RollBack Trans]', lsSQL);
  FTransaction.RollBack;
end;

procedure TtiDatabaseUIBAbs.StartTransaction;
begin
  If InTransaction Then
    Raise EtiOPFInternalException.Create('Attempt to start a transaction but transaction already exists.');
  Log(ClassName + ': [Start Trans]', lsSQL);
  FTransaction.StartTransaction;
end;

function TtiDatabaseUIBAbs.GetConnected : boolean;
begin
  Result := FDatabase.Connected;
end;

procedure TtiDatabaseUIBAbs.SetConnected(AValue : boolean);
Var
  lMessage : string;
  lCounter: integer;
begin

  try
    If (not AValue) Then
    begin
      Log('Disconnecting from %s', [DatabaseName]);
      FDatabase.Connected := false;
      Exit; //==>
    end;

    FDatabase.DatabaseName := DatabaseName;
    FDatabase.Params.Values['user_name'] := UserName;
    FDatabase.Params.Values['password'] := Password;

    for lCounter := 0 to Params.Count -1 do
      begin
        FDatabase.Params.Values[Params.Names[lCounter]] :=
          Params.ValueFromIndex[lCounter];
      end;

     FDatabase.Connected := True;

  Except
      // ToDo: Must come up with a better solution that this:
      //       try several times before raising an exception.
      //       Rather than calling 'Halt', just terminate this database connection,
      //       unless this is the first connection.
    On e : EUIBError Do
    begin
        // Invalid username / password error
      If (EUIBError(E).ErrorCode = 335544472) Then
        Raise EtiOPFDBExceptionUserNamePassword.Create(FLayerName, DatabaseName, UserName, Password)
      else
      begin
        lMessage :=
          'Error attempting to connect to database.' + Cr +
          e.Message + Cr +
          'Error code: ' + IntToStr(EUIBError(E).ErrorCode);
        Raise EtiOPFDBExceptionUserNamePassword.Create(FLayerName, DatabaseName, UserName, Password, lMessage)
      end;
    end;
    On E : Exception Do
      Raise EtiOPFDBExceptionUserNamePassword.Create(FLayerName, DatabaseName, UserName, Password, E.Message)
  end;
end;

procedure TtiDatabaseUIBAbs.ReadMetaDataTables(AData : TtiDBMetaData);
Var
  lQuery : TtiQuery;
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      lQuery.SQLText :=
        'SELECT RDB$RELATION_NAME as Table_Name ' +
        '  FROM RDB$RELATIONS ' +
        'WHERE ((RDB$SYSTEM_FLAG = 0) OR (RDB$SYSTEM_FLAG IS NULL)) ' +
        'ORDER BY RDB$RELATION_NAME ';
      lQuery.Open;
      While Not lQuery.EOF Do
      begin
        lTable := TtiDBMetaDataTable.Create;
        lTable.Name := Trim(lQuery.FieldAsString['table_name']);
        lTable.ObjectState := posPK;
        lMetaData.Add(lTable);
        lQuery.Next;
      end;
      lQuery.DetachDatabase;
      lMetaData.ObjectState := posClean;
    Finally
      Commit;
    end;
  Finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseUIBAbs.ReadMetaDataFields(AData : TtiDBMetaDataTable);
Var
  lTableName : string;
  lQuery : TtiQuery;
  lTable : TtiDBMetaDataTable;
  lField : TtiDBMetaDataField;
  lFieldType : integer;
  lFieldLength : integer;

const
  // Interbase field types
  //   select * from rdb$types
  //   where rdb$field_NAME = 'RDB$FIELD_TYPE'
  //   ORDER BY RDB$TYPE

  cIBField_SHORT = 7;
  cIBField_LONG = 8;
  cIBField_QUAD = 9;
  cIBField_FLOAT = 10;
  cIBField_DATE = 12;
  cIBField_TIME = 13;
  cIBField_TEXT = 14;
  cIBField_BIGINT = 16;
  cIBField_DOUBLE = 27;
  cIBField_TIMESTAMP = 35;
  cIBField_VARYING = 37;
  cIBField_CSTRING = 40;
  cIBField_BLOB_ID = 45;
  cIBField_BLOB = 261;


begin
  lTable := (AData As TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      lQuery.SQL.Text :=
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
      While Not lQuery.EOF Do
      begin
        lField := TtiDBMetaDataField.Create;
        lField.Name := Trim(lQuery.FieldAsString['field_name']);
        lFieldType := lQuery.FieldAsInteger['field_type'];
        lFieldLength := lQuery.FieldAsInteger['field_length'];

        lField.Width := 0;

        Case lFieldType Of
          cIBField_SHORT,
          cIBField_LONG,
          cIBField_BIGINT : lField.Kind := qfkInteger;

          cIBField_DOUBLE,
          cIBField_FLOAT : lField.Kind := qfkFloat;

          cIBField_TIMESTAMP,
          cIBField_DATE,
          cIBField_TIME : lField.Kind := qfkDateTime;

          cIBField_VARYING,
          cIBField_TEXT :
            begin
              lField.Kind := qfkString;
              lField.Width := lFieldLength;
            end;

          cIBField_BLOB :
            begin
              Assert(Not lQuery.FieldIsNull['field_sub_type'], 'field_sub_type is null');
              If lQuery.FieldAsInteger['field_sub_type'] = 1 Then
                lField.Kind := qfkLongString
              else if lQuery.FieldAsInteger['field_sub_type'] = 0 then
                lField.Kind := qfkBinary
              else
                Raise EtiOPFInternalException.Create('Invalid field_sub_type <' + IntToStr(lQuery.FieldAsInteger['field_sub_type']) + '>');
            end;
        else
          Raise EtiOPFInternalException.Create('Invalid field_sub_type <' + IntToStr(lFieldType) + '>');
        end;
        lField.ObjectState := posClean;
        lTable.Add(lField);
        lQuery.Next;
      end;
    Finally
      Commit;
    end;
    lQuery.DetachDatabase;
    lTable.ObjectState := posClean;
  Finally
    lQuery.Free;
  end;

  // This SQL was found somewhere on the web...
  //      lQuery.SQLText :=
  //        'select r.rdb$field_name as Column_Name,  ' +
  //        '	t.rdb$type_name, ' +
  //        '	f.rdb$field_length ' +
  //        ' ' +
  //        'from 	rdb$relation_fields r, rdb$types t, rdb$fields f  ' +
  //        ' ' +
  //        'where 	r.rdb$relation_name=''' + lTable.Name + ''' and  ' +
  //        '	f.rdb$field_name=r.rdb$field_source and  ' +
  //        '	t.rdb$field_name=''RDB$FIELD_TYPE'' and  ' +
  //        '	f.rdb$field_type=t.rdb$type ';

end;

function TtiDatabaseUIBAbs.FieldMetaDataToSQLCreate(const AFieldMetaData : TtiDBMetaDataField) : string;
Var
  lFieldName : string;
begin
  lFieldName := AFieldMetaData.Name;
  Case AFieldMetaData.Kind Of
    qfkString : result := 'VarChar( ' + IntToStr(AFieldMetaData.Width) + ' )';
    qfkInteger : result := 'Integer';
  //    qfkFloat: result := 'Decimal( 10, 5 )';
    qfkFloat : result := 'DOUBLE PRECISION';
      // Just for new version of IB (6.x)
      // DATE holds only DATE without TIME...
    qfkDateTime : If UIBDatabase.SQLDialect <> 1 Then
        result := 'TIMESTAMP'
      else
        result := 'Date';
{$IFDEF BOOLEAN_CHAR_1}
    qfkLogical : result := 'Char( 1 ) default ''F'' check( ' + lFieldName + ' in ( ''T'', ''F'' ))';
{$ELSE}
    qfkLogical : result := 'VarChar( 5 ) default ''FALSE'' check( ' + lFieldName + ' in ( ''TRUE'', ''FALSE'' )) ';
{$ENDIF}
    qfkBinary : result := 'Blob sub_type 0';
    qfkLongString : result := 'Blob sub_type 1';
  else
    Raise EtiOPFInternalException.Create('Invalid field kind.');
  end;
end;

function TtiDatabaseUIBAbs.Test : boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;


{ EtiOPFDBExceptionUserNamePassword }

constructor EtiOPFDBExceptionUserNamePassword.Create(const APerLayerName, ADatabaseName, AUserName, APassword, AMessage : string);
begin
  Inherited Create(APerLayerName, ADatabaseName, AUserName, APassword, AMessage);
  Message := 'Cannot connect to database. ' + CR + Message;
end;

{ EtiOPFDBException }

constructor EtiOPFDBException.Create(const APerLayerName, ADatabaseName, AUserName, APassword, AMessage : string);
begin
  Message :=
    'Database name:       ' + ADatabaseName + Cr +
    'User name:           ' + AUserName + Cr +
    'Password:            ' + tiReplicate('X', Length(APassword)) + Cr +
    'Persistence layer:   ' + APerLayerName;
  If AMessage <> '' Then
    Message := Message + Cr(2) +
      'Message:' + Cr + AMessage;
end;

end.

