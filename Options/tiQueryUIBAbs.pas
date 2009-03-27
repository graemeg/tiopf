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
    Constructor Create(Const pPerLayerName, pDatabaseName, pUserName, pPassword : String; Const pMessage : String = ''); Virtual;
  End;

  EtiOPFDBExceptionUserNamePassword = Class(EtiOPFDBException)
    Constructor Create(Const pPerLayerName, pDatabaseName, pUserName, pPassword : String; Const pMessage : String = ''); Override;
  End;

  // ---------------------------------------------------------------------------
  TtiDatabaseUIBAbs = Class(TtiDatabaseSQL)
  Private
    FDatabase : TUIBDatabase;
    FTransaction : TUIBTransaction;
    FLayerName : String;
  Protected
    Function FieldMetaDataToSQLCreate(Const pFieldMetaData :
      TtiDBMetaDataField) : String; Override;
    Function GetConnected : Boolean; Override;
    Procedure SetConnected(pbValue : boolean); Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure Commit; Override;
    Function InTransaction : Boolean; Override;
    Procedure ReadMetaDataFields(AData : TtiDBMetaDataTable); Override;
    Procedure ReadMetaDataTables(AData : TtiDBMetaData); Override;
    Procedure RollBack; Override;
    Procedure StartTransaction; Override;
    Function Test : Boolean; Override;
    Property UIBDatabase : TUIBDatabase Read FDatabase Write FDatabase;
    Property LayerName : String Read FLayerName Write FLayerName;
  End;

  // ---------------------------------------------------------------------------
  TtiQueryUIBAbs = Class(TtiQuerySQL)
  Private
    FActive : Boolean;
    FWasPrepared : Boolean;
    FQuery : TUIBQuery;
    FSQLParams : TSQLParams;
    Function IBFieldKindToTIFieldKind(pData : TUIBSQLVar) : TtiQueryFieldKind;
  Protected
    Function GetActive : Boolean; Override;
    Function GetEOF : Boolean; Override;
    Function GetFieldAsBoolean(Const psName : String) : Boolean; Override;
    Function GetFieldAsDateTime(Const psName : String) : TDateTime; Override;
    Function GetFieldAsFloat(Const psName : String) : Extended; Override;
    Function GetFieldAsInteger(Const psName : String) : Int64; Override;
    Function GetFieldAsString(Const psName : String) : String; Override;
 //   Function GetFieldAsVariant(Const psName : String) : Variant; Override;
    Function GetFieldIsNull(Const psName : String) : Boolean; Override;
    Function GetParamAsBoolean(Const psName : String) : Boolean; Override;
    Function GetParamAsDateTime(Const psName : String) : TDateTime; Override;
    Function GetParamAsFloat(Const psName : String) : Extended; Override;
    Function GetParamAsInteger(Const psName : String) : Int64; Override;
    Function GetParamAsString(Const psName : String) : String; Override;
    Function GetParamAsTextBLOB(Const psName : String) : String; Override;
//    Function GetParamAsVariant(Const psName : String) : Variant; Override;
    Function GetParamIsNull(Const psName : String) : Boolean; Override;
    Function GetSQL : TStrings; Override;
    Procedure InternalPrepare; Virtual;
    Procedure SetActive(Const Value : boolean); Override;
    Procedure SetParamAsBoolean(Const psName : String; Const Value : boolean);
      Override;
    Procedure SetParamAsDateTime(Const psName : String; Const Value : TDateTime);
      Override;
    Procedure SetParamAsFloat(Const psName : String; Const Value : Extended);
      Override;
    Procedure SetParamAsInteger(Const psName : String; Const Value : Int64);
      Override;
    Procedure SetParamAsString(Const psName, Value : String); Override;
    Procedure SetParamAsTextBLOB(Const psName, Value : String); Override;
//    Procedure SetParamAsVariant(Const psName : String; Const Value : Variant); Override;
    Procedure SetParamIsNull(Const psName : String; Const Value : Boolean); Override;
    Procedure SetSQL(Const Value : TStrings); Override;
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure AssignFieldAsStream(Const pName : String; Const pStream : TStream); Override;
    Procedure AssignParamFromStream(Const pName : String; Const pStream : TStream); Override;
    Procedure AssignParams(Const AParams : TtiQueryParams; Const AWhere : TtiQueryParams = Nil); Override;
    Procedure AssignParamToStream(Const pName : String; Const pStream : TStream); Override;
    Procedure AttachDatabase(pDatabase : TtiDatabase); Override;
    Procedure Close; Override;
    Procedure DetachDatabase; Override;
    function ExecSQL : Integer; Override;
    Function FieldCount : Integer; Override;
    Function FieldIndex(Const psName : String) : Integer; Override;
    Function FieldKind(pIndex : integer) : TtiQueryFieldKind; Override;
    Function FieldName(pIndex : integer) : String; Override;
    Function FieldSize(pIndex : integer) : Integer; Override;
    Function HasNativeLogicalType : Boolean; Override;
    Procedure Next; Override;
    Procedure Open; Override;
    Function ParamCount : Integer; Override;
    Function ParamName(pIndex : integer) : String; Override;
    Procedure Reset; Override;
  End;

Implementation

Uses
  DB
  , Math
  , tiUtils
  , tiDialogs
  , tiLog
  , TypInfo
  , tiOPFManager
  , tiExcept
  , tiConstants
{$IFDEF DELPHI6ORABOVE}
  , Variants
{$ENDIF}
  ;

Type
  TSQLType = Record
    SqlType : Integer;
    TypeName : String;
  End;

  TSQLTypes = Array[0..13] Of TSQLType;

  TPrivTypes = Record
    PrivFlag : Integer;
    PrivString : String;
  End;

Const
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

  SubTypes : Array[0..8] Of String = (
    'UNKNOWN', {do not localize}
    'TEXT', {do not localize}
    'BLR', {do not localize}
    'ACL', {do not localize}
    'RANGES', {do not localize}
    'SUMMARY', {do not localize}
    'FORMAT', {do not localize}
    'TRANSACTION_DESCRIPTION', {do not localize}
    'EXTERNAL_FILE_DESCRIPTION'); {do not localize}

  TriggerTypes : Array[0..6] Of String = (
    '', {do not localize}
    'BEFORE INSERT', {do not localize}
    'AFTER INSERT', {do not localize}
    'BEFORE UPDATE', {do not localize}
    'AFTER UPDATE', {do not localize}
    'BEFORE DELETE', {do not localize}
    'AFTER DELETE'); {do not localize}

  IntegralSubtypes : Array[0..2] Of String = (
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

Constructor TtiQueryUIBAbs.Create;
Begin
  Inherited;
  FSQLParams := TSQLParams.Create(csUTF8);
  FWasPrepared := false;
  FQuery := TUIBQuery.Create(Nil);
  With FQuery Do
  Begin
    FetchBlobs := True;
    OnError := etmStayIn;
  End;
  FActive := false;
End;

Destructor TtiQueryUIBAbs.Destroy;
Begin
  FQuery.Free;
  FSQLParams.Free;
  Inherited;
End;

Procedure TtiQueryUIBAbs.AssignFieldAsStream(Const pName : String; Const pStream :
  TStream);
Var
  iIndex : integer;
Begin
  Assert(pStream <> Nil, 'Stream not assigned');
  pStream.Position := 0;
  iIndex := FQuery.Fields.GetFieldIndex(UpperCase(pName));
  If FQuery.Fields.IsBlob[iIndex] Then
  Begin
    FQuery.ReadBlob(UpperCase(pName), pStream);
  End
  Else
  Begin
    Raise Exception.CreateFmt('Field is not a BLOB error in method AssignFieldAsStream of class %s', [ClassName]);
//    tiFmtException('Field is not a BLOB', Classname, 'AssignFieldAsStream');
  End;
End;

Procedure TtiQueryUIBAbs.AssignParamFromStream(Const pName : String; Const pStream :
  TStream);
Begin
  Assert(pStream <> Nil, 'Stream not assigned');
  InternalPrepare;
  pStream.Position := 0;
  FQuery.ParamsSetBlob(UpperCase(pName), pStream);
End;

Procedure TtiQueryUIBAbs.AssignParams(Const AParams : TtiQueryParams; Const AWhere : TtiQueryParams = Nil);
Begin
  If AParams = Nil Then
    Exit;
  Inherited;
End;

Procedure TtiQueryUIBAbs.AssignParamToStream(Const pName : String; Const pStream : TStream);
Begin
  Assert(pStream <> Nil, 'Stream not assigned');
  FQuery.ParamsSetBlob(pName, pStream);
//  Assert(false, 'not realized');
End;

Procedure TtiQueryUIBAbs.AttachDatabase(pDatabase : TtiDatabase);
Begin
  Inherited AttachDatabase(pDatabase);
  With FQuery Do
  Begin
    Database := TtiDatabaseUIBAbs(pDatabase).UIBDatabase;
    Transaction := Database.Transactions[0];
  End;
End;

Procedure TtiQueryUIBAbs.Close;
Begin
  Active := false;
End;

Procedure TtiQueryUIBAbs.DetachDatabase;
Begin
  Inherited DetachDatabase;
  If FActive Then
  Begin
    FQuery.Close;
    FActive := False;
  End;
  FQuery.Transaction := Nil;
  FQuery.Database := Nil;
End;

Function TtiQueryUIBAbs.ExecSQL : Integer;
Begin
  Log(ClassName + ': [Prepare] ' + tiNormalizeStr(self.SQLText), lsSQL);
  InternalPrepare;
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);
  FQuery.Execute;
  Result := -1;
End;

Function TtiQueryUIBAbs.FieldCount : Integer;
Begin
  {  if FQuery.EOF
    then result := 0
    else }result := FQuery.Fields.FieldCount;
End;

Function TtiQueryUIBAbs.FieldIndex(Const psName : String) : Integer;
Begin
  result := FQuery.Fields.GetFieldIndex(UpperCase(psName));
End;

Function TtiQueryUIBAbs.FieldKind(pIndex : integer) : TtiQueryFieldKind;
Var
  lValue : String;
Begin
  FQuery.Fields.CheckRange(pIndex);
  Result := IBFieldKindToTiFieldKind(FQuery.Fields.Data.SQLVar[pIndex]);
  If (result = qfkString) Then
  Begin
    lValue := FQuery.Fields.AsString[pIndex];
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
  End;
End;

Function TtiQueryUIBAbs.FieldName(pIndex : integer) : String;
Begin
  Result := FQuery.Fields.AliasName[pIndex];
End;

Function TtiQueryUIBAbs.FieldSize(pIndex : integer) : Integer;
Begin
  If FieldKind(pIndex) In [qfkInteger, qfkFloat, qfkDateTime,
    qfkLogical, qfkLongString] Then
    result := 0
  Else
    result := FQuery.Fields.SqlLen[pIndex];
End;

Function TtiQueryUIBAbs.GetActive : Boolean;
Begin
  result := FActive;
End;

Function TtiQueryUIBAbs.GetEOF : Boolean;
Begin
  result := FQuery.EOF;
End;

Function TtiQueryUIBAbs.GetFieldAsBoolean(Const psName : String) : Boolean;
Begin
  Result := FQuery.Fields.ByNameAsBoolean[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetFieldAsDateTime(Const psName : String) : TDateTime;
Begin
  result := FQuery.Fields.ByNameAsDateTime[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetFieldAsFloat(Const psName : String) : Extended;
Begin
  result := FQuery.Fields.ByNameAsDouble[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetFieldAsInteger(Const psName : String) : Int64;
Begin
  Result := FQuery.Fields.ByNameAsInt64[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetFieldAsString(Const psName : String) : String;
Begin
  result := FQuery.Fields.ByNameAsString[UpperCase(psName)];
End;

(*Function TtiQueryUIBAbs.GetFieldAsVariant(Const psName : String) : Variant;
Begin
//  assert(false, Format('No longer in use %s.GetFieldAsVariant', [self.ClassName]));
  Try
    Case FieldKind(FieldIndex(psName)) Of
  //      qfkBinary : result := FIBSQL.FieldByName(UpperCase(psName)).AsString;
      qfkLongString : result := FQuery.Fields.ByNameAsString[UpperCase(psName)];
      qfkInteger : result := FQuery.Fields.ByNameAsInteger[UpperCase(psName)];
    Else
      result := FQuery.Fields.ByNameAsVariant[UpperCase(psName)];
    End;
  Except
    On e : exception Do
      tiFmtException(e,
        ClassName,
        'GetFieldAsVariant',
        'Error reading field <' + psName + '>');
  End;
End;
  *)
Function TtiQueryUIBAbs.GetFieldIsNull(Const psName : String) : Boolean;
Begin
  result := FQuery.Fields.ByNameIsNull[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetParamAsBoolean(Const psName : String) : Boolean;
Var
  lValue : String;
Begin
  lValue := FQuery.Params.ByNameAsString[UpperCase(psName)];
{$IFDEF BOOLEAN_CHAR_1}
  result := SameText(lValue, 'T');
{$ELSE}
  result := SameText(lValue, 'TRUE');
{$ENDIF} // BOOLEAN_CHAR_1
End;

Function TtiQueryUIBAbs.GetParamAsDateTime(Const psName : String) : TDateTime;
Begin
  result := FQuery.Params.ByNameAsDateTime[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetParamAsFloat(Const psName : String) : Extended;
Begin
  result := FQuery.Params.ByNameAsDouble[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetParamAsInteger(Const psName : String) : Int64;
Begin
  result := FQuery.Params.ByNameAsInt64[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetParamAsString(Const psName : String) : String;
Begin
  if FQuery.Params.ByNameIsBlob[UpperCase(psName)] then
    result := '[BlobData]'
  else
    Result := FQuery.Params.ByNameAsString[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetParamAsTextBLOB(Const psName : String) : String;
Begin
  Result := FQuery.Params.ByNameAsString[UpperCase(psName)];
//  Result := FQuery.Params.
 // Result := FQuery.Params.ByNameAsWideString[UpperCase(psName)];
End;

(*Function TtiQueryUIBAbs.GetParamAsVariant(Const psName : String) : Variant;
Begin
//  assert(false, Format('No longer in use %s.GetParamAsVariant', [self.ClassName]));
  Result := FQuery.Params.ByNameAsVariant[UpperCase(psName)];
End;
  *)
Function TtiQueryUIBAbs.GetParamIsNull(Const psName : String) : Boolean;
Begin
  result := FQuery.Params.ByNameIsNull[UpperCase(psName)];
End;

Function TtiQueryUIBAbs.GetSQL : TStrings;
Begin
  result := FQuery.SQL;
End;

Function TtiQueryUIBAbs.HasNativeLogicalType : Boolean;
Begin
  Result := False;
End;

Function TtiQueryUIBAbs.IBFieldKindToTIFieldKind(pData : TUIBSQLVar) :
  TtiQueryFieldKind;
Begin
  Case (pData.SqlType And (Not 1)) Of
    SQL_TEXT, SQL_VARYING : result := qfkString;
    SQL_LONG, SQL_SHORT, SQL_INT64, SQL_QUAD : result := qfkInteger;
    SQL_DOUBLE, SQL_FLOAT, SQL_D_FLOAT : result := qfkFloat;
    SQL_BLOB : Case pData.SqlSubType Of
        isc_blob_untyped : result := qfkBinary;
        isc_blob_text : result := qfkLongString;
      Else
        result := qfkString; // Just to shup up the compiler
        raise EtiOPFInternalException.Create('Invalid Interbase/FireBird/IBX sqlsubtype');
      End;
    SQL_TIMESTAMP, SQL_TYPE_TIME, SQL_TYPE_DATE : result := qfkDateTime;
  Else
    result := qfkString; // Just to shup up the compiler
    Raise EtiOPFInternalException.Create('Invalid Interbase/Firebird/IBX sqltype');
  End;
End;

Procedure TtiQueryUIBAbs.Next;
Begin
  FQuery.Next;
End;

Procedure TtiQueryUIBAbs.Open;
Begin
  Active := true;
End;

Function TtiQueryUIBAbs.ParamCount : Integer;
Begin
  Result := FQuery.Params.ParamCount;
End;

Function TtiQueryUIBAbs.ParamName(pIndex : integer) : String;
Begin
  result := FQuery.Params.FieldName[pIndex];
End;

Procedure TtiQueryUIBAbs.Reset;
Begin
  Active := false;
  FQuery.SQL.Clear;
End;

Procedure TtiQueryUIBAbs.SetActive(Const Value : boolean);
Begin
 // Assert(Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  If Value Then
  Begin
    Try
      FQuery.Open;
      FActive := True;
    Except
        On e : exception Do
          Raise EtiOPFInternalException.Create(E.Message);
    End;
  End
  Else
  Begin
    FQuery.Close;
    FActive := False;
    FWasPrepared := False;
  End;
End;

Procedure TtiQueryUIBAbs.SetParamAsBoolean(Const psName : String; Const Value :
  boolean);
Begin
  InternalPrepare;
{$IFDEF BOOLEAN_CHAR_1}
  if Value then
    FQuery.Params.ByNameAsString[UpperCase(psName)] := 'T'
  else
    FQuery.Params.ByNameAsString[UpperCase(psName)] := 'F';
{$ELSE}
  {$IFDEF BOOLEAN_NUM_1}
    if Value then
      FQuery.Params.ByNameAsInt64[UpperCase(psName)] := 1
    else
      FQuery.Params.ByNameAsInt64[UpperCase(psName)] := 0;
  {$ELSE}
    if Value then
      FQuery.Params.ByNameAsString[UpperCase(psName)] := 'TRUE'
    else
      FQuery.Params.ByNameAsString[UpperCase(psName)] := 'FALSE';
  {$ENDIF}
{$ENDIF} // BOOLEAN_CHAR_1
End;

Procedure TtiQueryUIBAbs.SetParamAsDateTime(Const psName : String; Const Value :
  TDateTime);
Begin
  InternalPrepare;
  FQuery.Params.ByNameAsDateTime[UpperCase(psName)] := Value;
End;

Procedure TtiQueryUIBAbs.SetParamAsFloat(Const psName : String; Const Value : Extended);
Begin
  InternalPrepare;
  FQuery.Params.ByNameAsDouble[UpperCase(psName)] := Value;
End;

Procedure TtiQueryUIBAbs.SetParamAsInteger(Const psName : String; Const Value :
  Int64);
Begin
  InternalPrepare;
  FQuery.Params.ByNameAsInt64[UpperCase(psName)] := Value;
End;

Procedure TtiQueryUIBAbs.SetParamAsString(Const psName, Value : String);
Var
  sValue : String;
  iIndex : Integer;
Begin
  InternalPrepare;
  iIndex := FQuery.Params.GetFieldIndex(UpperCase(psName));
  If FQuery.Params.SQLType[iIndex] = SQL_BLOB Then
  Begin
    sValue := Value;
    FQuery.ParamsSetBlob(iIndex, sValue);
  End
  Else
  Begin
    FQuery.Params.ByNameAsString[UpperCase(psName)] := Value;
  End;
End;

Procedure TtiQueryUIBAbs.SetParamAsTextBLOB(Const psName, Value : String);
Var
  sValue : String;
Begin
  InternalPrepare;
  sValue := Value;
  FQuery.ParamsSetBlob(UpperCase(psName), sValue);
End;

(*Procedure TtiQueryUIBAbs.SetParamAsVariant(Const psName : String; Const Value :
  Variant);
Var
  lFieldKind : TtiQueryFieldKind;
  lValue : Variant;
  lsValue : String;
  iIndex : Word;
Begin
  InternalPrepare;
  lValue := Value;
  Try
    If VarIsNull(lValue) Then
    Begin
      FQuery.Params.ByNameIsNull[UpperCase(psName)] := true;
    End
    Else
    Begin
      iIndex := FSQLParams.GetFieldIndex(UpperCase(psName));
      FSQLParams.CheckRange(iIndex);
      lFieldKind := IBFieldKindToTIFieldKind(FSQLParams.Data.SQLVar[iIndex]);
      Case lFieldKind Of
        qfkDateTime : FQuery.Params.ByNameAsDateTime[UpperCase(psName)] := lValue;
        qfkFloat : FQuery.Params.ByNameAsDouble[UpperCase(psName)] := lValue;
        qfkInteger : FQuery.Params.ByNameAsInteger[UpperCase(psName)] := lValue;
        qfkString :
          Begin
            lsValue := VarToStr(lValue);
              // VarToStr on a boolean will return an integer so we must
              // check for booleans and convert to a string.
            If tiIsVariantOfType(Value, varBoolean) Then
            Begin
{$IFDEF BOOLEAN_CHAR_1}
              If Value Then
                FQuery.Params.ByNameAsString[UpperCase(psName)] := 'T'
              Else
                FQuery.Params.ByNameAsString[UpperCase(psName)] := 'F';
{$ELSE}
              If Value Then
                FQuery.Params.ByNameAsString[UpperCase(psName)] := 'TRUE'
              Else
                FQuery.Params.ByNameAsString[UpperCase(psName)] := 'FALSE';
{$ENDIF} // BOOLEAN_CHAR_1
            End
            Else
              FQuery.Params.ByNameAsString[UpperCase(psName)] := lsValue;
          End;
      Else
        tiFmtException('Unsupported variant field type', ClassName, 'SetParamAsVariant');
      End
    End;
  Except
    On e : exception Do
      tiFmtException(e, ClassName, 'SetParamAsVariant', 'ParameterName: ' + psName);
  End;
End;
  *)
Procedure TtiQueryUIBAbs.SetParamIsNull(Const psName : String; Const Value :
  Boolean);
Begin
  FQuery.Params.ByNameIsNull[UpperCase(psName)] := Value;
End;

Procedure TtiQueryUIBAbs.SetSQL(Const Value : TStrings);
Begin
  FQuery.SQL.Assign(Value);
End;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseUIB
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{
******************************** TtiDatabaseUIB ********************************
}

Constructor TtiDatabaseUIBAbs.Create;
Begin
  Inherited Create;
  FDatabase := TUIBDatabase.Create(Nil);
  FDatabase.SQLDialect := 3;
  FTransaction := TUIBTransaction.Create(Nil);
  With FTransaction Do
  Begin
    Database := FDatabase;
    AutoStart := False;
    AutoStop := False;
    AutoRetain := False;
  End;
End;

Destructor TtiDatabaseUIBAbs.Destroy;
Begin
  Try
    FDatabase.Connected := false;
    FTransaction.Database := Nil;
    FTransaction.Free;
    FDatabase.Free;
  Except
    On e : exception Do
      LogError(e.message);
  End;
  Inherited;
End;

Procedure TtiDatabaseUIBAbs.Commit;
Begin
  If Not InTransaction Then
    Raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  FTransaction.Commit;
End;

Function TtiDatabaseUIBAbs.FieldMetaDataToSQLCreate(Const pFieldMetaData :
  TtiDBMetaDataField) : String;
Var
  lFieldName : String;
Begin
  lFieldName := pFieldMetaData.Name;
  Case pFieldMetaData.Kind Of
    qfkString : result := 'VarChar( ' + IntToStr(pFieldMetaData.Width) + ' )';
    qfkInteger : result := 'Integer';
  //    qfkFloat: result := 'Decimal( 10, 5 )';
    qfkFloat : result := 'DOUBLE PRECISION';
      // Just for new version of IB (6.x)
      // DATE holds only DATE without TIME...
    qfkDateTime : If UIBDatabase.SQLDialect <> 1 Then
        result := 'TIMESTAMP'
      Else
        result := 'Date';
{$IFDEF BOOLEAN_CHAR_1}
    qfkLogical : result := 'Char( 1 ) default ''F'' check( ' + lFieldName + ' in ( ''T'', ''F'' ))';
{$ELSE}
    qfkLogical : result := 'VarChar( 5 ) default ''FALSE'' check( ' + lFieldName + ' in ( ''TRUE'', ''FALSE'' )) ';
{$ENDIF}
    qfkBinary : result := 'Blob sub_type 0';
    qfkLongString : result := 'Blob sub_type 1';
  Else
    Raise EtiOPFInternalException.Create('Invalid field kind.');
  End;
End;

Function TtiDatabaseUIBAbs.GetConnected : Boolean;
Begin
  Result := FDatabase.Connected;
End;

Function TtiDatabaseUIBAbs.InTransaction : Boolean;
Begin
  result := FTransaction.InTransaction;
End;

Procedure TtiDatabaseUIBAbs.ReadMetaDataFields(AData : TtiDBMetaDataTable);
Var
  lTableName : String;
  lQuery : TtiQuery;
  lTable : TtiDBMetaDataTable;
  lField : TtiDBMetaDataField;
  lFieldType : integer;
  lFieldLength : integer;

Const
  // Interbase field types
  //   select * from rdb$types
  //   where rdb$field_NAME = 'RDB$FIELD_TYPE'
  //   ORDER BY RDB$TYPE

  cIBField_LONG = 8;
  cIBField_DOUBLE = 27;
  cIBField_TIMESTAMP = 35;
  cIBField_DATE = 12;
  cIBField_TIME = 13;
  cIBField_VARYING = 37;
  cIBField_BLOB = 261;

  cIBField_SHORT = 7;
  cIBField_QUAD = 9;
  cIBField_FLOAT = 10;
  cIBField_TEXT = 14;
  cIBField_CSTRING = 40;
  cIBField_BLOB_ID = 45;

Begin
  lTable := (AData As TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  Try
    StartTransaction;
    Try
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
      Begin
        lField := TtiDBMetaDataField.Create;
        lField.Name := Trim(lQuery.FieldAsString['field_name']);
        lFieldType := lQuery.FieldAsInteger['field_type'];
        lFieldLength := lQuery.FieldAsInteger['field_length'];

        lField.Width := 0;

        Case lFieldType Of
          cIBField_LONG : lField.Kind := qfkInteger;
          cIBField_DOUBLE : lField.Kind := qfkFloat;
          cIBField_TIMESTAMP,
            cIBField_DATE,
            cIBField_TIME : lField.Kind := qfkDateTime;
          cIBField_VARYING,
            cIBField_TEXT :
            Begin
              lField.Kind := qfkString;
              lField.Width := lFieldLength;
            End;
          cIBField_BLOB :
            Begin
              Assert(Not lQuery.FieldIsNull['field_sub_type'], 'field_sub_type is null');
              If lQuery.FieldAsInteger['field_sub_type'] = 1 Then
                lField.Kind := qfkLongString
              Else
                Raise EtiOPFInternalException.Create('Invalid field_sub_type <' + IntToStr(lQuery.FieldAsInteger['field_sub_type']) + '>');
            End;
        Else
          Raise EtiOPFInternalException.Create('Invalid field_sub_type <' + IntToStr(lFieldType) + '>');
        End;
        lField.ObjectState := posClean;
        lTable.Add(lField);
        lQuery.Next;
      End;
    Finally
      Commit;
    End;
    lQuery.DetachDatabase;
    lTable.ObjectState := posClean;
  Finally
    lQuery.Free;
  End;

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

End;

Procedure TtiDatabaseUIBAbs.ReadMetaDataTables(AData : TtiDBMetaData);
Var
  lQuery : TtiQuery;
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
Begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  Try
    StartTransaction;
    Try
      lQuery.AttachDatabase(Self);
      lQuery.SQLText :=
        'SELECT RDB$RELATION_NAME as Table_Name ' +
        '  FROM RDB$RELATIONS ' +
        'WHERE ((RDB$SYSTEM_FLAG = 0) OR (RDB$SYSTEM_FLAG IS NULL)) ' +
        'ORDER BY RDB$RELATION_NAME ';
      lQuery.Open;
      While Not lQuery.EOF Do
      Begin
        lTable := TtiDBMetaDataTable.Create;
        lTable.Name := Trim(lQuery.FieldAsString['table_name']);
        lTable.ObjectState := posPK;
        lMetaData.Add(lTable);
        lQuery.Next;
      End;
      lQuery.DetachDatabase;
      lMetaData.ObjectState := posClean;
    Finally
      Commit;
    End;
  Finally
    lQuery.Free;
  End;
End;

Procedure TtiDatabaseUIBAbs.RollBack;
Begin
  FTransaction.RollBack;
End;

Procedure TtiDatabaseUIBAbs.SetConnected(pbValue : boolean);
Var
  lMessage : String;
  lCounter: integer;
Begin

  Try
    If (Not pbValue) Then
    Begin
      Log('Disconnecting from %s', [DatabaseName]);
      FDatabase.Connected := false;
      Exit; //==>
    End;

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
      //       Try several times before raising an exception.
      //       Rather than calling 'Halt', just terminate this database connection,
      //       unless this is the first connection.
    On e : EUIBError Do
    Begin
        // Invalid username / password error
      If (EUIBError(E).ErrorCode = 335544472) Then
        Raise EtiOPFDBExceptionUserNamePassword.Create(FLayerName, DatabaseName, UserName, Password)
      Else
      Begin
        lMessage :=
          'Error attempting to connect to database.' + Cr +
          e.Message + Cr +
          'Error code: ' + IntToStr(EUIBError(E).ErrorCode);
        Raise EtiOPFDBExceptionUserNamePassword.Create(FLayerName, DatabaseName, UserName, Password, lMessage)
      End;
    End;
    On E : Exception Do
      Raise EtiOPFDBExceptionUserNamePassword.Create(FLayerName, DatabaseName, UserName, Password, E.Message)
  End;
End;

Procedure TtiDatabaseUIBAbs.StartTransaction;
Begin
  If InTransaction Then
    Raise EtiOPFInternalException.Create('Attempt to start a transaction but transaction already exists.');
  FTransaction.StartTransaction;
End;

Function TtiDatabaseUIBAbs.Test : Boolean;
Begin
  result := false;
  Assert(false, 'Under construction');
End;


Procedure TtiQueryUIBAbs.InternalPrepare;
Var
  lStHandle : ISCStmtHandle;
Begin
  If Not FWasPrepared Then
  Begin
    FSQLParams.Clear;
    FSQLParams.Parse(FQuery.SQL.Text);
    FQuery.Prepare;
    lStHandle := FQuery.StHandle;
    FQuery.DataBase.Lib.DSQLDescribeBind(lStHandle, FQuery.DataBase.SqlDialect, FSQLParams);
    FWasPrepared := True;
  End;
End;

{ EtiOPFDBExceptionUserNamePassword }

Constructor EtiOPFDBExceptionUserNamePassword.Create(Const pPerLayerName,
  pDatabaseName, pUserName, pPassword, pMessage : String);
Begin
  Inherited Create(pPerLayerName, pDatabaseName, pUserName, pPassword, pMessage);
  Message := 'Cannot connect to database. ' + CR + Message;
End;

{ EtiOPFDBException }

Constructor EtiOPFDBException.Create(Const pPerLayerName, pDatabaseName,
  pUserName, pPassword, pMessage : String);
Begin
  Message :=
    'Database name:       ' + pDatabaseName + Cr +
    'User name:           ' + pUserName + Cr +
    'Password:            ' + tiReplicate('X', Length(pPassword)) + Cr +
    'Persistence layer:   ' + pPerLayerName;
  If pMessage <> '' Then
    Message := Message + Cr(2) +
      'Message:' + Cr + pMessage;
End;

End.

