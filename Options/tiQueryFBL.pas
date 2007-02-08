{
  This persistence layer uses the FBLib 0.83 Firebird Library of components.
  See [http://fblib.altervista.org] for more details.
  
  FBLib runs under Delphi, Kylix and Free Pascal.  FBLib also has support for
  the Firebird Services. Remote backup and restore, Stats, User maintenance,
  etc...
  
  The connection string format used is a little bit different to the standard
  Interbase/Firebird persistence layers.  FBLib uses a separate propertie for
  the Server Host and Database File on the server.  As a result, I needed to
  split the ADatabaseName param passed into the ConnectDatabase procedure.
  
  eg:
    gTIOPFManager.ConnectDatabase('192.168.0.20|E:\Databases\Test.fdb',
        'sysdba', 'masterkey', '');
        
  Note the | (pipe) sign between the IP address and the database name.
  
  Initial Author:  Graeme Geldenhuys (graemeg@gmail.com) - Feb 2006
}
unit tiQueryFBL;

{$I tiDefines.inc}

{.$Define FBL_Params_Mod}

interface
uses
  tiQuery
  ,Classes
  ,FBLDatabase
  ,FBLParamDsql
  ,FBLTransaction
  ,ibase_h
  ,tiDBConnectionPool
  ,tiObject
 ;


type
  TtiDatabaseFBL = class(TtiDatabaseSQL)
  private
    FDBase: TFBLDatabase;
    FTrans: TFBLTransaction;
  protected
    procedure   SetConnected(AValue: boolean); override;
    function    GetConnected: boolean; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    property    IBDatabase: TFBLDatabase read FDBase write FDBase;
    procedure   StartTransaction; override;
    function    InTransaction: boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    procedure   ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
    function    Test : boolean; override;
  end;


  TtiQueryFBL = class(TtiQuerySQL)
  private
    FQuery: TFBLParamDsql;    // handles params as names
    FbActive: boolean;
    function  IBFieldKindToTIFieldKind(AData: TXSQLVar): TtiQueryFieldKind;
    procedure Prepare;
  protected

    function  GetFieldAsString(const AName: string): string; override;
    function  GetFieldAsFloat(const AName: string): extended; override;
    function  GetFieldAsBoolean(const AName: string): boolean; override;
    function  GetFieldAsInteger(const AName: string): Int64; override;
    function  GetFieldAsDateTime(const AName: string): TDateTime; override;

    function  GetFieldAsStringByIndex(AIndex: Integer): string    ; override;
    function  GetFieldAsFloatByIndex(AIndex: Integer)  : extended    ; override;
    function  GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function  GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; override;
    function  GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; override;
    function  GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; override;

    function  GetSQL: TStrings; override;
    procedure SetSQL(const AValue: TStrings); override;
    function  GetActive: boolean; override;
    procedure SetActive(const AValue: boolean); override;
    function  GetEOF: boolean; override;
    function  GetParamAsString(const AName: string): string; override;
    function  GetParamAsBoolean(const AName: string): boolean; override;
    function  GetParamAsFloat(const AName: string): extended; override;
    function  GetParamAsInteger(const AName: string): Int64; override;
    function  GetParamAsDateTime(const AName: string): TDateTime; override;
    function  GetParamIsNull(const AName: string): Boolean; override;
    procedure SetParamAsString(const AName, AValue: string); override;
    procedure SetParamAsBoolean(const AName: string; const AValue: boolean); override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended); override;
    procedure SetParamAsInteger(const AName: string; const AValue: Int64); override;
    procedure SetParamAsDateTime(const AName: string; const AValue: TDateTime); override;
    procedure SetParamIsNull(const AName: string; const AValue: Boolean); override;
    function  GetFieldIsNull(const AName: string): Boolean; override;
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
    procedure   AssignFieldAsStreamByIndex(     AIndex : integer; const AValue : TStream); override;
    procedure   AssignParams(const AParams: TtiQueryParams; const AWhere: TtiQueryParams = nil); override;

    procedure   AttachDatabase(ADatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

    function    FieldCount: integer; override;
    function    FieldName(AIndex: integer): string; override;
    function    FieldIndex(const AName: string): integer; override;
    function    FieldKind(AIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(AIndex: integer): integer; override;
    function    HasNativeLogicalType : boolean; override;

  end;

implementation
uses
   tiUtils
  ,tiLog
  ,TypInfo
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  ,Variants
  ,FBLExcept
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryFBL
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiQueryFBL.Create;
begin
  inherited;
  FQuery := TFBLParamDsql.Create(nil);
end;


destructor TtiQueryFBL.Destroy;
begin
  FQuery.Free;
  inherited;
end;


procedure TtiQueryFBL.Close;
begin
  Active := false;
end;


procedure TtiQueryFBL.ExecSQL;
begin
  Log(ClassName + ': [Prepare] ' + tiNormalizeStr(self.SQLText), lsSQL);
  Prepare;
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);
  FQuery.ExecSQL;
end;


function TtiQueryFBL.GetFieldAsBoolean(const AName: string): boolean;
var
  lsValue: string;
begin
  lsValue := Trim(upperCase(FQuery.FieldByNameAsString(UpperCase(AName))));
  result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;


function TtiQueryFBL.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  result := FQuery.FieldByNameAsDateTime(UpperCase(AName));
end;


function TtiQueryFBL.GetFieldAsFloat(const AName: string): extended;
begin
  result := FQuery.FieldByNameAsDouble(UpperCase(AName));
end;


function TtiQueryFBL.GetFieldAsInteger(const AName: string): Int64;
begin
  result := FQuery.FieldByNameAsInt64(UpperCase(AName));
end;


function TtiQueryFBL.GetFieldAsString(const AName: string): string;
begin
  result := FQuery.FieldByNameAsString(UpperCase(AName));
end;


function TtiQueryFBL.GetFieldAsStringByIndex(AIndex: Integer): string;
begin
  Result := FQuery.FieldAsString(AIndex);
end;


function TtiQueryFBL.GetFieldAsFloatByIndex(AIndex: Integer): extended;
begin
  Result := FQuery.FieldAsFloat(AIndex);
end;


function TtiQueryFBL.GetFieldAsBooleanByIndex(AIndex: Integer): boolean ;
var
  lsValue: string;
begin
  lsValue := Trim(FQuery.FieldAsString(AIndex));
  result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;


function TtiQueryFBL.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
begin
  Result := FQuery.FieldAsInt64(AIndex);
end;


function TtiQueryFBL.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
begin
  Result := FQuery.FieldAsDateTime(AIndex);
end;


function TtiQueryFBL.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
begin
  Result := FQuery.FieldIsNull(AIndex);
end;


function TtiQueryFBL.GetActive: boolean;
begin
  result := FbActive;
end;


function TtiQueryFBL.GetEOF: boolean;
begin
  result := FQuery.EOF;
end;


function TtiQueryFBL.GetParamAsBoolean(const AName: string): boolean;
var
  lValue: string;
  idx: integer;
begin
//  Assert(false, 'Under construction');
//  idx := FQuery.ParamNames.IndexOf(AName);

(*
  lValue := FQuery.Params.ByName(UpperCase(AName)).AsString;
{$IFDEF BOOLEAN_CHAR_1}
  result := SameText(lValue, 'T');
{$ELSE}
  result := SameText(lValue, 'TRUE');
{$ENDIF} // BOOLEAN_CHAR_1
*)
end;


function TtiQueryFBL.GetParamAsDateTime(const AName: string): TDateTime;
begin
//  Assert(false, 'Under construction');
//  result := FQuery.Params.ByName(UpperCase(AName)).AsDateTime;
end;


function TtiQueryFBL.GetParamAsFloat(const AName: string): extended;
begin
//  Assert(false, 'Under construction');
//  result := FQuery.Params.ByName(UpperCase(AName)).AsDouble;
end;


function TtiQueryFBL.GetParamAsInteger(const AName: string): Int64;
begin
//  Assert(false, 'Under construction');
//  result := FQuery.Params.ByName(UpperCase(AName)).AsInt64;
end;


function TtiQueryFBL.GetParamAsString(const AName: string): string;
var
  i: integer;
begin
  {$IFDEF FBL_Params_Mod}
  i := FQuery.ParamNameToIndex(AName);
  if i >= 0 then
    result := FQuery.ParamValueAsString(i)
  else
  {$ELSE}
    result := '* not supported *';
  {$ENDIF}
end;


function TtiQueryFBL.GetSQL: TStrings;
begin
  result := FQuery.SQL;
end;


procedure TtiQueryFBL.Next;
begin
  FQuery.Next;
end;


procedure TtiQueryFBL.Open;
begin
  Log(ClassName + ': ' + tiNormalizeStr(self.SQLText), lsSQL);
  Log(ClassName + ': [Params] ' + ParamsAsString, lsSQL);
  Active := true;
end;


function TtiQueryFBL.ParamCount: integer;
begin
  Result := FQuery.ParamCount;
end;


function TtiQueryFBL.ParamName(AIndex: integer): string;
begin
  Result := FQuery.ParamName(AIndex);
end;


procedure TtiQueryFBL.SetActive(const AValue: boolean);
begin
  Assert(Database.TestValid(TtiDatabase), cTIInvalidObjectError);
  if AValue then
  begin
    FQuery.ExecSQL;
    FbActive := true;
  end else
  begin
    FQuery.Close;
    FbActive := false;
  end;
end;


procedure TtiQueryFBL.SetParamAsBoolean(const AName: string; const AValue: boolean);
begin
  Prepare;
{$IFDEF BOOLEAN_CHAR_1}
  if AValue then
    FQuery.ParamByNameAsString(UpperCase(AName), 'T')
  else
    FQuery.ParamByNameAsString(UpperCase(AName), 'F');
{$ELSE}
  if AValue then
    FQuery.ParamByNameAsString(UpperCase(AName), 'TRUE')
  else
    FQuery.ParamByNameAsString(UpperCase(AName), 'FALSE');
{$ENDIF BOOLEAN_CHAR_1}
end;


procedure TtiQueryFBL.SetParamAsDateTime(const AName: string; const AValue: TDateTime);
begin
  Prepare;
  FQuery.ParamByNameAsDateTime(UpperCase(AName), AValue);
end;


procedure TtiQueryFBL.SetParamAsFloat(const AName: string; const AValue: extended);
begin
  Prepare;
  FQuery.ParamByNameAsDouble(UpperCase(AName), AValue);
end;


procedure TtiQueryFBL.SetParamAsInteger(const AName: string; const AValue: Int64);
begin
  Prepare;
  FQuery.ParamByNameAsInt64(UpperCase(AName), AValue);
end;


procedure TtiQueryFBL.SetParamAsString(const AName, AValue: string);
begin
  Prepare;
  FQuery.ParamByNameAsString(UpperCase(AName), AValue);
end;


procedure TtiQueryFBL.SetSQL(const AValue: TStrings);
begin
  FQuery.SQL.Assign(AValue);
end;


procedure TtiQueryFBL.Prepare;
begin
  if FQuery.Prepared then
    Exit; //==>
  FQuery.Prepare;
end;


procedure TtiQueryFBL.AssignParamFromStream(const AName: string; const AStream: TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  Prepare;
  FQuery.BlobParamByNameLoadFromStream(UpperCase(AName), AStream);
end;


procedure TtiQueryFBL.AssignParamToStream(const AName: string; const AStream: TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
//  FQuery.Params.ByName(UpperCase(AName)).SaveToStream(AStream);
  AStream.Position := 0;
end;


procedure TtiQueryFBL.AssignFieldAsStream(const AName: string; const AStream: TStream);
begin
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  FQuery.BlobFieldByNameSaveToStream(AName, AStream);
end;


procedure TtiQueryFBL.AssignFieldAsStreamByIndex(AIndex : integer; const AValue : TStream);
begin
  Assert(AValue <> nil, 'Stream not assigned');
  AValue.Position := 0;
  FQuery.BlobFieldSaveToStream(AIndex, AValue);
end;


procedure TtiQueryFBL.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
  FQuery.Transaction := TtiDatabaseFBL(ADatabase).FTrans;
end;


procedure TtiQueryFBL.DetachDatabase;
begin
  inherited DetachDatabase;
  FQuery.Transaction := nil;
end;


function TtiQueryFBL.FieldCount: integer;
begin
  if FQuery.EOF then
    result := 0
  else
    Result := FQuery.FieldCount;
end;


function TtiQueryFBL.FieldName(AIndex: integer): string;
begin
  Result := FQuery.FieldName(AIndex);
end;


procedure TtiQueryFBL.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
end;


function TtiQueryFBL.FieldIndex(const AName: string): integer;
var
  i: integer;
begin
  for i := 0 to FQuery.FieldCount - 1 do
  begin
    if SameText(AName, FQuery.FieldName(i)) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseFBL
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

constructor TtiDatabaseFBL.Create;
begin
  inherited Create;
  FDBase := TFBLDatabase.Create(nil);
  FTrans := TFBLTransaction.Create(nil);
  FDBase.SQLDialect  := 3;
  FDBase.Protocol    := ptTcpIp;
  FTrans.Database    := FDBase;
end;


destructor TtiDatabaseFBL.Destroy;
begin
  try
    FTrans.Free;
    FDBase.Free;
  except
    on e: exception do
      LogError(e.message);
  end;
  inherited;
end;


procedure TtiDatabaseFBL.Commit;
begin
  if not InTransaction then
    raise EtiOPFInternalException.Create('Attempt to commit but not in a transaction.');
  Log(ClassName + ': [Commit Trans]', lsSQL);
  FTrans.Commit;
end;


function TtiDatabaseFBL.InTransaction: boolean;
begin
  Result := FTrans.InTransaction;
end;


procedure TtiDatabaseFBL.RollBack;
begin
  Log(ClassName + ': [RollBack Trans]', lsSQL);
  FTrans.RollBack;
end;


procedure TtiDatabaseFBL.StartTransaction;
begin
  if InTransaction then
    raise EtiOPFInternalException.Create('Attempt to start a transaction but transaction already exists.');
  Log(ClassName + ': [Start Trans]', lsSQL);
  FTrans.StartTransaction;
end;


function TtiQueryFBL.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lValue: string;
begin
  Result := IBFieldKindToTIFieldKind(FQuery.FieldAsXSQLVAR(AIndex));
  if (Result = qfkString) then
  begin
    lValue := FQuery.FieldAsString(AIndex);
    // ToDo: How to detect a logical field in a SQL database
    //       where logicals are represented as VarChar or Char?
    //       In the IBX layer we are currently looking for a value of
    //       'TRUE' or 'FALSE', but this is not reliable as it will not
    //       detect a null. Also, other ways of representing booleans
    //       might be used like 'T', 'F', '0', '1', etc...
    {$IFDEF BOOLEAN_CHAR_1}
      if SameText(lValue, 'T') or
         SameText(lValue, 'F') then
        result := qfkLogical;
    {$ELSE}
      if SameText(lValue, 'TRUE') or
         SameText(lValue, 'TRUE ') or
         SameText(lValue, 'FALSE') then
    {$ENDIF} // BOOLEAN_CHAR_1
  end;
end;


function TtiQueryFBL.IBFieldKindToTIFieldKind(AData: TXSQLVar): TtiQueryFieldKind;
begin
  case (AData.sqltype and (not 1)) of
    SQL_TEXT,
    SQL_VARYING:  result := qfkString;
    SQL_LONG,
    SQL_SHORT,
    SQL_INT64,
    SQL_QUAD:     if AData.SqlScale = 0 then
                    result := qfkInteger
                  else
                    result := qfkFloat;
    SQL_DOUBLE,
    SQL_FLOAT,
    SQL_D_FLOAT:  result := qfkFloat;
    SQL_BLOB:     begin
                    case AData.sqlsubtype of
                    isc_blob_untyped:   result := qfkBinary;
                    isc_blob_text:      result := qfkLongString;
                    else
                      raise EtiOPFInternalException.Create('Invalid FireBird sqlsubtype');
                    end;
                  end;
    SQL_TIMESTAMP,
    SQL_TYPE_TIME,
    SQL_TYPE_DATE: result := qfkDateTime;
  else
    raise EtiOPFInternalException.Create('Invalid FireBird sqltype');
  end;
end;


function TtiQueryFBL.FieldSize(AIndex: integer): integer;
begin
  if FieldKind(AIndex) in [ qfkInteger, qfkFloat, qfkDateTime,
                            qfkLogical, qfkLongString ] then
    Result := 0
  else
    Result := FQuery.FieldAsXSQLVAR(AIndex).sqllen;
end;


function TtiQueryFBL.GetParamIsNull(const AName: string): Boolean;
begin
//  Assert(False, 'Not implemented');
  Result := False;
end;


procedure TtiQueryFBL.SetParamIsNull(const AName: string; const AValue: Boolean);
begin
  Prepare;
  FQuery.ParamByNameAsNull(AName);
end;


function TtiDatabaseFBL.GetConnected: boolean;
begin
  Result := FDBase.Connected;
end;


procedure TtiDatabaseFBL.SetConnected(AValue: boolean);
var
  lMessage : string;
begin
  try
    if (not AValue) then
    begin
      Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
      FDBase.Disconnect;
      Exit; //==>
    end;

    { DatabaseName = <host>|<database> }
    if tiNumToken(DataBaseName, '|') = 0 then
    begin
      FDBase.Host    := tiToken(DatabaseName, '|', 1);
      FDBase.DBFile  := tiToken(DatabaseName, '|', 1);
      FDBase.Protocol := ptLocal;
    end
    else
    begin
      FDBase.Host    := tiToken(DatabaseName, '|', 1);
      FDBase.DBFile  := tiToken(DatabaseName, '|', 2);
    end;

    FDBase.User      := UserName;
    FDBase.Password  := Password;
    { Should we parse self.Params: TStringList for these and assign them? }
//    FDBase.Role
//    FDBase.CharacterSet

    FDBase.Connect;
  except
    on e: EFBLError do
    begin
      // Invalid username / password error
      if (EFBLError(E).ISC_ErrorCode = 335544472) then
        raise EtiOPFDBExceptionUserNamePassword.Create(cTIPersistFBL, DatabaseName, UserName, Password)
      else
      begin
        lMessage :=
          'Error attempting to connect to database.'
          + Cr + e.Message + Cr
          + 'Error code: ' + IntToStr(E.ISC_ErrorCode);
        raise EtiOPFDBExceptionUserNamePassword.Create(cTIPersistFBL, DatabaseName, UserName, Password, lMessage)
      end;
    end;
    on E: Exception do
    begin
      raise EtiOPFDBException.Create(cTIPersistFBL, DatabaseName, UserName, Password, E.Message);
    end;
  end;
end;


function TtiQueryFBL.GetFieldIsNull(const AName: string): Boolean;
begin
  Result := FQuery.FieldByNameIsNull(AName);
end;


// See borland communit article:
//   http://community.borland.com/article/0,1410,25259,00.html
// for more system table queries, or search google on 'interbase system tables'
procedure TtiDatabaseFBL.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lQuery: TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
begin
  lMetaData := (AData as TtiDBMetaData);
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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
        lTable := TtiDBMetaDataTable.Create;
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


procedure TtiDatabaseFBL.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTableName : string;
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType : integer;
  lFieldLength : integer;
const
// Interbase field types
//   select * from rdb$types
//   where rdb$field_NAME = 'RDB$FIELD_TYPE'
//   ORDER BY RDB$TYPE

  cIBField_LONG       = 8;
  cIBField_DOUBLE     = 27;
  cIBField_TIMESTAMP  = 35;
  cIBField_DATE       = 12;
  cIBField_TIME       = 13;
  cIBField_VARYING    = 37;
  cIBField_BLOB       = 261;

  cIBField_SHORT      = 7;
  cIBField_QUAD       = 9;
  cIBField_FLOAT      = 10;
  cIBField_TEXT       = 14;
  cIBField_CSTRING    = 40;
  cIBField_BLOB_ID    = 45;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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
        lField := TtiDBMetaDataField.Create;
        lField.Name := Trim(lQuery.FieldAsString['field_name']);
        lFieldType := lQuery.FieldAsInteger['field_type'];
        lFieldLength := lQuery.FieldAsInteger['field_length'];

        lField.Width := 0;

        case lFieldType of
          cIBField_LONG      : lField.Kind := qfkInteger;
          cIBField_DOUBLE    : lField.Kind := qfkFloat;
          cIBField_TIMESTAMP,
          cIBField_DATE,
          cIBField_TIME      : lField.Kind := qfkDateTime;
          cIBField_VARYING,
          cIBField_TEXT      : begin
                                  lField.Kind := qfkString;
                                  lField.Width := lFieldLength;
                                end;
          cIBField_BLOB      : begin
                                  Assert(not lQuery.FieldIsNull['field_sub_type'], 'field_sub_type is null');
                                  if lQuery.FieldAsInteger['field_sub_type'] = 1 then
                                    lField.Kind := qfkLongString
                                  else
                                    raise EtiOPFInternalException.Create('Invalid field_sub_type <' + IntToStr(lQuery.FieldAsInteger['field_sub_type']) + '>');
                                end;
        else
          raise EtiOPFInternalException.Create('Invalid Interbase FieldType <' + IntToStr(lFieldType) + '>');
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


function TtiDatabaseFBL.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName : string;
begin
  lFieldName := AFieldMetaData.Name;
  case AFieldMetaData.Kind of
    qfkString: result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger: result := 'Integer';
//    qfkFloat: result := 'Decimal(10, 5)';
    qfkFloat: result := 'DOUBLE PRECISION';
    // Just for new version of IB (6.x)
    // DATE holds only DATE without TIME...
    qfkDateTime: if IBDatabase.SQLDialect <> 1 then
        result := 'TIMESTAMP'
      else
        result := 'Date';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical   : result := 'Char(1) default ''F'' check(' + lFieldName + ' in (''T'', ''F''))';
    {$ELSE}
    qfkLogical   : result := 'VarChar(5) default ''FALSE'' check(' + lFieldName + ' in (''TRUE'', ''FALSE'')) ';
    {$ENDIF}
    qfkBinary: result := 'Blob sub_type 0';
    qfkLongString: result := 'Blob sub_type 1';
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;


// ToDo: Shouldn't this be in the abstract?
procedure TtiQueryFBL.AssignParams(const AParams, AWhere: TtiQueryParams);
begin
  if AParams = nil then
    Exit;
  Prepare;
  inherited;
end;


class procedure TtiDatabaseFBL.CreateDatabase(const ADatabaseName, AUserName, APassword: string);
var
  lDatabase : TtiDatabaseFBL;
begin
//  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
  lDatabase := TtiDatabaseFBL.Create;
  try
    lDatabase.FDBase.DBFile      := ADatabaseName;
    lDatabase.FDBase.User        := AUserName;
    lDatabase.FDBase.Password    := APassword;
    lDatabase.FDBase.CreateDatabase(ADatabaseName, AUserName, APassword);
  finally
    lDatabase.Free;
  end;
end;

// Test is an Interbase server is available. Will not test for the existance of a GDB
// file, just if the server is up. Will also return the server version.
// Problem is, that this routine takes an IP address and an interbase connection
// string comprises an IPAddress:PathName. So this will have to be parsed into
// its two parts. Some valid IB connection strings that would have to be paresd are:

//  C:\Data\MyData.gdb
//  \Data\MyData.gdb
//  MyData.gdb
//  ..\Data\MyData.gdb
//  Server:C:\Data\MyData.gdb
//  123.456.789:C:\Data\MyData.gdb
//  LocalHost:C:\Data\MyData.gdb
//  Rules: If there is only one ':', then assume it's a file name only.
//  If there are two ':', then assume a machine name.
{
function TtiDatabaseFBL.TestServer(const pServerIPAddress : string; var pServerVersion : string): boolean;
var
  lIBSP: TIBServerProperties;
begin
  lIBSP:= TIBServerProperties.Create(nil);
  try
    lIBSP.LoginPrompt := false;
    lIBSP.Params.Clear;
    lIBSP.Params.Append('user_name=' + uUser);
    lIBSP.Params.Append('password=' + uPwd);
    lIBSP.ServerName  := pServerIPAddress;
    if SameText(lIBSP.ServerName, 'LocalHost') then
      lIBSP.Protocol := Local
    else
      lIBSP.Protocol := TCP;
    lIBSP.Options := [Version];
    try
      try
        lIBSP.Active := True;
        lIBSP.FetchVersionInfo;
        pServerVersion := lIBSP.VersionInfo.ServerVersion;
        result := true;
        lIBSP.Active := False;
      except
        on e: Exception do
          result := false;
        end;
    finally
      lIBSP.Active := False;
    end;
  finally
    lIBSP.Free;
  end;
end;
}

class function TtiDatabaseFBL.DatabaseExists(const ADatabaseName,AUserName, APassword: string): boolean;
//var
//  lDatabase : TtiDatabaseFBL;
begin
  Result := False;
//  Assert(False, 'DatabaseExists not implemented in ' + ClassName);
{  lDatabase := TtiDatabaseFBL.Create;
  try
    lDatabase.FDBase.DatabaseName := ADatabaseName;
    lDatabase.FDBase.Params.Values['user_name']:= AUserName;
    lDatabase.FDBase.Params.Values['password']:= APassword;
    try
      lDatabase.FDBase.Connected := true;
      result := true;
    except
      on e:exception do
        result := false;
    end;
    lDatabase.FDBase.Connected := false;
  finally
    lDatabase.Free;
  end;
}
end;


function TtiQueryFBL.HasNativeLogicalType: boolean;
begin
  result := false;
end;


function TtiDatabaseFBL.Test: boolean;
begin
  Result := False;
  Assert(False, 'Under construction');
end;


initialization
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    cTIPersistFBL,
    TtiDBConnectionPoolDataAbs,
    TtiQueryFBL,
    TtiDatabaseFBL);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistFBL);

end.
