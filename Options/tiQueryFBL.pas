{
  This persistence layer uses the FBLib 0.83 Firebird Library of components.
  See [http://fblib.altervista.org] for more details.
  
  FBLib runs under Delphi, Kylix and Free Pascal.  FBLib also has support for
  the Firebird Services. Remote backup and restore, Stats, User maintenance,
  etc...
  
  The connection string format used is a little bit different to the standard
  Interbase/Firebird persistence layers.  FBLib uses a separate propertie for
  the Server Host and Database File on the server.  As a result, I needed to
  split the pDatabaseName param passed into the ConnectDatabase procedure.
  
  eg:
    gTIOPFManager.ConnectDatabase('192.168.0.20|E:\Databases\Test.fdb',
        'sysdba', 'masterkey', '');
        
  Note the | (pipe) sign between the IP address and the database name.
  
  Initial Author:  Graeme Geldenhuys (graemeg@gmail.com) - Feb 2006
}
unit tiQueryFBL;

{$I tiDefines.inc}

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
    procedure   SetConnected(pbValue: boolean); override;
    function    GetConnected: boolean; override;
    function    FieldMetaDataToSQLCreate(const pFieldMetaData: TtiDBMetaDataField): string; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    class function  DatabaseExists( const pDatabaseName, pUserName, pPassword : string ) : boolean ; override ;
    class procedure CreateDatabase( const pDatabaseName, pUserName, pPassword : string ) ; override ;
    property    IBDatabase: TFBLDatabase read FDBase write FDBase;
    procedure   StartTransaction; override;
    function    InTransaction: boolean; override;
    procedure   Commit; override;
    procedure   RollBack; override;
    procedure   ReadMetaDataTables(pData: TtiDBMetaData); override;
    procedure   ReadMetaDataFields(pData: TtiDBMetaDataTable); override;
    function    Test : boolean ; override ;
  end;


  TtiQueryFBL = class(TtiQuerySQL)
  private
    FQuery: TFBLParamDsql;    // handles params as names
    FbActive: boolean;
    function  IBFieldKindToTIFieldKind(pData: TXSQLVar): TtiQueryFieldKind;
    procedure Prepare;
  protected

    function  GetFieldAsString(const psName: string): string; override;
    function  GetFieldAsFloat(const psName: string): extended; override;
    function  GetFieldAsBoolean(const psName: string): boolean; override;
    function  GetFieldAsInteger(const psName: string): Int64; override;
    function  GetFieldAsDateTime(const psName: string): TDateTime; override;

    function  GetFieldAsStringByIndex(pIndex: Integer): string     ; override ;
    function  GetFieldAsFloatByIndex(pIndex: Integer)   : extended     ; override ;
    function  GetFieldAsBooleanByIndex(pIndex: Integer) : boolean  ; override ;
    function  GetFieldAsIntegerByIndex(pIndex: Integer) : Int64    ; override ;
    function  GetFieldAsDateTimeByIndex(pIndex: Integer):TDateTime ; override ;
    function  GetFieldIsNullByIndex(pIndex: Integer):Boolean       ; override ;

    function  GetSQL: TStrings; override;
    procedure SetSQL(const Value: TStrings); override;
    function  GetActive: boolean; override;
    procedure SetActive(const Value: boolean); override;
    function  GetEOF: boolean; override;
    function  GetParamAsString(const psName: string): string; override;
    function  GetParamAsBoolean(const psName: string): boolean; override;
    function  GetParamAsFloat(const psName: string): extended; override;
    function  GetParamAsInteger(const psName: string): Int64; override;
    function  GetParamAsDateTime(const psName: string): TDateTime; override;
    function  GetParamIsNull(const psName: string): Boolean; override;
    procedure SetParamAsString(const psName, Value: string); override;
    procedure SetParamAsBoolean(const psName: string; const Value: boolean); override;
    procedure SetParamAsFloat(const psName: string; const Value: extended); override;
    procedure SetParamAsInteger(const psName: string; const Value: Int64); override;
    procedure SetParamAsDateTime(const psName: string; const Value: TDateTime); override;
    procedure SetParamIsNull(const psName: string; const Value: Boolean); override;
    function  GetFieldIsNull(const psName: string): Boolean; override;
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
    procedure   AssignFieldAsStreamByIndex(      pIndex : integer ; const pValue : TStream ) ; override ;
    procedure   AssignParams(const pParams: TtiQueryParams; const pWhere: TtiQueryParams = nil); override;

    procedure   AttachDatabase(pDatabase: TtiDatabase); override;
    procedure   DetachDatabase; override;
    procedure   Reset; override;

    function    FieldCount: integer; override;
    function    FieldName(pIndex: integer): string; override;
    function    FieldIndex(const psName: string): integer; override;
    function    FieldKind(pIndex: integer): TtiQueryFieldKind; override;
    function    FieldSize(pIndex: integer): integer; override;
    function    HasNativeLogicalType : boolean ; override ;

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
  FQuery.ExecSQL;
end;


function TtiQueryFBL.GetFieldAsBoolean(const psName: string): boolean;
var
  lsValue: string;
begin
  lsValue := Trim( upperCase(FQuery.FieldByNameAsString(UpperCase(psName))));
  result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;


function TtiQueryFBL.GetFieldAsDateTime(const psName: string): TDateTime;
begin
  result := FQuery.FieldByNameAsDateTime(UpperCase(psName));
end;


function TtiQueryFBL.GetFieldAsFloat(const psName: string): extended;
begin
  result := FQuery.FieldByNameAsDouble(UpperCase(psName));
end;


function TtiQueryFBL.GetFieldAsInteger(const psName: string): Int64;
begin
  result := FQuery.FieldByNameAsInt64(UpperCase(psName));
end;


function TtiQueryFBL.GetFieldAsString(const psName: string): string;
begin
  result := FQuery.FieldByNameAsString(UpperCase(psName));
end;


function TtiQueryFBL.GetFieldAsStringByIndex(pIndex: Integer): string;
begin
  Result := FQuery.FieldAsString(pIndex);
end;


function TtiQueryFBL.GetFieldAsFloatByIndex(pIndex: Integer): extended;
begin
  Result := FQuery.FieldAsFloat(pIndex);
end;


function TtiQueryFBL.GetFieldAsBooleanByIndex(pIndex: Integer) : boolean  ;
var
  lsValue: string;
begin
  lsValue := Trim(FQuery.FieldAsString(pIndex));
  result := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;


function TtiQueryFBL.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
begin
  Result := FQuery.FieldAsInt64(pIndex);
end;


function TtiQueryFBL.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
begin
  Result := FQuery.FieldAsDateTime(pIndex);
end;


function TtiQueryFBL.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
begin
  Result := FQuery.FieldIsNull(pIndex);
end;


function TtiQueryFBL.GetActive: boolean;
begin
  result := FbActive;
end;


function TtiQueryFBL.GetEOF: boolean;
begin
  result := FQuery.EOF;
end;


function TtiQueryFBL.GetParamAsBoolean(const psName: string): boolean;
var
  lValue: string;
begin
  Assert(false, 'Under construction');
(*
  lValue := FQuery.Params.ByName(UpperCase(psName)).AsString;
{$IFDEF BOOLEAN_CHAR_1}
  result := SameText(lValue, 'T');
{$ELSE}
  result := SameText(lValue, 'TRUE');
{$ENDIF} // BOOLEAN_CHAR_1
*)
end;


function TtiQueryFBL.GetParamAsDateTime(const psName: string): TDateTime;
begin
  Assert(false, 'Under construction');
//  result := FQuery.Params.ByName(UpperCase(psName)).AsDateTime;
end;


function TtiQueryFBL.GetParamAsFloat(const psName: string): extended;
begin
  Assert(false, 'Under construction');
//  result := FQuery.Params.ByName(UpperCase(psName)).AsDouble;
end;


function TtiQueryFBL.GetParamAsInteger(const psName: string): Int64;
begin
  Assert(false, 'Under construction');
//  result := FQuery.Params.ByName(UpperCase(psName)).AsInt64;
end;


function TtiQueryFBL.GetParamAsString(const psName: string): string;
begin
  Assert(false, 'Under construction');
//  result := FQuery.Params.ByName(UpperCase(psName)).AsString;
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
  Active := true;
end;


function TtiQueryFBL.ParamCount: integer;
begin
  Result := FQuery.ParamCount;
end;


function TtiQueryFBL.ParamName(pIndex: integer): string;
begin
  Result := FQuery.ParamName(pIndex);
end;


procedure TtiQueryFBL.SetActive(const Value: boolean);
begin
  Assert( Database.TestValid(TtiDatabase), cTIInvalidObjectError );
  if Value then
  begin
    FQuery.ExecSQL;
    FbActive := true;
  end else
  begin
    FQuery.Close;
    FbActive := false;
  end;
end;


procedure TtiQueryFBL.SetParamAsBoolean(const psName: string; const Value: boolean);
begin
  Prepare ;
{$IFDEF BOOLEAN_CHAR_1}
  if Value then
    FQuery.ParamByNameAsString(UpperCase(psName), 'T')
  else
    FQuery.ParamByNameAsString(UpperCase(psName), 'F');
{$ELSE}
  if Value then
    FQuery.ParamByNameAsString(UpperCase(psName), 'TRUE')
  else
    FQuery.ParamByNameAsString(UpperCase(psName), 'FALSE');
{$ENDIF BOOLEAN_CHAR_1}
end;


procedure TtiQueryFBL.SetParamAsDateTime(const psName: string; const Value: TDateTime);
begin
  Prepare;
  FQuery.ParamByNameAsDateTime(UpperCase(psName), Value);
end;


procedure TtiQueryFBL.SetParamAsFloat(const psName: string; const Value: extended);
begin
  Prepare ;
  FQuery.ParamByNameAsDouble(UpperCase(psName), Value);
end;


procedure TtiQueryFBL.SetParamAsInteger(const psName: string; const Value: Int64);
begin
  Prepare ;
  FQuery.ParamByNameAsInt64(UpperCase(psName), Value);
end;


procedure TtiQueryFBL.SetParamAsString(const psName, Value: string);
begin
  Prepare ;
  FQuery.ParamByNameAsString(UpperCase(psName), Value);
end;


procedure TtiQueryFBL.SetSQL(const Value: TStrings);
begin
  FQuery.SQL.Assign(Value);
end;


procedure TtiQueryFBL.Prepare;
begin
  if FQuery.Prepared then
    Exit ; //==>
  FQuery.Prepare;
end;


procedure TtiQueryFBL.AssignParamFromStream(const pName: string; const pStream: TStream);
begin
  Assert(pStream <> nil, 'Stream not assigned');
  pStream.Position := 0;
  Prepare ;
  FQuery.BlobParamByNameLoadFromStream(UpperCase(pName), pStream);
end;


procedure TtiQueryFBL.AssignParamToStream(const pName: string; const pStream: TStream);
begin
  Assert(pStream <> nil, 'Stream not assigned');
//  FQuery.Params.ByName(UpperCase(pName)).SaveToStream(pStream);
  pStream.Position := 0;
end;


procedure TtiQueryFBL.AssignFieldAsStream(const pName: string; const pStream: TStream);
begin
  Assert(pStream <> nil, 'Stream not assigned');
  pStream.Position := 0;
  FQuery.BlobFieldByNameSaveToStream(pName, pStream);
end;


procedure TtiQueryFBL.AssignFieldAsStreamByIndex( pIndex : integer ; const pValue : TStream ) ;
begin
  Assert(pValue <> nil, 'Stream not assigned');
  pValue.Position := 0;
  FQuery.BlobFieldSaveToStream(pIndex, pValue);
end;


procedure TtiQueryFBL.AttachDatabase(pDatabase: TtiDatabase);
begin
  inherited AttachDatabase(pDatabase);
  FQuery.Transaction := TtiDatabaseFBL(pDatabase).FTrans;
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


function TtiQueryFBL.FieldName(pIndex: integer): string;
begin
  Result := FQuery.FieldName(pIndex);
end;


procedure TtiQueryFBL.Reset;
begin
  Active := false;
  FQuery.SQL.Clear;
end;


function TtiQueryFBL.FieldIndex(const psName: string): integer;
var
  i: integer;
begin
  for i := 0 to FQuery.FieldCount - 1 do
  begin
    if SameText(psName, FQuery.FieldName(i)) then
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
  FDBase.SQLDialect   := 3;
  FDBase.Protocol     := ptTcpIp;
  FTrans.Database     := FDBase;
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


function TtiQueryFBL.FieldKind(pIndex: integer): TtiQueryFieldKind;
var
  lValue: string;
begin
  Result := IBFieldKindToTIFieldKind(FQuery.FieldAsXSQLVAR(pIndex));
  if (Result = qfkString) then
  begin
    lValue := FQuery.FieldAsString(pIndex);
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
      if SameText(lValue, 'TRUE' ) or
         SameText(lValue, 'TRUE ') or
         SameText(lValue, 'FALSE') then
    {$ENDIF} // BOOLEAN_CHAR_1
  end;
end;


function TtiQueryFBL.IBFieldKindToTIFieldKind(pData: TXSQLVar): TtiQueryFieldKind;
begin
  case (pData.sqltype and (not 1)) of
    SQL_TEXT,
    SQL_VARYING:  result := qfkString;
    SQL_LONG,
    SQL_SHORT,
    SQL_INT64,
    SQL_QUAD:     if pData.SqlScale = 0 then
                    result := qfkInteger
                  else
                    result := qfkFloat;
    SQL_DOUBLE,
    SQL_FLOAT,
    SQL_D_FLOAT:  result := qfkFloat;
    SQL_BLOB:     begin
                    case pData.sqlsubtype of
                    isc_blob_untyped:   result := qfkBinary;
                    isc_blob_text:      result := qfkLongString;
                    else
                      raise EtiOPFInternalException.Create('Invalid FireBird sqlsubtype');
                    end ;
                  end ;
    SQL_TIMESTAMP,
    SQL_TYPE_TIME,
    SQL_TYPE_DATE: result := qfkDateTime;
  else
    raise EtiOPFInternalException.Create('Invalid FireBird sqltype');
  end;
end;


function TtiQueryFBL.FieldSize(pIndex: integer): integer;
begin
  if FieldKind(pIndex) in [ qfkInteger, qfkFloat, qfkDateTime,
                            qfkLogical, qfkLongString ] then
    Result := 0
  else
    Result := FQuery.FieldAsXSQLVAR(pIndex).sqllen;
end;


function TtiQueryFBL.GetParamIsNull(const psName: string): Boolean;
begin
  Assert(False, 'Not implemented');
  Result := False;
end;


procedure TtiQueryFBL.SetParamIsNull(const psName: string; const Value: Boolean);
begin
  Prepare ;
  FQuery.ParamByNameAsNull(psName);
end;


function TtiDatabaseFBL.GetConnected: boolean;
begin
  Result := FDBase.Connected;
end;


procedure TtiDatabaseFBL.SetConnected(pbValue: boolean);
var
  lMessage : string ;
begin
  try
    if (not pbValue) then
    begin
      Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
      FDBase.Disconnect;
      Exit; //==>
    end;

    { DatabaseName = <host>|<database> }
    if tiNumToken(DataBaseName, '|') = 1 then
    begin
      FDBase.Host     := tiToken(DatabaseName, '|', 1);
      FDBase.DBFile   := tiToken(DatabaseName, '|', 1);
      FDBase.Protocol := ptLocal;
    end
    else
    begin
      FDBase.Host     := tiToken(DatabaseName, '|', 1);
      FDBase.DBFile   := tiToken(DatabaseName, '|', 2);
    end;

    FDBase.User       := UserName;
    FDBase.Password   := Password;

    FDBase.Connect;
  except
    on e: EFBLError do
    begin
      // Invalid username / password error
      if (EFBLError(E).ISC_ErrorCode = 335544472) then
        raise EtiOPFDBExceptionUserNamePassword.Create( cTIPersistFBL, DatabaseName, UserName, Password )
      else
      begin
        lMessage :=
          'Error attempting to connect to database.'
          + Cr + e.Message + Cr
          + 'Error code: ' + IntToStr(E.ISC_ErrorCode);
        raise EtiOPFDBExceptionUserNamePassword.Create( cTIPersistFBL, DatabaseName, UserName, Password, lMessage )
      end;
    end;
    on E: Exception do
    begin
      raise EtiOPFDBException.Create( cTIPersistFBL, DatabaseName, UserName, Password, E.Message );
    end;
  end;
end;


function TtiQueryFBL.GetFieldIsNull(const psName: string): Boolean;
begin
  Result := FQuery.FieldByNameIsNull(psName);
end;


// See borland communit article:
//   http://community.borland.com/article/0,1410,25259,00.html
// for more system table queries, or search google on 'interbase system tables'
procedure TtiDatabaseFBL.ReadMetaDataTables(pData: TtiDBMetaData);
var
  lQuery: TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
begin
  lMetaData := (pData as TtiDBMetaData);
  lQuery := gTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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


procedure TtiDatabaseFBL.ReadMetaDataFields(pData: TtiDBMetaDataTable);
var
  lTableName : string ;
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType : integer ;
  lFieldLength  : integer ;
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
  lTable := (pData as TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name) ;
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
        lFieldType := lQuery.FieldAsInteger['field_type'] ;
        lFieldLength  := lQuery.FieldAsInteger['field_length'] ;

        lField.Width := 0 ;

        case lFieldType of
          cIBField_LONG       : lField.Kind := qfkInteger;
          cIBField_DOUBLE     : lField.Kind := qfkFloat;
          cIBField_TIMESTAMP,
          cIBField_DATE,
          cIBField_TIME       : lField.Kind := qfkDateTime;
          cIBField_VARYING,
          cIBField_TEXT       : begin
                                  lField.Kind := qfkString ;
                                  lField.Width := lFieldLength ;
                                end ;
          cIBField_BLOB       : begin
                                  Assert( not lQuery.FieldIsNull['field_sub_type'], 'field_sub_type is null' ) ;
                                  if lQuery.FieldAsInteger['field_sub_type'] = 1 then
                                    lField.Kind := qfkLongString
                                  else
                                    raise EtiOPFInternalException.Create( 'Invalid field_sub_type <' + IntToStr(lQuery.FieldAsInteger['field_sub_type']) + '>') ;
                                end ;
        else
          raise EtiOPFInternalException.Create( 'Invalid Interbase FieldType <' + IntToStr( lFieldType ) + '>') ;
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


function TtiDatabaseFBL.FieldMetaDataToSQLCreate(const pFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName : string ;
begin
  lFieldName := pFieldMetaData.Name ;
  case pFieldMetaData.Kind of
    qfkString: result := 'VarChar( ' + IntToStr(pFieldMetaData.Width) + ' )';
    qfkInteger: result := 'Integer';
//    qfkFloat: result := 'Decimal( 10, 5 )';
    qfkFloat: result := 'DOUBLE PRECISION';
    // Just for new version of IB (6.x)
    // DATE holds only DATE without TIME...
    qfkDateTime: if IBDatabase.SQLDialect <> 1 then
        result := 'TIMESTAMP'
      else
        result := 'Date';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical    : result := 'Char( 1 ) default ''F'' check( ' + lFieldName + ' in ( ''T'', ''F'' ))' ;
    {$ELSE}
    qfkLogical    : result := 'VarChar( 5 ) default ''FALSE'' check( ' + lFieldName + ' in ( ''TRUE'', ''FALSE'' )) ' ;
    {$ENDIF}
    qfkBinary: result := 'Blob sub_type 0';
    qfkLongString: result := 'Blob sub_type 1';
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;


// ToDo: Shouldn't this be in the abstract?
procedure TtiQueryFBL.AssignParams(const pParams, pWhere: TtiQueryParams);
begin
  if pParams = nil then
    Exit;
  Prepare ;
  inherited ;
end;


class procedure TtiDatabaseFBL.CreateDatabase(const pDatabaseName, pUserName, pPassword: string);
var
  lDatabase : TtiDatabaseFBL ;
begin
//  Assert(False, 'CreateDatabase not implemented in ' + ClassName);
  lDatabase := TtiDatabaseFBL.Create ;
  try
    lDatabase.FDBase.DBFile       := pDatabaseName;
    lDatabase.FDBase.User         := pUserName;
    lDatabase.FDBase.Password     := pPassword;
    lDatabase.FDBase.CreateDatabase(pDatabaseName, pUserName, pPassword);
  finally
    lDatabase.Free;
  end ;
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
function TtiDatabaseFBL.TestServer(const pServerIPAddress : string ; var pServerVersion : string ): boolean;
var
  lIBSP: TIBServerProperties;
begin
  lIBSP:= TIBServerProperties.Create(nil);
  try
    lIBSP.LoginPrompt := false ;
    lIBSP.Params.Clear;
    lIBSP.Params.Append( 'user_name=' + uUser );
    lIBSP.Params.Append( 'password=' + uPwd );
    lIBSP.ServerName   := pServerIPAddress ;
    if SameText( lIBSP.ServerName, 'LocalHost' ) then
      lIBSP.Protocol := Local
    else
      lIBSP.Protocol := TCP;
    lIBSP.Options := [Version];
    try
      try
        lIBSP.Active  := True;
        lIBSP.FetchVersionInfo;
        pServerVersion := lIBSP.VersionInfo.ServerVersion ;
        result := true ;
        lIBSP.Active  := False;
      except
        on e: Exception do
          result := false ;
        end;
    finally
      lIBSP.Active := False;
    end;
  finally
    lIBSP.Free;
  end ;
end;
}

class function TtiDatabaseFBL.DatabaseExists(const pDatabaseName,pUserName, pPassword: string): boolean;
//var
//  lDatabase : TtiDatabaseFBL ;
begin
  Result := False;
//  Assert(False, 'DatabaseExists not implemented in ' + ClassName);
{  lDatabase := TtiDatabaseFBL.Create ;
  try
    lDatabase.FDBase.DatabaseName := pDatabaseName;
    lDatabase.FDBase.Params.Values['user_name'] := pUserName;
    lDatabase.FDBase.Params.Values['password'] := pPassword;
    try
      lDatabase.FDBase.Connected := true ;
      result := true ;
    except
      on e:exception do
        result := false ;
    end ;
    lDatabase.FDBase.Connected := false ;
  finally
    lDatabase.Free;
  end ;
}
end;


function TtiQueryFBL.HasNativeLogicalType: boolean;
begin
  result := false ;
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
