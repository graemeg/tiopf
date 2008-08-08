unit tiQueryZeosMySQL;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiQuery
  ,tiQueryZeosAbs
  ;

type
  TtiDatabaseZeosMySQL = Class(TtiDatabaseZeosAbs)
  protected
    function FieldMetaDataToSQLCreate(const pFieldMetaData: TtiDBMetaDataField): string; override;
    procedure SetupDBParams; override;
  public
    constructor Create; override;
    procedure ReadMetaDataTables(pData: TtiDBMetaData); override;
    procedure ReadMetaDataFields(pData: TtiDBMetaDataTable); override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: String): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword : string); override;
  end;

implementation

uses
   tiDBConnectionPool
  ,tiObject
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  ,ZDbcIntfs
  ,ZDbcMySqlMetadata, Dialogs
  ;

{ TtiDatabaseZeosMySQL }

constructor TtiDatabaseZeosMySQL.Create;
begin
  inherited;
  //Connection.Properties.Add('DIALECT=3');
  Connection.TransactIsolationLevel := tiReadCommitted;
end;

class procedure TtiDatabaseZeosMySQL.CreateDatabase(const ADatabaseName,
  AUserName, APassword: string);
var
  lDatabase : TtiDatabaseZeosMySQL;
  conn: IZConnection;
  lzQuery: IZStatement;
  SQL: string;
begin
  lDatabase := TtiDatabaseZeosMySQL.Create;
  try
    lDatabase.DatabaseName := 'mysql';
    lDatabase.UserName := AUserName;
    lDatabase.Password := APassword;
    lDatabase.Connection.Protocol := 'mysql';
    try
      lDatabase.Connected := True;
      conn := lDatabase.Connection.dbcConnection;
      lzQuery := conn.CreateStatement;
      SQL := Format('create database %s',[ADatabaseName]);
      try
        lzQuery.Execute( SQL );
      except
        on E: Exception do
        begin
          raise EtiOPFInternalException.Create(E.Message) ;
        end;
      end;
      lzQuery := nil;
    finally
      lDatabase.Connection.Disconnect;
    end;
  finally
    lDatabase.Free;
  end;
end;

class procedure TtiDatabaseZeosMySQL.DropDatabase(const ADatabaseName,
  AUserName, APassword: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseZeosMySQL.DatabaseExists(const ADatabaseName,
  AUserName, APassword: String): boolean;
var
  lDatabase: TtiDatabaseZeosMySQL;
begin
  lDatabase := TtiDatabaseZeosMySQL.Create;
  try
    lDatabase.DatabaseName := ADatabaseName;
    lDatabase.UserName := AUserName;
    lDatabase.Password := APassword;
    lDatabase.Connection.Protocol := 'mysql';
    try
      lDatabase.Connected := true;
      result := true;
    except
      on e:exception do
        result := false;
    end;
    lDatabase.Connected := false;
  finally
    lDatabase.Free;
  end ;
end;

function TtiDatabaseZeosMySQL.FieldMetaDataToSQLCreate(
  const pFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := pFieldMetaData.Name;

  case pFieldMetaData.Kind of
    qfkString: result := 'VarChar( ' + IntToStr(pFieldMetaData.Width) + ' )';
    qfkInteger: result := 'Integer';
//    qfkFloat: result := 'Decimal( 10, 5 )';
    qfkFloat: result := 'DOUBLE PRECISION';
    // Just for new version of IB (6.x)
    // DATE holds only DATE without TIME...
    qfkDateTime: result := 'datetime'; {if Connection.Properties.Values['DIALECT'] <> '1' then
        result := 'TIMESTAMP'
      else
        result := 'Date';}
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical    : result := 'Char(1) default ''F''';
    {$ELSE}
    qfkLogical    : result := 'tinyint(1) default ''0''';
    {$ENDIF}
    qfkBinary: result := 'Blob';
    qfkLongString: result := 'longtext';
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

procedure TtiDatabaseZeosMySQL.ReadMetaDataFields(
  pData: TtiDBMetaDataTable);
var
  lTableName : string ;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType : Integer ;
  lFieldLength  : integer;
  conn: IZConnection;  
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
const
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
  lTableName := UpperCase(lTable.Name);
  try
    conn := Self.Connection.DbcConnection;
    Metadata := conn.GetMetadata;
    ResultSet := Metadata.GetColumns('', '', lTableName, '');
    while ResultSet.Next do
    begin
      lField := TtiDBMetaDataField.Create;
      lField.Name := ResultSet.GetStringByName('COLUMN_NAME');
      lFieldType := ResultSet.GetIntByName('DATA_TYPE');//
      lFieldLength := ResultSet.GetIntByName('COLUMN_SIZE');
      lField.Width := 0 ;
      case lFieldType of
         cIBField_LONG       : lField.Kind := qfkInteger;
         cIBField_DOUBLE     : lField.Kind := qfkFloat;
         cIBField_TIMESTAMP,
         cIBField_DATE,
         cIBField_TIME       : lField.Kind := qfkDateTime;
         cIBField_QUAD,
         cIBField_VARYING,
         cIBField_TEXT       : begin
                                 lField.Kind := qfkString ;
                                 lField.Width := lFieldLength ;
                               end ;
         cIBField_BLOB       : begin
                                  //Assert( not lQuery.FieldIsNull['field_sub_type'], 'field_sub_type is null' ) ;
                                  //if lQuery.FieldAsInteger['field_sub_type'] = 0 then
                                    lField.Kind := qfkBinary
                                  //else
                                  //if lQuery.FieldAsInteger['field_sub_type'] = 1 then
                                  //  lField.Kind := qfkLongString
                                  //else
                                  //  raise EtiOPFInternalException.Create( 'Invalid field_sub_type <' + IntToStr(lQuery.FieldAsInteger['field_sub_type']) + '>') ;
                                end ;//}
      else
        raise EtiOPFInternalException.Create( 'Invalid MySQL FieldType <' + IntToStr( lFieldType ) + '>') ;
      end;
      lField.ObjectState := posClean;
      lTable.Add(lField);
    end;
  finally
    lTable.ObjectState := posClean;
  end;
end;

procedure TtiDatabaseZeosMySQL.ReadMetaDataTables(pData: TtiDBMetaData);
var
  lQuery: TtiQuery;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
begin
  lMetaData := (pData as TtiDBMetaData);
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      { SQL Views are now also included }
      lQuery.SQLText := 'show tables';
//      'SELECT RDB$RELATION_NAME as Table_Name ' +
//      '  FROM RDB$RELATIONS ' +
//      'WHERE ((RDB$SYSTEM_FLAG = 0) OR (RDB$SYSTEM_FLAG IS NULL)) ' +
//        '  AND (RDB$VIEW_SOURCE IS NULL) ' +
//      'ORDER BY RDB$RELATION_NAME ';
      lQuery.Open;
      while not lQuery.EOF do
      begin
        lTable := TtiDBMetaDataTable.Create;
        lTable.Name := Trim(lQuery.FieldAsStringByIndex[0]);
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

procedure TtiDatabaseZeosMySQL.SetupDBParams;
var
  lParams : TStringList;
begin
  lParams := TStringList.Create;
  try
    lParams.Assign(Params);
    Connection.Database := DatabaseName;
    Connection.User := UserName;
    Connection.Password := Password;

    if lParams.Values['HOSTNAME'] <> '' then
    begin
      Connection.HostName := lParams.Values['HOSTNAME'];
      lParams.Delete(lParams.IndexOfName('HOSTNAME'));
    end;
    if lParams.Values['PORT'] <> '' then
    begin
      Connection.Port := StrToInt(lParams.Values['PORT']);
      lParams.Delete(lParams.IndexOfName('PORT'));
    end;
//    if Params.Values['CODEPAGE'] <> '' then
//      Connection.Properties.Add('CODEPAGE=' + Params.Values['CODEPAGE']);
    Connection.Properties.AddStrings(lParams);
  finally
    lParams.Free;
  End;
end;

end.
