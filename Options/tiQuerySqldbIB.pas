{
  This persistence layer uses standard Free Pascal SqlDB (Firebird) components.

  The connection string format is the same as the standard Interbase/Firebird
  persistence layers.

  eg:

    GTIOPFManager.ConnectDatabase('192.168.0.20:E:\Databases\Test.fdb',
        'sysdba', 'masterkey', '');

    ...or with extra connection parameters...

    GTIOPFManager.ConnectDatabase('192.168.0.20:E:\Databases\Test.fdb',
        'sysdba', 'masterkey', 'charset=UTF8,role=admin');

  If you specify extra connection parameters (which are optional), they
  are in name=value pairs and separated by a comma - as shown above.

}
unit tiQuerySqldbIB;

{$I tiDefines.inc}

{.$Define LOGSQLDB}

interface

uses
  tiQuery
  ,Classes
  ,sqldb
  ,IBConnection
  ,tiPersistenceLayers
  ,tiQuerySqldb
  ;

type
  TtiPersistenceLayerSqldIB = class(TtiPersistenceLayerSqldDB)
  protected
    function    GetPersistenceLayerName: string; override;
    function    GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure   AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseSQLDBIB = class(TtiDatabaseSQLDB)
  protected
    class function CreateSQLConnection: TSQLConnection; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
    function    HasNativeLogicalType: Boolean; override;
  public
    procedure   ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
  end;


implementation

uses
  SysUtils,
  tiObject,
  tiExcept,
{$ifdef LOGSQLDB}
  tiLog,
{$endif}
  tiOPFManager,
  tiConstants;


{ TtiPersistenceLayerSqldIB }

procedure TtiPersistenceLayerSqldIB.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName := cTIPersistSqldbIB;
  APersistenceLayerDefaults.DatabaseName      := CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username          := 'SYSDBA';
  APersistenceLayerDefaults.Password          := 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase   := True;
  APersistenceLayerDefaults.CanCreateDatabase := True;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL     := True;
end;

function TtiPersistenceLayerSqldIB.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseSQLDBIB;
end;

function TtiPersistenceLayerSqldIB.GetPersistenceLayerName: string;
begin
  Result := cTIPersistSqldbIB;
end;


{ TtiDatabaseSQLDBIB }

class function TtiDatabaseSQLDBIB.CreateSQLConnection: TSQLConnection;
begin
  Result := TIBConnection.Create(nil);
  TIBConnection(Result).Dialect := 3;
end;

function TtiDatabaseSQLDBIB.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := AFieldMetaData.Name;
  case AFieldMetaData.Kind of
    qfkString: Result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger: Result    := 'Integer';
//    qfkLargeInt: Result := 'BigInt';  // aka Numeric(18,0) a 8 byte type - only available in dialect 3 databases 
    qfkFloat: Result      := 'DOUBLE PRECISION';    // 'or Decimal(10, 5)';
    qfkDateTime:
        // Take into account dialect
        if TIBConnection(SQLConnection).Dialect <> 1 then
          Result := 'TIMESTAMP'
        else
          Result := 'Date';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical: Result    := 'Char(1) default ''F'' check(' +
        lFieldName + ' in (''T'', ''F''))';
    {$ELSE}
      {$IFDEF BOOLEAN_NUM_1}
    qfkLogical: Result    := 'SmallInt default 0 check(' +
        lFieldName + ' in (1, 0)) ';
      {$ELSE}
    qfkLogical: Result    := 'VarChar(5) default ''FALSE'' check(' +
        lFieldName + ' in (''TRUE'', ''FALSE'')) ';
      {$ENDIF}
    {$ENDIF}
    qfkBinary: Result     := 'Blob sub_type 0';
    qfkLongString: Result := 'Blob sub_type 1';
    else
      raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

function TtiDatabaseSQLDBIB.HasNativeLogicalType: Boolean;
begin
  Result := False;
end;

procedure TtiDatabaseSQLDBIB.ReadMetaDataTables(AData: TtiDBMetaData);
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

procedure TtiDatabaseSQLDBIB.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTableName: string;
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType: integer;
  lFieldLength: integer;
const
  cIBField_SHORT     = 7;
  cIBField_LONG      = 8;
  cIBField_FLOAT     = 10;
  cIBField_DATE      = 12;
  cIBField_TIME      = 13;
  cIBField_TEXT      = 14;
  cIBField_BIGINT    = 16;
  cIBField_DOUBLE    = 27;
  cIBField_TIMESTAMP = 35;
  cIBField_VARYING   = 37;
  cIBField_BLOB      = 261;
{ cIBField_QUAD      = 9;
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
          cIBField_SHORT,
          cIBField_LONG,
          cIBField_BIGINT:  lField.Kind := qfkInteger;
          cIBField_DOUBLE,
          cIBField_FLOAT:   lField.Kind := qfkFloat;
          cIBField_TIMESTAMP,
          cIBField_DATE,
          cIBField_TIME:    lField.Kind := qfkDateTime;
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
            else if lQuery.FieldAsInteger['field_sub_type'] = 0 then
              lField.Kind := qfkBinary
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


initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldIB);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbIB);

end.

