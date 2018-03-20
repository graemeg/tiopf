{
  This persistence layer uses standard Free Pascal SqlDB (MS-SQL Server) components.

  The connection string format is the same standard format as used by the rest
  of the tiOPF's persistence layers.

  eg:

    GTIOPFManager.ConnectDatabase('192.168.0.20:QTS3D', 'sa', 'password', '');

    ...or with a server instance...

    GTIOPFManager.ConnectDatabase('servername\instancename:QTS3D', 'sa', 'password', '');

    ...or with extra connection parameters...

    GTIOPFManager.ConnectDatabase('servername:databasename',
        'sa', 'password', 'TextSize=16777216,ApplicationName=YourAppName');

  If you specify extra connection parameters (which are optional), they
  are in name=value pairs and separated by a comma - as shown above.


  TODO:
    - Metadata extraction still needs to be implemented

}
unit tiQuerySqldbMSSQL;

{$I tiDefines.inc}

{.$Define LOGSQLDB}

interface

uses
  tiQuery
  ,Classes
  ,sqldb
  ,mssqlconn
  ,tiPersistenceLayers
  ,tiQuerySqldb
  ;

type
  TtiPersistenceLayerSqldbMSSQL = class(TtiPersistenceLayerSqldDB)
  protected
    function    GetPersistenceLayerName: string; override;
    function    GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure   AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseSQLDBMSSQL = class(TtiDatabaseSQLDB)
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


{ TtiPersistenceLayerSqldbMSSQL }

procedure TtiPersistenceLayerSqldbMSSQL.AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName := cTIPersistSqldbMSSQL;
  APersistenceLayerDefaults.DatabaseName      := CDefaultDatabaseDirectory + CDefaultDatabaseName;
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username          := 'sa';
  APersistenceLayerDefaults.Password          := 'password';
  APersistenceLayerDefaults.CanDropDatabase   := True;
  APersistenceLayerDefaults.CanCreateDatabase := True;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL     := True;
end;

function TtiPersistenceLayerSqldbMSSQL.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseSQLDBMSSQL;
end;

function TtiPersistenceLayerSqldbMSSQL.GetPersistenceLayerName: string;
begin
  Result := cTIPersistSqldbMSSQL;
end;


{ TtiDatabaseSQLDBMSSQL }

class function TtiDatabaseSQLDBMSSQL.CreateSQLConnection: TSQLConnection;
begin
  Result := TMSSQLConnection.Create(nil);
end;

function TtiDatabaseSQLDBMSSQL.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := AFieldMetaData.Name;
  case AFieldMetaData.Kind of
    qfkString:
        Result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger:
        Result := 'Integer';
//    qfkLargeInt: Result := 'BigInt';  // aka Numeric(18,0) a 8 byte type - only available in dialect 3 databases
    qfkFloat:
        Result := 'DOUBLE PRECISION';    // 'or Decimal(10, 5)';
    qfkDateTime:
        Result := 'DATETIME';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical:
        Result := 'Char(1) default ''F'' check(' + lFieldName + ' in (''T'', ''F''))';
    {$ELSE}
      {$IFDEF BOOLEAN_NUM_1}
    qfkLogical:
        Result := 'SmallInt default 0 check(' + lFieldName + ' in (1, 0)) ';
      {$ELSE}
    qfkLogical:
        Result := 'VarChar(5) default ''FALSE'' check(' + lFieldName + ' in (''TRUE'', ''FALSE'')) ';
      {$ENDIF}
    {$ENDIF}
    qfkBinary:
        Result := 'Blob sub_type 0';
    qfkLongString:
        Result := 'Blob sub_type 1'; // or maybe Varchar(max) ???
    else
      raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

function TtiDatabaseSQLDBMSSQL.HasNativeLogicalType: Boolean;
begin
  Result := False;
end;

procedure TtiDatabaseSQLDBMSSQL.ReadMetaDataTables(AData: TtiDBMetaData);
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
      { SQL Views are also included }
      lQuery.SQLText := 'select TABLE_NAME from INFORMATION_SCHEMA.TABLES';
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

procedure TtiDatabaseSQLDBMSSQL.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTableName: string;
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType: string;
  lFieldLength: integer;
begin
  lTable     := (AData as TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery     := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      lQuery.SQLText :=
        'select COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH ' +
        'from INFORMATION_SCHEMA.COLUMNS ' +
        'where TABLE_NAME = ''' + lTableName + ''' order by ORDINAL_POSITION';

      lQuery.Open;
      while not lQuery.EOF do
      begin
        lField       := TtiDBMetaDataField.Create;
        lField.Name  := Trim(lQuery.FieldAsString['COLUMN_NAME']);
        lFieldType   := lQuery.FieldAsString['DATA_TYPE'];
        lField.Width := 0;

        if  (lFieldType = 'char') or
            (lFieldType = 'varchar') or
            (lFieldType = 'nchar') or
            (lFieldType = 'nvarchar') or
            (lFieldType = 'xml') then
          lField.Kind := qfkString
        else if (lFieldType = 'int') or
            (lFieldType = 'smallint') or
            (lFieldType = 'tinyint') or
            (lFieldType = 'bigint') or
            (lFieldType = 'uniqueidentifier') then
          lField.Kind := qfkInteger
        else if (lFieldType = 'bit') then
          lField.Kind := qfkLogical
        else if (lFieldType = 'datetime') or
            (lFieldType = 'date') or
            (lFieldType = 'time') then
          lField.Kind := qfkDateTime
        else if (lFieldType = 'decimal') or
            (lFieldType = 'float') or
            (lFieldType = 'money') or
            (lFieldType = 'real') or
            (lFieldType = 'numeric') then
          lField.Kind := qfkFloat
        else if (lFieldType = 'image') or
            (lFieldType = 'varbinary') then
          lField.Kind := qfkBinary
        else
          raise EtiOPFInternalException.Create(
              'Unhandled FieldType <' + lFieldType + '> in persistence layer');

        lFieldLength := lQuery.FieldAsInteger['CHARACTER_MAXIMUM_LENGTH'];
        if (lField.Kind = qfkString) then
        begin
          lField.Width := lFieldLength;
          if (lFieldType = 'xml') or (lFieldLength = -1) then // ie: XML or varchar(max)
            lField.Kind := qfkLongString;
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
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(TtiPersistenceLayerSqldbMSSQL);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbMSSQL);

end.

