{
  This persistence layer uses standard Free Pascal SQLDB (PostGreSQL) components.

  The connection string format is show below.

  eg:
    GTIOPFManager.ConnectDatabase('192.168.0.20:mydatabasename',
        'myusername', 'mypassword', '');

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQuerySqldbPQ;

{$I tiDefines.inc}

// for debug purposes only
{.$define LOGSQLDB}

interface

uses
  tiQuery
  ,Classes
  ,sqldb
  ,PQConnection
  ,tiPersistenceLayers
  ,tiQuerySqldb;

type
  TtiPersistenceLayerSqldPQ = class(TtiPersistenceLayerSqldDB)
  protected
    function    GetPersistenceLayerName: string; override;
    function    GetDatabaseClass: TtiDatabaseClass; override;
  public
    procedure   AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseSQLDBPQ = Class(TtiDatabaseSQLDB)
  protected
    class function CreateSQLConnection: TSQLConnection; override;
    function    HasNativeLogicalType: boolean; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string; override;
  public
    procedure   ReadMetaDataTables(AData: TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData: TtiDBMetaDataTable); override;
  end;



implementation

uses
{$ifdef LOGSQLDB}
  tiLog,
{$endif}
  SysUtils,
  tiOPFManager,
  tiConstants,
  tiExcept,
  tiObject;


{ TtiPersistenceLayerSqldPQ }

procedure TtiPersistenceLayerSqldPQ.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistSqldbPQ;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseName;
  APersistenceLayerDefaults.Username:= 'postgres';
  APersistenceLayerDefaults.Password:= 'postgres';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerSqldPQ.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseSQLDBPQ;
end;

function TtiPersistenceLayerSqldPQ.GetPersistenceLayerName: string;
begin
  result:= cTIPersistSqldbPQ;
end;


{ TtiDatabaseSQLDBPQ }

Class function TtiDatabaseSQLDBPQ.CreateSQLConnection: TSQLConnection;
begin
  Result:=TPQConnection.Create(Nil);
end;


function TtiDatabaseSQLDBPQ.HasNativeLogicalType: boolean;
begin
  {$IFDEF BOOLEAN_CHAR_1}
  Result := False;
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function TtiDatabaseSQLDBPQ.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := AFieldMetaData.Name;
  case AFieldMetaData.Kind of
    qfkString:      Result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger:     Result := 'Integer';
    qfkFloat:       Result := 'DOUBLE PRECISION';  //  or 'Decimal(10, 5)';
    qfkDateTime:    Result := 'TIMESTAMP';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical:     Result := 'Char(1) default ''F'' check(' +
        lFieldName + ' in (''T'', ''F''))';
    {$ELSE}
    qfkLogical:     Result := 'VarChar(5) default ''FALSE'' check(' +
        lFieldName + ' in (''TRUE'', ''FALSE'')) ';
    {$ENDIF}
    qfkBinary:      Result := 'Bytea';
    qfkLongString:  Result := 'TEXT';
    else
      raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

procedure TtiDatabaseSQLDBPQ.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
begin
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      { SQL Views are now also included }
      lQuery.SQLText :=
          'SELECT table_name ' +
          '  FROM information_schema.tables ' +
          '  WHERE ((table_type = ''VIEW'') or (table_type = ''BASE TABLE'')) ' +
          '    AND table_schema NOT IN (''pg_catalog'', ''information_schema'') ' +
          '    AND table_name !~ ''^pg_'' ';
      lQuery.Open;
      while not lQuery.EOF do
      begin
        lTable := TtiDBMetaDataTable.Create;
        lTable.Name := lQuery.FieldAsString['table_name'];
        lTable.ObjectState := posPK;
        AData.Add(lTable);
        lQuery.Next;
      end;
      lQuery.DetachDatabase;
      AData.ObjectState := posClean;
    finally
      Commit;
    end;
  finally
    lQuery.Free;
  end;
end;

procedure TtiDatabaseSQLDBPQ.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lQuery: TtiQuery;
  lField: TtiDBMetaDataField;
  lFieldType: string;
  lFieldLength: integer;
begin
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
  try
    StartTransaction;
    try
      lQuery.AttachDatabase(Self);
      lQuery.SQLText :=
          'SELECT ordinal_position,              ' +
          '       column_name,                   ' +
          '       data_type,                     ' +
          '       column_default,                ' +
          '       is_nullable,                   ' +
          '       character_maximum_length,      ' +
          '       numeric_precision              ' +
          '    FROM information_schema.columns   ' +
          '   WHERE table_name = ''' + lowercase(AData.Name) + ''' ' +
          'ORDER BY ordinal_position             ';
      lQuery.Open;
      while not lQuery.EOF do
      begin
        lField        := TtiDBMetaDataField.Create;
        lField.Name   := lQuery.FieldAsString['column_name'];
        lFieldType    := lQuery.FieldAsString['data_type'];
        lFieldLength  := lQuery.FieldAsInteger['character_maximum_length'];
        lField.Width  := 0;

        if (lFieldType = 'character varying')
          or (lFieldType = 'character') then
        begin
          lField.Kind := qfkString;
          lField.Width := lFieldLength;
        end
        else if lFieldType = 'integer' then
        begin
          lField.Kind := qfkInteger;
        end
{
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
}
        else
          raise EtiOPFInternalException.Create('Invalid Interbase FieldType <' + lFieldType + '>');

        lField.ObjectState := posClean;
        AData.Add(lField);
        lQuery.Next;
      end;  // while
    finally
      Commit;
    end;
    lQuery.DetachDatabase;
    AData.ObjectState := posClean;
  finally
    lQuery.Free;
  end;
end;

initialization

  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerSqldPQ);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistSqldbPQ);

end.
