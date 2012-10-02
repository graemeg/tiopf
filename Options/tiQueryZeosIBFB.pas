{
  This persistence layer uses ZEOS components to communicate with a Firebird
  database server. This unit is now a merged unit encapsulating all Firebird
  Server versions.

  The connection string format is the same as the standard Interbase/Firebird
  persistence layers.

  If you specify extra connection parameters, they are in name=value pairs and
  separated by a comma - as shown below.

  IMPORTANT:
  The only extra requirement here is that the PROTOCOL parameter must be
  specified in the connection call, so that ZEOS library knows which
  Firebird database driver to load. Here are some examples:

  Firebird 1.5
  ------------
    GTIOPFManager.ConnectDatabase('192.168.0.20:E:\Databases\Test.fdb',
        'sysdba', 'masterkey', 'protocol=firebird-1.5');

  Firebird 2.1
  ------------
    GTIOPFManager.ConnectDatabase('192.168.0.20:E:\Databases\Test.fdb',
        'sysdba', 'masterkey', 'protocol=firebird-2.1,charset=UTF8,role=admin');

  Firebird 2.5
  ------------
    GTIOPFManager.ConnectDatabase('192.168.0.20:E:\Databases\Test.fdb',
        'sysdba', 'masterkey', 'protocol=firebird-2.5,role=admin');

}
unit tiQueryZeosIBFB;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiQuery
  ,tiQueryZeosAbs
  ,tiPersistenceLayers
  ;

type

  TtiPersistenceLayerZeosFB = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;


  TtiDatabaseZeosIBFB = class(TtiDatabaseZeosAbs)
  protected
    function FieldMetaDataToSQLCreate(const pFieldMetaData: TtiDBMetaDataField): string; override;
    procedure SetupDBParams; override;
  public
    constructor Create; override;
    procedure ReadMetaDataTables(pData: TtiDBMetaData); override;
    procedure ReadMetaDataFields(pData: TtiDBMetaDataTable); override;
    class function DatabaseExists(const ADatabaseName, AUserName, APassword: String; const AParams: string=''): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string=''); override;
    class procedure DropDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string=''); override;
  end;


implementation

uses
  tiObject
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  ,ZDbcIntfs
  ;

const
  cErrorProtocolParameterNeeded = ' needs a ''protocol'' param';

{ TtiPersistenceLayerZeosFB }

function TtiPersistenceLayerZeosFB.GetPersistenceLayerName: string;
begin
  Result := cTIPersistZeosFB;
end;

function TtiPersistenceLayerZeosFB.GetDatabaseClass: TtiDatabaseClass;
begin
  Result := TtiDatabaseZeosIBFB;
end;

function TtiPersistenceLayerZeosFB.GetQueryClass: TtiQueryClass;
begin
  Result := TtiQueryZeos;
end;

procedure TtiPersistenceLayerZeosFB.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName := cTIPersistZeosFB;
  APersistenceLayerDefaults.DatabaseName :=
    CDefaultDatabaseDirectory + CDefaultDatabaseName + '.fdb';
  APersistenceLayerDefaults.Username := 'SYSDBA';
  APersistenceLayerDefaults.Password := 'masterkey';
  APersistenceLayerDefaults.CanDropDatabase := False;
  APersistenceLayerDefaults.CanCreateDatabase := True;
  APersistenceLayerDefaults.CanSupportMultiUser := True;
  APersistenceLayerDefaults.CanSupportSQL := True;
end;

{ TtiDatabaseZeosIBFB }

procedure TtiDatabaseZeosIBFB.SetupDBParams;
var
  lParams: TStringList;
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

    if lParams.Values['PROTOCOL'] <> '' then
    begin
      Connection.Protocol := lParams.Values['PROTOCOL'];
      lParams.Delete(lParams.IndexOfName('PROTOCOL'));
    end
    else
      raise EtiOPFProgrammerException.Create(ClassName + cErrorProtocolParameterNeeded);

//    if Params.Values['CODEPAGE'] <> '' then
//      Connection.Properties.Add('CODEPAGE=' + Params.Values['CODEPAGE']);

    Connection.Properties.AddStrings(lParams);
  finally
    lParams.Free;
  end;
end;

constructor TtiDatabaseZeosIBFB.Create;
begin
  inherited;

  Connection.Properties.Add('DIALECT=3');
  Connection.TransactIsolationLevel := tiReadCommitted;
end;

procedure TtiDatabaseZeosIBFB.ReadMetaDataTables(pData: TtiDBMetaData);
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

procedure TtiDatabaseZeosIBFB.ReadMetaDataFields(pData: TtiDBMetaDataTable);
var
  lTableName: string;
  lQuery: TtiQuery;
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lFieldType: integer;
  lFieldLength: integer;
const
  // Interbase field types
  //   select * from rdb$types
  //   where rdb$field_NAME = 'RDB$FIELD_TYPE'
  //   ORDER BY RDB$TYPE

  cIBField_SHORT     = 7;
  cIBField_LONG      = 8;
  cIBField_QUAD      = 9;
  cIBField_FLOAT     = 10;
  cIBField_DATE      = 12;
  cIBField_TIME      = 13;
  cIBField_TEXT      = 14;
  cIBField_BIGINT    = 16;
  cIBField_DOUBLE    = 27;
  cIBField_TIMESTAMP = 35;
  cIBField_VARYING   = 37;
  cIBField_CSTRING   = 40;
  cIBField_BLOB_ID   = 45;
  cIBField_BLOB      = 261;

begin
  lTable := (pData as TtiDBMetaDataTable);
  lTableName := UpperCase(lTable.Name);
  lQuery := GTIOPFManager.PersistenceLayers.CreateTIQuery(TtiDatabaseClass(ClassType));
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
              if lQuery.FieldAsInteger['field_sub_type'] = 0 then
                lField.Kind := qfkBinary
              else if lQuery.FieldAsInteger['field_sub_type'] = 1 then
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

class function TtiDatabaseZeosIBFB.DatabaseExists(const ADatabaseName,
  AUserName, APassword: String; const AParams: string): boolean;
var
  lDatabase: TtiDatabaseZeosIBFB;
  lParams: TStringList;
begin
  lDatabase := TtiDatabaseZeosIBFB.Create;
  try

    lDatabase.DatabaseName := ADatabaseName;
    lDatabase.UserName := AUserName;
    lDatabase.Password := APassword;
    lParams := TStringList.Create;
    try
      lParams.Text := AParams;
      lDatabase.Params.AddStrings(lParams);
    finally
      lParams.Free;
    end;

    try
      lDatabase.Connected := True;
      Result := True;
    except
      on e: Exception do
        Result := False;
    end;
    lDatabase.Connected := False;
  finally
    lDatabase.Free;
  end;
end;

class procedure TtiDatabaseZeosIBFB.CreateDatabase(const ADatabaseName,
  AUserName, APassword: string; const AParams: string);
var
  lDatabase: TtiDatabaseZeosIBFB;
  lParams: TStringList;
begin
  lDatabase := TtiDatabaseZeosIBFB.Create;
  try
    lDatabase.DatabaseName := ADatabaseName;
    lDatabase.UserName := AUserName;
    lDatabase.Password := APassword;

    lParams := TStringList.Create;
    try
      lParams.Text := AParams;
      lDatabase.Params.AddStrings(lParams);
    finally
      lParams.Free;
    end;

    lDatabase.SetupDBParams;

    { Default character set can be passed as a parameter. Probably
      the page_size too. }
    lDatabase.Connection.Properties.Add(
      Format('createnewdatabase=create database ''%s'' user ''%s'' password ''%s'' page_size 4096;', [lDatabase.Connection.Database, lDatabase.Connection.User,
      lDatabase.Connection.Password]));

    lDatabase.Connected := True;
    lDatabase.Connected := False;
  finally
    lDatabase.Free;
  end;
end;

class procedure TtiDatabaseZeosIBFB.DropDatabase(const ADatabaseName,
  AUserName, APassword: string; const AParams: string);
begin
  Assert(False, 'DropDatabase not implemented in ' + ClassName);
end;

function TtiDatabaseZeosIBFB.FieldMetaDataToSQLCreate(
  const pFieldMetaData: TtiDBMetaDataField): string;
var
  lFieldName: string;
begin
  lFieldName := pFieldMetaData.Name;
  case pFieldMetaData.Kind of
    qfkString: Result := 'VarChar( ' + IntToStr(pFieldMetaData.Width) + ' )';
    qfkInteger: Result := 'Integer';
    //    qfkLargeInt: Result := 'BigInt';  // aka Numeric(18,0) a 8 byte type - only available in dialect 3 databases
    qfkFloat: Result := 'DOUBLE PRECISION';
    qfkDateTime:
      // Take into account dialect
      if Connection.Properties.Values['DIALECT'] <> '1' then
        Result := 'TIMESTAMP'
      else
        Result := 'Date';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical:
      Result := 'Char(1) default ''F'' check( ' + lFieldName + ' in ( ''T'', ''F'' ))';
    {$ELSE}
      {$IFDEF BOOLEAN_NUM_1}
    qfkLogical:
      Result := 'SmallInt default 0 check(' + lFieldName + ' in (1, 0)) ';
      {$ELSE}
    qfkLogical:
      Result := 'VarChar(5) default ''FALSE'' check( ' + lFieldName + ' in ( ''TRUE'', ''FALSE'' )) ';
      {$ENDIF}
    {$ENDIF}
    qfkBinary: Result := 'Blob sub_type 0';
    qfkLongString: Result := 'Blob sub_type 1';
    else
      raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerZeosFB);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistZeosFB);

end.
