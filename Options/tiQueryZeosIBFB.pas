unit tiQueryZeosIBFB;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiQuery
  ,tiQueryZeosAbs
  ;

type

  TtiDatabaseZeosIBFB = Class(TtiDatabaseZeosAbs)
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
  ;


{ TtiDatabaseZeosIBFB }

procedure TtiDatabaseZeosIBFB.SetupDBParams;
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
                                  if lQuery.FieldAsInteger['field_sub_type'] = 0 then
                                    lField.Kind := qfkBinary
                                  else
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

class function TtiDatabaseZeosIBFB.DatabaseExists(const ADatabaseName,
  AUserName, APassword: String): boolean;
var
  lDatabase: TtiDatabaseZeosIBFB;
begin
  lDatabase := TtiDatabaseZeosIBFB.Create;
  try
    lDatabase.Connection.Database := ADatabaseName;
    lDatabase.Connection.User     := AUserName;
    lDatabase.Connection.Password := APassword;

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

class procedure TtiDatabaseZeosIBFB.CreateDatabase(const ADatabaseName,
  AUserName, APassword: string);
var
  lDatabase : TtiDatabaseZeosIBFB;
begin
  lDatabase := TtiDatabaseZeosIBFB.Create;
  try
    lDatabase.Connection.Database := ADatabaseName;
    lDatabase.Connection.User     := AUserName;
    lDatabase.Connection.Password := APassword;

    { Default character set can be passed as a parameter. Probably
      the page_size too. }
    lDatabase.Connection.Properties.Add(
      Format('createnewdatabase=create database ''%s'' user ''%s'' password ''%s'' page_size 4096;',
        [lDatabase.Connection.Database, lDatabase.Connection.User, lDatabase.Connection.Password]));

    lDatabase.Connection.Connect;
    lDatabase.Connection.Disconnect;
  finally
    lDatabase.Free;
  end;
end;

class procedure TtiDatabaseZeosIBFB.DropDatabase(const ADatabaseName,
  AUserName, APassword: string);
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
    qfkString: result := 'VarChar( ' + IntToStr(pFieldMetaData.Width) + ' )';
    qfkInteger: result := 'Integer';
//    qfkFloat: result := 'Decimal( 10, 5 )';
    qfkFloat: result := 'DOUBLE PRECISION';
    // Just for new version of IB (6.x)
    // DATE holds only DATE without TIME...
    qfkDateTime: if Connection.Properties.Values['DIALECT'] <> '1' then
        result := 'TIMESTAMP'
      else
        result := 'Date';
    {$IFDEF BOOLEAN_CHAR_1}
    qfkLogical    : result := 'Char(1) default ''F'' check( ' + lFieldName + ' in ( ''T'', ''F'' ))';
    {$ELSE}
    qfkLogical    : result := 'VarChar(5) default ''FALSE'' check( ' + lFieldName + ' in ( ''TRUE'', ''FALSE'' )) ';
    {$ENDIF}
    qfkBinary: result := 'Blob sub_type 0';
    qfkLongString: result := 'Blob sub_type 1';
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

end.
