unit tiQueryDBEPostgreSQL;

interface
uses
  Classes,
  DBXpress,
  SQLExpr,
  tiQueryDBEAbs,
  tiQuery;

type

  // ---------------------------------------------------------------------------
  TtiDatabaseDBEPostgreSQL = class( TtiDatabaseDBEAbs )
  protected
    procedure   SetupDBParams ; override ;
    function    FieldMetaDataToSQLCreate( const pFieldMetaData : TtiDBMetaDataField ) : string ; override ;
  public
    class function  DatabaseExists( const psDatabaseName, psUserName, psPassword : string ):boolean ; override ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    procedure       ReadMetaDataTables( pData : TtiDBMetaData ) ; override ;
    procedure       ReadMetaDataFields( pData : TtiDBMetaDataTable ) ; override ;
    procedure       RollBack ; override ; // Not supported in BDE
    function        Test : boolean ; override ;
    procedure       CreateTable( const pTableMetaData: TtiDBMetaDataTable); override;
  end ;

  // ---------------------------------------------------------------------------
  TtiQueryDBEPostgreSQL = class( TtiQueryDBE ) ;


implementation
uses
  tiDBConnectionPool
  ,tiObject
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  , DB;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDatabaseBDEParadox
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
class procedure TtiDatabaseDBEPostgreSQL.CreateDatabase(const psDatabaseName,psUserName, psPassword: string);
begin
  Assert( false, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseDBEPostgreSQL.DatabaseExists(const psDatabaseName,psUserName, psPassword: string): boolean;
var
  lDB : TtiDatabaseDBEPostgreSQL;
begin
(*
  Assert( false, 'DatabaseExists not implemented in ' + ClassName);
*)
  lDB := TtiDatabaseDBEPostgreSQL.Create;
  lDB.Params.Clear;
  lDB.Params.Add('Database=' + psDatabaseName);
  lDB.Params.Add('User_Name=' + psUserName);
  lDB.Params.Add('Password=' + psPassword);
  try
    try
      Result := True;
    except
      Result := False;
    end;
  finally
    lDB.Free;
  end;
end;

function TtiDatabaseDBEPostgreSQL.FieldMetaDataToSQLCreate( const pFieldMetaData: TtiDBMetaDataField): string;
begin
  case pFieldMetaData.Kind of
    qfkString     : result := 'varchar( ' + IntToStr( pFieldMetaData.Width ) + ' )' ;
    qfkInteger    : result := 'integer'  ;
    qfkFloat      : result := 'double' ;
    qfkDateTime   : result := 'timestamp'  ;
    qfkLogical    : result := 'bool'  ;
    qfkBinary     : result := 'bytea'  ;
    qfkLongString : result := 'text'  ;
  else
    raise EtiOPFInternalException.Create('FieldMetaDataToSQLCreate - Invalid FieldKind') ;
  end ;
end;

procedure TtiDatabaseDBEPostgreSQL.ReadMetaDataFields(pData: TtiDBMetaDataTable);
var
  lTable : TtiDBMetaDataTable ;
  lField : TtiDBMetaDataField ;
  lDelphiTable : TSQLTable;
  i : integer ;
  sSchema : String;
begin
  lTable := pData;
  lDelphiTable := TSQLTable.Create( nil ) ;
  try
    lDelphiTable.SQLConnection := SQLConnection;
    sSchema := SQLConnection.Params.Values['Role_Name'];
    lDelphiTable.SchemaName := sSchema;
    lDelphiTable.TableName := LowerCase(lTable.Name);
    lDelphiTable.FieldDefs.Update ;
    for i := 0 to lDelphiTable.FieldDefs.Count - 1 do
    begin
      lField := TtiDBMetaDataField.Create ;
      lField.Name := lDelphiTable.FieldDefs[i].Name ;
      lField.Width := lDelphiTable.FieldDefs[i].Size ;
      lField.ObjectState := posClean ;
      lTable.Add( lField ) ;
    end ;
    lTable.ObjectState := posClean ;
  finally
    lDelphiTable.Free;
  end ;
end;

procedure TtiDatabaseDBEPostgreSQL.ReadMetaDataTables(pData: TtiDBMetaData);
var
  lMetaData : TtiDBMetaData ;
  lTable : TtiDBMetaDataTable ;
  lsl : TStringList;
  sSQL : String;
  query : TSQLQuery;
begin
  lMetaData := pData;
  lsl := TStringList.Create ;
  query := TSQLQuery.Create(nil);
  try
    query.SQLConnection := SQLConnection;
    sSQL := 'SELECT c.relname, pg_catalog.pg_get_userbyid(c.relowner) AS relowner, ' +
      'pg_catalog.obj_description(c.oid, ''(pg_class'') AS relcomment, ' +
      '(SELECT spcname FROM pg_catalog.pg_tablespace pt WHERE pt.oid=c.reltablespace) AS tablespace ' +
      'FROM pg_catalog.pg_class c ' +
      'LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace ' +
      'WHERE c.relkind = ''r'' ' +
      'AND nspname=''public'' ' +
      'ORDER BY c.relname';
    query.SQL.Clear;
    query.SQL.Add(sSQL);
    query.Open;
    while Not(query.Eof) do
      begin
        lTable := TtiDBMetaDataTable.Create ;
        lTable.Name := query.FieldValues['relname'];
        lTable.ObjectState := posPK ;
        lMetaData.Add( lTable ) ;
        lMetaData.ObjectState := posClean ;
        query.Next;
      end;
    query.Close;
(*
    tiFilesToStringList( DatabaseName,
                         '*.db',
                         lsl,
                         false ) ;
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create ;
      lTable.Name := tiExtractFileNameOnly( lsl.Strings[i] ) ;
      lTable.ObjectState := posPK ;
      lMetaData.Add( lTable ) ;
      lMetaData.ObjectState := posClean ;
    end ;
*)
  finally
    lsl.Free;
    query.Free;
  end ;
end;

procedure TtiDatabaseDBEPostgreSQL.RollBack;
begin
  inherited;
end;

procedure TtiDatabaseDBEPostgreSQL.SetupDBParams;
begin
  SQLConnection.DriverName := 'PostgreSQL';
  SQLConnection.LoginPrompt := False;
  SQLConnection.LibraryName := 'dbexppge.dll';
  SQLConnection.VendorLib := 'libpq_pge.dll';
  SQLConnection.GetDriverFunc := 'getSQLDriverPOSTGRESQL';
  SQLConnection.Params.Clear;
  SQLConnection.Params.Add('Database=' + DatabaseName);
  SQLConnection.Params.Add('User_Name=' + UserName);
  SQLConnection.Params.Add('Password=' + Password);
end;

function TtiDatabaseDBEPostgreSQL.Test: boolean;
begin
  result := false ;
  Assert( false, 'Under construction' ) ;
end;

procedure TtiDatabaseDBEPostgreSQL.CreateTable( const pTableMetaData: TtiDBMetaDataTable);
var
  lSQL : string ;
  i : integer ;
begin

  lSQL := '' ;
  for i := 0 to pTableMetaData.Count - 1 do
  begin
    lSQL := tiAddTrailingValue( lSQL, ',' + CrLf ) ;
    lSQL := lSQL +
            pTableMetaData.Items[i].Name + ' ' +
            FieldMetaDataToSQLCreate( pTableMetaData.Items[i] ) ;
  end ;
  lSQL := 'create table ' + pTableMetaData.Name + CrLf + '(' + CrLf +
          lSQL + CrLf +
          ') without oids' ;
  try
    ExecSQL( lSQL ) ;
  except
    on e:exception do
      raise EtiOPFInternalException('Unable to execute create SQL : ' + e.Message) ;
  end;

end;

initialization
  gTIOPFManager.RegPerLayers.__RegisterPersistenceLayer(
              cTIPersistDBEPostgreSQL,
              TtiDBConnectionPoolDataAbs,
              TtiQueryDBEPostgreSQL,
              TtiDatabaseDBEPostgreSQL );
finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.RegPerLayers.__UnRegisterPersistenceLayer( cTIPersistDBEPostgreSQL ) ;
end.

