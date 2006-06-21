unit tiQueryADOSQLServer;

{$I tiDefines.inc}

interface
uses
  Classes,
  ADODb,
  tiQueryADOAbs,
  tiQuery;

type
  TtiDatabaseADOSQLServer = class(TtiDatabaseADOAbs)
  private
  protected
    procedure   SetupDBParams ; override ;
    function    FieldMetaDataToSQLCreate( const pFieldMetaData : TtiDBMetaDataField ) : string ; override ;
  public
    class function  DatabaseExists( const psDatabaseName, psUserName, psPassword : string ):boolean ; override ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    procedure       ReadMetaDataTables( pData : TtiDBMetaData ) ; override ;
    procedure       ReadMetaDataFields( pData : TtiDBMetaDataTable ) ; override ;
    function        Test : boolean ; override ;
  end ;

  TtiQueryADOSQLServer = class(TtiQueryADO);

const
  cTIPersistADOSQLServer = 'ADOSQLServer';


implementation

uses
   tiDBConnectionPool
  ,tiObject
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  ;

{ TtiDatabaseADOSQLServer }

class procedure TtiDatabaseADOSQLServer.CreateDatabase(const psDatabaseName, psUserName, psPassword: string);
begin
  Assert( false, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseADOSQLServer.DatabaseExists(const psDatabaseName, psUserName, psPassword: string):boolean;
begin
  result := false ;
  Assert( false, 'DatabaseExists not implemented in ' + ClassName);
end;

function TtiDatabaseADOSQLServer.FieldMetaDataToSQLCreate(
  const pFieldMetaData: TtiDBMetaDataField): string;
begin
  case pFieldMetaData.Kind of
    qfkString     : result := 'VarChar( ' + IntToStr( pFieldMetaData.Width ) + ' )' ;
    qfkInteger    : result := 'Integer' ;
    qfkFloat      : result := 'float' ;
    qfkDateTime   : result := 'datetime' ;
    qfkLogical    : result := 'bit';
    qfkBinary     : result := 'Image' ;
    qfkLongString : result := 'text' ;
  else
    raise Exception.Create( 'Invalid FieldKind') ;
  end ;
end;

procedure TtiDatabaseADOSQLServer.ReadMetaDataFields(pData: TtiDBMetaDataTable);
var
  lTable : TtiDBMetaDataTable ;
  lField : TtiDBMetaDataField ;
  lDelphiTable : TADOTable ;
  i : integer ;
begin
  lTable := ( pData as TtiDBMetaDataTable ) ;
  lDelphiTable := TADOTable.Create( nil ) ;
  try
    lDelphiTable.Connection := Connection ;
    lDelphiTable.TableName := lTable.Name ;
    lDelphiTable.FieldDefs.Update ;
    for i := 0 to lDelphiTable.FieldDefs.Count - 1 do
    begin
      lField := TtiDBMetaDataField.Create ;
      lField.Name := lDelphiTable.FieldDefs[i].Name ;
      lField.ObjectState := posClean ;
      lTable.Add( lField ) ;
    end ;
    lTable.ObjectState := posClean ;
  finally
    lDelphiTable.Free;
  end ;
end;

procedure TtiDatabaseADOSQLServer.ReadMetaDataTables(pData: TtiDBMetaData);
var
  lMetaData : TtiDBMetaData ;
  lTable : TtiDBMetaDataTable ;
  lsl : TStringList ;
  i : integer ;
begin
  lMetaData := ( pData as TtiDBMetaData ) ;
  lsl := TStringList.Create ;
  try
    Connection.GetTableNames(lsl, false);
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create ;
      lTable.Name := lsl.Strings[i] ;
      lTable.ObjectState := posPK ;
      lMetaData.Add( lTable ) ;
      lMetaData.ObjectState := posClean ;
    end ;
  finally
    lsl.Free;
  end ;
end;

procedure TtiDatabaseADOSQLServer.SetupDBParams;
var
  UserNameString: string;
  l_Server,
  l_Database: string;
  l_Delimiter: integer;
begin
  // DatabaseName should be SERVERNAME:DATABASE
  l_Delimiter := Pos(':',DatabaseName);

  if l_Delimiter > 0 then
  begin
    l_Server := Copy(DatabaseName,1,l_Delimiter - 1);
    l_Database := Copy(DatabaseName,l_Delimiter + 1,Length(DatabaseName));
  end
  else
    raise Exception.Create( 'Invalid DatabaseName.' );
  Connection.LoginPrompt := false;
  Connection.IsolationLevel := ilReadCommitted ;

  if UpperCase(UserName) <> 'NULL' then
  begin
    UserNameString :=
      'User ID=' + UserName + ';' +
      'Password=' + Password + ';';
  end
  else
    UserNameString := 'Integrated Security=SSPI;';

  Connection.ConnectionString :=
    'Provider=SQLOLEDB.1;' +
    'Persist Security Info=False;' +
    'Initial Catalog=' + l_Database + ';' +
    UserNameString +
    'Data Source=' + l_Server;

end;

function TtiDatabaseADOSQLServer.Test: boolean;
begin
  result := false ;
  Assert( false, 'Under construction' ) ;
end;

Initialization
   gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
               cTIPersistADOSQLServer,
               TtiDBConnectionPoolDataAbs,
               TtiQueryADOSQLServer,
               TtiDatabaseADOSQLServer ) ;

finalization
  if not tiOPFManager.ShuttingDown then
   gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer( cTIPersistADOSQLServer ) ;

end.

