{
  Connection format:   <server>\<instance>:<database>
    eg:    localhost\SQLExpress:panacee

}
unit tiQueryADOSQLServer;

{$I tiDefines.inc}

interface
uses
  Classes,
  ADODb,
  tiQueryADOAbs,
  tiQuery,
  tiPersistenceLayers,
  tiDBConnectionPool
  ;

type

  TtiPersistenceLayerADOSQLServer = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseADOSQLServer = class(TtiDatabaseADOAbs)
  private
  protected
    function    GetConnectionString: string; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData : TtiDBMetaDataField): string; override;
  public
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams:string = ''): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams:string = ''); override;
    procedure       ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
  end;

  TtiQueryADOSQLServer = class(TtiQueryADO);

implementation

uses
   tiObject
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
 ;

{ TtiDatabaseADOSQLServer }

class procedure TtiDatabaseADOSQLServer.CreateDatabase(const ADatabaseName, AUserName, APassword: string; const AParams: string);
begin
  Assert(false, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseADOSQLServer.DatabaseExists(const ADatabaseName, AUserName, APassword: string; const AParams: string): boolean;
var
  lDatabase: TtiDatabaseADOSQLServer;
begin
  lDatabase := TtiDatabaseADOSQLServer.Create;
  try
    try
      lDatabase.Connect(ADatabaseName, AUserName, APassword, '');
      Result := True;
    except
      on E: Exception do
        Result := False;
    end;
    lDatabase.Connected := False;
  finally
    lDatabase.Free;
  end;
end;

function TtiDatabaseADOSQLServer.FieldMetaDataToSQLCreate(
  const AFieldMetaData: TtiDBMetaDataField): string;
begin
  case AFieldMetaData.Kind of
    qfkString    : result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger   : result := 'Integer';
    qfkFloat     : result := 'float';
    qfkDateTime  : result := 'datetime';
    qfkLogical   : result := 'bit';
    qfkBinary    : result := 'varbinary(max)'; // the 'image' data type is considered deprecated by Microsoft
    qfkLongString : result := 'text';
  else
    raise Exception.Create('Invalid FieldKind');
  end;
end;

function TtiDatabaseADOSQLServer.GetConnectionString: string;
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
    raise Exception.Create('Invalid DatabaseName.');

  if UpperCase(UserName) <> 'NULL' then
  begin
    UserNameString :=
      'User ID=' + UserName + ';' +
      'Password=' + Password + ';';
  end
  else
    UserNameString := 'Integrated Security=SSPI;';

  Result :=
    'Provider=SQLOLEDB.1;' +
    'Persist Security Info=False;' +
    'Initial Catalog=' + l_Database + ';' +
    UserNameString +
    'Data Source=' + l_Server;
end;

procedure TtiDatabaseADOSQLServer.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lDelphiTable: TADOTable;
  i: integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lDelphiTable := TADOTable.Create(nil);
  try
    lDelphiTable.Connection := Connection;
    lDelphiTable.TableName := lTable.Name;
    lDelphiTable.FieldDefs.Update;
    for i := 0 to lDelphiTable.FieldDefs.Count - 1 do
    begin
      lField := TtiDBMetaDataField.Create;
      lField.Name := lDelphiTable.FieldDefs[i].Name;
      lField.ObjectState := posClean;
      lTable.Add(lField);
    end;
    lTable.ObjectState := posClean;
  finally
    lDelphiTable.Free;
  end;
end;

procedure TtiDatabaseADOSQLServer.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
  lsl: TStringList;
  i: integer;
begin
  lMetaData := (AData as TtiDBMetaData);
  lsl := TStringList.Create;
  try
    Connection.GetTableNames(lsl, false);
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create;
      lTable.Name := lsl.Strings[i];
      lTable.ObjectState := posPK;
      lMetaData.Add(lTable);
      lMetaData.ObjectState := posClean;
    end;
  finally
    lsl.Free;
  end;
end;

function TtiDatabaseADOSQLServer.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseADOSQLServer.TIQueryClass: TtiQueryClass;
begin
  result := TtiQueryADOSQLServer;
end;

{ TtiPersistenceLayerADOSQLServer }

procedure TtiPersistenceLayerADOSQLServer.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistADOSQLServer;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + '.XXX';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username:= 'XXX';
  APersistenceLayerDefaults.Password:= 'XXX';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerADOSQLServer.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseADOSQLServer;
end;

function TtiPersistenceLayerADOSQLServer.GetPersistenceLayerName: string;
begin
  result:= cTIPersistADOSQLServer;
end;

function TtiPersistenceLayerADOSQLServer.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryADOSQLServer;
end;

Initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerADOSQLServer);

finalization
  if not tiOPFManager.ShuttingDown then
   GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistADOSQLServer);

end.


