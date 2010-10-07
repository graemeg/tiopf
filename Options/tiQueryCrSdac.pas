unit tiQueryCrSdac;

{$I tiDefines.inc}

interface
uses
  Classes,
  DBAccess, MSAccess,
  tiQueryCrAbs,
  tiQuery,
  tiPersistenceLayers,
  tiDBConnectionPool
  ;

type

  TtiPersistenceLayerCrSdac = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseCrSdac = class(TtiDatabaseCrAbs)
  private
  protected
    function    GetConnectionString: string; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData : TtiDBMetaDataField): string; override;
  public
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string):boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    procedure       ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
  end;


implementation

uses
   tiObject
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
 ;

{ TtiDatabaseCrSdac }

class procedure TtiDatabaseCrSdac.CreateDatabase(const ADatabaseName, AUserName, APassword: string);
begin
  Assert(false, 'CreateDatabase not implemented in ' + ClassName);
end;

class function TtiDatabaseCrSdac.DatabaseExists(const ADatabaseName, AUserName, APassword: string):boolean;
var
  lDatabase: TtiDatabaseCrSdac;
begin
  lDatabase := TtiDatabaseCrSdac.Create;
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

function TtiDatabaseCrSdac.FieldMetaDataToSQLCreate(
  const AFieldMetaData: TtiDBMetaDataField): string;
begin
  case AFieldMetaData.Kind of
    qfkString    : result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger   : result := 'Integer';
    qfkFloat     : result := 'float';
    qfkDateTime  : result := 'datetime';
    qfkLogical   : result := 'bit';
    qfkBinary    : result := 'Image';
    qfkLongString : result := 'text';
  else
    raise Exception.Create('Invalid FieldKind');
  end;
end;

function TtiDatabaseCrSdac.GetConnectionString: string;
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

procedure TtiDatabaseCrSdac.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable: TtiDBMetaDataTable;
  lField: TtiDBMetaDataField;
  lDelphiTable: TMSTable;
  i: integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lDelphiTable := TMSTable.Create(nil);
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

procedure TtiDatabaseCrSdac.ReadMetaDataTables(AData: TtiDBMetaData);
var
  posDot: integer;
  lMetaData: TtiDBMetaData;
  lTable: TtiDBMetaDataTable;
  lsl: TStringList;
  i: integer;
  tableName: string;
begin
  lMetaData := (AData as TtiDBMetaData);
  lsl := TStringList.Create;
  try
    Connection.GetTableNames(lsl);
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create;
      tableName:= lsl.Strings[i];
      posDot := Pos('.', tableName);

      lTable.Name := Copy(lsl.Strings[i], posDot + 1, MaxInt);
      lTable.ObjectState := posPK;
      lMetaData.Add(lTable);
      lMetaData.ObjectState := posClean;
    end;
  finally
    lsl.Free;
  end;
end;

function TtiDatabaseCrSdac.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseCrSdac.TIQueryClass: TtiQueryClass;
begin
  result := TtiQueryCrSdac;
end;

{ TtiPersistenceLayerCrSdac }

procedure TtiPersistenceLayerCrSdac.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistCrSdac;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + '.XXX';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username:= 'XXX';
  APersistenceLayerDefaults.Password:= 'XXX';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanSupportMultiUser:= True;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerCrSdac.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseCrSdac;
end;

function TtiPersistenceLayerCrSdac.GetPersistenceLayerName: string;
begin
  result:= cTIPersistCrSdac;
end;

function TtiPersistenceLayerCrSdac.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryCrSdac;
end;

Initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerCrSdac);

finalization
  if not tiOPFManager.ShuttingDown then
   GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistCrSdac);

end.


