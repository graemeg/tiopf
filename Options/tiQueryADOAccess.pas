unit tiQueryADOAccess;

{$I tiDefines.inc}

interface
uses
  Classes
  ,ADODb
  ,tiQueryADOAbs
  ,tiQuery
  ,tiPersistenceLayers
 ;

type

  TtiPersistenceLayerADOAccess = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseADOAccess = class(TtiDatabaseADOAbs)
  protected
    function    GetConnectionString: string; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData : TtiDBMetaDataField): string; override;
  public
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string): boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    procedure       ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
  end;

  TtiQueryADOAccess = class(TtiQueryADO);


implementation
uses
   tiObject
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  ,DB
  ,ADOX_TLB
 ;

const
  GLOBAL_PASSWORD='GLOBAL_PASSWORD';
  cADOProvider   = 'Provider=Microsoft.Jet.OLEDB.4.0';

  cAccessFileVersion2    = '3';
  cAccessFileVersion95   = '4';
  cAccessFileVersion2000 = '5';
  cAccessFileVersion2002 = '6';
  // Set this constant for the MSAccess file version created by CreateDatabase
  cAccessFileVersion     = cAccessFileVersion2000;


//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDatabaseADOAccess
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
class procedure TtiDatabaseADOAccess.CreateDatabase(const ADatabaseName, AUserName, APassword: string);
var
  lADOXCatalog : TADOXCatalog;
  lConnectionString : string;
begin
  if not DirectoryExists(ExtractFilePath(ADatabaseName)) then
    tiForceDirectories(ExtractFilePath(ADatabaseName));

  // http://delphi.about.com/library/weekly/aa072401b.htm
  lConnectionString :=
    cADOProvider +
    ';Data Source=' + ADatabaseName +
    ';Jet OLEDB:Engine Type=' + cAccessFileVersion;

  lADOXCatalog := TADOXCatalog.Create(nil);
  try
    lADOXCatalog.Create1(lConnectionString);
  finally
    lADOXCatalog.Free;
  end;
end;

class function TtiDatabaseADOAccess.DatabaseExists(const ADatabaseName,AUserName, APassword: string):boolean;
var
  lDatabase : TtiDatabaseADOAccess;
begin
  lDatabase := TtiDatabaseADOAccess.Create;
  try
    try
      lDatabase.Connect(ADatabaseName, AUserName, APassword, '');
      result := true;
    except
      on e:exception do
        result := false;
    end;
    lDatabase.Connected := false;
  finally
    lDatabase.Free;
  end;
end;

function TtiDatabaseADOAccess.FieldMetaDataToSQLCreate(
  const AFieldMetaData: TtiDBMetaDataField): string;
begin
  case AFieldMetaData.Kind of
    qfkString    : result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger   : result := 'Integer';
    qfkFloat     : result := 'Numeric(11, 5)';
    qfkDateTime  : result := 'Date';
    qfkLogical   : result := 'Logical';
    qfkBinary    : result := 'Image';
    qfkLongString : result := 'Memo';
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

function TtiDatabaseADOAccess.GetConnectionString: string;
var
  pwd_str: string;
begin
  if SameText(UserName, GLOBAL_PASSWORD) then
  begin
    pwd_str:=Format('Jet OLEDB:Database Password=%s;', [Password]);
  end
  else if (not SameText(UserName, 'null')) and
          (UserName <> '') then
  begin
    pwd_str:=Format('Password=%s;User ID=%s;', [Password, UserName]);
  end
  else pwd_str:='';
  result :=
    Format(cADOProvider + ';' +
            pwd_str +
            'Data Source=%s;' +
            'Persist Security Info=False',
            [DatabaseName]);
end;

procedure TtiDatabaseADOAccess.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable : TtiDBMetaDataTable;
  lField : TtiDBMetaDataField;
  lDelphiTable : TADOTable;
  i : integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lDelphiTable := TADOTable.Create(nil);
  try
    lDelphiTable.Connection := Connection;
    lDelphiTable.TableName := lTable.Name;
    {$IFDEF DELPHI5}
    // Table must be active, but only with D5
    lDelphiTable.Active := true;
    {$ENDIF}
    lDelphiTable.FieldDefs.Update;
    for i := 0 to lDelphiTable.FieldDefs.Count - 1 do
    begin
      lField := TtiDBMetaDataField.Create;
      lField.Name := lDelphiTable.FieldDefs[i].Name;
      lField.Kind := FieldDataTypeToTIQueryFieldKind(lDelphiTable.FieldDefs[i].DataType);
      if lField.Kind = qfkString then
        lField.Width := lDelphiTable.FieldDefs[i].Size
      else
        lField.Width := 0;
      lField.ObjectState := posClean;
      lTable.Add(lField);
    end;
    lTable.ObjectState := posClean;
    {$IFDEF DELPHI5}
    try
      // This call will raise an exception under D5 if the record set is empty.
      lDelphiTable.Active := false;
    except end;
    {$ENDIF}
  finally
    lDelphiTable.Free;
  end;
end;

procedure TtiDatabaseADOAccess.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
  lsl : TStringList;
  i : integer;
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


{ Piero Bonanno: 18/11/2001
  I've downloaded your latest version of tiOPF. I've noted that there
  isn't support for username and password, so I've added it (but I think
  is not much used in Access). Since, in my opinion, global password for
  Access (that is, a password for the entire db), I've added a little
  trick: the global password can be specified usig 'GLOBAL_PASSWORD' as
  username. It works, but I think should be support for custom params
  (like roles in IB). The TtiDatabase could have a TStringList for the
  custom params (accessed by CustomParams.Values[<param_name>]).}

function TtiDatabaseADOAccess.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseADOAccess.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryADOAccess;
end;

{ TtiPersistenceLayerADOAccess }

procedure TtiPersistenceLayerADOAccess.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistADOAccess;
  APersistenceLayerDefaults.DatabaseName:= 'Demo.mdb';
  APersistenceLayerDefaults.UserName:= 'null';
  APersistenceLayerDefaults.Password:= 'null';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= False;
  APersistenceLayerDefaults.CanSupportSQL:= True;
end;

function TtiPersistenceLayerADOAccess.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseADOAccess;
end;

function TtiPersistenceLayerADOAccess.GetPersistenceLayerName: string;
begin
  result:= cTIPersistADOAccess;
end;

function TtiPersistenceLayerADOAccess.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryADOAccess;
end;

Initialization
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerADOAccess);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistADOAccess);

end.
