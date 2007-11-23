unit tiQueryBDEParadox;

{$I tiDefines.inc}

interface
uses
  Classes
  ,dbTables
  ,tiQuery
  ,tiQueryBDEAbs
  ,tiPersistenceLayers
 ;

type

  TtiPersistenceLayerBDEParadox = class(TtiPersistenceLayer)
  protected
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetPersistenceLayerName: string; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseBDEParadox = class(TtiDatabaseBDEAbs)
  protected
    procedure   SetupDBParams; override;
    function    FieldMetaDataToSQLCreate(const AFieldMetaData : TtiDBMetaDataField): string; override;
  public
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string):boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    procedure       ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure       ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    procedure       RollBack; override; // Not supported in BDE
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
  end;

  TtiQueryBDEParadox = class(TtiQueryBDE);


implementation
uses
   tiObject
  ,tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiExcept
  ,SysUtils
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
 ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDatabaseBDEParadox
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
class procedure TtiDatabaseBDEParadox.CreateDatabase(const ADatabaseName,AUserName, APassword: string);
var
  lDir : string;
begin
  lDir := ExpandFileName(ADatabaseName);
  if DatabaseExists(lDir,AUserName,APassword) then
    raise EtiOPFDBExceptionAlreadyExists.Create(cTIPersistBDEParadox, ADatabaseName, AUserName, APassword);
  if not ForceDirectories(lDir) then
    raise EtiOPFDBExceptionCanNotCreateDatabase.Create(cTIPersistBDEParadox, ADatabaseName, AUserName, APassword);
end;

class function TtiDatabaseBDEParadox.DatabaseExists(const ADatabaseName,AUserName, APassword: string): boolean;
var
  lDir : string;
begin
  lDir := ExpandFileName(ADatabaseName);
  result := DirectoryExists(lDir);
end;

function TtiDatabaseBDEParadox.FieldMetaDataToSQLCreate(const AFieldMetaData: TtiDBMetaDataField): string;
begin
  case AFieldMetaData.Kind of
    qfkString    : result := 'VarChar(' + IntToStr(AFieldMetaData.Width) + ')';
    qfkInteger   : result := 'Integer' ;
    qfkFloat     : result := 'Numeric(11, 5)';
    qfkDateTime  : result := 'TimeStamp' ;
    qfkLogical   : result := 'Boolean' ;
    qfkBinary    : result := 'Blob(1, 2)' ;
    qfkLongString : result := 'Blob(1, 1)' ;
  else
    raise EtiOPFInternalException.Create('Invalid FieldKind');
  end;
end;

procedure TtiDatabaseBDEParadox.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable : TtiDBMetaDataTable;
  lField : TtiDBMetaDataField;
  lDelphiTable : TTable;
  i : integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lDelphiTable := TTable.Create(nil);
  try
    lDelphiTable.DatabaseName := DatabaseName;
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

procedure TtiDatabaseBDEParadox.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
  lsl : TStringList;
  i : integer;
begin
  lMetaData := (AData as TtiDBMetaData);
  lsl := TStringList.Create;
  try
    tiFilesToStringList(DatabaseName,
                         '*.db',
                         lsl,
                         false);
    for i := 0 to lsl.Count - 1 do
    begin
      lTable := TtiDBMetaDataTable.Create;
      lTable.Name := tiExtractFileNameOnly(lsl.Strings[i]);
      lTable.ObjectState := posPK;
      lMetaData.Add(lTable);
      lMetaData.ObjectState := posClean;
    end;
  finally
    lsl.Free;
  end;
end;

procedure TtiDatabaseBDEParadox.RollBack;
begin
  inherited;
{
  In "borland.public.cppbuilder.database" found this:
  <quote>
  The following limitations apply to local transactions:
      Automatic crash recovery is not provided.
      Data definition statements are not supported.
      For Paradox, local transactions can only be performed on tables with
  valid indexes. Data cannot be rolled back on Paradox tables that do not
  have indexes.
      Transactions cannot be run against temporary tables.
      Transactions cannot be run against the BDE ASCII driver.
      TransIsolation level must only be set to tiDirtyRead.

  Closing a cursor on a table during a transaction rolls back the
  transaction unless:

      Several tables are open.
      The cursor is closed on a table to which no changes were made.
  </quote>
}
end;

procedure TtiDatabaseBDEParadox.SetupDBParams;
begin
  Database.TransIsolation := tiDirtyRead;

  // Use this line if you want to use an existing BDE alias
  //Database.AliasName := DatabaseName;

  // Use these lines if you want to create the BDE alias on the fly
  Database.DriverName := 'STANDARD';
  Database.Params.Values[ 'PATH' ]:= DatabaseName;
  Database.Params.Values[ 'DEFAULT DRIVER' ]:= 'PARADOX';

end;

function TtiDatabaseBDEParadox.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseBDEParadox.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryBDEParadox;
end;

{ TtiPersistenceLayerBDEParadox }

procedure TtiPersistenceLayerBDEParadox.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistBDEParadox;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + 'BDEParadox';
  APersistenceLayerDefaults.Username:= 'null';
  APersistenceLayerDefaults.Password:= 'null';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= False;
end;

function TtiPersistenceLayerBDEParadox.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseBDEParadox;
end;

function TtiPersistenceLayerBDEParadox.GetPersistenceLayerName: string;
begin
  result:= cTIPersistBDEParadox;
end;

function TtiPersistenceLayerBDEParadox.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryBDEParadox;
end;

Initialization
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerBDEParadox);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistBDEParadox);

end.



