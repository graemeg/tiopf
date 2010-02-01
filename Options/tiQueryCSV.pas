unit tiQueryCSV;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiDataBuffer_BOM
  ,tiQueryTXTAbs
  ,tiPersistenceLayers
 ;

type

  TtiPersistenceLayerCSV = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDatabaseCSV = class(TtiDatabaseTXTFlatFileAbs)
  protected
    procedure   SaveDataSet(const pDataSet : TtiDataBuffer); override;
    procedure   ReadDataSet(const pDataSet : TtiDataBuffer); override;
  public
    constructor Create; override;
    function    TIQueryClass: TtiQueryClass; override;
  end;

  TtiQueryCSV = class(TtiQueryTXTAbs)
  public
    constructor Create; override;
    procedure   SelectRow(const ATableName : string; const AWhere : TtiQueryParams = nil); override;
    procedure   InsertRow(const ATableName : string; const AParams : TtiQueryParams); override;
    procedure   DeleteRow(const ATableName : string; const AWhere : TtiQueryParams = nil); override;
    procedure   UpdateRow(const ATableName : string; const AParams : TtiQueryParams; const AWhere : TtiQueryParams); override;
  end;

implementation
uses
  tiOPFManager
  ,tiConstants
  ,tiXML
  ,tiExcept
  ,SysUtils
 ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseCSV
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{ TODO :
SaveDataSet and ReadDataSet should probably be in the parent class.
The only difference is that the particular writer class
(in this case TCSVToTIDatatSet) is required.  Perhaps this could be
a classtype value set in the TtiDatabase??? constructor.
eg.
  lWriter := TTXTToTIDataSetClass.Create;
 }

procedure TtiDatabaseCSV.SaveDataSet(const pDataSet: TtiDataBuffer);
var
  lWriter : TCSVToTIDataSet;
  lFileName : string;
begin
  lWriter := TCSVToTIDataSet.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name);
    lWriter.Save(pDataSet, lFileName);
  finally
    lWriter.Free;
  end;
  SetDirty(pDataSet, False);
end;

function TtiDatabaseCSV.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryCSV;
end;

procedure TtiDatabaseCSV.ReadDataSet(const pDataSet: TtiDataBuffer);
var
  lFileName : string;
  lWriter : TCSVToTIDataSet;
begin
  lWriter := TCSVToTIDataSet.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name);
    pDataSet.Clear;
    lWriter.Read(pDataSet, lFileName);
  finally
    lWriter.Free;
  end;
  SetDirty(pDataSet, False);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *
 * TtiQueryCSV
 *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{ TODO :
SelectRow, etc. should probably be in the parent class
since it is generic except for reference to TtiDatabaseCSV.
cf  TODO for SaveDataSet above.
 }
procedure TtiQueryCSV.SelectRow(const ATableName: string;const AWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
begin
  lDataSet := (Database as TtiDatabaseCSV).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFInternalException.Create('Unable to find table <' + ATableName + '>');
  DoSelectRows(lDataSet, AWhere);
  CurrentRecordIndex := 0;
end;

procedure TtiQueryCSV.DeleteRow(const ATableName: string; const AWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  i : integer;
  lSetDirty : boolean;
begin
  lDataSet := (Database as TtiDatabaseCSV).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFInternalException.Create('Unable to find table <' + ATableName + '>');
  DoSelectRows(lDataSet, AWhere);
  lSetDirty := SelectedRows.Count > 0;
  for i := SelectedRows.Count - 1 downto 0 do
    lDataSet.Remove(TtiDataBufferRow(SelectedRows.Items[i]));
  if lSetDirty then
    TtiDatabaseCSV(Database).SetDirty(lDataSet,true);
  SelectedRows.Clear;
end;

procedure TtiQueryCSV.InsertRow(const ATableName: string; const AParams: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  lDataSetRow : TtiDataBufferRow;
begin
  lDataSet := (Database as TtiDatabaseCSV).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFInternalException.Create('Unable to find table <' + ATableName + '>');
  lDataSetRow := lDataSet.AddInstance;
  DoUpdateRow(lDataSetRow, AParams);
  (Database as TtiDatabaseCSV).SetDirty(lDataSet,true);
end;

procedure TtiQueryCSV.UpdateRow(const ATableName: string; const AParams, AWhere : TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  i : integer;
  lSetDirty : boolean;
begin
  lDataSet := TtiDatabaseCSV(Database).FindDataSetByName(ATableName);
  if lDataSet = nil then
     raise EtiOPFInternalException.Create('Unable to find table <' + ATableName + '>');
  DoSelectRows(lDataSet, AWhere);
  lSetDirty := SelectedRows.Count > 0;
  for i := 0 to SelectedRows.Count - 1 do
    DoUpdateRow(TtiDataBufferRow(SelectedRows.Items[i]), AParams);
  if lSetDirty then
    TtiDatabaseCSV(Database).SetDirty(lDataSet,true);
  SelectedRows.Clear;
end;

constructor TtiDatabaseCSV.Create;
begin
  inherited;
  FilenameExt := 'CSV';
end;

constructor TtiQueryCSV.Create;
begin
  inherited;
  FReservedChars := rcCSV;
end;

{ TtiPersistenceLayerCSV }

procedure TtiPersistenceLayerCSV.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= CTIPersistCSV;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseDirectory + CDefaultDatabaseName + 'CSV';
  APersistenceLayerDefaults.IsDatabaseNameFilePath:= True;
  APersistenceLayerDefaults.Username:= 'null';
  APersistenceLayerDefaults.Password:= 'null';
  APersistenceLayerDefaults.CanDropDatabase:= False;
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= False;
  APersistenceLayerDefaults.CanSupportSQL:= False;
end;

function TtiPersistenceLayerCSV.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseCSV;
end;

function TtiPersistenceLayerCSV.GetPersistenceLayerName: string;
begin
  result:= cTIPersistCSV;
end;

function TtiPersistenceLayerCSV.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryCSV;
end;

initialization
  GTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerCSV);

finalization
  if not tiOPFManager.ShuttingDown then
    GTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistCSV);


end.
