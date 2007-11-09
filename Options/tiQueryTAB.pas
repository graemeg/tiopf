{$I tiDefines.inc}

unit tiQueryTAB;

interface
uses
   tiQuery
  ,tiDataBuffer_BOM
  ,tiQueryTXTAbs
  ,tiPersistenceLayers
  ,tiDBConnectionPool
 ;

type

  TtiPersistenceLayerTab = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  end;

  TtiDBConnectionPoolDataTAB = Class(TtiDBConnectionPoolDataTXTAbs);

  TtiDatabaseTAB = class(TtiDatabaseTXTFlatFileAbs)
  protected
    procedure   SaveDataSet(const pDataSet : TtiDataBuffer); override;
    procedure   ReadDataSet(const pDataSet : TtiDataBuffer); override;
  public
    constructor Create; override;
    function    TIQueryClass: TtiQueryClass; override;
  end;

  TtiQueryTAB = class(TtiQueryTXTAbs)
  protected
  public
    constructor Create; override;
    procedure   SelectRow(const ATableName : string; const AWhere : TtiQueryParams = nil); override;
    procedure   InsertRow(const ATableName : string; const AParams : TtiQueryParams); override;
    procedure   DeleteRow(const ATableName : string; const AWhere : TtiQueryParams = nil); override;
    procedure   UpdateRow(const ATableName : string; const AParams : TtiQueryParams; const AWhere : TtiQueryParams); override;
  end;


implementation
uses
   tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiXML
  ,tiExcept
  ,SysUtils
 ;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseTAB
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{ TODO :
SaveDataSet and ReadDataSet should probably be in the parent class.
The only difference is that the particular writer class
(in this case TTABToTIDatatSet) is required.  Perhaps this could be
a classtype value set in the TtiDatabase??? constructor.
eg.
  lWriter := TTXTToTIDataSetClass.Create;
 }

procedure TtiDatabaseTAB.SaveDataSet(const pDataSet: TtiDataBuffer);
var
  lWriter : TTABToTIDataSet;
  lFileName : string;
begin
  lWriter := TTABToTIDataSet.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name);
    lWriter.Save(pDataSet, lFileName);
  finally
    lWriter.Free;
  end;
end;

function TtiDatabaseTAB.TIQueryClass: TtiQueryClass;
begin
  result:=TtiQueryTAB;
end;

procedure TtiDatabaseTAB.ReadDataSet(const pDataSet: TtiDataBuffer);
var
  lFileName : string;
  lWriter : TTABToTIDataSet;
begin
  lWriter := TTABToTIDataSet.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name);
    pDataSet.Clear;
    lWriter.Read(pDataSet, lFileName);
  finally
    lWriter.Free;
  end;
end;



{ TtiQueryTAB }

{ TODO :
SelectRow, etc. should probably be in the parent class
since it is generic except for reference to TtiDatabaseTAB.
cf  TODO for SaveDataSet above.
 }
procedure TtiQueryTAB.SelectRow(const ATableName: string;const AWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
begin
  lDataSet := (Database as TtiDatabaseTAB).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise Exception.Create('Unable to find table <' + ATableName + '>');
  DoSelectRows(lDataSet, AWhere);
  CurrentRecordIndex := 0;
end;

procedure TtiQueryTAB.DeleteRow(const ATableName: string; const AWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  i : integer;
  lSetDirty : boolean;
begin
  lDataSet := (Database as TtiDatabaseTAB).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFInternalException.Create('Unable to find table <' + ATableName + '>');
  DoSelectRows(lDataSet, AWhere);
  lSetDirty := SelectedRows.Count > 0;
  for i := SelectedRows.Count - 1 downto 0 do
    lDataSet.Remove(TtiDataBufferRow(SelectedRows.Items[i]));
  if lSetDirty then
    (Database as TtiDatabaseTAB).SetDirty(lDataSet,true);
  SelectedRows.Clear;
end;

procedure TtiQueryTAB.InsertRow(const ATableName: string; const AParams: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  lDataSetRow : TtiDataBufferRow;
begin
  lDataSet := (Database as TtiDatabaseTAB).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFInternalException.Create('Unable to find table <' + ATableName + '>');
  lDataSetRow := lDataSet.AddInstance;
  DoUpdateRow(lDataSetRow, AParams);
  (Database as TtiDatabaseTAB).SetDirty(lDataSet,true);
end;

procedure TtiQueryTAB.UpdateRow(const ATableName: string; const AParams, AWhere : TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  i : integer;
  lSetDirty : boolean;
begin
  lDataSet := TtiDatabaseTAB(Database).FindDataSetByName(ATableName);
  if lDataSet = nil then
     raise EtiOPFInternalException.Create('Unable to find table <' + ATableName + '>');
  DoSelectRows(lDataSet, AWhere);
  lSetDirty := SelectedRows.Count > 0;
  for i := 0 to SelectedRows.Count - 1 do
    DoUpdateRow(TtiDataBufferRow(SelectedRows.Items[i]), AParams);
  if lSetDirty then
    TtiDatabaseTAB(Database).SetDirty(lDataSet,true);
  SelectedRows.Clear;
end;

constructor TtiDatabaseTAB.Create;
begin
  inherited;
  FilenameExt := 'TXT';
end;

constructor TtiQueryTAB.Create;
begin
  inherited;
  FReservedChars := rcTAB;
end;

{ TtiPersistenceLayerTab }

function TtiPersistenceLayerTab.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseTAB;
end;

function TtiPersistenceLayerTab.GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
begin
  result:= TtiDBConnectionPoolDataTAB;
end;

function TtiPersistenceLayerTab.GetPersistenceLayerName: string;
begin
  result:= cTIPersistTAB;
end;

function TtiPersistenceLayerTab.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryTAB;
end;

Initialization
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerTab);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistTAB);


end.



