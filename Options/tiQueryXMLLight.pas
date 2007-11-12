unit tiQueryXMLLight;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiDataBuffer_BOM
  ,tiQueryTXTAbs
  ,tiXML
  ,tiXMLToTIDataset
  ,tiPersistenceLayers
  ,tiDBConnectionPool
 ;

const
  cDBParamReadOnly = 'readonly';
  cDBParamCompress = 'compress';
  cErrorUnableToFindTable = 'Unable to find table <%s>';

type

  TtiPersistenceLayerXMLLight = class(TtiPersistenceLayer)
  protected
    function GetPersistenceLayerName: string; override;
    function GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass; override;
    function GetDatabaseClass: TtiDatabaseClass; override;
    function GetQueryClass: TtiQueryClass; override;
  public
    procedure AssignPersistenceLayerDefaults(const APersistenceLayerDefaults: TtiPersistenceLayerDefaults); override;
  end;

  TtiDBConnectionPoolDataXMLLight = Class(TtiDBConnectionPoolDataTXTAbs);

  TtiDatabaseXMLLight = class(TtiDatabaseTXTAbs)
  private
    FAsString : string;
    FPersistToFile: boolean;
    FReadOnly: Boolean;
    FCompress: string;
    FOptXMLDBSize: TtiOptXMLDBSize;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    function        GetAsString: string;
    procedure       SetAsString(const AValue: string);
  protected
    procedure       SetConnected(AValue : boolean); override;
    procedure       Read; virtual;
    procedure       Save; virtual;
  public
    constructor     Create; override;
    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string):boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    procedure       CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure       DropTable(const ATableMetaData: TtiDBMetaDataTable); override;
    procedure       Commit; override;
    procedure       RollBack; override;

    property        PersistToFile : boolean read FPersistToFile write FPersistToFile;
    property        AsString : string read GetAsString write SetAsString;
    function        Test : boolean; override;
    function        TIQueryClass: TtiQueryClass; override;
    property        ReadOnly : Boolean read FReadOnly Write FReadOnly;
    property        OptXMLDBSize: TtiOptXMLDBSize read FOptXMLDBSize Write FOptXMLDBSize;
    property        XMLFieldNameStyle: TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle;

  end;

  TtiQueryXMLLight = class(TtiQueryTXTAbs)
  public
    constructor Create; override;
    procedure   SelectRow(const ATableName : string; const AWhere : TtiQueryParams = nil); override;
    procedure   InsertRow(const ATableName : string; const AParams : TtiQueryParams); override;
    procedure   DeleteRow(const ATableName : string; const AWhere : TtiQueryParams = nil); override;
    procedure   UpdateRow(const ATableName : string; const AParams : TtiQueryParams; const AWhere : TtiQueryParams); override;
  end;

function tiMakeXMLLightParams(pReadOnly: Boolean; const pCompress: string;
                               pOptDBSize: TtiOptXMLDBSize;
                               pFieldNameSyle: TtiXMLFieldNameStyle): string;

// ToDo: Refactor RegisterPersistenceLayer so it works as a class method
//       on the DBConnection or DBConnectionPool, or create a TPersistenceLayerXXX
//       class for the purpose
procedure RegisterPersistenceLayer(const APersistenceLayers: TtiPersistenceLayers);

implementation
uses
   tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiLog
  ,tiExcept
  ,SysUtils

 ;

procedure RegisterPersistenceLayer(const APersistenceLayers: TtiPersistenceLayers);
begin
  Assert(APersistenceLayers.TestValid, CTIErrorInvalidObject);
  APersistenceLayers.__RegisterPersistenceLayer(
    TtiPersistenceLayerXMLLight);
end;

function tiMakeXMLLightParams(pReadOnly: Boolean; const pCompress: string;
                               pOptDBSize: TtiOptXMLDBSize;
                               pFieldNameSyle: TtiXMLFieldNameStyle): string;
var
  lReadOnly: string;
  lCompress: string;
  lOptXMLDBSize: string;
  lFieldNamesStyle: string;
begin

  lReadOnly       := 'readonly=' + tiBoolToStr(pReadOnly);
  lCompress       := cDBParamCompress + '=' + pCompress;
  lOptXMLDBSize   := cgXMLTagOptXMLDBSize + '=' + cOptXMLDBSize[pOptDBSize];
  lFieldNamesStyle := cgXMLFieldNameStyle + '=' + cXMLFieldNameStyles[pFieldNameSyle];

  result :=
    lReadOnly + ',' +
    lCompress + ',' +
    lOptXMLDBSize + ',' +
    lFieldNamesStyle;

end;

{ TtiQueryXMLLight }

{ TODO :
SelectRow, etc. should probably be in the parent class
since it is generic except for reference to TtiDatabaseCSV.
cf  TODO for SaveDataSet above.
 }
procedure TtiQueryXMLLight.SelectRow(const ATableName: string;const AWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
begin
  lDataSet := (Database as TtiDatabaseXMLLight).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindTable, [ATableName]);
  DoSelectRows(lDataSet, AWhere);
  CurrentRecordIndex := 0;
end;

procedure TtiQueryXMLLight.DeleteRow(const ATableName: string; const AWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  i : integer;
begin
  lDataSet := (Database as TtiDatabaseXMLLight).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindTable, [ATableName]);
  DoSelectRows(lDataSet, AWhere);
  for i := SelectedRows.Count - 1 downto 0 do
    lDataSet.Remove(TtiDataBufferRow(SelectedRows.Items[i]));
  SelectedRows.Clear;
end;

procedure TtiQueryXMLLight.InsertRow(const ATableName: string; const AParams: TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  lDataSetRow : TtiDataBufferRow;
begin
  lDataSet := (Database as TtiDatabaseXMLLight).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindTable, [ATableName]);
  lDataSetRow := lDataSet.AddInstance;
  DoUpdateRow(lDataSetRow, AParams);
end;

procedure TtiQueryXMLLight.UpdateRow(const ATableName: string; const AParams, AWhere : TtiQueryParams);
var
  lDataSet : TtiDataBuffer;
  i : integer;
begin
  lDataSet := TtiDatabaseXMLLight(Database).FindDataSetByName(ATableName);
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindTable, [ATableName]);
  DoSelectRows(lDataSet, AWhere);
  for i := 0 to SelectedRows.Count - 1 do
    DoUpdateRow(TtiDataBufferRow(SelectedRows.Items[i]), AParams);
  SelectedRows.Clear;
end;


{ TtiDatabaseXMLLight }

procedure TtiDatabaseXMLLight.Commit;
begin
  Save;
  SetInTransaction(False);
end;

constructor TtiDatabaseXMLLight.Create;
begin
  inherited Create;
  FPersistToFile := true;
  FReadOnly := False;
  FOptXMLDBSize := optDBSizeOff;
  FXMLFieldNameStyle := xfnsString;
  FCompress := cgsCompressNone;
end;

class procedure TtiDatabaseXMLLight.CreateDatabase(const ADatabaseName,
  AUserName, APassword: string);
var
  lDatabase : TtiDatabaseXMLLight;
begin
  lDatabase := TtiDatabaseXMLLight.Create;
  try
    lDatabase.DatabaseName := ADatabaseName;
    lDatabase.Save;
  finally
    lDatabase.Free;
  end;
end;

procedure TtiDatabaseXMLLight.CreateTable(const ATableMetaData: TtiDBMetaDataTable);
begin
  inherited CreateTable(ATableMetaData);
  Save;
end;

class function TtiDatabaseXMLLight.DatabaseExists(const ADatabaseName,
  AUserName, APassword: string): boolean;
begin
  result := FileExists(ADatabaseName);
end;

procedure TtiDatabaseXMLLight.DropTable(const ATableMetaData: TtiDBMetaDataTable);
begin
  inherited DropTable(ATableMetaData);
  Save;
end;

function TtiDatabaseXMLLight.GetAsString: string;
begin
  Assert(not FPersistToFile, 'AsString not available when PersistToFile = True');
  if InTransaction then
    Save;
  result := FAsString;
end;

procedure TtiDatabaseXMLLight.Read;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lXMLToTIDataSets.DataSets := DataSets;
    lXMLToTIDataSets.OptXMLDBSize := FOptXMLDBSize;
    lXMLToTIDataSets.XMLFieldNameStyle := FXMLFieldNameStyle;
    lXMLToTIDataSets.Compress := FCompress;
    if PersistToFile then
    begin
      lXMLToTIDataSets.LoadFromFile(DatabaseName, ReadOnly)
    end
    else
      lXMLToTIDataSets.AsString := FAsString;
  finally
    lXMLToTIDataSets.Free;
  end;
end;

procedure TtiDatabaseXMLLight.RollBack;
begin
  Read;
  SetInTransaction(false);
end;

procedure TtiDatabaseXMLLight.Save;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter;
begin
  if FReadOnly then
    Exit; //==>
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create;
  try
    lXMLToTIDataSets.DataSets := DataSets;
    lXMLToTIDataSets.OptXMLDBSize := OptXMLDBSize;
    lXMLToTIDataSets.XMLFieldNameStyle := FXMLFieldNameStyle;
    lXMLToTIDataSets.Compress := FCompress;
    if PersistToFile then
      lXMLToTIDataSets.SaveToFile(DatabaseName)
    else
      FAsString := lXMLToTIDataSets.AsString;
  finally
    lXMLToTIDataSets.Free;
  end;
end;

procedure TtiDatabaseXMLLight.SetAsString(const AValue: string);
begin
  Assert(not FPersistToFile, 'AsString not available when PersistToFile = True');
  FAsString := AValue;
  Read;
end;

procedure TtiDatabaseXMLLight.SetConnected(AValue: boolean);
var
  lCompress: string;
begin
  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    DataSets.Clear;
    FConnected := false;
    Exit; //==>
  end;
  ReadOnly := tiStrToBool(Params.Values[cDBParamReadOnly]);
  lCompress := Params.Values[cDBParamCompress];
  if lCompress = '' then
    FCompress := cgsCompressNone
  else
    FCompress := lCompress;
  if Params.Values[cgXMLTagOptXMLDBSize] <> '' then
    FOptXMLDBSize := tiStringToOptXMLDBSize(Params.Values[cgXMLTagOptXMLDBSize]);
  if Params.Values[cgXMLFieldNameStyle] <> '' then
    XMLFieldNameStyle := tiStringToXMLFieldNameStyle(Params.Values[cgXMLFieldNameStyle]);
  Read;
  FConnected := true;
end;

function TtiDatabaseXMLLight.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');
end;

function TtiDatabaseXMLLight.TIQueryClass: TtiQueryClass;
begin
  result:= TtiQueryXMLLight;
end;

constructor TtiQueryXMLLight.Create;
begin
  inherited;
  FReservedChars := rcXML;
end;

{ TtiPersistenceLayerXMLLight }

procedure TtiPersistenceLayerXMLLight.AssignPersistenceLayerDefaults(
  const APersistenceLayerDefaults: TtiPersistenceLayerDefaults);
begin
  Assert(APersistenceLayerDefaults.TestValid, CTIErrorInvalidObject);
  APersistenceLayerDefaults.PersistenceLayerName:= cTIPersistXMLLight;
  APersistenceLayerDefaults.DatabaseName:= CDefaultDatabaseName + '.XMLLight';
  APersistenceLayerDefaults.UserName:= 'null';
  APersistenceLayerDefaults.Password:= 'null';
  APersistenceLayerDefaults.CanCreateDatabase:= True;
  APersistenceLayerDefaults.CanSupportMultiUser:= False;
end;

function TtiPersistenceLayerXMLLight.GetDatabaseClass: TtiDatabaseClass;
begin
  result:= TtiDatabaseXMLLight;
end;

function TtiPersistenceLayerXMLLight.GetDBConnectionPoolDataClass: TtiDBConnectionPoolDataClass;
begin
  result:= TtiDBConnectionPoolDataXMLLight;
end;

function TtiPersistenceLayerXMLLight.GetPersistenceLayerName: string;
begin
  result:= cTIPersistXMLLight;
end;

function TtiPersistenceLayerXMLLight.GetQueryClass: TtiQueryClass;
begin
  result:= TtiQueryXMLLight;
end;

Initialization
  RegisterPersistenceLayer(gTIOPFManager.PersistenceLayers);

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer(cTIPersistXMLLight);

end.



