unit tiQueryXMLLight;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiDataBuffer_BOM
  ,tiQueryTXTAbs
  ,tiXML
  ,tiXMLTotiDataSet
  ;

const
  cDBParamReadOnly = 'readonly';
  cDBParamCompress = 'compress';
  cErrorUnableToFindTable = 'Unable to find table <%s>';

type

  TtiDBConnectionPoolDataXMLLight = Class( TtiDBConnectionPoolDataTXTAbs );

  TtiDatabaseXMLLight = class( TtiDatabaseTXTAbs )
  private
    FAsString : string ;
    FPersistToFile: boolean;
    FReadOnly: Boolean;
    FCompress: string;
    FOptXMLDBSize: TtiOptXMLDBSize;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    function        GetAsString: string;
    procedure       SetAsString(const Value: string);
  protected
    procedure       SetConnected( pbValue : boolean ) ; override;
    procedure       Read ; virtual ;
    procedure       Save ; virtual ;
  public
    constructor     Create ; override ;
    class function  DatabaseExists( const psDatabaseName, psUserName, psPassword : string ):boolean ; override ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    procedure       CreateTable( const pTableMetaData : TtiDBMetaDataTable ) ; override ;
    procedure       DropTable( const pTableMetaData: TtiDBMetaDataTable); override ;
    procedure       Commit ; override ;
    procedure       RollBack ; override ;

    property        PersistToFile : boolean read FPersistToFile write FPersistToFile ;
    property        AsString : string read GetAsString write SetAsString ;
    function        Test : boolean ; override ;
    property        ReadOnly : Boolean read FReadOnly Write FReadOnly;
    property        OptXMLDBSize: TtiOptXMLDBSize read FOptXMLDBSize Write FOptXMLDBSize ;
    property        XMLFieldNameStyle: TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle;

  end;

  TtiQueryXMLLight = class( TtiQueryTXTAbs )
  public
    constructor Create ; override ;
    procedure   SelectRow( const pTableName : string ; const pWhere : TtiQueryParams = nil ) ; override ;
    procedure   InsertRow( const pTableName : string ; const pParams : TtiQueryParams ) ; override ;
    procedure   DeleteRow( const pTableName : string ; const pWhere  : TtiQueryParams = nil ) ; override ;
    procedure   UpdateRow( const pTableName : string ; const pParams : TtiQueryParams ; const pWhere  : TtiQueryParams ) ; override ;
  end;

function tiMakeXMLLightParams( pReadOnly: Boolean ; const pCompress: string ;
                               pOptDBSize: TtiOptXMLDBSize ;
                               pFieldNameSyle: TtiXMLFieldNameStyle): string;


implementation
uses
   tiUtils
  ,tiOPFManager
  ,tiConstants
  ,tiLog
//  ,tiDialogs
  ,tiExcept
  ,SysUtils

  ;

function tiMakeXMLLightParams( pReadOnly: Boolean ; const pCompress: string ;
                               pOptDBSize: TtiOptXMLDBSize ;
                               pFieldNameSyle: TtiXMLFieldNameStyle): string;
var
  lReadOnly: string;
  lCompress: string;
  lOptXMLDBSize: string;
  lFieldNamesStyle: string;
begin

  lReadOnly        := 'readonly=' + tiBoolToStr(pReadOnly);
  lCompress        := cDBParamCompress + '=' + pCompress;
  lOptXMLDBSize    := cgXMLTagOptXMLDBSize + '=' + cOptXMLDBSize[pOptDBSize];
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
procedure TtiQueryXMLLight.SelectRow(const pTableName: string;const pWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer ;
begin
  lDataSet := ( Database as TtiDatabaseXMLLight ).FindDataSetByName( pTableName ) ;
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindTable, [pTableName]);
  DoSelectRows(lDataSet, pWhere);
  CurrentRecordIndex := 0 ;
end;

procedure TtiQueryXMLLight.DeleteRow(const pTableName: string; const pWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer ;
  i : integer ;
begin
  lDataSet := ( Database as TtiDatabaseXMLLight ).FindDataSetByName( pTableName ) ;
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindTable, [pTableName]);
  DoSelectRows(lDataSet, pWhere);
  for i := SelectedRows.Count - 1 downto 0 do
    lDataSet.Remove( TtiDataBufferRow( SelectedRows.Items[i] )) ;
  SelectedRows.Clear ;
end;

procedure TtiQueryXMLLight.InsertRow(const pTableName: string; const pParams: TtiQueryParams);
var
  lDataSet : TtiDataBuffer ;
  lDataSetRow : TtiDataBufferRow ;
begin
  lDataSet := ( Database as TtiDatabaseXMLLight ).FindDataSetByName( pTableName ) ;
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindTable, [pTableName]);
  lDataSetRow := lDataSet.AddInstance ;
  DoUpdateRow( lDataSetRow, pParams ) ;
end;

procedure TtiQueryXMLLight.UpdateRow(const pTableName: string; const pParams, pWhere : TtiQueryParams);
var
  lDataSet : TtiDataBuffer ;
  i : integer ;
begin
  lDataSet := TtiDatabaseXMLLight( Database ).FindDataSetByName( pTableName ) ;
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToFindTable, [pTableName]);
  DoSelectRows(lDataSet, pWhere);
  for i := 0 to SelectedRows.Count - 1 do
    DoUpdateRow( TtiDataBufferRow( SelectedRows.Items[i]), pParams ) ;
  SelectedRows.Clear ;
end;


{ TtiDatabaseXMLLight }

procedure TtiDatabaseXMLLight.Commit;
begin
  Save ;
  SetInTransaction(False) ;
end;

constructor TtiDatabaseXMLLight.Create;
begin
  inherited Create;
  FPersistToFile := true ;
  FReadOnly := False ;
  FOptXMLDBSize := optDBSizeOff ;
  FXMLFieldNameStyle := xfnsString ;
  FCompress := cgsCompressNone;
end;

class procedure TtiDatabaseXMLLight.CreateDatabase(const psDatabaseName,
  psUserName, psPassword: string);
var
  lDatabase : TtiDatabaseXMLLight ;
begin
  lDatabase := TtiDatabaseXMLLight.Create ;
  try
    lDatabase.DatabaseName := psDatabaseName ;
    lDatabase.Save ;
  finally
    lDatabase.Free;
  end ;
end;

procedure TtiDatabaseXMLLight.CreateTable(const pTableMetaData: TtiDBMetaDataTable);
begin
  inherited CreateTable(pTableMetaData);
  Save ;
end;

class function TtiDatabaseXMLLight.DatabaseExists(const psDatabaseName,
  psUserName, psPassword: string): boolean;
begin
  result := FileExists(psDatabaseName);
end;

procedure TtiDatabaseXMLLight.DropTable(const pTableMetaData: TtiDBMetaDataTable);
begin
  inherited DropTable(pTableMetaData);
  Save ;
end;

function TtiDatabaseXMLLight.GetAsString: string;
begin
  Assert( not FPersistToFile, 'AsString not available when PersistToFile = True' ) ;
  if InTransaction then
    Save ;
  result := FAsString ;
end;

procedure TtiDatabaseXMLLight.Read;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
begin
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lXMLToTIDataSets.DataSets := DataSets ;
    lXMLToTIDataSets.OptXMLDBSize := FOptXMLDBSize ;
    lXMLToTIDataSets.XMLFieldNameStyle := FXMLFieldNameStyle ;
    lXMLToTIDataSets.Compress := FCompress ;
    if PersistToFile then
    begin
      lXMLToTIDataSets.LoadFromFile(DatabaseName, ReadOnly)
    end
    else
      lXMLToTIDataSets.AsString := FAsString ;
  finally
    lXMLToTIDataSets.Free;
  end ;
end;

procedure TtiDatabaseXMLLight.RollBack;
begin
  Read ;
  SetInTransaction(false) ;
end;

procedure TtiDatabaseXMLLight.Save;
var
  lXMLToTIDataSets : TtiXMLToDataSetReadWriter ;
begin
  if FReadOnly then
    Exit ; //==>
  lXMLToTIDataSets := TtiXMLToDataSetReadWriter.Create ;
  try
    lXMLToTIDataSets.DataSets := DataSets ;
    lXMLToTIDataSets.OptXMLDBSize := OptXMLDBSize ;
    lXMLToTIDataSets.XMLFieldNameStyle := FXMLFieldNameStyle ;
    lXMLToTIDataSets.Compress := FCompress ;
    if PersistToFile then
      lXMLToTIDataSets.SaveToFile(DatabaseName)
    else
      FAsString := lXMLToTIDataSets.AsString ;
  finally
    lXMLToTIDataSets.Free;
  end ;
end;

procedure TtiDatabaseXMLLight.SetAsString(const Value: string);
begin
  Assert( not FPersistToFile, 'AsString not available when PersistToFile = True' ) ;
  FAsString := Value ;
  Read ;
end;

procedure TtiDatabaseXMLLight.SetConnected(pbValue: boolean);
var
  lCompress: string;
begin
  if ( not pbValue ) then
  begin
    Log( 'Disconnecting from %s', [DatabaseName], lsConnectionPool ) ;
    DataSets.Clear ;
    FConnected := false ;
    Exit ; //==>
  end;
  ReadOnly := tiStrToBool(Params.Values[cDBParamReadOnly]);
  lCompress := Params.Values[cDBParamCompress];
  if lCompress = '' then
    FCompress := cgsCompressNone
  else
    FCompress := lCompress;
  if Params.Values[cgXMLTagOptXMLDBSize] <> '' then
    FOptXMLDBSize  := tiStringToOptXMLDBSize(Params.Values[cgXMLTagOptXMLDBSize]);
  if Params.Values[cgXMLFieldNameStyle] <> '' then
    XMLFieldNameStyle := tiStringToXMLFieldNameStyle(Params.Values[cgXMLFieldNameStyle]);
  Read ;
  FConnected := true ;
end;

function TtiDatabaseXMLLight.Test: boolean;
begin
  result := false ;
  Assert( false, 'Under construction' ) ;
end;

constructor TtiQueryXMLLight.Create;
begin
  inherited;
  FReservedChars := rcXML;
end;

Initialization
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
              cTIPersistXMLLight,
              TtiDBConnectionPoolDataXMLLight,
              TtiQueryXMLLight,
              TtiDatabaseXMLLight ) ;

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer( cTIPersistXMLLight ) ;


end.



