unit tiQueryCSV;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,tiDataBuffer_BOM
  ,tiQueryTXTAbs
  ;

type

  TtiDBConnectionPoolDataCSV = Class( TtiDBConnectionPoolDataTXTAbs );

  TtiDatabaseCSV = class( TtiDatabaseTXTFlatFileAbs )
  protected
    procedure   SaveDataSet(const pDataSet : TtiDataBuffer); override ;
    procedure   ReadDataSet(const pDataSet : TtiDataBuffer); override ;
  public
    constructor Create ; override ;
  end;

  TtiQueryCSV = class( TtiQueryTXTAbs )
  public
    constructor Create; override ;
    procedure   SelectRow( const pTableName : string ; const pWhere : TtiQueryParams = nil ) ; override ;
    procedure   InsertRow( const pTableName : string ; const pParams : TtiQueryParams ) ; override ;
    procedure   DeleteRow( const pTableName : string ; const pWhere  : TtiQueryParams = nil ) ; override ;
    procedure   UpdateRow( const pTableName : string ; const pParams : TtiQueryParams ; const pWhere  : TtiQueryParams ) ; override ;
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
  lWriter : TCSVToTIDataSet ;
  lFileName : string ;
begin
  lWriter := TCSVToTIDataSet.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name) ;
    lWriter.Save(pDataSet, lFileName );
  finally
    lWriter.Free;
  end;
end;

procedure TtiDatabaseCSV.ReadDataSet(const pDataSet: TtiDataBuffer);
var
  lFileName : string ;
  lWriter : TCSVToTIDataSet;
begin
  lWriter := TCSVToTIDataSet.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name) ;
    pDataSet.Clear ;
    lWriter.Read(pDataSet, lFileName );
  finally
    lWriter.Free;
  end;
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
procedure TtiQueryCSV.SelectRow(const pTableName: string;const pWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer ;
begin
  lDataSet := ( Database as TtiDatabaseCSV ).FindDataSetByName( pTableName ) ;
  if lDataSet = nil then
    raise EtiOPFInternalException.Create( 'Unable to find table <' + pTableName + '>') ;
  DoSelectRows(lDataSet, pWhere);
  CurrentRecordIndex := 0 ;
end;

procedure TtiQueryCSV.DeleteRow(const pTableName: string; const pWhere: TtiQueryParams);
var
  lDataSet : TtiDataBuffer ;
  i : integer ;
  lSetDirty : boolean ;
begin
  lDataSet := ( Database as TtiDatabaseCSV ).FindDataSetByName( pTableName ) ;
  if lDataSet = nil then
    raise EtiOPFInternalException.Create( 'Unable to find table <' + pTableName + '>') ;
  DoSelectRows(lDataSet, pWhere);
  lSetDirty := SelectedRows.Count > 0 ;
  for i := SelectedRows.Count - 1 downto 0 do
    lDataSet.Remove( TtiDataBufferRow( SelectedRows.Items[i] )) ;
  if lSetDirty then
    TtiDatabaseCSV(Database).SetDirty(lDataSet,true);
  SelectedRows.Clear ;
end;

procedure TtiQueryCSV.InsertRow(const pTableName: string; const pParams: TtiQueryParams);
var
  lDataSet : TtiDataBuffer ;
  lDataSetRow : TtiDataBufferRow ;
begin
  lDataSet := ( Database as TtiDatabaseCSV ).FindDataSetByName( pTableName ) ;
  if lDataSet = nil then
    raise EtiOPFInternalException.Create( 'Unable to find table <' + pTableName + '>') ;
  lDataSetRow := lDataSet.AddInstance ;
  DoUpdateRow( lDataSetRow, pParams ) ;
  (Database as TtiDatabaseCSV).SetDirty(lDataSet,true);
end;

procedure TtiQueryCSV.UpdateRow(const pTableName: string; const pParams, pWhere : TtiQueryParams);
var
  lDataSet : TtiDataBuffer ;
  i : integer ;
  lSetDirty : boolean ;
begin
  lDataSet := TtiDatabaseCSV( Database ).FindDataSetByName( pTableName ) ;
  if lDataSet = nil then
     raise EtiOPFInternalException.Create( 'Unable to find table <' + pTableName + '>') ;
  DoSelectRows(lDataSet, pWhere);
  lSetDirty := SelectedRows.Count > 0 ;
  for i := 0 to SelectedRows.Count - 1 do
    DoUpdateRow( TtiDataBufferRow( SelectedRows.Items[i]), pParams ) ;
  if lSetDirty then
    TtiDatabaseCSV(Database).SetDirty(lDataSet,true);
  SelectedRows.Clear ;
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

initialization
  gTIOPFManager.PersistenceLayers.__RegisterPersistenceLayer(
              cTIPersistCSV,
              TtiDBConnectionPoolDataCSV,
              TtiQueryCSV,
              TtiDatabaseCSV ) ;

finalization
  if not tiOPFManager.ShuttingDown then
    gTIOPFManager.PersistenceLayers.__UnRegisterPersistenceLayer( cTIPersistCSV ) ;


end.


