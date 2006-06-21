{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Purpose:                 

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiQueryXMLLight;

interface
uses
   tiQuery
  ,tiDataSet_BOM
  ,tiQueryTXTAbs
  ,tiXML
  ,tiXMLTotiDataSet
  ;

const
  cDBParamReadOnly = 'readonly';
  cDBParamCompress = 'compress';

type

  // ---------------------------------------------------------------------------
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

  // ---------------------------------------------------------------------------
  TtiQueryXMLLight = class( TtiQueryTXTAbs )
  public
    constructor Create ; override ;
    procedure   SelectRow( const pTableName : string ; const pWhere : TtiQueryParams = nil ) ; override ;
    procedure   InsertRow( const pTableName : string ; const pParams : TtiQueryParams ) ; override ;
    procedure   DeleteRow( const pTableName : string ; const pWhere  : TtiQueryParams = nil ) ; override ;
    procedure   UpdateRow( const pTableName : string ; const pParams : TtiQueryParams ; const pWhere  : TtiQueryParams ) ; override ;
  end;



implementation
uses
   SysUtils
  ,tiUtils
  ,tiPersist
  ,cTIPersist
  ,tiLog
  ,tiDialogs

  ;

{ TtiQueryXMLLight }

{ TODO :
SelectRow, etc. should probably be in the parent class
since it is generic except for reference to TtiDatabaseCSV.
cf  TODO for SaveDataSet above.
 }
procedure TtiQueryXMLLight.SelectRow(const pTableName: string;const pWhere: TtiQueryParams);
var
  lDataSet : TtiDataSet ;
begin
  try
    lDataSet := ( Database as TtiDatabaseXMLLight ).FindDataSetByName( pTableName ) ;
    if lDataSet = nil then
      tiFmtException( 'Unable to find table <' + pTableName + '>', ClassName, 'SelectRow' ) ;
    DoSelectRows(lDataSet, pWhere);
    CurrentRecordIndex := 0 ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SelectRow' ) ;
  end ;
end;

procedure TtiQueryXMLLight.DeleteRow(const pTableName: string; const pWhere: TtiQueryParams);
var
  lDataSet : TtiDataSet ;
  i : integer ;
begin
  try
    lDataSet := ( Database as TtiDatabaseXMLLight ).FindDataSetByName( pTableName ) ;
    if lDataSet = nil then
      tiFmtException( 'Unable to find table <' + pTableName + '>', ClassName, 'DeleteRow' ) ;
    DoSelectRows(lDataSet, pWhere);
    for i := SelectedRows.Count - 1 downto 0 do
      lDataSet.Remove( TtiDataSetRow( SelectedRows.Items[i] )) ;
    SelectedRows.Clear ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'DeleteRow' ) ;
  end ;
end;

procedure TtiQueryXMLLight.InsertRow(const pTableName: string; const pParams: TtiQueryParams);
var
  lDataSet : TtiDataSet ;
  lDataSetRow : TtiDataSetRow ;
begin
  try
    lDataSet := ( Database as TtiDatabaseXMLLight ).FindDataSetByName( pTableName ) ;
    if lDataSet = nil then
      tiFmtException( 'Unable to find table <' + pTableName + '>', ClassName, 'InsertRow' ) ;
    lDataSetRow := lDataSet.AddInstance ;
    DoUpdateRow( lDataSetRow, pParams ) ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'InsertRow' ) ;
  end ;
end;

procedure TtiQueryXMLLight.UpdateRow(const pTableName: string; const pParams, pWhere : TtiQueryParams);
var
  lDataSet : TtiDataSet ;
  i : integer ;
begin
  try
    lDataSet := TtiDatabaseXMLLight( Database ).FindDataSetByName( pTableName ) ;
    if lDataSet = nil then
       tiFmtException( 'Unable to find table <' + pTableName + '>', ClassName, 'UpdateRow' ) ;
    DoSelectRows(lDataSet, pWhere);
    for i := 0 to SelectedRows.Count - 1 do
      DoUpdateRow( TtiDataSetRow( SelectedRows.Items[i]), pParams ) ;
    SelectedRows.Clear ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'UpdateRow' ) ;
  end ;
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
begin
  if ( not pbValue ) then
  begin
    LogFmt( 'Disconnecting from %s', [DatabaseName] ) ;
    DataSets.Clear ;
    FConnected := false ;
    Exit ; //==>
  end;
  ReadOnly := SameText(Params.Values[cDBParamReadOnly], 'true');
  FCompress := Params.Values[cDBParamCompress];
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
  gtiPerMgr.RegPerLayers.__RegisterPersistenceLayer(
              cTIPersistXMLLight,
              TtiDBConnectionPoolDataXMLLight,
              TtiQueryXMLLight,
              TtiDatabaseXMLLight ) ;

finalization
  gtiPerMgr.RegPerLayers.__UnRegisterPersistenceLayer( cTIPersistXMLLight ) ;


end.



