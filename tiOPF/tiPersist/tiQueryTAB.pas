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

unit tiQueryTAB;

interface
uses
   tiQuery
  ,tiDataSet_BOM
  ,tiQueryTXTAbs
  ;

type

  // ---------------------------------------------------------------------------
  TtiDBConnectionPoolDataTAB = Class( TtiDBConnectionPoolDataTXTAbs );

  // ---------------------------------------------------------------------------
  TtiDatabaseTAB = class( TtiDatabaseTXTFlatFileAbs )
  protected
    procedure   SaveDataSet(const pDataSet : TtiDataSet); override ;
    procedure   ReadDataSet(const pDataSet : TtiDataSet); override ;
  public
    constructor create ; override ;
  end;

  // ---------------------------------------------------------------------------
  TtiQueryTAB = class( TtiQueryTXTAbs )
  protected
  public
    constructor Create; override;
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
  ,tiXML
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

procedure TtiDatabaseTAB.SaveDataSet(const pDataSet: TtiDataSet);
var
  lWriter : TTABToTIDataSet ;
  lFileName : string ;
begin
  lWriter := TTABToTIDataSet.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name) ;
    lWriter.Save(pDataSet, lFileName );
  finally
    lWriter.Free;
  end;
end;

procedure TtiDatabaseTAB.ReadDataSet(const pDataSet: TtiDataSet);
var
  lFileName : string ;
  lWriter : TTABToTIDataSet;
begin
  lWriter := TTABToTIDataSet.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name) ;
    pDataSet.Clear ;
    lWriter.Read(pDataSet, lFileName );
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
procedure TtiQueryTAB.SelectRow(const pTableName: string;const pWhere: TtiQueryParams);
var
  lDataSet : TtiDataSet ;
begin
  try
    lDataSet := ( Database as TtiDatabaseTAB ).FindDataSetByName( pTableName ) ;
    if lDataSet = nil then
      tiFmtException( 'Unable to find table <' + pTableName + '>', ClassName, 'SelectRow' ) ;
    DoSelectRows(lDataSet, pWhere);
    CurrentRecordIndex := 0 ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'SelectRow' ) ;
  end ;
end;

procedure TtiQueryTAB.DeleteRow(const pTableName: string; const pWhere: TtiQueryParams);
var
  lDataSet : TtiDataSet ;
  i : integer ;
  lSetDirty : boolean ;
begin
  try
    lDataSet := ( Database as TtiDatabaseTAB ).FindDataSetByName( pTableName ) ;
    if lDataSet = nil then
      tiFmtException( 'Unable to find table <' + pTableName + '>', ClassName, 'DeleteRow' ) ;
    DoSelectRows(lDataSet, pWhere);
    lSetDirty := SelectedRows.Count > 0 ;
    for i := SelectedRows.Count - 1 downto 0 do
      lDataSet.Remove( TtiDataSetRow( SelectedRows.Items[i] )) ;
    if lSetDirty then
      (Database as TtiDatabaseTAB).SetDirty(lDataSet,true);
    SelectedRows.Clear ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'DeleteRow' ) ;
  end ;
end;

procedure TtiQueryTAB.InsertRow(const pTableName: string; const pParams: TtiQueryParams);
var
  lDataSet : TtiDataSet ;
  lDataSetRow : TtiDataSetRow ;
begin
  try
    lDataSet := ( Database as TtiDatabaseTAB ).FindDataSetByName( pTableName ) ;
    if lDataSet = nil then
      tiFmtException( 'Unable to find table <' + pTableName + '>', ClassName, 'InsertRow' ) ;
    lDataSetRow := lDataSet.AddInstance ;
    DoUpdateRow( lDataSetRow, pParams ) ;
    (Database as TtiDatabaseTAB).SetDirty(lDataSet,true);
  except
    on e:exception do
      tiFmtException( e, ClassName, 'InsertRow' ) ;
  end ;
end;

procedure TtiQueryTAB.UpdateRow(const pTableName: string; const pParams, pWhere : TtiQueryParams);
var
  lDataSet : TtiDataSet ;
  i : integer ;
  lSetDirty : boolean ;
begin
  try
    lDataSet := TtiDatabaseTAB( Database ).FindDataSetByName( pTableName ) ;
    if lDataSet = nil then
       tiFmtException( 'Unable to find table <' + pTableName + '>', ClassName, 'UpdateRow' ) ;
    DoSelectRows(lDataSet, pWhere);
    lSetDirty := SelectedRows.Count > 0 ;
    for i := 0 to SelectedRows.Count - 1 do
      DoUpdateRow( TtiDataSetRow( SelectedRows.Items[i]), pParams ) ;
    if lSetDirty then
      TtiDatabaseTAB(Database).SetDirty(lDataSet,true);
    SelectedRows.Clear ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'UpdateRow' ) ;
  end ;
end;

constructor TtiDatabaseTAB.create;
begin
  inherited;
  FilenameExt := 'TXT';
end;

constructor TtiQueryTAB.Create;
begin
  inherited;
  FReservedChars := rcTAB;
end;

Initialization
  gtiPerMgr.RegPerLayers.__RegisterPersistenceLayer(
              cTIPersistTAB,
              TtiDBConnectionPoolDataTAB,
              TtiQueryTAB,
              TtiDatabaseTAB ) ;

finalization
  gtiPerMgr.RegPerLayers.__UnRegisterPersistenceLayer( cTIPersistTAB ) ;


end.



