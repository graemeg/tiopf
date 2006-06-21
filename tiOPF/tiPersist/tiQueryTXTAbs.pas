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

unit tiQueryTXTAbs;

interface
uses
   tiQuery
  ,Classes
  ,tiClassToDBMap_BOM
  ,tiPtnVisPerObj
  ,tiDBConnectionPool
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiDataSet_BOM
  ,Contnrs
  ,tiObjAbs
  ,tiDataSet_Cli
  ,tiStreams
  ,SysUtils
  ,tiXML
  ;

type

  TtiDatabaseTXTAbs = class ;

  // ---------------------------------------------------------------------------
  TtiDBConnectionPoolDataTXTAbs = Class( TtiDBConnectionPoolDataAbs )
  public
    procedure InitDBConnectionPool ; override ;
  end ;

  TtiDatabaseTXTAbs = class( TtiDatabase )
  private
    FDataSets : TtiDataSets ;
    FInTransaction : boolean ;
  protected
    FConnected : boolean ;
    procedure   SetInTransaction(pValue : Boolean);
    function    GetConnected : boolean ; override ;
    property    DataSets : TtiDataSets read FDataSets ;
  public
    constructor create ; override ;
    destructor  Destroy ; override ;
    procedure   StartTransaction ; override ;
    function    InTransaction : boolean ; override ;
    procedure   ReadMetaDataTables( pData : TPersistent ) ; override ;
    procedure   ReadMetaDataFields( pData : TPersistent ) ; override ;
    function    FindDataSetByName(const pName : string ) : TtiDataSet ;
    procedure   CreateTable( const pTableMetaData : TtiDBMetaDataTable ) ; override ;
    procedure   DropTable( const pTableMetaData: TtiDBMetaDataTable); override ;
    function    Test : boolean ; override ;
  end;

  // ---------------------------------------------------------------------------
  TtiDatabaseTXTFlatFileAbs = class( TtiDatabaseTXTAbs )
  private
    FDirtyList : TObjectList ;
    FFilenameExt : string ;


  protected
    function    GetDataSetCount: integer;
    function    ExpandFileName( const pTableName : string ) : string ;
    procedure   SetDirty(const pDataSet : TtiDataSet; pDirty : boolean) ;
    function    IsDirty(const pDataSet : TtiDataSet) : boolean ;
    procedure   SetConnected( pbValue : boolean ) ; override;
    property    DataSetCount : integer read GetDataSetCount ;
    procedure   SaveDataSet(const pDataSet : TtiDataSet); virtual ;
    procedure   ReadDataSet(const pDataSet : TtiDataSet); virtual ;
    property    FilenameExt : string  read FFilenameExt write FFilenameExt;

  public
    constructor create ; override ;
    destructor  Destroy ; override ;

    class function  DatabaseExists( const psDatabaseName, psUserName, psPassword : string ):boolean ; override ;
    class procedure CreateDatabase( const psDatabaseName, psUserName, psPassword : string ) ; override ;
    procedure   Commit ; override ;
    procedure   RollBack ; override ;

    procedure   DropTable( const pTableMetaData : TtiDBMetaDataTable ) ; override ;
    procedure   CreateTable( const pTableMetaData : TtiDBMetaDataTable ) ; override ;

  end ;

  // ---------------------------------------------------------------------------
  TtiQueryTXTAbs = class( TtiQueryNonSQL )
  private
    FCurrentRecordIndex : integer ;
    FSelectedRows : TObjectList ;
    FActive : boolean ;
  protected
    FXMLRCTrans: IXMLReservedCharsTranslator;
    FReservedChars : TtiReservedChar ;

    procedure   DoSelectRows( const pDataSet : TtiDataSet ; pWhere : TtiQueryParams ) ;
    procedure   DoUpdateRow(  const pDataSetRow : TtiDataSetRow ; pParams : TtiQueryParams ) ;
    function    GetParamAsString(const psName: string): string; override ;
    procedure   SetParamAsString(const psName, Value: string); override ;
    procedure   DoAssignParamToCell( const pCell : TtiDataSetCell ; pParam : TtiQueryParamAbs ) ;

    function    CurrentRow : TtiDataSetRow ;

    function    GetFieldAsString(const psName: string): string;     override;
    function    GetFieldAsFloat(const psName: string): real;        override;
    function    GetFieldAsBoolean(const psName: string): boolean;   override;
    function    GetFieldAsInteger(const psName: string): Int64;     override;
    function    GetFieldAsDateTime(const psName: string):TDateTime; override;
    function    GetFieldIsNull(const psName: string): Boolean;      override;

    function    GetFieldAsStringByIndex(pIndex: Integer): string     ; override;
    function    GetFieldAsFloatByIndex(pIndex: Integer)   : real     ; override;
    function    GetFieldAsBooleanByIndex(pIndex: Integer) : boolean  ; override;
    function    GetFieldAsIntegerByIndex(pIndex: Integer) : Int64    ; override;
    function    GetFieldAsDateTimeByIndex(pIndex: Integer):TDateTime ; override;
    function    GetFieldIsNullByIndex(pIndex: Integer):Boolean       ; override;

    function    GetSQL: TStrings; override ;
    procedure   SetSQL(const Value: TStrings); override ;
    function    GetActive: boolean; override ;
    procedure   SetActive(const Value: boolean); override ;
    function    GetEOF: boolean; override ;

    property    CurrentRecordIndex : Integer     read FCurrentRecordIndex write FCurrentRecordIndex;
    property    SelectedRows      : TObjectList read FSelectedRows       write FSelectedRows;

  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Open    ; override ;
    procedure   Close   ; override ;
    procedure   Next    ; override ;
    procedure   ExecSQL ; override ;

    procedure    AttachDatabase( pDatabase : TtiDatabase ) ; override ;
    procedure    DetachDatabase ;  override ;
    procedure    Reset ; override ;

    function     FieldCount : integer ; override ;
    function     FieldName( pIndex : integer ) : string ; override ;
    function     FieldIndex( const psName : string ) : integer ; override ;
    function     FieldKind( pIndex : integer ) : TtiQueryFieldKind ; override ;
    function     FieldSize( pIndex : integer ) : integer ; override ;
    function     HasNativeLogicalType : boolean ; override ;

  public

  published

  end ;


  TTXTToTIDataSetAbs = class( TtiObjAbs )
  private
    FTextFileMetaData: TTextFileMetaDatas;
    FFieldDelim: string;
    FStringDelim: string;
    FStringDelimAsChar: AnsiChar;
    FRowDelim   : string;
    FStream     : TtiFileStream ;
    FDataSet    : TtiDataSet ;
    FDataSetRow : TtiDataSetRow ;
    function  QuoteStr(const pStr : string):string;
    procedure WriteFieldNames;
    procedure WriteRow(const pRow : TtiDataSetRow);
    procedure ReadFieldNames ;
    procedure ReadRow ;
    procedure DoExtractFieldName( pIndex : integer ; const pValue : string ) ;
    procedure DoExtractData( pIndex : integer ; const pValue : string ) ;
    procedure DoExtractDefaultFieldNames( pIndex : integer ; const pValue : string ) ;
    procedure SetStringDelim(const Value: string);
  protected
  public
    constructor Create ; virtual ;
    property    TextFileMetaData : TTextFileMetaDatas read FTextFileMetaData write FTextFileMetaData ;
    property    FieldDelim  : string read FFieldDelim write FFieldDelim ;
    property    StringDelim : string read FStringDelim write SetStringDelim ;
    property    RowDelim    : string read FRowDelim write FRowDelim ;
    procedure   Save( pDataSet : TtiDataSet ; pFileName : TFileName ) ;
    procedure   Read( pDataSet : TtiDataSet ; pFileName : TFileName ) ;
  end ;

  // ---------------------------------------------------------------------------
  TCSVToTIDataSet = class( TTXTToTIDataSetAbs )
  public
    constructor Create; override ;
  end;

  // ---------------------------------------------------------------------------
  TTABToTIDataSet = class( TTXTToTIDataSetAbs )
  public
    constructor Create; override ;
  end;



implementation
uses
   tiLog
  ,tiUtils
  ,TypInfo
  ,tiPersist
  ,cTIPersist
  ,Windows
  ,tiDialogs
  ,tiRJMime
  ;

const
  cuXMLTagTable     = 'table' ;
  cuXMLTagTables    = 'tables' ;
  cuXMLTagTableName = 'table_name' ;
  cuXMLTagFields    = 'fields' ;
  cuXMLTagField     = 'field' ;
  cuXMLTagFieldName = 'field_name' ;
  cuXMLTagFieldKind = 'field_kind' ;
  cuXMLTagFieldSize = 'field_Size' ;
  cuXMLTagRows      = 'rows' ;
  cuXMLTagRow       = 'row' ;

  // ToDo: Implement escape chars for strings
  cDSCharCr    = #13 ; cgDSSubstCharCr    = '&cr;' ;
  cDSCharLf    = #10 ; cgDSSubstCharLf    = '&lf;' ;
  cDSCharComma = #13 ; cgDSSubstCharComma = '&cr;' ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiQueryTXTAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiQueryTXTAbs.Create;
begin
  inherited;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator ;
  FSelectedRows := TObjectList.Create(false) ;
  FActive := false ;
end;

// -----------------------------------------------------------------------------
destructor TtiQueryTXTAbs.Destroy;
begin
  FSelectedRows.Free;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.Close;
begin
  Active := false ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.ExecSQL;
begin
  Assert( false, 'Not implemented in ' + ClassName ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.GetFieldAsBoolean(const psName: string): boolean;
var
  lCell : TtiDataSetCell ;
begin
  lCell := CurrentRow.FindByFieldName(psName);
  if lCell = nil then
    tiFmtException( 'Invalid field name <' + psName + '>', ClassName, 'GetFieldAsBoolean' ) ;
  result :=
     SameText( lCell.ValueAsString, 'TRUE' ) or
     SameText( lCell.ValueAsString, 'T' ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.GetFieldAsDateTime(const psName: string): TDateTime;
var
  lCell : TtiDataSetCell ;
begin
  lCell := CurrentRow.FindByFieldName(psName);
  if lCell = nil then
    tiFmtException( 'Invalid field name <' + psName + '>', ClassName, 'GetFieldAsDateTime' ) ;
  if lCell.ValueAsString <> '' then
    result := tiXMLStringToDateTime( lCell.ValueAsString )
  else
    result := 0 ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.GetFieldAsString(const psName: string): string;
var
  lCell : TtiDataSetCell ;
  ls : string ;
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  lCell := CurrentRow.FindByFieldName(psName);
  if lCell = nil then
    tiFmtException( 'Invalid field name <' + psName + '>', ClassName, 'GetFieldAsString' ) ;
  ls := lCell.ValueAsString ;
  result := FXMLRCTrans.InsertReserved( FReservedChars, ls ) ;
end;

function TtiQueryTXTAbs.GetParamAsString(const psName: string): string;
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  result := FXMLRCTrans.InsertReserved( FReservedChars, Params.GetValueAsString(psName));
end;

procedure TtiQueryTXTAbs.SetParamAsString(const psName, Value: string);
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  Params.SetValueAsString(psName, FXMLRCTrans.InsertReserved( FReservedChars, Value ));
end;

procedure TtiQueryTXTAbs.DoAssignParamToCell(const pCell: TtiDataSetCell;pParam: TtiQueryParamAbs);
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  if pParam.Kind = qfkString then
    pCell.ValueAsString := FXMLRCTrans.RemoveReserved(FReservedChars, pParam.GetValueAsString)
  else
    pCell.ValueAsString := pParam.GetValueAsString;
end;

function TtiQueryTXTAbs.GetFieldAsFloat(const psName: string): real;
var
  lCell : TtiDataSetCell ;
begin
  lCell := CurrentRow.FindByFieldName(psName);
  if lCell = nil then
    tiFmtException( 'Invalid field name <' + psName + '>', ClassName, 'GetFieldAsFloat' ) ;
  if lCell.ValueAsString <> '' then
    result := StrToFloat( lCell.ValueAsString )
  else
    result := 0 ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.GetFieldAsInteger(const psName: string): Int64;
var
  lCell : TtiDataSetCell ;
{$IFDEF VER130}
  lValue : Integer ;
{$ELSE}
  lValue : Int64 ;
{$ENDIF}
begin
  lCell := CurrentRow.FindByFieldName(psName);
  if lCell = nil then
    tiFmtException( 'Invalid field name <' + psName + '>', ClassName, 'GetFieldAsInteger' ) ;
  if lCell.ValueAsString <> '' then
    lValue := StrToInt(lCell.ValueAsString)
  else
    lValue := 0 ;
  result := lValue ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.GetActive: boolean;
begin
  result := FActive ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.GetEOF: boolean;
begin
  result := FCurrentRecordIndex >= FSelectedRows.Count ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.GetSQL: TStrings;
begin
  Assert( false, ClassName + '.GetSQL not implemented' ) ;
  result := nil ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.Next;
begin
  Inc( FCurrentRecordIndex ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.Open;
begin
  Active := true ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.SetActive(const Value: boolean);
begin
  if Active = Value then
    Exit ; //==>
  if Active and Not Value then
  begin
    FCurrentRecordIndex := -1 ;
    FSelectedRows.Clear ;
    FActive := false ;
    Exit ; //==>
  end ;
  Assert( false, 'Can not call ' + ClassName + '.SetActive(true)' ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.SetSQL(const Value: TStrings);
begin
  Assert( false, ClassName + '.SetSQL not implemented' ) ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.AttachDatabase(pDatabase: TtiDatabase);
begin
  inherited AttachDatabase(pDatabase);
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.DetachDatabase;
begin
  inherited DetachDatabase ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.FieldCount: integer;
begin
  if CurrentRow = nil then
  begin
    tiFmtException( 'No rows currently selected so can''t perform this operation', ClassName, 'FieldName' );
    result := 0 ;
  end
  else
    result := CurrentRow.Count ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.FieldName(pIndex: integer): string;
begin
  if CurrentRow = nil then
    tiFmtException( 'No rows currently selected so can''t perform this operation', ClassName, 'FieldName' )
  else
    result := CurrentRow.Items[pIndex].DataSetField.Name ;
end;

// -----------------------------------------------------------------------------
procedure TtiQueryTXTAbs.Reset;
begin
  Assert( false, 'Under construction' ) ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.FieldIndex(const psName: string): integer;
begin
  result := -1 ;
  if CurrentRow = nil then
    tiFmtException( 'No rows currently selected so can''t perform this operation', ClassName, 'FieldName' )
  else
    result := CurrentRow.FindByFieldName(psName).Index ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDatabaseTXTAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TtiDatabaseTXTFlatFileAbs.create;
begin
  inherited ;
  FFilenameExt := 'XXX'; //  This should be overridden
  FDirtyList := TObjectList.Create(false) ;
end;

// -----------------------------------------------------------------------------
destructor TtiDatabaseTXTFlatFileAbs.Destroy;
begin
  FDirtyList.Free;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseTXTFlatFileAbs.Commit;
var
  i : integer ;
begin
  for i := 0 to FDataSets.Count - 1 do
    if IsDirty(FDataSets.Items[i]) then
    begin
      SaveDataSet(FDataSets.Items[i]);
    end;
  FInTransaction := false ;
end;

// -----------------------------------------------------------------------------
function TtiDatabaseTXTAbs.InTransaction: boolean;
begin
  result := FInTransaction ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseTXTFlatFileAbs.RollBack;
var
  i : integer ;
begin
  for i := 0 to FDataSets.Count - 1 do
    if IsDirty(FDataSets.Items[i]) then
      ReadDataSet(FDataSets.Items[i]);
  FInTransaction := false ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseTXTAbs.StartTransaction;
begin
  FInTransaction := true ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseTXTAbs.ReadMetaDataFields(pData: TPersistent);
var
  lTable      : TtiDBMetaDataTable ;
  lDataSet    : TtiDataSet ;
  i : integer;
begin
  lTable := ( pData as TtiDBMetaDataTable ) ;
  lDataSet := FDataSets.FindByName(lTable.Name ) ;
  Assert( lDataSet <> nil,
          'Can not find metadata for <' + lTable.Name + '>' ) ;
  for i := 0 to lDataSet.Fields.Count - 1 do
    lTable.AddInstance(lDataSet.Fields.Items[i].Name,
                       lDataSet.Fields.Items[i].Kind,
                       lDataSet.Fields.Items[i].Width);
end;

procedure TtiDatabaseTXTAbs.ReadMetaDataTables(pData: TPersistent);
var
  lMetaData : TtiDBMetaData ;
  lTable : TtiDBMetaDataTable ;
  i : integer ;
begin
  lMetaData := ( pData as TtiDBMetaData ) ;
  for i := 0 to FDataSets.Count - 1 do
  begin
    lTable := TtiDBMetaDataTable.Create ;
    lTable.Name := FDataSets.Items[i].Name ;
    lTable.ObjectState := posPK ;
    lMetaData.Add( lTable ) ;
    lMetaData.ObjectState := posClean ;
  end ;
end;

// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.FieldKind(pIndex: integer): TtiQueryFieldKind;
begin
  result := qfkString ;
  if CurrentRow = nil then
    tiFmtException( 'No rows currently selected so can''t perform this operation', ClassName, 'FieldName' )
  else
    result := CurrentRow.Items[pIndex].DataSetField.Kind ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.FieldSize(pIndex: integer): integer;
begin
  result := 0 ;
  if CurrentRow = nil then
    tiFmtException( 'No rows currently selected so can''t perform this operation', ClassName, 'FieldName' )
  else
    result := CurrentRow.Items[pIndex].DataSetField.Width ;
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseTXTFlatFileAbs.SetConnected(pbValue: boolean);
var
  lsl : TStringList ;
  i : integer ;
  lDataSet : TtiDataSet ;
begin

  if ( not pbValue ) then
  begin
    LogFmt( 'Disconnecting from %s', [DatabaseName] ) ;
    FDataSets.Clear ;
    FConnected := false ;
    Exit ; //==>
  end;

  if ( not DirectoryExists( DatabaseName )) then
  begin
    ForceDirectories( DatabaseName ) ;
    if not DirectoryExists( DatabaseName ) then
      tiFmtException( 'Unable to find or create directory <' +
                      DatabaseName + '>',
                      ClassName, 'SetConnected' ) ;
  end;
  lsl := TStringList.Create ;
  try
    tiFilesToStringList(DatabaseName, '*.' + FilenameExt, lsl, false);
    for i := 0 to lsl.Count - 1 do
    begin
      lDataSet := TtiDataSet.Create ;
      lDataSet.Name := tiRemoveExtension (ExtractFileName( lsl.Strings[i]) );
      // ToDo: Should read datasets on demand, not all at once in the
      //       Connect method. Perpahs maintain another list of datasets that
      //       have been read.
      ReadDataSet(lDataSet);
      FDataSets.Add(lDataSet);
    end;
  finally
    lsl.Free;
  end;
  FConnected := true ;
end;

// -----------------------------------------------------------------------------
function TtiQueryTXTAbs.GetFieldIsNull(const psName: string): Boolean;
var
  lCell : TtiDataSetCell ;
begin
  lCell := CurrentRow.FindByFieldName(psName);
  if lCell = nil then
    tiFmtException( 'Invalid field name <' + psName + '>', ClassName, 'GetFieldIsNull' ) ;
  result := lCell.ValueAsString = '' ;
end;

procedure TtiDatabaseTXTFlatFileAbs.CreateTable( const pTableMetaData: TtiDBMetaDataTable);
var
  lDataSet : TtiDataSet ;
begin
  inherited CreateTable(pTableMetaData);
  lDataSet := FDataSets.FindByName( pTableMetaData.Name ) ;
  Assert( lDataSet.TestValid(TtiDataSet), cTIInvalidObjectError ) ;
  SaveDataSet(lDataSet);
end;

// -----------------------------------------------------------------------------
procedure TtiDatabaseTXTFlatFileAbs.DropTable( const pTableMetaData: TtiDBMetaDataTable);
var
  lDataSet : TtiDataSet;
  lFileName : string ;
begin
  lDataSet := FDataSets.FindByName( pTableMetaData.Name ) ;
  Assert( lDataSet.TestValid(TtiDataSet), cTIInvalidObjectError ) ;
  lFileName := ExpandFileName(lDataSet.Name);
  inherited DropTable(pTableMetaData);
  Assert( FileExists( lFileName ), 'File <' + lFileName + '> does not exist' ) ;
  SysUtils.DeleteFile( lFileName ) ;
  Assert( not FileExists( lFileName ), 'Unable to delete file <' + lFileName + '>' ) ;
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBConnectionPoolDataTXTAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBConnectionPoolDataTXTAbs.InitDBConnectionPool;
begin
  DBConnectionPool.MinPoolSize := 0 ;
  DBConnectionPool.MaxPoolSize := 1 ;
end;

function TtiDatabaseTXTFlatFileAbs.GetDataSetCount: integer;
begin
  result := FDataSets.Count ;
end;

function TtiDatabaseTXTFlatFileAbs.ExpandFileName(const pTableName: string): string;
var
  lFileName : string ;
begin
  if tiExtractExtension(pTableName) = '' then
    lFileName := tiSwapExt(pTableName, FilenameExt)
  else
    lFileName := pTableName ;
  result :=
    tiAddTrailingSlash( DatabaseName ) +
    lFileName ;
end;

procedure TtiDatabaseTXTFlatFileAbs.SetDirty(const pDataSet : TtiDataSet; pDirty : boolean);
var
  lDirty : boolean ;
begin
  lDirty := IsDirty(pDataSet) ;
  if (pDirty) and ( not lDirty) then
    FDirtyList.Add(pDataSet)
  else if (not pDirty) and (lDirty)then
    FDirtyList.Remove(pDataSet);
end;

function TtiDatabaseTXTFlatFileAbs.IsDirty(const pDataSet: TtiDataSet): boolean;
begin
  result := FDirtyList.IndexOf(pDataSet)<>-1;
end;

procedure TtiDatabaseTXTFlatFileAbs.SaveDataSet(const pDataSet: TtiDataSet);
var
  lWriter : TTXTToTIDataSetAbs ;
  lFileName : string ;
begin
  lWriter := TTXTToTIDataSetAbs.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name) ;
    lWriter.Save(pDataSet, lFileName );
  finally
    lWriter.Free;
  end;
end;

procedure TtiDatabaseTXTFlatFileAbs.ReadDataSet(const pDataSet: TtiDataSet);
var
  lFileName : string ;
  lWriter : TTXTToTIDataSetAbs;
begin
  lWriter := TTXTToTIDataSetAbs.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name) ;
    pDataSet.Clear ;
    lWriter.Read(pDataSet, lFileName );
  finally
    lWriter.Free;
  end;
end;

procedure TtiQueryTXTAbs.DoSelectRows(const pDataSet: TtiDataSet; pWhere: TtiQueryParams);
  function _SelectRow( const pDataSetRow : TtiDataSetRow ; pWhere : TtiQueryParams ) : boolean ;
  var
    i : integer ;
    lCell : TtiDataSetCell ;
    lParamVal : String ;
  begin
    result := true ;
    for i := 0 to pWhere.Count - 1 do
    begin
      lCell := pDataSetRow.FindByFieldName( pWhere.Items[i].Name );
      if lCell = nil then
        tiFmtException( 'Invalid field name <' + pWhere.Items[i].Name + '>', ClassName, 'DoSelectRows' ) ;
      lParamVal := pWhere.Items[i].GetValueAsString ;
      if lCell.ValueAsString <> lParamVal then
      begin
        result := false;
        Exit ; //==>
      end;
    end ;
  end;
var
  i : integer ;
begin
  FSelectedRows.Clear ;
  FCurrentRecordIndex := 0 ;
  if pWhere = nil then
    for i := 0 to pDataSet.Count - 1 do
      FSelectedRows.Add( pDataSet.Items[i] )
  else
    for i := 0 to pDataSet.Count - 1 do
      if _SelectRow( pDataSet.Items[i], pWhere ) then
        FSelectedRows.Add( pDataSet.Items[i] ) ;
end;

procedure TtiQueryTXTAbs.DoUpdateRow(const pDataSetRow: TtiDataSetRow;pParams: TtiQueryParams);
var
  i : integer ;
  lCell : TtiDataSetCell ;
begin
  FSelectedRows.Clear ;
  FCurrentRecordIndex := 0 ;
  for i := 0 to pParams.Count - 1 do
  begin
    lCell := pDataSetRow.FindByFieldName( pParams.Items[i].Name );
    if lCell = nil then
      tiFmtException( 'Invalid field name <' + pParams.Items[i].Name + '>', ClassName, 'DoSelectRows' ) ;
    DoAssignParamToCell( lCell, pParams.Items[i] ) ;
  end ;
end;

function TtiDatabaseTXTAbs.FindDataSetByName(const pName : string ) : TtiDataSet ;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FDataSets.Count - 1 do
    if SameText( pName, TtiDataSet( FDataSets.Items[i] ).Name ) then
    begin
      result := TtiDataSet( FDataSets.Items[i] );
      Exit ; //==>
    end
end;

function TtiQueryTXTAbs.CurrentRow: TtiDataSetRow;
begin
  Assert( FCurrentRecordIndex < FSelectedRows.Count, 'FCurrentRecordIndex >= FSelectedRows.Count' ) ;
  if FSelectedRows.Count > 0 then
  begin
    result := TtiDataSetRow(FSelectedRows.Items[FCurrentRecordIndex]);
    Assert( Result.TestValid, cTIInvalidObjectError );
  end
  else
    result := nil ;
end;

class procedure TtiDatabaseTXTFlatFileAbs.CreateDatabase(const psDatabaseName,psUserName, psPassword: string);
begin
  if DatabaseExists(psDatabaseName,psUserName,psPassword) then
    raise EtiOPFDBExceptionAlreadyExists.Create(cTIPersistBDEParadox, psDatabaseName, psUserName, psPassword);
  if not ForceDirectories(psDatabaseName) then
    raise EtiOPFDBExceptionCanNotCreateDatabase.Create(cTIPersistBDEParadox, psDatabaseName, psUserName, psPassword);
end;

class function TtiDatabaseTXTFlatFileAbs.DatabaseExists(const psDatabaseName,psUserName, psPassword: string):boolean;
begin
  result := DirectoryExists( psDatabaseName ) ;
end;


{ TTXTToTIDataSetAbs }

constructor TTXTToTIDataSetAbs.Create;
begin
  inherited Create ;
  TextFileMetaData := [tfmdFieldName];
  FFieldDelim      := ',' ;
  StringDelim     := '"' ;
  FRowDelim        := CrLf ;
end;

procedure TTXTToTIDataSetAbs.DoExtractData(pIndex: integer; const pValue: string);
var
  lCell : TtiDataSetCell ;
begin
  Assert( FDataSetRow.TestValid, cTIInvalidObjectError );
  if FDataSetRow.Count < pIndex then
  begin
    lCell := FDataSetRow.AddInstance ;
    lCell.ValueAsString := pValue ;
  end 
  else
    FDataSetRow.Items[pIndex-1].ValueAsString := pValue ;
end;

procedure TTXTToTIDataSetAbs.DoExtractDefaultFieldNames(pIndex: integer; const pValue: string);
begin
  if FDataSet.Fields.Count < pIndex then
    FDataSet.Fields.AddInstance( 'Field'+IntToStr(pIndex), qfkString )
  else
    FDataSet.Fields.Items[pIndex-1].Name := 'Field'+IntToStr(pIndex) ;
end;

procedure TTXTToTIDataSetAbs.DoExtractFieldName(pIndex: integer; const pValue: string);
begin
  if FDataSet.Fields.Count < pIndex then
    FDataSet.Fields.AddInstance( pValue, qfkString )
  else
    FDataSet.Fields.Items[pIndex-1].Name := pValue ;
end;

function TTXTToTIDataSetAbs.QuoteStr(const pStr: string): string;
begin
  result := StringDelim + pStr + StringDelim ;
end;

procedure TTXTToTIDataSetAbs.Read(pDataSet: TtiDataSet; pFileName: TFileName);
begin
  Assert( pDataSet.TestValid, cTIInvalidObjectError );
  FDataSet := pDataSet ;
  FStream := TtiFileStream.CreateReadOnly(pFileName) ;
  try
    FStream.LineDelim := RowDelim ;
    if tfmdFieldName in TextFileMetaData then
      ReadFieldNames ;
    while not FStream.EOF do
      ReadRow ;
  finally
    FDataSet := nil ;
    FreeAndNil(FStream);
  end;
end;

procedure TTXTToTIDataSetAbs.ReadFieldNames;
var
  ls : string ;
begin
  FDataSet.Fields.Clear ;
  ls := FStream.ReadLn;
  stExtractTokensL( ls, FieldDelim, FStringDelimAsChar, false, DoExtractFieldName );
end;

procedure TTXTToTIDataSetAbs.ReadRow;
var
  ls : string ;
begin
  ls := FStream.ReadLn;
  if FDataSet.Fields.Count = 0 then
    stExtractTokensL( ls, FieldDelim, FStringDelimAsChar, false, DoExtractDefaultFieldNames );
  FDataSetRow := FDataSet.AddInstance ;
  stExtractTokensL( ls, FieldDelim, FStringDelimAsChar, false, DoExtractData );
end;

procedure TTXTToTIDataSetAbs.Save(pDataSet: TtiDataSet; pFileName: TFileName);
var
  i : integer ;             
begin
  Assert( pDataSet.TestValid, cTIInvalidObjectError );
  FDataSet := pDataSet ;
  FStream := TtiFileStream.CreateReadWrite(pFileName, true ) ;
  try
    FStream.LineDelim := RowDelim ;
    if tfmdFieldName in TextFileMetaData then
      WriteFieldNames ;
    for i := 0 to FDataSet.Count - 1 do
      WriteRow( FDataSet.Items[i] );
  finally
    FDataSet := nil ;
    FreeAndNil(FStream);
  end;
end;

procedure TTXTToTIDataSetAbs.SetStringDelim(const Value: string);
begin
  Assert(Length(Value)<=1, 'StringDelim must be a single character');
  FStringDelim := Value;
  if FStringDelim = '' then
    FStringDelimAsChar := #0
  else
    FStringDelimAsChar := StringDelim[1];
end;

procedure TTXTToTIDataSetAbs.WriteFieldNames;
var
  i : integer ;
begin
  Assert(FStream<>nil, 'FStream not assigned' ) ;
  Assert( FDataSet.TestValid, cTIInvalidObjectError );
  for i := 0 to FDataSet.Fields.Count - 1 do
  begin
    Assert( FDataSet.Fields.Items[i].TestValid, cTIInvalidObjectError );
    if i >= 1 then
      FStream.Write(FieldDelim);
    FStream.Write(QuoteStr(FDataSet.Fields.Items[i].Name));
  end ;
  if FDataSet.Fields.Count > 0 then
    FStream.WriteLn ;
end ;

procedure TTXTToTIDataSetAbs.WriteRow(const pRow: TtiDataSetRow);
var
  i : integer ;
begin
  for i := 0 to pRow.Count - 1 do
  begin
    if i > 0 then
      FStream.Write(FieldDelim);
    if pRow.Items[i].DataSetField.Kind = qfkString then
      FStream.Write(QuoteStr(pRow.Items[i].ValueAsString))
    else
      FStream.Write(pRow.Items[i].ValueAsString);
  end;
  if FDataSet.Fields.Count > 0 then
    FStream.WriteLn ;
end;

{ TCSVToTIDataSet }

constructor TCSVToTIDataSet.Create;
begin
  inherited Create ;
  FieldDelim      := ',' ;
  StringDelim     := '"' ;
  RowDelim        := CrLf ;
end;


{ TTABToTIDataSet }

constructor TTABToTIDataSet.Create;
begin
  inherited Create ;
  FieldDelim      := #09 ;
  StringDelim     := '"' ;
  RowDelim        := CrLf ;
end;


function TtiQueryTXTAbs.HasNativeLogicalType: boolean;
begin
  result := false ;
end;

{ TtiDatabaseTXTAbs }

constructor TtiDatabaseTXTAbs.create;
begin
  inherited;
  FConnected := false ;
  FInTransaction := false ;
  FDataSets := TtiDataSets.Create ;
end;

destructor TtiDatabaseTXTAbs.Destroy;
begin
  FDataSets.Free;
  inherited;
end;

procedure TtiDatabaseTXTAbs.CreateTable(const pTableMetaData: TtiDBMetaDataTable);
var
  lDataSet : TtiDataSet ;
  i : integer ;
begin
  lDataSet := FDataSets.FindByName( pTableMetaData.Name ) ;
  if lDataSet <> nil then
    tiFmtException( 'Attempt to create duplicate table name <' +
                    pTableMetaData.Name + '>',
                    ClassName, 'CreateTable' ) ;
  lDataSet := TtiDataSet.Create ;
  FDataSets.Add( lDataSet ) ;
  lDataSet.Name := tiExtractFileNameOnly(pTableMetaData.Name);
  for i := 0 to pTableMetaData.Count - 1 do
    lDataSet.Fields.AddInstance(pTableMetaData.Items[i].Name,
                                pTableMetaData.Items[i].Kind,
                                pTableMetaData.Items[i].Width);
end;

procedure TtiDatabaseTXTAbs.DropTable(const pTableMetaData: TtiDBMetaDataTable);
var
  lDataSet : TtiDataSet;
begin
  lDataSet := FDataSets.FindByName( pTableMetaData.Name ) ;
  if lDataSet = nil then
    tiFmtException( 'Attempt to delete unknown table <' +
                    pTableMetaData.Name + '>',
                    ClassName,
                    'CreateTable' ) ;
  FDataSets.Remove(lDataSet);
  Assert( FDataSets.FindByName( pTableMetaData.Name ) = nil,
          'Unable to remove metadata for <' + pTableMetaData.Name + '>' ) ;
end;

function TtiDatabaseTXTAbs.GetConnected: boolean;
begin
  result := FConnected ;
end;

procedure TtiDatabaseTXTAbs.SetInTransaction(pValue: Boolean);
begin
  FInTransaction := pValue ;
end;

function TtiDatabaseTXTAbs.Test: boolean;
begin
  result := false;
  Assert( false, 'Under construction' ) ;  
end;

function TtiQueryTXTAbs.GetFieldAsBooleanByIndex(pIndex: Integer): boolean;
var
  lCell : TtiDataSetCell ;
begin
  lCell := CurrentRow.Items[pIndex];
  if lCell = nil then
    tiFmtException( 'Invalid field index <' + IntToStr(pIndex) + '>', ClassName, 'GetFieldAsBooleanByIndex' ) ;
  result :=
     SameText( lCell.ValueAsString, 'TRUE' ) or
     SameText( lCell.ValueAsString, 'T' ) ;
end;

function TtiQueryTXTAbs.GetFieldAsDateTimeByIndex(pIndex: Integer): TDateTime;
var
  lCell : TtiDataSetCell ;
begin
  lCell := CurrentRow.Items[pIndex];
  if lCell = nil then
    tiFmtException( 'Invalid field index <' + IntToStr(pIndex) + '>', ClassName, 'GetFieldAsDateTimeByIndex' ) ;
  if lCell.ValueAsString <> '' then
    result := tiXMLStringToDateTime( lCell.ValueAsString )
  else
    result := 0 ;
end;

function TtiQueryTXTAbs.GetFieldAsFloatByIndex(pIndex: Integer): real;
var
  lCell : TtiDataSetCell ;
begin
  lCell := CurrentRow.Items[pIndex];
  if lCell = nil then
    tiFmtException( 'Invalid field index <' + IntToStr(pIndex) + '>', ClassName, 'GetFieldAsFloatByIndex' ) ;
  if lCell.ValueAsString <> '' then
    result := StrToFloat( lCell.ValueAsString )
  else
    result := 0 ;
end;

function TtiQueryTXTAbs.GetFieldAsIntegerByIndex(pIndex: Integer): Int64;
var
  lCell : TtiDataSetCell ;
{$IFDEF VER130}
  lValue : Integer ;
{$ELSE}
  lValue : Int64 ;
{$ENDIF}
begin
  lCell := CurrentRow.Items[pIndex];
  if lCell = nil then
    tiFmtException( 'Invalid field index <' + IntToStr(pIndex) + '>', ClassName, 'GetFieldAsIntegerByIndex' ) ;
  if lCell.ValueAsString <> '' then
    lValue := StrToInt(lCell.ValueAsString)
  else
    lValue := 0 ;
  result := lValue ;
end;

function TtiQueryTXTAbs.GetFieldAsStringByIndex(pIndex: Integer): string;
var
  lCell : TtiDataSetCell ;
  ls : string ;
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  lCell := CurrentRow.Items[pIndex];
  if lCell = nil then
    tiFmtException( 'Invalid field index <' + IntToStr(pIndex) + '>', ClassName, 'GetFieldAsStringByIndex' ) ;
  ls := lCell.ValueAsString ;
  result := FXMLRCTrans.InsertReserved( FReservedChars, ls ) ;
end;

function TtiQueryTXTAbs.GetFieldIsNullByIndex(pIndex: Integer): Boolean;
var
  lCell : TtiDataSetCell ;
begin
  lCell := CurrentRow.Items[pIndex];
  if lCell = nil then
    tiFmtException( 'Invalid field index <' + IntToStr(pIndex) + '>', ClassName, 'GetFieldIsNullByIndex' ) ;
  result := lCell.ValueAsString = '' ;
end;

end.



