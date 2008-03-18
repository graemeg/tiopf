unit tiQueryTXTAbs;

{$I tiDefines.inc}

interface
uses
   tiQuery
  ,Classes
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ,tiDataBuffer_BOM
  ,Contnrs
  ,tiBaseObject
  ,tiDataBuffer_Cli
  ,tiStreams
  ,SysUtils
  ,tiXML
 ;

const
  cErrorInvalidFieldName = 'Invalid field name <%s>';
  cErrorNoRowsSelected   = 'No rows currently selected so can''t perform this operation';
  cErrorUnableToAccessDirectory = 'Unable to find or create directory <%s>';
  cErrorAttemtpToCreateDuplicateTableName = 'Attempt to create duplicate table name <%s>';
  cErrorAttemtpToDeleteNonExistantTable = 'Attemtp to delete table that does not exist <%s>';
  cErrorInvalidFieldIndex = 'Invalid field index <%d>';

type

  TtiDatabaseTXTAbs = class;

  TtiDatabaseTXTAbs = class(TtiDatabase)
  private
    FDataSets : TtiDataBuffers;
    FInTransaction : boolean;
  protected
    FConnected : boolean;
    procedure   SetInTransaction(AValue : Boolean);
    function    GetConnected : boolean; override;
    property    DataSets : TtiDataBuffers read FDataSets;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   StartTransaction; override;
    function    InTransaction : boolean; override;
    procedure   ReadMetaDataTables(AData : TtiDBMetaData); override;
    procedure   ReadMetaDataFields(AData : TtiDBMetaDataTable); override;
    function    FindDataSetByName(const AName : string): TtiDataBuffer;
    procedure   CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure   DropTable(const ATableMetaData: TtiDBMetaDataTable); override;
    function    Test : boolean; override;
  end;

  TtiDatabaseTXTFlatFileAbs = class(TtiDatabaseTXTAbs)
  private
    FDirtyList : TObjectList;
    FFilenameExt : string;

  protected
    procedure   ClearDirtyList;
    function    GetDataSetCount: integer;
    function    ExpandFileName(const ATableName : string): string;
    procedure   SetDirty(const pDataSet : TtiDataBuffer; pDirty : boolean);
    function    IsDirty(const pDataSet : TtiDataBuffer): boolean;
    procedure   SetConnected(AValue : boolean); override;
    property    DataSetCount : integer read GetDataSetCount;
    procedure   SaveDataSet(const pDataSet : TtiDataBuffer); virtual;
    procedure   ReadDataSet(const pDataSet : TtiDataBuffer); virtual;
    property    FilenameExt : string  read FFilenameExt write FFilenameExt;

  public
    constructor Create; override;
    destructor  Destroy; override;

    class function  DatabaseExists(const ADatabaseName, AUserName, APassword : string):boolean; override;
    class procedure CreateDatabase(const ADatabaseName, AUserName, APassword : string); override;
    procedure   Commit; override;
    procedure   RollBack; override;

    procedure   DropTable(const ATableMetaData : TtiDBMetaDataTable); override;
    procedure   CreateTable(const ATableMetaData : TtiDBMetaDataTable); override;

  end;

  TtiQueryTXTAbs = class(TtiQueryNonSQL)
  private
    FCurrentRecordIndex : integer;
    FSelectedRows : TObjectList;
    FActive : boolean;
    procedure ClearSelectedRows;
  protected
    FXMLRCTrans: IXMLReservedCharsTranslator;
    FReservedChars : TtiReservedChar;

    procedure   DoSelectRows(const pDataSet : TtiDataBuffer; AWhere : TtiQueryParams);
    procedure   DoUpdateRow( const pDataSetRow : TtiDataBufferRow; AParams : TtiQueryParams);
    function    GetParamAsString(const AName: string): string; override;
    procedure   SetParamAsString(const AName, AValue: string); override;
    procedure   DoAssignParamToCell(const pCell : TtiDataBufferCell; pParam : TtiQueryParamAbs);

    function    CurrentRow : TtiDataBufferRow;

    function    GetFieldAsString(const AName: string): string;     override;
    function    GetFieldAsFloat(const AName: string): Extended;    override;
    function    GetFieldAsBoolean(const AName: string): boolean;   override;
    function    GetFieldAsInteger(const AName: string): Int64;     override;
    function    GetFieldAsDateTime(const AName: string):TDateTime; override;
    function    GetFieldIsNull(const AName: string): Boolean;      override;

    function    GetFieldAsStringByIndex(AIndex: Integer): string    ; override;
    function    GetFieldAsFloatByIndex(AIndex: Integer)  : extended; override;
    function    GetFieldAsBooleanByIndex(AIndex: Integer): boolean ; override;
    function    GetFieldAsIntegerByIndex(AIndex: Integer): Int64   ; override;
    function    GetFieldAsDateTimeByIndex(AIndex: Integer):TDateTime; override;
    function    GetFieldIsNullByIndex(AIndex: Integer):Boolean      ; override;

    function    GetSQL: TStrings; override;
    procedure   SetSQL(const AValue: TStrings); override;
    function    GetActive: boolean; override;
    procedure   SetActive(const AValue: boolean); override;
    function    GetEOF: boolean; override;

    property    CurrentRecordIndex : Integer     read FCurrentRecordIndex write FCurrentRecordIndex;
    property    SelectedRows     : TObjectList read FSelectedRows;

  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Open   ; override;
    procedure   Close  ; override;
    procedure   Next   ; override;
    procedure   ExecSQL; override;

    procedure    AttachDatabase(ADatabase : TtiDatabase); override;
    procedure    DetachDatabase;  override;
    procedure    Reset; override;

    function     FieldCount : integer; override;
    function     FieldName(AIndex : integer): string; override;
    function     FieldIndex(const AName : string): integer; override;
    function     FieldKind(AIndex : integer): TtiQueryFieldKind; override;
    function     FieldSize(AIndex : integer): integer; override;
    function     HasNativeLogicalType : boolean; override;

  public

  end;


  TTXTToTIDataSetAbs = class(TtiBaseObject)
  private
    FTextFileMetaData: TTextFileMetaDatas;
    FFieldDelim: string;
    FStringDelim: string;
    FStringDelimAsChar: AnsiChar;
    FRowDelim  : string;
    FStream    : TtiFileStream;
    FDataSet   : TtiDataBuffer;
    FDataSetRow : TtiDataBufferRow;
    function  QuoteStr(const AStr : string):string;
    procedure WriteFieldNames;
    procedure WriteRow(const pRow : TtiDataBufferRow);
    procedure ReadFieldNames;
    procedure ReadRow;
    procedure DoExtractFieldName(AIndex : integer; const AValue : string);
    procedure DoExtractData(AIndex : integer; const AValue : string);
    procedure DoExtractDefaultFieldNames(AIndex : integer; const AValue : string);
    procedure SetStringDelim(const AValue: string);
  protected
  public
    constructor Create; virtual;
    property    TextFileMetaData : TTextFileMetaDatas read FTextFileMetaData write FTextFileMetaData;
    property    FieldDelim : string read FFieldDelim write FFieldDelim;
    property    StringDelim : string read FStringDelim write SetStringDelim;
    property    RowDelim   : string read FRowDelim write FRowDelim;
    procedure   Save(pDataSet : TtiDataBuffer; AFileName : TFileName);
    procedure   Read(pDataSet : TtiDataBuffer; AFileName : TFileName);
  end;

  TCSVToTIDataSet = class(TTXTToTIDataSetAbs)
  public
    constructor Create; override;
  end;

  TTABToTIDataSet = class(TTXTToTIDataSetAbs)
  public
    constructor Create; override;
  end;



implementation
uses
   tiLog
  ,tiObject
  ,tiUtils
  ,TypInfo
  ,tiConstants
  ,tiExcept
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
 ;

const
  cuXMLTagTable     = 'table';
  cuXMLTagTables    = 'tables';
  cuXMLTagTableName = 'table_name';
  cuXMLTagFields    = 'fields';
  cuXMLTagField     = 'field';
  cuXMLTagFieldName = 'field_name';
  cuXMLTagFieldKind = 'field_kind';
  cuXMLTagFieldSize = 'field_Size';
  cuXMLTagRows      = 'rows';
  cuXMLTagRow       = 'row';

  // ToDo: Implement escape chars for strings
  cDSCharCr    = #13; cgDSSubstCharCr    = '&cr;';
  cDSCharLf    = #10; cgDSSubstCharLf    = '&lf;';
  cDSCharComma = #13; cgDSSubstCharComma = '&cr;';


{ TtiQueryTXTAbs }

constructor TtiQueryTXTAbs.Create;
begin
  inherited;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator;
  FSelectedRows := TObjectList.Create(false);
  FActive := false;
end;


destructor TtiQueryTXTAbs.Destroy;
begin
  FSelectedRows.Free;
  inherited;
end;


procedure TtiQueryTXTAbs.ClearSelectedRows;
begin
  FSelectedRows.Clear;
  FSelectedRows.Capacity:= 0;
  FCurrentRecordIndex := -1;
end;

procedure TtiQueryTXTAbs.Close;
begin
  Active := false;
end;


procedure TtiQueryTXTAbs.ExecSQL;
begin
  Assert(false, 'Not implemented in ' + ClassName);
end;


function TtiQueryTXTAbs.GetFieldAsBoolean(const AName: string): boolean;
var
  lCell : TtiDataBufferCell;
begin
  lCell := CurrentRow.FindByFieldName(AName);
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldName, [AName]);
  result :=
     SameText(lCell.ValueAsString, 'TRUE') or
     SameText(lCell.ValueAsString, 'T');
end;


function TtiQueryTXTAbs.GetFieldAsDateTime(const AName: string): TDateTime;
var
  lCell : TtiDataBufferCell;
begin
  lCell := CurrentRow.FindByFieldName(AName);
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldName, [AName]);
  if lCell.ValueAsString <> '' then
    result := tiXMLStringToDateTime(lCell.ValueAsString)
  else
    result := 0;
end;


function TtiQueryTXTAbs.GetFieldAsString(const AName: string): string;
var
  lCell : TtiDataBufferCell;
  ls : string;
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  lCell := CurrentRow.FindByFieldName(AName);
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldName, [AName]);
  ls := lCell.ValueAsString;
  result := FXMLRCTrans.InsertReserved(FReservedChars, ls);
end;


function TtiQueryTXTAbs.GetParamAsString(const AName: string): string;
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  result := FXMLRCTrans.InsertReserved(FReservedChars, Params.GetValueAsString(AName));
end;


procedure TtiQueryTXTAbs.SetParamAsString(const AName, AValue: string);
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  Params.SetValueAsString(AName, FXMLRCTrans.InsertReserved(FReservedChars, AValue));
end;


procedure TtiQueryTXTAbs.DoAssignParamToCell(const pCell: TtiDataBufferCell;pParam: TtiQueryParamAbs);
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  if pParam.Kind = qfkString then
    pCell.ValueAsString := FXMLRCTrans.RemoveReserved(FReservedChars, pParam.GetValueAsString)
  else
    pCell.ValueAsString := pParam.GetValueAsString;
end;


function TtiQueryTXTAbs.GetFieldAsFloat(const AName: string): extended;
var
  lCell : TtiDataBufferCell;
begin
  lCell := CurrentRow.FindByFieldName(AName);
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldName, [AName]);
  if lCell.ValueAsString <> '' then
    result := StrToFloat(lCell.ValueAsString)
  else
    result := 0;
end;


function TtiQueryTXTAbs.GetFieldAsInteger(const AName: string): Int64;
var
  lCell : TtiDataBufferCell;
  lValue : Int64;
begin
  lCell := CurrentRow.FindByFieldName(AName);
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldName, [AName]);
  if lCell.ValueAsString <> '' then
    lValue := StrToInt64(lCell.ValueAsString)
  else
    lValue := 0;
  result := lValue;
end;


function TtiQueryTXTAbs.GetActive: boolean;
begin
  result := FActive;
end;


function TtiQueryTXTAbs.GetEOF: boolean;
begin
  result := FCurrentRecordIndex >= FSelectedRows.Count;
end;


function TtiQueryTXTAbs.GetSQL: TStrings;
begin
  Assert(false, ClassName + '.GetSQL not implemented');
  result := nil;
end;


procedure TtiQueryTXTAbs.Next;
begin
  Inc(FCurrentRecordIndex);
end;


procedure TtiQueryTXTAbs.Open;
begin
  Active := true;
end;


procedure TtiQueryTXTAbs.SetActive(const AValue: boolean);
begin
  if Active = AValue then
    Exit; //==>
  if Active and Not AValue then
  begin
    ClearSelectedRows;
    FActive := false;
    Exit; //==>
  end;
  Assert(false, 'Can not call ' + ClassName + '.SetActive(true)');
end;


procedure TtiQueryTXTAbs.SetSQL(const AValue: TStrings);
begin
  Assert(false, ClassName + '.SetSQL not implemented');
end;


procedure TtiQueryTXTAbs.AttachDatabase(ADatabase: TtiDatabase);
begin
  inherited AttachDatabase(ADatabase);
end;


procedure TtiQueryTXTAbs.DetachDatabase;
begin
  inherited DetachDatabase;
end;


function TtiQueryTXTAbs.FieldCount: integer;
begin
  if CurrentRow = nil then
  begin
    raise EtiOPFProgrammerException.Create(cErrorNoRowsSelected);
    result := 0;
  end
  else
    result := CurrentRow.Count;
end;


function TtiQueryTXTAbs.FieldName(AIndex: integer): string;
begin
  if CurrentRow = nil then
    raise EtiOPFProgrammerException.Create(cErrorNoRowsSelected)
  else
    result := CurrentRow.Items[AIndex].DataSetField.Name;
end;


procedure TtiQueryTXTAbs.Reset;
begin
  Assert(false, 'Under construction');
end;


function TtiQueryTXTAbs.FieldIndex(const AName: string): integer;
begin
  if CurrentRow = nil then
    raise EtiOPFProgrammerException.Create(cErrorNoRowsSelected)
  else
    result := CurrentRow.FindByFieldName(AName).Index;
end;


{ TtiDatabaseTXTAbs }

constructor TtiDatabaseTXTFlatFileAbs.Create;
begin
  inherited;
  FFilenameExt := 'XXX'; //  This should be overridden
  FDirtyList := TObjectList.Create(false);
end;


destructor TtiDatabaseTXTFlatFileAbs.Destroy;
begin
  FDirtyList.Free;
  inherited;
end;


procedure TtiDatabaseTXTFlatFileAbs.ClearDirtyList;
begin
  FDirtyList.Clear;
  FDirtyList.Capacity:= FDirtyList.Count;
end;

procedure TtiDatabaseTXTFlatFileAbs.Commit;
var
  i : integer;
begin
  for i := 0 to FDataSets.Count - 1 do
    if IsDirty(FDataSets.Items[i]) then
      SaveDataSet(FDataSets.Items[i]);
  FInTransaction := false;
end;


function TtiDatabaseTXTAbs.InTransaction: boolean;
begin
  result := FInTransaction;
end;


procedure TtiDatabaseTXTFlatFileAbs.RollBack;
var
  i : integer;
begin
  for i := 0 to FDataSets.Count - 1 do
    if IsDirty(FDataSets.Items[i]) then
    begin
      ReadDataSet(FDataSets.Items[i]);
      SetDirty(FDataSets.Items[i],False);
    end;
  FInTransaction := false;
end;


procedure TtiDatabaseTXTAbs.StartTransaction;
begin
  FInTransaction := true;
end;


procedure TtiDatabaseTXTAbs.ReadMetaDataFields(AData: TtiDBMetaDataTable);
var
  lTable     : TtiDBMetaDataTable;
  lDataSet   : TtiDataBuffer;
  i : integer;
begin
  lTable := (AData as TtiDBMetaDataTable);
  lDataSet := FDataSets.FindByName(lTable.Name);
  Assert(lDataSet <> nil,
          'Can not find metadata for <' + lTable.Name + '>');
  for i := 0 to lDataSet.Fields.Count - 1 do
    lTable.AddInstance(lDataSet.Fields.Items[i].Name,
                       lDataSet.Fields.Items[i].Kind,
                       lDataSet.Fields.Items[i].Width);
end;


procedure TtiDatabaseTXTAbs.ReadMetaDataTables(AData: TtiDBMetaData);
var
  lMetaData : TtiDBMetaData;
  lTable : TtiDBMetaDataTable;
  i : integer;
begin
  lMetaData := (AData as TtiDBMetaData);
  for i := 0 to FDataSets.Count - 1 do
  begin
    lTable := TtiDBMetaDataTable.Create;
    lTable.Name := FDataSets.Items[i].Name;
    lTable.ObjectState := posPK;
    lMetaData.Add(lTable);
    lMetaData.ObjectState := posClean;
  end;
end;


// This code is cloned in TtiQueryIB - Looks like we need to abstract more
// and introduce a TDataSet version of the TtiQuery
function TtiQueryTXTAbs.FieldKind(AIndex: integer): TtiQueryFieldKind;
begin
  if CurrentRow = nil then
    raise EtiOPFProgrammerException.Create(cErrorNoRowsSelected)
  else
    result := CurrentRow.Items[AIndex].DataSetField.Kind;
end;


function TtiQueryTXTAbs.FieldSize(AIndex: integer): integer;
begin
  if CurrentRow = nil then
    raise EtiOPFProgrammerException.Create(cErrorNoRowsSelected)
  else
    result := CurrentRow.Items[AIndex].DataSetField.Width;
end;


procedure TtiDatabaseTXTFlatFileAbs.SetConnected(AValue: boolean);
var
  lsl : TStringList;
  i : integer;
  lDataSet : TtiDataBuffer;
begin

  if (not AValue) then
  begin
    Log('Disconnecting from %s', [DatabaseName], lsConnectionPool);
    ClearDirtyList;
    FDataSets.Clear;
    FConnected := false;
    Exit; //==>
  end;

  if (not DirectoryExists(DatabaseName)) then
  begin
    ForceDirectories(DatabaseName);
    if not DirectoryExists(DatabaseName) then
      raise EtiOPFProgrammerException.CreateFmt(cErrorUnableToAccessDirectory, [DatabaseName]);

  end;
  lsl := TStringList.Create;
  try
    tiFilesToStringList(DatabaseName, '*.' + FilenameExt, lsl, false);
    for i := 0 to lsl.Count - 1 do
    begin
      lDataSet := TtiDataBuffer.Create;
      lDataSet.Name := tiRemoveExtension (ExtractFileName(lsl.Strings[i]));
      // ToDo: Should read datasets on demand, not all at once in the
      //       Connect method. Perpahs maintain another list of datasets that
      //       have been read.
      ReadDataSet(lDataSet);
      FDataSets.Add(lDataSet);
    end;
  finally
    lsl.Free;
  end;
  FConnected := true;
end;


function TtiQueryTXTAbs.GetFieldIsNull(const AName: string): Boolean;
var
  lCell : TtiDataBufferCell;
begin
  lCell := CurrentRow.FindByFieldName(AName);
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldName, [AName]);
  result := lCell.ValueAsString = '';
end;


procedure TtiDatabaseTXTFlatFileAbs.CreateTable(const ATableMetaData: TtiDBMetaDataTable);
var
  lDataSet : TtiDataBuffer;
begin
  inherited CreateTable(ATableMetaData);
  lDataSet := FDataSets.FindByName(ATableMetaData.Name);
  Assert(lDataSet.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  SaveDataSet(lDataSet);
end;


procedure TtiDatabaseTXTFlatFileAbs.DropTable(const ATableMetaData: TtiDBMetaDataTable);
var
  lDataSet : TtiDataBuffer;
  lFileName : string;
begin
  lDataSet := FDataSets.FindByName(ATableMetaData.Name);
  Assert(lDataSet.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  lFileName := ExpandFileName(lDataSet.Name);
  inherited DropTable(ATableMetaData);
  Assert(FileExists(lFileName), 'File <' + lFileName + '> does not exist');
  SysUtils.DeleteFile(lFileName);
  Assert(not FileExists(lFileName), 'Unable to delete file <' + lFileName + '>');
end;


// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBConnectionPoolDataTXTAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiDatabaseTXTFlatFileAbs.GetDataSetCount: integer;
begin
  result := FDataSets.Count;
end;

function TtiDatabaseTXTFlatFileAbs.ExpandFileName(const ATableName: string): string;
var
  lFileName : string;
begin
  if tiExtractExtension(ATableName) = '' then
    lFileName := tiSwapExt(ATableName, FilenameExt)
  else
    lFileName := ATableName;
  result :=
    tiAddTrailingSlash(DatabaseName) +
    lFileName;
end;


procedure TtiDatabaseTXTFlatFileAbs.SetDirty(const pDataSet : TtiDataBuffer; pDirty : boolean);
var
  LDirty : boolean;
begin
  LDirty := IsDirty(pDataSet);
  if (pDirty) and (not LDirty) then
    FDirtyList.Add(pDataSet)
  else if (not pDirty) and (LDirty)then
    FDirtyList.Remove(pDataSet);
  FDirtyList.Capacity:= FDirtyList.Count; // To Suppress leak reported in DUnit2
end;


function TtiDatabaseTXTFlatFileAbs.IsDirty(const pDataSet: TtiDataBuffer): boolean;
begin
  result := FDirtyList.IndexOf(pDataSet)<>-1;
end;


procedure TtiDatabaseTXTFlatFileAbs.SaveDataSet(const pDataSet: TtiDataBuffer);
var
  lWriter : TTXTToTIDataSetAbs;
  lFileName : string;
begin
  lWriter := TTXTToTIDataSetAbs.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name);
    lWriter.Save(pDataSet, lFileName);
  finally
    lWriter.Free;
  end;
  SetDirty(pDataSet,False);
end;


procedure TtiDatabaseTXTFlatFileAbs.ReadDataSet(const pDataSet: TtiDataBuffer);
var
  lFileName : string;
  lWriter : TTXTToTIDataSetAbs;
begin
  lWriter := TTXTToTIDataSetAbs.Create;
  try
    // Set the writers properties, based on optional parameters passed to the DB
    lFileName := ExpandFileName(pDataSet.Name);
    pDataSet.Clear;
    lWriter.Read(pDataSet, lFileName);
  finally
    lWriter.Free;
  end;
  SetDirty(pDataSet,False);
end;


procedure TtiQueryTXTAbs.DoSelectRows(const pDataSet: TtiDataBuffer; AWhere: TtiQueryParams);
  function _SelectRow(const pDataSetRow : TtiDataBufferRow; AWhere : TtiQueryParams): boolean;
  var
    i : integer;
    lCell : TtiDataBufferCell;
    lParamVal : String;
  begin
    result := true;
    for i := 0 to AWhere.Count - 1 do
    begin
      lCell := pDataSetRow.FindByFieldName(AWhere.Items[i].Name);
      if lCell = nil then
        raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldName, [AWhere.Items[i].Name]);
      lParamVal := AWhere.Items[i].GetValueAsString;
      if lCell.ValueAsString <> lParamVal then
      begin
        result := false;
        Exit; //==>
      end;
    end;
  end;
var
  i : integer;
begin
  ClearSelectedRows;
  if AWhere = nil then
    for i := 0 to pDataSet.Count - 1 do
      FSelectedRows.Add(pDataSet.Items[i])
  else
    for i := 0 to pDataSet.Count - 1 do
      if _SelectRow(pDataSet.Items[i], AWhere) then
        FSelectedRows.Add(pDataSet.Items[i]);
end;


procedure TtiQueryTXTAbs.DoUpdateRow(const pDataSetRow: TtiDataBufferRow;AParams: TtiQueryParams);
var
  i : integer;
  lCell : TtiDataBufferCell;
begin
  ClearSelectedRows;
  for i := 0 to AParams.Count - 1 do
  begin
    lCell := pDataSetRow.FindByFieldName(AParams.Items[i].Name);
    if lCell = nil then
      raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldName, [AParams.Items[i].Name]);
    DoAssignParamToCell(lCell, AParams.Items[i]);
  end;
end;


function TtiDatabaseTXTAbs.FindDataSetByName(const AName : string): TtiDataBuffer;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FDataSets.Count - 1 do
    if SameText(AName, TtiDataBuffer(FDataSets.Items[i]).Name) then
    begin
      result := TtiDataBuffer(FDataSets.Items[i]);
      Exit; //==>
    end
end;


function TtiQueryTXTAbs.CurrentRow: TtiDataBufferRow;
begin
  Assert(FCurrentRecordIndex < FSelectedRows.Count, 'FCurrentRecordIndex >= FSelectedRows.Count');
  if FSelectedRows.Count > 0 then
  begin
    result := TtiDataBufferRow(FSelectedRows.Items[FCurrentRecordIndex]);
    Assert(Result.TestValid, CTIErrorInvalidObject);
  end
  else
    result := nil;
end;


class procedure TtiDatabaseTXTFlatFileAbs.CreateDatabase(const ADatabaseName,AUserName, APassword: string);
begin
  if DatabaseExists(ADatabaseName,AUserName,APassword) then
    raise EtiOPFDBExceptionAlreadyExists.Create('Generic flat file persistence layer', ADatabaseName, AUserName, APassword);
  if not ForceDirectories(ADatabaseName) then
    raise EtiOPFDBExceptionCanNotCreateDatabase.Create('Generic flat file persistence layer', ADatabaseName, AUserName, APassword);
end;


class function TtiDatabaseTXTFlatFileAbs.DatabaseExists(const ADatabaseName,AUserName, APassword: string):boolean;
begin
  result := DirectoryExists(ADatabaseName);
end;


{ TTXTToTIDataSetAbs }

constructor TTXTToTIDataSetAbs.Create;
begin
  inherited Create;
  TextFileMetaData := [tfmdFieldName];
  FFieldDelim      := ',';
  StringDelim      := '"';
  FRowDelim        := CrLf;
end;


procedure TTXTToTIDataSetAbs.DoExtractData(AIndex: integer; const AValue: string);
var
  lCell : TtiDataBufferCell;
begin
  Assert(FDataSetRow.TestValid, CTIErrorInvalidObject);
  if FDataSetRow.Count < AIndex then
  begin
    lCell := FDataSetRow.AddInstance;
    lCell.ValueAsString := AValue;
  end 
  else
    FDataSetRow.Items[AIndex-1].ValueAsString := AValue;
end;


procedure TTXTToTIDataSetAbs.DoExtractDefaultFieldNames(AIndex: integer; const AValue: string);
begin
  if FDataSet.Fields.Count < AIndex then
    FDataSet.Fields.AddInstance('Field'+IntToStr(AIndex), qfkString)
  else
    FDataSet.Fields.Items[AIndex-1].Name := 'Field'+IntToStr(AIndex);
end;


procedure TTXTToTIDataSetAbs.DoExtractFieldName(AIndex: integer; const AValue: string);
begin
  if FDataSet.Fields.Count < AIndex then
    FDataSet.Fields.AddInstance(AValue, qfkString)
  else
    FDataSet.Fields.Items[AIndex-1].Name := AValue;
end;


function TTXTToTIDataSetAbs.QuoteStr(const AStr: string): string;
begin
  result := StringDelim + AStr + StringDelim;
end;


procedure TTXTToTIDataSetAbs.Read(pDataSet: TtiDataBuffer; AFileName: TFileName);
begin
  Assert(pDataSet.TestValid, CTIErrorInvalidObject);
  FDataSet := pDataSet;
  FStream := TtiFileStream.CreateReadOnly(AFileName);
  try
    FStream.LineDelim := RowDelim;
    if tfmdFieldName in TextFileMetaData then
      ReadFieldNames;
    while not FStream.EOF do
      ReadRow;
  finally
    FDataSet := nil;
    FreeAndNil(FStream);
  end;
end;


procedure TTXTToTIDataSetAbs.ReadFieldNames;
var
  ls : string;
begin
  FDataSet.Fields.Clear;
  ls := FStream.ReadLn;
  stExtractTokensL(ls, FieldDelim, FStringDelimAsChar, false, DoExtractFieldName);
end;


procedure TTXTToTIDataSetAbs.ReadRow;
var
  ls : string;
begin
  ls := FStream.ReadLn;
  if FDataSet.Fields.Count = 0 then
    stExtractTokensL(ls, FieldDelim, FStringDelimAsChar, false, DoExtractDefaultFieldNames);
  FDataSetRow := FDataSet.AddInstance;
  stExtractTokensL(ls, FieldDelim, FStringDelimAsChar, false, DoExtractData);
end;


procedure TTXTToTIDataSetAbs.Save(pDataSet: TtiDataBuffer; AFileName: TFileName);
var
  i : integer;             
begin
  Assert(pDataSet.TestValid, CTIErrorInvalidObject);
  FDataSet := pDataSet;
  FStream := TtiFileStream.CreateReadWrite(AFileName, true);
  try
    FStream.LineDelim := RowDelim;
    if tfmdFieldName in TextFileMetaData then
      WriteFieldNames;
    for i := 0 to FDataSet.Count - 1 do
      WriteRow(FDataSet.Items[i]);
  finally
    FDataSet := nil;
    FreeAndNil(FStream);
  end;
end;


procedure TTXTToTIDataSetAbs.SetStringDelim(const AValue: string);
begin
  Assert(Length(AValue)<=1, 'StringDelim must be a single character');
  FStringDelim := AValue;
  if FStringDelim = '' then
    FStringDelimAsChar := #0
  else
    FStringDelimAsChar := StringDelim[1];
end;


procedure TTXTToTIDataSetAbs.WriteFieldNames;
var
  i : integer;
begin
  Assert(FStream<>nil, 'FStream not assigned');
  Assert(FDataSet.TestValid, CTIErrorInvalidObject);
  for i := 0 to FDataSet.Fields.Count - 1 do
  begin
    Assert(FDataSet.Fields.Items[i].TestValid, CTIErrorInvalidObject);
    if i >= 1 then
      FStream.Write(FieldDelim);
    FStream.Write(QuoteStr(FDataSet.Fields.Items[i].Name));
  end;
  if FDataSet.Fields.Count > 0 then
    FStream.WriteLn;
end;


procedure TTXTToTIDataSetAbs.WriteRow(const pRow: TtiDataBufferRow);
var
  i : integer;
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
    FStream.WriteLn;
end;


{ TCSVToTIDataSet }

constructor TCSVToTIDataSet.Create;
begin
  inherited Create;
  FieldDelim     := ',';
  StringDelim    := '"';
  RowDelim       := CrLf;
end;


{ TTABToTIDataSet }

constructor TTABToTIDataSet.Create;
begin
  inherited Create;
  FieldDelim     := #09;
  StringDelim    := '"';
  RowDelim       := CrLf;
end;


function TtiQueryTXTAbs.HasNativeLogicalType: boolean;
begin
  result := false;
end;


{ TtiDatabaseTXTAbs }

constructor TtiDatabaseTXTAbs.Create;
begin
  inherited;
  FConnected := false;
  FInTransaction := false;
  FDataSets := TtiDataBuffers.Create;
end;


destructor TtiDatabaseTXTAbs.Destroy;
begin
  FDataSets.Free;
  inherited;
end;


procedure TtiDatabaseTXTAbs.CreateTable(const ATableMetaData: TtiDBMetaDataTable);
var
  lDataSet : TtiDataBuffer;
  i : integer;
begin
  lDataSet := FDataSets.FindByName(ATableMetaData.Name);
  if lDataSet <> nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorAttemtpToCreateDuplicateTableName, [ATableMetaData.Name]);
  lDataSet := TtiDataBuffer.Create;
  FDataSets.Add(lDataSet);
  lDataSet.Name := tiExtractFileNameOnly(ATableMetaData.Name);
  for i := 0 to ATableMetaData.Count - 1 do
    lDataSet.Fields.AddInstance(ATableMetaData.Items[i].Name,
                                ATableMetaData.Items[i].Kind,
                                ATableMetaData.Items[i].Width);
end;


procedure TtiDatabaseTXTAbs.DropTable(const ATableMetaData: TtiDBMetaDataTable);
var
  lDataSet : TtiDataBuffer;
begin
  lDataSet := FDataSets.FindByName(ATableMetaData.Name);
  if lDataSet = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorAttemtpToDeleteNonExistantTable, [ATableMetaData.Name]);
  FDataSets.Remove(lDataSet);
  Assert(FDataSets.FindByName(ATableMetaData.Name) = nil,
          'Unable to remove metadata for <' + ATableMetaData.Name + '>');
end;


function TtiDatabaseTXTAbs.GetConnected: boolean;
begin
  result := FConnected;
end;


procedure TtiDatabaseTXTAbs.SetInTransaction(AValue: Boolean);
begin
  FInTransaction := AValue;
end;


function TtiDatabaseTXTAbs.Test: boolean;
begin
  result := false;
  Assert(false, 'Under construction');  
end;


function TtiQueryTXTAbs.GetFieldAsBooleanByIndex(AIndex: Integer): boolean;
var
  lCell : TtiDataBufferCell;
begin
  lCell := CurrentRow.Items[AIndex];
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldIndex, [AIndex]);
  result :=
     SameText(lCell.ValueAsString, 'TRUE') or
     SameText(lCell.ValueAsString, 'T');
end;


function TtiQueryTXTAbs.GetFieldAsDateTimeByIndex(AIndex: Integer): TDateTime;
var
  lCell : TtiDataBufferCell;
begin
  lCell := CurrentRow.Items[AIndex];
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldIndex, [AIndex]);
  if lCell.ValueAsString <> '' then
    result := tiXMLStringToDateTime(lCell.ValueAsString)
  else
    result := 0;
end;


function TtiQueryTXTAbs.GetFieldAsFloatByIndex(AIndex: Integer): extended;
var
  lCell : TtiDataBufferCell;
begin
  lCell := CurrentRow.Items[AIndex];
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldIndex, [AIndex]);
  if lCell.ValueAsString <> '' then
    result := StrToFloat(lCell.ValueAsString)
  else
    result := 0;
end;


function TtiQueryTXTAbs.GetFieldAsIntegerByIndex(AIndex: Integer): Int64;
var
  lCell : TtiDataBufferCell;
  lValue : Int64;
begin
  lCell := CurrentRow.Items[AIndex];
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldIndex, [AIndex]);
  if lCell.ValueAsString <> '' then
    lValue := StrToInt64(lCell.ValueAsString)
  else
    lValue := 0;
  result := lValue;
end;


function TtiQueryTXTAbs.GetFieldAsStringByIndex(AIndex: Integer): string;
var
  lCell : TtiDataBufferCell;
  ls : string;
begin
  Assert(FReservedChars <> rcUnassigned, 'FReservedChars not assigned');
  lCell := CurrentRow.Items[AIndex];
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldIndex, [AIndex]);
  ls := lCell.ValueAsString;
  result := FXMLRCTrans.InsertReserved(FReservedChars, ls);
end;


function TtiQueryTXTAbs.GetFieldIsNullByIndex(AIndex: Integer): Boolean;
var
  lCell : TtiDataBufferCell;
begin
  lCell := CurrentRow.Items[AIndex];
  if lCell = nil then
    raise EtiOPFProgrammerException.CreateFmt(cErrorInvalidFieldIndex, [AIndex]);
  result := lCell.ValueAsString = '';
end;


end.
