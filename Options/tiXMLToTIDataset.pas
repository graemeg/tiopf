unit tiXMLToTIDataset;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiDataBuffer_BOM
  ,tiQuery
  ,tiStreams
  ,tiXML
  // Delphi
  ,Classes
 ;

const
  cErrorInvalidXMLWriterState          = 'Invalid tiXMLWriter.State';
  cErrorInvalidXMLDBParserState        = 'Invalid TtiXMLDBParserState';
  cErrorInvalidXMLFieldNameStyle       = 'Invalid TtiXMLFieldNameStyle';
  cErrorInvalidXMLFieldNameStyleStr    = 'Invalid TtiXMLFieldNameStyle <%s>';
  cErrorParsingXMLAtPos                = 'Error parsing XML at position %d';
  cErrorInvalidTXMLAttributeParseState = 'Invalid TXMLAttributeParseState';

type

{
<?xml version="1.0"?>
 <xmldocdata>
  <tiopf version="1.300"/>
  <tables>
    <table table_name="sqlman_group">
      <fields>
        <field field_name="oid" field_kind="string" field_Size="36"/>
        <field field_name="group_name" field_kind="string" field_Size="50"/>
        ... repeats
      </fields>
      <rows>
        <row oid="1000" group_name="1000" disp_order="1000"/>
        <row oid="2000" group_name="2000" disp_order="2000"/>
        ... repeats
      </rows>
    </table>
    ... repeats
  </tables>
</xmldocdata>
}

  TtiXMLFieldNameStyle = (xfnsInteger, xfnsString);
const

  cXMLFieldNameStyles : array[TtiXMLFieldNameStyle] of string =
    ('integer', 'string');

  function tiStringToXMLFieldNameStyle(const AValue: string): TtiXMLFieldNameStyle;


type

  TtiXMLDBParserState = (xdbsRoot, xdbsTables, xdbsTable, xdbsFields, xdbsField,
                          xdbsRows, xdbsRow, xdbsEnd);

  TXMLToDataSetReader = class(TtiBaseObject)
  private
    FXML: string;
    FState: TtiXMLDBParserState;
    FPos: Cardinal;
    FLen: Cardinal;
    FDataSets : TtiDataBuffers;
    FDataSet: TtiDataBuffer;
    FDataSetRow: TtiDataBufferRow;
    FXMLRCTrans: IXMLReservedCharsTranslator;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FXMLTags : TtiXMLTags;
    FCompress: string;
    procedure   SetXML(const AValue: string);
    function    ParseForSingleAttribute(const AAttrName: string): string;
    procedure   ParseForAttributesUntil(const pUntil: string);
    function    MatchToken(const pToken: string; pLength: byte; pNewState: TtiXMLDBParserState): boolean;
    function    GetOptXMLDBSize: TtiOptXMLDBSize;
    procedure   SetOptXMLDBSize(const AValue: TtiOptXMLDBSize);
  protected
    property    XML : string read FXML write SetXML;
    property    State : TtiXMLDBParserState read FState;
    property    DataSets: TtiDataBuffers read FDataSets write FDataSets;
    property    Pos : Cardinal read FPos;
    property    Len : Cardinal read FLen;

    procedure   DoAddTable(const ATableName : string); virtual;
    procedure   DoAddField(const AFieldName, AFieldKind, pFieldSize : string); virtual;
    procedure   DoAddRow; virtual;
    procedure   DoAddCell(const AFieldName : string; pFieldValue : string); virtual;
    procedure   Next; virtual;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Execute(const pXML: string; const pDataSets: TtiDataBuffers);
    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle;
    property    OptXMLDBSize: TtiOptXMLDBSize read GetOptXMLDBSize Write SetOptXMLDBSize;
    property    Compress: string read FCompress Write FCompress;
  end;

  TtiXMLWriterState = (xwsEmpty, xwsFields, xwsRows, xwsRow);

  // Provides Write only access to a tiOPF XML file
  TtiDataBufferToXMLWriter = class(TtiBaseObject)
  private
    FState : TtiXMLWriterState;
    FRow : string;
    FCellIndex: Integer;
    FPreSizedStream : TtiPreSizedStream;
    FXMLRCTrans: IXMLReservedCharsTranslator;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FXMLTags : TtiXMLTags;
    FCompress: string;
    procedure   DoAddCellAsString(const AName, AValue: string);
    function    CloseCurrentXML: string;
    procedure   InsertQueryMetaData(const ATableName: string; const AQuery: TtiQuery);
    function    InsertQueryData(const AQuery: TtiQuery):Integer;
    procedure   InsertDataSetMetaData(const pDataSet: TtiDataBuffer);
    procedure   InsertDataSetData(const pDataSet: TtiDataBuffer);
    function    GetOptXMLDBSize: TtiOptXMLDBSize;
    procedure   SetOptXMLDBSize(const AValue: TtiOptXMLDBSize);
  protected
    function    GetAsString: string;
    function    GetTableData: string;
    procedure   SetState(pState: TtiXMLWriterState);
  public
    constructor Create; overload;
    constructor Create(AInitialSize, AGrowBy : Int64); overload;
    destructor  Destroy; override;

    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle;
    property    OptXMLDBSize     : TtiOptXMLDBSize      read GetOptXMLDBSize      Write SetOptXMLDBSize;
    property    XMLTags          : TtiXMLTags read FXMLTags;
    property    Compress: string read FCompress Write FCompress;

    procedure   AddTable(const AName : string);
    procedure   AddField(const AFieldName : string; AFieldKind : TtiQueryFieldKind; AFieldWidth : Integer = 0);
    procedure   AddRow;
    procedure   AddCellAsString( const AName : string; const AValue : string);
    procedure   AddCellAsInteger(const AName : string; const AValue : Int64);
    procedure   AddCellAsFloat(  const AName : string; const AValue : Extended);
    procedure   AddCellAsDateTime(const AName : string; const AValue : TDateTime);
    procedure   AddCellAsBoolean(const AName : string; const AValue : Boolean);
    procedure   AddCellAsStream( const AName : string; const AValue : TStream);
    property    AsString: string read GetAsString;
    property    TableData: string read GetTableData;
    // Returns the number of rows added
    function    AssignFromTIQuery(const ATableName : string;const AQuery : TtiQuery): Integer;
    procedure   AssignMetaDataFromTIQuery(const ATableName : string;const AQuery : TtiQuery);
    procedure   AssignFromTIDataSets(const pDataSets: TtiDataBuffers);
    procedure   AssignFromTIDataSet(const pDataSet: TtiDataBuffer);

  end;

  // Provides Read/Write access to a tiOPF XML file
  TtiXMLToDataSetReadWriter = class(TtiBaseObject)
  private
    FDataSets: TtiDataBuffers;
    FDataSet : TtiDataBuffer;
    FMetaDataField : TtiDBMetaDataField;
    FDataSetRow : TtiDataBufferRow;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FOptXMLDBSize: TtiOptXMLDBSize;
    FCompress: string;
    procedure SetCompress(const AValue: string);

  protected
    procedure  Clear;
    procedure  SetAsString(const AValue: string);
    function   GetAsString: string;

  public
    constructor Create;
    destructor  Destroy; override;
    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle;

    property    DataSets : TtiDataBuffers read FDataSets write FDataSets;
    procedure   LoadFromFile(const AFileName : string; pReadOnly : Boolean = false);
    procedure   SaveToFile(const AFileName : string);
    property    AsString : string read GetAsString write SetAsString;
    property    OptXMLDBSize: TtiOptXMLDBSize read FOptXMLDBSize Write FOptXMLDBSize;
    property    Compress: string read FCompress Write SetCompress;

  end;

// Some helper methods
procedure tiXMLStringToTIDataSets(const AXMLString : string;
  const ADataSets : TtiDataBuffers); overload;
procedure tiXMLStringToTIDataSets(const AXMLString : string;
  const ADataSets : TtiDataBuffers;
  const AOptXMLDBSize: TtiOptXMLDBSize;
  const AXMLFieldNameStyle: TtiXMLFieldNameStyle); overload;
function  tiTIDataSetsToXMLString(const pDataSets : TtiDataBuffers): string;
function  tiTIDataSetToXMLString( const pDataSet : TtiDataBuffer): string;

// ToDo: Move these to implementation when optimisation is done
const
  cWriteRecordCount = 500;

implementation
uses
   tiUtils
  ,tiConstants
  ,tiExcept
  ,tiCompress
  // Delphi
  ,SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows // Debugging
  {$ENDIF}
 ;

procedure tiXMLStringToTIDataSets(
  const AXMLString : string;
  const ADataSets : TtiDataBuffers);
begin
  tiXMLStringToTIDataSets(AXMLString, ADataSets,
    optDBSizeOff, xfnsString);
end;

procedure tiXMLStringToTIDataSets(
  const AXMLString : string;
  const ADataSets : TtiDataBuffers;
  const AOptXMLDBSize: TtiOptXMLDBSize;
  const AXMLFieldNameStyle: TtiXMLFieldNameStyle);
var
  LXMLToTIDataSet : TtiXMLToDataSetReadWriter;
begin
  LXMLToTIDataSet := TtiXMLToDataSetReadWriter.Create;
  try
    LXMLToTIDataSet.DataSets := ADataSets;
    LXMLToTIDataSet.OptXMLDBSize:= AOptXMLDBSize;
    LXMLToTIDataSet.XMLFieldNameStyle:= AXMLFieldNameStyle;
    LXMLToTIDataSet.AsString := AXMLString;
  finally
    LXMLToTIDataSet.Free;
  end;
end;

function tiTIDataSetsToXMLString(const pDataSets : TtiDataBuffers): string;
var
  lXMLToTIDataSet : TtiXMLToDataSetReadWriter;
begin
  lXMLToTIDataSet := TtiXMLToDataSetReadWriter.Create;
  try
    lXMLToTIDataSet.DataSets := pDataSets;
    result := lXMLToTIDataSet.AsString;
  finally
    lXMLToTIDataSet.Free;
  end;
end;

function  tiTIDataSetToXMLString( const pDataSet : TtiDataBuffer): string;
var
  lDataSets: TtiDataBuffers;
begin
  lDataSets:= TtiDataBuffers.Create;
  try
    lDataSets.Add(pDataSet);
    try
      Result := tiTIDataSetsToXMLString(lDataSets);
    finally
      lDataSets.Extract(pDataSet);
    end;
  finally
    lDataSets.Free;
  end;
end;

function tiStringToXMLFieldNameStyle(const AValue: string): TtiXMLFieldNameStyle;
var
  i : TtiXMLFieldNameStyle;
begin
  for i := Low(TtiXMLFieldNameStyle) to High(TtiXMLFieldNameStyle) do
    if SameText(AValue, cXMLFieldNameStyles[i]) then
    begin
      Result := i;
      Exit; //==>
    end;
  raise Exception.CreateFmt(cErrorInvalidXMLFieldNameStyleStr, [AValue]);
  //Result := Low(TtiXMLFieldNameStyle); // To shut the compiler up
end;

{ TtiXMLToDataSetReadWriter }

constructor TtiXMLToDataSetReadWriter.Create;
begin
  inherited;
  FXMLFieldNameStyle := xfnsString;
  FCompress         := cgsCompressNone;
end;

destructor TtiXMLToDataSetReadWriter.Destroy;
begin
  inherited;
end;

procedure TtiXMLToDataSetReadWriter.LoadFromFile(const AFileName: string; pReadOnly : Boolean = false);
var
  lStream : TFileStream;
  ls : string;
begin
  Assert(AFileName <> '', 'File name not assigned');
  if not pReadOnly then
    lStream := TFileStream.Create(AFileName, fmOpenReadWrite or fmShareDenyNone)
  else
    lStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    ls := tiStreamToString(lStream);
    SetAsString(ls);
  finally
    lStream.Free;
  end;
end;

{ TtiTIDataSetsToXML }

function TtiXMLToDataSetReadWriter.GetAsString: string;
var
  lXMLWriter : TtiDataBufferToXMLWriter;
begin
  Assert(DataSets.TestValid(TtiDataBuffers, true), CTIErrorInvalidObject);
  lXMLWriter := TtiDataBufferToXMLWriter.Create;
  try
    lXMLWriter.XMLFieldNameStyle := FXMLFieldNameStyle;
    lXMLWriter.OptXMLDBSize := FOptXMLDBSize;
    lXMLWriter.Compress := FCompress;
    // This makes it possible to write out an empty file
    if DataSets <> nil then
      lXMLWriter.AssignFromTIDataSets(DataSets);
    Result := lXMLWriter.AsString;
  finally
    lXMLWriter.Free;
  end;
end;

procedure TtiXMLToDataSetReadWriter.SetAsString(const AValue: string);
var
  lXMLToDataSetReader: TXMLToDataSetReader;
begin
  Assert(FDataSets.TestValid(TtiDataBuffers), CTIErrorInvalidObject);
  Clear;
  lXMLToDataSetReader:= TXMLToDataSetReader.Create;
  try
    lXMLToDataSetReader.OptXMLDBSize := FOptXMLDBSize;
    lXMLToDataSetReader.XMLFieldNameStyle := FXMLFieldNameStyle;
    lXMLToDataSetReader.Compress := FCompress;
    lXMLToDataSetReader.Execute(AValue, FDataSets);
  finally
    lXMLToDataSetReader.Free;
  end;
end;

procedure TtiXMLToDataSetReadWriter.SaveToFile(const AFileName: string);
var
  LDirectory: string;
  lStream : TFileStream;
  ls : string;
begin
  Assert(AFileName <> '', 'File name not assigned');
  LDirectory:= ExtractFilePath(AFileName);
  if (LDirectory <> '') and
     (not DirectoryExists(LDirectory)) then
    tiForceDirectories(LDirectory);
  lStream := TFileStream.Create(AFileName, fmCreate or fmShareExclusive);
  try
    ls := AsString;
    tiStringToStream(ls, lStream);
  finally
    lStream.Free;
  end;
end;

procedure TtiXMLToDataSetReadWriter.Clear;
begin
  FDataSet      := nil;
  FMetaDataField := nil;
  FDataSetRow   := nil;
  FDataSets.Clear;
end;

procedure TXMLToDataSetReader.Execute(const pXML: string; const pDataSets: TtiDataBuffers);
begin
  Assert(pDataSets.TestValid(TtiDataBuffers), CTIErrorInvalidObject);
  XML := pXML;
  DataSets := pDataSets;
  while FState <> xdbsEnd do
    Next;
end;

function TXMLToDataSetReader.MatchToken(const pToken : string;
                                        pLength : byte;
                                        pNewState: TtiXMLDBParserState): boolean;
var
  lStr : string;
begin
  lStr := Copy(FXML, FPos, pLength);
  if (lStr = pToken)  then
  begin
    Inc(FPos, pLength);
    result := true;
    FState := pNewState;
  end else
    result := false;
end;

procedure TXMLToDataSetReader.Next;
var
  lTableName: string;
  lFieldName: string;
  lFieldKind: string;
  lFieldSize: string;
  lPos: Cardinal;
begin
  lPos := FPos;
  while (FPos <= FLen) do
  begin
    case FState of
    xdbsRoot : if MatchToken(FXMLTags.TablesStart, FXMLTags.LenTablesStart, xdbsTables) then
                 Break; //==>
    xdbsTables :
      begin
        if MatchToken(FXMLTags.TableStart,FXMLTags.LenTableStart, xdbsTable) then
          Break; //==>
        if MatchToken(FXMLTags.TableEnd,FXMLTags.LenTableEnd, xdbsTables) then
          Break; //==>
        if MatchToken(FXMLTags.TablesEnd,FXMLTags.LenTablesEnd, xdbsEnd) then
          Break; //==>
      end;
    xdbsTable :
      begin
        lTableName := ParseForSingleAttribute(FXMLTags.TableName);
        DoAddTable(lTableName);
        FState := xdbsFields;
        Break; //==>
      end;
    xdbsFields :
      begin
        if MatchToken(FXMLTags.FieldStart,FXMLTags.LenFieldStart, xdbsField) then
          Break; //==>
        if MatchToken(FXMLTags.FieldsEnd,FXMLTags.LenFieldsEnd, xdbsRows) then
          Break; //==>
      end;
    xdbsField :
      begin
        lFieldName := ParseForSingleAttribute(FXMLTags.FieldName);
        lFieldKind := ParseForSingleAttribute(FXMLTags.FieldKind);
        lFieldSize := ParseForSingleAttribute(FXMLTags.FieldSize);
        DoAddField(lFieldName, lFieldKind, lFieldSize);
        FState := xdbsFields;
        Break; //==>
      end;
    xdbsRows :
      begin
        if MatchToken(FXMLTags.RowsStartEnd,FXMLTags.LenRowsStartEnd, xdbsTables) then
          Break; //==>
        if MatchToken(FXMLTags.RowStart,FXMLTags.LenRowStart, xdbsRow) then
        begin
          DoAddRow;
          Break; //==>
        end;
        if MatchToken(FXMLTags.RowsEnd,FXMLTags.LenRowsEnd, xdbsTables) then
          Break; //==>
      end;
    xdbsRow:
      begin
        ParseForAttributesUntil('/>');
        FState := xdbsRows;
        Break; //==>
      end;
    else
      raise exception.Create(cErrorInvalidXMLDBParserState);
    end;
    Inc(FPos);
  end;
  if lPos = FPos then
    raise Exception.CreateFmt(cErrorParsingXMLAtPos, [lPos]);
end;

procedure TXMLToDataSetReader.ParseForAttributesUntil(const pUntil: string);
var
  lState    : TtiXMLAttributeParseState;
  lStart    : Cardinal;
  lChar     : Char;
  lName     : string;
  lValue    : string;
  lStr : string;
begin
  lState    := xapsWaitForAttr;
  lStart    := 0; // To fix a compiler warning
  While (FPos <= FLen) do
  begin
    lStr := Copy(FXML, FPos, Length(pUntil));
    if (lStr = pUntil) then
    begin
      Inc(FPos,Length(pUntil));
      Exit; //==>
    end;
    lChar := FXML[FPos];
    case lState of
    xapsWaitForAttr : begin
                        if lChar <> #32 then
                        begin
                          lStart := FPos;
                          Inc(lState);
                        end;
                      end;
    xapsInAttrName : begin
                        if CharInSet(lChar, [#32, '=']) then
                        begin
                          Inc(lState);
                          lName := Copy(FXML, lStart, FPos - lStart);
                        end;
                      end;
    xapsWaitForAttrValue : begin
                        if (lChar = '"') then
                        begin
                          Inc(lState);
                          lStart := FPos;
                        end;
                      end;
    xapsInAttrValue : begin
                        if (lChar = '"') then
                        begin
                          lState := Low(TtiXMLAttributeParseState);
                          lValue := Copy(FXML, lStart+1, FPos - lStart - 1);
                          DoAddCell(lName, lValue);
                        end;
                      end;
    else
      raise EtiOPFInternalException.Create(cErrorInvalidTXMLAttributeParseState);
    end;
      Inc(FPos);
  end;
end;

function TXMLToDataSetReader.ParseForSingleAttribute(const AAttrName: string): string;
var
  lState    : TtiXMLAttributeParseState;
  lStart    : Cardinal;
  lChar     : Char;
  lStr : string;
begin
  lState    := xapsWaitForAttr;
  lStart    := 0; // To fix a compiler warning

  while FPos <= FLen do
  begin
    case lState of
    xapsWaitForAttr : begin
                        lStr := Copy(FXML, FPos, Length(AAttrName));
                        if (lStr = AAttrName) then
                        begin
                          Inc(FPos,Length(AAttrName)-1);
                          lState := xapsWaitForAttrValue;
                        end;
                      end;
    xapsInAttrName : begin
                        raise EtiOPFInternalException.Create(cErrorInvalidTXMLAttributeParseState);
                      end;
    xapsWaitForAttrValue : begin
                            lChar := FXML[FPos];
                            if (lChar = '"') then
                            begin
                              lState := xapsInAttrValue;
                              lStart := FPos;
                            end;
                          end;
    xapsInAttrValue : begin
                        lChar := FXML[FPos];
                        if (lChar = '"') then
                        begin
                          Result := Copy(FXML, lStart+1, FPos - lStart - 1);
                          Inc(FPos);
                          Exit; //==>
                        end;
                      end;
    else
      raise EtiOPFInternalException.Create(cErrorInvalidTXMLAttributeParseState);
    end;
    Inc(FPos);
  end;
end;

procedure TXMLToDataSetReader.SetXML(const AValue: string);
begin
  if FCompress <> cgsCompressNone then
    FXML := tiDecompressString(AValue, FCompress)
  else
    FXML := AValue;
  FLen := Length(FXML);
  FPos := 1;
  FState := xdbsRoot;
end;

procedure TXMLToDataSetReader.DoAddField(const AFieldName, AFieldKind, pFieldSize: string);
var
  lFieldKind : TtiQueryFieldKind;
  lFieldWidth : integer;
begin
  Assert(FDataSet.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  lFieldKind := StrToQueryFieldKind(AFieldKind);
  lFieldWidth := StrToInt(pFieldSize);
  FDataSet.Fields.AddInstance(AFieldName, lFieldKind, lFieldWidth);
end;

procedure TXMLToDataSetReader.DoAddRow;
begin
  Assert(FDataSet.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  FDataSetRow := FDataSet.AddInstance;
end;

procedure TXMLToDataSetReader.DoAddTable(const ATableName: string);
begin
  Assert(FDataSets.TestValid(TtiDataBuffers), CTIErrorInvalidObject);
  FDataSet := FDataSets.AddInstance(ATableName);
end;

procedure TXMLToDataSetReader.DoAddCell(const AFieldName: string; pFieldValue: string);
var
  lCell: TtiDataBufferCell;
  lIndex: Integer;
begin
  Assert(FDataSetRow.TestValid(TtiDataBufferRow), CTIErrorInvalidObject);
  case FXMLFieldNameStyle of
    xfnsString : lCell := FDataSetRow.FindByFieldName(AFieldName);
    xfnsInteger: begin
                   lIndex := tiDecodeWordBase26(AFieldName);
                   lCell := FDataSetRow.Items[lIndex];
                 end;
  else
    raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
  end;

  //if FDataSet.Fields.FindByFieldName(AFieldName).Kind = qfkString then
  if lCell.DataSetField.Kind = qfkString then
    lCell.ValueAsString := FXMLRCTrans.InsertReserved(rcXML, pFieldValue)
  else
    lCell.ValueAsString := pFieldValue;

end;

procedure TtiXMLToDataSetReadWriter.SetCompress(const AValue: string);
begin
  FCompress := AValue;
end;

{ TtiDataBufferToXMLWriter }

procedure TtiDataBufferToXMLWriter.AddCellAsBoolean(const AName: string;const AValue: Boolean);
begin
  DoAddCellAsString(AName, tiBooleanToStr(AValue));
end;

procedure TtiDataBufferToXMLWriter.AddCellAsDateTime(const AName: string; const AValue: TDateTime);
begin
  DoAddCellAsString(AName, tiDateTimeAsXMLString(AValue));
end;

procedure TtiDataBufferToXMLWriter.AddCellAsFloat(const AName: string;const AValue: Extended);
begin
  DoAddCellAsString(AName, FloatToStr(AValue));
end;

procedure TtiDataBufferToXMLWriter.AddCellAsInteger(const AName: string;const AValue: Int64);
begin
  DoAddCellAsString(AName, IntToStr(AValue));
end;

procedure TtiDataBufferToXMLWriter.AddCellAsStream(const AName: string; const AValue: TStream);
var
  ls : string;
  lStream : TStringStream;
  lPos : Integer;
begin
  lPos := AValue.Position;
  try
    lStream := TStringStream.Create('');
    try
      AValue.Position := 0;
      MimeEncodeStream(AValue, lStream);
      ls := lStream.DataString;
    finally
      lStream.Free;
    end;
  finally
    AValue.Position := lPos;
  end;
  DoAddCellAsString(AName, ls);
end;

procedure TtiDataBufferToXMLWriter.AddCellAsString(const AName, AValue: string);
var
  ls : string;
begin
  ls := FXMLRCTrans.RemoveReserved(rcXML, AValue);
  DoAddCellAsString(AName, ls);
end;                                

procedure TtiDataBufferToXMLWriter.DoAddCellAsString(const AName, AValue: string);
begin
  Assert(FState = xwsRow, 'State <> xwsRow');
  if FRow <> '' then FRow := FRow + ' ';
  case FXMLFieldNameStyle of
    xfnsString : FRow := FRow + LowerCase(AName) + '="' + AValue + '"';
    xfnsInteger: begin
                   FRow := FRow + tiEncodeWordBase26(FCellIndex) + '="' + AValue + '"';
                   Inc(FCellIndex);
                 end;
  else
    raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
  end;
end;

procedure TtiDataBufferToXMLWriter.AddField(const AFieldName: string;
  AFieldKind: TtiQueryFieldKind; AFieldWidth: Integer);
var
  ls : string;
begin
  Assert(FState = xwsFields, 'State <> xwsFields');
  ls :=
    '<' + FXMLTags.Field + ' ' + FXMLTags.FieldName + '="' + AFieldName + '" '
                               + FXMLTags.FieldKind + '="' + LowerCase(cgaQueryFieldKind[AFieldKind]) + '" '
                               + FXMLTags.FieldSize + '="' + IntToStr(AFieldWidth) + '"/>';
  FPreSizedStream.Write(ls);
end;

procedure TtiDataBufferToXMLWriter.AddRow;
var
  ls : string;
begin
  case FState of
    xwsFields: ls := FXMLTags.FieldsEnd + FXMLTags.RowsStart;
    xwsRow:    ls := FRow + FXMLTags.RowEnd;
  else
    raise EtiOPFProgrammerException.Create(cErrorInvalidXMLWriterState);
  end;
  FPreSizedStream.Write(ls + FXMLTags.RowStart);
  FRow := '';
  FState := xwsRow;
  FCellIndex:= 0;
end;

function TtiDataBufferToXMLWriter.CloseCurrentXML:string;
begin
  case FState of
  xwsEmpty:  Result := '';
  xwsFields: Result := FXMLTags.FieldsEnd + FXMLTags.RowsStart + FXMLTags.RowsEnd + FXMLTags.TableEnd;
  xwsRows:   Result := FXMLTags.RowsEnd + FXMLTags.TableEnd;
  xwsRow:    Result := FRow + FXMLTags.RowEnd + FXMLTags.RowsEnd + FXMLTags.TableEnd;
  else
    raise EtiOPFProgrammerException.Create(cErrorInvalidXMLWriterState);
  end;
end;

procedure TtiDataBufferToXMLWriter.AddTable(const AName: string);
var
  ls : string;
begin
  ls :=
    CloseCurrentXML +
    '<' + FXMLTags.Table + ' ' +
    FXMLTags.TableName + '="' + AName + '"' + '>' +
    tiXMLTag(FXMLTags.Fields);
  FPreSizedStream.Write(ls);
  SetState(xwsFields);
end;

constructor TtiDataBufferToXMLWriter.Create;
begin
  Create(cStreamStartSize, cStreamGrowBy);
end;

destructor TtiDataBufferToXMLWriter.Destroy;
begin
  FXMLTags.Free;
  FPreSizedStream.Free;
  inherited;
end;

function TtiDataBufferToXMLWriter.GetAsString: string;
var
  lResult : string;
begin
  lResult :=
    FXMLTags.DatabaseHeader +
    TableData +
    FXMLTags.DatabaseFooter;
  if FCompress = cgsCompressNone then
    Result := lResult
  else
    Result := tiDecompressString(lResult, FCompress);
end;

procedure TtiDataBufferToXMLWriter.SetState(pState: TtiXMLWriterState);
begin
  FState := pState;
end;

function TtiDataBufferToXMLWriter.GetTableData: string;
begin
  Result := FPreSizedStream.AsString + CloseCurrentXML;
end;

function TtiDataBufferToXMLWriter.AssignFromTIQuery(const ATableName : string; const AQuery: TtiQuery): Integer;
begin
  Assert(ATableName <> '', 'ATableName not assigned');
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  InsertQueryMetaData(ATableName, AQuery);
  Result := InsertQueryData(AQuery);
end;

procedure TtiDataBufferToXMLWriter.InsertQueryMetaData(const ATableName : string;const AQuery: TtiQuery);
var
  i : integer;
  lFieldName : string;
  lFieldKind : TtiQueryFieldKind;
  lFieldSize : integer;
begin
  Assert(ATableName <> '', 'ATableName not assigned');
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  AddTable(LowerCase(ATableName));
  for i := 0 to AQuery.FieldCount - 1 do
  begin
    lFieldName := AQuery.FieldName(i);
    lFieldKind := AQuery.FieldKind(i);
    lFieldSize := AQuery.FieldSize(i);
    AddField(lFieldName, lFieldKind, lFieldSize);
  end;
end;

function TtiDataBufferToXMLWriter.InsertQueryData(const AQuery: TtiQuery): integer;
  procedure _QueryNamesToStringList(const AQuery : TtiQuery; const pNames : TStringList);
  var
    i : Integer;
  begin
    case FXMLFieldNameStyle of
    xfnsString : begin
                   for i := 0 to AQuery.FieldCount - 1 do
                     pNames.Add(LowerCase(AQuery.FieldName(i)));
                 end;
    xfnsInteger: begin
                   for i := 0 to AQuery.FieldCount - 1 do
                     pNames.Add(tiEncodeWordBase26(i));
                 end;
    else
      raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
    end;
  end;

var
  i : integer;
  lNameQuery : string;
  lStream : TMemoryStream;
  lNames : TStringList;
begin
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  Result := 0;
  lNames := TStringList.Create;
  try
    _QueryNamesToStringList(AQuery, lNames);
    while not AQuery.EOF do
    begin
      AddRow;
      for i := 0 to AQuery.FieldCount - 1 do
      begin
        lNameQuery := AQuery.FieldName(i);
        if not AQuery.FieldIsNullByIndex[i] then
        begin
          case AQuery.FieldKind(i) of
          qfkString,
          qfkLongString : AddCellAsString( lNames.Strings[i],AQuery.FieldAsString[  lNameQuery]);
          qfkInteger   : AddCellAsInteger(lNames.Strings[i],AQuery.FieldAsInteger[ lNameQuery]);
          qfkFloat     : AddCellAsFloat(  lNames.Strings[i],AQuery.FieldAsFloat[   lNameQuery]);
          qfkDateTime  : AddCellAsDateTime(lNames.Strings[i],AQuery.FieldAsDateTime[lNameQuery]);
          qfkLogical   : AddCellAsBoolean(lNames.Strings[i],AQuery.FieldAsBoolean[ lNameQuery]);
          qfkBinary    : begin
                            lStream := TMemoryStream.Create;
                            try
                              AQuery.AssignFieldAsStream(lNameQuery,lStream);
                              AddCellAsStream(lNames.Strings[i],lStream);
                            finally
                              lStream.Free;
                            end;
                          end;
          else
            raise EtiOPFProgrammerException.Create(cErrorInvalidTtiQueryFieldKind);
          end;
        end else
        begin
          AddCellAsString(lNames.Strings[i],'');
        end;
      end;
      Inc(Result);
      AQuery.Next;
    end;
  finally
    lNames.Free;
  end;
end;

procedure TtiDataBufferToXMLWriter.AssignFromTIDataSets(const pDataSets: TtiDataBuffers);
var
  i : integer;
begin
  Assert(pDataSets.TestValid(TtiDataBuffers), CTIErrorInvalidObject);
  for i := 0 to pDataSets.Count - 1 do
    AssignFromTIDataSet(pDataSets.Items[i]);
end;

procedure TtiDataBufferToXMLWriter.InsertDataSetData(const pDataSet: TtiDataBuffer);
  procedure _DataSetNamesToStringList(const pDataSet : TtiDataBuffer; const pNames : TStringList);
  var
    i : Integer;
  begin
    case FXMLFieldNameStyle of
    xfnsString : begin
                   for i := 0 to pDataSet.Fields.Count - 1 do
                     pNames.Add(LowerCase(pDataSet.Fields.Items[i].Name));
                 end;
    xfnsInteger: begin
                   for i := 0 to pDataSet.Fields.Count - 1 do
                     pNames.Add(tiEncodeWordBase26(i));
                 end;
    else
      raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
    end;
  end;
var
  i, j : integer;
  lKind : TtiQueryFieldKind;
  lNames : TStringList;
  lStream : TMemoryStream;
begin
  Assert(pDataSet.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  lNames := TStringList.Create;
  try
    _DataSetNamesToStringList(pDataSet, lNames);
    for i := 0 to pDataSet.Count - 1 do
    begin
      AddRow;
      for j := 0 to pDataSet.Fields.Count - 1 do
      begin
        lKind := pDataSet.Fields.Items[j].Kind;
        if j <> 0 then FRow := FRow + ' ';
        case lKind of
          qfkString,
          qfkLongString : AddCellAsString( lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsString);
          qfkInteger   : AddCellAsInteger(lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsInteger);
          qfkFloat     : AddCellAsFloat(  lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsFloat);
          qfkDateTime  : AddCellAsDateTime(lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsDateTime);
          qfkLogical   : AddCellAsBoolean(lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsBool);
          qfkBinary    : begin
                            lStream := TMemoryStream.Create;
                            try
                              pDataSet.Items[i].Items[j].AssignToStream(lStream);
                              AddCellAsStream(lNames.Strings[j],lStream);
                            finally
                              lStream.Free;
                            end;
                          end;
        else
          raise EtiOPFProgrammerException.Create(cErrorInvalidTtiQueryFieldKind);
        end;
      end;
    end;
  finally
    lNames.Free;
  end;
end;

procedure TtiDataBufferToXMLWriter.InsertDataSetMetaData(const pDataSet: TtiDataBuffer);
var
  i : integer;
  lFieldName : string;
  lFieldKind : TtiQueryFieldKind;
  lFieldSize : integer;
begin
  Assert(pDataSet.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  AddTable(LowerCase(pDataSet.Name));
  for i := 0 to pDataSet.Fields.Count - 1 do
  begin
    lFieldName := pDataSet.Fields.Items[i].Name;
    lFieldKind := pDataSet.Fields.Items[i].Kind;
    lFieldSize := pDataSet.Fields.Items[i].Width;
    AddField(lFieldName, lFieldKind, lFieldSize);
  end;
end;

function TtiDataBufferToXMLWriter.GetOptXMLDBSize: TtiOptXMLDBSize;
begin
  Result := FXMLTags.OptXMLDBSize;
end;

procedure TtiDataBufferToXMLWriter.SetOptXMLDBSize(const AValue: TtiOptXMLDBSize);
begin
  FXMLTags.OptXMLDBSize := AValue;
end;

constructor TtiDataBufferToXMLWriter.Create(AInitialSize, AGrowBy: Int64);
begin
  inherited Create;
  FXMLTags := TtiXMLTags.Create;
  FXMLFieldNameStyle := xfnsString;
  FCompress:= cgsCompressNone;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator;
  FPreSizedStream := TtiPreSizedStream.Create(AInitialSize, AGrowBy);
  FState := xwsEmpty;
  FRow := '';
  FCellIndex:= 0;
end;

constructor TXMLToDataSetReader.Create;
begin
  inherited;
  FXMLFieldNameStyle := xfnsString;
  FCompress := cgsCompressNone;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator;
  FXMLTags := TtiXMLTags.Create;
end;

destructor TXMLToDataSetReader.Destroy;
begin
  FXMLTags.Free;
  inherited;
end;

function TXMLToDataSetReader.GetOptXMLDBSize: TtiOptXMLDBSize;
begin
  Result := FXMLTags.OptXMLDBSize;
end;

procedure TXMLToDataSetReader.SetOptXMLDBSize(const AValue: TtiOptXMLDBSize);
begin
  FXMLTags.OptXMLDBSize := AValue;
end;

procedure TtiDataBufferToXMLWriter.AssignMetaDataFromTIQuery(const ATableName: string; const AQuery: TtiQuery);
begin
  Assert(ATableName <> '', 'ATableName not assigned');
  Assert(AQuery.TestValid(TtiQuery), CTIErrorInvalidObject);
  InsertQueryMetaData(ATableName, AQuery);
end;

procedure TtiDataBufferToXMLWriter.AssignFromTIDataSet(const pDataSet: TtiDataBuffer);
begin
  Assert(pDataSet.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  InsertDataSetMetaData(pDataSet);
  InsertDataSetData(pDataSet);
end;

end.
