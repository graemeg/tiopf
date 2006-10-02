unit tiXMLToTIDataSet;

{$I tiDefines.inc}

interface
uses
  // tiOPF
   tiBaseObject
  ,tiDataBuffer_BOM
  ,tiQuery
  ,tiStreams
  ,tiXML
  // Delphi
  ,Classes
  ,Inifiles
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

  TtiXMLFieldNameStyle = ( xfnsInteger, xfnsString ) ;
const

  cXMLFieldNameStyles : array[TtiXMLFieldNameStyle] of string =
    ( 'integer', 'string' ) ;

  function tiStringToXMLFieldNameStyle(const pValue: string): TtiXMLFieldNameStyle;


type

  TtiXMLDBParserState = ( xdbsRoot, xdbsTables, xdbsTable, xdbsFields, xdbsField,
                          xdbsRows, xdbsRow, xdbsEnd ) ;

  TXMLToDataSetReader = class( TtiBaseObject )
  private
    FXML: string;
    FState: TtiXMLDBParserState;
    FPos: Cardinal ;
    FLen: Cardinal ;
    FDataSets : TtiDataBuffers;
    FDataSet: TtiDataBuffer;
    FDataSetRow: TtiDataBufferRow;
    FXMLRCTrans: IXMLReservedCharsTranslator;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FXMLTags : TtiXMLTags ;
    FCompress: string;
    procedure   SetXML(const Value: string);
    function    ParseForSingleAttribute(const pAttrName: string): string;
    procedure   ParseForAttributesUntil(const pUntil: string);
    function    MatchToken(const pToken: string; pLength: byte; pNewState: TtiXMLDBParserState ): boolean;
    function    GetOptXMLDBSize: TtiOptXMLDBSize;
    procedure   SetOptXMLDBSize(const Value: TtiOptXMLDBSize);
  protected
    property    XML : string read FXML write SetXML ;
    property    State : TtiXMLDBParserState read FState ;
    property    DataSets: TtiDataBuffers read FDataSets write FDataSets ;
    property    Pos : Cardinal read FPos;
    property    Len : Cardinal read FLen;

    procedure   DoAddTable(const pTableName : string ); virtual ;
    procedure   DoAddField(const pFieldName, pFieldKind, pFieldSize : string ); virtual ;
    procedure   DoAddRow ; virtual ;
    procedure   DoAddCell( const pFieldName : string ; pFieldValue : string ) ; virtual ;
    procedure   Next; virtual ;
  public
    constructor Create;
    destructor  Destroy; override ;
    procedure   Execute(const pXML: string; const pDataSets: TtiDataBuffers);
    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle ;
    property    OptXMLDBSize: TtiOptXMLDBSize read GetOptXMLDBSize Write SetOptXMLDBSize;
    property    Compress: string read FCompress Write FCompress ;
  end ;

  TtiXMLWriterState = ( xwsEmpty, xwsFields, xwsRows, xwsRow ) ;

  // Provides Write only access to a tiOPF XML file
  TtiDataBufferToXMLWriter = class( TtiBaseObject )
  private
    FState : TtiXMLWriterState ;
    FRow : string ;
    FCellIndex: Integer;
    FPreSizedStream : TtiPreSizedStream ;
    FXMLRCTrans: IXMLReservedCharsTranslator;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FXMLTags : TtiXMLTags ;
    FCompress: string;
    procedure   DoAddCellAsString(const pName, pValue: string);
    function    CloseCurrentXML: string;
    procedure   InsertQueryMetaData(const pTableName: string ; const pQuery: TtiQuery);
    function    InsertQueryData(const pQuery: TtiQuery):Integer;
    procedure   InsertDataSetMetaData(const pDataSet: TtiDataBuffer);
    procedure   InsertDataSetData(const pDataSet: TtiDataBuffer);
    function    GetOptXMLDBSize: TtiOptXMLDBSize;
    procedure   SetOptXMLDBSize(const Value: TtiOptXMLDBSize);
  protected
    function    GetAsString: string;
    function    GetTableData: string;
    procedure   SetState(pState: TtiXMLWriterState);
  public
    constructor Create ; overload ;
    constructor Create(pInitialSize, pGrowBy : Int64) ; overload ;
    destructor  Destroy ; override ;

    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle ;
    property    OptXMLDBSize      : TtiOptXMLDBSize      read GetOptXMLDBSize      Write SetOptXMLDBSize ;
    property    XMLTags           : TtiXMLTags read FXMLTags ;
    property    Compress: string read FCompress Write FCompress ;

    procedure   AddTable( const pName : string ) ;
    procedure   AddField( const pFieldName : string; pFieldKind : TtiQueryFieldKind ; pFieldWidth : Integer = 0 ) ;
    procedure   AddRow ;
    procedure   AddCellAsString(  const pName : string; const pValue : string) ;
    procedure   AddCellAsInteger( const pName : string; const pValue : Int64) ;
    procedure   AddCellAsFloat(   const pName : string; const pValue : Extended) ;
    procedure   AddCellAsDateTime(const pName : string; const pValue : TDateTime) ;
    procedure   AddCellAsBoolean( const pName : string; const pValue : Boolean) ;
    procedure   AddCellAsStream(  const pName : string; const pValue : TStream) ;
    property    AsString: string read GetAsString;
    property    TableData: string read GetTableData;
    // Returns the number of rows added
    function    AssignFromTIQuery(const pTableName : string ;const pQuery : TtiQuery ): Integer;
    procedure   AssignMetaDataFromTIQuery(const pTableName : string ;const pQuery : TtiQuery );
    procedure   AssignFromTIDataSets(const pDataSets: TtiDataBuffers);
    procedure   AssignFromTIDataSet(const pDataSet: TtiDataBuffer);

  end ;

  // Provides Read/Write access to a tiOPF XML file
  TtiXMLToDataSetReadWriter = class( TtiBaseObject )
  private
    FDataSets: TtiDataBuffers;
    FDataSet : TtiDataBuffer;
    FMetaDataField : TtiDBMetaDataField ;
    FDataSetRow : TtiDataBufferRow ;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FOptXMLDBSize: TtiOptXMLDBSize;
    FCompress: string;
    procedure SetCompress(const Value: string);

  protected
    procedure  Clear ;
    procedure  SetAsString(const Value: string);
    function   GetAsString: string;

  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle ;

    property    DataSets : TtiDataBuffers read FDataSets write FDataSets ;
    procedure   LoadFromFile( const pFileName : string; pReadOnly : Boolean = false ) ;
    procedure   SaveToFile( const pFileName : string ) ;
    property    AsString : string read GetAsString write SetAsString;
    property    OptXMLDBSize: TtiOptXMLDBSize read FOptXMLDBSize Write FOptXMLDBSize ;
    property    Compress: string read FCompress Write SetCompress ;

  end;

// Some helper methods
procedure tiXMLStringToTIDataSets( const pXMLString : string ; const pDataSets : TtiDataBuffers ) ;
function  tiTIDataSetsToXMLString( const pDataSets : TtiDataBuffers ) : string ;
function  tiTIDataSetToXMLString(  const pDataSet : TtiDataBuffer ) : string ;

// ToDo: Move these to implementation when optimisation is done
const
  cWriteRecordCount = 500 ;

implementation
uses
  // tiOPF
   tiUtils
  ,tiDialogs // For debugging
  ,tiDataBuffer_Cli // For debugging
  ,tiConstants
  ,tiLog
  ,tiExcept
  ,tiCompress
  ,tiCompressZLib
  // Delphi
  ,SysUtils
  ,Windows // Debugging
  ;

procedure tiXMLStringToTIDataSets( const pXMLString : string ; const pDataSets : TtiDataBuffers ) ;
var
  lXMLToTIDataSet : TtiXMLToDataSetReadWriter ;
begin
  lXMLToTIDataSet := TtiXMLToDataSetReadWriter.Create ;
  try
    lXMLToTIDataSet.DataSets := pDataSets ;
    lXMLToTIDataSet.AsString := pXMLString;
  finally
    lXMLToTIDataSet.Free;
  end;
end;

function tiTIDataSetsToXMLString( const pDataSets : TtiDataBuffers ) : string ;
var
  lXMLToTIDataSet : TtiXMLToDataSetReadWriter ;
begin
  lXMLToTIDataSet := TtiXMLToDataSetReadWriter.Create ;
  try
    lXMLToTIDataSet.DataSets := pDataSets ;
    result := lXMLToTIDataSet.AsString;
  finally
    lXMLToTIDataSet.Free;
  end;
end;

function  tiTIDataSetToXMLString(  const pDataSet : TtiDataBuffer ) : string ;
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
end ;

function tiStringToXMLFieldNameStyle(const pValue: string): TtiXMLFieldNameStyle;
var
  i : TtiXMLFieldNameStyle;
begin
  for i := Low(TtiXMLFieldNameStyle) to High(TtiXMLFieldNameStyle) do
    if SameText(pValue, cXMLFieldNameStyles[i]) then
    begin
      Result := i ;
      Exit ; //==>
    end ;
  raise Exception.CreateFmt(cErrorInvalidXMLFieldNameStyleStr, [pValue]);
  //Result := Low(TtiXMLFieldNameStyle); // To shut the compiler up
end ;

{ TtiXMLToDataSetReadWriter }

constructor TtiXMLToDataSetReadWriter.Create;
begin
  inherited ;
  FXMLFieldNameStyle := xfnsString ;
  FCompress          := cgsCompressNone ;
end;

destructor TtiXMLToDataSetReadWriter.Destroy;
begin
  inherited;
end;

procedure TtiXMLToDataSetReadWriter.LoadFromFile(const pFileName: string; pReadOnly : Boolean = false);
var
  lStream : TFileStream;
  ls : string ;
begin
  Assert(pFileName <> '', 'File name not assigned');
  if not pReadOnly then
    lStream := TFileStream.Create(pFileName, fmOpenReadWrite or fmShareDenyNone)
  else
    lStream := TFileStream.Create(pFileName, fmOpenRead or fmShareDenyNone);
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
  Assert( DataSets.TestValid(TtiDataBuffers, true), cTIInvalidObjectError ) ;
  lXMLWriter := TtiDataBufferToXMLWriter.Create;
  try
    lXMLWriter.XMLFieldNameStyle := FXMLFieldNameStyle ;
    lXMLWriter.OptXMLDBSize := FOptXMLDBSize ;
    lXMLWriter.Compress := FCompress;
    // This makes it possible to write out an empty file
    if DataSets <> nil then
      lXMLWriter.AssignFromTIDataSets(DataSets);
    Result := lXMLWriter.AsString;
  finally
    lXMLWriter.Free;
  end;
end;

procedure TtiXMLToDataSetReadWriter.SetAsString(const Value: string);
var
  lXMLToDataSetReader: TXMLToDataSetReader ;
begin
  Assert( FDataSets.TestValid(TtiDataBuffers), cTIInvalidObjectError );
  Clear ;
  lXMLToDataSetReader:= TXMLToDataSetReader.Create ;
  try
    lXMLToDataSetReader.OptXMLDBSize := FOptXMLDBSize ;
    lXMLToDataSetReader.XMLFieldNameStyle := FXMLFieldNameStyle ;
    lXMLToDataSetReader.Compress := FCompress;
    lXMLToDataSetReader.Execute(Value, FDataSets);
  finally
    lXMLToDataSetReader.Free;
  end;
end;

procedure TtiXMLToDataSetReadWriter.SaveToFile(const pFileName: string);
var
  lStream : TFileStream;
  ls : string ;
begin
  Assert(pFileName <> '', 'File name not assigned');
  lStream := TFileStream.Create(pFileName, fmCreate or fmShareExclusive);
  try
    ls := AsString;
    tiStringToStream(ls, lStream);
  finally
    lStream.Free;
  end;
end;

procedure TtiXMLToDataSetReadWriter.Clear;
begin
  FDataSet       := nil ;
  FMetaDataField := nil ;
  FDataSetRow    := nil ;
  FDataSets.Clear ;
end;

procedure TXMLToDataSetReader.Execute(const pXML: string; const pDataSets: TtiDataBuffers);
begin
  Assert( pDataSets.TestValid(TtiDataBuffers), cTIInvalidObjectError);
  XML := pXML;
  DataSets := pDataSets ;
  while FState <> xdbsEnd do
    Next ;
end;

function TXMLToDataSetReader.MatchToken(const pToken : string;
                                        pLength : byte ;
                                        pNewState: TtiXMLDBParserState ): boolean;
var
  lStr : string;
begin
  lStr := Copy( FXML, FPos, pLength);
  if ( lStr = pToken )  then
  begin
    Inc(FPos, pLength);
    result := true;
    FState := pNewState;
  end else
    result := false ;
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
  while ( FPos <= FLen ) do
  begin
    case FState of
    xdbsRoot : if MatchToken(FXMLTags.TablesStart, FXMLTags.LenTablesStart, xdbsTables ) then
                 Break ; //==>
    xdbsTables :
      begin
        if MatchToken(FXMLTags.TableStart,FXMLTags.LenTableStart, xdbsTable) then
          Break ; //==>
        if MatchToken(FXMLTags.TableEnd,FXMLTags.LenTableEnd, xdbsTables) then
          Break ; //==>
        if MatchToken(FXMLTags.TablesEnd,FXMLTags.LenTablesEnd, xdbsEnd) then
          Break ; //==>
      end ;
    xdbsTable :
      begin
        lTableName := ParseForSingleAttribute(FXMLTags.TableName);
        DoAddTable(lTableName);
        FState := xdbsFields;
        Break ; //==>
      end ;
    xdbsFields :
      begin
        if MatchToken(FXMLTags.FieldStart,FXMLTags.LenFieldStart, xdbsField) then
          Break ; //==>
        if MatchToken(FXMLTags.FieldsEnd,FXMLTags.LenFieldsEnd, xdbsRows) then
          Break ; //==>
      end ;
    xdbsField :
      begin
        lFieldName := ParseForSingleAttribute(FXMLTags.FieldName);
        lFieldKind := ParseForSingleAttribute(FXMLTags.FieldKind);
        lFieldSize := ParseForSingleAttribute(FXMLTags.FieldSize);
        DoAddField(lFieldName, lFieldKind, lFieldSize);
        FState := xdbsFields;
        Break ; //==>
      end ;
    xdbsRows :
      begin
        if MatchToken(FXMLTags.RowsStartEnd,FXMLTags.LenRowsStartEnd, xdbsTables) then
          Break ; //==>
        if MatchToken(FXMLTags.RowStart,FXMLTags.LenRowStart, xdbsRow) then
        begin
          DoAddRow;
          Break ; //==>
        end;
        if MatchToken(FXMLTags.RowsEnd,FXMLTags.LenRowsEnd, xdbsTables) then
          Break ; //==>
      end ;
    xdbsRow:
      begin
        ParseForAttributesUntil('/>');
        FState := xdbsRows;
        Break ; //==>
      end ;
    else
      raise exception.Create(cErrorInvalidXMLDBParserState);
    end ;
    Inc(FPos);
  end ;
  if lPos = FPos then
    raise Exception.CreateFmt(cErrorParsingXMLAtPos, [lPos]);
end;

procedure TXMLToDataSetReader.ParseForAttributesUntil(const pUntil: string);
var
  lState     : TtiXMLAttributeParseState ;
  lStart     : Cardinal ;
  lChar      : Char ;
  lName      : string ;
  lValue     : string ;
  lStr : string ;
begin
  lState     := xapsWaitForAttr ;
  lStart     := 0 ; // To fix a compiler warning
  While ( FPos <= FLen ) do
  begin
    lStr := Copy(FXML, FPos, Length(pUntil));
    if ( lStr = pUntil ) then
    begin
      Inc(FPos,Length(pUntil));
      Exit ; //==>
    end ;
    lChar := FXML[FPos] ;
    case lState of
    xapsWaitForAttr : begin
                        if lChar <> #32 then
                        begin
                          lStart := FPos;
                          Inc(lState) ;
                        end;
                      end ;
    xapsInAttrName  : begin
                        if lChar in [#32, '='] then
                        begin
                          Inc(lState) ;
                          lName := Copy( FXML, lStart, FPos - lStart );
                        end ;
                      end ;
    xapsWaitForAttrValue : begin
                        if ( lChar = '"' ) then
                        begin
                          Inc(lState);
                          lStart := FPos ;
                        end;
                      end ;
    xapsInAttrValue : begin
                        if ( lChar = '"' ) then
                        begin
                          lState := Low(TtiXMLAttributeParseState) ;
                          lValue := Copy( FXML, lStart+1, FPos - lStart - 1 );
                          DoAddCell(lName, lValue);
                        end;
                      end ;
    else
      raise EtiOPFInternalException.Create(cErrorInvalidTXMLAttributeParseState);
    end;
      Inc(FPos);
  end ;
end;

function TXMLToDataSetReader.ParseForSingleAttribute(const pAttrName: string): string;
var
  lState     : TtiXMLAttributeParseState ;
  lStart     : Cardinal ;
  lChar      : Char ;
  lStr : string ;
begin
  lState     := xapsWaitForAttr ;
  lStart     := 0 ; // To fix a compiler warning

  while FPos <= FLen do
  begin
    case lState of
    xapsWaitForAttr : begin
                        lStr := Copy(FXML, FPos, Length(pAttrName));
                        if ( lStr = pAttrName ) then
                        begin
                          Inc(FPos,Length(pAttrName)-1);
                          lState := xapsWaitForAttrValue;
                        end;
                      end ;
    xapsInAttrName  : begin
                        raise EtiOPFInternalException.Create(cErrorInvalidTXMLAttributeParseState);
                      end ;
    xapsWaitForAttrValue : begin
                            lChar := FXML[FPos] ;
                            if ( lChar = '"' ) then
                            begin
                              lState := xapsInAttrValue;
                              lStart := FPos ;
                            end;
                          end ;
    xapsInAttrValue : begin
                        lChar := FXML[FPos] ;
                        if ( lChar = '"' ) then
                        begin
                          Result := Copy( FXML, lStart+1, FPos - lStart - 1 );
                          Inc(FPos);
                          Exit ; //==>
                        end;
                      end ;
    else
      raise EtiOPFInternalException.Create(cErrorInvalidTXMLAttributeParseState);
    end;
    Inc(FPos);
  end ;
end;

procedure TXMLToDataSetReader.SetXML(const Value: string);
begin
  if FCompress <> cgsCompressNone then
    FXML := tiDecompressString(Value, FCompress)
  else
    FXML := Value;
  FLen := Length(FXML);
  FPos := 1 ;
  FState := xdbsRoot ;
end;

procedure TXMLToDataSetReader.DoAddField(const pFieldName, pFieldKind, pFieldSize: string);
var
  lFieldKind : TtiQueryFieldKind;
  lFieldWidth : integer;
begin
  Assert( FDataSet.TestValid(TtiDataBuffer), cTIInvalidObjectError);
  lFieldKind := StrToQueryFieldKind(pFieldKind);
  lFieldWidth := StrToInt(pFieldSize);
  FDataSet.Fields.AddInstance(pFieldName, lFieldKind, lFieldWidth );
end;

procedure TXMLToDataSetReader.DoAddRow;
begin
  Assert( FDataSet.TestValid(TtiDataBuffer), cTIInvalidObjectError);
  FDataSetRow := FDataSet.AddInstance;
end;

procedure TXMLToDataSetReader.DoAddTable(const pTableName: string);
begin
  Assert( FDataSets.TestValid(TtiDataBuffers), cTIInvalidObjectError);
  FDataSet := FDataSets.AddInstance(pTableName);
end;

procedure TXMLToDataSetReader.DoAddCell(const pFieldName: string; pFieldValue: string);
var
  lCell: TtiDataBufferCell;
  lIndex: Integer ;
begin
  Assert( FDataSetRow.TestValid(TtiDataBufferRow), cTIInvalidObjectError);
  case FXMLFieldNameStyle of
    xfnsString : lCell := FDataSetRow.FindByFieldName(pFieldName);
    xfnsInteger: begin
                   lIndex := tiDecodeWordBase26(pFieldName) ;
                   lCell := FDataSetRow.Items[lIndex];
                 end ;
  else
    raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
  end ;

  //if FDataSet.Fields.FindByFieldName(pFieldName).Kind = qfkString then
  if lCell.DataSetField.Kind = qfkString then
    lCell.ValueAsString := FXMLRCTrans.InsertReserved( rcXML, pFieldValue )
  else
    lCell.ValueAsString := pFieldValue ;

end;

procedure TtiXMLToDataSetReadWriter.SetCompress(const Value: string);
begin
  FCompress := Value;
end;

{ TtiDataBufferToXMLWriter }

procedure TtiDataBufferToXMLWriter.AddCellAsBoolean(const pName: string;const pValue: Boolean);
begin
  DoAddCellAsString(pName, tiBooleanToStr(pValue));
end;

procedure TtiDataBufferToXMLWriter.AddCellAsDateTime(const pName: string; const pValue: TDateTime);
begin
  DoAddCellAsString(pName, tiDateTimeAsXMLString(pValue));
end;

procedure TtiDataBufferToXMLWriter.AddCellAsFloat(const pName: string;const pValue: Extended);
begin
  DoAddCellAsString(pName, FloatToStr(pValue));
end;

procedure TtiDataBufferToXMLWriter.AddCellAsInteger(const pName: string;const pValue: Int64);
begin
  DoAddCellAsString(pName, IntToStr(pValue));
end;

procedure TtiDataBufferToXMLWriter.AddCellAsStream(const pName: string; const pValue: TStream);
var
  ls : string;
  lStream : TStringStream ;
  lPos : Integer ;
begin
  lPos := pValue.Position;
  try
    lStream := TStringStream.Create('') ;
    try
      pValue.Position := 0 ;
      MimeEncodeStream(pValue, lStream);
      ls := lStream.DataString ;
    finally
      lStream.Free;
    end;
  finally
    pValue.Position := lPos;
  end ;
  DoAddCellAsString(pName, ls);
end;

procedure TtiDataBufferToXMLWriter.AddCellAsString(const pName, pValue: string);
var
  ls : string ;
begin
  ls := FXMLRCTrans.RemoveReserved( rcXML, pValue ) ;
  DoAddCellAsString(pName, ls);
end ;                                

procedure TtiDataBufferToXMLWriter.DoAddCellAsString(const pName, pValue: string);
begin
  Assert(FState = xwsRow, 'State <> xwsRow');
  if FRow <> '' then FRow := FRow + ' ' ;
  case FXMLFieldNameStyle of
    xfnsString : FRow := FRow + LowerCase(pName) + '="' + pValue + '"';
    xfnsInteger: begin
                   FRow := FRow + tiEncodeWordBase26(FCellIndex) + '="' + pValue + '"' ;
                   Inc(FCellIndex);
                 end ;
  else
    raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
  end ;
end;

procedure TtiDataBufferToXMLWriter.AddField(const pFieldName: string;
  pFieldKind: TtiQueryFieldKind; pFieldWidth: Integer);
var
  ls : string;
begin
  Assert(FState = xwsFields, 'State <> xwsFields');
  ls :=
    '<' + FXMLTags.Field + ' ' + FXMLTags.FieldName + '="' + pFieldName + '" '
                               + FXMLTags.FieldKind + '="' + LowerCase(cgaQueryFieldKind[pFieldKind]) + '" '
                               + FXMLTags.FieldSize + '="' + IntToStr( pFieldWidth ) + '"/>' ;
  FPreSizedStream.Write(ls);
end;

procedure TtiDataBufferToXMLWriter.AddRow;
var
  ls : string ;
begin
  case FState of
    xwsFields: ls := FXMLTags.FieldsEnd + FXMLTags.RowsStart;
    xwsRow:    ls := FRow + FXMLTags.RowEnd;
  else
    raise EtiOPFProgrammerException.Create( cErrorInvalidXMLWriterState );
  end;
  FPreSizedStream.Write(ls + FXMLTags.RowStart);
  FRow := '' ;
  FState := xwsRow;
  FCellIndex:= 0 ;
end;

function TtiDataBufferToXMLWriter.CloseCurrentXML:string;
begin
  case FState of
  xwsEmpty:  Result := '' ;
  xwsFields: Result := FXMLTags.FieldsEnd + FXMLTags.RowsStart + FXMLTags.RowsEnd + FXMLTags.TableEnd ;
  xwsRows:   Result := FXMLTags.RowsEnd + FXMLTags.TableEnd ;
  xwsRow:    Result := FRow + FXMLTags.RowEnd + FXMLTags.RowsEnd + FXMLTags.TableEnd ;
  else
    raise EtiOPFProgrammerException.Create( cErrorInvalidXMLWriterState );
  end ;
end;

procedure TtiDataBufferToXMLWriter.AddTable(const pName: string);
var
  ls : string ;
begin
  ls :=
    CloseCurrentXML +
    '<' + FXMLTags.Table + ' ' +
    FXMLTags.TableName + '="' + pName + '"' + '>' +
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
  lResult : string ;
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

function TtiDataBufferToXMLWriter.AssignFromTIQuery(const pTableName : string ; const pQuery: TtiQuery): Integer;
begin
  Assert( pTableName <> '', 'pTableName not assigned');
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  InsertQueryMetaData(pTableName, pQuery);
  Result := InsertQueryData(pQuery);
end;

procedure TtiDataBufferToXMLWriter.InsertQueryMetaData(const pTableName : string ;const pQuery: TtiQuery);
var
  i : integer ;
  lFieldName : string ;
  lFieldKind : TtiQueryFieldKind ;
  lFieldSize : integer ;
begin
  Assert( pTableName <> '', 'pTableName not assigned');
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  AddTable(LowerCase(pTableName));
  for i := 0 to pQuery.FieldCount - 1 do
  begin
    lFieldName := pQuery.FieldName(i) ;
    lFieldKind := pQuery.FieldKind(i) ;
    lFieldSize := pQuery.FieldSize(i);
    AddField(lFieldName, lFieldKind, lFieldSize);
  end ;
end;

function TtiDataBufferToXMLWriter.InsertQueryData(const pQuery: TtiQuery): integer;
  procedure _QueryNamesToStringList(const pQuery : TtiQuery; const pNames : TStringList);
  var
    i : Integer ;
  begin
    case FXMLFieldNameStyle of
    xfnsString : begin
                   for i := 0 to pQuery.FieldCount - 1 do
                     pNames.Add(LowerCase(pQuery.FieldName(i)));
                 end;
    xfnsInteger: begin
                   for i := 0 to pQuery.FieldCount - 1 do
                     pNames.Add(tiEncodeWordBase26(i));
                 end;
    else
      raise Exception.Create(cErrorInvalidXMLFieldNameStyle);
    end ;
  end ;

var
  i : integer ;
  lNameQuery : string ;
  lStream : TMemoryStream ;
  lNames : TStringList;
begin
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  Result := 0 ;
  lNames := TStringList.Create;
  try
    _QueryNamesToStringList(pQuery, lNames);
    while not pQuery.EOF do
    begin
      AddRow;
      for i := 0 to pQuery.FieldCount - 1 do
      begin
        lNameQuery := pQuery.FieldName(i);
        if not pQuery.FieldIsNullByIndex[i] then
        begin
          case pQuery.FieldKind(i) of
          qfkString,
          qfkLongString : AddCellAsString(  lNames.Strings[i],pQuery.FieldAsString[  lNameQuery]);
          qfkInteger    : AddCellAsInteger( lNames.Strings[i],pQuery.FieldAsInteger[ lNameQuery]);
          qfkFloat      : AddCellAsFloat(   lNames.Strings[i],pQuery.FieldAsFloat[   lNameQuery]);
          qfkDateTime   : AddCellAsDateTime(lNames.Strings[i],pQuery.FieldAsDateTime[lNameQuery]);
          qfkLogical    : AddCellAsBoolean( lNames.Strings[i],pQuery.FieldAsBoolean[ lNameQuery]);
          qfkBinary     : begin
                            lStream := TMemoryStream.Create;
                            try
                              pQuery.AssignFieldAsStream(lNameQuery,lStream);
                              AddCellAsStream(lNames.Strings[i],lStream);
                            finally
                              lStream.Free;
                            end;
                          end ;
          else
            raise EtiOPFProgrammerException.Create(cErrorInvalidTtiQueryFieldKind) ;
          end ;
        end else
        begin
          AddCellAsString(lNames.Strings[i],'') ;
        end ;
      end ;
      Inc(Result);
      pQuery.Next ;
    end ;
  finally
    lNames.Free;
  end;
end;

procedure TtiDataBufferToXMLWriter.AssignFromTIDataSets(const pDataSets: TtiDataBuffers);
var
  i : integer ;
begin
  Assert( pDataSets.TestValid(TtiDataBuffers), cTIInvalidObjectError);
  for i := 0 to pDataSets.Count - 1 do
    AssignFromTIDataSet(pDataSets.Items[i]);
end;

procedure TtiDataBufferToXMLWriter.InsertDataSetData(const pDataSet: TtiDataBuffer);
  procedure _DataSetNamesToStringList(const pDataSet : TtiDataBuffer; const pNames : TStringList);
  var
    i : Integer ;
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
    end ;
  end ;
var
  i, j : integer ;
  lKind : TtiQueryFieldKind ;
  lNames : TStringList ;
  lStream : TMemoryStream;
begin
  Assert( pDataSet.TestValid(TtiDataBuffer), cTIInvalidObjectError );
  lNames := TStringList.Create;
  try
    _DataSetNamesToStringList(pDataSet, lNames);
    for i := 0 to pDataSet.Count - 1 do
    begin
      AddRow;
      for j := 0 to pDataSet.Fields.Count - 1 do
      begin
        lKind := pDataSet.Fields.Items[j].Kind ;
        if j <> 0 then FRow := FRow + ' ' ;
        case lKind of
          qfkString,
          qfkLongString : AddCellAsString(  lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsString);
          qfkInteger    : AddCellAsInteger( lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsInteger);
          qfkFloat      : AddCellAsFloat(   lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsFloat);
          qfkDateTime   : AddCellAsDateTime(lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsDateTime);
          qfkLogical    : AddCellAsBoolean( lNames.Strings[j],pDataSet.Items[i].Items[j].ValueAsBool);
          qfkBinary     : begin
                            lStream := TMemoryStream.Create;
                            try
                              pDataSet.Items[i].Items[j].AssignToStream(lStream);
                              AddCellAsStream(lNames.Strings[j],lStream);
                            finally
                              lStream.Free;
                            end;
                          end ;
        else
          raise EtiOPFProgrammerException.Create(cErrorInvalidTtiQueryFieldKind);
        end ;
      end ;
    end ;
  finally
    lNames.Free;
  end;
end;

procedure TtiDataBufferToXMLWriter.InsertDataSetMetaData(const pDataSet: TtiDataBuffer);
var
  i : integer ;
  lFieldName : string ;
  lFieldKind : TtiQueryFieldKind ;
  lFieldSize : integer ;
begin
  Assert( pDataSet.TestValid(TtiDataBuffer), cTIInvalidObjectError );
  AddTable(LowerCase(pDataSet.Name));
  for i := 0 to pDataSet.Fields.Count - 1 do
  begin
    lFieldName := pDataSet.Fields.Items[i].Name ;
    lFieldKind := pDataSet.Fields.Items[i].Kind ;
    lFieldSize := pDataSet.Fields.Items[i].Width;
    AddField(lFieldName, lFieldKind, lFieldSize);
  end ;
end;

function TtiDataBufferToXMLWriter.GetOptXMLDBSize: TtiOptXMLDBSize;
begin
  Result := FXMLTags.OptXMLDBSize;
end;

procedure TtiDataBufferToXMLWriter.SetOptXMLDBSize(const Value: TtiOptXMLDBSize);
begin
  FXMLTags.OptXMLDBSize := Value ;
end;

constructor TtiDataBufferToXMLWriter.Create(pInitialSize, pGrowBy: Int64);
begin
  inherited Create;
  FXMLTags := TtiXMLTags.Create ;
  FXMLFieldNameStyle := xfnsString;
  FCompress:= cgsCompressNone;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator;
  FPreSizedStream := TtiPreSizedStream.Create(pInitialSize, pGrowBy);
  FState := xwsEmpty ;
  FRow := '';
  FCellIndex:= 0 ;
end;

constructor TXMLToDataSetReader.Create;
begin
  inherited;
  FXMLFieldNameStyle := xfnsString;
  FCompress := cgsCompressNone;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator;
  FXMLTags := TtiXMLTags.Create ;
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

procedure TXMLToDataSetReader.SetOptXMLDBSize(const Value: TtiOptXMLDBSize);
begin
  FXMLTags.OptXMLDBSize := Value ;
end;

procedure TtiDataBufferToXMLWriter.AssignMetaDataFromTIQuery(const pTableName: string; const pQuery: TtiQuery);
begin
  Assert( pTableName <> '', 'pTableName not assigned');
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  InsertQueryMetaData(pTableName, pQuery);
end;

procedure TtiDataBufferToXMLWriter.AssignFromTIDataSet(const pDataSet: TtiDataBuffer);
begin
  Assert( pDataSet.TestValid(TtiDataBuffer), cTIInvalidObjectError);
  InsertDataSetMetaData(pDataSet);
  InsertDataSetData(pDataSet);
end;

end.



