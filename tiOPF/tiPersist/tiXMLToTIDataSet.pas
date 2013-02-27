unit tiXMLToTIDataSet;

{$I tiDefines.inc}

interface
uses
   tiObjAbs
  ,Classes
  ,tiDataSet_BOM
  ,tiQuery
  ,tiStreams
  ,tiXML
  ,Inifiles
  ;

const
  cErrorInvalidXMLWriterState       = 'Invalid tiXMLWriter.State';
  cErrorInvalidXMLDBParserState     = 'Invalid TtiXMLDBParserState';
  cErrorInvalidXMLFieldNameStyle    = 'Invalid TtiXMLFieldNameStyle';
  cErrorInvalidXMLFieldNameStyleStr = 'Invalid TtiXMLFieldNameStyle <%s>';
  cErrorParsingXMLAtPos             = 'Error parsing XML at position %d';
  
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

  TtiPreSizedStream = class( TtiObjAbs )
  private
    FStream : TMemoryStream ;
    FStreamSize : Int64;
    FGrowBy     : Int64;
    FDataSize   : Int64;
  public
    constructor Create(pInitialSize, pGrowBy : Int64);
    destructor  Destroy; override;
    procedure   Write(const pStr: string);
    function    AsString: string ;
  end;

  TtiXMLDBParserState = ( xdbsRoot, xdbsTables, xdbsTable, xdbsFields, xdbsField,
                          xdbsRows, xdbsRow, xdbsEnd ) ;

  TXMLToDataSetReader = class( TtiObjAbs )
  private
    FXML: string;
    FState: TtiXMLDBParserState;
    FPos: Cardinal ;
    FLen: Cardinal ;
    FDataSets : TtiDataSets;
    FDataSet: TtiDataSet;
    FDataSetRow: TtiDataSetRow;
    FXMLRCTrans: IXMLReservedCharsTranslator;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FXMLTags : TtiXMLTags ;
    procedure   SetXML(const Value: string);
    function    ParseForSingleAttribute(const pAttrName: string): string;
    procedure   ParseForAttributesUntil(const pUntil: string);
    function    MatchToken(const pToken: string; pLength: byte; pNewState: TtiXMLDBParserState ): boolean;
    function    GetOptXMLDBSize: TtiOptXMLDBSize;
    procedure   SetOptXMLDBSize(const Value: TtiOptXMLDBSize);
  protected
    property    XML : string read FXML write SetXML ;
    property    State : TtiXMLDBParserState read FState ;
    property    DataSets: TtiDataSets read FDataSets write FDataSets ;
    property    Pos : Cardinal read FPos;
    property    Len : Cardinal read FLen;

    procedure   DoAddTable(const pTableName : string ); virtual ;
    procedure   DoAddField(const pFieldName, pFieldKind, pFieldSize : string ); virtual ;
    procedure   DoAddRow ; virtual ;
    procedure   DoAddCell( const pFieldName : string ; pFieldValue : string ) ; virtual ;
    procedure   Next; virtual ;
  public
    constructor Create;
    destructor  Destory;
    procedure   Execute(const pXML: string; const pDataSets: TtiDataSets);
    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle ;
    property    OptXMLDBSize: TtiOptXMLDBSize read GetOptXMLDBSize Write SetOptXMLDBSize;
  end ;

  TtiXMLWriterState = ( xwsEmpty, xwsFields, xwsRows, xwsRow ) ;

  // Provides Write only access to a tiOPF XML file
  TtiDataSetToXMLWriter = class( TtiObjAbs )
  private
    FState : TtiXMLWriterState ;
    FRow : string ;
    FCellIndex: Integer;
    FPreSizedStream : TtiPreSizedStream ;
    FXMLRCTrans: IXMLReservedCharsTranslator;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FXMLTags : TtiXMLTags ;
    procedure   DoAddCellAsString(const pName, pValue: string);
    function    CloseCurrentXML: string;
    procedure   InsertQueryMetaData(const pTableName: string ; const pQuery: TtiQuery);
    function    InsertQueryData(const pQuery: TtiQuery):Integer;
    procedure   InsertDataSetMetaData(const pDataSet: TtiDataSet);
    procedure   InsertDataSetData(const pDataSet: TtiDataSet);
    function    GetOptXMLDBSize: TtiOptXMLDBSize;
    procedure   SetOptXMLDBSize(const Value: TtiOptXMLDBSize);
  protected
    function    GetAsString: string;
    function    GetTableData: string;
    procedure   SetState(pState: TtiXMLWriterState);
  public
    constructor Create ;
    destructor  Destroy ; override ;

    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle ;
    property    OptXMLDBSize      : TtiOptXMLDBSize      read GetOptXMLDBSize      Write SetOptXMLDBSize ;
    property    XMLTags           : TtiXMLTags read FXMLTags ;

    procedure   AddTable( const pName : string ) ;
    procedure   AddField( const pFieldName : string; pFieldKind : TtiQueryFieldKind ; pFieldWidth : Integer = 0 ) ;
    procedure   AddRow ;
    procedure   AddCellAsString(  const pName : string; const pValue : string) ;
    procedure   AddCellAsInteger( const pName : string; const pValue : Int64) ;
    procedure   AddCellAsFloat(   const pName : string; const pValue : Real) ;
    procedure   AddCellAsDateTime(const pName : string; const pValue : TDateTime) ;
    procedure   AddCellAsBoolean( const pName : string; const pValue : Boolean) ;
    procedure   AddCellAsStream(  const pName : string; const pValue : TStream) ;
    property    AsString: string read GetAsString;
    property    TableData: string read GetTableData;
    // Returns the number of rows added
    function    AssignFromTIQuery(const pTableName : string ;const pQuery : TtiQuery ): Integer;
    procedure   AssignFromTIDataSets(const pDataSets: TtiDataSets);

  end ;

  // Provides Read/Write access to a tiOPF XML file
  TtiXMLToDataSetReadWriter = class( TtiObjAbs )
  private
    FDataSets: TtiDataSets;
    FDataSet : TtiDataSet;
    FMetaDataField : TtiDBMetaDataField ;
    FDataSetRow : TtiDataSetRow ;
    FXMLFieldNameStyle: TtiXMLFieldNameStyle;
    FOptXMLDBSize: TtiOptXMLDBSize;

  protected
    procedure  Clear ;
    procedure  SetAsString(const Value: string);
    function   GetAsString: string;

  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    XMLFieldNameStyle : TtiXMLFieldNameStyle read FXMLFieldNameStyle Write FXMLFieldNameStyle ;

    property    DataSets : TtiDataSets read FDataSets write FDataSets ;
    procedure   LoadFromFile( const pFileName : string; pReadOnly : Boolean = false ) ;
    procedure   SaveToFile( const pFileName : string ) ;
    property    AsString : string read GetAsString write SetAsString;
    property    OptXMLDBSize: TtiOptXMLDBSize read FOptXMLDBSize Write FOptXMLDBSize ;

  end;

// Some helper methods
procedure tiXMLStringToTIDataSets( const pXMLString : string ; const pDataSets : TtiDataSets ) ;
function  tiTIDataSetsToXMLString( const pDataSets : TtiDataSets ) : string ;
function  tiTIDataSetToXMLString(  const pDataSet : TtiDataSet ) : string ;

// ToDo: Move these to implementation when optimisation is done
const
  cWriteRecordCount = 500 ;
//  cTableStart  = '<' + cgXMLTagTable + ' '   ; cLenTableStart  = Length(cTableStart);
//  cTableEnd    = '</' + cgXMLTagTable + '>'  ; cLenTableEnd    = Length(cTableEnd);
//  cFieldsStart = '<' + cgXMLTagFields + '>'  ; cLenFieldsStart = Length(cFieldsStart);
//  cFieldsEnd   = '</' + cgXMLTagFields + '>' ; cLenFieldsEnd   = Length(cFieldsEnd);
//  cFieldStart  = '<' + cgXMLTagField + ' '   ; cLenFieldStart  = Length(cFieldStart);
//  cFieldEnd    = '/>'                        ; cLenFieldEnd    = Length(cFieldEnd);
//  cRowsStart   = '<' + cgXMLTagRows + '>'    ; cLenRowsStart   = Length(cRowsStart);
//  cRowsEnd     = '</' + cgXMLTagRows + '>'   ; cLenRowsEnd     = Length(cRowsEnd);
//  cRowEnd      = '/>'                        ; cLenRowEnd      = Length(cRowEnd);
  cStartBuffer = 2000000;
  cIncBuffer   =  500000;

implementation
uses
  tiUtils
  ,tiDialogs // For debugging
  ,tiDataSet_Cli // For debugging
  ,SysUtils
  ,cTIPersist
  ,tiLog
  ,Windows // Debugging
  ,tiExcept
  ,tiRJMime
  ;

procedure tiXMLStringToTIDataSets( const pXMLString : string ; const pDataSets : TtiDataSets ) ;
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

function tiTIDataSetsToXMLString( const pDataSets : TtiDataSets ) : string ;
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

function  tiTIDataSetToXMLString(  const pDataSet : TtiDataSet ) : string ;
var
  lDataSets: TtiDataSets;
begin
  lDataSets:= TtiDataSets.Create;
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
//  lPreSizedStream : TtiPreSizedStream;
  lXMLWriter : TtiDataSetToXMLWriter;
begin
  Assert( DataSets.TestValid(TtiDataSets, true), cTIInvalidObjectError ) ;
  lXMLWriter := TtiDataSetToXMLWriter.Create;
  try
    lXMLWriter.XMLFieldNameStyle := FXMLFieldNameStyle ;
    lXMLWriter.OptXMLDBSize := FOptXMLDBSize ;
    // This makes it possible to write out an empty file
    if DataSets <> nil then
      lXMLWriter.AssignFromTIDataSets(DataSets);
    Result := lXMLWriter.AsString;
  finally
    lXMLWriter.Free;
  end;

{
  lPreSizedStream := TtiPreSizedStream.Create(cStartBuffer, cIncBuffer);
  try
    lXMLWriter := TtiDataSetToXMLWriter.Create;
    try
      // This makes it possible to write out an empty file
      if DataSets <> nil then
        lXMLWriter.AssignFromTIDataSets(DataSets);
      lPreSizedStream.Write(lXMLWriter.AsString);
    finally
      lXMLWriter.Free;
    end;
    result := lPreSizedStream.AsString;
  finally
    lPreSizedStream.Free;
  end;
}
end;

procedure TtiXMLToDataSetReadWriter.SetAsString(const Value: string);
var
  lXMLToDataSetReader: TXMLToDataSetReader ;
begin
  Assert( FDataSets.TestValid(TtiDataSets), cTIInvalidObjectError );
  Clear ;
  lXMLToDataSetReader:= TXMLToDataSetReader.Create ;
  try
    lXMLToDataSetReader.OptXMLDBSize := FOptXMLDBSize ;
    lXMLToDataSetReader.XMLFieldNameStyle := FXMLFieldNameStyle ;
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

procedure TXMLToDataSetReader.Execute(const pXML: string; const pDataSets: TtiDataSets);
begin
  Assert( pDataSets.TestValid(TtiDataSets), cTIInvalidObjectError);
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
      raise exception.create(cErrorInvalidXMLDBParserState);
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
      tiFmtException( 'Invalid TXMLAttributeParseState', ClassName, 'ParseForAttributes' ) ;
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
                        tiFmtException( 'Invalid TXMLAttributeParseState', ClassName, 'ParseForAttributes' ) ;
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
      tiFmtException( 'Invalid TXMLAttributeParseState', ClassName, 'ParseForAttributes' ) ;
    end;
    Inc(FPos);
  end ;
end;

procedure TXMLToDataSetReader.SetXML(const Value: string);
begin
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
  Assert( FDataSet.TestValid(TtiDataSet), cTIInvalidObjectError);
  lFieldKind := StrToQueryFieldKind(pFieldKind);
  lFieldWidth := StrToInt(pFieldSize);
  FDataSet.Fields.AddInstance(pFieldName, lFieldKind, lFieldWidth );
end;

procedure TXMLToDataSetReader.DoAddRow;
begin
  Assert( FDataSet.TestValid(TtiDataSet), cTIInvalidObjectError);
  FDataSetRow := FDataSet.AddInstance;
end;

procedure TXMLToDataSetReader.DoAddTable(const pTableName: string);
begin
  Assert( FDataSets.TestValid(TtiDataSets), cTIInvalidObjectError);
  FDataSet := FDataSets.AddInstance(pTableName);
end;

procedure TXMLToDataSetReader.DoAddCell(const pFieldName: string; pFieldValue: string);
var
  lCell: TtiDataSetCell;
  lIndex: Integer ;
begin
  Assert( FDataSetRow.TestValid(TtiDataSetRow), cTIInvalidObjectError);
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

{ TtiDataSetToXMLWriter }

procedure TtiDataSetToXMLWriter.AddCellAsBoolean(const pName: string;const pValue: Boolean);
begin
  DoAddCellAsString(pName, tiBooleanToStr(pValue));
end;

procedure TtiDataSetToXMLWriter.AddCellAsDateTime(const pName: string; const pValue: TDateTime);
begin
  DoAddCellAsString(pName, tiDateTimeAsXMLString(pValue));
end;

procedure TtiDataSetToXMLWriter.AddCellAsFloat(const pName: string;const pValue: Real);
begin
  DoAddCellAsString(pName, FloatToStr(pValue));
end;

procedure TtiDataSetToXMLWriter.AddCellAsInteger(const pName: string;const pValue: Int64);
begin
  DoAddCellAsString(pName, IntToStr(pValue));
end;

procedure TtiDataSetToXMLWriter.AddCellAsStream(const pName: string; const pValue: TStream);
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

procedure TtiDataSetToXMLWriter.AddCellAsString(const pName, pValue: string);
var
  ls : string ;
begin
  ls := FXMLRCTrans.RemoveReserved( rcXML, pValue ) ;
  DoAddCellAsString(pName, ls);
end ;                                

procedure TtiDataSetToXMLWriter.DoAddCellAsString(const pName, pValue: string);
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

procedure TtiDataSetToXMLWriter.AddField(const pFieldName: string;
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

procedure TtiDataSetToXMLWriter.AddRow;
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

function TtiDataSetToXMLWriter.CloseCurrentXML:string;
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

procedure TtiDataSetToXMLWriter.AddTable(const pName: string);
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

constructor TtiDataSetToXMLWriter.Create;
begin
  inherited ;
  FXMLTags := TtiXMLTags.Create ;
  FXMLFieldNameStyle := xfnsString;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator;
  FPreSizedStream := TtiPreSizedStream.Create(cStartBuffer, cIncBuffer);
  FState := xwsEmpty ;
  FRow := '';
  FCellIndex:= 0 ;
end;

destructor TtiDataSetToXMLWriter.Destroy;
begin
  FXMLTags.Free;
  FPreSizedStream.Free;
  inherited;
end;

function TtiDataSetToXMLWriter.GetAsString: string;
begin
  Result :=
    FXMLTags.DatabaseHeader +
    TableData +
    FXMLTags.DatabaseFooter;
end;

procedure TtiDataSetToXMLWriter.SetState(pState: TtiXMLWriterState);
begin
  FState := pState;
end;

function TtiDataSetToXMLWriter.GetTableData: string;
begin
  Result := FPreSizedStream.AsString + CloseCurrentXML;
end;

function TtiDataSetToXMLWriter.AssignFromTIQuery(const pTableName : string ; const pQuery: TtiQuery): Integer;
begin
  Assert( pTableName <> '', 'pTableName not assigned');
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  InsertQueryMetaData(pTableName, pQuery);
  Result := InsertQueryData(pQuery);
end;

procedure TtiDataSetToXMLWriter.InsertQueryMetaData(const pTableName : string ;const pQuery: TtiQuery);
var
  i : integer ;
  lFieldName : string ;
  lFieldKind : TtiQueryFieldKind ;
  lFieldSize : integer ;
begin
  Assert( pTableName <> '', 'pTableName not assigned');
  Assert( pQuery.TestValid(TtiQuery), cTIInvalidObjectError );
  try
    AddTable(LowerCase(pTableName));
    for i := 0 to pQuery.FieldCount - 1 do
    begin
      lFieldName := pQuery.FieldName(i) ;
      lFieldKind := pQuery.FieldKind(i) ;
      lFieldSize := pQuery.FieldSize(i);
      AddField(lFieldName, lFieldKind, lFieldSize);
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'InsertQueryMetaData' ) ;
  end ;
end;

function TtiDataSetToXMLWriter.InsertQueryData(const pQuery: TtiQuery): integer;
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
  try
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
              tiFmtException( 'Invalid QueryFieldKind', ClassName, 'InsertResponseData' ) ;
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
  except
    on e:exception do
      tiFmtException( e, ClassName, 'InsertQueryData' ) ;
  end ;
end;

procedure TtiDataSetToXMLWriter.AssignFromTIDataSets(const pDataSets: TtiDataSets);
var
  i : integer ;
begin
  Assert( pDataSets.TestValid(TtiDataSets), cTIInvalidObjectError);
  for i := 0 to pDataSets.Count - 1 do
  begin
    InsertDataSetMetaData(pDataSets.Items[i]);
    InsertDataSetData(pDataSets.Items[i]);
  end;
end;

procedure TtiDataSetToXMLWriter.InsertDataSetData(const pDataSet: TtiDataSet);
  procedure _DataSetNamesToStringList(const pDataSet : TtiDataSet; const pNames : TStringList);
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
begin
  Assert( pDataSet.TestValid(TtiDataSet), cTIInvalidObjectError );
  try
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
          qfkLongString : FRow := FRow + lNames.Strings[j] + '="' +
                                  FXMLRCTrans.RemoveReserved( rcXML,
                                    pDataSet.Items[i].Items[j].ValueAsString ) + '"';
          qfkInteger,
          qfkFloat,
          qfkDateTime,
          qfkLogical,
          qfkBinary     : FRow := FRow + lNames.Strings[j] + '="' +
                                  pDataSet.Items[i].Items[j].ValueAsString + '"';
          else
            tiFmtException( 'Invalid QueryFieldKind', ClassName, 'InsertDataSetData' ) ;
          end ;
        end ;
      end ;
    finally
      lNames.Free;
    end;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'InsertDataSetData' ) ;
  end ;
end;

procedure TtiDataSetToXMLWriter.InsertDataSetMetaData(const pDataSet: TtiDataSet);
var
  i : integer ;
  lFieldName : string ;
  lFieldKind : TtiQueryFieldKind ;
  lFieldSize : integer ;
begin
  Assert( pDataSet.TestValid(TtiDataSet), cTIInvalidObjectError );
  try
    AddTable(LowerCase(pDataSet.Name));
    for i := 0 to pDataSet.Fields.Count - 1 do
    begin
      lFieldName := pDataSet.Fields.Items[i].Name ;
      lFieldKind := pDataSet.Fields.Items[i].Kind ;
      lFieldSize := pDataSet.Fields.Items[i].Width;
      AddField(lFieldName, lFieldKind, lFieldSize);
    end ;
  except
    on e:exception do
      tiFmtException( e, ClassName, 'InsertQueryMetaData' ) ;
  end ;
end;

function TtiDataSetToXMLWriter.GetOptXMLDBSize: TtiOptXMLDBSize;
begin
  Result := FXMLTags.OptXMLDBSize;
end;

procedure TtiDataSetToXMLWriter.SetOptXMLDBSize(const Value: TtiOptXMLDBSize);
begin
  FXMLTags.OptXMLDBSize := Value ;
end;

{ TtiPreSizedStream }

function TtiPreSizedStream.AsString: string;
begin
  FStream.Position := 0 ;
  SetLength(Result,  FDataSize);
  FStream.Read( Result[1], FDataSize ) ;
end;

constructor TtiPreSizedStream.Create(pInitialSize, pGrowBy: Int64);
begin
  FStream := TMemoryStream.Create;
  FStreamSize := pInitialSize ;
  FGrowBy := pGrowBy;
  FStream.Size := FStreamSize ;
end;

destructor TtiPreSizedStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TtiPreSizedStream.Write(const pStr: string);
var
  lPC : PChar ;
  lLen : Integer ;
begin
  lPC := PChar( pStr ) ;
  lLen := length( pStr ) ;
  while FStreamSize < FDataSize + lLen do
  begin
    Inc( FStreamSize, cIncBuffer );
    FStream.Size := FStreamSize;
  end ;
  FStream.WriteBuffer( lPC^, lLen ) ;
  Inc(FDataSize, lLen);
end;

constructor TXMLToDataSetReader.Create;
begin
  inherited;
  FXMLFieldNameStyle := xfnsString;
  FXMLRCTrans:= CreateXMLReservedCharsTranslator;
  FXMLTags := TtiXMLTags.Create ;
end;

destructor TXMLToDataSetReader.Destory;
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

end.



