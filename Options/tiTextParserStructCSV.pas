unit tiTextParserStructCSV;

{$I tiDefines.inc}

interface
uses
  tiBaseObject
  ,tiObject
  ,Classes
  ,SysUtils
  ,Contnrs
  ,tiTextParser
  ;

const
  cErrorInvalidTextParserState = 'Invalid tiOPF text parser - state.';
  cErrorIGroupNotFound         = 'Data group in an "I" row not found in registered property setters' ;
  cErrorDGroupNotFound         = 'Data group in an "D" row not found in registered property setters' ;
  cErrorDuplicateGroupName     = 'Duplicate group name in an "I" row';
  cErrorDuplicateColumnName    = 'Duplicate column name in an "I" row';
  cErrorFirstCharNotIorD       = 'First character not an "I" or "D"' ;
  cErrorIColumnNotFound        = 'Column in an "I" row not found in registered property setters' ;
  cErrorMoreDataThanMetaData   = 'More columns of data than meta data';
  cErrorMoreMetaDataThanData   = 'More columns of meta data than data';
  cErrorInFieldSetter          = 'Error in field setter';
  cErrorInOnRowEvent           = 'Error in OnRowEvent';

type

  ETextParserStructCSVException = class( Exception )
  private
    FRow: Integer;
    FCol: Integer;
    FToken: string;
    FUnFormattedMessage: string;
  public
    constructor Create( const pMessage: string ;
                        pRow: Integer ; pCol: Integer ;
                        const pToken: string ) ;
    property Row: Integer read FRow Write FRow ;
    property Col: Integer read FCol Write FCol ;
    property Token: string read FToken Write FToken;
    property UnFormattedMessage: string read FUnFormattedMessage ;
  end ;

  ETextParserStructCSVOnRowException = class(ETextParserStructCSVException)
  private
    FDataGroupName: string;
    FEventMessage: string;
  public
    constructor Create( const pMessage: string ;
                        pRow: Integer ; pCol: Integer ;
                        const pDataGroup: string;
                        const pEventMessage: string ) ;
    property DataGroupName: string read FDataGroupName Write FDataGroupName;
    property EventMessage: string read FEventMessage Write FEventMessage;
  end ;

  ETextParserStructCSVDataGroupException = class(ETextParserStructCSVException)
  private
    FDataGroupName: string;
  public
    constructor Create( const pMessage: string ;
                        pRow: Integer ; pCol: Integer ;
                        const pDataGroup: string;
                        const pToken: string ) ;
    property DataGroupName: string read FDataGroupName Write FDataGroupName;
  end ;

  ETextParserStructCSVSetterException = class(ETextParserStructCSVException)
  private
    FSetterErrorMessage: string;
  public
    constructor Create( const pMessage: string ;
                        pRow: Integer ; pCol: Integer ;
                        const pDataGroup: string;
                        const pField: string ;
                        const pValue: string;
                        const pSetterErrorMessage: string ) ;
    property SetterErrorMessage: string read FSetterErrorMessage Write FSetterErrorMessage;
  end ;

  TtiStructCSVMetaDatas = class;
  TtiStructCSVMetaData = class;
  TtiStructCSVMetaDataItem = class;
  TtiTextParserRowEvent    = procedure( const pDataGroup: string) of object ;
  TtiTextParserSetterEvent = procedure( const pDataGroup: string ;
                                        const pFieldName: string ;
                                        const pValue: string) of object ;


  TtiStructCSVMetaDatas = class(TtiObjectList)
  private
    FLatestUsed : TtiStructCSVMetaData;
  protected
    function    GetItems(i: integer): TtiStructCSVMetaData ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiStructCSVMetaData); reintroduce ;
  public
    property    Items[i:integer] : TtiStructCSVMetaData read GetItems write SetItems ;
    procedure   Add( pObject : TtiStructCSVMetaData   ; pDefDispOrdr : boolean = true ) ; reintroduce ;

    function    FindByDataGroupName(const pDataGroupName: string): TtiStructCSVMetaData;
    function    FindCreateByDataGroupName(const pDataGroupName: string;
                                          const pRowStartEvent: TtiTextParserRowEvent;
                                          const pRowEndEvent:   TtiTextParserRowEvent): TtiStructCSVMetaData;

    property    LatestUsed: TtiStructCSVMetaData read FLatestUsed;
    function    AddInstance(const pGroupName     : string ;
                            const pFieldName     : string;
                            const pRowStartEvent : TtiTextParserRowEvent;
                            const pSetter        : TtiTextParserSetterEvent;
                            const pRowEndEvent   : TtiTextParserRowEvent):TtiStructCSVMetaDataItem;
    procedure   ClearIndexedItems;
  end;

  TtiStructCSVMetaData = class( TtiObjectList )
  private
    FDataGroupName: string;
    FIndexList: TObjectList;
    FOnRowStartEvent: TtiTextParserRowEvent;
    FOnRowEndEvent: TtiTextParserRowEvent;
    procedure SetDataGroupName(const Value: string);
    function  GetIndexedItems(i: Integer): TtiStructCSVMetaDataItem;
    function  GetIndexedCount: Integer;
  protected
    function    GetItems(i: integer): TtiStructCSVMetaDataItem ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiStructCSVMetaDataItem); reintroduce ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Items[i:integer] : TtiStructCSVMetaDataItem read GetItems write SetItems ;
    procedure   Add( pObject : TtiStructCSVMetaDataItem   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    DataGroupName: string read FDataGroupName Write SetDataGroupName;
    property    OnRowStartEvent: TtiTextParserRowEvent read FOnRowStartEvent Write FOnRowStartEvent;
    property    OnRowEndEvent:   TtiTextParserRowEvent read FOnRowEndEvent Write FOnRowEndEvent;
    function    AddInstance(const pFieldName: string; const pSetter: TtiTextParserSetterEvent ):TtiStructCSVMetaDataItem;
    function    FindByFieldName(const pFieldName: string):TtiStructCSVMetaDataItem;
    function    AddToIndex(const pFieldName: string):Boolean;
    property    IndexedItems[i:Integer]:TtiStructCSVMetaDataItem read GetIndexedItems;
    property    IndexedCount: Integer read GetIndexedCount;
    function    FindInIndex(const pFieldName: string):TtiStructCSVMetaDataItem;
    procedure   ClearIndexedItems;
  end ;

  TtiStructCSVMetaDataItem = class( TtiObject )
  private
    FFieldName: string;
    FFieldSetter: TtiTextParserSetterEvent;
  public
    property FieldName: string read FFieldName Write FFieldName ;
    property FieldSetter: TtiTextParserSetterEvent read FFieldSetter Write FFieldSetter;
  end ;


  TtpemsState = ( tpemsStartOfRow,
                  tpemsStartOfIRow, tpemsICell,
                  tpemsStartOfDRow, tpemsDCell );


  TTextParserStructCSV = class( TtiBaseObject )
  private
    FTextFileParser: TtiTextParser;
    FState: TtpemsState;
    FMetaDatas: TtiStructCSVMetaDatas;
    FCellIndex: Integer;
    FLogExceptions: Boolean;
    FExceptions: TStringList;
    FInDataRow: Boolean;
    procedure Clear;
  private
    procedure DoOnNewLine ;
    procedure DoOnEndOfLine ;
    procedure DoOnEndOfText ;
    procedure DoOnCellEnd( const pString: string );
    procedure RaiseStructCSVException( const pMessage: string ;
                        pRow: Integer ; pCol: Integer ;
                        const pToken: string );
    procedure RaiseStructCSVDataGroupException( const pMessage: string ;
                        pRow: Integer ; pCol: Integer ;
                        const pDataGroup: string;
                        const pToken: string ) ;
    procedure RaiseStructCSVSetterExceptionCreate( const pMessage: string ;
                        pRow: Integer ; pCol: Integer ;
                        const pDataGroup: string;
                        const pField: string;
                        const pValue: string ;
                        const pSetterErrorMessage: string );

    procedure RaiseStructCSVOnRowExceptionCreate(
                        const pMessage : string ;
                        pRow: Integer ; pCol: Integer ;
                        const pDataGroup: string;
                        const pEventMessage: string ) ;

  public
    constructor Create;
    destructor  Destroy ; override ;
    procedure   ParseString(const pString: string);
    procedure   ParseFile(const pFileName: string);
    procedure   ParseStream(AStream: TStream);
    property    MetaDatas: TtiStructCSVMetaDatas read FMetaDatas;
    property    LogExceptions: Boolean read FLogExceptions Write FLogExceptions;
    property    ExceptionMessages: TStringList read FExceptions;
    function    Row: Integer;
    function    Col(const pToken: string): Integer; overload ;
    function    Col: Integer; overload ;
  end ;


implementation
uses
  tiConstants
  ,tiExcept
  ;


const
  cIPrefix = 'I';
  cDPrefix = 'D';


{ TTextParserStructCSV }

constructor TTextParserStructCSV.Create;
begin
  inherited Create;
  FTextFileParser:= TtiTextParser.Create;
  FTextFileParser.OnNewLine   := DoOnNewLine;
  FTextFileParser.OnEndOfLine := DoOnEndOfLine;
  FTextFileParser.OnCellEnd   := DoOnCellEnd;
  FTextFileParser.OnEndOfText := DoOnEndOfText;
  FMetaDatas:= TtiStructCSVMetaDatas.Create;
  FExceptions:= TStringList.Create;
  FInDataRow:= False;
end;


destructor TTextParserStructCSV.Destroy;
begin
  FTextFileParser.Free;
  FMetaDatas.Free;
  FExceptions.Free;
  inherited;
end;


procedure TTextParserStructCSV.DoOnCellEnd(const pString: string);
var
  lMetaData: TtiStructCSVMetaData;
  lMetaDataItem: TtiStructCSVMetaDataItem;
  lFieldSetter: TtiTextParserSetterEvent;
begin
  case FState of
  tpemsStartOfRow :    begin
                         if pString = cDPrefix then
                           FState := tpemsStartOfDRow
                         else if pString = cIPrefix then
                           FState := tpemsStartOfIRow
                         else
                           RaiseStructCSVException(
                                   cErrorFirstCharNotIorD,
                                   Row, Col(pString),
                                   pString ) ;
                         FInDataRow := (FState = tpemsStartOfDRow);
                       end ;
  tpemsStartOfIRow  :  begin
                         FState := tpemsICell ;
                         FCellIndex := 0 ;
                         lMetaData := FMetaDatas.FindByDataGroupName(pString);
                         if lMetaData = nil then
                           RaiseStructCSVException(
                                   cErrorIGroupNotFound,
                                   Row, Col(pString),
                                   pString )
                         else if lMetaData.IndexedCount <> 0 then
                           RaiseStructCSVDataGroupException(
                                   cErrorDuplicateGroupName,
                                   Row, Col(pString),
                                   pString,
                                   '' ) ;
                       end ;
  tpemsICell   :       begin
                         Inc(FCellIndex);
                         if FMetaDatas.LatestUsed.FindInIndex(pString) <> nil then
                           RaiseStructCSVDataGroupException(
                                   cErrorDuplicateColumnName,
                                   Row, Col(pString),
                                   FMetaDatas.LatestUsed.DataGroupName,
                                   pString )
                         else if not FMetaDatas.LatestUsed.AddToIndex(pString) then
                           RaiseStructCSVDataGroupException(
                                   cErrorIColumnNotFound,
                                   Row, Col(pString),
                                   FMetaDatas.LatestUsed.DataGroupName,
                                   pString ) ;
                       end ;
  tpemsStartOfDRow  :  begin
                         FState := tpemsDCell ;
                         FCellIndex := 0 ;
                         if FMetaDatas.FindByDataGroupName(pString) = nil then
                           RaiseStructCSVException(
                                   cErrorDGroupNotFound,
                                   Row, Col(pString),
                                   pString ) ;
                         Assert( FMetaDatas.LatestUsed.TestValid(TtiStructCSVMetaData), cTIInvalidObjectError );
                         try
                           FMetaDatas.LatestUsed.OnRowStartEvent(pString);
                         except
                           on e:Exception do
                             RaiseStructCSVOnRowExceptionCreate(
                                   cErrorInOnRowEvent,
                                   Row, Col(pString),
                                   FMetaDatas.LatestUsed.DataGroupName,
                                   e.message ) ;
                         end;
                       end ;
  tpemsDCell   :       begin
                         Assert( FMetaDatas.LatestUsed.TestValid, cTIInvalidObjectError );
                         Inc(FCellIndex);
                         // Only trigger the setter if pString contains a value
                         if pString <> '' then
                         begin
                           try
                             lMetaDataItem := FMetaDatas.LatestUsed.IndexedItems[FCellIndex-1];
                             Assert( lMetaDataItem.TestValid, cTIInvalidObjectError );
                             lFieldSetter := lMetaDataItem.FieldSetter;
                             try
                               lFieldSetter(FMetaDatas.LatestUsed.DataGroupName,
                                            lMetaDataItem.FieldName,
                                            pString);
                             except
                               on e:Exception do
                                 RaiseStructCSVSetterExceptionCreate(
                                       cErrorInFieldSetter,
                                       Row, Col(pString),
                                       FMetaDatas.LatestUsed.DataGroupName,
                                       lMetaDataItem.FieldName,
                                       pString,
                                       e.message ) ;
                             end ;
                           except
                             on e:EListError do
                               RaiseStructCSVDataGroupException(
                                     cErrorMoreDataThanMetaData,
                                     Row, Col(pString),
                                     FMetaDatas.LatestUsed.DataGroupName,
                                     pString ) ;
                             on e:Exception do
                               raise ;
                           end ;
                         end;
                       end ;
  else
    raise Exception.Create(cErrorInvalidTextParserState);
  end;

end;

procedure TTextParserStructCSV.DoOnNewLine;
begin
  Assert( FMetaDatas.LatestUsed.TestValid, cTIInvalidObjectError );
  FState := tpemsStartOfRow ;
  if FCellIndex < FMetaDatas.LatestUsed.IndexedCount then
    RaiseStructCSVDataGroupException(
         cErrorMoreMetaDataThanData,
         Row, Col(FTextFileParser.Token),
         FMetaDatas.LatestUsed.DataGroupName,
         FTextFileParser.Token ) ;
end;

procedure TTextParserStructCSV.ParseFile(const pFileName: string);
begin
  Assert(pFileName <> '', 'pFileName not assigned');
  Assert(FileExists(pFileName), 'File not found <' + pFileName + '>');
  Clear;
  FTextFileParser.ParseFile(pFileName);
end;

procedure TTextParserStructCSV.Clear;
begin
  FCellIndex := 0 ;
  FState := tpemsStartOfRow;
  FExceptions.Clear;
  FMetaDatas.ClearIndexedItems;
  FInDataRow:= False;
end;

procedure TTextParserStructCSV.ParseString(const pString: string);
begin
  Clear;
  FTextFileParser.ParseString(pString);
end;

function TTextParserStructCSV.Col(const pToken: string): Integer;
begin
  Result := FTextFileParser.Col - Length(pToken);
end;

function TTextParserStructCSV.Col: Integer;
begin
  Result := FTextFileParser.Col;
end;

function TTextParserStructCSV.Row: Integer;
begin
  Result := FTextFileParser.Row + 1;
end;

procedure TTextParserStructCSV.DoOnEndOfText;
begin
  Assert( FMetaDatas.LatestUsed.TestValid, cTIInvalidObjectError );
  // FState := tpemsStartOfRow ;
  if {(FState = tpemsDCell ) and}
     (FCellIndex < FMetaDatas.LatestUsed.IndexedCount) then
    RaiseStructCSVDataGroupException(
         cErrorMoreMetaDataThanData,
         Row, Col(FTextFileParser.Token),
         FMetaDatas.LatestUsed.DataGroupName,
         FTextFileParser.Token ) ;
end;

procedure TTextParserStructCSV.RaiseStructCSVException(
  const pMessage: string; pRow, pCol: Integer; const pToken: string);
var
  lE : ETextParserStructCSVException ;
begin
  lE := ETextParserStructCSVException.Create( pMessage, pRow, pCol, pToken ) ;
  if FLogExceptions then
    FExceptions.Add(lE.Message)
  else
    raise lE;
  lE.Free;
end;

procedure TTextParserStructCSV.RaiseStructCSVDataGroupException(
  const pMessage: string; pRow, pCol: Integer; const pDataGroup,
  pToken: string);
var
  lE : ETextParserStructCSVDataGroupException;
begin
  lE := ETextParserStructCSVDataGroupException.Create(
      pMessage, pRow, pCol, pDataGroup, pToken );
  if FLogExceptions then
    FExceptions.Add(lE.Message)
  else
    raise lE;
  lE.Free;
end;

procedure TTextParserStructCSV.RaiseStructCSVSetterExceptionCreate(
  const pMessage: string; pRow, pCol: Integer; const pDataGroup, pField, pValue,
  pSetterErrorMessage: string);
var
  lE : ETextParserStructCSVSetterException;
begin
  lE := ETextParserStructCSVSetterException.Create(
              pMessage,
              pRow, pCol,
              pDataGroup,
              pField,
              pValue,
              pSetterErrorMessage );
  if FLogExceptions then
    FExceptions.Add(lE.Message)
  else
    raise lE;
  lE.Free;
end;

procedure TTextParserStructCSV.RaiseStructCSVOnRowExceptionCreate(
  const pMessage: string; pRow, pCol: Integer; const pDataGroup,
  pEventMessage: string);
var
  lE : ETextParserStructCSVOnRowException;
begin
  lE := ETextParserStructCSVOnRowException.Create(
              pMessage,
              pRow, pCol,
              pDataGroup,
              pEventMessage );
  if FLogExceptions then
    FExceptions.Add(lE.Message)
  else
    raise lE;
  lE.Free;
end;

procedure TTextParserStructCSV.DoOnEndOfLine;
var
  lMetaDataItem: TtiStructCSVMetaData;
begin
  if not FInDataRow then
    Exit ; //==>
  Assert( FMetaDatas.LatestUsed.TestValid, cTIInvalidObjectError );
  lMetaDataItem := FMetaDatas.LatestUsed;
  lMetaDataItem.OnRowEndEvent(FMetaDatas.LatestUsed.DataGroupName);
end;

procedure TTextParserStructCSV.ParseStream(AStream: TStream);
begin
  Assert(AStream<>nil, 'AStream not assigned');
  Assert(FTextFileParser.TestValid, cErrorTIPerObjAbsTestValid);
  Clear;
  FTextFileParser.ParseStream(AStream);
end;

{ TtiStructCSVMetaDatas }

procedure TtiStructCSVMetaDatas.Add(pObject: TtiStructCSVMetaData; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, false ) ;
end;

function TtiStructCSVMetaDatas.AddInstance(
  const pGroupName     : string;
  const pFieldName     : string;
  const pRowStartEvent : TtiTextParserRowEvent;
  const pSetter        : TtiTextParserSetterEvent;
  const pRowEndEvent   : TtiTextParserRowEvent): TtiStructCSVMetaDataItem;
var
  lStructCSVMetaData : TtiStructCSVMetaData;
begin
  lStructCSVMetaData := FindCreateByDataGroupName(pGroupName, pRowStartEvent, pRowEndEvent);
  if lStructCSVMetaData <> nil then
    Result := lStructCSVMetaData.AddInstance(pFieldName, pSetter)
  else
    Result := nil ;
end;

procedure TtiStructCSVMetaDatas.ClearIndexedItems;
var
  i : Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].ClearIndexedItems;
end;

function TtiStructCSVMetaDatas.FindByDataGroupName(const pDataGroupName: string): TtiStructCSVMetaData;
var
  i : Integer ;
begin

  if ( FLatestUsed <> nil ) and
     ( FLatestUsed.DataGroupName = pDataGroupName ) then
  begin
    Result := FLatestUsed;
    Exit ; //==>
  end;

  for i := 0 to Count - 1 do
    if Items[i].DataGroupName = pDataGroupName then
    begin
      Result := Items[i];
      FLatestUsed := Result ;
      Exit ; //==>
    end;
  Result := nil ;

end;

function TtiStructCSVMetaDatas.FindCreateByDataGroupName(const pDataGroupName: string;
                                                         const pRowStartEvent: TtiTextParserRowEvent;
                                                         const pRowEndEvent: TtiTextParserRowEvent): TtiStructCSVMetaData;
begin
  Assert(pDataGroupName <> '', 'pDataGroupName = ''''');
  Assert(Assigned(pRowStartEvent), 'pRowStartEvent not assigned');
  Assert(Assigned(pRowEndEvent), 'pRowEndEvent not assigned');

  Result := FindByDataGroupName(pDataGroupName);
  if ( Result <> nil ) then
    Exit ; //==>
  Result                 := TtiStructCSVMetaData.Create;
  Result.DataGroupName   := pDataGroupName;
  Result.OnRowStartEvent := pRowStartEvent;
  Result.OnRowEndEvent   := pRowEndEvent;
  Add(Result);
  FLatestUsed := Result ;
end;

function TtiStructCSVMetaDatas.GetItems(i: integer): TtiStructCSVMetaData;
begin
  result := TtiStructCSVMetaData( inherited GetItems( i )) ;
end;

procedure TtiStructCSVMetaDatas.SetItems(i: Integer; const Value: TtiStructCSVMetaData);
begin
  inherited SetItems( i, Value ) ;
end;

{ TtiStructCSVMetaData }

procedure TtiStructCSVMetaData.Add(pObject: TtiStructCSVMetaDataItem; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, false ) ;
end;

function TtiStructCSVMetaData.AddInstance(const pFieldName: string; const pSetter: TtiTextParserSetterEvent ): TtiStructCSVMetaDataItem;
begin
  Assert(FindByFieldName(pFieldName) = nil, 'Attempt to add duplicate instance <' + pFieldName + '>' ) ;
  Assert(Assigned(pSetter), 'pSetter not assigned');
  Result := TtiStructCSVMetaDataItem.Create;
  Result.FieldName := pFieldName;
  Result.FieldSetter := pSetter;
  Add(Result);
end;

function TtiStructCSVMetaData.AddToIndex(const pFieldName: string): Boolean;
var
  lo : TtiStructCSVMetaDataItem;
begin
  lo := FindByFieldName(pFieldName);
  if lo = nil then
  begin
    Result := False;
    Exit ; //==>
  end;
  Assert( lo.TestValid(TtiStructCSVMetaDataItem), cTIInvalidObjectError );
  FIndexList.Add(lo);
  Result := True;
end;

procedure TtiStructCSVMetaData.ClearIndexedItems;
begin
  FIndexList.Clear;
end;

constructor TtiStructCSVMetaData.Create;
begin
  inherited;
  FIndexList:= TObjectList.Create(False);
end;

destructor TtiStructCSVMetaData.Destroy;
begin
  FIndexList.Free;
  inherited;
end;

function TtiStructCSVMetaData.FindByFieldName(const pFieldName: string): TtiStructCSVMetaDataItem;
var
  i: Integer;
begin
  Result := nil ;
  for i := 0 to Count - 1 do
    if Items[i].FieldName = pFieldName then
    begin
      Result := Items[i];
      Exit ; //==>
    end;
end;

function TtiStructCSVMetaData.FindInIndex(const pFieldName: string): TtiStructCSVMetaDataItem;
var
  i: Integer;
begin
  for i := 0 to FIndexList.Count - 1 do
    if TtiStructCSVMetaDataItem(FIndexList.Items[i]).FieldName = pFieldName then
    begin
      Result := TtiStructCSVMetaDataItem(FIndexList.Items[i]);
      Exit ; //==>
    end;
  Result := nil ;
end;

function TtiStructCSVMetaData.GetIndexedCount: Integer;
begin
  Result := FIndexList.Count;
end;

function TtiStructCSVMetaData.GetIndexedItems(i: Integer): TtiStructCSVMetaDataItem;
begin
  Result := TtiStructCSVMetaDataItem(FIndexList.Items[i]);
end;

function TtiStructCSVMetaData.GetItems(
  i: integer): TtiStructCSVMetaDataItem;
begin
  result := TtiStructCSVMetaDataItem( inherited GetItems( i )) ;
end;

procedure TtiStructCSVMetaData.SetDataGroupName(const Value: string);
begin
  FDataGroupName := Value;
end;

procedure TtiStructCSVMetaData.SetItems(i: Integer; const Value: TtiStructCSVMetaDataItem);
begin
  inherited SetItems( i, Value ) ;
end;

{ ETextParserStructCSVException }

constructor ETextParserStructCSVException.Create(const pMessage: string;
  pRow, pCol: Integer; const pToken: string);
var
  lMessage : string ;
begin
  FRow   := pRow;
  FCol   := pCol;
  FToken := pToken;
  FUnFormattedMessage := pMessage ;
  lMessage :=
    pMessage +
    '. Column name: ' + pToken +
    '; Row in data file: ' + IntToStr(FRow) +
    '; Col in data file: ' + IntToStr(FCol);
  inherited Create(lMessage);
end;

{ ETextParserStructCSVDataGroupException }

constructor ETextParserStructCSVDataGroupException.Create(
  const pMessage: string; pRow, pCol: Integer; const pDataGroup,
  pToken: string);
begin
  inherited Create(pMessage, pRow, pCol, pToken);
  FDataGroupName := pDataGroup;
  Message :=
    pMessage +
    '. Group name: ' + pDataGroup +
    '; Column name: ' + pToken +
    '; Row in data file: ' + IntToStr(pRow) +
    '; Col in data file: ' + IntToStr(pCol);
end;

{ ETextParserStructCSVSetterException }

constructor ETextParserStructCSVSetterException.Create(
  const pMessage: string; pRow, pCol: Integer; const pDataGroup, pField, pValue,
  pSetterErrorMessage: string);
begin
  inherited Create(pMessage, pRow, pCol, pValue);
  Message :=
    pMessage +
    '. Group name: ' + pDataGroup +
    '; Column name: ' + pField +
    '; Value: ' + pValue +
    '; Row in data file: ' + IntToStr(pRow) +
    '; Col in data file: ' + IntToStr(pCol) +
    '; Error message: ' + pSetterErrorMessage ;
  FSetterErrorMessage := pSetterErrorMessage ;
end;

{ ETextParserStructCSVOnRowException }

constructor ETextParserStructCSVOnRowException.Create(
  const pMessage: string; pRow, pCol: Integer; const pDataGroup,
  pEventMessage: string);
begin
  inherited Create(pMessage, pRow, pCol, pDataGroup);
  FDataGroupName := pDataGroup;
  FEventMessage  := pEventMessage;
  Message :=
    pMessage +
    '. Group name: ' + pDataGroup +
    '; Event message: ' + pEventMessage +
    '; Row in data file: ' + IntToStr(pRow) +
    '; Col in data file: ' + IntToStr(pCol);
end;

end.
