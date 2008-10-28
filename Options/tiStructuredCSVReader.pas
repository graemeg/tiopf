unit tiStructuredCSVReader;

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
  cErrorIGroupNotFound         = 'Data group in an "I" row not found in registered property setters';
  cErrorDGroupNotFound         = 'Data group in an "D" row not found in registered property setters';
  cErrorDuplicateGroupName     = 'Duplicate group name in an "I" row';
  cErrorDuplicateColumnName    = 'Duplicate column name in an "I" row';
  cErrorFirstCharNotIorD       = 'First character not an "I" or "D"';
  cErrorIColumnNotFound        = 'Column in an "I" row not found in registered property setters';
  cErrorMoreDataThanMetaData   = 'More columns of data than meta data';
  cErrorMoreMetaDataThanData   = 'More columns of meta data than data';
  cErrorInFieldSetter          = 'Error in field setter';
  cErrorInOnRowEvent           = 'Error in OnRowEvent';

type

  ETextParserStructCSVException = class(Exception)
  private
    FRow: Integer;
    FCol: Integer;
    FToken: string;
    FUnFormattedMessage: string;
  public
    constructor Create(const AMessage: string;
                        pRow: Integer; pCol: Integer;
                        const pToken: string);
    property Row: Integer read FRow Write FRow;
    property Col: Integer read FCol Write FCol;
    property Token: string read FToken Write FToken;
    property UnFormattedMessage: string read FUnFormattedMessage;
  end;

  ETextParserStructCSVOnRowException = class(ETextParserStructCSVException)
  private
    FDataGroupName: string;
    FEventMessage: string;
  public
    constructor Create(const AMessage: string;
                        pRow: Integer; pCol: Integer;
                        const pDataGroup: string;
                        const pEventMessage: string);
    property DataGroupName: string read FDataGroupName Write FDataGroupName;
    property EventMessage: string read FEventMessage Write FEventMessage;
  end;

  ETextParserStructCSVDataGroupException = class(ETextParserStructCSVException)
  private
    FDataGroupName: string;
  public
    constructor Create(const AMessage: string;
                        pRow: Integer; pCol: Integer;
                        const pDataGroup: string;
                        const pToken: string);
    property DataGroupName: string read FDataGroupName Write FDataGroupName;
  end;

  ETextParserStructCSVSetterException = class(ETextParserStructCSVException)
  private
    FSetterErrorMessage: string;
  public
    constructor Create(const AMessage: string;
                        pRow: Integer; pCol: Integer;
                        const pDataGroup: string;
                        const pField: string;
                        const AValue: string;
                        const pSetterErrorMessage: string);
    property SetterErrorMessage: string read FSetterErrorMessage Write FSetterErrorMessage;
  end;

  TtiStructCSVMetaDatas = class;
  TtiStructCSVMetaData = class;
  TtiStructCSVMetaDataItem = class;
  TtiTextParserRowEvent    = procedure(const pDataGroup: string) of object;
  TtiTextParserSetterEvent = procedure(const pDataGroup: string;
                                        const AFieldName: string;
                                        const AValue: string) of object;


  TtiStructCSVMetaDatas = class(TtiObjectList)
  private
    FLatestUsed : TtiStructCSVMetaData;
  protected
    function    GetItems(i: integer): TtiStructCSVMetaData; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiStructCSVMetaData); reintroduce;
  public
    property    Items[i:integer]: TtiStructCSVMetaData read GetItems write SetItems;
    procedure   Add(AObject: TtiStructCSVMetaData); reintroduce;
    function    FindByDataGroupName(const pDataGroupName: string): TtiStructCSVMetaData;
    function    FindCreateByDataGroupName(const pDataGroupName: string;
                                          const pRowStartEvent: TtiTextParserRowEvent;
                                          const pRowEndEvent: TtiTextParserRowEvent): TtiStructCSVMetaData;
    property    LatestUsed: TtiStructCSVMetaData read FLatestUsed;
    function    AddInstance(const AGroupName: string;
                            const AFieldName: string;
                            const pRowStartEvent: TtiTextParserRowEvent;
                            const pSetter: TtiTextParserSetterEvent;
                            const pRowEndEvent: TtiTextParserRowEvent):TtiStructCSVMetaDataItem;
    procedure   ClearIndexedItems;
  end;


  TtiStructCSVMetaData = class(TtiObjectList)
  private
    FDataGroupName: string;
    FIndexList: TObjectList;
    FOnRowStartEvent: TtiTextParserRowEvent;
    FOnRowEndEvent: TtiTextParserRowEvent;
    procedure SetDataGroupName(const AValue: string);
    function  GetIndexedItems(i: Integer): TtiStructCSVMetaDataItem;
    function  GetIndexedCount: Integer;
  protected
    function    GetItems(i: integer): TtiStructCSVMetaDataItem; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiStructCSVMetaDataItem); reintroduce;
  public
    constructor Create; override;
    destructor  Destroy; override;
    function    AddInstance(const AFieldName: string; const pSetter: TtiTextParserSetterEvent):TtiStructCSVMetaDataItem;
    function    AddToIndex(const AFieldName: string):Boolean;
    function    FindByFieldName(const AFieldName: string):TtiStructCSVMetaDataItem;
    function    FindInIndex(const AFieldName: string):TtiStructCSVMetaDataItem;
    procedure   Add(AObject: TtiStructCSVMetaDataItem); reintroduce;
    procedure   ClearIndexedItems;
    property    DataGroupName: string read FDataGroupName Write SetDataGroupName;
    property    IndexedCount: Integer read GetIndexedCount;
    property    IndexedItems[i:Integer]:TtiStructCSVMetaDataItem read GetIndexedItems;
    property    Items[i:integer]: TtiStructCSVMetaDataItem read GetItems write SetItems;
    property    OnRowEndEvent: TtiTextParserRowEvent read FOnRowEndEvent Write FOnRowEndEvent;
    property    OnRowStartEvent: TtiTextParserRowEvent read FOnRowStartEvent Write FOnRowStartEvent;
  end;


  TtiStructCSVMetaDataItem = class(TtiObject)
  private
    FFieldName: string;
    FFieldSetter: TtiTextParserSetterEvent;
  public
    property FieldName: string read FFieldName Write FFieldName;
    property FieldSetter: TtiTextParserSetterEvent read FFieldSetter Write FFieldSetter;
  end;


  TtpemsState = (tpemsStartOfRow, tpemsStartOfIRow, tpemsICell, tpemsStartOfDRow, tpemsDCell);


  TTextParserStructCSV = class(TtiBaseObject)
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
    procedure DoOnNewLine;
    procedure DoOnEndOfLine;
    procedure DoOnEndOfText;
    procedure DoOnCellEnd(const AString: string);
    procedure RaiseStructCSVException(const AMessage: string;
                        pRow: Integer; pCol: Integer;
                        const pToken: string);
    procedure RaiseStructCSVDataGroupException(const AMessage: string;
                        pRow: Integer; pCol: Integer;
                        const pDataGroup: string;
                        const pToken: string);
    procedure RaiseStructCSVSetterExceptionCreate(const AMessage: string;
                        pRow: Integer; pCol: Integer;
                        const pDataGroup: string;
                        const pField: string;
                        const AValue: string;
                        const pSetterErrorMessage: string);

    procedure RaiseStructCSVOnRowExceptionCreate(
                        const AMessage : string;
                        pRow: Integer; pCol: Integer;
                        const pDataGroup: string;
                        const pEventMessage: string);

  public
    constructor Create;
    destructor  Destroy; override;
    procedure   ParseString(const AString: string);
    procedure   ParseFile(const AFileName: string);
    procedure   ParseStream(AStream: TStream);
    property    MetaDatas: TtiStructCSVMetaDatas read FMetaDatas;
    property    LogExceptions: Boolean read FLogExceptions Write FLogExceptions;
    property    ExceptionMessages: TStringList read FExceptions;
    function    Row: Integer;
    function    Col(const pToken: string): Integer; overload;
    function    Col: Integer; overload;
  end;


implementation
uses
  tiConstants
 ;

{ TTextParserStructCSV }

constructor TTextParserStructCSV.Create;
begin
  inherited Create;
  FTextFileParser := TtiTextParser.Create;
  FTextFileParser.OnNewLine     := DoOnNewLine;
  FTextFileParser.OnEndOfLine   := DoOnEndOfLine;
  FTextFileParser.OnCellEnd     := DoOnCellEnd;
  FTextFileParser.OnEndOfText   := DoOnEndOfText;
  FMetaDatas    := TtiStructCSVMetaDatas.Create;
  FExceptions   := TStringList.Create;
  FInDataRow    := False;
end;


destructor TTextParserStructCSV.Destroy;
begin
  FTextFileParser.Free;
  FMetaDatas.Free;
  FExceptions.Free;
  inherited;
end;


procedure TTextParserStructCSV.DoOnCellEnd(const AString: string);
var
  lMetaData: TtiStructCSVMetaData;
  lMetaDataItem: TtiStructCSVMetaDataItem;
  lFieldSetter: TtiTextParserSetterEvent;
begin
  case FState of
  tpemsStartOfRow :    begin
                         if AString = CStructCSVPrefixD then
                           FState := tpemsStartOfDRow
                         else if AString = CStructCSVPrefixI then
                           FState := tpemsStartOfIRow
                         else
                           RaiseStructCSVException(
                                   cErrorFirstCharNotIorD,
                                   Row, Col(AString),
                                   AString);
                         FInDataRow := (FState = tpemsStartOfDRow);
                       end;
  tpemsStartOfIRow :  begin
                         FState := tpemsICell;
                         FCellIndex := 0;
                         lMetaData := FMetaDatas.FindByDataGroupName(AString);
                         if lMetaData = nil then
                           RaiseStructCSVException(
                                   cErrorIGroupNotFound,
                                   Row, Col(AString),
                                   AString)
                         else if lMetaData.IndexedCount <> 0 then
                           RaiseStructCSVDataGroupException(
                                   cErrorDuplicateGroupName,
                                   Row, Col(AString),
                                   AString,
                                   '');
                       end;
  tpemsICell  :       begin
                         Inc(FCellIndex);
                         if FMetaDatas.LatestUsed.FindInIndex(AString) <> nil then
                           RaiseStructCSVDataGroupException(
                                   cErrorDuplicateColumnName,
                                   Row, Col(AString),
                                   FMetaDatas.LatestUsed.DataGroupName,
                                   AString)
                         else if not FMetaDatas.LatestUsed.AddToIndex(AString) then
                           RaiseStructCSVDataGroupException(
                                   cErrorIColumnNotFound,
                                   Row, Col(AString),
                                   FMetaDatas.LatestUsed.DataGroupName,
                                   AString);
                       end;
  tpemsStartOfDRow :  begin
                         FState := tpemsDCell;
                         FCellIndex := 0;
                         if FMetaDatas.FindByDataGroupName(AString) = nil then
                           RaiseStructCSVException(
                                   cErrorDGroupNotFound,
                                   Row, Col(AString),
                                   AString);
                         Assert(FMetaDatas.LatestUsed.TestValid(TtiStructCSVMetaData), CTIErrorInvalidObject);
                         try
                           FMetaDatas.LatestUsed.OnRowStartEvent(AString);
                         except
                           on e:Exception do
                             RaiseStructCSVOnRowExceptionCreate(
                                   cErrorInOnRowEvent,
                                   Row, Col(AString),
                                   FMetaDatas.LatestUsed.DataGroupName,
                                   e.message);
                         end;
                       end;
  tpemsDCell  :       begin
                         Assert(FMetaDatas.LatestUsed.TestValid, CTIErrorInvalidObject);
                         Inc(FCellIndex);
                         // Only trigger the setter if AString contains a value
                         if AString <> '' then
                         begin
                           try
                             lMetaDataItem := FMetaDatas.LatestUsed.IndexedItems[FCellIndex-1];
                             Assert(lMetaDataItem.TestValid, CTIErrorInvalidObject);
                             lFieldSetter := lMetaDataItem.FieldSetter;
                             try
                               lFieldSetter(FMetaDatas.LatestUsed.DataGroupName,
                                            lMetaDataItem.FieldName,
                                            AString);
                             except
                               on e:Exception do
                                 RaiseStructCSVSetterExceptionCreate(
                                       cErrorInFieldSetter,
                                       Row, Col(AString),
                                       FMetaDatas.LatestUsed.DataGroupName,
                                       lMetaDataItem.FieldName,
                                       AString,
                                       e.message);
                             end;
                           except
                             on e:EListError do
                               RaiseStructCSVDataGroupException(
                                     cErrorMoreDataThanMetaData,
                                     Row, Col(AString),
                                     FMetaDatas.LatestUsed.DataGroupName,
                                     AString);
                             on e:Exception do
                               raise;
                           end;
                         end;
                       end;
  else
    raise Exception.Create(cErrorInvalidTextParserState);
  end;
end;

procedure TTextParserStructCSV.DoOnNewLine;
begin
  Assert(FMetaDatas.LatestUsed.TestValid, CTIErrorInvalidObject);
  FState := tpemsStartOfRow;
  if FCellIndex < FMetaDatas.LatestUsed.IndexedCount then
    RaiseStructCSVDataGroupException(
         cErrorMoreMetaDataThanData,
         Row, Col(FTextFileParser.Token),
         FMetaDatas.LatestUsed.DataGroupName,
         FTextFileParser.Token);
end;

procedure TTextParserStructCSV.ParseFile(const AFileName: string);
begin
  Assert(AFileName <> '', 'AFileName not assigned');
  Assert(FileExists(AFileName), 'File not found <' + AFileName + '>');
  Clear;
  FTextFileParser.ParseFile(AFileName);
end;

procedure TTextParserStructCSV.Clear;
begin
  FCellIndex := 0;
  FState := tpemsStartOfRow;
  FExceptions.Clear;
  FMetaDatas.ClearIndexedItems;
  FInDataRow:= False;
end;

procedure TTextParserStructCSV.ParseString(const AString: string);
begin
  Clear;
  FTextFileParser.ParseString(AString);
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
  Assert(FMetaDatas.LatestUsed.TestValid, CTIErrorInvalidObject);
  // FState := tpemsStartOfRow;
  if {(FState = tpemsDCell) and}
     (FCellIndex < FMetaDatas.LatestUsed.IndexedCount) then
    RaiseStructCSVDataGroupException(
         cErrorMoreMetaDataThanData,
         Row, Col(FTextFileParser.Token),
         FMetaDatas.LatestUsed.DataGroupName,
         FTextFileParser.Token);
end;

procedure TTextParserStructCSV.RaiseStructCSVException(
  const AMessage: string; pRow, pCol: Integer; const pToken: string);
var
  lE : ETextParserStructCSVException;
begin
  lE := ETextParserStructCSVException.Create(AMessage, pRow, pCol, pToken);
  if FLogExceptions then
    FExceptions.Add(lE.Message)
  else
    raise lE;
  lE.Free;
end;

procedure TTextParserStructCSV.RaiseStructCSVDataGroupException(
  const AMessage: string; pRow, pCol: Integer; const pDataGroup,
  pToken: string);
var
  lE : ETextParserStructCSVDataGroupException;
begin
  lE := ETextParserStructCSVDataGroupException.Create(
      AMessage, pRow, pCol, pDataGroup, pToken);
  if FLogExceptions then
    FExceptions.Add(lE.Message)
  else
    raise lE;
  lE.Free;
end;

procedure TTextParserStructCSV.RaiseStructCSVSetterExceptionCreate(
  const AMessage: string; pRow, pCol: Integer; const pDataGroup, pField, AValue,
  pSetterErrorMessage: string);
var
  lE : ETextParserStructCSVSetterException;
begin
  lE := ETextParserStructCSVSetterException.Create(
              AMessage,
              pRow, pCol,
              pDataGroup,
              pField,
              AValue,
              pSetterErrorMessage);
  if FLogExceptions then
    FExceptions.Add(lE.Message)
  else
    raise lE;
  lE.Free;
end;

procedure TTextParserStructCSV.RaiseStructCSVOnRowExceptionCreate(
  const AMessage: string; pRow, pCol: Integer; const pDataGroup,
  pEventMessage: string);
var
  lE : ETextParserStructCSVOnRowException;
begin
  lE := ETextParserStructCSVOnRowException.Create(
              AMessage,
              pRow, pCol,
              pDataGroup,
              pEventMessage);
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
    Exit; //==>
  Assert(FMetaDatas.LatestUsed.TestValid, CTIErrorInvalidObject);
  lMetaDataItem := FMetaDatas.LatestUsed;
  lMetaDataItem.OnRowEndEvent(FMetaDatas.LatestUsed.DataGroupName);
end;

procedure TTextParserStructCSV.ParseStream(AStream: TStream);
begin
  Assert(AStream<>nil, 'AStream not assigned');
  Assert(FTextFileParser.TestValid, CTIErrorInvalidObject);
  Clear;
  FTextFileParser.ParseStream(AStream);
end;

{ TtiStructCSVMetaDatas }

procedure TtiStructCSVMetaDatas.Add(AObject: TtiStructCSVMetaData);
begin
  inherited Add(AObject);
end;

function TtiStructCSVMetaDatas.AddInstance(
  const AGroupName    : string;
  const AFieldName    : string;
  const pRowStartEvent : TtiTextParserRowEvent;
  const pSetter       : TtiTextParserSetterEvent;
  const pRowEndEvent  : TtiTextParserRowEvent): TtiStructCSVMetaDataItem;
var
  lStructCSVMetaData : TtiStructCSVMetaData;
begin
  lStructCSVMetaData := FindCreateByDataGroupName(AGroupName, pRowStartEvent, pRowEndEvent);
  if lStructCSVMetaData <> nil then
    Result := lStructCSVMetaData.AddInstance(AFieldName, pSetter)
  else
    Result := nil;
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
  i : Integer;
begin

  if (FLatestUsed <> nil) and
     (FLatestUsed.DataGroupName = pDataGroupName) then
  begin
    Result := FLatestUsed;
    Exit; //==>
  end;

  for i := 0 to Count - 1 do
    if Items[i].DataGroupName = pDataGroupName then
    begin
      Result := Items[i];
      FLatestUsed := Result;
      Exit; //==>
    end;
  Result := nil;

end;

function TtiStructCSVMetaDatas.FindCreateByDataGroupName(const pDataGroupName: string;
                                                         const pRowStartEvent: TtiTextParserRowEvent;
                                                         const pRowEndEvent: TtiTextParserRowEvent): TtiStructCSVMetaData;
begin
  Assert(pDataGroupName <> '', 'pDataGroupName = ''''');
  Assert(Assigned(pRowStartEvent), 'pRowStartEvent not assigned');
  Assert(Assigned(pRowEndEvent), 'pRowEndEvent not assigned');

  Result := FindByDataGroupName(pDataGroupName);
  if (Result <> nil) then
    Exit; //==>
  Result                := TtiStructCSVMetaData.Create;
  Result.DataGroupName  := pDataGroupName;
  Result.OnRowStartEvent := pRowStartEvent;
  Result.OnRowEndEvent  := pRowEndEvent;
  Add(Result);
  FLatestUsed := Result;
end;

function TtiStructCSVMetaDatas.GetItems(i: integer): TtiStructCSVMetaData;
begin
  result := TtiStructCSVMetaData(inherited GetItems(i));
end;

procedure TtiStructCSVMetaDatas.SetItems(i: Integer; const AValue: TtiStructCSVMetaData);
begin
  inherited SetItems(i, AValue);
end;

{ TtiStructCSVMetaData }

procedure TtiStructCSVMetaData.Add(AObject: TtiStructCSVMetaDataItem);
begin
  inherited Add(AObject);
end;

function TtiStructCSVMetaData.AddInstance(const AFieldName: string; const pSetter: TtiTextParserSetterEvent): TtiStructCSVMetaDataItem;
begin
  Assert(FindByFieldName(AFieldName) = nil, Format('Attempt to add duplicate TtiTextParserSetterEvent "%s" to DataGroup "%s"', [AFieldName, DataGroupName]));
  Assert(Assigned(pSetter), 'pSetter not assigned');
  Result := TtiStructCSVMetaDataItem.Create;
  Result.FieldName := AFieldName;
  Result.FieldSetter := pSetter;
  Add(Result);
end;

function TtiStructCSVMetaData.AddToIndex(const AFieldName: string): Boolean;
var
  lo : TtiStructCSVMetaDataItem;
begin
  lo := FindByFieldName(AFieldName);
  if lo = nil then
  begin
    Result := False;
    Exit; //==>
  end;
  Assert(lo.TestValid(TtiStructCSVMetaDataItem), CTIErrorInvalidObject);
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

function TtiStructCSVMetaData.FindByFieldName(const AFieldName: string): TtiStructCSVMetaDataItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].FieldName = AFieldName then
    begin
      Result := Items[i];
      Exit; //==>
    end;
end;

function TtiStructCSVMetaData.FindInIndex(const AFieldName: string): TtiStructCSVMetaDataItem;
var
  i: Integer;
begin
  for i := 0 to FIndexList.Count - 1 do
    if TtiStructCSVMetaDataItem(FIndexList.Items[i]).FieldName = AFieldName then
    begin
      Result := TtiStructCSVMetaDataItem(FIndexList.Items[i]);
      Exit; //==>
    end;
  Result := nil;
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
  result := TtiStructCSVMetaDataItem(inherited GetItems(i));
end;

procedure TtiStructCSVMetaData.SetDataGroupName(const AValue: string);
begin
  FDataGroupName := AValue;
end;

procedure TtiStructCSVMetaData.SetItems(i: Integer; const AValue: TtiStructCSVMetaDataItem);
begin
  inherited SetItems(i, AValue);
end;

{ ETextParserStructCSVException }

constructor ETextParserStructCSVException.Create(const AMessage: string;
  pRow, pCol: Integer; const pToken: string);
var
  lMessage : string;
begin
  FRow  := pRow;
  FCol  := pCol;
  FToken := pToken;
  FUnFormattedMessage := AMessage;
  lMessage :=
    AMessage +
    '. Column name: ' + pToken +
    '; Row in data file: ' + IntToStr(FRow) +
    '; Col in data file: ' + IntToStr(FCol);
  inherited Create(lMessage);
end;

{ ETextParserStructCSVDataGroupException }

constructor ETextParserStructCSVDataGroupException.Create(
  const AMessage: string; pRow, pCol: Integer; const pDataGroup,
  pToken: string);
begin
  inherited Create(AMessage, pRow, pCol, pToken);
  FDataGroupName := pDataGroup;
  Message :=
    AMessage +
    '. Group name: ' + pDataGroup +
    '; Column name: ' + pToken +
    '; Row in data file: ' + IntToStr(pRow) +
    '; Col in data file: ' + IntToStr(pCol);
end;

{ ETextParserStructCSVSetterException }

constructor ETextParserStructCSVSetterException.Create(
  const AMessage: string; pRow, pCol: Integer; const pDataGroup, pField, AValue,
  pSetterErrorMessage: string);
begin
  inherited Create(AMessage, pRow, pCol, AValue);
  Message :=
    AMessage +
    '. Group name: ' + pDataGroup +
    '; Column name: ' + pField +
    '; AValue: ' + AValue +
    '; Row in data file: ' + IntToStr(pRow) +
    '; Col in data file: ' + IntToStr(pCol) +
    '; Error message: ' + pSetterErrorMessage;
  FSetterErrorMessage := pSetterErrorMessage;
end;

{ ETextParserStructCSVOnRowException }

constructor ETextParserStructCSVOnRowException.Create(
  const AMessage: string; pRow, pCol: Integer; const pDataGroup,
  pEventMessage: string);
begin
  inherited Create(AMessage, pRow, pCol, pDataGroup);
  FDataGroupName := pDataGroup;
  FEventMessage := pEventMessage;
  Message :=
    AMessage +
    '. Group name: ' + pDataGroup +
    '; Event message: ' + pEventMessage +
    '; Row in data file: ' + IntToStr(pRow) +
    '; Col in data file: ' + IntToStr(pCol);
end;

end.
