unit tiMIMEMultipartFormDataDecoder;

interface
uses
  tiBaseObject,
  tiObject,
  Classes;

type

  TtiMIMEMultipartFormDataItemList = class;
  TtiMIMEMultipartFormDataItem = class;
  TtiMIMEMultipartFormDataDecoder = class;

  TtiMIMEMultipartFormDataItemList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiMIMEMultipartFormDataItem; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiMIMEMultipartFormDataItem); reintroduce;
    procedure   DeriveFieldValues(const AData: TStream; out AFieldName: string; out AFileName: string; const AFieldData: TStream);
    procedure DeriveFieldMetaData(const ALine: string; out AFieldName, AFileName: string);
  public
    property    Items[i:integer] : TtiMIMEMultipartFormDataItem read GetItems write SetItems;
    procedure   Add(const AObject : TtiMIMEMultipartFormDataItem); reintroduce;
    procedure   AddInstance(const AData: TStream);
  end;

  TtiMIMEMultipartFormDataItem = class(TtiObject)
  private
    FRawData: TStream;
    FFieldName: string;
    FData: TStream;
    FFileName: string;
    procedure SetRawData(const AValue: TStream);
    procedure SetData(const AValue: TStream);
    function GetDataAsString: string;
  protected
    function    GetParent: TtiMIMEMultipartFormDataItemList; reintroduce;
  published
  public
    constructor Create; override;
    destructor Destroy; override;
    property    Parent: TtiMIMEMultipartFormDataItemList read GetParent;
    property    RawData: TStream read FRawData write SetRawData;
    property    Data: TStream read FData write SetData;
    property    DataAsString: string read GetDataAsString;
    property    FieldName: string read FFieldName write FFieldName;
    property    FileName: string read FFileName write FFileName;
  end;

  TtiMIMEMultipartFormDataDecoder = class(TtiBaseObject)
  private
    FItemList: TtiMIMEMultipartFormDataItemList;
    FBlockDelimeter: string;
  protected
    procedure GetBlockDelimeter(const AStream: TStream);
    procedure ProcessBlock(const AStream: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    property ItemList: TtiMIMEMultipartFormDataItemList read FItemList;
    procedure Execute(const AStream: TStream);
  end;

implementation
uses
  tiUtils, tiStreams, SysUtils ;

{ TtiMultipartFormDataDecoder }

constructor TtiMIMEMultipartFormDataDecoder.Create;
begin
  inherited;
  FItemList:= TtiMIMEMultipartFormDataItemList.Create;
end;

destructor TtiMIMEMultipartFormDataDecoder.Destroy;
begin
  FItemList.Free;
  inherited;
end;

procedure TtiMIMEMultipartFormDataDecoder.Execute(const AStream: TStream);
var
  LStream: TMemoryStream;
begin
  GetBlockDelimeter(AStream);
  LStream:= TMemoryStream.Create;
  try
  while AStream.Position < AStream.Size do
  begin
    tiStreamReadToNextToken(AStream, FBlockDelimeter + CrLf, LStream);
    ProcessBlock(LStream);
  end;
  finally
    LStream.Free;
  end;
end;

procedure TtiMIMEMultipartFormDataDecoder.GetBlockDelimeter(const AStream: TStream);
begin
  AStream.Position:= 0;
  FBlockDelimeter:= tiStreamReadToNextToken(AStream, CrLf);
end;

procedure TtiMIMEMultipartFormDataDecoder.ProcessBlock(const AStream: TStream);
var
  LStream: TMemoryStream;
begin
  AStream.Position:= 0;
  LStream:=TMemoryStream.Create;
  try
    LStream.Size:= 0;
    tiStreamReadToNextToken(AStream, FBlockDelimeter, LStream);
    FItemList.AddInstance(LStream);
  finally
    LStream.Free;
  end;
end;

{ TtMIMEiMultipartFormDataItem }

constructor TtiMIMEMultipartFormDataItem.Create;
begin
  inherited;
  FRawData:= TMemoryStream.Create;
  FData:= TMemoryStream.Create;
end;

destructor TtiMIMEMultipartFormDataItem.Destroy;
begin
  FRawData.Free;
  FData.Free;
  inherited;
end;

function TtiMIMEMultipartFormDataItem.GetDataAsString: string;
begin
  result:= tiStreamToString(Data);
end;

function TtiMIMEMultipartFormDataItem.GetParent: TtiMIMEMultipartFormDataItemList;
begin
  result:= inherited GetParent as TtiMIMEMultipartFormDataItemList;
end;

procedure TtiMIMEMultipartFormDataItem.SetData(const AValue: TStream);
begin
  FData.Size:= 0;
  AValue.Position:= 0;
  FData.CopyFrom(AValue, AValue.Size);
end;

procedure TtiMIMEMultipartFormDataItem.SetRawData(const AValue: TStream);
begin
  FRawData.Size:= 0;
  AValue.Position:= 0;
  FRawData.CopyFrom(AValue, AValue.Size);
end;

{ TtMIMEiMultipartFormDataItemList }

procedure TtiMIMEMultipartFormDataItemList.Add(
  const AObject: TtiMIMEMultipartFormDataItem);
begin
  inherited Add(AObject);
end;

procedure TtiMIMEMultipartFormDataItemList.AddInstance(const AData: TStream);
var
  LItem: TtiMIMEMultipartFormDataItem;
  LFieldName: string;
  LFileName: string;
  LFieldData: TStream;
begin
  LFieldData:= TMemoryStream.Create;
  try
    DeriveFieldValues(AData, LFieldName, LFileName, LFieldData);
    LItem:= TtiMIMEMultipartFormDataItem.Create;
    Add(LItem);
    LItem.RawData:= AData;
    LItem.Data:= LFieldData;
    LItem.FieldName:= LFieldName;
    LItem.FileName:= LFileName;
  finally
    LFieldData.Free;
  end;

end;

procedure TtiMIMEMultipartFormDataItemList.DeriveFieldMetaData(
  const ALine: string;
  out  AFieldName, AFileName: string);
var
  LSL: TStringList;
begin
  LSL:= TStringList.Create;
  try
    LSL.QuoteChar:= '"';
    LSL.LineBreak:= '; ';
    LSL.Text:= ALine;
    AFieldName:= AnsiDequotedStr(LSL.Values['name'], '"');
    AFileName:= AnsiDequotedStr(LSL.Values['filename'], '"');
  finally
    LSL.Free;
  end;

end;

procedure TtiMIMEMultipartFormDataItemList.DeriveFieldValues(
  const AData: TStream;
  out  AFieldName, AFileName: string;
  const AFieldData: TStream);
var
  LLine1: string;
  LLine2: string;
begin
  AData.Position:= 0;
  LLine1:= (tiStreamReadToNextToken(AData, CrLf));
  LLine2:= (tiStreamReadToNextToken(AData, CrLf));
  if Trim(LLine2) <> '' then
    tiStreamReadToNextToken(AData, CrLf);
  DeriveFieldMetaData(LLine1, AFieldName, AFileName);
  AFieldData.CopyFrom(AData, AData.Size - AData.Position - 2);
end;

function TtiMIMEMultipartFormDataItemList.GetItems(
  i: integer): TtiMIMEMultipartFormDataItem;
begin
  result:= inherited GetItems(i) as TtiMIMEMultipartFormDataItem;
end;

procedure TtiMIMEMultipartFormDataItemList.SetItems(i: integer;
  const AValue: TtiMIMEMultipartFormDataItem);
begin
  inherited SetItems(i, AValue);
end;

end.
