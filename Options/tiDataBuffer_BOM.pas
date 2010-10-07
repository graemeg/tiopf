unit tiDataBuffer_BOM;

{$I tiDefines.inc}

interface
uses
   Classes
  ,tiConstants
  ,tiBaseObject
  ,tiQuery
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,Contnrs
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}
 ;

const
  cErrorTIDataSetCellMetaData = CTIErrorInternal + 'tiDataBuffer metadata and data are out of sync';
  cErrorDataConversion = 'Data conversion error. Can not convert <%s> to a %s';
  cErrorDataMetaDataMisMatch = 'Mismatch between data and metadata';

type

//  Write, then DUnit a CSV to tiDataBuffer reader
//  Derive the field widths and data types
//  Add a ColNamesInFirstRow prop
//  Write the tiQueryCSV class

  TtiDataBufferList   = class;
  TtiDataBuffer       = class;
  TtiDataBufferRow    = class;
  TtiDataBufferCell   = class;

  TtiDataBufferList = class(TtiBaseObject)
  private
    FList : TObjectList;
    function  GetItems(AIndex: Integer): TtiDataBuffer;
    procedure SetItems(AIndex: Integer; const AValue: TtiDataBuffer);
  public
    constructor Create;
    destructor  Destroy; override;
    property    Items[AIndex:Integer]:TtiDataBuffer read GetItems write SetItems;
    procedure   Add(const AData : TtiDataBuffer);
    function    AddInstance(const AName : string = ''): TtiDataBuffer;
    procedure   Clear;
    function    Count : integer;
    function    FindByName(const AName : string): TtiDataBuffer;
    function    FindCell(const ADataBufferName: string;
        const AFieldName: string; const ARowIndex: Integer): TtiDataBufferCell;
    procedure   Remove(const AValue : TtiDataBuffer);
    procedure   Extract(const AValue: TtiDataBuffer);
  end;


  TtiDataBuffer = class(TtiBaseObject)
  private
    FFields: TtiDBMetaDataTable;
    FRows : TObjectList;
    FName: string;
    function GetRows: TList;
  protected
    function  GetItems(AIndex: integer): TtiDataBufferRow;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    property    Items[ AIndex : integer ]: TtiDataBufferRow read GetItems;
    function    Count : integer;
    procedure   Add(const AValue : TtiDataBufferRow);
    function    AddInstance : TtiDataBufferRow;
    property    List : TList read GetRows;
    function    IndexOf(const AValue : TtiDataBufferRow): integer;
    function    FindByFieldValue(const AFieldName, AFieldValue: string): TtiDataBufferRow;
    function    FindCell(const AFieldName: string; const ARowIndex: Integer): TtiDataBufferCell;
    property    Name : string read FName write FName;
    procedure   Remove(const pRow : TtiDataBufferRow);
    procedure   Delete(const AIndex: Integer);
    property    Fields : TtiDBMetaDataTable read FFields write FFields;
  end;


  TtiDataBufferRow = class(TtiBaseObject)
  private
    FList : TObjectList;
    FOwner : TtiDataBuffer;
  protected
    function  GetItems(AIndex: integer): TtiDataBufferCell;
    procedure SetItems(AIndex: integer; const AValue: TtiDataBufferCell);
    function  GetOwner: TtiDataBuffer; reintroduce;
    procedure SetOwner(const AValue: TtiDataBuffer);
    function  GetIndex : integer;
  public
    constructor Create;
    destructor  Destroy; override;
    property    Items[ AIndex : integer ]: TtiDataBufferCell read GetItems write SetItems;
    property    Owner : TtiDataBuffer read GetOwner      write SetOwner;
    procedure   Add(const AValue : TtiDataBufferCell);
    function    AddInstance : TtiDataBufferCell;
    function    IndexOf(const AValue : TtiDataBufferCell): integer;
    function    Count : integer;
    property    Index : integer read GetIndex;
    function    FindByFieldName(const AName : string): TtiDataBufferCell;
  end;


  TtiDataBufferCell = class(TtiBaseObject)
  private
    FOwner : TtiDataBufferRow;
    FValue : String;
    function  GetDataSetField: TtiDBMetaDataField;
    function  GetName: string;
    function  GetValueAsBool: Boolean;
    function  GetValueAsDateTime: TDateTime;
    function  GetValueAsFloat: Extended;
    function  GetValueAsInt: Int64;
    procedure SetValueAsBool(const AValue: Boolean);
    procedure SetValueAsDateTime(const AValue: TDateTime);
    procedure SetValueAsFloat(const AValue: Extended);
    procedure SetValueAsInt(const AValue: Int64);
  protected
    function  GetIndex : integer;
  public
    property  Owner : TtiDataBufferRow read FOwner write FOwner;
    property  DataSetField : TtiDBMetaDataField read GetDataSetField;
    property  Index : integer read GetIndex;
    property  ValueAsString : string read FValue write FValue;
    property  ValueAsInteger : Int64 read GetValueAsInt Write SetValueAsInt;
    property  ValueAsDateTime : TDateTime read GetValueAsDateTime Write SetValueAsDateTime;
    property  ValueAsBool : Boolean read GetValueAsBool Write SetValueAsBool;
    property  ValueAsFloat: Extended read GetValueAsFloat Write SetValueAsFloat;
    procedure AssignToStream(AStream: TStream);
    procedure AssignFromStream(AStream: TStream);
    property  Name : string read GetName;
  end;


implementation
uses
   tiUtils
  ,tiStreams
  ,tiExcept
  ,SysUtils
 ;

{ TtiDataBufferList }

procedure TtiDataBufferList.Add(const AData: TtiDataBuffer);
begin
  FList.Add(AData);
end;

function TtiDataBufferList.AddInstance(const AName: string): TtiDataBuffer;
begin
  result := TtiDataBuffer.Create;
  result.Name := AName;
  Add(result);
end;

procedure TtiDataBufferList.Clear;
begin
  FList.Clear;
end;

function TtiDataBufferList.Count: integer;
begin
  result := FList.Count;
end;

constructor TtiDataBufferList.Create;
begin
  inherited;
  FList := TObjectList.Create(true);
end;

destructor TtiDataBufferList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TtiDataBufferList.Extract(const AValue: TtiDataBuffer);
begin
  FList.Extract(AValue);
  FList.Capacity:= FList.Count; // To suppress a reported leak in testing
end;

function TtiDataBufferList.FindByName(const AName: string): TtiDataBuffer;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, AName) then
    begin
      result := Items[i];
      Break; //==>
    end;
end;

function TtiDataBufferList.FindCell(const ADataBufferName: string;
  const AFieldName: string; const ARowIndex: Integer): TtiDataBufferCell;
var
  LDataBuffer: TtiDataBuffer;
begin
  LDataBuffer := FindByName(ADataBufferName);
  if Assigned(LDataBuffer) then
    Result := LDataBuffer.FindCell(AFieldName, ARowIndex)
  else
    Result := nil;
end;

function TtiDataBufferList.GetItems(AIndex: Integer): TtiDataBuffer;
begin
  result := TtiDataBuffer(FList.Items[AIndex])
end;

procedure TtiDataBufferList.Remove(const AValue: TtiDataBuffer);
begin
  FList.Remove(AValue);
  FList.Capacity:= FList.Count; // To suppress a reported leak in testing
end;

procedure TtiDataBufferList.SetItems(AIndex: Integer; const AValue: TtiDataBuffer);
begin
  FList.Items[AIndex]:=AValue;
end;

{ TtiDataBuffer }

procedure TtiDataBuffer.Add(const AValue: TtiDataBufferRow);
begin
  FRows.Add(AValue);
  AValue.Owner := Self;
end;

function TtiDataBuffer.AddInstance: TtiDataBufferRow;
var
  i : integer;
begin
  result := TtiDataBufferRow.Create;
  Add(Result);
  for i := 0 to Fields.Count - 1 do
    Result.AddInstance;
end;

procedure TtiDataBuffer.Clear;
begin
  FFields.Clear;
  FRows.Clear;
end;

function TtiDataBuffer.Count: integer;
begin
  result := FRows.Count;
end;

constructor TtiDataBuffer.Create;
begin
  inherited;
  FRows  := TObjectList.Create(true);
  FFields := TtiDBMetaDataTable.Create;
end;

destructor TtiDataBuffer.Destroy;
begin
  FRows.Free;
  FFields.Free;
  inherited;
end;

function TtiDataBuffer.GetItems(AIndex: integer): TtiDataBufferRow;
begin
  result := TtiDataBufferRow(FRows.Items[AIndex]);
end;

function TtiDataBuffer.GetRows: TList;
begin
  result := FRows;
end;

function TtiDataBuffer.IndexOf(const AValue: TtiDataBufferRow): integer;
begin
  result := FRows.IndexOf(AValue);
end;

function TtiDataBuffer.FindByFieldValue(const AFieldName, AFieldValue: string): TtiDataBufferRow;
var
  LFieldIndex: Integer;
  I: Integer;
begin
  Result := nil;

  LFieldIndex := Fields.IndexOfFieldName(AFieldName);
  if LFieldIndex >= 0 then
  begin
    for I := 0 to Pred(Count) do
      if Items[I].Items[LFieldIndex].ValueAsString = AFieldValue then
      begin
        Result := Items[I];
        Break; //==>
      end;
  end;
end;

function TtiDataBuffer.FindCell(const AFieldName: string;
  const ARowIndex: Integer): TtiDataBufferCell;
begin
  if (ARowIndex >= 0) and (ARowIndex < Count) then
    Result := Items[ARowIndex].FindByFieldName(AFieldName)
  else
    Result := nil;
end;

procedure TtiDataBuffer.Remove(const pRow: TtiDataBufferRow);
begin
  FRows.Remove(pRow);
end;

procedure TtiDataBuffer.Delete(const AIndex: Integer);
begin
  FRows.Delete(AIndex);
end;

{ TtiDataBufferRow }

procedure TtiDataBufferRow.Add(const AValue: TtiDataBufferCell);
begin
  Assert(Owner.TestValid(TtiDataBuffer), CTIErrorInvalidObject);
  if (Owner.Fields.Count > 0) and
     (Count = Owner.Fields.Count) then
    raise EtiOPFProgrammerException.Create(cErrorDataMetaDataMisMatch);
  FList.Add(AValue);
  AValue.Owner := self;
end;

function TtiDataBufferRow.AddInstance: TtiDataBufferCell;
begin
  result := TtiDataBufferCell.Create;
  Add(result);
end;

function TtiDataBufferRow.Count: integer;
begin
  result := FList.Count;
end;

constructor TtiDataBufferRow.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TtiDataBufferRow.Destroy;
begin
  FList.Free;
  inherited;
end;

function TtiDataBufferRow.FindByFieldName(const AName: string): TtiDataBufferCell;
var
  lField : TtiDBMetaDataField;
  lIndex : integer;
begin
  result := nil;
  lField := Owner.Fields.FindByFieldName(AName);
  if lField = nil then
    Exit; //==>
  lIndex := lField.Index;
  Assert(lIndex < Count, 'Columns and metat data out of sync.');
  result := Items[lIndex];
end;

function TtiDataBufferRow.GetIndex: integer;
begin
  Assert(Owner.TestValid, CTIErrorInvalidObject);
  result := Owner.IndexOf(Self);
end;

function TtiDataBufferRow.GetItems(AIndex: integer): TtiDataBufferCell;
begin
  result := TtiDataBufferCell(FList.Items[AIndex]);
end;

function TtiDataBufferRow.GetOwner: TtiDataBuffer;
begin
  result := FOwner;
end;

procedure TtiDataBufferRow.SetOwner(const AValue: TtiDataBuffer);
begin
  FOwner := AValue;
end;

function TtiDataBufferRow.IndexOf(const AValue: TtiDataBufferCell): integer;
begin
  result := FList.IndexOf(AValue);
end;

procedure TtiDataBufferRow.SetItems(AIndex: integer; const AValue: TtiDataBufferCell);
begin
  FList.Items[AIndex]:= AValue;
end;

{ TtiDataBufferCell }

function  TtiDataBufferCell.GetIndex : integer;
begin
  Assert(Owner.TestValid, CTIErrorInvalidObject);
  result := Owner.IndexOf(Self);
end;

function TtiDataBufferCell.GetName: string;
var
  lMetaData : TtiDBMetaDataField;
begin
  lMetaData := Owner.Owner.Fields.Items[Index];
  result := lMetaData.Name;
end;

function TtiDataBufferCell.GetValueAsBool: Boolean;
begin
  try
    Result := tiStrToBool(FValue);
  except
    on e:Exception do
      raise Exception.CreateFmt(cErrorDataConversion, [FValue,'Boolean']);
  end;
end;

function TtiDataBufferCell.GetValueAsDateTime: TDateTime;
begin
  try
    Result := tiXMLStringToDateTime(FValue);
  except
    on e:Exception do
      raise Exception.CreateFmt(cErrorDataConversion, [FValue,'TDateTime']);
  end;
end;

function TtiDataBufferCell.GetValueAsFloat: Extended;
begin
  try
    Result := StrToFloatDef(FValue, 0);
  except
    on e:Exception do
      raise Exception.CreateFmt(cErrorDataConversion, [FValue,'Float']);
  end;
end;

function TtiDataBufferCell.GetValueAsInt: Int64;
begin
  try
    Result := StrToInt64Def(FValue, 0);
  except
    on e:Exception do
      raise Exception.CreateFmt(cErrorDataConversion, [FValue,'Integer']);
  end;
end;

procedure TtiDataBufferCell.SetValueAsBool(const AValue: Boolean);
begin
  FValue := tiBooleanToStr(AValue);
end;

procedure TtiDataBufferCell.SetValueAsDateTime(const AValue: TDateTime);
begin
  FValue := tiDateTimeAsXMLString(AValue);
end;

procedure TtiDataBufferCell.SetValueAsFloat(const AValue: Extended);
begin
  FValue:= FloatToStr(AValue);
end;

procedure TtiDataBufferCell.SetValueAsInt(const AValue: Int64);
begin
  FValue := IntToStr(AValue);
end;

procedure TtiDataBufferCell.AssignFromStream(AStream: TStream);
var
  LStream: TMemoryStream;
  LPos : integer;
begin
  Assert(AStream<>nil, 'AStream not assigned');

  // Mime encode stream directly as it may contain binary data.
  LStream := TMemoryStream.Create;
  try
    LPos := AStream.Position;
    AStream.Position := 0;
    MimeEncodeStreamNoCRLF(AStream, LStream);
    FValue := tiStreamToString(LStream);
    AStream.Position := LPos;
  finally
    LStream.Free;
  end;
end;

procedure TtiDataBufferCell.AssignToStream(AStream: TStream);
var
  LStream: TMemoryStream;
begin
  Assert(AStream<>nil, 'AStream not assigned');

  // Mime decode stream directly as output may contain binary data.
  LStream := TMemoryStream.Create;
  try
    tiStringToStream(FValue, LStream);
    LStream.Position := 0;
    AStream.Size := 0;
    MimeDecodeStream(LStream, AStream);
    AStream.Position := 0;
  finally
    LStream.Free;
  end;
end;

function TtiDataBufferCell.GetDataSetField: TtiDBMetaDataField;
begin
  Assert(Owner.TestValid,       CTIErrorInvalidObject);
  Assert(Owner.Owner.TestValid, CTIErrorInvalidObject);
  if Owner.Count <> Owner.Owner.Fields.Count then
    raise exception.Create(cErrorTIDataSetCellMetaData);
  result := Owner.Owner.Fields.Items[Index];
end;

end.

