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
  cErrorTIDataSetCellMetaData = cTIInternalError + 'tiDataSet metadata and data are out of sync' ;
  cErrorDataConversion = 'Data conversion error. Can not convert <%s> to a %s';
  cErrorDataMetaDataMisMatch = 'Mismatch between data and metadata';

type

//  Write, then DUnit a CSV to tiDataSet reader
//  Derive the field widths and data types
//  Add a ColNamesInFirstRow prop
//  Write the tiQueryCSV class

  TtiDataBuffers      = class ;
  TtiDataBuffer       = class ;
  TtiDataBufferRow    = class ;
  TtiDataBufferCell   = class ;

  TtiDataBuffers = class( TtiBaseObject )
  private
    FList : TObjectList ;
    function  GetItems(pIndex: Integer): TtiDataBuffer;
    procedure SetItems(pIndex: Integer; const Value: TtiDataBuffer);
  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    Items[pIndex:Integer]:TtiDataBuffer read GetItems write SetItems ;
    procedure   Add(const pData : TtiDataBuffer);
    function    AddInstance(const pName : string = '' ) : TtiDataBuffer ;
    procedure   Clear;
    function    Count : integer ;
    function    FindByName(const pName : string) : TtiDataBuffer ;
    procedure   Remove(const pValue : TtiDataBuffer);
    procedure   Extract(const pValue: TtiDataBuffer);
  end ;


  TtiDataBuffer = class( TtiBaseObject )
  private
    FFields: TtiDBMetaDataTable;
    FRows  : TObjectList ;
    FName: string;
    function GetRows: TList;
  protected
    function  GetItems(pIndex: integer): TtiDataBufferRow;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    procedure   Clear ;
    property    Items[ pIndex : integer ] : TtiDataBufferRow read GetItems ;
    function    Count : integer ;
    procedure   Add( const pValue : TtiDataBufferRow ) ;
    function    AddInstance : TtiDataBufferRow ;
    property    List : TList read GetRows ;
    function    IndexOf(const pValue : TtiDataBufferRow ) : integer ;
    property    Name : string read FName write FName ;
    procedure   Remove(const pRow : TtiDataBufferRow);
    property    Fields : TtiDBMetaDataTable read FFields write FFields ;
  end ;


  TtiDataBufferRow = class( TtiBaseObject )
  private
    FList : TObjectList ;
    FOwner : TtiDataBuffer ;
  protected
    function  GetItems(pIndex: integer): TtiDataBufferCell;
    procedure SetItems(pIndex: integer; const Value: TtiDataBufferCell);
    function  GetOwner: TtiDataBuffer; reintroduce ;
    procedure SetOwner(const Value: TtiDataBuffer);
    function  GetIndex : integer ;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    Items[ pIndex : integer ] : TtiDataBufferCell read GetItems write SetItems ;
    property    Owner : TtiDataBuffer read GetOwner      write SetOwner ;
    procedure   Add( const pValue : TtiDataBufferCell ) ;
    function    AddInstance : TtiDataBufferCell ;
    function    IndexOf(const pValue : TtiDataBufferCell ) : integer ;
    function    Count : integer ;
    property    Index : integer read GetIndex ;
    function    FindByFieldName(const pName : string): TtiDataBufferCell ;
  end ;


  TtiDataBufferCell = class( TtiBaseObject )
  private
    FOwner : TtiDataBufferRow ;
    FValue : String ;
    function  GetDataSetField: TtiDBMetaDataField;
    function  GetName: string;
    function  GetValueAsBool: Boolean;
    function  GetValueAsDateTime: TDateTime;
    function  GetValueAsFloat: real;
    function  GetValueAsInt: Int64;
    procedure SetValueAsBool(const Value: Boolean);
    procedure SetValueAsDateTime(const Value: TDateTime);
    procedure SetValueAsFloat(const Value: real);
    procedure SetValueAsInt(const Value: Int64);
  protected
    function  GetIndex : integer ;
  public
    property  Owner : TtiDataBufferRow read FOwner write FOwner ;
    property  DataSetField : TtiDBMetaDataField read GetDataSetField ;
    property  Index : integer read GetIndex ;
    property  ValueAsString : string read FValue write FValue ;
    property  ValueAsInteger : Int64 read GetValueAsInt Write SetValueAsInt ;
    property  ValueAsDateTime : TDateTime read GetValueAsDateTime Write SetValueAsDateTime;
    property  ValueAsBool : Boolean read GetValueAsBool Write SetValueAsBool ;
    property  ValueAsFloat: real read GetValueAsFloat Write SetValueAsFloat;
    procedure AssignToStream(pStream: TStream);
    procedure AssignFromStream(pStream: TStream);
    property  Name : string read GetName ;
  end ;


implementation
uses
   tiUtils
  ,tiXML
  ,tiStreams
  ,tiExcept
  ,SysUtils
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDataBuffer
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDataBuffer.Add(const pValue: TtiDataBufferRow);
begin
  FRows.Add(pValue);
  pValue.Owner := Self ;
end;

function TtiDataBuffer.AddInstance: TtiDataBufferRow;
var
  i : integer ;
begin
  result := TtiDataBufferRow.Create;
  Add(Result);
  for i := 0 to Fields.Count - 1 do
    Result.AddInstance ;
end;

procedure TtiDataBuffer.Clear;
begin
  FFields.Clear ;
  FRows.Clear ;
end;

function TtiDataBuffer.Count: integer;
begin
  result := FRows.Count ;
end;

constructor TtiDataBuffer.Create;
begin
  inherited;
  FRows   := TObjectList.Create(true) ;
  FFields := TtiDBMetaDataTable.Create ;
end;

destructor TtiDataBuffer.Destroy;
begin
  FRows.Free ;
  FFields.Free ;
  inherited;
end;

function TtiDataBuffer.GetItems(pIndex: integer): TtiDataBufferRow;
begin
  result := TtiDataBufferRow(FRows.Items[pIndex]);
end;

procedure TtiDataBufferRow.Add(const pValue: TtiDataBufferCell);
begin
  Assert( Owner.TestValid(TtiDataBuffer), cTIInvalidObjectError );
  if ( Owner.Fields.Count > 0 ) and
     ( Count = Owner.Fields.Count ) then
    raise EtiOPFProgrammerException.Create(cErrorDataMetaDataMisMatch);
  FList.Add(pValue);
  pValue.Owner := self ;
end;

function TtiDataBufferRow.AddInstance: TtiDataBufferCell;
begin
  result := TtiDataBufferCell.Create;
  Add(result);
end;

function TtiDataBufferRow.Count: integer;
begin
  result := FList.Count ;
end;

constructor TtiDataBufferRow.Create;
begin
  inherited ;
  FList := TObjectList.Create ;
end;

destructor TtiDataBufferRow.Destroy;
begin
  FList.Free ;
  inherited;
end;

function TtiDataBufferRow.FindByFieldName(const pName: string): TtiDataBufferCell;
var
  lField : TtiDBMetaDataField;
  lIndex : integer ;
begin
  result := nil ;
  lField := Owner.Fields.FindByFieldName(pName);
  if lField = nil then
    Exit ; //==>
  lIndex := lField.Index;
  Assert(lIndex < Count, 'Columns and metat data out of sync.') ;
  result := Items[lIndex];
end;

function TtiDataBufferRow.GetIndex: integer;
begin
  Assert( Owner.TestValid, cTIInvalidObjectError );
  result := Owner.IndexOf(Self);
end;

function TtiDataBufferRow.GetItems(pIndex: integer): TtiDataBufferCell;
begin
  result := TtiDataBufferCell(FList.Items[pIndex]) ;
end;

function TtiDataBufferRow.GetOwner: TtiDataBuffer;
begin
  result := FOwner ;
end;

function TtiDataBufferRow.IndexOf(const pValue: TtiDataBufferCell): integer;
begin
  result := FList.IndexOf(pValue);
end;

procedure TtiDataBufferRow.SetItems(pIndex: integer; const Value: TtiDataBufferCell);
begin
  FList.Items[pIndex] := Value ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDataBufferCell
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDataBufferCell.AssignFromStream(pStream: TStream);
var
  lValue : string ;
begin
  Assert(pStream<>nil, 'pStream not assigned');
  lValue := tiStreamToString(pStream);
  FValue := MimeEncodeString(lValue);
end;

procedure TtiDataBufferCell.AssignToStream(pStream: TStream);
var
  lValue : string ;
begin
  Assert(pStream<>nil, 'pStream not assigned');
  lValue := MimeDecodeString(FValue);
  tiStringToStream(lValue,pStream);
end;

function TtiDataBufferCell.GetDataSetField: TtiDBMetaDataField;
begin
  Assert( Owner.TestValid,       cTIInvalidObjectError );
  Assert( Owner.Owner.TestValid, cTIInvalidObjectError );
  if Owner.Count <> Owner.Owner.Fields.Count then
    raise exception.Create(cErrorTIDataSetCellMetaData);
  result := Owner.Owner.Fields.Items[Index] ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDataBufferField
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function  TtiDataBufferCell.GetIndex : integer ;
begin
  Assert( Owner.TestValid, cTIInvalidObjectError );
  result := Owner.IndexOf(Self);
end;

procedure TtiDataBufferRow.SetOwner(const Value: TtiDataBuffer);
begin
  FOwner := Value ;
end;

function TtiDataBuffer.GetRows: TList;
begin
  result := FRows ;
end;

function TtiDataBuffer.IndexOf(const pValue: TtiDataBufferRow): integer;
begin
  result := FRows.IndexOf(pValue);
end;

{ TtiDataBuffers }

procedure TtiDataBuffers.Add(const pData: TtiDataBuffer);
begin
  FList.Add(pData);
end;

function TtiDataBuffers.AddInstance(const pName: string): TtiDataBuffer;
begin
  result := TtiDataBuffer.Create ;
  result.Name := pName ;
  Add(result);
end;

procedure TtiDataBuffers.Clear;
begin
  FList.Clear ;
end;

function TtiDataBuffers.Count: integer;
begin
  result := FList.Count ;
end;

constructor TtiDataBuffers.Create;
begin
  inherited ;
  FList := TObjectList.Create(true) ;
end;

destructor TtiDataBuffers.Destroy;
begin
  FList.Free;
  inherited ;
end;

procedure TtiDataBuffers.Extract(const pValue: TtiDataBuffer);
begin
  FList.Extract(pValue);
end;

function TtiDataBuffers.FindByName(const pName: string): TtiDataBuffer;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].Name, pName ) then
    begin
      result := Items[i];
      Break ; //==>
    end;
end;

function TtiDataBuffers.GetItems(pIndex: Integer): TtiDataBuffer;
begin
  result := TtiDataBuffer(FList.Items[pIndex])
end;

procedure TtiDataBuffers.Remove(const pValue: TtiDataBuffer);
begin
  FList.Remove(pValue);
end;

procedure TtiDataBuffers.SetItems(pIndex: Integer; const Value: TtiDataBuffer);
begin
  FList.Items[pIndex]:=Value;
end;

procedure TtiDataBuffer.Remove(const pRow: TtiDataBufferRow);
begin
  FRows.Remove(pRow);
end;

function TtiDataBufferCell.GetName: string;
var
  lMetaData : TtiDBMetaDataField ;
begin
  lMetaData := Owner.Owner.Fields.Items[Index] ;
  result := lMetaData.Name ;
end;

function TtiDataBufferCell.GetValueAsBool: Boolean;
begin
  try
    Result := tiStrToBool(FValue);
  except
    on e:Exception do
      raise Exception.CreateFmt( cErrorDataConversion, [FValue,'Boolean']);
  end ;
end;

function TtiDataBufferCell.GetValueAsDateTime: TDateTime;
begin
  try
    Result := tiXMLStringToDateTime(FValue);
  except
    on e:Exception do
      raise Exception.CreateFmt( cErrorDataConversion, [FValue,'TDateTime']);
  end ;
end;

function TtiDataBufferCell.GetValueAsFloat: real;
begin
  try
    Result := StrToFloatDef(FValue, 0);
  except
    on e:Exception do
      raise Exception.CreateFmt( cErrorDataConversion, [FValue,'Float']);
  end ;
end;

function TtiDataBufferCell.GetValueAsInt: Int64;
begin
  try
    Result := StrToInt64Def(FValue, 0);
  except
    on e:Exception do
      raise Exception.CreateFmt( cErrorDataConversion, [FValue,'Integer']);
  end ;
end;

procedure TtiDataBufferCell.SetValueAsBool(const Value: Boolean);
begin
  FValue := tiBooleanToStr(Value);
end;

procedure TtiDataBufferCell.SetValueAsDateTime(const Value: TDateTime);
begin
  FValue := tiDateTimeAsXMLString(Value);
end;

procedure TtiDataBufferCell.SetValueAsFloat(const Value: real);
begin
  FValue:= FloatToStr(Value);
end;

procedure TtiDataBufferCell.SetValueAsInt(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

end.

