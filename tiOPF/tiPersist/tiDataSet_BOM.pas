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

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit tiDataSet_BOM;

interface
uses
   Classes
  ,ctiPersist
  ,tiObjAbs
  ,tiQuery
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,Contnrs
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}
  ;

const
  cErrorTIDataSetCellMetaData = cTIInternalError + 'tiDataSet metadata and data are out of sync' ;
  cErrorDataConversion = 'Data conversion error. Can not convert <%s> to a %s';

type

//  Write, then DUnit a CSV to tiDataSet reader
//  Derive the field widths and data types
//  Add a ColNamesInFirstRow prop
//  Write the tiQueryCSV class

  TtiDataSets      = class ;
  TtiDataSet       = class ;
  TtiDataSetRow    = class ;
  TtiDataSetCell   = class ;

  TtiDataSets = class( TtiObjAbs )
  private
    FList : TObjectList ;
    function  GetItems(pIndex: Integer): TtiDataSet;
    procedure SetItems(pIndex: Integer; const Value: TtiDataSet);
  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    Items[pIndex:Integer]:TtiDataSet read GetItems write SetItems ;
    procedure   Add(const pData : TtiDataSet);
    function    AddInstance(const pName : string = '' ) : TtiDataSet ;
    procedure   Clear;
    function    Count : integer ;
    function    FindByName(const pName : string) : TtiDataSet ;
    procedure   Remove(const pValue : TtiDataSet);
    procedure   Extract(const pValue: TtiDataSet);
  end ;

  TtiDataSet = class( TtiObjAbs )
  private
    FFields: TtiDBMetaDataTable;
    FRows  : TObjectList ;
    FName: string;
    function GetRows: TList;
  protected
    function  GetItems(pIndex: integer): TtiDataSetRow;
  public
    constructor create ;
    destructor  destroy ; override ;
    procedure   Clear ;
    property    Items[ pIndex : integer ] : TtiDataSetRow read GetItems ;
    function    Count : integer ;
    procedure   Add( const pValue : TtiDataSetRow ) ;
    function    AddInstance : TtiDataSetRow ;
    property    List : TList read GetRows ;
    function    IndexOf(const pValue : TtiDataSetRow ) : integer ;
    property    Name : string read FName write FName ;
    procedure   Remove(const pRow : TtiDataSetRow);
  published
    property    Fields : TtiDBMetaDataTable read FFields write FFields ;
  end ;

  TtiDataSetRow = class( TtiObjAbs )
  private
    FList : TObjectList ;
    FOwner : TtiDataSet ;
  protected
    function  GetItems(pIndex: integer): TtiDataSetCell;
    procedure SetItems(pIndex: integer; const Value: TtiDataSetCell);
    function  GetOwner: TtiDataSet; reintroduce ;
    procedure SetOwner(const Value: TtiDataSet);
    function  GetIndex : integer ;
  public
    constructor Create ;
    destructor  Destroy ; override ;
    property    Items[ pIndex : integer ] : TtiDataSetCell read GetItems write SetItems ;
    property    Owner : TtiDataSet read GetOwner      write SetOwner ;
    procedure   Add( const pValue : TtiDataSetCell ) ;
    function    AddInstance : TtiDataSetCell ;
    function    IndexOf(const pValue : TtiDataSetCell ) : integer ;
    function    Count : integer ;
    property    Index : integer read GetIndex ;
    function    FindByFieldName(const pName : string): TtiDataSetCell ;
  end ;

  TtiDataSetCell = class( TtiObjAbs )
  private
    FOwner : TtiDataSetRow ;
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
    property  Owner : TtiDataSetRow read FOwner write FOwner ;
    property  DataSetField : TtiDBMetaDataField read GetDataSetField ;
    property  Index : integer read GetIndex ;
  published
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
  SysUtils
  ,tiUtils
  ,tiXML
  ,tiRJMime
  ;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDataSet
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDataSet.Add(const pValue: TtiDataSetRow);
begin
  FRows.Add(pValue);
  pValue.Owner := Self ;
end;

function TtiDataSet.AddInstance: TtiDataSetRow;
var
  i : integer ;
begin
  result := TtiDataSetRow.Create;
  Add(Result);
  for i := 0 to Fields.Count - 1 do
    Result.AddInstance ;
end;

procedure TtiDataSet.Clear;
begin
  FFields.Clear ;
  FRows.Clear ;
end;

function TtiDataSet.Count: integer;
begin
  result := FRows.Count ;
end;

constructor TtiDataSet.create;
begin
  inherited;
  FRows   := TObjectList.Create(true) ;
  FFields := TtiDBMetaDataTable.Create ;
end;

destructor TtiDataSet.destroy;
begin
  FRows.Free ;
  FFields.Free ;
  inherited;
end;

function TtiDataSet.GetItems(pIndex: integer): TtiDataSetRow;
begin
  result := TtiDataSetRow(FRows.Items[pIndex]);
end;

procedure TtiDataSetRow.Add(const pValue: TtiDataSetCell);
begin
  Assert( Owner.TestValid(TtiDataSet), cTIInvalidObjectError );
  if ( Owner.Fields.Count > 0 ) and
     ( Count = Owner.Fields.Count ) then
    tiFmtException( 'Attempt to add more column data than fields described in the MetaData',
                    ClassName, 'Add' ) ;
  FList.Add(pValue);
  pValue.Owner := self ;
end;

function TtiDataSetRow.AddInstance: TtiDataSetCell;
begin
  result := TtiDataSetCell.Create;
  Add(result);
end;

function TtiDataSetRow.Count: integer;
begin
  result := FList.Count ;
end;

constructor TtiDataSetRow.Create;
begin
  inherited ;
  FList := TObjectList.Create ;
end;

destructor TtiDataSetRow.Destroy;
begin
  FList.Free ;
  inherited;
end;

function TtiDataSetRow.FindByFieldName(const pName: string): TtiDataSetCell;
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

function TtiDataSetRow.GetIndex: integer;
begin
  Assert( Owner.TestValid, cTIInvalidObjectError );
  result := Owner.IndexOf(Self);
end;

function TtiDataSetRow.GetItems(pIndex: integer): TtiDataSetCell;
begin
  result := TtiDataSetCell(FList.Items[pIndex]) ;
end;

function TtiDataSetRow.GetOwner: TtiDataSet;
begin
  result := FOwner ;
end;

function TtiDataSetRow.IndexOf(const pValue: TtiDataSetCell): integer;
begin
  result := FList.IndexOf(pValue);
end;

procedure TtiDataSetRow.SetItems(pIndex: integer; const Value: TtiDataSetCell);
begin
  FList.Items[pIndex] := Value ;
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDataSetCell
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDataSetCell.AssignFromStream(pStream: TStream);
var
  lValue : string ;
begin
  Assert(pStream<>nil, 'pStream not assigned');
  lValue := tiStreamToString(pStream);
  FValue := MimeEncodeString(lValue);
end;

procedure TtiDataSetCell.AssignToStream(pStream: TStream);
var
  lValue : string ;
begin
  Assert(pStream<>nil, 'pStream not assigned');
  lValue := MimeDecodeString(FValue);
  tiStringToStream(lValue,pStream);
end;

function TtiDataSetCell.GetDataSetField: TtiDBMetaDataField;
begin
  Assert( Owner.TestValid,       cTIInvalidObjectError );
  Assert( Owner.Owner.TestValid, cTIInvalidObjectError );
  if Owner.Count <> Owner.Owner.Fields.Count then
    raise exception.create(cErrorTIDataSetCellMetaData);
  result := Owner.Owner.Fields.Items[Index] ;
end;

{
function TtiDataSetCell.GetValueAsStr: string;
var
  lDataSetFieldKind : TtiDBMetaDataField ;
begin
  lDataSetFieldKind := DataSetField ;
  Assert( lDataSetFieldKind.TestValid, cTIInvalidObjectError );
  // Add formatting here as required, perhaps we should have a display
  // mask property on the DataSetField object.
  case lDataSetFieldKind.Kind of
  qfkString     : result := VarToStr( Value ) ;
  qfkInteger    : result := VarToStr( Value ) ;
  qfkFloat      : result := VarToStr( Value ) ;
  qfkDateTime   : result := tiDateTimeToStr( Value ) ;
  qfkLogical    : begin
                    if Value then
                      result := cTrue
                    else
                      result := cFalse ;
                  end ;
  qfkLongString : result := VarToStr( Value ) ;
  qfkBinary     : result := VarToStr( Value ) ;
  else
    tiFmtException( 'Invalid DataSetField.FieldKind', ClassName, 'GetValueAsStr' ) ;
  end ;
//  qfkMacro,
end;
}

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TtiDataSetField
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

function  TtiDataSetCell.GetIndex : integer ;
begin
  Assert( Owner.TestValid, cTIInvalidObjectError );
  result := Owner.IndexOf(Self);
end;

procedure TtiDataSetRow.SetOwner(const Value: TtiDataSet);
begin
  FOwner := Value ;
end;

function TtiDataSet.GetRows: TList;
begin
  result := FRows ;
end;

function TtiDataSet.IndexOf(const pValue: TtiDataSetRow): integer;
begin
  result := FRows.IndexOf(pValue);
end;

{ TtiDataSets }

procedure TtiDataSets.Add(const pData: TtiDataSet);
begin
  FList.Add(pData);
end;

function TtiDataSets.AddInstance(const pName: string): TtiDataSet;
begin
  result := TtiDataSet.Create ;
  result.Name := pName ;
  Add(result);
end;

procedure TtiDataSets.Clear;
begin
  FList.Clear ;
end;

function TtiDataSets.Count: integer;
begin
  result := FList.Count ;
end;

constructor TtiDataSets.Create;
begin
  inherited ;
  FList := TObjectList.Create(true) ;
end;

destructor TtiDataSets.Destroy;
begin
  FList.Free;
  inherited ;
end;

procedure TtiDataSets.Extract(const pValue: TtiDataSet);
begin
  FList.Extract(pValue);
end;

function TtiDataSets.FindByName(const pName: string): TtiDataSet;
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

function TtiDataSets.GetItems(pIndex: Integer): TtiDataSet;
begin
  result := TtiDataSet(FList.Items[pIndex])
end;

procedure TtiDataSets.Remove(const pValue: TtiDataSet);
begin
  FList.Remove(pValue);
end;

procedure TtiDataSets.SetItems(pIndex: Integer; const Value: TtiDataSet);
begin
  FList.Items[pIndex]:=Value;
end;

procedure TtiDataSet.Remove(const pRow: TtiDataSetRow);
begin
  FRows.Remove(pRow);
end;

function TtiDataSetCell.GetName: string;
var
  lMetaData : TtiDBMetaDataField ;
begin
  lMetaData := Owner.Owner.Fields.Items[Index] ;
  result := lMetaData.Name ;
end;

function TtiDataSetCell.GetValueAsBool: Boolean;
begin
  try
    Result := tiStrToBool(FValue);
  except
    on e:Exception do
      raise Exception.CreateFmt( cErrorDataConversion, [FValue,'Boolean']);
  end ;
end;

function TtiDataSetCell.GetValueAsDateTime: TDateTime;
begin
  try
    Result := tiXMLStringToDateTime(FValue);
  except
    on e:Exception do
      raise Exception.CreateFmt( cErrorDataConversion, [FValue,'TDateTime']);
  end ;
end;

function TtiDataSetCell.GetValueAsFloat: real;
begin
  try
    if FValue <> '' then
      Result := StrToFloat(FValue)
    else
      Result := 0 ;
  except
    on e:Exception do
      raise Exception.CreateFmt( cErrorDataConversion, [FValue,'Float']);
  end ;
end;

function TtiDataSetCell.GetValueAsInt: Int64;
begin
  try
    Result := StrToInt64Def(FValue, 0);
  except
    on e:Exception do
      raise Exception.CreateFmt( cErrorDataConversion, [FValue,'Integer']);
  end ;
end;

procedure TtiDataSetCell.SetValueAsBool(const Value: Boolean);
begin
  FValue := tiBooleanToStr(Value);
end;

procedure TtiDataSetCell.SetValueAsDateTime(const Value: TDateTime);
begin
  FValue := tiDateTimeAsXMLString(Value);
end;

procedure TtiDataSetCell.SetValueAsFloat(const Value: real);
begin
  FValue:= FloatToStr(Value);
end;

procedure TtiDataSetCell.SetValueAsInt(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

end.

