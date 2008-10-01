{
  This unit implements a base class for all tiQuery descendents that have a
  TDataset they work with, and which use TParams. It handles all field/
  parameter handling.

  Initial Author:  Michael Van Canneyt (michael@freepascal.org) - Aug 2008
}

unit tiQueryDataset;

{$I tiDefines.inc}

{ For debug purposes only }
{.$DEFINE LOGQUERYDATASET}

interface

uses
  Classes,
  SysUtils,
  tiQuery,
  DB;

type
  {: Abstract class for any persistence layers that use TDataset descendants.
     Which includes almost all of them. }
  TtiQueryDataset = class(TtiQuerySQL)
  private
    FDataset: TDataset;
    FParams: TParams;
    procedure SetDataset(const AValue: TDataset);
    procedure SetParams(const AValue: TParams);
  protected
    property Dataset: TDataset read FDataset write SetDataset;
    property Params: TParams read FParams write SetParams;

    // General dataset methods.
    function GetActive: Boolean; override;
    function GetEOF: Boolean; override;
    procedure Open; override;
    procedure Close; override;
    procedure Next; override;

    // Called before parameters are accessed.
    procedure CheckPrepared; virtual;

    // Field Overrides
    function FieldCount: integer; override;
    function FieldName(AIndex: integer): string; override;
    function FieldIndex(const AName: string): integer; override;
    function FieldKind(AIndex: integer): TtiQueryFieldKind; override;
    function FieldSize(AIndex: integer): integer; override;

    function GetFieldAsString(const AName: string): string; override;
    function GetFieldAsFloat(const AName: string): extended; override;
    function GetFieldAsBoolean(const AName: string): Boolean; override;
    function GetFieldAsInteger(const AName: string): int64; override;
    function GetFieldAsDateTime(const AName: string): TDateTime; override;

    function GetFieldAsStringByIndex(AIndex: integer): string; override;
    function GetFieldAsFloatByIndex(AIndex: integer): extended; override;
    function GetFieldAsBooleanByIndex(AIndex: integer): Boolean; override;
    function GetFieldAsIntegerByIndex(AIndex: integer): int64; override;
    function GetFieldAsDateTimeByIndex(AIndex: integer): TDateTime; override;
    function GetFieldIsNullByIndex(AIndex: integer): Boolean; override;
    function GetFieldIsNull(const AName: string): Boolean; override;

    procedure AssignFieldAsStream(const AName: string; const AStream: TStream); override;
    procedure AssignFieldAsStreamByIndex(AIndex: integer; const AValue: TStream); override;

    // Parameter Overrides
    function ParamCount: integer; override;
    function ParamName(AIndex: integer): string; override;

    function GetParamAsString(const AName: string): string; override;
    function GetParamAsBoolean(const AName: string): Boolean; override;
    function GetParamAsFloat(const AName: string): extended; override;
    function GetParamAsInteger(const AName: string): int64; override;
    function GetParamAsDateTime(const AName: string): TDateTime; override;
    function GetParamAsTextBLOB(const AName: string): string; override;
    function GetParamIsNull(const AName: string): Boolean; override;

    procedure SetParamAsString(const AName, AValue: string); override;
    procedure SetParamAsBoolean(const AName: string; const AValue: Boolean); override;
    procedure SetParamAsFloat(const AName: string; const AValue: extended); override;
    procedure SetParamAsInteger(const AName: string; const AValue: int64); override;
    procedure SetParamAsDateTime(const AName: string; const AValue: TDateTime); override;
    procedure SetParamAsTextBLOB(const AName, AValue: string); override;
    procedure SetParamIsNull(const AName: string; const AValue: Boolean); override;

    procedure AssignParamFromStream(const AName: string; const AStream: TStream); override;
    procedure AssignParamToStream(const AName: string; const AStream: TStream); override;
    procedure AssignParams(const AParams: TtiQueryParams; const AWhere: TtiQueryParams = nil); override;
  end;


implementation

uses
  TypInfo
  {$ifdef LOGQUERYDATASET}
  ,tiLog
  {$endif}
  ,tiExcept
  ,Variants
  ;

{ TtiQueryDataset }

procedure TtiQueryDataset.SetDataset(const AValue: TDataset);
begin
  FDataset := AValue;
end;

procedure TtiQueryDataset.SetParams(const AValue: TParams);
begin
  FParams := AValue;
end;

function TtiQueryDataset.GetFieldAsBoolean(const AName: string): Boolean;
var
  lsValue: string;
begin
  lsValue := Trim(upperCase(FDataset.FieldByName(AName).AsString));
  Result  := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;

function TtiQueryDataset.GetFieldAsDateTime(const AName: string): TDateTime;
begin
  Result := FDataset.FieldByName(AName).AsDateTime;
end;

function TtiQueryDataset.GetFieldAsFloat(const AName: string): extended;
begin
  Result := FDataset.FieldByName(AName).AsFloat;
end;

function TtiQueryDataset.GetFieldAsInteger(const AName: string): int64;

Var
  lField : TField;

begin
  lField:=FDataset.FieldByName(AName);
  If lField is TLargeIntField then
    Result:=(lField as TLargeIntField).AsLargeInt
  else
    Result := lfield.AsInteger;
end;

function TtiQueryDataset.GetFieldAsString(const AName: string): string;
var
  lField : TField;
  lStream : TStringStream;
begin
  lField := FDataset.FieldByName(AName);
  if lField is TMemoField then
  begin
    lStream := TStringStream.Create('');
    try
      TMemoField(lField).SaveToStream(lStream);
      lStream.Position := 0;
      result := lStream.DataString;
    finally
      lStream.Free;
    end;
  end
  else
    result := lField.AsString;
end;

function TtiQueryDataset.GetFieldAsStringByIndex(AIndex: integer): string;

Var
  lField : TField;
  lStream : TStringStream;

begin
  lField:=FDataset.Fields[AIndex];
  if lField is TMemoField then
    begin
    lStream := TStringStream.Create('');
    try
      TMemoField(lField).SaveToStream(lStream);
      lStream.Position := 0;
      result := lStream.DataString;
    finally
      lStream.Free;
    end;
    end
  else
    result := lField.AsString;
end;


function TtiQueryDataset.GetFieldAsFloatByIndex(AIndex: integer): extended;
begin
  Result := FDataset.Fields[AIndex].AsFloat;
end;

function TtiQueryDataset.GetFieldAsIntegerByIndex(AIndex: integer): int64;

Var
  lField : TField;

begin
  lField:=FDataset.Fields[AIndex];
  if (lField is TLargeIntField) then
    Result := (lField as TLargeIntField).AsLargeInt
  else
    Result := lField.AsInteger;
end;


function TtiQueryDataset.GetFieldAsBooleanByIndex(AIndex: integer): Boolean;
var
  lsValue: string;
begin
  lsValue := Trim(FDataset.Fields[AIndex].AsString);
  Result  := (lsValue = 'T') or
    (lsValue = 'TRUE') or
    (lsValue = 'Y') or
    (lsValue = 'YES') or
    (lsValue = '1');
end;


function TtiQueryDataset.GetFieldAsDateTimeByIndex(AIndex: integer): TDateTime;
begin
  Result := FDataset.Fields[AIndex].AsDateTime;
end;

function TtiQueryDataset.GetFieldIsNullByIndex(AIndex: integer): Boolean;
begin
  Result := FDataset.Fields[AIndex].IsNull;
end;

function TtiQueryDataset.GetFieldIsNull(const AName: string): Boolean;
begin
  Result := FDataset.FieldByName(AName).IsNull;
end;

procedure TtiQueryDataset.AssignParamFromStream(const AName: string; const AStream: TStream);
begin
  CheckPrepared;
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  FParams.ParamByName(AName).LoadFromStream(AStream, ftBlob);
end;

procedure TtiQueryDataset.AssignParamToStream(const AName: string; const AStream: TStream);
var
  lBinData: olevariant;
  lDataPtr: Pointer;
  lHigh, lLow, lLen: integer;
  lParameter: TParam;
begin
  Assert(AStream <> nil, 'Stream not assigned');
  lParameter := FParams.ParamByName(AName);
  lLow       := VarArrayLowBound(lParameter.Value, 1);
  lHigh      := VarArrayHighBound(lParameter.Value, 1);
  lLen       := lHigh - lLow + 1;
  lBinData   := VarArrayCreate([0, lLen], varByte);
  lBinData   := lParameter.Value;
  lDataPtr   := VarArrayLock(lBinData);
  try
    AStream.WriteBuffer(lDataPtr^, lLen);
  finally
    VarArrayUnlock(lBinData);
  end;

{$IFDEF FPC}
  {$Note  Please try this option of saving to a Stream as well }
{$ENDIF}
{
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  (FParams.ParamByName(AName) as TBlobField).SaveToStream(AStream);
  AStream.Position := 0;
}
end;

procedure TtiQueryDataset.AssignFieldAsStream(const AName: string; const AStream: TStream);
begin
{$IFDEF FPC}
  {$Note Look at AssignParamToStream if this doesn't work}
{$ENDIF}
  Assert(AStream <> nil, 'Stream not assigned');
  AStream.Position := 0;
  (FDataset.FieldByName(AName) as TBlobField).SaveToStream(AStream);
end;

procedure TtiQueryDataset.AssignFieldAsStreamByIndex(AIndex: integer; const AValue: TStream);
begin
  Assert(AValue <> nil, 'Stream not assigned');
  AValue.Position := 0;
  (FDataset.Fields[AIndex] as TBlobField).SaveToStream(AValue);
end;

function TtiQueryDataset.FieldCount: integer;
begin
  Result := FDataset.FieldCount;
end;

function TtiQueryDataset.FieldName(AIndex: integer): string;
begin
  Result := FDataset.Fields[AIndex].Name;
end;

function TtiQueryDataset.GetActive: Boolean;
begin
{$ifdef LOGQUERYDATASET}
  Log('>>> TtiQueryDataset.GetActive');
{$endif}
  Result := FDataset.Active; //FbActive;
{$ifdef LOGQUERYDATASET}
  Log('<<< TtiQueryDataset.GetActive');
{$endif}
end;

function TtiQueryDataset.FieldIndex(const AName: string): integer;
begin
  Result := FDataset.FieldByName(AName).Index;
end;

function TtiQueryDataset.FieldKind(AIndex: integer): TtiQueryFieldKind;
var
  lDataType: TFieldType;
begin
  lDataType := FDataset.Fields[AIndex].DataType;
  case lDataType of
    ftString,
    ftWideString:
        Result := qfkString;

    ftSmallint,
    ftInteger,
    ftWord,
    ftLargeint:
        Result := qfkInteger;

    ftBoolean:
        Result := qfkLogical;

    ftFloat,
    ftCurrency,
    ftBCD:
        Result := qfkFloat;

    ftDate,
    ftTime,
    ftDateTime:
        Result := qfkDateTime;

    ftBlob,
    ftGraphic,
    ftVarBytes:
        Result := qfkBinary;

    ftMemo,
    ftFmtMemo:
        Result := qfkLongString;
    {$ifdef DELPHI10ORABOVE}
    ftWideMemo :                                result := qfkLongString;
    {$endif}

    else
      raise EtiOPFException.Create('Invalid Dataset.Fields[ AIndex ].DataType <' +
        GetEnumName(TypeInfo(TFieldType), Ord(lDataType)));
  end;
end;

function TtiQueryDataset.FieldSize(AIndex: integer): integer;
begin
  case FieldKind(AIndex) of
    qfkString    : result := FDataset.FieldDefs[ AIndex ].Size;
    qfkLongString : result := 0;
    qfkInteger   : result := 0;
    qfkFloat     : result := 0;
    qfkDateTime  : result := 0;
    qfkBinary    : result := 0;
    qfkLogical   : result := 0;
  else
    raise EtiOPFInternalException.Create('Invalid field type');
  end;
end;

procedure TtiQueryDataset.AssignParams(const AParams: TtiQueryParams; const AWhere: TtiQueryParams = nil);
begin
  if AParams = nil then
    Exit;
  CheckPrepared;
  inherited;
end;

procedure TtiQueryDataset.Next;
begin
  FDataset.Next;
end;

function TtiQueryDataset.GetEOF: Boolean;
begin
{$ifdef LOGQUERYDATASET}
  Log('>>> TtiQueryDataset.GetEOF');
{$endif}
  Result := FDataset.EOF;
{$ifdef LOGQUERYDATASET}
  Log('<<< TtiQueryDataset.GetEOF');
{$endif}
end;

procedure TtiQueryDataset.Open;
begin
{$ifdef LOGQUERYDATASET}
  Log('>>> TtiQueryDataset.Open');
{$endif}
  Active := True;
{$ifdef LOGQUERYDATASET}
  Log('<<< TtiQueryDataset.Open');
{$endif}
end;

procedure TtiQueryDataset.Close;
begin
{$ifdef LOGQUERYDATASET}
  Log('>>> TtiQueryDataset.Close);
{$endif}
  Active := False;
{$ifdef LOGQUERYDATASET}
  Log('<<< TtiQueryDataset.Close');
{$endif}
end;

procedure TtiQueryDataset.CheckPrepared;
begin
  if (FParams = nil) then
    raise EtiOPFException.Create('No parameters assigned');
end;

function TtiQueryDataset.GetParamAsBoolean(const AName: string): Boolean;
var
  lValue: string;
begin
  If HasNativeLogicalType then
    result := FParams.ParamByName(AName).AsBoolean
  else
    begin
    lValue := FParams.ParamByName(AName).AsString;
  {$IFDEF BOOLEAN_CHAR_1}
    Result := SameText(lValue, 'T');
  {$ELSE}
    Result := SameText(lValue, 'TRUE');
   {$ENDIF}
   end;
end;

function TtiQueryDataset.GetParamAsDateTime(const AName: string): TDateTime;
begin
  Result := FParams.ParamByName(AName).AsDateTime;
end;

function TtiQueryDataset.GetParamAsTextBLOB(const AName: string): string;
begin
  Result := FParams.ParamByName(AName).AsString;
end;

function TtiQueryDataset.GetParamIsNull(const AName: string): Boolean;
begin
  Result := FParams.ParamByName(AName).IsNull;
end;

function TtiQueryDataset.GetParamAsFloat(const AName: string): extended;
begin
  Result := FParams.ParamByName(AName).AsFloat;
end;

function TtiQueryDataset.GetParamAsInteger(const AName: string): int64;
begin
  Result := FParams.ParamByName(AName).AsInteger;
end;

function TtiQueryDataset.GetParamAsString(const AName: string): string;
begin
{$ifdef LOGQUERYDATASET}
  Log('>>> TtiQueryDataset.GetParamAsString (' + AName + ')');
{$endif}
  Result := FParams.ParamByName(AName).AsString;
{$ifdef LOGQUERYDATASET}
  Log('<<< TtiQueryDataset.GetParamAsString (' + Aname + ')');
{$endif}
end;

function TtiQueryDataset.ParamCount: integer;
begin
  Result := FParams.Count;
end;

function TtiQueryDataset.ParamName(AIndex: integer): string;
begin
  Result := FParams[AIndex].Name;
end;

procedure TtiQueryDataset.SetParamAsBoolean(const AName: string; const AValue: Boolean);
begin
  CheckPrepared;
  If HasNativeLogicalType then
    FParams.ParamByName(AName).AsBoolean:=AValue
  else
    begin
  {$IFDEF BOOLEAN_CHAR_1}
    if AValue then
      FParams.ParamByName(AName).AsString := 'T'
    else
      FParams.ParamByName(AName).AsString := 'F';
  {$ELSE}
    if AValue then
      FParams.ParamByName(AName).AsString := 'TRUE'
    else
      FParams.ParamByName(AName).AsString := 'FALSE';
  {$ENDIF}
  end;
end;

procedure TtiQueryDataset.SetParamAsDateTime(const AName: string; const AValue: TDateTime);
begin
  CheckPrepared;
  FParams.ParamByName(AName).AsDateTime := AValue;
end;

procedure TtiQueryDataset.SetParamAsTextBLOB(const AName, AValue: string);
begin
{$ifdef LOGQUERYDATASET}
  log('>>> TtiQueryDataset.SetParamAsTextBLOB');
{$endif}
  CheckPrepared;
  FParams.ParamByName(AName).AsString := AValue;
{$ifdef LOGQUERYDATASET}
  log('<<< TtiQueryDataset.SetParamAsTextBLOB');
{$endif}
end;

procedure TtiQueryDataset.SetParamIsNull(const AName: string; const AValue: Boolean);
begin
  if AValue then
    FParams.ParamByName(AName).Clear;
end;

procedure TtiQueryDataset.SetParamAsFloat(const AName: string; const AValue: extended);
begin
{$ifdef LOGQUERYDATASET}
  log('>>> TtiQueryDataset.SetParamAsFloat');
{$endif}
  CheckPrepared;
  FParams.ParamByName(AName).AsFloat := AValue;
{$ifdef LOGQUERYDATASET}
  log('<<< TtiQueryDataset.SetParamAsFloat');
{$endif}
end;

procedure TtiQueryDataset.SetParamAsInteger(const AName: string; const AValue: int64);
begin
{$ifdef LOGQUERYDATASET}
  log('>>> TtiQueryDataset.SetParamAsInteger');
{$endif}
  CheckPrepared;
  FParams.ParamByName(AName).AsInteger := AValue;
{$ifdef LOGQUERYDATASET}
  log('<<< TtiQueryDataset.SetParamAsInteger');
{$endif}
end;

procedure TtiQueryDataset.SetParamAsString(const AName, AValue: string);
begin
{$ifdef LOGQUERYDATASET}
  log('>>> TtiQueryDataset.SetParamAsString');
{$endif}
  CheckPrepared;
  FParams.ParamByName(AName).AsString := AValue;
{$ifdef LOGQUERYDATASET}
  log('<<< TtiQueryDataset.SetParamAsString');
{$endif}
end;


end.

