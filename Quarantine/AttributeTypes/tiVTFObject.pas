{
  This is simply a sample unit showing one way of implementing
  Attribute Classes like TtiFieldXXX into a new base object.
  It allows for easy creation of attribute types and runtime
  modification of the class property list.

  NOTE:
    * This is only a sample and was put together in 20 minutes.
    * There are still many pitfalls in this class
      - visitor access to attributes
      - common Value property instead of AsXXX
      - nasty type casting in business objects
    * many more...

}

unit tiVTFObject;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, tiObject;

type

  TVTFType = (vtVariant, vtString, vtInteger, vtDateTime, vtFloat,
    vtCurrency, vtBoolean, vtBlob, vtMemo, vtObject, vtObjectList);


  TtiVTFObject = class(TtiObject)
  private
    FFieldList: TtiFieldList;
    function    GetFieldItem(AFieldName: string): TtiFieldAbs;
    procedure   SetFieldItem(AFieldName: string; const AObject: TtiFieldAbs);
  public
    constructor Create; override;
    destructor  Destroy; override;
    function    AddNewField(const AFieldName: string; const AValueType: TVTFType): TtiFieldAbs;
    function    AddStringField(const AFieldName: string; const AValue: string): TtiFieldString;
    function    AddIntegerField(const AFieldName: string; const AValue: integer): TtiFieldInteger;
    function    AddDateTimeField(const AFieldName: string; const AValue: TDateTime): TtiFieldDateTime;
    function    AddFloatField(const AFieldName: string; const AValue: Extended): TtiFieldFloat;
    property    FieldItem[AFieldName: string]: TtiFieldAbs read GetFieldItem write SetFieldItem; default;
  published
    property    FieldList: TtiFieldList read FFieldList;
  end;



implementation


{ TtiVTFObject }

function TtiVTFObject.GetFieldItem(AFieldName: string): TtiFieldAbs;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to FFieldList.Count-1 do
  begin
    if FFieldList.Items[i].FieldName = AFieldName then
    begin
      Result := FFieldList.Items[i];
      Exit;
    end;
  end;
  raise Exception.Create('Failed to find field <' + AFieldName + '>');
end;

procedure TtiVTFObject.SetFieldItem(AFieldName: string; const AObject: TtiFieldAbs);
var
  i: integer;
begin
  for i := 0 to FFieldList.Count-1 do
  begin
    if FFieldList.Items[i].FieldName = AFieldName then
    begin
      FFieldList.Items[i] := AObject;
      Exit;
    end;
  end;
end;

constructor TtiVTFObject.Create;
begin
  inherited Create;
  FFieldList := TtiFieldList.Create;
  FFieldList.OwnsObjects := True;
end;

destructor TtiVTFObject.Destroy;
begin
  FFieldList.Free;
  inherited Destroy;
end;

function TtiVTFObject.AddNewField(const AFieldName: string; const AValueType: TVTFType): TtiFieldAbs;

  procedure UnsupportedType;
  begin
    raise Exception.Create('Unsupported Type detected');
  end;

begin
  { TODO : Create a ValueType factory! }
  result := nil;

  case AValueType of
    vtVariant:
        UnsupportedType;
    vtString:
        begin
          Result := TtiFieldString.Create(self);
          Result.FieldName := AFieldName;
        end;
    vtInteger:
        begin
          result := TtiFieldInteger.Create(self);
          result.FieldName := AFieldName;
        end;
    vtDateTime:
        begin
          result := TtiFieldDateTime.Create(self);
          result.FieldName := AFieldName;
        end;
    vtFloat:
        begin
          result := TtiFieldFloat.Create(self);
          result.FieldName := AFieldName;
        end;
    vtCurrency:
        begin
          result := TtiFieldFloat.Create(self);
          result.FieldName := AFieldName;
        end;
    vtBoolean:
        begin
          result := TtiFieldBoolean.Create(self);
          result.FieldName := AFieldName;
        end;
    vtBlob:
        UnsupportedType;
    vtMemo:
        UnsupportedType;
    vtObject:
        UnsupportedType;
    vtObjectList:
        UnsupportedType;
  end;

  if Assigned(result) then
    FFieldList.Add(result);
end;

function TtiVTFObject.AddStringField(const AFieldName: string;
  const AValue: string): TtiFieldString;
begin
  Result := TtiFieldString.Create(self);
  Result.FieldName := AFieldName;
  Result.AsString := AValue;
  FFieldList.Add(Result);
end;

function TtiVTFObject.AddIntegerField(const AFieldName: string;
  const AValue: integer): TtiFieldInteger;
begin
  Result := TtiFieldInteger.Create(self);
  Result.FieldName := AFieldName;
  Result.AsInteger := AValue;
  FFieldList.Add(Result);
end;

function TtiVTFObject.AddDateTimeField(const AFieldName: string;
  const AValue: TDateTime): TtiFieldDateTime;
begin
  Result := TtiFieldDateTime.Create(self);
  Result.FieldName := AFieldName;
  Result.AsDateTime := AValue;
  FFieldList.Add(Result);
end;

function TtiVTFObject.AddFloatField(const AFieldName: string;
  const AValue: Extended): TtiFieldFloat;
begin
  Result := TtiFieldFloat.Create(self);
  Result.FieldName := AFieldName;
  Result.AsFloat := AValue;
  FFieldList.Add(Result);
end;



end.

