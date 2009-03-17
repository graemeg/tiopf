{
  Just a quick and dirty example of how business classes could be created.
}
unit model;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, tiObject, tiVTFObject;

type

  // no attribute properties specified. Properties accessed via FieldList.
  TPerson = class(TtiVTFObject)
  public
    constructor Create; override;
  end;


  // One example of how to surface the properties or "." (dot) notation access.
  TAddress = class(TtiVTFObject)
  private
    function    GetLine1: String;
    procedure   SetLine1(const AValue: String);
    function    GetLine2: String;
    procedure   SetLine2(const AValue: String);
    function    GetCity: String;
    procedure   SetCity(const AValue: String);
    function    GetPostCode: Integer;
    procedure   SetPostCode(const AValue: Integer);
  public
    constructor Create; override;
    property    Line1: String read GetLine1 write SetLine1;
    property    Line2: String read GetLine2 write SetLine2;
    property    City: String read GetCity write SetCity;
    property    PostCode: Integer read GetPostCode write SetPostCode;
  end;


implementation

{ TPerson }

constructor TPerson.Create;
var
  lField: TtiFieldAbs;
begin
  inherited Create;
  // ******   Option 1
{
  AddNewField('FirstName', vtString).AsString := 'Graeme';
  AddNewField('LastName', vtString).AsString := 'Geldenhuys';
  AddNewField('Age', vtInteger).AsString := '32';
}


  // ******   Option 2
{
  lField := TtiFieldString.Create(self);
  with TtiFieldString(lField) do
  begin
    FieldName := 'FirstName';
    AsString := 'Graeme';
  end;
  FFieldList.Add(lField);

  lField := TtiFieldString.Create(self);
  with TtiFieldString(lField) do
  begin
    FieldName := 'LastName';
    AsString := 'Geldenhuys';
  end;
  FFieldList.Add(lField);

  lField := TtiFieldInteger.Create(self);
  with TtiFieldInteger(lField) do
  begin
    FieldName := 'Age';
    AsInteger := 34;
  end;
  FFieldList.Add(lField);

  lField := TtiFieldDateTime.Create(self);
  with TtiFieldDateTime(lField) do
  begin
    FieldName := 'DateOfBirth';
    AsDateTime := EncodeDate(1974, 12, 31);
  end;
  FFieldList.Add(lField);
}

  // ******   Option 3
  AddStringField('FirstName', 'Graeme');
  AddStringField('LastName', 'Geldenhuys');
  AddIntegerField('Age', 34);
  AddDateTimeField('DateOfBirth', EncodeDate(1974, 12, 16));
end;

{ TAddress }

function TAddress.GetLine1: String;
begin
  Result := FieldItem['Line1'].AsString;
end;

procedure TAddress.SetLine1(const AValue: String);
begin
  FieldItem['Line1'].AsString := AValue;
end;

function TAddress.GetLine2: String;
begin
  Result := FieldItem['Line2'].AsString;
end;

procedure TAddress.SetLine2(const AValue: String);
begin
  FieldItem['Line2'].AsString := AValue;
end;

function TAddress.GetCity: String;
begin
  Result := FieldItem['City'].AsString;
end;

procedure TAddress.SetCity(const AValue: String);
begin
  FieldItem['City'].AsString := AValue;
end;

function TAddress.GetPostCode: Integer;
begin
  Result := TtiFieldInteger(FieldItem['PostCode']).AsInteger;
end;

procedure TAddress.SetPostCode(const AValue: Integer);
begin
  TtiFieldInteger(FieldItem['PostCode']).AsInteger := AValue;
end;

constructor TAddress.Create;
begin
  inherited Create;
  AddStringField('Line1', '');
  AddStringField('Line2', '');
  AddStringField('City', '');
  AddIntegerField('PostCode', 0);
  FieldItem['PostCode'].IsNull := True;
end;


end.

