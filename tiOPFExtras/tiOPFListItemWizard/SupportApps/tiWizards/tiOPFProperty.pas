{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Chris Latta, Data Solutions Pty Ltd
  for inclusion in the tiOPF (TechInsite Object Persistence Framework) from
    TechInsite Pty. Ltd.
    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm
    EMail:           info@techinsite.com.au

  Please submit changes to tiOPF@techinsite.com.au

  Purpose: An Open Tools API Wizard for Delphi to assist in the creation
  of a unit defining a TPerObjList which owns TPerObjAbs items

  Revision History:
    March 2002, Chris Latta, Created

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit tiOPFProperty;

interface

uses
  Classes;

type
  TPropertyVisibility = (pvPrivate, pvProtected, pvPublished, pvPublic);

  TPropertyAccess = (paNone, paField, paMethod, paBoth);

  TProperty = class;   // Forward reference

  // TProperties is a collection of properties sorted by Visibility-Name
  // Each property has a unique Name
  TProperties = class(TCollection)
  private
  public
    constructor Create(ItemClass: TCollectionItemClass); reintroduce;
    function Add(Name: string; Visibility: TPropertyVisibility; DataType: string;
      ReadAccess, WriteAccess: TPropertyAccess): TProperty; overload;
    function Add(Name, Visibility, DataType, ReadAccess,
      WriteAccess: string): TProperty; overload;
    function Add(AProperty: TProperty): TProperty; overload;
    procedure Clone(AProperties: TProperties);
    procedure DeleteByName(Value: string);
    function IndexOf(Value: string): Integer;
    function IndexOfPrior(Name: string; Visibility: TPropertyVisibility): Integer;
    function FindByName(Value: string): TProperty;
  end;

  TProperty = class(TCollectionItem)
  private
    function GetReadAccessAsString: string;
    procedure SetReadAccessAsString(Value: string);
    function GetWriteAccessAsString: string;
    procedure SetWriteAccessAsString(Value: string);
    function GetVisibilityAsString: string;
    procedure SetVisibilityAsString(const Value: string);
  public
    Name: string;
    Visibility: TPropertyVisibility;
    DataType: string;
    ReadAccess: TPropertyAccess;
    WriteAccess: TPropertyAccess;
    property ReadAccessAsString: string read GetreadAccessAsString
      Write SetReadAccessAsString;
    property WriteAccessAsString: string read GetWriteAccessAsString
      Write SetWriteAccessAsString;
    property VisibilityAsString: string read GetVisibilityAsString
      Write SetVisibilityAsString;
    constructor Create(Collection: TCollection); override;
  end;

implementation

uses
  SysUtils;

{ Miscellaneous conversion functions }

function PropertyAccessAsString(Value: TPropertyAccess): string;
begin
  case Value of
    paNone: Result := 'None';
    paField: Result := 'Field';
    paMethod: Result := 'Method';
    paBoth: Result := 'Method + Field';
  end;
end;

function StringAsPropertyAccess(Value: string): TPropertyAccess;
begin
  if Value = 'None' then
    Result := paNone
  else if Value = 'Field' then
    Result := paField
  else if Value = 'Method' then
    Result := paMethod
  else if Value = 'Method + Field' then
    Result := paBoth
  else
    Result := paNone;
end;

function PropertyVisibilityAsString(Value: TPropertyVisibility): string;
begin
  case Value of
    pvPrivate: Result := 'Private';
    pvProtected: Result := 'Protected';
    pvPublished: Result := 'Published';
    pvPublic: Result := 'Public';
  end;
end;

function StringAsPropertyVisibility(Value: string): TPropertyVisibility;
var
  ValueCaps: string;
begin
  ValueCaps := UpperCase(Value);
  if ValueCaps = 'PRIVATE' then
    Result := pvPrivate
  else if ValueCaps = 'PROTECTED' then
    Result := pvProtected
  else if ValueCaps = 'PUBLISHED' then
    Result := pvPublished
  else
    Result := pvPublic;
end;


{ TProperty }

constructor TProperty.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Visibility := pvPublic;
  ReadAccess := paField;
  WriteAccess := paField;
end;

function TProperty.GetReadAccessAsString: string;
begin
  Result := PropertyAccessAsString(ReadAccess);
end;

function TProperty.GetWriteAccessAsString: string;
begin
  Result := PropertyAccessAsString(WriteAccess);
end;

procedure TProperty.SetReadAccessAsString(Value: string);
begin
  ReadAccess := StringAsPropertyAccess(Value);
end;

procedure TProperty.SetWriteAccessAsString(Value: string);
begin
  WriteAccess := StringAsPropertyAccess(Value);
end;

function TProperty.GetVisibilityAsString: string;
begin
  Result := PropertyVisibilityAsString(Visibility);
end;

procedure TProperty.SetVisibilityAsString(const Value: string);
begin
  Visibility := StringAsPropertyVisibility(Value);
end;

{ TProperties }

constructor TProperties.Create(ItemClass: TCollectionItemClass);
begin
  inherited Create(TProperty);
end;

function TProperties.Add(Name: string; Visibility: TPropertyVisibility;
  DataType: string; ReadAccess, WriteAccess: TPropertyAccess): TProperty;
var
  i: Integer;
begin
  i := IndexOf(Name);
  if i = -1 then
  begin
    i := IndexOfPrior(Name, Visibility);
    // If no items or prior item is last, then append this item
    if (Count = 0) or (i >= Count-1) then
      Result := TProperty(inherited Add)
    // If no items are prior, then insert at the top
    else if i = -1 then
      Result := TProperty(inherited Insert(0))
    // Otherwise insert after the prior item found
    else
      Result := TProperty(inherited Insert(i+1));
  end
  else
    Result := TProperty(Items[i]);

  Result.Name := Name;
  Result.Visibility := Visibility;
  Result.DataType := DataType;
  Result.ReadAccess := ReadAccess;
  Result.WriteAccess := WriteAccess;
end;

function TProperties.Add(Name, Visibility, DataType, ReadAccess,
  WriteAccess: string): TProperty;
begin
  // Call the other method with converted parameters
  Result := Add(Name, StringAsPropertyVisibility(Visibility), DataType
    , StringAsPropertyAccess(ReadAccess), StringAsPropertyAccess(WriteAccess));
end;

function TProperties.Add(AProperty: TProperty): TProperty;
begin
  // Call the other method with converted parameters
  Result := Add(AProperty.Name, AProperty.Visibility, AProperty.DataType
    , AProperty.ReadAccess, AProperty.WriteAccess);
end;

procedure TProperties.Clone(AProperties: TProperties);
var
  i: Integer;
begin
  Clear;
  for i := 0 to AProperties.Count-1 do
    Add(TProperty(AProperties.Items[i]));
end;

procedure TProperties.DeleteByName(Value: string);
var
  i: Integer;
begin
  i := IndexOf(Value);
  if i > -1 then
    inherited Delete(i);
end;

function TProperties.FindByName(Value: string): TProperty;
var
  i: Integer;
begin
  Result := nil;
  i := IndexOf(Value);
  if i > -1 then
    Result := TProperty(Items[i]);
end;

function TProperties.IndexOf(Value: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if TProperty(Items[i]).Name = Value then
      Result := i;
end;

function TProperties.IndexOfPrior(Name: string; Visibility: TPropertyVisibility): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if (TProperty(Items[i]).Visibility < Visibility) then
      Result := i
    else if (TProperty(Items[i]).Visibility = Visibility)
      and (TProperty(Items[i]).Name < Name) then
     Result := i;
end;

end.



