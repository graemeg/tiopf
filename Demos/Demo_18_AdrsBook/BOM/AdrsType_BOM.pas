unit AdrsType_BOM;

{.$I tiDefines.inc}

interface
uses
  tiObject;

const
  CErrorAdrsTypeAbsTextNotAssigned = 'Text not assigned';

type

  TAdrsTypeListAbs = class;
  TAdrsTypeAbs = class;
  TAdrsTypeList = class;
  TAdrsType = class;
  TEAdrsTypeList = class;
  TEAdrsType = class;

  // Abstract list
  TAdrsTypeListAbs    = class(TtiObjectList)
  protected
    function GetItems(i: integer): TAdrsTypeAbs; reintroduce;
    procedure SetItems(i: integer; const AValue: TAdrsTypeAbs); reintroduce;
  public
    procedure   Read; override;
    procedure   Save; override;
    function    Find(const AOIDToFind: string): TAdrsTypeAbs; reintroduce;
    function    AdrsTypeAsStringByOID(const AOID: string): string;
    function    FindOIDByAdrsTypeAsString(const AValue: string): string;
    property    Items[i:integer]: TAdrsTypeAbs read GetItems write SetItems;
  end;

  // Abstract item
  TAdrsTypeAbs     = class(TtiObject)
  private
    FText: string;
  protected
    function    GetCaption: string; override;
  public
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property    Text: string read FText write FText;
  end;

  // Concrete EAdrsTypeList
  TAdrsTypeList    = class(TAdrsTypeListAbs)
  protected
    function    GetItems(i: integer): TAdrsType; reintroduce;
    procedure   SetItems(i: integer; const AValue: TAdrsType); reintroduce;
  public
    property    Items[i:integer]: TAdrsType read GetItems write SetItems;
    procedure   Add(const AObject    : TAdrsType); reintroduce;
  end;

  // Concrete EAdrsType Item
  TAdrsType     = class(TAdrsTypeAbs)
  end;

  // Concrete EAdrsTypeList
  TEAdrsTypeList    = class(TAdrsTypeListAbs)
  protected
    function    GetItems(i: integer): TEAdrsType; reintroduce;
    procedure   SetItems(i: integer; const Value: TEAdrsType); reintroduce;
  public
    property    Items[i:integer]: TEAdrsType read GetItems write SetItems;
    procedure   Add(const AObject    : TEAdrsType); reintroduce;
  end;

  // Concrete EAdrsType Item
  TEAdrsType     = class(TAdrsTypeAbs)
  end;

implementation
uses
  SysUtils;

procedure TEAdrsTypeList.Add(const AObject: TEAdrsType);
begin
  inherited Add(AObject);
end;

function TEAdrsTypeList.GetItems(i: integer): TEAdrsType;
begin
  result:= TEAdrsType(inherited GetItems(i));
end;

function TAdrsTypeListAbs.AdrsTypeAsStringByOID(const AOID: string): string;
var
  LItem: TAdrsTypeAbs;
begin
  LItem:= Find(AOID);
  if LItem <> nil then
    result:= LItem.Text
  else
    result:= '';
end;

function TAdrsTypeListAbs.Find(const AOIDToFind: string): TAdrsTypeAbs;
begin
  result:= inherited Find(AOIDToFind) as TAdrsTypeAbs;
end;

function TAdrsTypeListAbs.FindOIDByAdrsTypeAsString(
  const AValue: string): string;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Text, AValue) then
    begin
      result:= Items[i].OID.AsString;
      Exit; //==>
    end;
  result:= '';
end;

procedure TAdrsTypeListAbs.Read;
begin
  inherited;
  SortByOID;
end;

procedure TAdrsTypeListAbs.Save;
begin
  inherited;
end;

procedure TEAdrsTypeList.SetItems(i: integer; const Value: TEAdrsType);
begin
  inherited SetItems(i, Value);
end;

{ TAdrsTypeList }

procedure TAdrsTypeList.Add(const AObject: TAdrsType);
begin
  inherited Add(AObject);
end;

function TAdrsTypeList.GetItems(i: integer): TAdrsType;
begin
  result:= inherited GetItems(i) as TAdrsType;
end;

procedure TAdrsTypeList.SetItems(i: integer; const AValue: TAdrsType);
begin
  inherited SetItems(i, AValue);
end;

{ TAdrsTypeAbs }

function TAdrsTypeListAbs.GetItems(i: integer): TAdrsTypeAbs;
begin
  result:= inherited GetItems(i) as TAdrsTypeAbs;
end;

function TAdrsTypeAbs.GetCaption: string;
begin
  result:= Text;
end;

function TAdrsTypeAbs.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  if Text = '' then
    AErrors.AddError('Text', CErrorAdrsTypeAbsTextNotAssigned);
  Result:= AErrors.Count = 0;
end;

procedure TAdrsTypeListAbs.SetItems(i: integer; const AValue: TAdrsTypeAbs);
begin
  inherited SetItems(i, AValue);
end;

end.
