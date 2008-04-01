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
  public
    procedure   Read; override;
    procedure   Save; override;
    function    Find(const AOIDToFind: string): TAdrsTypeAbs; reintroduce;
    function    AdrsTypeAsStringByOID(const AOID: string): string;
  end;

  // Abstract item
  TAdrsTypeAbs     = class(TtiObject)
  private
    FText: string;
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

function TAdrsTypeAbs.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  if Text = '' then
    AErrors.AddError('Text', CErrorAdrsTypeAbsTextNotAssigned);
  Result:= AErrors.Count = 0;
end;

end.
