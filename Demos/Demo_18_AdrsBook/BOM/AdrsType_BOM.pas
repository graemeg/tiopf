unit AdrsType_BOM;

{.$I tiDefines.inc}

interface
uses
   tiObject
  ,Classes
  ,tiOID
 ;

type

  TEAdrsTypeList     = class;
  TEAdrsType = class;

  TEAdrsTypeList    = class(TtiObjectList)
  protected
    function    GetItems(i: integer): TEAdrsType; reintroduce;
    procedure   SetItems(i: integer; const Value: TEAdrsType); reintroduce;
  public
    procedure   Read; override;
    procedure   Save; override;
    property    Items[i:integer]: TEAdrsType read GetItems write SetItems;
    procedure   Add(const AObject    : TEAdrsType); reintroduce;
  end;

  TEAdrsType     = class(TtiObject)
  private
    FText: string;
  protected
    function    GetOwner: TEAdrsTypeList; reintroduce;
    procedure   SetOwner(const Value: TEAdrsTypeList); reintroduce;
  public
    property    Owner      : TEAdrsTypeList             read GetOwner      write SetOwner;
  published
    property    Text: string read FText write FText;
  end;

implementation
uses
  tiUtils
  ,TypInfo
  ,tiOPFManager
  ,tiAutoMap
  ,SysUtils
  ,tiVisitorDB
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAdrsTypeList
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TEAdrsTypeList.Add(const AObject: TEAdrsType);
begin
  inherited Add(AObject);
end;

function TEAdrsTypeList.GetItems(i: integer): TEAdrsType;
begin
  result:= TEAdrsType(inherited GetItems(i));
end;

procedure TEAdrsTypeList.Read;
begin
  inherited;
  SortByOID;
end;

procedure TEAdrsTypeList.Save;
begin
  inherited;
end;

procedure TEAdrsTypeList.SetItems(i: integer; const Value: TEAdrsType);
begin
  inherited SetItems(i, Value);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAdrsTypeList
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TEAdrsType.GetOwner: TEAdrsTypeList;
begin
  result:= TEAdrsTypeList(inherited GetOwner);
end;

procedure TEAdrsType.SetOwner(const Value: TEAdrsTypeList);
begin
  inherited SetOwner(Value);
end;

end.








