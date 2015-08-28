unit orderline;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  tiObject,
  order,
  product;


type
  // forward declaration
  TOrderLineList = class;


  TOrderLine = class(TtiObject)
  private
    FProduct: TProduct;
    FQuantity: Integer;
    FUnitSalePrice: Currency;
    function    GetDisplayProduct: string;
    function    GetDisplayQuantity: string;
    function    GetDisplayUnitPrice: string;
  protected
    function    GetOwner: TOrderLineList; reintroduce;
    procedure   SetOwner(const Value: TOrderLineList); reintroduce;
  public
    property    Owner: TOrderLineList read GetOwner write SetOwner;
    property    Product: TProduct read FProduct write FProduct;
  published
    property    Quantity: Integer read FQuantity write FQuantity;
    property    UnitSalePrice: Currency read FUnitSalePrice  write FUnitSalePrice;
    property    DisplayQuantity: string read GetDisplayQuantity;
    property    DisplayUnitPrice: string read GetDisplayUnitPrice;
    property    DisplayProduct: string read GetDisplayProduct;
  end;


  TOrderLineList = class(TtiObjectList)
  private
    FOrder: TOrder;
  protected
    function    GetItems(i: integer): TOrderLine; reintroduce;
    procedure   SetItems(i: integer; const Value: TOrderLine); reintroduce;
  public
    property    Items[i:integer]: TOrderLine read GetItems write SetItems;
    function    Add(const AObject: TOrderLine): integer; reintroduce;
    procedure   Read; overload; override;
    property    Order: TOrder read FOrder write FOrder;
  end;


implementation

uses
  tiOPFManager
  ,demoUtils
  ;

{ TOrderLine }

function TOrderLine.GetDisplayQuantity: string;
begin
  Result := IntToStr(Quantity);
end;

function TOrderLine.GetDisplayProduct: string;
begin
  Result := Product.Caption;
end;

function TOrderLine.GetDisplayUnitPrice: string;
begin
  Result := DisplayCurrency(UnitSalePrice);
end;

function TOrderLine.GetOwner: TOrderLineList;
begin
  result := TOrderLineList(inherited GetOwner);
end;

procedure TOrderLine.SetOwner(const Value: TOrderLineList);
begin
  inherited SetOwner(Value);
end;

{ TOrderLineList }

function TOrderLineList.GetItems(i: integer): TOrderLine;
begin
  result := TOrderLine(inherited GetItems(i));
end;

procedure TOrderLineList.SetItems(i: integer; const Value: TOrderLine);
begin
  inherited SetItems(i, Value);
end;

function TOrderLineList.Add(const AObject: TOrderLine): integer;
begin
  result := inherited Add(AObject);
end;

procedure TOrderLineList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('orderline_readlist', self);
end;

end.

