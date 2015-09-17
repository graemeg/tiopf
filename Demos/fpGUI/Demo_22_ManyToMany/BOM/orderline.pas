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
    function    GetDisplayTotalPrice: string;
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
    property    DisplayTotalPrice: string read GetDisplayTotalPrice;
  end;


  TOrderLineList = class(TtiObjectList)
  private
    FOrder: TOrder;
    function    GetOrderTotal: Currency;
  protected
    function    GetItems(i: integer): TOrderLine; reintroduce;
    procedure   SetItems(i: integer; const Value: TOrderLine); reintroduce;
  public
    property    Items[i:integer]: TOrderLine read GetItems write SetItems;
    function    Add(const AObject: TOrderLine): integer; reintroduce;
    procedure   Read; overload; override;
    property    Order: TOrder read FOrder write FOrder;
    property    OrderTotal: Currency read GetOrderTotal;
  end;


implementation

uses
  tiOPFManager
  ,demoUtils
  ,tiIteratorIntf
  ;

{ TOrderLine }

function TOrderLine.GetDisplayQuantity: string;
begin
  Result := IntToStr(Quantity);
end;

function TOrderLine.GetDisplayTotalPrice: string;
begin
  Result := DisplayCurrency(Quantity * UnitSalePrice);
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

{ Just for goot measure, here is an example of using iterator interfaces
  to loop through our list object. }
function TOrderLineList.GetOrderTotal: Currency;
var
  itr: ItiOPFListIterator;
  o: TOrderLine;
begin
  Result := 0.0;
  itr := gIteratorFactory.tiListIterator(self);
  while itr.HasNext do
  begin
    o := TOrderLine(itr.Next);
    Result := Result + (o.Quantity * o.UnitSalePrice);
  end;
end;

end.

