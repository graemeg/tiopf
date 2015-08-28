unit product;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject;

type
  // forward declaration
  TProductList = class;


  TProduct = class(TtiObject)
  private
    FProdName: string;
    FUnitListPrice: Currency;
    FUnitsInStock: Integer;
    function    GetDisplayPrice: string;
    function    GetDisplayStock: string;
    procedure   SetProdName(AValue: string);
    procedure   SetUnitListPrice(AValue: Currency);
    procedure   SetUnitsInStock(AValue: Integer);
  protected
    function    GetOwner: TProductList; reintroduce;
    procedure   SetOwner(const Value: TProductList); reintroduce;
    function    GetCaption: string; override;
  public
    property    Owner: TProductList read GetOwner write SetOwner;
  published
    property    ProdName: string read FProdName write SetProdName;
    property    UnitListPrice: Currency read FUnitListPrice write SetUnitListPrice;
    property    UnitsInStock: Integer read FUnitsInStock write SetUnitsInStock;
    property    DisplayPrice: string read GetDisplayPrice;
    property    DisplayStock: string read GetDisplayStock;
  end;


  TProductList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TProduct; reintroduce;
    procedure   SetItems(i: integer; const Value: TProduct); reintroduce;
  public
    property    Items[i:integer]: TProduct read GetItems write SetItems;
    function    Add(const AObject: TProduct): integer; reintroduce;
    procedure   Read; overload; override;
  end;

implementation

uses
  tiOPFManager,
  demoUtils;

{ TProduct }

procedure TProduct.SetProdName(AValue: string);
begin
  if FProdName = AValue then Exit;
  BeginUpdate;
  FProdName := AValue;
  EndUpdate;
end;

function TProduct.GetDisplayPrice: string;
begin
  Result := DisplayCurrency(UnitListPrice);
end;

function TProduct.GetDisplayStock: string;
begin
  Result := IntToStr(UnitsInStock);
end;

procedure TProduct.SetUnitListPrice(AValue: Currency);
begin
  if FUnitListPrice = AValue then Exit;
  BeginUpdate;
  FUnitListPrice := AValue;
  EndUpdate;
end;

procedure TProduct.SetUnitsInStock(AValue: Integer);
begin
  if FUnitsInStock = AValue then Exit;
  BeginUpdate;
  FUnitsInStock := AValue;
  EndUpdate;
end;

function TProduct.GetOwner: TProductList;
begin
  result := TProductList(inherited GetOwner);
end;

procedure TProduct.SetOwner(const Value: TProductList);
begin
  inherited SetOwner(Value);
end;

function TProduct.GetCaption: string;
begin
  Result := ProdName;
end;

{ TProductList }

function TProductList.GetItems(i: integer): TProduct;
begin
  result := TProduct(inherited GetItems(i));
end;

procedure TProductList.SetItems(i: integer; const Value: TProduct);
begin
  inherited SetItems(i, Value);
end;

function TProductList.Add(const AObject: TProduct): integer;
begin
  result := inherited Add(AObject);
end;

procedure TProductList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('product_readlist', self);
end;

end.

