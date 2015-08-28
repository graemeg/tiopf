unit orderline_vis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiVisitorDB;

type

  TVisOrderLine_ReadList = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
    procedure   MapRowToObject; override;
  end;


implementation

uses
  tiObject,
  tiLog,
  orderline,
  demoUtils,
  app_bom,
  product;

{ TVisOrderLine_ReadList }

function TVisOrderLine_ReadList.AcceptVisitor: boolean;
begin
  Result := (Visited is TOrderLineList) and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TVisOrderLine_ReadList.Init;
begin
  inherited Init;
  Query.SQLText := 'select * from orderlines where ol_oid_orders = :order';
end;

procedure TVisOrderLine_ReadList.SetupParams;
var
  lData: TOrderLineList;
begin
  lData := (Visited as TOrderLineList);
  lData.Order.OID.AssignToTIQuery('order', Query);
end;

procedure TVisOrderLine_ReadList.MapRowToObject;
var
  lData: TOrderLine;
  lProduct: TProduct;
begin
  lProduct := nil;
  lData := TOrderLine.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.Quantity       := Query.FieldAsInteger['OL_QUANTITY'];
  lData.UnitSalePrice  := IntToCurr(Query.FieldAsInteger['OL_UNITSALEPRICE']);

  // Lookup our product and set the reference

  if gDemoApp.ProductList.Count = 0 then
    raise Exception.Create('ProductList count = 0 when in should not be!');
  lProduct := gDemoapp.ProductList.Find(Query.FieldAsString['OL_OID_PRODUCTS']);
  if lProduct = nil then
    raise Exception.CreateFmt('Unable to find Product with OID <%s>', [Query.FieldAsString['OL_OID_PRODUCTS']]);
  lData.Product := lProduct;

  lData.ObjectState := posClean;
  (Visited as TtiObjectList).Add(lData);

end;

end.

