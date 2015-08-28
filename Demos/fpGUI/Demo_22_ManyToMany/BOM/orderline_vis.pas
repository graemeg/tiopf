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
  demoUtils;

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
begin
  lData := TOrderLine.Create;
  lData.OID.AssignFromTIQuery(Query);
//  lData.OlOidOrders      := Query.FieldAsString[   'OL_OID_ORDERS'    ];
//  lData.Products    := Query.FieldAsString[   'OL_OID_PRODUCTS'  ];
  lData.Quantity       := Query.FieldAsInteger['OL_QUANTITY'];
  lData.UnitSalePrice  := IntToCurr(Query.FieldAsInteger['OL_UNITSALEPRICE']);
  lData.ObjectState := posClean;
  (Visited as TtiObjectList).Add(lData);

end;

end.

