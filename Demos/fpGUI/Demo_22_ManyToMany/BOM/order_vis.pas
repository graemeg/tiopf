unit order_vis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiVisitorDB;

type

  TVisOrder_ReadList = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init; override;
    procedure   SetupParams; override;
    procedure   MapRowToObject; override;
  end;


implementation

uses
  tiObject,
  tiLog, tiUtils,
  order;

{ TVisOrder_ReadList }

function TVisOrder_ReadList.AcceptVisitor: boolean;
begin
  Result := (Visited is TOrderList) and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TVisOrder_ReadList.Init;
begin
  inherited Init;
  Query.SQLText := 'select * from orders where or_oid_customer = :customer';
end;

procedure TVisOrder_ReadList.SetupParams;
var
  lData: TOrderList;
begin
  lData := (Visited as TOrderList);
  lData.Customer.OID.AssignToTIQuery('customer', Query);
end;

procedure TVisOrder_ReadList.MapRowToObject;
var
  lData: TOrder;
begin
  lData := TOrder.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.OrderDate := tiIntlDateStorAsDateTime(Query.FieldAsString['OR_ORDERDATE']);
  lData.SoldBy := Query.FieldAsString['OR_SOLDBY'];
  lData.ObjectState := posClean;
  (Visited as TtiObjectList).Add(lData);
end;

end.

