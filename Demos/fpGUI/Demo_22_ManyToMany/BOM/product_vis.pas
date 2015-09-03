unit product_vis;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiVisitorDB;

type

  TVisProduct_ReadList = class(TtiVisitorSelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init; override;
    procedure   MapRowToObject; override;
  end;


implementation

uses
  tiObject,
  tiLog,
  demoUtils,
  product;

{ TVisProduct_ReadList }

function TVisProduct_ReadList.AcceptVisitor: boolean;
begin
  Result := (Visited is TProductList) and (Visited.ObjectState = posEmpty);
  Log([ClassName, Visited.ClassName, Visited.ObjectStateAsString, Result], lsAcceptVisitor);
end;

procedure TVisProduct_ReadList.Init;
begin
  inherited Init;
  Query.SQLText := 'select * from products';
end;

procedure TVisProduct_ReadList.MapRowToObject;
var
  lData: TProduct;
begin
  lData := TProduct.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.ProdName := Query.FieldAsString['PR_PRODNAME'];
  lData.UnitListPrice := IntToCurr(Query.FieldAsInteger['PR_UNITLISTPRICE']);
  lData.UnitsInStock := Query.FieldAsInteger['PR_UNITSINSTOCK'];
  lData.ObjectState := posClean;
  (Visited as TtiObjectList).Add(lData);
end;

end.

