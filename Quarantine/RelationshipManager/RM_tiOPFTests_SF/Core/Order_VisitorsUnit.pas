unit Order_VisitorsUnit;

interface

uses
  tiVisitorDB;

type
  TvisOrder_Read = class (TVisOwnedQrySelect)
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
    procedure MapRowToObject ; override ;
  end;

  TVisOrder_Create = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisOrder_Update = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisOrder_Delete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init          ; override ;
    procedure SetupParams    ; override ;
  end ;

procedure RegisterOrderVisitors;

implementation

uses
  tiObject,
  tiOPFManager,
  BOMUnit;

procedure RegisterOrderVisitors;
begin
  gTIOPFManager.RegReadVisitor(TvisOrder_Read);
  gTIOPFManager.RegSaveVisitor(TVisOrder_Create);
  gTIOPFManager.RegSaveVisitor(TVisOrder_Update);
  gTIOPFManager.RegSaveVisitor(TVisOrder_Delete);
end;

{ TvisOrder_Read }

function TvisOrder_Read.AcceptVisitor: boolean;
begin
  result := ( Visited is TOrderList ) and
            ( Visited.ObjectState = posEmpty );
end;

procedure TvisOrder_Read.Init;
begin
  Query.SQLText :=
    'select ORDER_ID, ORDER_DESCRIPTION, ORDER_QUANTITY from ORDERS';
end;

procedure TvisOrder_Read.MapRowToObject;
var
  temp: TOrder;
begin
  temp := TOrder.Create;
  temp.OID.AssignFromTIQuery('ORDER_ID', Query);
  temp.OrderDescription := Query.FieldAsString['ORDER_DESCRIPTION'];
  temp.Quantity := Query.FieldAsInteger['ORDER_QUANTITY'];
  temp.ObjectState := posClean;
  TOrderList(Visited).Add(temp);
end;

procedure TvisOrder_Read.SetupParams;
begin
  // Do Nothing because there are no parameters to initialize in the query
end;

{ TVisOrder_Create }

function TVisOrder_Create.AcceptVisitor: boolean;
begin
  result := ( Visited is TOrder ) and
            ( Visited.ObjectState = posCreate );
end;

procedure TVisOrder_Create.Init;
begin
  Query.SQLText :=
    'insert into ORDERS values(:OID, :ORDER_DESCRIPTION, :ORDER_QUANTITY)';
end;

procedure TVisOrder_Create.SetupParams;
var
  temp: TOrder;
begin
  temp := TOrder(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['ORDER_DESCRIPTION'] := temp.OrderDescription;
  Query.ParamAsInteger['ORDER_QUANTITY'] := temp.Quantity;
end;

{ TVisOrder_Update }

function TVisOrder_Update.AcceptVisitor: boolean;
begin
  result := ( Visited is TOrder) and
            ( Visited.ObjectState = posUpdate );
end;

procedure TVisOrder_Update.Init;
begin
  Query.SQLText :=
    'update ORDERS set ' +
        'ORDER_DESCRIPTION = :ORDER_DESCRIPTION, ' +
        'ORDER_QUANTITY = :ORDER_QUANTITY ' +
    'where ORDER_ID = :OID';
end;

procedure TVisOrder_Update.SetupParams;
var
  temp: TOrder;
begin
  temp := TOrder(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['ORDER_DESCRIPTION'] := temp.OrderDescription;
  Query.ParamAsInteger['ORDER_QUANTITY'] := temp.Quantity;
end;

{ TVisOrder_Delete }

function TVisOrder_Delete.AcceptVisitor: boolean;
begin
  result := ( Visited is TOrder ) and
            ( Visited.ObjectState = posDelete );
end;

procedure TVisOrder_Delete.Init;
begin
  Query.SQLText := 'delete from ORDERS where ORDER_ID = :OID';
end;

procedure TVisOrder_Delete.SetupParams;
var
  temp: TOrder;
begin
  temp := TOrder(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
end;

end.
