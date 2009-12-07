unit Customer_VisitorsUnit;

interface

uses
  tiVisitorDB;

type
  TvisCustomer_Read = class (TVisOwnedQrySelect)
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
    procedure MapRowToObject ; override ;
  end;

  TVisCustomer_Create = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisCustomer_Update = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init           ; override ;
    procedure SetupParams    ; override ;
  end ;

  TVisCustomer_Delete = class( TVisOwnedQryUpdate )
  protected
    function  AcceptVisitor : boolean ; override ;
    procedure Init          ; override ;
    procedure SetupParams    ; override ;
  end ;

procedure RegisterCustomerVisitors;

implementation

uses
  tiObject,
  tiOPFManager,
  BOMUnit;

  procedure RegisterCustomerVisitors;
  begin
    gTIOPFManager.RegReadVisitor(TvisCustomer_Read);
    gTIOPFManager.RegSaveVisitor(TVisCustomer_Create);
    gTIOPFManager.RegSaveVisitor(TVisCustomer_Update);
    gTIOPFManager.RegSaveVisitor(TVisCustomer_Delete);
  end;

{ TvisCustomer_Read }

function TvisCustomer_Read.AcceptVisitor: boolean;
begin
  result := ( Visited is TCustomerList ) and
            ( Visited.ObjectState = posEmpty );
end;

procedure TvisCustomer_Read.Init;
begin
  Query.SQLText := 'select CUSTOMER_ID, CUSTOMER_NAME from CUSTOMERS';
end;

procedure TvisCustomer_Read.MapRowToObject;
var
  temp : TCustomer;
begin
  temp := TCustomer.Create;
  temp.OID.AssignFromTIQuery('CUSTOMER_ID',Query);
  temp.Name := Query.FieldAsString['CUSTOMER_NAME'];
  temp.ObjectState := posClean;
  TCustomerList(Visited).Add(Temp);
end;

procedure TvisCustomer_Read.SetupParams;
begin
  // Do Nothing because there are no parameters to initialize in the query
end;

{ TVisCustomer_Create }

function TVisCustomer_Create.AcceptVisitor: boolean;
begin
  result := ( Visited is TCustomer ) and
            ( Visited.ObjectState = posCreate );
end;

procedure TVisCustomer_Create.Init;
begin
  Query.SQLText :=
    'insert into CUSTOMERS values(:OID, :CUSTOMER_NAME)';
end;

procedure TVisCustomer_Create.SetupParams;
var
  temp: TCustomer;
begin
  temp := TCustomer(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['CUSTOMER_NAME'] := temp.Name;
end;

{ TVisCustomer_Update }

function TVisCustomer_Update.AcceptVisitor: boolean;
begin
  result := ( Visited is TCustomer) and
            ( Visited.ObjectState = posUpdate );
end;

procedure TVisCustomer_Update.Init;
begin
  Query.SQLText :=
    'update CUSTOMERS set CUSTOMER_NAME = :CUSTOMER_NAME where CUSTOMER_ID = :OID';
end;

procedure TVisCustomer_Update.SetupParams;
var
  temp: TCustomer;
begin
  temp := TCustomer(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
  Query.ParamAsString['CUSTOMER_NAME'] := temp.Name;
end;

{ TVisCustomer_Delete }

function TVisCustomer_Delete.AcceptVisitor: boolean;
begin
  result := ( Visited is TCustomer) and
            ( Visited.ObjectState = posDelete );
end;

procedure TVisCustomer_Delete.Init;
begin
  Query.SQLText := 'delete from CUSTOMERS where CUSTOMER_ID = :OID';
end;

procedure TVisCustomer_Delete.SetupParams;
var
  temp: TCustomer;
begin
  temp := TCustomer(Visited);
  temp.OID.AssignToTIQuery('OID', Query);
end;

end.
