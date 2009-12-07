unit BOMUnit;

//Firebird DB setup DDL:
//
//
//CREATE TABLE CUSTOMERS (
//  CUSTOMER_ID VARCHAR(36) NOT NULL,
//  CUSTOMER_NAME VARCHAR(40));
//
//
//ALTER TABLE CUSTOMERS ADD PRIMARY KEY (CUSTOMER_ID);
//
//
//CREATE TABLE ORDERS (
//  ORDER_ID VARCHAR(36) NOT NULL,
//  ORDER_DESCRIPTION VARCHAR(100),
//  ORDER_QUANTITY INTEGER);
//
//
//ALTER TABLE ORDERS ADD PRIMARY KEY (ORDER_ID);
//
//
//CREATE TABLE RELATIONSHIPS (
//  OID VARCHAR(36) NOT NULL,
//  FROM_OID VARCHAR(36),
//  TO_OID VARCHAR(36),
//  REL_TYPE INTEGER);
//
//
//ALTER TABLE RELATIONSHIPS ADD PRIMARY KEY (OID);

interface

uses
  tiObject,
  RelationshipManagerUnit;

type

  TCustomer = class;
  TOrder = class;

  TOrder = class(TBusinessObject)
    private
      FItemDescription: string;
      FQuantity: integer;
      procedure SetItemDescription(const Value: string);
      procedure SetQuantity(const Value: integer);
      function GetCustomer: TCustomer;
    protected
      procedure SetDeleted(const Value: boolean); override;
    public
      property Customer: TCustomer read GetCustomer;
    published
      property OrderDescription: string read FItemDescription write SetItemDescription;
      property Quantity: integer read FQuantity write SetQuantity;
  end;

  TOrderList = class(TtiObjectList)
    protected
      function GetItems(Index: integer): TOrder; reintroduce;
      procedure SetItems(Index: integer; const Value: TOrder); reintroduce;
    public
      property Items[Index: integer]: TOrder read GetItems write SetItems; default;
    end;

  TCustomer = class(TBusinessObject)
    private
      FName: string;
      FOrders: TOrderList;
      procedure SetName(const Value: string);
      function GetOrders: TOrderList;
    protected
      procedure SetDeleted(const Value: boolean); override;
    public
      constructor Create; override;
      procedure AddOrder(AOrder: TOrder);
      procedure RemoveOrder(AOrder: TOrder);
      property Orders: TOrderList read GetOrders;
    published
      property Name: string read FName write SetName;
  end;

  TCustomerList = class(TtiObjectList)
    protected
      function GetItems(Index: integer): TCustomer; reintroduce;
      procedure SetItems(Index: integer; const Value: TCustomer); reintroduce;
    public
      property Items[Index: integer]: TCustomer read GetItems write SetItems; default;
    end;

  TMyCustomListStrategy = class(TListStrategy)
    public
      function GetSourceList(ARelType: integer): TtiObjectList; override;
      function GetTargetList(ARelType: integer): TtiObjectList; override;
    end;

var
  CustomerList: TCustomerList;
  OrderList: TOrderList;

implementation

uses
  SysUtils;

const
  REL_TYPE_CUSTOMER_ORDER = 1;

{ TOrder }

function TOrder.GetCustomer: TCustomer;
begin
  result := TCustomer(RelationshipManager.FindSourceObject(Self,REL_TYPE_CUSTOMER_ORDER));
end;

procedure TOrder.SetDeleted(const Value: boolean);
begin
  RelationshipManager.DeleteObjectRelationships(Self);
  inherited SetDeleted(Value);
end;

procedure TOrder.SetItemDescription(const Value: string);
begin
  FItemDescription := Value;
end;

procedure TOrder.SetQuantity(const Value: integer);
begin
  FQuantity := Value;
end;

{ TOrderList }

function TOrderList.GetItems(Index: integer): TOrder;
begin
  result := TOrder(inherited GetItems(Index));
end;

procedure TOrderList.SetItems(Index: integer; const Value: TOrder);
begin
  inherited SetItems(Index,Value);
end;

{ TCustomer }

constructor TCustomer.Create;
begin
  inherited;
  //Create a non-owning list descending from TtiObjectList
  //The RelationshipManager.ListStrategy will decide who owns the objects
  FOrders := TOrderList.Create;
  FOrders.OwnsObjects := false;
end;

procedure TCustomer.AddOrder(AOrder: TOrder);
begin
  RelationshipManager.AddRelationship(Self,AOrder,REL_TYPE_CUSTOMER_ORDER);

  //Here we have a one to many relationship: orders do not exist without their
  //customer, so customer is in charge of adding the order to the order list.
  //In a many to many relationship, adding source and target objects to their
  //storage list is the responsibility of the object creator, and the line
  //below would not be required.
  RelationshipManager.GetTargetList(REL_TYPE_CUSTOMER_ORDER).Add(AOrder);
end;

procedure TCustomer.RemoveOrder(AOrder: TOrder);
begin
  AOrder.Deleted := true;
  RelationshipManager.RemoveRelationship(Self,AOrder,REL_TYPE_CUSTOMER_ORDER);
end;

function TCustomer.GetOrders: TOrderList;
begin
  RelationshipManager.FindTargetObjects(Self,REL_TYPE_CUSTOMER_ORDER,FOrders);
  result := FOrders;
end;

procedure TCustomer.SetDeleted(const Value: boolean);
var
  i: integer;
begin
  with Orders do
    begin
      for i := 0 to pred(Count) do
        TOrder(Items[i]).Deleted := true;
    end;
  RelationshipManager.DeleteObjectRelationships(Self);

  inherited SetDeleted(Value);
end;

procedure TCustomer.SetName(const Value: string);
begin
  FName := Value;
end;

{ TCustomerList }

function TCustomerList.GetItems(Index: integer): TCustomer;
begin
  result := TCustomer(inherited GetItems(Index));
end;

procedure TCustomerList.SetItems(Index: integer; const Value: TCustomer);
begin
  inherited SetItems(Index,Value);
end;

{ TMyCustomListStrategy }

function TMyCustomListStrategy.GetSourceList(ARelType: integer): TtiObjectList;
begin
  case ARelType of
    REL_TYPE_CUSTOMER_ORDER:
      //here we simply refer to a globally known CustomerList,
      //but we could imagine refering to a singleton, or other variations
      result := CustomerList;
    //other relationship types here...
  end;
end;

function TMyCustomListStrategy.GetTargetList(ARelType: integer): TtiObjectList;
begin
  case ARelType of
    REL_TYPE_CUSTOMER_ORDER:
      //here we simply refer to a globally known OrderList,
      //but we could imagine refering to a singleton, or other variations
      result := OrderList;
    //other relationship types here...
  end;
end;

end.
