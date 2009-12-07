unit BOMUnitTests;

interface

uses
  TestFrameWork,
  RelationshipManagerUnit,
  BOMUnit;

type
  TOrderTests = class(TTestCase)
    private
      RM: TRelationshipManager;
      ListStrategy: TListStrategy;
      Customer: TCustomer;
      Order: TOrder;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure TestCreate;
      procedure TestAddOrder;
    end;

  TCustomerTests = class(TTestCase)
    private
      RM: TRelationshipManager;
      ListStrategy: TListStrategy;
      Customer: TCustomer;
      Order1: TOrder;
      Order2: TOrder;
    protected
      procedure SetUp; override;
      procedure TearDown; override;
    published
      procedure TestCreate;
      procedure TestAddOrder;
      procedure TestRemoveOrder;
    end;

implementation

uses
  SysUtils,
  Contnrs,
  Dialogs;
  
{ TOrderTests }

procedure TOrderTests.SetUp;
begin
  inherited;
  //We create non-owning lists because we take care of destroying the objects
  //in TearDown.
  OrderList := TOrderList.Create;
  OrderList.OwnsObjects := false;
  ListStrategy := TMyCustomListStrategy.Create;

  //For testing purposes, we create a RM that is not the default
  //(singleton) instance GetRelationshipManager.
  //This is to guarantee test independance.
  RM := TRelationshipManager.Create;
  RM.ListStrategy := ListStrategy;

  Customer := TCustomer.Create;
  Customer.RelationshipManager := RM;

  Order := TOrder.Create;
  Order.RelationshipManager := RM;
end;

procedure TOrderTests.TearDown;
begin
  FreeAndNil(OrderList);
  FreeAndNil(RM);
  FreeAndNil(ListStrategy);
  FreeAndNil(Order);
  FreeAndNil(Customer);
  inherited;
end;

procedure TOrderTests.TestCreate;
begin
  CheckNull(Order.Customer,'Customer not nil error');
end;

procedure TOrderTests.TestAddOrder;
begin
  Customer.AddOrder(Order);

  CheckTrue(Order.Customer = Customer,'Customer error');
end;

{ TCustomerTests }

procedure TCustomerTests.SetUp;
begin
  inherited;
  //We create non-owning lists because we take care of destroying the objects
  //in TearDown.
  CustomerList := TCustomerList.Create;
  CustomerList.OwnsObjects := false;
  OrderList := TOrderList.Create;
  OrderList.OwnsObjects := false;
  ListStrategy := TMyCustomListStrategy.Create;

  //For testing purposes, we create a RM that is not the default
  //(singleton) instance GetRelationshipManager.
  //This is to guarantee test independance.
  RM := TRelationshipManager.Create;
  RM.ListStrategy := ListStrategy;

  Customer := TCustomer.Create;
  Customer.RelationshipManager := RM;
  Order1 := TOrder.Create;
  Order1.RelationshipManager := RM;
  Order2 := TOrder.Create;
  Order2.RelationshipManager := RM;
end;

procedure TCustomerTests.TearDown;
begin
  FreeAndNil(CustomerList);
  FreeAndNil(OrderList);
  FreeAndNil(RM);
  FreeAndNil(ListStrategy);
  FreeAndNil(Customer);
  FreeAndNil(Order1);
  FreeAndNil(Order2);
  inherited;
end;

procedure TCustomerTests.TestAddOrder;
begin
  Customer.AddOrder(Order1);

  with Customer.Orders do
    begin
      CheckEquals(1, Count, 'Count error');
      CheckTrue(Items[0] = Order1, 'Items[0] error');
    end;
end;

procedure TCustomerTests.TestCreate;
begin
  CheckEquals(0, Customer.Orders.Count, 'Count error');
end;

procedure TCustomerTests.TestRemoveOrder;
begin
  Customer.AddOrder(Order1);
  Customer.AddOrder(Order2);

  Customer.RemoveOrder(Order1);

  with Customer.Orders do
    begin
      CheckEquals(1, Count, 'Count error');
      CheckTrue(Items[0] = Order2, 'Items[0] error');
    end;
end;

initialization

  TestFramework.RegisterTest('BOMUnitTests Suite',
    TOrderTests.Suite);

  TestFramework.RegisterTest('BOMUnitTests Suite',
    TCustomerTests.Suite);

end.
