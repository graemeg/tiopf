unit order;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  tiObject,
  customer;

type
  // forward declaration
  TOrderList = class;

  TOrder = class(TtiObject)
  private
    FOrderDate: TDateTime;
    FSoldBy: string;
    function GetOrderDateAsString: string;
    function    GetOrderID: string;
    procedure   SetOrderDate(AValue: TDateTime);
    procedure   SetSoldBy(AValue: string);
  protected
    function    GetOwner: TOrderList; reintroduce;
    procedure   SetOwner(const Value: TOrderList); reintroduce;
  public
    property    Owner: TOrderList read GetOwner write SetOwner;
  published
    property    OrderDate: TDateTime read FOrderDate write SetOrderDate;
    property    SoldBy: string read FSoldBy write SetSoldBy;
    property    OrderID: string read GetOrderID;
    property    OrderDateAsString: string read GetOrderDateAsString;
  end;


  TOrderList = class(TtiObjectList)
  private
    FCustomer: TCustomer;
  protected
    function    GetItems(i: integer): TOrder; reintroduce;
    procedure   SetItems(i: integer; const Value: TOrder); reintroduce;
  public
    property    Items[i:integer]: TOrder read GetItems write SetItems;
    function    Add(const AObject: TOrder): integer; reintroduce;
    procedure   Read; overload; override;
    property    Customer: TCustomer read FCustomer write FCustomer;
  end;


implementation

uses
  tiOPFManager;

{ TOrder }

procedure TOrder.SetOrderDate(AValue: TDateTime);
begin
  if FOrderDate = AValue then Exit;
  BeginUpdate;
  FOrderDate := AValue;
  EndUpdate;
end;

function TOrder.GetOrderID: string;
begin
  Result := OID.AsString;
end;

function TOrder.GetOrderDateAsString: string;
begin
  Result := FormatDateTime('dd MMM yyyy', FOrderDate);
end;

procedure TOrder.SetSoldBy(AValue: string);
begin
  if FSoldBy = AValue then Exit;
  BeginUpdate;
  FSoldBy := AValue;
  EndUpdate;
end;

function TOrder.GetOwner: TOrderList;
begin
  result := TOrderList(inherited GetOwner);
end;

procedure TOrder.SetOwner(const Value: TOrderList);
begin
  inherited SetOwner(Value);
end;

{ TOrderList }

function TOrderList.GetItems(i: integer): TOrder;
begin
  result := TOrder(inherited GetItems(i));
end;

procedure TOrderList.SetItems(i: integer; const Value: TOrder);
begin
  inherited SetItems(i, Value);
end;

function TOrderList.Add(const AObject: TOrder): integer;
begin
  result := inherited Add(AObject);
end;

procedure TOrderList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('order_readlist', self);
end;

end.

