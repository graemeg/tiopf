unit customer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tiObject;

type
  // forward declaration
  TCustomerList = class;


  TCustomer = class(TtiObject)
  private
    FFirstname: String;
    FLastname: String;
    FPhone: String;
    procedure   SetFirstname(AValue: String);
    procedure   SetLastname(AValue: String);
    procedure   SetPhone(AValue: String);
  protected
    function    GetOwner: TCustomerList; reintroduce;
    procedure   SetOwner(const Value: TCustomerList); reintroduce;
    function    GetCaption: string; override;
  public
    property    Owner: TCustomerList read GetOwner write SetOwner;
    procedure   Read; override;
    procedure   Save; override;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; overload; override;
  published
    property    Firstname: String read FFirstname write SetFirstname;
    property    Lastname: String read FLastname write SetLastname;
    property    Phone: String read FPhone write SetPhone;
  end;


  TCustomerList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TCustomer; reintroduce;
    procedure   SetItems(i: integer; const Value: TCustomer); reintroduce;
  public
    property    Items[i:integer]: TCustomer read GetItems write SetItems;
    function    Add(const AObject: TCustomer): integer; reintroduce;
    procedure   Read; overload; override;
  end;


implementation

uses
  tiOPFManager;

{ TCustomer }

procedure TCustomer.SetFirstname(AValue: String);
begin
  if FFirstname = AValue then Exit;
  BeginUpdate;
  FFirstname := AValue;
  EndUpdate;
end;

procedure TCustomer.SetLastname(AValue: String);
begin
  if FLastname = AValue then Exit;
  BeginUpdate;
  FLastname := AValue;
  EndUpdate;
end;

procedure TCustomer.SetPhone(AValue: String);
begin
  if FPhone = AValue then Exit;
  BeginUpdate;
  FPhone := AValue;
  EndUpdate;
end;

function TCustomer.GetOwner: TCustomerList;
begin
  result := TCustomerList(inherited GetOwner);
end;

procedure TCustomer.SetOwner(const Value: TCustomerList);
begin
  inherited SetOwner(Value);
end;

function TCustomer.GetCaption: string;
begin
  Result := Firstname + ' ' + Lastname;
end;

procedure TCustomer.Read;
begin
  GTIOPFManager.VisitorManager.Execute('customer_read', self);
end;

procedure TCustomer.Save;
begin
  GTIOPFManager.VisitorManager.Execute('customer_save', self);
end;

function TCustomer.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  Result := inherited IsValid(AErrors);
end;

{ TCustomerList }

function TCustomerList.GetItems(i: integer): TCustomer;
begin
  result := TCustomer(inherited GetItems(i));
end;

procedure TCustomerList.SetItems(i: integer; const Value: TCustomer);
begin
  inherited SetItems(i, Value);
end;

function TCustomerList.Add(const AObject: TCustomer): integer;
begin
  result := inherited Add(AObject);
end;

procedure TCustomerList.Read;
begin
  GTIOPFManager.VisitorManager.Execute('customer_readlist', self);
end;

end.

