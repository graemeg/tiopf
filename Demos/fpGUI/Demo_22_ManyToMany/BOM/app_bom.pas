unit app_bom;

{$mode objfpc}{$H+}

interface

uses
  customer
  ,product
  ,tiBaseObject
  ;

type
  TDemoApp = class(TtiBaseObject)
  private
    FCustomerList: TCustomerList;
    FProductList: TProductList;
  public
    constructor Create;
    destructor  Destroy; override;
    property    CustomerList: TCustomerList read FCustomerList;
    property    ProductList: TProductList read FProductList;
  end;


// singleton of application object
function gDemoApp: TDemoApp;

implementation

var
  uDemoApp: TDemoApp;

function gDemoApp: TDemoApp;
begin
  if not Assigned(uDemoApp) then
    uDemoApp := TDemoApp.Create;
  Result := uDemoApp;
end;


{ TDemoApp }

constructor TDemoApp.Create;
begin
  inherited Create;
  FCustomerList := TCustomerList.Create;
  FProductList := TProductList.Create;
end;

destructor TDemoApp.Destroy;
begin
  FProductList.Free;
  FCustomerList.Free;
  inherited Destroy;
end;


initialization
finalization
  uDemoApp.Free;

end.

