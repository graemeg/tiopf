unit VendorListU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseListU, ActnList, StdCtrls, ExtCtrls, tiFocusPanel, tiVTListView,
  Buttons, tiObject, tiVirtualTrees;

type
  TfrmVendorList = class(TfrmBaseList)
    lvParts: TtiVTListView;
    procedure LVItemArrive(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
    procedure LVItemLeave(pVT: TtiCustomVirtualTree; AData: TtiObject;
      AItem: PVirtualNode);
  private
  protected
     procedure SetColumns; override;
    function CreateNewItem: TtiObject; override;
    function EditItem(AItem: TtiObject): boolean; override;
  public
    { Public declarations }
  end;

var
  frmVendorList: TfrmVendorList;

implementation

uses MastApp_BOM, VendorEditU;

{$R *.dfm}

{ TfrmCustomerList }

function TfrmVendorList.CreateNewItem: TtiObject;
begin
  result:= TVendor.CreateNew;
end;

function TfrmVendorList.EditItem(AItem: TtiObject): boolean;
begin
  Result:= TfrmVendor.Execute(AItem);
end;

procedure TfrmVendorList.LVItemArrive(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  lvParts.Data := ( AData as TVendor ).Parts ;
end;

procedure TfrmVendorList.LVItemLeave(pVT: TtiCustomVirtualTree;
  AData: TtiObject; AItem: PVirtualNode);
begin
  inherited;
  lvParts.Data := nil;
end;

procedure TfrmVendorList.SetColumns;
begin
  inherited;
  LV.AddColumn('VendorName', vttkString, 'Vendor', 150);
  LV.AddColumn('Address1', vttkString, 'Address 1', 150);
  LV.AddColumn('Address2', vttkString, 'Address 2', 150);
  LV.AddColumn('City', vttkString, 'City', 100);

  lvParts.AddColumn('Description', vttkString, 'Description', 150);
  lvParts.AddColumn('OnHand', vttkInt, 'On hand', 60);
  lvParts.AddColumn('OnOrder', vttkInt, 'On Order', 60);
  lvParts.AddColumn('Cost', vttkCurrency, 'Cost', 75);
  lvParts.AddColumn('ListPrice', vttkCurrency, 'List Price', 75);
end;

end.
