unit PartsListU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BaseListU, ActnList, StdCtrls, ExtCtrls, tiFocusPanel, tiVTListView,
  Buttons, tiObject;

type
  TfrmPartsList = class(TfrmBaseList)
  private
  protected
     procedure SetColumns; override;
    function CreateNewItem: TtiObject; override;
    function EditItem(AItem: TtiObject): boolean; override;
  public
    { Public declarations }
  end;

var
  frmPartsList: TfrmPartsList;

implementation

uses MastApp_BOM, CustomerEditU, PartEditU;

{$R *.dfm}

{ TfrmCustomerList }

function TfrmPartsList.CreateNewItem: TtiObject;
begin
  result:= TPart.CreateNew;
end;

function TfrmPartsList.EditItem(AItem: TtiObject): boolean;
begin
  Result:= TfrmPart.Execute(AItem);

end;

procedure TfrmPartsList.SetColumns;
begin
  inherited;
  LV.AddColumn('PartNo', vttkInt, 'Part No', 80);
  LV.AddColumn('Description', vttkString, 'Description', 150);
  LV.AddColumn('OnHand', vttkInt, 'On hand', 60);
  LV.AddColumn('OnOrder', vttkInt, 'On Order', 60);
  LV.AddColumn('Cost', vttkCurrency, 'Cost', 75);
  LV.AddColumn('ListPrice', vttkCurrency, 'List Price', 75);

end;

end.
