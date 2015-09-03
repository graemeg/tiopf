unit frm_neworder;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form,
  fpg_button, fpg_label, fpg_checkbox, fpg_edit,
  fpg_grid, fpg_combobox, fpg_popupcalendar,
  tiModelMediator,
  order, orderline, customer;

type

  TNewOrderForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: NewOrderForm}
    btnHelp: TfpgButton;
    grdOrderDetails: TfpgStringGrid;
    btnOK: TfpgButton;
    btnAddLine: TfpgButton;
    btnRemoveLine: TfpgButton;
    lblCustomer: TfpgLabel;
    cbCustomer: TfpgComboBox;
    lblDate: TfpgLabel;
    calOrderDate: TfpgCalendarCombo;
    lblSoldBy: TfpgLabel;
    edtSoldBy: TfpgEdit;
    cbDispatched: TfpgCheckBox;
    edtTotalAmount: TfpgEdit;
    lblAmount: TfpgLabel;
    lblDispated: TfpgLabel;
    btnCancel: TfpgButton;
    {@VFD_HEAD_END: NewOrderForm}
    FMediator: TtiModelMediator;
    FOrder: TOrder;
    FOrderLineList: TOrderLineList;
    procedure   SetupMediators;
    procedure   FormShow(Sender: TObject);
    procedure   btnAddLineClicked(Sender: TObject);
  public
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure ShowNewOrder;

implementation

uses
  app_bom;

{@VFD_NEWFORM_IMPL}

procedure ShowNewOrder;
var
  frm: TNewOrderForm;
begin
  frm := TNewOrderForm.Create(nil);
  try
    if frm.ShowModal = mrOK then
    begin
//      frm.FOrder.Save;
    end;
    frm.FOrder.Free;
  finally
    frm.Free;
  end;
end;

procedure TNewOrderForm.SetupMediators;
begin
  FMediator := TtiModelMediator.Create(self);
  FMediator.AddComposite('DisplayProduct(200,"Description");DisplayUnitPrice(85,"Unit Price",>);DisplayQuantity(80,"Quantity",>);DisplayTotalPrice(85,"Total",>)', grdOrderDetails);
  FMediator.Subject := FOrderLineList;
  FMediator.Active := True;
end;

procedure TNewOrderForm.FormShow(Sender: TObject);
begin
  FOrder := TOrder.CreateNew;
  SetupMediators;
end;

procedure TNewOrderForm.btnAddLineClicked(Sender: TObject);
var
  lLine: TOrderLine;
begin
  lLine := TOrderLine.CreateNew;

  FOrderLineList.Add(lLine);
end;

procedure TNewOrderForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: NewOrderForm}
  Name := 'NewOrderForm';
  SetPosition(521, 216, 627, 289);
  WindowTitle := 'New Order';
  Hint := '';
  IconName := '';
  ShowHint := True;
  WindowPosition := wpOneThirdDown;
  OnShow := @FormShow;

  btnHelp := TfpgButton.Create(self);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(12, 259, 24, 24);
    Anchors := [anLeft,anBottom];
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.help';
    TabOrder := 5;
  end;

  grdOrderDetails := TfpgStringGrid.Create(self);
  with grdOrderDetails do
  begin
    Name := 'grdOrderDetails';
    SetPosition(10, 129, 587, 122);
    Anchors := [anLeft,anRight,anBottom];
    BackgroundColor := TfpgColor($80000002);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 1;
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(456, 259, 80, 23);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 6;
  end;

  btnAddLine := TfpgButton.Create(self);
  with btnAddLine do
  begin
    Name := 'btnAddLine';
    SetPosition(600, 129, 23, 23);
    Anchors := [anRight,anBottom];
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.add';
    TabOrder := 5;
  end;

  btnRemoveLine := TfpgButton.Create(self);
  with btnRemoveLine do
  begin
    Name := 'btnRemoveLine';
    SetPosition(600, 152, 23, 23);
    Anchors := [anRight,anBottom];
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.remove';
    TabOrder := 6;
  end;

  lblCustomer := TfpgLabel.Create(self);
  with lblCustomer do
  begin
    Name := 'lblCustomer';
    SetPosition(10, 10, 70, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Customer:';
  end;

  cbCustomer := TfpgComboBox.Create(self);
  with cbCustomer do
  begin
    Name := 'cbCustomer';
    SetPosition(85, 5, 215, 24);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    FocusItem := -1;
    TabOrder := 8;
  end;

  lblDate := TfpgLabel.Create(self);
  with lblDate do
  begin
    Name := 'lblDate';
    SetPosition(10, 40, 70, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Date:';
  end;

  calOrderDate := TfpgCalendarCombo.Create(self);
  with calOrderDate do
  begin
    Name := 'calOrderDate';
    SetPosition(85, 35, 120, 24);
    BackgroundColor := TfpgColor($80000002);
    DateFormat := 'dd MMM yyy';
    DayColor := TfpgColor($000000);
    FontDesc := '#List';
    Hint := '';
    HolidayColor := TfpgColor($000000);
    SelectedColor := TfpgColor($000000);
    SingleClickSelect := True;
    TabOrder := 10;
  end;

  lblSoldBy := TfpgLabel.Create(self);
  with lblSoldBy do
  begin
    Name := 'lblSoldBy';
    SetPosition(10, 70, 70, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Sold By:';
  end;

  edtSoldBy := TfpgEdit.Create(self);
  with edtSoldBy do
  begin
    Name := 'edtSoldBy';
    SetPosition(85, 65, 215, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 12;
    Text := '';
  end;

  cbDispatched := TfpgCheckBox.Create(self);
  with cbDispatched do
  begin
    Name := 'cbDispatched';
    SetPosition(85, 95, 120, 19);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 12;
    Text := 'Dispatched';
  end;

  edtTotalAmount := TfpgEdit.Create(self);
  with edtTotalAmount do
  begin
    Name := 'edtTotalAmount';
    SetPosition(512, 35, 85, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 13;
    Text := '';
    ReadOnly := True;
  end;

  lblAmount := TfpgLabel.Create(self);
  with lblAmount do
  begin
    Name := 'lblAmount';
    SetPosition(425, 40, 80, 15);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Amount:';
  end;

  lblDispated := TfpgLabel.Create(self);
  with lblDispated do
  begin
    Name := 'lblDispated';
    SetPosition(495, 10, 100, 15);
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Text := 'DISPATCHED';
    TextColor := TfpgColor($FF800000);
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(540, 259, 80, 23);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 16;
  end;

  {@VFD_BODY_END: NewOrderForm}
  {%endregion}
end;



end.
