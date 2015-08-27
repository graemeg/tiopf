unit frm_orders;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_grid, fpg_button,
  tiModelMediator,
  customer, order;

type

  TOrderListForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: OrderListForm}
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    btnHelp: TfpgButton;
    grdOrders: TfpgStringGrid;
    btnClose: TfpgButton;
    {@VFD_HEAD_END: OrderListForm}
    FMediator: TtiModelMediator;
    FData: TOrderList;
    procedure   SetupMediators;
    procedure   FormShow(Sender: TObject);
    procedure   btnAddClicked(Sender: TObject);
    procedure   btnEditClicked(Sender: TObject);
    procedure   btnDeleteClicked(Sender: TObject);
    procedure   btnHelpClicked(Sender: TObject);
    procedure   btnCloseClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure ShowOrders(const ACustomer: TCustomer);

implementation

uses
  tiLog
  ;

procedure ShowOrders(const ACustomer: TCustomer);
var
  frm: TOrderListForm;
begin
  frm := TOrderListForm.Create(nil);
  try
    frm.FData.Customer := ACustomer;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TOrderListForm.btnAddClicked(Sender: TObject);
begin

end;

procedure TOrderListForm.btnEditClicked(Sender: TObject);
begin

end;

procedure TOrderListForm.btnDeleteClicked(Sender: TObject);
begin

end;

procedure TOrderListForm.btnHelpClicked(Sender: TObject);
begin

end;

procedure TOrderListForm.btnCloseClicked(Sender: TObject);
begin
  Close;
end;

constructor TOrderListForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TOrderList.Create;
end;

procedure TOrderListForm.SetupMediators;
begin
  FMediator := TtiModelMediator.Create(self);
  FMediator.AddComposite('OrderID(80,"No.");OrderDateAsString(120,"Order Date");SoldBy(85)', grdOrders);
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

procedure TOrderListForm.FormShow(Sender: TObject);
begin
  FData.Read;
  SetupMediators;
  Log('Order count = ' + IntToStr(FData.Count));
end;

destructor TOrderListForm.Destroy;
begin
  FMediator.Active := False;
  FData.Free;
  inherited Destroy;
end;

procedure TOrderListForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: OrderListForm}
  Name := 'OrderListForm';
  SetPosition(521, 216, 511, 260);
  WindowTitle := 'Orders';
  Hint := '';
  IconName := '';
  ShowHint := True;
  WindowPosition := wpOneThirdDown;
  OnShow := @FormShow;

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(12, 8, 92, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.add';
    TabOrder := 2;
    OnClick := @btnAddClicked;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(106, 8, 92, 24);
    Text := 'Edit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.edit';
    TabOrder := 3;
    OnClick := @btnEditClicked;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(200, 8, 92, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := 'stdimg.remove';
    TabOrder := 4;
    OnClick := @btnDeleteClicked;
  end;

  btnHelp := TfpgButton.Create(self);
  with btnHelp do
  begin
    Name := 'btnHelp';
    SetPosition(12, 230, 24, 24);
    Anchors := [anLeft,anBottom];
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'stdimg.help';
    TabOrder := 5;
    OnClick := @btnHelpClicked;
  end;

  grdOrders := TfpgStringGrid.Create(self);
  with grdOrders do
  begin
    Name := 'grdOrders';
    SetPosition(10, 35, 491, 187);
    Anchors := [anLeft,anRight,anTop,anBottom];
    BackgroundColor := TfpgColor($80000002);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 1;
  end;

  btnClose := TfpgButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(420, 230, 80, 23);
    Text := 'Close';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnCloseClicked;
  end;

  {@VFD_BODY_END: OrderListForm}
  {%endregion}
end;


end.
