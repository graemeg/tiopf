unit frm_orderdetails;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fpg_base, fpg_main, fpg_form, fpg_grid, fpg_button,
  tiModelMediator,
  order, orderline;

type

  TOrderDetailsForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: OrderDetailsForm}
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    btnHelp: TfpgButton;
    grdOrderDetails: TfpgStringGrid;
    btnClose: TfpgButton;
    btnDispatch: TfpgButton;
    {@VFD_HEAD_END: OrderDetailsForm}
    FMediator: TtiModelMediator;
    FData: TOrderLineList;
    procedure   SetupMediators;
    procedure   FormShow(Sender: TObject);
    procedure   btnAddClicked(Sender: TObject);
    procedure   btnEditClicked(Sender: TObject);
    procedure   btnDeleteClicked(Sender: TObject);
    procedure   btnHelpClicked(Sender: TObject);
    procedure   btnCloseClicked(Sender: TObject);
    procedure   btnDispatchClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

procedure ShowOrderDetails(const AOrder: TOrder);

implementation

uses
  tiLog
  ;

procedure ShowOrderDetails(const AOrder: TOrder);
var
  frm: TOrderDetailsForm;
begin
  frm := TOrderDetailsForm.Create(nil);
  try
    frm.FData.Order := AOrder;
    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

{@VFD_NEWFORM_IMPL}

procedure TOrderDetailsForm.btnAddClicked(Sender: TObject);
begin

end;

procedure TOrderDetailsForm.btnEditClicked(Sender: TObject);
begin

end;

procedure TOrderDetailsForm.btnDeleteClicked(Sender: TObject);
begin

end;

procedure TOrderDetailsForm.btnHelpClicked(Sender: TObject);
begin

end;

procedure TOrderDetailsForm.btnCloseClicked(Sender: TObject);
begin
  Close;
end;

procedure TOrderDetailsForm.btnDispatchClicked(Sender: TObject);
begin

end;

constructor TOrderDetailsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData := TOrderLineList.Create;
end;

procedure TOrderDetailsForm.SetupMediators;
begin
  FMediator := TtiModelMediator.Create(self);
  FMediator.AddComposite('DisplayProduct(200,"");DisplayUnitPrice(85,"Unit Price",>);DisplayQuantity(120,"Quantity",>)', grdOrderDetails);
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

procedure TOrderDetailsForm.FormShow(Sender: TObject);
begin
  FData.Read;
  SetupMediators;
  Log('OrderLine count = ' + IntToStr(FData.Count));
end;

destructor TOrderDetailsForm.Destroy;
begin
  FMediator.Active := False;
  FData.Free;
  inherited Destroy;
end;

procedure TOrderDetailsForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' }
  {@VFD_BODY_BEGIN: OrderDetailsForm}
  Name := 'OrderDetailsForm';
  SetPosition(521, 216, 511, 260);
  WindowTitle := 'Order Details';
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

  grdOrderDetails := TfpgStringGrid.Create(self);
  with grdOrderDetails do
  begin
    Name := 'grdOrderDetails';
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

  btnDispatch := TfpgButton.Create(self);
  with btnDispatch do
  begin
    Name := 'btnDispatch';
    SetPosition(440, 10, 60, 23);
    Anchors := [anRight,anTop];
    Text := 'Dispatch';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnDispatchClicked;
  end;

  {@VFD_BODY_END: OrderDetailsForm}
  {%endregion}
end;


end.
