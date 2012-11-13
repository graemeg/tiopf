unit frmContactMaint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_edit,
  fpg_widget, fpg_form, fpg_label, fpg_button,
  fpg_listview, fpg_memo, fpg_panel,
  model, tiModelMediator;

type

  TContactEditForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: ContactEditForm}
    lblName1: TfpgLabel;
    edFName: TfpgEdit;
    lblName2: TfpgLabel;
    edLName: TfpgEdit;
    lblName3: TfpgLabel;
    edEmail: TfpgEdit;
    lblName4: TfpgLabel;
    edMobile: TfpgEdit;
    lblName5: TfpgLabel;
    meComments: TfpgMemo;
    lblName6: TfpgLabel;
    lvAddresses: TfpgListView;
    btnSave: TfpgButton;
    btnCancel: TfpgButton;
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    btnDebug: TfpgButton;
    ImgPanel: TfpgImagePanel;
    {@VFD_HEAD_END: ContactEditForm}
    FData: TContact;
    FMediator: TtiModelMediator;
    FAdrsMediator: TtiModelMediator;
    procedure SetData(const AValue: TContact);
    procedure SetupMediators;
    procedure btnDebugClicked(Sender: TObject);
    procedure btnEditClicked(Sender: TObject);
    procedure btnAddClicked(Sender: TObject);
    procedure btnDelClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
    property  Data: TContact read FData write SetData;
  end;

{@VFD_NEWFORM_DECL}

function EditContact(AData: TContact): Boolean;


implementation

uses
  tiDialogs,
  tiListMediators,
  tiBaseMediator,
  fpg_imgfmt_png,
  contactmanager,
  contact_views,
  frmAddressMaint;


function EditContact(AData: TContact): Boolean;
var
  frm: TContactEditForm;
begin
  frm:= TContactEditForm.Create(nil);
  try
    frm.SetData(AData);
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;


{@VFD_NEWFORM_IMPL}

procedure TContactEditForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.AddProperty('FirstName', edFName);
    FMediator.AddProperty('LastName', edLName);
    FMediator.AddProperty('EMail', edEmail);
    FMediator.AddProperty('Mobile', edMobile);
    FMediator.AddProperty('Comments', meComments);
    FMediator.AddProperty('Photo', ImgPanel);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;

  if not Assigned(FAdrsMediator) then
  begin
    FAdrsMediator := TtiModelMediator.Create(self);
    FAdrsMediator.AddComposite({'AddressType.Name;}'AddressType4GUI(50,"Type");Nr;Street;Telephone1', lvAddresses);
  end;
  FAdrsMediator.Subject := FData.AddressList;
  FAdrsMediator.Active := True;
end;

procedure TContactEditForm.btnDebugClicked(Sender: TObject);
begin
  tiShowString(FData.AsDebugString);
end;

procedure TContactEditForm.btnEditClicked(Sender: TObject);
var
  obj: TAddress;
begin
  obj := TAddress(TtiListViewMediatorView(FAdrsMediator.FindByComponent(lvAddresses).Mediator).SelectedObject);
  tiShowString(obj.AsDebugString);

  if not Assigned(obj) then
    Exit; //==>

  if EditAddress(obj) then
  begin
    // do nothing
  end;
end;

procedure TContactEditForm.btnAddClicked(Sender: TObject);
var
  obj: TAddress;
begin
  obj := TAddress.CreateNew;
  if Assigned(obj) then
  begin
    if EditAddress(obj) then
      FData.AddressList.Add(obj)
    else
      obj.Free;
  end;
end;

procedure TContactEditForm.btnDelClicked(Sender: TObject);
var
  obj: TAddress;
begin
  obj := TAddress(TtiListViewMediatorView(FAdrsMediator.FindByComponent(lvAddresses).Mediator).SelectedObject);
  if Assigned(obj) then
  begin
    if tiAppConfirmation('Are you sure you want to delete this item?') then
    begin
      obj.Deleted := True;
    end;
  end;

end;

procedure TContactEditForm.SetData(const AValue: TContact);
begin
  if FData=AValue then exit;
  FData:=AValue;
  SetupMediators;
end;

procedure TContactEditForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: ContactEditForm}
  Name := 'ContactEditForm';
  SetPosition(513, 423, 537, 414);
  WindowTitle := 'Contact Edit Form';
  Hint := '';

  lblName1 := TfpgLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Firstname:';
  end;

  edFName := TfpgEdit.Create(self);
  with edFName do
  begin
    Name := 'edFName';
    SetPosition(8, 24, 216, 22);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 1;
    Text := '';
  end;

  lblName2 := TfpgLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 52, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Lastname:';
  end;

  edLName := TfpgEdit.Create(self);
  with edLName do
  begin
    Name := 'edLName';
    SetPosition(8, 68, 216, 22);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 3;
    Text := '';
  end;

  lblName3 := TfpgLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 96, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'EMail:';
  end;

  edEmail := TfpgEdit.Create(self);
  with edEmail do
  begin
    Name := 'edEmail';
    SetPosition(8, 112, 216, 22);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 5;
    Text := '';
  end;

  lblName4 := TfpgLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(8, 140, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Mobile:';
  end;

  edMobile := TfpgEdit.Create(self);
  with edMobile do
  begin
    Name := 'edMobile';
    SetPosition(8, 156, 216, 22);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 7;
    Text := '';
  end;

  lblName5 := TfpgLabel.Create(self);
  with lblName5 do
  begin
    Name := 'lblName5';
    SetPosition(8, 184, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Comments:';
  end;

  meComments := TfpgMemo.Create(self);
  with meComments do
  begin
    Name := 'meComments';
    SetPosition(8, 200, 216, 80);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 9;
    Lines.text := 'Hello';
  end;

  lblName6 := TfpgLabel.Create(self);
  with lblName6 do
  begin
    Name := 'lblName6';
    SetPosition(264, 140, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Addresses:';
  end;

  lvAddresses := TfpgListView.Create(self);
  with lvAddresses do
  begin
    Name := 'lvAddresses';
    SetPosition(264, 156, 260, 124);
    Hint := '';
    MultiSelect := False;
    ShowHeaders := True;
    TabOrder := 11;
  end;

  btnSave := TfpgButton.Create(self);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(364, 380, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Save';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 12;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(448, 380, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 13;
  end;

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(264, 284, 52, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 14;
    OnClick := @btnAddClicked;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(320, 284, 52, 24);
    Text := 'Edit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 15;
    OnClick := @btnEditClicked;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(376, 284, 52, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 16;
    OnClick := @btnDelClicked;
  end;

  btnDebug := TfpgButton.Create(self);
  with btnDebug do
  begin
    Name := 'btnDebug';
    SetPosition(8, 380, 100, 24);
    Anchors := [anLeft,anBottom];
    Text := 'Debug (Show)';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 17;
    OnClick := @btnDebugClicked;
  end;

  ImgPanel := TfpgImagePanel.Create(self);
  with ImgPanel do
  begin
    Name := 'ImgPanel';
    SetPosition(416, 12, 108, 108);
    Anchors := [anRight,anTop];
  end;

  {@VFD_BODY_END: ContactEditForm}
end;

initialization
  // For more specific behaviour
  gMediatorManager.RegisterMediator(TContactPhotoMediator, TContact, 'Photo');

end.
