unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_widget, fpg_form, fpg_button,
  fpg_grid, fpg_checkbox, fpg_panel, fpg_listview, fpg_listbox, fpg_label,
  Model, tiModelMediator;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    lvName1: TfpgListView;
    grdName1: TfpgStringGrid;
    pnlName1: TfpgPanel;
    btnAddViaCode: TfpgButton;
    btnChange: TfpgButton;
    btnShowModel: TfpgButton;
    btnDelete: TfpgButton;
    btnQuit: TfpgButton;
    chkShowDeleted: TfpgCheckBox;
    lstName1: TfpgListBox;
    Label1: TfpgLabel;
    Label2: TfpgLabel;
    Label3: TfpgLabel;
    {@VFD_HEAD_END: MainForm}
    FPersonList: TPersonList;       // The subject of our mediator.
    FMediator: TtiModelMediator;
    procedure   btnViaCodeChangeClick(Sender: TObject);
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnViaCodeAddClick(Sender: TObject);
    procedure   btnShowModelClick(Sender: TObject);
    procedure   cbShowDeletedClick(Sender: TObject);
    procedure   btnDeletedClicked(Sender: TObject);
    procedure   SetupMediators;
    procedure   FormShow(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiBaseMediator, tiListMediators, tiDialogs;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnViaCodeChangeClick(Sender: TObject);
begin
  { The BeginUpdate/EndUpdate will let the Item notify its observers
    only once, even though two change where made.
    Note:
    This is for observers to the Item, not the List that the Item belongs to! }
  FPersonList.Items[1].BeginUpdate;
  FPersonList.Items[1].Name := 'I have changed via code';
  FPersonList.Items[1].Age  := 99;
  FPersonList.Items[1].EndUpdate;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnViaCodeAddClick(Sender: TObject);
var
  lData: TPerson;
begin
  lData := TPerson.Create;
  lData.Name := 'I am new';
  lData.Age := 44;
  FPersonList.Add(lData);
end;

procedure TMainForm.btnShowModelClick(Sender: TObject);
begin
  tiShowString(FPersonList.AsDebugString);
end;

procedure TMainForm.cbShowDeletedClick(Sender: TObject);
var
  med: TtiMediatorView;
begin
  med := FMediator.FindByComponent(grdName1).Mediator;
  TtiCustomListMediatorView(med).ShowDeleted := chkShowDeleted.Checked;

  med := FMediator.FindByComponent(lvName1).Mediator;
  TtiCustomListMediatorView(med).ShowDeleted := chkShowDeleted.Checked;

  med := FMediator.FindByComponent(lstName1).Mediator;
  TtiCustomListMediatorView(med).ShowDeleted := chkShowDeleted.Checked;
end;

procedure TMainForm.btnDeletedClicked(Sender: TObject);
var
  med: TtiMediatorView;
begin
  med := FMediator.FindByComponent(grdName1).Mediator;
  TtiStringGridMediatorView(med).SelectedObject.Deleted := True;
  FPersonList.NotifyObservers;
end;

procedure TMainForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.Name := 'DemoFormMediator';
    FMediator.AddComposite('Caption(150,"Name",<);Age(50,"Age",>);GenderGUI(80,"Gender",|)', grdName1);
    FMediator.AddComposite('Caption(150,"Name",<);Age(55,"Age",>);GenderGUI(65,"Gender",|)', lvName1);
    { In the following line of code 'Name' refers to the TPerson.Name property.
      We could also have left the ADisplayNames property empty, which meant it
      would then default to TPerson.Caption }
    FMediator.AddComposite('Name', lstName1);
  end;
  FMediator.Subject := FPersonList;
  FMediator.Active := True;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPersonList := GeneratePersonList;
end;

destructor TMainForm.Destroy;
begin
  FMediator.Active := False;
  FPersonList.Free;
  inherited Destroy;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  SetupMediators;
  FPersonList.NotifyObservers;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(460, 192, 617, 422);
  WindowTitle := 'Demo 20: ListMediators';
  Hint := '';
  IconName := '';
  OnShow := @FormShow;

  lvName1 := TfpgListView.Create(self);
  with lvName1 do
  begin
    Name := 'lvName1';
    SetPosition(8, 72, 292, 122);
    Hint := '';
    MultiSelect := False;
    ShowHeaders := True;
    TabOrder := 1;
  end;

  grdName1 := TfpgStringGrid.Create(self);
  with grdName1 do
  begin
    Name := 'grdName1';
    SetPosition(316, 72, 292, 265);
    BackgroundColor := TfpgColor($80000002);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 1;
    Tag := 99;
  end;

  pnlName1 := TfpgPanel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(8, 12, 600, 28);
    Anchors := [anLeft,anRight,anTop];
    Alignment := taLeftJustify;
    BackgroundColor := TfpgColor($FF000080);
    FontDesc := '#Label2';
    Hint := '';
    Margin := 8;
    Text := 'ListMediator Demo';
    TextColor := TfpgColor($FFFFFBF0);
  end;

  btnAddViaCode := TfpgButton.Create(self);
  with btnAddViaCode do
  begin
    Name := 'btnAddViaCode';
    SetPosition(8, 361, 120, 24);
    Text := 'Add via Code';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnViaCodeAddClick;
  end;

  btnChange := TfpgButton.Create(self);
  with btnChange do
  begin
    Name := 'btnChange';
    SetPosition(8, 389, 120, 24);
    Text := 'Change via Code';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnViaCodeChangeClick;
  end;

  btnShowModel := TfpgButton.Create(self);
  with btnShowModel do
  begin
    Name := 'btnShowModel';
    SetPosition(132, 389, 96, 24);
    Text := 'Show Model';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    OnClick := @btnShowModelClick;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(232, 389, 80, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 6;
    OnClick := @btnDeletedClicked;
  end;

  btnQuit := TfpgButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(528, 389, 80, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    OnClick := @btnQuitClicked;
  end;

  chkShowDeleted := TfpgCheckBox.Create(self);
  with chkShowDeleted do
  begin
    Name := 'cbShowDeleted';
    SetPosition(148, 361, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 8;
    Text := 'Show Deleted';
    OnChange := @cbShowDeletedClick;
  end;

  lstName1 := TfpgListBox.Create(self);
  with lstName1 do
  begin
    Name := 'lstName1';
    SetPosition(8, 217, 292, 120);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 9;
  end;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(10, 55, 215, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'ListView:';
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(318, 55, 205, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'StringGrid:';
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(10, 200, 145, 15);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Listbox:';
  end;

  {@VFD_BODY_END: MainForm}
end;


initialization
  RegisterFallbackListMediators;

end.
