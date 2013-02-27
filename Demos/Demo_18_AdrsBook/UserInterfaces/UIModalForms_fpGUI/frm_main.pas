unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_grid,
  tiModelMediator;

type

  TFormMain = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: FormMain}
    btnAdd: TfpgButton;
    btnEdit: TfpgButton;
    btnDelete: TfpgButton;
    Grid1: TfpgStringGrid;
    btnRefresh: TfpgButton;
    {@VFD_HEAD_END: FormMain}
    FMediator: TtiModelMediator;
    procedure Refresh;
    procedure SetupMediators;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiOPFManager,
  tiBaseMediator,
  tiMediators,
  tiListMediators,
  Adrs_BOM,
  Adrs_Dependencies,
  Adrs_Singleton,
//  FPersonEdit,
  tiGUIUtils,
  tiGUIINI;

const
  GRID_COLUMNS = 'Title(70);Firstname(120,"First Name");Lastname(120,"Last Name")';

{@VFD_NEWFORM_IMPL}

procedure TFormMain.Refresh;
begin
  FreeAndNilAdrsBook;
  SetupMediators;
  GAdrsBook.PersonList.SortByProps(['Firstname', 'Lastname']);
end;

procedure TFormMain.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.AddComposite(GRID_COLUMNS, Grid1);
  end;
  FMediator.Subject := GAdrsBook.PersonList;
  FMediator.Active := True;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  GGUIINI.ReadFormState(Self);

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  GGUIINI.WriteFormState(Self);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Adrs_Dependencies.ConnectToDatabase;

  WindowTitle := WindowTitle + ' (' + ConnectionDetailsAsString + ')';

  Refresh;
end;

procedure TFormMain.btnRefreshClick(Sender: TObject);
begin
  Refresh;
end;

procedure TFormMain.btnAddClick(Sender: TObject);
begin
  //
end;

procedure TFormMain.btnEditClick(Sender: TObject);
begin
  //
end;

procedure TFormMain.btnDeleteClick(Sender: TObject);
begin
  //
end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnCreate := @FormCreate;
  OnDestroy := @FormDestroy;
  OnShow := @FormShow;
end;

procedure TFormMain.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: FormMain}
  Name := 'FormMain';
  SetPosition(354, 275, 493, 275);
  WindowTitle := 'Demo 18: Address Book';
  Hint := '';

  btnAdd := TfpgButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(4, 4, 80, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnAddClick;
  end;

  btnEdit := TfpgButton.Create(self);
  with btnEdit do
  begin
    Name := 'btnEdit';
    SetPosition(88, 4, 80, 24);
    Text := 'Edit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnEditClick;
  end;

  btnDelete := TfpgButton.Create(self);
  with btnDelete do
  begin
    Name := 'btnDelete';
    SetPosition(172, 4, 80, 24);
    Text := 'Delete';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnDeleteClick;
  end;

  Grid1 := TfpgStringGrid.Create(self);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(4, 32, 484, 204);
    BackgroundColor := TfpgColor($80000002);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 4;
    Anchors := [anTop, anLeft, anBottom, anRight];
  end;

  btnRefresh := TfpgButton.Create(self);
  with btnRefresh do
  begin
    Name := 'btnRefresh';
    SetPosition(408, 244, 80, 24);
    Text := 'Refresh';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 5;
    Anchors := [anRight, anBottom];
    OnClick := @btnRefreshClick;
  end;

  {@VFD_BODY_END: FormMain}
  {%endregion}
end;


initialization
  RegisterFallBackMediators;

  gMediatorManager.RegisterMediator(TtiStringGridMediatorView, TPersonList);

end.
