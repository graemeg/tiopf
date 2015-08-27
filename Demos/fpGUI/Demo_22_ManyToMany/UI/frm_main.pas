unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form,
  fpg_grid
  ,tiMediators
  ,tiListMediators
  ,tiModelMediator
  ,customer
  ;

type

  TMainForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    grdCustomers: TfpgStringGrid;
    {@VFD_HEAD_END: MainForm}
    FMediator: TtiModelMediator;
    FCustomers: TCustomerList;
    procedure SetupMediators;
    procedure FormShow(Sender: TObject);
  public
    destructor Destroy; override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  tiLog
  ;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.SetupMediators;
begin
  FMediator := TtiModelMediator.Create(self);
  FMediator.AddComposite('FirstName(120);LastName(200);Phone(85,'',>)', grdCustomers);
  FMediator.Subject := FCustomers;
  FMediator.Active := True;

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FCustomers := TCustomerList.Create;
  FCustomers.Read;
  SetupMediators;
  Log('Customer count = ' + IntToStr(FCustomers.Count));
end;

destructor TMainForm.Destroy;
begin
  FMediator.Active := False;
  FCustomers.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(521, 216, 609, 311);
  WindowTitle := 'MainForm';
  Hint := '';
  IconName := '';
  ShowHint := True;
  WindowPosition := wpOneThirdDown;
  OnShow := @FormShow;

  grdCustomers := TfpgStringGrid.Create(self);
  with grdCustomers do
  begin
    Name := 'grdCustomers';
    SetPosition(35, 30, 535, 235);
    BackgroundColor := TfpgColor($80000002);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 0;
    RowSelect := False;
    TabOrder := 1;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


initialization
  RegisterFallBackMediators;
  RegisterFallBackListMediators;

end.
