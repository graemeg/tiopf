unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Grids, StdCtrls, Menus, tiBaseMediator,
  tiModelMediator, tiMediators, tiListMediators, ContactDisplay;

type
  TfrmDemoMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    GContacts: TStringGrid;
    Edit1: TMenuItem;
    Add1: TMenuItem;
    Edit2: TMenuItem;
    Delete1: TMenuItem;
    System1: TMenuItem;
    CityList1: TMenuItem;
    CountryList1: TMenuItem;
    AddressTypeList1: TMenuItem;
    btnShow: TButton;
    procedure CountryListClick(Sender: TObject);
    procedure DeleteContactClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure AddContactClick(Sender: TObject);
    procedure CityListClick(Sender: TObject);
    procedure AddressTypeListClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure EditContactClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
  private
    FMediator: TtiModelMediator;
    FDisplayList : TContactDisplayList;
    procedure SetupMediators;
  public
    { Public declarations }
  published
    Property DisplayList : TContactDisplayList Read FDisplayList Write FDisplayList;
  end;

var
  frmDemoMain: TfrmDemoMain;

implementation

{$R *.dfm}

uses
  model,
  contactmanager,
  frmEditContact,
  frmCityList,
  frmCountryList,
  frmAddressTypeList;

{ TForm1 }

procedure TfrmDemoMain.FormCreate(Sender: TObject);
begin
  RegisterFallBackMediators;
  RegisterFallBackListmediators;
  FDisplayList := TContactDisplayList.CreateCustom(gContactManager.ContactList);
  gContactManager.PopulateContacts;
  SetupMediators;
end;

procedure TfrmDemoMain.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.AddComposite(
        'FirstName(60,"First Name");LastName(90,"Last Name");EMail(130);' +
        'DateOfBirth(80,"Date of Birth");Mobile(100);Comments(100);HomeAddress(200,"Home Address")', GContacts);
  end;
  FMediator.Subject := DisplayList;
  FMediator.Active := True;
end;

procedure TfrmDemoMain.MIExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmDemoMain.AddressTypeListClick(Sender: TObject);
begin
  ShowAddressTypes(gContactManager.AddressTypeList);
end;

procedure TfrmDemoMain.CityListClick(Sender: TObject);
begin
  ShowCities(gContactManager.CityList);
end;

procedure TfrmDemoMain.CountryListClick(Sender: TObject);
begin
  ShowCountries(gContactManager.CountryList);
end;

procedure TfrmDemoMain.AddContactClick(Sender: TObject);
var
  c: TContact;
begin
  c := TContact.CreateNew;
  c.DateOfBirth := Date;
  if EditContact(c) then
    gContactManager.ContactList.Add(c)
  else
    c.Free;
end;

procedure TfrmDemoMain.EditContactClick(Sender: TObject);
var
  D : TContactDisplay;
  C : TContact;
  M : TtiMediatorView;
begin
  M := FMediator.FindByComponent(GContacts).Mediator;
  D := TContactDisplay(TtiStringGridMediatorView(M).SelectedObject);
  C := D.Contact;
  if Assigned(C) then
    if EditContact(C) then
    begin
      // we can save contact here or modify EditContact to handle it for us.
    end;
end;

procedure TfrmDemoMain.DeleteContactClick(Sender: TObject);
var
  D : TContactDisplay;
  C : TContact;
  M : TtiMediatorView;
begin
  M := FMediator.FindByComponent(GContacts).Mediator;
  D := TContactDisplay(TtiStringGridMediatorView(M).SelectedObject);
  C := D.Contact;
  if Assigned(c) then
  begin
    gContactManager.ContactList.Extract(c);
    c.Deleted:=True;
    c.Free;
    m.ObjectToGui;
  end;
end;

procedure TfrmDemoMain.btnShowClick(Sender: TObject);
var
  D : TContactDisplay;
  C : TContact;
  M : TtiMediatorView;
begin
  M := FMediator.FindByComponent(GContacts).Mediator;
  D := TContactDisplay(TtiStringGridMediatorView(M).SelectedObject);
  C := D.Contact;
  if Assigned(C) then
    ShowMessage(C.AsDebugString);
end;

end.

