unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Grids, tiBaseMediator, tiModelMediator, tiMediators,
  tiListMediators, ContactDisplay;

type

  { TMainForm }

  TMainForm = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MIAddressTypeList: TMenuItem;
    MICountryList: TMenuItem;
    MICityList: TMenuItem;
    MIDeleteContact: TMenuItem;
    MIEditContact: TMenuItem;
    MIAddContact: TMenuItem;
    MIExit: TMenuItem;
    MISystem: TMenuItem;
    MIFile: TMenuItem;
    MIEdit: TMenuItem;
    GContacts: TStringGrid;
    ToolBar1: TToolBar;
    TBAdd: TToolButton;
    TBEdit: TToolButton;
    TBDelete: TToolButton;
    procedure CountryListClick(Sender: TObject);
    procedure DeleteContactClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MIAddContactClick(Sender: TObject);
    procedure CityListClick(Sender: TObject);
    procedure MIAddressTypeListClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure EditContactClick(Sender: TObject);
    procedure GContactsHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
  private
    FMediator: TtiModelMediator;
    FDisplayList : TContactDisplayList;
    procedure SetupMediators;
  published
    Property DisplayList : TContactDisplayList Read FDisplayList Write FDisplayList;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  model,
  contactmanager,
  frmEditContact,
  frmcitylist,
  frmCountryList,
  frmAddressTypeList;

{ TMainForm }

procedure TMainForm.MIExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.EditContactClick(Sender: TObject);
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

procedure TMainForm.GContactsHeaderClick(Sender: TObject; IsColumn: Boolean; Index: Integer);
begin
  if IsColumn then
  begin
    // We only want to sort by FName and LName
    if Index = 0 then
      gContactManager.ContactList.SortByProps(['FirstName'])
    else if Index = 1 then
      gContactManager.ContactList.SortByProps(['LastName']);
  end;
end;

procedure TMainForm.MIAddContactClick(Sender: TObject);
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

procedure TMainForm.CityListClick(Sender: TObject);
begin
  ShowCities(gContactManager.CityList);
end;

procedure TMainForm.MIAddressTypeListClick(Sender: TObject);
begin
  ShowAddressTypes(gContactManager.AddressTypeList);
end;

procedure TMainForm.DeleteContactClick(Sender: TObject);
var
  D : TContactDisplay;
  C : TContact;
  M : TtiMediatorView;
begin
  M := FMediator.FindByComponent(GContacts).Mediator;
  D := TContactDisplay(TtiStringGridMediatorView(M).SelectedObject);
  C := D.Contact;
  if Assigned(C) then
  begin
    gContactManager.ContactList.Extract(c);
    C.Deleted:=True;
    C.Free;
    M.ObjectToGui;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RegisterFallBackMediators;
  RegisterFallBackListmediators;
  FDisplayList := TContactDisplayList.CreateCustom(gContactManager.ContactList);
  gContactManager.PopulateContacts;
  SetupMediators;
//  GContacts.OnHeaderClick := @GContactsHeaderClick;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDisplayList.Free;
end;

procedure TMainForm.CountryListClick(Sender: TObject);
begin
  ShowCountries(gContactManager.CountryList);
end;

procedure TMainForm.SetupMediators;
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


end.

