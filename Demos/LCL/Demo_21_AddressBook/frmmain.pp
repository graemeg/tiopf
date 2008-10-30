unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, Grids, tiBaseMediator, tiFormMediator, tiMediators, tiListMediators;

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
    procedure MIAddContactClick(Sender: TObject);
    procedure CityListClick(Sender: TObject);
    procedure MIAddressTypeListClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure EditContactClick(Sender: TObject);
  private
    { private declarations }
    FMediator: TFormMediator;
    procedure SetupMediators;
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

uses
  model,
  contactmanager,
  frmeditcontact,
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
  c: TContact;
begin
  c := TContact(TStringGridMediator(FMediator.FindByComponent(GContacts).Mediator).SelectedObject);
  if Assigned(c) then
    if EditContact(c) then
      begin
        // we can save contact here
      end;
end;

procedure TMainForm.MIAddContactClick(Sender: TObject);
var
  c: TContact;
begin
  c := TContact.CreateNew;
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
  c: TContact;
  M : TMediatorView;
begin
  M:=FMediator.FindByComponent(GContacts).Mediator;
  c := TContact(TStringGridMediator(M).SelectedObject);
  if Assigned(c) then
    begin
    gcontactmanager.ContactList.extract(c);
    C.Deleted:=True;
    M.ObjectToGui;
    end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  RegisterFallBackMediators;
  RegisterFallBackListmediators;
  gcontactmanager.PopulateContacts;
  SetupMediators;
end;

procedure TMainForm.CountryListClick(Sender: TObject);
begin
  ShowCountries(gContactManager.CountryList);
end;

procedure TMainForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddComposite('FirstName;LastName(130);EMail(180);Mobile(130);Comments(200)', GContacts);
  end;
  FMediator.Subject := gContactManager.ContactList;
  FMediator.Active := True;
end;

initialization
  {$I frmmain.lrs}

end.

