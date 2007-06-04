unit formmain;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics,
  Dialogs, Menus, Buttons, StdCtrls, ComCtrls, views;

type

  { TMainFrm }

  TMainFrm = class(TForm)
    LVContacts: TListView;
    MainMenu1: TMainMenu;
    miEditEdit: TMenuItem;
    miEditDelete: TMenuItem;
    miEditInsert: TMenuItem;
    miEdit: TMenuItem;
    miSystemCities: TMenuItem;
    miSystemCountries: TMenuItem;
    miSystem: TMenuItem;
    miFile: TMenuItem;
    miFileExit: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miEditEditClick(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miSystemCitiesClick(Sender: TObject);
  private
    mContactList: TContacts_ListView_Mediator;
  public
    { public declarations }
  end; 

var
  MainFrm: TMainFrm;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ENDIF}


uses
  model, contactmanager, formcontactmaint, formcitylist;

{ TMainFrm }

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  gContactManager.PopulateContacts;
  mContactList := TContacts_ListView_Mediator.CreateCustom(gContactManager.ContactList, LVContacts, 'FirstName;LastName(130);EMail(180);Mobile(130);Comments');
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  mContactList.Free;
end;

procedure TMainFrm.miEditEditClick(Sender: TObject);
var
  c: TContact;
begin
  c := TContact(mContactList.SelectedObject);
  if not Assigned(c) then
    Exit; //==>

  if TContactMaintFrm.EditContact(c) then
  begin
    // we can save contact here
  end;
end;

procedure TMainFrm.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainFrm.miSystemCitiesClick(Sender: TObject);
begin
  TCityListFrm.ShowCities;
end;

{$IFDEF FPC}
initialization
  {$I formmain.lrs}
{$ENDIF}

end.

