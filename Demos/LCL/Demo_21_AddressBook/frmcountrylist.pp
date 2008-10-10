unit frmCountryList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, model, tiFormMediator;

type

  { TCountryListForm }

  TCountryListForm = class(TForm)
    BAdd: TButton;
    BEdit: TButton;
    BDelete: TButton;
    BClose: TButton;
    GCountries: TStringGrid;
    procedure BAddClick(Sender: TObject);
    procedure BDeleteClick(Sender: TObject);
    procedure BEditClick(Sender: TObject);
  private
    FData: TCountryList;
    FMediator: TFormMediator;
    procedure SetData(const AValue: TCountryList);
    procedure SetupMediators;
  public
    property Data: TCountryList read FData write SetData;
  end;

var
  CountryListForm: TCountryListForm;

procedure ShowCountries(const AList: TCountryList);

implementation

uses
  contactmanager,tiBaseMediator,tiListMediators,frmEditCountry;


procedure ShowCountries(const AList: TCountryList);
var
  frm: TCountryListForm;
begin
  frm := TCountryListForm.Create(nil);
  try
    frm.SetData(AList);
    frm.ShowModal;
  finally;
    frm.Free;
  end;
end;

{ TCountryListForm }

procedure TCountryListForm.BEditClick(Sender: TObject);

var
  c: TCountry;

begin
  c := TCountry(TStringGridMediator(FMediator.FindByComponent(gCountries).Mediator).SelectedObject);
  if Assigned(c) then
    if EditCountry(c) then
    begin
      // we can save country here
    end;
end;

procedure TCountryListForm.BAddClick(Sender: TObject);

var
  c: TCountry;

begin
  C:=TCountry.Create;
  if EditCountry(c) then
    begin
    // we can save country here
    gcontactmanager.CountryList.Add(C);
    end
  else
    c.Free;
end;

procedure TCountryListForm.BDeleteClick(Sender: TObject);

var
  c: TCountry;
  M : TMediatorView;

begin
  M:=FMediator.FindByComponent(gCountries).Mediator;
  c := TCountry(TStringGridMediator(M).SelectedObject);
  if Assigned(c) then
    begin
    gContactManager.CountryList.Extract(c);
    M.ObjectToGui;
    C.Deleted:=True;
    end;
end;

procedure TCountryListForm.SetData(const AValue: TCountryList);
begin
  FData:=Avalue;
  SetupMediators;
end;

procedure TCountryListForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddComposite('ISO(60);Name(110)', gCountries);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

initialization
  {$I frmcountrylist.lrs}

end.

