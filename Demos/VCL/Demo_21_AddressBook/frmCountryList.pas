unit frmCountryList;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, model, tiModelMediator;

type
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
    FMediator: TtiModelMediator;
    procedure SetData(const AValue: TCountryList);
    procedure SetupMediators;
  public
    property Data: TCountryList read FData write SetData;
  end;

var
  CountryListForm: TCountryListForm;

procedure ShowCountries(const AList: TCountryList);

implementation

{$R *.dfm}

uses
  contactmanager, tiBaseMediator, tiListMediators, frmEditCountry;


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
  c := TCountry(TtiStringGridMediatorView(FMediator.FindByComponent(gCountries).Mediator).SelectedObject);
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
  M : TtiMediatorView;

begin
  M:=FMediator.FindByComponent(gCountries).Mediator;
  c := TCountry(TtiStringGridMediatorView(M).SelectedObject);
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
    FMediator := TtiModelMediator.Create(self);
    FMediator.AddComposite('ISO(60);Name(110)', gCountries);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

end.