unit frmEditCountry;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, model,
  tiModelMediator, StdCtrls;

type
  TEditCountryForm = class(TForm)
    BCancel: TButton;
    BOK: TButton;
    EISo: TEdit;
    EName: TEdit;
    Label1: TLabel;
    LEName: TLabel;
  private
    FMediator: TtiModelMediator;
    FData: TCountry;
    procedure SetData(const AValue: TCountry);
    procedure SetupMediators;
  public
    property  Data: TCountry read FData write SetData;
  end;

var
  EditCountryForm: TEditCountryForm;

function EditCountry(AData: TCountry): boolean;

implementation

{$R *.dfm}

function EditCountry(AData: TCountry): boolean;

var
  frm: TEditCountryForm;

begin
  frm:= TEditCountryForm.Create(nil);
  try
    frm.Data:=AData;
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;

{ TEditCountryForm }

procedure TEditCountryForm.SetData(const AValue: TCountry);
begin
  FData:=Avalue;
  SetupMediators;
end;

procedure TEditCountryForm.SetupMediators;
begin
  if not Assigned(FMediator) then
  begin
    FMediator := TtiModelMediator.Create(self);
    FMediator.AddProperty('Name', eName);
    FMediator.AddProperty('ISO', eISO);
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

end.