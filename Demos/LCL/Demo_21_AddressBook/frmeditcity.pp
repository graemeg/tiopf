unit frmeditcity;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, model,
  tiFormMediator, StdCtrls;

type

  { TEditCityForm }

  TEditCityForm = class(TForm)
    BSave: TButton;
    BCancel: TButton;
    CBCountry: TComboBox;
    EName: TEdit;
    EZip: TEdit;
    LEName: TLabel;
    Label2: TLabel;
    LCBCountry: TLabel;
  private
    FMediator: TFormMediator;
    FData: TCity;
    procedure SetData(const AValue: TCity);
    procedure SetupMediators;
  public
    property  Data: TCity read FData write SetData;
  end;

var
  EditCityForm: TEditCityForm;

function EditCity(AData: TCity): boolean;

implementation

uses
  tiBaseMediator, contactmanager, typinfo;


function EditCity(AData: TCity): boolean;
var
  frm: TEditCityForm;
begin
  frm:= TEditCityForm.Create(nil);
  try
    frm.Data:=AData;
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;

{ TEditCityForm }

procedure TEditCityForm.SetData(const AValue: TCity);
begin
  FData:=AValue;
  SetupMediators;
end;

procedure TEditCityForm.SetupMediators;

begin
  if not Assigned(FMediator) then
  begin
    FMediator := TFormMediator.Create(self);
    FMediator.AddProperty('Name', eName);
    FMediator.AddProperty('ZIP', eZIP);
    FMediator.AddProperty('Country', cbCountry).ValueList := gContactManager.CountryList;
  end;
  FMediator.Subject := FData;
  FMediator.Active := True;
end;

initialization
  {$I frmeditcity.lrs}

end.

