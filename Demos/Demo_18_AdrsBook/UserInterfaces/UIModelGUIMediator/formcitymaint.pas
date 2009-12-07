unit formcitymaint;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls,
  Graphics, Dialogs, StdCtrls, Buttons, model, views;

type
  TCityMaintFrm = class(TForm)
    btnSave: TButton;
    btnCancel: TButton;
    cbCountry: TComboBox;
    edCityName: TEdit;
    edZip: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);
    procedure edCityNameChange(Sender: TObject);
    procedure edZipChange(Sender: TObject);
  private
    FData: TCity;
    mName: TCity_Name_TEdit_Mediator;
    mZip: TCity_Zip_TEdit_Mediator;
    mCountry: TCity_Country_TCombobox_Mediator;
    procedure SetData(const AValue: TCity);
    procedure SetupMediators;
  public
    class function EditCity(AData: TCity): boolean;
    property  Data: TCity read FData write SetData;
  end; 


implementation

uses
  contactmanager;

{ TCityMaintFrm }

procedure TCityMaintFrm.SetupMediators;
begin
  mName:= TCity_Name_TEdit_Mediator.CreateCustom(edCityName, Data, 'Name', 'Text');
  mZip:= TCity_Zip_TEdit_Mediator.CreateCustom(edZip, Data, 'Zip', 'Text');
  mCountry:= TCity_Country_TCombobox_Mediator.CreateCustom(gContactManager.CountryList, cbCountry, Data, 'Country');

  Data.NotifyObservers;
end;

procedure TCityMaintFrm.FormShow(Sender: TObject);
begin
  SetupMediators;
end;

procedure TCityMaintFrm.edCityNameChange(Sender: TObject);
begin
  mName.GUIChanged;
end;

procedure TCityMaintFrm.edZipChange(Sender: TObject);
begin
  mZip.GUIChanged;
end;

procedure TCityMaintFrm.SetData(const AValue: TCity);
begin
  if FData=AValue then exit;
  FData:=AValue;
end;

class function TCityMaintFrm.EditCity(AData: TCity): boolean;
var
  frm: TCityMaintFrm;
begin
  frm:= TCityMaintFrm.Create(nil);
  try
    frm.Data:= AData;
    result:= frm.ShowModal = mrOK;
  finally
    frm.Free;
  end;
end;

{$IFDEF FPC}
initialization
  {$I formcitymaint.lrs}
{$ENDIF}

end.

