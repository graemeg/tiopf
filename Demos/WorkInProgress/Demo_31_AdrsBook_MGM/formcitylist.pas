unit formcitylist;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ComCtrls, views;

type
  TCityListFrm = class(TForm)
    btnEdit: TButton;
    btnNew: TButton;
    btnDelete: TButton;
    btnClose: TButton;
    LVCities: TListView;
    Panel1: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LVCitiesDblClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    mCityList: TCityList_ListView_Mediator;
    procedure SetupMediators;
  public
    class procedure ShowCities;
  end; 

var
  CityListFrm: TCityListFrm;

implementation

{$IFNDEF FPC}
  {$R *.dfm}
{$ENDIF}

uses
  contactmanager, formcitymaint, model;

{ TCityListFrm }

procedure TCityListFrm.FormShow(Sender: TObject);
begin
  SetupMediators;
end;

procedure TCityListFrm.LVCitiesDblClick(Sender: TObject);
var
  c: TCity;
begin
  c := mCityList.SelectedObject as TCity;
  if not Assigned(c) then
    Exit; //==>
    
  if TCityMaintFrm.EditCity(c) then
  begin
    gContactManager.CityList.NotifyObservers;
  end;
end;

procedure TCityListFrm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TCityListFrm.FormDestroy(Sender: TObject);
begin
  mCityList.Free;
end;

procedure TCityListFrm.SetupMediators;
begin
  mCityList := TCityList_ListView_Mediator.CreateCustom(gContactManager.CityList, LVCities, 'Name(110);Zip(80);CountryAsString(150)');
end;

class procedure TCityListFrm.ShowCities;
var
  frm: TCityListFrm;
begin
  frm := TCityListFrm.Create(nil);
  try
    frm.ShowModal;
  finally;
    frm.Free;
  end;
end;

{$IFDEF FPC}
initialization
  {$I formcitylist.lrs}
{$ENDIF}

end.

