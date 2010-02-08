unit FMainCollectionWithFilter;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM, tiVTListView, Variants;

type
  TFormCollectionHardCoded = class(TForm)
    paeClientName: TtiPerAwareEdit;
    btnSearch: TButton;
    lblCount: TLabel;
    LV: TtiVTListView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
  private
    FClients: TClients;
  public
    { Public declarations }
  end;

var
  FormCollectionHardCoded: TFormCollectionHardCoded;

implementation
  
{$R *.dfm}

procedure TFormCollectionHardCoded.FormCreate(Sender: TObject);
begin
  LV.AddColumn('ClientID',   vttkString, 'Client ID', 80);
  LV.AddColumn('ClientName', vttkString, 'Client name', 350);
  FClients:= TClients.Create;
end;

procedure TFormCollectionHardCoded.FormDestroy(Sender: TObject);
begin
  FClients.Free;
end;

procedure TFormCollectionHardCoded.btnSearchClick(Sender: TObject);
var
  LClientsLike: TClientsLike;
begin
  LV.Data:= nil;
  LClientsLike:= TClientsLike.Create;
  try
    LClientsLike.ClientNameLike:= paeClientName.Value;
    LClientsLike.Clients:= FClients;
    LClientsLike.Read;
  finally
    LClientsLike.Free;
  end;
  LV.Data:= FClients;
  lblCount.Caption:= 'Count: ' + IntToStr(FClients.Count);
end;

end.
