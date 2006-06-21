unit FMainCollectionWithFilter;

{$I tiDefines.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, tiFocusPanel, tiPerAwareCtrls, ActnList,
  Client_BOM
  {$IFDEF DELPHI6ORABOVE}
  ,Variants
  {$ENDIF}
  ,tiMemoReadOnly, tiListView
  ;


type
  TFormCollectionHardCoded = class(TForm)
    paeClientName: TtiPerAwareEdit;
    tiMemoReadOnly1: TtiMemoReadOnly;
    btnSearch: TButton;
    LV: TtiListView;
    lblCount: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
  private
    FClients : TClientsByName ;
  public
    { Public declarations }
  end;

var
  FormCollectionHardCoded: TFormCollectionHardCoded;

implementation
uses
  tiPerObjOIDGUID
  ;
  
{$R *.dfm}

procedure TFormCollectionHardCoded.FormCreate(Sender: TObject);
begin
  LV.AddColumn('ClientID',   lvtkString, 'Client ID', 80);
  LV.AddColumn('ClientName', lvtkString, 'Client name', 200 );
  FClients := TClientsByName.Create ;
end;

procedure TFormCollectionHardCoded.FormDestroy(Sender: TObject);
begin
  FClients.Free ;
end;

procedure TFormCollectionHardCoded.btnSearchClick(Sender: TObject);
begin
  LV.Data := nil ;
  FClients.Clear ;
  FClients.ClientName := paeClientName.Value ;
  FClients.Read ;
  LV.Data := FClients.List ;
  lblCount.Caption := 'Count: ' + IntToStr( FClients.Count );
end;

end.
