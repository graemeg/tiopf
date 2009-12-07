unit FMainVisitorBasics;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Client_BOM, StdCtrls;

type
  TFormMainVisitorBasics = class(TForm)
    btnAddClient: TButton;
    btnShowList: TButton;
    btnRunClientVisitor: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClientClick(Sender: TObject);
    procedure btnShowListClick(Sender: TObject);
    procedure btnRunClientVisitorClick(Sender: TObject);
  private
    FClients: TClients;
  public
    { Public declarations }
  end;

var
  FormMainVisitorBasics: TFormMainVisitorBasics;

implementation
uses
   tiObject
  ,tiDialogs
 ;
  
{$R *.DFM}

procedure TFormMainVisitorBasics.FormCreate(Sender: TObject);
begin
  FClients:= TClients.Create;
end;

procedure TFormMainVisitorBasics.FormDestroy(Sender: TObject);
begin
  FClients.Free;
end;

procedure TFormMainVisitorBasics.btnAddClientClick(Sender: TObject);
var
  lClient: TClient;
begin
  lClient:= TClient.Create;
  lClient.OID.AsString:= IntToStr(GetTickCount); // Not how you do it in real life!
  lClient.ClientName  := 'Test ' + DateTimeToStr(Now);
  lClient.ClientID    := IntToStr(GetTickCount);
  FClients.Add(lClient);
end;

procedure TFormMainVisitorBasics.btnShowListClick(Sender: TObject);
begin
  tiMessageDlg(FClients.AsDebugString, ['OK']);
end;

procedure TFormMainVisitorBasics.btnRunClientVisitorClick(Sender: TObject);
var
  lVis: TClientVisitor;
begin
  lVis:= TClientVisitor.Create;
  try
    FClients.Iterate(lVis);
  finally
    lVis.Free;
  end;
end;

end.
