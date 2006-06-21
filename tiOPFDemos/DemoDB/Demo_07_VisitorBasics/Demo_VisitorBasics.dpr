program Demo_VisitorBasics;

uses
  Forms,
  tiPerObjOIDGUID,
  FMainVisitorBasics in 'FMainVisitorBasics.pas' {FormMainVisitorBasics},
  Client_BOM in 'Client_BOM.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainVisitorBasics, FormMainVisitorBasics);
  Application.Run;
end.
