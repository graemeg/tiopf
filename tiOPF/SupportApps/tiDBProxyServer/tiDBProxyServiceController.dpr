program tiDBProxyServiceController;

uses
  madExcept,
  madLinkDisAsm,
  Forms,
  FMainTIDBProxyServiceController in 'FMainTIDBProxyServiceController.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
