program tiDataPump;

uses
  madExcept,
  madLinkDisAsm,
  Forms,
  FMainTIDataPump in 'FMainTIDataPump.pas' {Form1},
  tiDataPump_BOM in 'tiDataPump_BOM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
