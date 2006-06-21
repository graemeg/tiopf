program DemoTISplitter;

uses
  Forms,
  FMain in 'FMain.pas' {FormMain};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
