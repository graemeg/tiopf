program DemoTIReadOnly;

uses
  tiLog,
  Forms,
  FMain in 'FMain.pas' {FormMain};

{$R *.RES}

begin
setuplogforclient ;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
