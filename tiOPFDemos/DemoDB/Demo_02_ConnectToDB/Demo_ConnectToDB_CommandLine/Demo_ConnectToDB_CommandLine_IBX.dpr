program Demo_ConnectToDB_CommandLine_IBX;

uses
  Forms,
  FMain in 'FMain.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
