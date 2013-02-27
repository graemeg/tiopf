program DemoMultiThrdSingleObj;

uses
  tiLog,
  Forms,
  FMain in 'FMain.pas' {Form1},
  TestPerObjThreadList in 'TestPerObjThreadList.pas';

{$R *.RES}

begin
  SetupLogForClient ;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
