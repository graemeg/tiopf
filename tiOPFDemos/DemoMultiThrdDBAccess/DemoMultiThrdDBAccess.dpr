program DemoMultiThrdDBAccess;

uses
  tiLog,
  tiPersist,
  Forms,
  FMain in 'FMain.pas' {Form1};

{$R *.RES}

begin
  SetupLogForClient ;
  Application.Initialize;
  gTIPerMgr.LoadPersistenceFramework ;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
  gTIPerMgr.UnLoadPersistenceFramework ;
end.
