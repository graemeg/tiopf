program Demo_ConnectToDatabase;

uses
  Forms,
  FMainConnectToDBCode in 'FMainConnectToDBCode.pas' {FormMainConnectToDBCode},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainConnectToDBCode, FormMainConnectToDBCode);
  Application.Run;
end.
