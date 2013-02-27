program Demo_ConnectToDB_Code;

uses
  Forms,
  FPickDatabase in '..\..\Common\FPickDatabase.pas' {FormPickDatabase},
  FMainConnectToDBCode in 'FMainConnectToDBCode.pas' {FormMainConnectToDBCode};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainConnectToDBCode, FormMainConnectToDBCode);
  Application.Run;
end.
