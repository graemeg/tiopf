program Demo_OneToMany;

uses
  Forms,
  tiLog,
  tiLogToGUI,
  Client_AutoMap_Svr,
  Client_HardCodedVisitors_Svr,
  Client_DBIndependentVisitors_Svr,
  Client_BOM in 'Client_BOM.pas',
  FMainOneToMany in 'FMainOneToMany.pas' {FormMainOneToMany},
  FClientEdit in 'FClientEdit.pas' {FormClientEdit},
  FPhoneNumberEdit in 'FPhoneNumberEdit.pas' {FormPhoneNumberEdit},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  WhichPersistenceMechanism in '..\Common\WhichPersistenceMechanism.pas',
  FtiPerEditDialog in '..\..\GUI\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FtiDialogAbs in '..\..\GUI\FtiDialogAbs.pas' {FormTiDialogAbs};

{$R *.res}

begin
  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit; //==>


  if not WhichPersistenceMechanism.Execute(
    Client_AutoMap_Svr.RegisterMappings,
    Client_HardCodedVisitors_Svr.RegisterVisitors,
    Client_DBIndependentVisitors_Svr.RegisterVisitors) then
    Exit; //==>

  Application.CreateForm(TFormMainOneToMany, FormMainOneToMany);
  Application.Run;

end.

