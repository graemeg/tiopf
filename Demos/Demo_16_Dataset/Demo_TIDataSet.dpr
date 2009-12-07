program Demo_TIDataSet;

uses
  Forms,
  tiLog,
  tiLogToGUI,
  Client_AutoMap_Svr,
  Client_HardCodedVisitors_Svr,
  Client_DBIndependentVisitors_Svr,
  Client_BOM in 'Client_BOM.pas',
  FMainOneToMany in 'FMainOneToMany.pas' {FormMainOneToMany},
  FPickDatabase in '..\Common\FPickDatabase.pas' {FormPickDatabase},
  FConnectToDatabase in '..\Common\FConnectToDatabase.pas' {FormConnectToDatabase},
  WhichPersistenceMechanism in '..\Common\WhichPersistenceMechanism.pas',
  FtiDialogAbs in '..\..\GUI\VCLForms\FtiDialogAbs.pas' {FormTiDialogAbs},
  FtiPerEditDialog in '..\..\GUI\VCLForms\FtiPerEditDialog.pas' {FormTIPerEditDialog};

{$R *.res}

begin
  Application.Initialize;
  if not TFormConnectToDatabase.Execute then
    Exit ; //==>


  if not WhichPersistenceMechanism.Execute(
    Client_AutoMap_Svr.RegisterMappings,
    Client_HardCodedVisitors_Svr.RegisterVisitors,
    Client_DBIndependentVisitors_Svr.RegisterVisitors) then
    Exit; //==>

  Application.CreateForm(TFormMainOneToMany, FormMainOneToMany);
  Application.Run;

end.
