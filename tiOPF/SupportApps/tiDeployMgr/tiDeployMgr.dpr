program tiDeployMgr;

uses
  madExcept,
  madLinkDisAsm,
  tiLog,
  tiPersist,
  Forms,
  tiDeployMgr_Dependencies in 'tiDeployMgr_Dependencies.pas',
  FtiDialogAbs in '..\..\tiPerAwareCtrls\FtiDialogAbs.pas' {FormTiDialogAbs},
  FtiPerEditDialog in '..\..\tiPerAwareCtrls\FtiPerEditDialog.pas' {FormTIPerEditDialog},
  FDeployMgrMain in 'FDeployMgrMain.pas' {FormMain},
  FDeployMgrChild_App in 'FDeployMgrChild_App.pas' {FormTIDeployChild_App},
  FEditFile in 'FEditFile.pas' {FormEditFile},
  FEditParam in 'FEditParam.pas' {FormEditParams},
  FDeployMgrChild_Launch in 'FDeployMgrChild_Launch.pas' {FormTIDeployChild_Launch};

{$R *.RES}

begin

  SetupLogForClient ;
  tiDeployMgr_Dependencies.ConnectToDatabase;
  Application.Initialize;
  Application.Title := 'TechInsite Deployment Manager';
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
