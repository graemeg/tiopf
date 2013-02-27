program tiFileSync;

uses
  madExcept,
  madLinkDisAsm,
  tiObjAbs,
  tiLog,
  Forms,
  tiPersist,
  tiUtils,
  FMain in 'FMain.pas' {FormMain},
  FViewFiles in 'FViewFiles.pas' {FormViewFiles},
  FProgress in 'FProgress.pas' {FormProgress},
  FFileSyncSetupEdit in 'FFileSyncSetupEdit.pas' {FormFileSyncSetupEdit},
  FFileNameFilterEdit in 'FFileNameFilterEdit.pas' {FormFileNameFilterEdit},
  tiFileSyncDependencies in 'tiFileSyncDependencies.pas',
  FtiDialogAbs in '..\..\..\tiPerAwareCtrls\FtiDialogAbs.pas' {FormTiDialogAbs};

{$R *.RES}

begin
  SetupLogForClient ;
  Application.Initialize;
  tiFileSyncDependencies.RegisterMappings;
  tiFileSyncDependencies.ConnectToDatabase;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
