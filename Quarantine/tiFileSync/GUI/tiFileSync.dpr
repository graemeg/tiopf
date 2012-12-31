program tiFileSync;

uses
  tiBaseObject,
  tiLog,
  Forms,
  tiOPFManager,
  tiUtils,
  FMain in 'FMain.pas' {FormMain},
  FViewFiles in 'FViewFiles.pas' {FormViewFiles},
  FProgress in 'FProgress.pas' {FormProgress},
  FFileSyncSetupEdit in 'FFileSyncSetupEdit.pas' {FormFileSyncSetupEdit},
  FFileNameFilterEdit in 'FFileNameFilterEdit.pas' {FormFileNameFilterEdit},
  FtiDialogAbs in '..\..\..\tiPerAwareCtrls\FtiDialogAbs.pas' {FormTiDialogAbs},
  tiFileSyncDependencies in '..\Core\tiFileSyncDependencies.pas';

{$R *.RES}

begin
  SetupLogForClient ;
  Application.Initialize;
  tiFileSyncDependencies.RegisterMappings;
  tiFileSyncDependencies.ConnectToDatabase;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
