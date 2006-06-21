program tiAppLaunch;

uses
  madExcept,
  madLinkDisAsm,
  Forms,
  tiDeployMgr_Dependencies in 'tiDeployMgr_Dependencies.pas',
  FAppLaunch in 'FAppLaunch.pas' {FormAppLaunch},
  ctiAppLaunch in 'ctiAppLaunch.pas';

{$R *.RES}

begin
  tiDeployMgr_Dependencies.Execute ;
end.
