procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
    DoStopAllServices
  else if CurStep = ssPostInstall then
  begin
    DoInstallAllServices;
    DoStartAllServices;
  end;
end;

procedure CurUninstallStepChanged(CurUninstallStep: TUninstallStep);
begin
  if CurUninstallStep = usUninstall then
  begin
    DoStopAllServices;
    DoUnInstallAllServices;
  end;
end;

