object tiDBProxyServer: TtiDBProxyServer
  OldCreateOrder = False
  OnCreate = ServiceCreate
  DisplayName = 'tiDBProxyService'
  StartType = stDisabled
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
