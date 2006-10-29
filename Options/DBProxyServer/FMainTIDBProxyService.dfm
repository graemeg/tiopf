object tiDBProxyServer: TtiDBProxyServer
  OldCreateOrder = False
  OnCreate = ServiceCreate
  DisplayName = 'tiDBProxyService'
  AfterInstall = ServiceAfterInstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Left = 440
  Top = 138
  Height = 150
  Width = 215
end
