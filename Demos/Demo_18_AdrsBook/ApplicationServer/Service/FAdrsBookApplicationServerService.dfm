object AddressBookApplicationServer: TAddressBookApplicationServer
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'tiOPF - Address book application server'
  StartType = stManual
  AfterInstall = ServiceAfterInstall
  AfterUninstall = ServiceAfterUninstall
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
