program AdrsBookApplicationServer;

uses
  SvcMgr,
  FAdrsBookApplicationServerService in 'FAdrsBookApplicationServerService.pas' {AddressBookApplicationServer: TService};

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  {$ifdef DELPHI2007ORABOVE}
  if not Application.DelayInitialize or Application.Installing then
  {$endif}
    Application.Initialize;
  Application.CreateForm(TAddressBookApplicationServer, AddressBookApplicationServer);
  Application.Run;
end.


