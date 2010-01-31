unit tiApplicationServer_TST;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork
 ;

type

  TTestTIApplicationServer = class(TtiTestCase)
  published
    procedure tiDBProxyServer_Create;
    procedure tiDBProxyServer_ServerVersion;
    procedure tiDBProxyServer_ExecuteRemoteXML;
    procedure tiDBProxyServer_TestAlive1;
    procedure tiDBProxyServer_TestHTML;
    procedure tiDBProxyServer_TestXML;
    procedure tiDBProxyServer_TestAlive;
  end;

procedure RegisterTests;

implementation
uses
  tiTestDependencies
  ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIApplicationServer);
end;

const
  cPort= 81;

{ TTestTIWebServer }

procedure TTestTIApplicationServer.tiDBProxyServer_Create;
begin

end;

procedure TTestTIApplicationServer.tiDBProxyServer_ExecuteRemoteXML;
begin

end;

procedure TTestTIApplicationServer.tiDBProxyServer_ServerVersion;
begin

end;

procedure TTestTIApplicationServer.tiDBProxyServer_TestAlive;
begin

end;

procedure TTestTIApplicationServer.tiDBProxyServer_TestAlive1;
begin

end;

procedure TTestTIApplicationServer.tiDBProxyServer_TestHTML;
begin

end;

procedure TTestTIApplicationServer.tiDBProxyServer_TestXML;
begin

end;

end.
