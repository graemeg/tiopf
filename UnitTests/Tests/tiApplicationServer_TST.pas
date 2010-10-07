unit tiApplicationServer_TST;

{$I tiDefines.inc}

interface
uses
   tiTestFrameWork
 ;

type

  TtiApplicationServerTestCase = class(TtiTestCase)
  published
    procedure tiApplicationServer_Create;
    procedure tiApplicationServer_ServerVersion;
    procedure tiApplicationServer_ExecuteRemoteXML;
    procedure tiApplicationServer_TestAlive1;
    procedure tiApplicationServer_TestHTML;
    procedure tiApplicationServer_TestXML;
    procedure tiApplicationServer_TestAlive;
  end;

procedure RegisterTests;

implementation
uses
  tiTestDependencies
  ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TtiApplicationServerTestCase);
end;

const
  cPort= 81;

{ TTestTIWebServer }

procedure TtiApplicationServerTestCase.tiApplicationServer_Create;
begin

end;

procedure TtiApplicationServerTestCase.tiApplicationServer_ExecuteRemoteXML;
begin

end;

procedure TtiApplicationServerTestCase.tiApplicationServer_ServerVersion;
begin

end;

procedure TtiApplicationServerTestCase.tiApplicationServer_TestAlive;
begin

end;

procedure TtiApplicationServerTestCase.tiApplicationServer_TestAlive1;
begin

end;

procedure TtiApplicationServerTestCase.tiApplicationServer_TestHTML;
begin

end;

procedure TtiApplicationServerTestCase.tiApplicationServer_TestXML;
begin

end;

end.
