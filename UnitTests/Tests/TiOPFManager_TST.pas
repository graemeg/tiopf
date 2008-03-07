unit TiOPFManager_TST;

interface
uses
  tiTestFramework
  ,tiOPFManager
  ;

type
  TTesttiOPFManager = class(TtiOPFTestCase)
  published
    procedure CreateDestroy;
  end;

procedure RegisterTests;

implementation
uses
  SysUtils,
  tiTestDependencies;

  { TTesttiOPFManager }

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTesttiOPFManager);
end;

procedure TTesttiOPFManager.CreateDestroy;
var
  L: TtiOPFManager;
begin
  L:= TtiOPFManager.Create;
  try
    Check(L.Caption = L.ClassName,
      'FtiOPFManager.Caption should be ' + L.ClassName + ' but was ' + L.Caption);
    Check(L.DefaultPerLayerName = '',
      'FtiOPFManager.DefaultPerLayerName should empty but was ' + L.DefaultPerLayerName);
  finally
    L.Free;
  end;
end;

end.
