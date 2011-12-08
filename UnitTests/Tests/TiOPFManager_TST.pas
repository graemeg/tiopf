unit TiOPFManager_TST;

{$I tiDefines.inc}

interface
uses
  tiTestFramework;

type
  TTesttiOPFManager = class(TtiTestCase)
  published
    procedure CreateDestroy;
  end;

procedure RegisterTests;

implementation
uses
   SysUtils
  ,tiOPFManager
  ,tiTestDependencies
 ;

  { TTesttiOPFManager }

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTesttiOPFManager);
end;

procedure TTesttiOPFManager.CreateDestroy;
var
  L: TtiOPFManager;
begin
  L:= TtiOPFManager.Create;
  try
    Check(L.Caption = L.ClassName,
      'FtiOPFManager.Caption should be ' + L.ClassName + ' but was ' + L.Caption);
    Check(L.DefaultPersistenceLayerName = '',
      'FtiOPFManager.DefaultPersistenceLayerName should empty but was ' + L.DefaultPersistenceLayerName);
  finally
    L.Free;
  end;
end;

end.
