unit TiOPFManager_TST;

interface
uses
  tiTestFramework
  ,tiOPFManager
  ;

type
  TTesttiOPFManager = class(TtiTestCase)
  private
    FtiOPFManager: TtiOPFManager;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure CreateDestroy;
  end;

procedure RegisterTests;

implementation
uses
  SysUtils
  ,tiTestDependencies
  ;
  { TTesttiOPFManager }

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTesttiOPFManager);
end;

procedure TTesttiOPFManager.CreateDestroy;
begin
  FtiOPFManager := TtiOPFManager.Create;
  Check(FtiOPFManager.Caption = FtiOPFManager.ClassName,
    'FtiOPFManager.Caption should be ' + FtiOPFManager.ClassName + ' but was ' + FtiOPFManager.Caption);
  Check(FtiOPFManager.DefaultPerLayerName = '',
    'FtiOPFManager.DefaultPerLayerName should empty but was ' + FtiOPFManager.DefaultPerLayerName);
end;

procedure TTesttiOPFManager.SetUp;
begin
  FtiOPFManager := nil;
end;

procedure TTesttiOPFManager.TearDown;
begin
  FtiOPFManager.Free;
end;

end.
