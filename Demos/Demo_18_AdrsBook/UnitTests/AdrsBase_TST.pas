unit AdrsBase_TST;

interface
uses
  tiTestFramework,
  tiVisitorDB,
  Adrs_BOM,
  AdrsType_BOM,
  AdrsType_TSTSetup,
  Person_TSTSetup,
  Adrs_TSTSetup,
  EAdrs_TSTSetup;

type

  TAdrsBaseTestCase = class(TtiTestCase)
  private
    FPersistenceLayerName: string;
    FAdrsTypeTestSetup: TAdrsTypeTestSetup;
    FPersonTestSetup: TPersonTestSetup;
    FAdrsTestSetup: TAdrsTestSetup;
    FEAdrsTestSetup: TEAdrsTestSetup;

    procedure EmptyTables;

  protected
    procedure SetUpOnce; override;
    procedure SetUp; override;
    procedure TearDown;override;
    property  PersistenceLayerName: string read FPersistenceLayerName write FPersistenceLayerName;

    property  AdrsTypeSetup: TAdrsTypeTestSetup read FAdrsTypeTestSetup;
    property  PersonTestSetup: TPersonTestSetup read FPersonTestSetup;
    property  AdrsTestSetup: TAdrsTestSetup read FAdrsTestSetup;
    property  EAdrsTestSetup: TEAdrsTestSetup read FEAdrsTestSetup;

  public
    constructor Create; override;
    destructor  Destroy; override;

  end;


procedure RegisterTests;

implementation
uses
  TestFramework,
  SysUtils,
  tiObject,
  tiOPFManager,
  tiQuery,
  AdrsUnitTestConstants;

{ TTestAdrs }

procedure RegisterTests;
begin
  TestFramework.RegisterTest(TAdrsBaseTestCase.Suite);
end;

constructor TAdrsBaseTestCase.Create;
begin
  inherited;
  FAdrsTypeTestSetup:= TAdrsTypeTestSetup.Create(Self);
  FPersonTestSetup:= TPersonTestSetup.Create(Self);
  FAdrsTestSetup:= TAdrsTestSetup.Create(Self);
  FEAdrsTestSetup:= TEAdrsTestSetup.Create(Self);
end;

destructor TAdrsBaseTestCase.Destroy;
begin
  FAdrsTypeTestSetup.Free;
  FPersonTestSetup.Free;
  FAdrsTestSetup.Free;
  FEAdrsTestSetup.Free;
  inherited;
end;

procedure TAdrsBaseTestCase.EmptyTables;
begin
  GTIOPFManager.DeleteRow('adrs', nil);
  GTIOPFManager.DeleteRow('eadrs', nil);
  GTIOPFManager.DeleteRow('person', nil);
  GTIOPFManager.DeleteRow('eadrs_type', nil);
  GTIOPFManager.DeleteRow('adrs_type', nil);
end;

procedure TAdrsBaseTestCase.Setup;
begin
  inherited;
  EmptyTables;
end;

procedure TAdrsBaseTestCase.SetUpOnce;
begin
  inherited;
  GTIOPFManager.DefaultPersistenceLayerName:= PersistenceLayerName;
end;

procedure TAdrsBaseTestCase.TearDown;
begin
  EmptyTables;
  inherited;
end;


end.
