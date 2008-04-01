unit Adrs_TST;

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

  TAdrsTestCase = class(TtiTestCase)
  private
    FAdrsTypeTestSetup: TAdrsTypeTestSetup;
    FPersonTestSetup: TPersonTestSetup;
    FAdrsTestSetup: TAdrsTestSetup;
    FEAdrsTestSetup: TEAdrsTestSetup;

    procedure EmptyTables;

  protected
    procedure SetUp; override;
    procedure TearDown;override;

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
  TestFramework.RegisterTest(TAdrsTestCase.Suite);
end;

constructor TAdrsTestCase.Create;
begin
  inherited;
  FAdrsTypeTestSetup:= TAdrsTypeTestSetup.Create(Self);
  FPersonTestSetup:= TPersonTestSetup.Create(Self);
  FAdrsTestSetup:= TAdrsTestSetup.Create(Self);
  FEAdrsTestSetup:= TEAdrsTestSetup.Create(Self);
end;

destructor TAdrsTestCase.Destroy;
begin
  FAdrsTypeTestSetup.Free;
  FPersonTestSetup.Free;
  FAdrsTestSetup.Free;
  FEAdrsTestSetup.Free;
  inherited;
end;

procedure TAdrsTestCase.EmptyTables;
begin
  GTIOPFManager.DeleteRow('adrs', nil);
  GTIOPFManager.DeleteRow('eadrs', nil);
  GTIOPFManager.DeleteRow('person', nil);
  GTIOPFManager.DeleteRow('eadrs_type', nil);
  GTIOPFManager.DeleteRow('adrs_type', nil);
end;

procedure TAdrsTestCase.Setup;
begin
  inherited;
  EmptyTables;
  FreeAndNilAdrsBook;
end;

procedure TAdrsTestCase.TearDown;
begin
  EmptyTables;
  inherited;
end;


end.
