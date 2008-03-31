unit Adrs_TST;

interface
uses
  tiTestFramework,
  tiVisitorDB,
  Adrs_BOM,
  AdrsType_BOM,
  AdrsType_TSTSetup;

type

  //TTestAdrs = class(TTestPerFrameworkConnectAbs)
  TAdrsTestCase = class(TtiTestCase)
  private
    FAdrsTypeTestSetup: TAdrsTypeTestSetup;
    procedure EmptyTables;
  protected
    procedure SetUp; override;
    procedure TearDown;override;

    property  AdrsTypeSetup: TAdrsTypeTestSetup read FAdrsTypeTestSetup;

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
end;

destructor TAdrsTestCase.Destroy;
begin
  FAdrsTypeTestSetup.Free;
  inherited;
end;

procedure TAdrsTestCase.EmptyTables;
begin
//  GTIOPFManager.DeleteRow('Adrs', nil);
  GTIOPFManager.DeleteRow('eadrs', nil);
  GTIOPFManager.DeleteRow('person', nil);
//  GTIOPFManager.DeleteRow('Company', nil);
  GTIOPFManager.DeleteRow('eadrs_type', nil);
//  GTIOPFManager.DeleteRow('Lookup_List_Name', nil);
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
