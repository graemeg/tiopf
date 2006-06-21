unit tiDeployMgr_TST;

interface
uses
  TestFramework
  ;

type

  TTestDeployMgrSQL = class( TTestCase )
  protected
    procedure Setup ;
    procedure TearDown ;
  published
    procedure ReadPK ;
  end ;

implementation

{ TTestDeployMgrSQL }

procedure TTestDeployMgrSQL.ReadPK;
begin
  check(false)
end;

procedure TTestDeployMgrSQL.Setup;
begin
  inherited ;
end;

procedure TTestDeployMgrSQL.TearDown;
begin
  inherited ;
end;

initialization
  RegisterTest(TTestDeployMgrSQL.Suite);

end.
