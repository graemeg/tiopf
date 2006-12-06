unit tiDUnitTestSetup;

interface
uses
   tiBaseObject
  ,TestFramework
  ;

type

  TtiTestSetup = class(TtiBaseObject)
  private
    FTestCase: TTestCase;
  protected
    property    TC : TTestCase read FTestCase;
  public
    constructor Create(const ATestCase : TTestCase); virtual ;
  end;

implementation

constructor TtiTestSetup.Create(const ATestCase: TTestCase);
begin
  inherited Create;
  Assert(ATestCase <> nil, 'TestCase not assigned');
  FTestCase := ATestCase;
end;

end.

