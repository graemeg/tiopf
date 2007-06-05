unit AdrsBook_TST;

{$I tiDefines.inc}

interface
uses
  TestFramework
;

type

  TTestAdrsBook = class(TTestCase)
  private
  protected
  published
    procedure TestStuff;
  end;

implementation
uses
  Adrs_BOM
 ;
  
{ TTestAdrsBook }

procedure TTestAdrsBook.TestStuff;
begin

end;

Initialization
  RegisterTest(TTestAdrsBook.Suite);

end.


