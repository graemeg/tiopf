unit tiFileSyncReaderHTTP_TST;

interface
uses
  TestFramework
  ;

type
  TTestFileSyncReaderHTTP = class( TTestCase )
  published
  end ;

procedure RegisterTests ;

implementation

procedure RegisterTests ;
begin
  RegisterTest( TTestFileSyncReaderHTTP.Suite ) ;
end ;

end.
