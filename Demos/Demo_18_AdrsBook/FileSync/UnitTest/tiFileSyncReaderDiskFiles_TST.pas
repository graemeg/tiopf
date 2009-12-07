unit tiFileSyncReaderDiskFiles_TST;

interface
uses
  TestFramework
  ;

type

  TTestFileSyncReaderDiskFiles = class( TTestCase )
  published
  end ;

procedure RegisterTests ;

implementation

procedure RegisterTests ;
begin
  RegisterTest( TTestFileSyncReaderDiskFiles ) ;
end ;

end.
