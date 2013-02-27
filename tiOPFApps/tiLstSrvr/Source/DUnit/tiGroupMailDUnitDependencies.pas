unit tiGroupMailDUnitDependencies;

interface
uses
  tiMessage_TST
  ;

procedure RegisterTests ;

implementation

procedure RegisterTests ;
begin
  tiMessage_TST.RegisterTests;
end;

end.
