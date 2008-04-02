unit DUnitAdrsBookDependencies;

{$I tiDefines.inc}

interface

procedure RegisterTests;

implementation
uses
  Adrs_tst,
  AdrsType_tst,
  Person_tst;

procedure RegisterTests;
begin
  AdrsType_tst.RegisterTests;
  Person_tst.RegisterTests;
end;

end.



