unit DUnitAdrsBookDependencies;

{$I tiDefines.inc}

interface
uses
  tiOPFManager;

procedure ConnectToDatabase;
procedure RegisterTests;

implementation
uses
  AdrsBook_tst,
  Adrs_tst,
  AdrsType_tst,
  Person_tst;

procedure ConnectToDatabase;
begin
  GTIOPFManager.ConnectDatabase('adrs', '..\Data\adrs.fdb', 'SYSDBA', 'masterkey', '', '');
end;

procedure RegisterTests;
begin
  AdrsType_tst.RegisterTests;
  Person_tst.RegisterTests;
end;

end.



