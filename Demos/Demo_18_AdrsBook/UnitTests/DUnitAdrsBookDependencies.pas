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
  Person_tst,
  Company_tst;

procedure ConnectToDatabase;
begin
  GTIOPFManager.ConnectDatabase('adrs', '..\Data\adrs.fdb', 'SYSDBA', 'masterkey', '', '');
end;

procedure RegisterTests;
begin
  AdrsBook_tst.RegisterTests;
  AdrsType_tst.RegisterTests;
  Person_tst.RegisterTests;
  Company_tst.RegisterTests;
end;

end.



