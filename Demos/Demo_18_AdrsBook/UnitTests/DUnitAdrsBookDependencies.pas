unit DUnitAdrsBookDependencies;

{$I tiDefines.inc}

interface

procedure RegisterTests;
procedure ConnectToDatabase;

implementation
uses
  tiOPFManager,
  tiConstants,
  AdrsBase_TST,
  AdrsType_TST,
  AdrsCreateXML_TST,
  Person_TST,
  Adrs_SrvAutoMap,
  tiQueryIBX,
  tiQueryXMLLight;

procedure RegisterTests;
begin
  AdrsType_TST.RegisterTests;
  Person_TST.RegisterTests;
  AdrsCreateXML_TST.RegisterTests;
end;

procedure ConnectToDatabase;
begin
  Adrs_SrvAutoMap.RegisterMappings;
  GTIOPFManager.DefaultPersistenceLayerName:= cTIPersistIBX;
  GTIOPFManager.ConnectDatabase('adrs', 'adrs.fdb', 'SYSDBA', 'masterkey', '', '');
  GTIOPFManager.DefaultPersistenceLayerName:= cTIPersistXMLLight;
  GTIOPFManager.ConnectDatabase('adrs', 'adrs.xmllight', '', '', '', '');
end;


end.



