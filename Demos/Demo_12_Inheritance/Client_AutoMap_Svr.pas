unit Client_AutoMap_Svr;

interface

procedure RegisterMappings;

implementation
uses
   tiOPFManager
  ,tiAutoMap
  ,Client_BOM
 ;

// RegisterMappings only gets called in the One to Many - Auto Map demo
procedure RegisterMappings;
begin
  //                                              Class,       Table,       Property,     Column
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientAbs, 'Client_Abs', 'OID',        'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientAbs, 'Client_Abs', 'ClientID',   'Client_ID'   );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClientAbs);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientCompany, 'Client_Company', 'OID',         'OID', [pktDB, pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientCompany, 'Client_Company', 'CompanyName', 'Company_Name' );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients,    TClientCompany);
  GTIOPFManager.ClassDBMappingMgr.RegisterInheritance(TClientAbs, TClientCompany);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'OID',        'OID', [pktDB, pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'GivenName',  'Given_Name' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'FamilyName', 'Family_Name' );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients,    TClientPerson);
  GTIOPFManager.ClassDBMappingMgr.RegisterInheritance(TClientAbs, TClientPerson);

end;

end.
