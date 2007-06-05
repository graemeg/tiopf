unit Client_AutoMap_Svr;

interface

procedure RegisterMappings;

implementation
uses
   tiOPFManager
  ,tiClassToDBMap_BOM
  ,Client_BOM
 ;

// RegisterMappings only gets called in the One to Many - Auto Map demo
procedure RegisterMappings;
begin
  //                                              Class,       Table,       Property,     Column
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientAbs, 'Client_Abs', 'OID',        'OID', [pktDB]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientAbs, 'Client_Abs', 'ClientID',   'Client_ID'   );
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClientAbs);

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientCompany, 'Client_Company', 'OID',         'OID', [pktDB, pktFK]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientCompany, 'Client_Company', 'CompanyName', 'Company_Name' );
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients,    TClientCompany);
  gTIOPFManager.ClassDBMappingMgr.RegisterInheritance(TClientAbs, TClientCompany);

  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'OID',        'OID', [pktDB, pktFK]);
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'GivenName',  'Given_Name' );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClientPerson, 'Client_Person', 'FamilyName', 'Family_Name' );
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients,    TClientPerson);
  gTIOPFManager.ClassDBMappingMgr.RegisterInheritance(TClientAbs, TClientPerson);

end;

end.
