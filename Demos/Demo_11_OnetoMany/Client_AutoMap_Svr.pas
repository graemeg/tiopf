unit Client_AutoMap_Svr;

interface

procedure RegisterMappings ;

implementation
uses
   tiOPFManager
  ,tiClassToDBMap_BOM
  ,Client_BOM
  ;

// RegisterMappings only gets called in the One to Many - Auto Map demo
procedure RegisterMappings ;
begin
  //                                              Class,   Table,    Property,     Column
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID', [pktDB] );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'  );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'    ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);

  //                                              Class,        Table,          Property,     Column
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'OID',        'OID',        [pktDB] );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'Owner.OID',  'Client_OID', [pktFK] );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'NumberType', 'Number_Type'         );
  gTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'NumberText', 'Number_Text'         ) ;
  gTIOPFManager.ClassDBMappingMgr.RegisterCollection(TPhoneNumbers, TPhoneNumber);

end ;

end.
