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
  //                                              Class,   Table,    Property,     Column
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'   );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);

  //                                              Class,        Table,          Property,     Column
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'OID',        'OID',        [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'Owner.OID',  'Client_OID', [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'NumberType', 'Number_Type'        );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPhoneNumber, 'Phone_Number', 'NumberText', 'Number_Text'        );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TPhoneNumbers, TPhoneNumber);

end;

end.
