unit Client_AutoMap_Svr;

interface

procedure RegisterMappings;

implementation
uses
   tiOPFManager
  ,tiAutoMap
  ,Client_BOM
 ;

procedure RegisterMappings;
begin
  //                                              Class,   Table,    Property,     Column,       Special Info
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID',        [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'        );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'          );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
end;
end.
