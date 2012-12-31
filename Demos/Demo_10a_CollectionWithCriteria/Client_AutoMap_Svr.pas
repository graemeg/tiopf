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
{$IFDEF DELPHI2010ORABOVE}
  // Auto mapping supports attribute mapping specifications in Delphi 2010
  // and later.

  // Mappings are automatically created for classes using table and column
  // attributes if the class has the TAutoMap attribute
  // We don't use that in this demo because we give the user the option of
  // which persistence mechanism they'd like to use.

  // Alternative: Explicitly register using table and column attributes
  //   if the class does not have the TAutoMap attribute
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient);
{$ELSE}
  // Alternative: Explicitly register table-property-column mappings
  //                                                Class,   Table,    Property,     Column,       Special Info
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'OID',        'OID',        [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientName', 'Client_Name'        );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TClient, 'Client', 'ClientID',   'Client_ID'          );
{$ENDIF}

  // List class to class mapping
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TClients, TClient);
end;

end.

