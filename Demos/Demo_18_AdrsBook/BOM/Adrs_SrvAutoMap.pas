unit Adrs_SrvAutoMap;

{$I tiDefines.inc}

interface

procedure RegisterMappings;

implementation
uses
  tiAutoMap,
  tiOPFManager,
  Adrs_BOM,
  AdrsType_BOM;


procedure RegisterMappings;
begin
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrsType, 'adrs_type', 'oid',  'oid', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrsType, 'adrs_type', 'text', 'text');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TAdrsTypeList, TAdrsType);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrsType, 'eadrs_type', 'oid',  'oid', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrsType, 'eadrs_type', 'text', 'text');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TEAdrsTypeList, TEAdrsType);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'oid',       'oid', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'LastName',  'last_name');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'FirstName', 'first_name');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'Title',     'title'       );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'Initials',  'initials'    );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'Notes',     'notes'       );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TPersonList, TPerson);

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, 'adrs', 'OID',         'oid', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, 'adrs', 'Owner.OID',   'oid_person', [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, 'adrs', 'OIDAdrsType', 'oid_adrs_type' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, 'adrs', 'Lines',       'lines'    );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, 'adrs', 'Suburb',      'suburb'   );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, 'adrs', 'State',       'state'    );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, 'adrs', 'PCode',       'pCode'    );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, 'adrs', 'Country',     'country'  );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TAddressList, TAdrs );

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, 'eadrs', 'OID',         'oid', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, 'eadrs', 'Owner.OID',   'oid_person', [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, 'eadrs', 'OIDAdrsType', 'oid_eadrs_type');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, 'eadrs', 'Text',        'eadrs_text');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TEAddressList, TEAdrs );

end;

end.

