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
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrsType, 'eadrs_type', 'oid',  'oid', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrsType, 'eadrs_type', 'text', 'text');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TEAdrsTypeList, TEAdrsType);

//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TLookupListItem, cTableName_LookUpListValue, 'OID',      'OID',      [pktDB]);
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TLookupListItem, cTableName_LookUpListValue, 'Owner.OID',      'Owner_OID',      [pktFK]);
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TLookupListItem, cTableName_LookUpListValue, 'Text',     'Item_Text'        );
//  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TLookupList, TLookupListItem );
//
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCompany, cTableName_Company, 'OID',         'OID',         [pktDB]);
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCompany, cTableName_Company, 'CompanyName', 'Company_Name', [pktReadable]        );
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCompany, cTableName_Company, 'Notes',       'Notes'               );
//  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TCompanies, TCompany );
//
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'oid',       'oid', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'LastName',  'last_name'{, [pktReadable]} );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, 'person', 'FirstName', 'first_name'{, [pktReadable]}  );
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
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, 'eadrs', 'OIDAdrsType', 'oid_adrs_type');
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, 'eadrs', 'Text',        'eadrs_text');
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TEAddressList, TEAdrs );

end;

end.

