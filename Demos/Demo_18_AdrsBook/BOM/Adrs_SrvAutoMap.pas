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

//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'OID',         'OID', [pktDB]);
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'Owner.OID',   'Owner_OID', [pktFK]);
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'AdrsTypeOID', 'Adrs_Type' );
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'Country',     'Country'  );
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'Lines',       'Lines'    );
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'Suburb',      'Suburb'   );
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'State',       'State'    );
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'PCode',       'PCode'    );
//  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TAdrsList, TAdrs );
//
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, cTableName_EAdrs, 'OID',         'OID', [pktDB]);
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, cTableName_EAdrs, 'Owner.OID',         'Owner_OID', [pktFK]);
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, cTableName_EAdrs, 'AdrsTypeOID', 'EAdrs_Type' );
//  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, cTableName_EAdrs, 'Text', 'EAdrs_Text' );
//  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TEAdrsList, TEAdrs );

end;

end.

