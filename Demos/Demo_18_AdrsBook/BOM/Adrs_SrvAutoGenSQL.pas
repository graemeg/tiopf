{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Peter Hinrichsen, TechInsite Pty. Ltd.
  as the tiOPF (TechInsite Object Persistence Framework)

    23 Victoria Pde, Collingwood, Melbourne, Victoria 3066 Australia
    PO Box 429, Abbotsford, Melbourne, Victoria 3067 Australia
    Phone: +61 3 9419 6456 Fax:   +61 3 9419 1682
    Latest source:   www.techinsite.com.au/tiOPF/Download.htm
    Documentation:   www.techinsite.com.au/tiOPF/Doc/
    Support:         www.techinsite.com.au/tiOPF/MailingList.htm

  Please submit changes to tiOPF@techinsite.com.au

  Revision history:

  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit Adrs_SrvAutoGenSQL;

interface

procedure RegisterMappings;

implementation
uses
  Adrs_BOM
  ,tiAutoMap
  ,tiOPFManager
  ,AdrsMetaData_BOM
 ;


procedure RegisterMappings;
begin
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TLookupList,     cTableName_LookupListName,  'OID',      'OID',      [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TLookupList,     cTableName_LookupListName,  'ListName', 'List_Name'        );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TLookupLists, TLookupList );

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TLookupListItem, cTableName_LookUpListValue, 'OID',      'OID',      [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TLookupListItem, cTableName_LookUpListValue, 'Owner.OID',      'Owner_OID',      [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TLookupListItem, cTableName_LookUpListValue, 'Text',     'Item_Text'        );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TLookupList, TLookupListItem );

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCompany, cTableName_Company, 'OID',         'OID',         [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCompany, cTableName_Company, 'CompanyName', 'Company_Name', [pktReadable]        );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TCompany, cTableName_Company, 'Notes',       'Notes'               );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TCompanies, TCompany );

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, cTableName_Person, 'OID',       'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, cTableName_Person, 'Owner.OID', 'Owner_OID', [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, cTableName_Person, 'Notes',     'Notes'       );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, cTableName_Person, 'LastName',  'Family_Name', [pktReadable] );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, cTableName_Person, 'FirstName', 'First_Name', [pktReadable]  );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, cTableName_Person, 'Title',     'Title'       );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TPerson, cTableName_Person, 'Initials',  'Initials'    );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TPeople, TPerson );

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'OID',         'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'Owner.OID',   'Owner_OID', [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'AdrsTypeOID', 'Adrs_Type' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'Country',     'Country'  );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'Lines',       'Lines'    );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'Suburb',      'Suburb'   );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'State',       'State'    );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TAdrs, cTableName_Adrs, 'PCode',       'PCode'    );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TAdrsList, TAdrs );

  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, cTableName_EAdrs, 'OID',         'OID', [pktDB]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, cTableName_EAdrs, 'Owner.OID',         'Owner_OID', [pktFK]);
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, cTableName_EAdrs, 'AdrsTypeOID', 'EAdrs_Type' );
  GTIOPFManager.ClassDBMappingMgr.RegisterMapping(TEAdrs, cTableName_EAdrs, 'Text', 'EAdrs_Text' );
  GTIOPFManager.ClassDBMappingMgr.RegisterCollection(TEAdrsList, TEAdrs );

end;

initialization
  RegisterMappings;

end.

