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

procedure RegisterMappings ;

implementation
uses
  Adrs_BOM
  ,tiClassToDBMap_BOM
  ,tiPersist
  ;


procedure RegisterMappings ;
begin
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TLookupList,     'Lookup_List_Name',  'OID',      'OID',      [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TLookupList,     'Lookup_List_Name',  'ListName', 'List_Name'         ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TLookupLists, TLookupList  ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TLookupListItem, 'LOOKUP_LIST_VALUE', 'OID',      'OID',      [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TLookupListItem, 'LOOKUP_LIST_VALUE', 'Owner.OID',      'Owner_OID',      [pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TLookupListItem, 'LOOKUP_LIST_VALUE', 'Text',     'Item_Text'         ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TLookupList, TLookupListItem  ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TCompany, 'Company', 'OID',         'OID',         [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TCompany, 'Company', 'CompanyName', 'Company_Name'         ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TCompany, 'Company', 'Notes',       'Notes'                ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TCompanies, TCompany  ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TPerson, 'Person', 'OID',       'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TPerson, 'Person', 'Owner.OID',       'Owner_OID', [pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TPerson, 'Person', 'Notes',     'Notes'        ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TPerson, 'Person', 'LastName',  'Family_Name'  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TPerson, 'Person', 'FirstName', 'First_Name'   ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TPerson, 'Person', 'Title',     'Title'        ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TPerson, 'Person', 'Initials',  'Initials'     ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TPeople, TPerson  ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TAdrs, 'Adrs', 'OID',         'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TAdrs, 'Adrs', 'Owner.OID',   'Owner_OID', [pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TAdrs, 'Adrs', 'AdrsTypeOID', 'Adrs_Type'  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TAdrs, 'Adrs', 'Country',     'Country'   ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TAdrs, 'Adrs', 'Lines',       'Lines'     ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TAdrs, 'Adrs', 'Suburb',      'Suburb'    ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TAdrs, 'Adrs', 'State',       'State'     ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TAdrs, 'Adrs', 'PCode',       'PCode'     ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TAdrsList, TAdrs  ) ;

  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TEAdrs, 'EAdrs', 'OID',         'OID', [pktDB] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TEAdrs, 'EAdrs', 'Owner.OID',         'Owner_OID', [pktFK] ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TEAdrs, 'EAdrs', 'AdrsTypeOID', 'EAdrs_Type'  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterMapping( TEAdrs, 'EAdrs', 'Text', 'EAdrs_Text'  ) ;
  gTIPerMgr.ClassDBMappingMgr.RegisterCollection( TEAdrsList, TEAdrs  ) ;

end ;

initialization
  RegisterMappings ;

end.
