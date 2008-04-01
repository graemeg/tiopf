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

  Change history:
    Created: Jan 2000

  Notes: Family of visitors to manage mapping of
         address book classes to Interbase

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit Adrs_SrvHardCodeSQL;

interface
uses
  tiPtnVisSQL
  ,Adrs_BOM
 ;

type

  // Read the lookup lists
  //----------------------------------------------------------------------------
  TVisLookupListRead = class(TVisOwnedQrySelect)
  private
    FLastLookupList: TLookupList;
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read the primary keys of all the companies in the database
  //----------------------------------------------------------------------------
  TVisCompanyRead_PK = class(TVisOwnedQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read the primary keys of all the people in the database
  //----------------------------------------------------------------------------
  TVisPersonRead_PK = class(TVisOwnedQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read the primary keys of all the company employees in the database
  //----------------------------------------------------------------------------
  TVisCompanyEmployeeRead_PK = class(TVisOwnedQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
//    procedure   Final                  ; override;
  end;

  // Read the details of one person
  //----------------------------------------------------------------------------
  TVisPersonRead_Detail = class(TVisOwnedQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read the details of one company
  //----------------------------------------------------------------------------
  TVisCompanyRead_Detail = class(TVisOwnedQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
//    procedure   Final                  ; override;
  end;

  // Read all the address(es) for a person
  //----------------------------------------------------------------------------
  TVisAdrsRead = class(TVisOwnedQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read all the EAddress(es) for a person
  //----------------------------------------------------------------------------
  TVisEAdrsRead = class(TVisOwnedQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Insert a new company
  //----------------------------------------------------------------------------
  TVisCompanyCreate = class(TVisOwnedQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Update an existing company
  //----------------------------------------------------------------------------
  TVisCompanyUpdate = class(TVisOwnedQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Delete an existing company
  //----------------------------------------------------------------------------
  TVisCompanyDelete = class(TVisOwnedQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Insert a new person
  //----------------------------------------------------------------------------
  TVisPersonCreate = class(TVisOwnedQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Update a person
  //----------------------------------------------------------------------------
  TVisPersonUpdate = class(TVisOwnedQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Delete a person
  //----------------------------------------------------------------------------
  TVisPersonDelete = class(TVisOwnedQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Insert a new EAddress
  //----------------------------------------------------------------------------
  TVisEAdrsCreate = class(TVisOwnedQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Update an existing EAddress
  //----------------------------------------------------------------------------
  TVisEAdrsUpdate = class(TVisOwnedQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Delete an EAddress
  //----------------------------------------------------------------------------
  TVisEAdrsDelete = class(TVisOwnedQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Insert a new address
  //----------------------------------------------------------------------------
  TVisAdrsCreate = class(TVisOwnedQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  //----------------------------------------------------------------------------
  TVisAdrsUpdate = class(TVisOwnedQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  //----------------------------------------------------------------------------
  TVisAdrsDelete = class(TVisOwnedQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  //----------------------------------------------------------------------------
  TVisPersonListObjectStateDeleted = class(TVisQryObjectStateDeleted)
  protected
    function  AcceptVisitor : boolean; override;
  end;

  //----------------------------------------------------------------------------
  TVisAddressListObjectStateDeleted = class(TVisQryObjectStateDeleted)
  protected
    function  AcceptVisitor : boolean; override;
  end;

  //----------------------------------------------------------------------------
  TVisEAddressListObjectStateDeleted = class(TVisQryObjectStateDeleted)
  protected
    function  AcceptVisitor : boolean; override;
  end;

implementation
uses
  tiVisitorDB
  ,tiVisitor
  ,cAdrs
  ,cQueryNames
  ,tiUtils
  ,SysUtils
  ,tiOPFManager
 ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisLookupListRead
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisLookupListRead.AcceptVisitor: boolean;
begin
  result:= (Visited is TLookupLists) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisLookupListRead.Init;
begin
  Query.SQL.Text:=
    'SELECT  ' +
    '     N.OID       AS OID_LIST_NAME ' +
    '    ,N.LIST_NAME AS LIST_NAME ' +
    '    ,V.OID       AS OID_LIST_VALUE ' +
    '    ,V.ITEM_TEXT AS ITEM_TEXT ' +
    'FROM  ' +
    '     LOOKUP_LIST_NAME  N ' +
    '    ,LOOKUP_LIST_VALUE V ' +
    'WHERE ' +
    '    V.OWNER_OID = N.OID    ' +
    'ORDER BY ' +
    '     LIST_NAME ' +
    '    ,ITEM_TEXT ';
end;

procedure TVisLookupListRead.MapRowToObject;
var
  lData: TLookupListItem;
begin
  if (FLastLookupList = nil) or
     (FLastLookupList.OID.EqualsQueryField('OID_LIST_NAME', Query)) then
  begin
    FLastLookupList            := TLookupList.Create;
    FLastLookupList.OID.AssignFromTIQuery('OID_LIST_NAME', Query);
    FLastLookupList.ListName   := Query.FieldAsString[   'LIST_NAME'      ];
    FLastLookupList.ObjectState:= posClean;
    TPerVisList(Visited).Add(FLastLookupList);
  end;

  lData            := TLookupListItem.Create;
  lData.OID.AssignFromTIQuery('OID_LIST_VALUE', Query);
  lData.Text       := Query.FieldAsString[   'ITEM_TEXT'           ];
  FLastLookupList.Add(lData);
  lData.ObjectState:= posClean;
end;

procedure TVisLookupListRead.SetupParams;
begin
  // Do nothing
end;



// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCompanyRead_PK
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisCompanyRead_PK.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompanies) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisCompanyRead_PK.Init;
begin
  Query.SQL.Text:=
    'SELECT ' +
    '    OID ' +
    '    ,COMPANY_NAME ' +
    'FROM ' +
    '    COMPANY ' +
    'ORDER BY ' +
    '    COMPANY_NAME ';
end;

procedure TVisCompanyRead_PK.MapRowToObject;
var
  lData: TCompany;
begin
  lData            := TCompany.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.CompanyName:= Query.FieldAsString[  'Company_Name' ];
  lData.ObjectState:= posPK;
  TtiObjectList(Visited).Add(lData);
end;

procedure TVisCompanyRead_PK.SetupParams;
begin
  // Do nothing
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPersonRead
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPersonRead_PK.AcceptVisitor: boolean;
begin
  result:= (Visited is TPeople) and
            (Visited.Owner is TAdrsBook);
end;

procedure TVisPersonRead_PK.Init;
begin
  Query.SQL.Text:=
    'SELECT ' +
    '    OID ' +
    '    ,FIRST_NAME ' +
    '    ,FAMILY_NAME ' +
    'FROM ' +
    '    PERSON ' +
    'WHERE ' +
    '    OWNER_OID = -1 ' +
    'ORDER BY ' +
    '    FAMILY_NAME ' +
    '    ,FIRST_NAME ';
end;

procedure TVisPersonRead_PK.SetupParams;
begin
  // Do nothing
end;

procedure TVisPersonRead_PK.MapRowToObject;
var
  lData: TPerson;
begin
  lData            := TPerson.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.LastName   := Query.FieldAsString[  'Family_Name' ];
  lData.FirstName  := Query.FieldAsString[  'First_Name' ];
  lData.ObjectState:= posPK;
  TtiObjectList(Visited).Add(lData);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCompanyEmployeeRead_PK
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisCompanyEmployeeRead_PK.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompany) and
            (Visited.ObjectState = posPK);
end;

//procedure TVisCompanyEmployeeRead_PK.Final;
//begin
//  // Do nothing
//end;

procedure TVisCompanyEmployeeRead_PK.Init;
begin
  Query.SQL.Text:=
    'SELECT ' +
    '    OID ' +
    '    ,FIRST_NAME ' +
    '    ,FAMILY_NAME ' +
    'FROM ' +
    '  PERSON ' +
    'WHERE ' +
    '    OWNER_OID =:OWNER_OID ' +
    'ORDER BY ' +
    '   FAMILY_NAME ' +
    '  ,FIRST_NAME ';
end;

procedure TVisCompanyEmployeeRead_PK.MapRowToObject;
var
  lData: TPerson;
begin
  lData            := TPerson.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.LastName   := Query.FieldAsString[  'First_Name' ];
  lData.FirstName  := Query.FieldAsString[  'Family_Name' ];
  lData.ObjectState:= posPK;
  TCompany(Visited).People.Add(lData);
end;

procedure TVisCompanyEmployeeRead_PK.SetupParams;
begin
  Visited.OID.AssignToTIQuery('Owner_OID', Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisAdrsRead
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisAdrsRead.AcceptVisitor: boolean;
begin
  result:= (Visited is TAdrsList) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisAdrsRead.Init;
begin
  Query.SQL.Text:=
    'SELECT ' +
    '     OID ' +
    '    ,OWNER_OID ' +
    '    ,ADRS_TYPE ' +
    '    ,LINES ' +
    '    ,SUBURB ' +
    '    ,STATE ' +
    '    ,PCODE ' +
    '    ,COUNTRY ' +
    'FROM ' +
    '    ADRS ' +
    'WHERE ' +
    '   OWNER_OID =:OWNER_OID ';
end;

procedure TVisAdrsRead.MapRowToObject;
var
  lData: TAdrs;
begin
  lData:= TAdrs.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.AdrsType:= TAdrsBook(Visited.TopOfHierarchy).AdrsTypes.Find(Query.FieldAsString[  'Adrs_Type' ]);
  lData.Lines   := Query.FieldAsString[  'Lines'     ] ;
  lData.State   := Query.FieldAsString[  'State'     ] ;
  lData.PCode   := Query.FieldAsString[  'PCode'     ] ;
  lData.Country := Query.FieldAsString[  'Country'   ] ;
  lData.ObjectState:= posClean;
  TAdrsList(Visited).Add(lData);
end;

procedure TVisAdrsRead.SetupParams;
begin
  Visited.Owner.OID.AssignToTIQuery('owner_oid', Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisEAdrsRead
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisEAdrsRead.AcceptVisitor: boolean;
begin
  result:= (Visited is TEAdrsList) and
            (Visited.ObjectState = posEmpty);
end;

procedure TVisEAdrsRead.Init;
begin
  Query.SQL.Text:=
    'SELECT ' +
    '   OID ' +
    '  ,OWNER_OID ' +
    '  ,EADRS_TYPE ' +
    '  ,EADRS_TEXT ' +
    'FROM ' +
    '   EADRS ' +
    'WHERE ' +
    '   OWNER_OID =:OWNER_OID ' +
    ' ';
end;

procedure TVisEAdrsRead.MapRowToObject;
var
  lData: TEAdrs;
begin
  lData:= TEAdrs.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.AdrsType:= TAdrsBook(Visited.TopOfHierarchy).AdrsTypes.Find(Query.FieldAsString[  'EAdrs_Type' ]);
  lData.Text     := Query.FieldAsString[  'EADRS_Text'       ];
  lData.ObjectState:= posClean;
  TEAdrsList(Visited).Add(lData);
end;

procedure TVisEAdrsRead.SetupParams;
begin
//  TEAddressList(Visited).Clear;
  Visited.Owner.OID.AssignToTIQuery('owner_oid', Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPersonRead_Detail
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPersonRead_Detail.AcceptVisitor: boolean;
begin
  result:= (Visited is TPerson) and
            (TtiObject(Visited).ObjectState = posPK);
end;

// -----------------------------------------------------------------------------
procedure TVisPersonRead_Detail.Init;
begin
  Query.SQL.Text:=
    'SELECT ' +
    '     TITLE ' +
    '    ,INITIALS ' +
    '    ,NOTES ' +
    'FROM ' +
    '  PERSON ' +
    'WHERE ' +
    '    OID =:OID ';
end;

// -----------------------------------------------------------------------------
procedure TVisPersonRead_Detail.MapRowToObject;
var
  lData: TPerson;
begin
  lData:= TPerson(Visited);
  lData.Title   := Query.FieldAsString[ 'Title'    ];
  lData.Initials:= Query.FieldAsString[ 'Initials' ];
  lData.Notes   := Query.FieldAsString[ 'Notes'    ];
end;

// -----------------------------------------------------------------------------
procedure TVisPersonRead_Detail.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPersonUpate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPersonUpdate.AcceptVisitor: boolean;
begin
  result:= (Visited is TPerson) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisPersonUpdate.Init;
begin
  Query.SQL.Text:=
    'UPDATE PERSON ' +
    'SET ' +
    '   FAMILY_NAME =:FAMILY_NAME ' +
    '   ,FIRST_NAME =:FIRST_NAME ' +
    '   ,TITLE      =:TITLE ' +
    '   ,INITIALS   =:INITIALS ' +
    '   ,NOTES      =:NOTES ' +
    'WHERE ' +
    '  OID        =:OID ';
end;

procedure TVisPersonUpdate.SetupParams;
var
  lData: TPerson;
begin
  lData:= TPerson(Visited);
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsString[ 'First_Name' ] := lData.FirstName;
  Query.ParamAsString[ 'Family_Name' ]:= lData.LastName;
  Query.ParamAsString[ 'Title' ]      := lData.Title;
  Query.ParamAsString[ 'Initials' ]   := lData.Initials;
  Query.ParamAsString[ 'Notes' ]      := lData.Notes;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPersonDelete
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPersonDelete.AcceptVisitor: boolean;
begin
  result:= (Visited is TPerson) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisPersonDelete.Init;
begin
  Query.SQL.Text:=
    'DELETE FROM PERSON ' +
    'WHERE ' +
    '  OID        =:OID ';
end;

procedure TVisPersonDelete.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPersonCreate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPersonCreate.AcceptVisitor: boolean;
begin
  result:= (Visited is TPerson) and
            (TPerson(Visited).ObjectState = posCreate);
end;

procedure TVisPersonCreate.Init;
begin
  Query.SQL.Text:=
    'INSERT INTO PERSON ' +
    '(' +
    '      OID ' +
    '     ,OWNER_OID ' +
    '     ,FIRST_NAME ' +
    '     ,FAMILY_NAME ' +
    '     ,TITLE ' +
    '     ,INITIALS ' +
    '     ,NOTES ' +
    ') ' +
    'VALUES ' +
    '(' +
    '     :OID ' +
    '     ,:OWNER_OID ' +
    '     ,:FIRST_NAME ' +
    '     ,:FAMILY_NAME ' +
    '     ,:TITLE ' +
    '     ,:INITIALS ' +
    '     ,:NOTES ' +
    ') ' +
    ' ';
end;

procedure TVisPersonCreate.SetupParams;
var
  lData: TPerson;
begin
  lData:= TPerson(Visited);
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsString[  'First_Name' ] := lData.FirstName;
  Query.ParamAsString[  'Family_Name' ]:= lData.LastName;
  Query.ParamAsString[  'Title' ]      := lData.Title;
  Query.ParamAsString[  'Initials' ]   := lData.Initials;
  Query.ParamAsString[  'Notes' ]      := lData.Notes;
  lData.Owner.OID.AssignToTIQuery(Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisEAdrsCreate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisEAdrsCreate.AcceptVisitor: boolean;
begin
  result:= (Visited is TEAdrs) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisEAdrsCreate.Init;
begin
  Query.SQL.Text:=
    'INSERT INTO EADRS ' +
    '(' +
    '   OID ' +
    '  ,OWNER_OID ' +
    '  ,EADRS_TYPE ' +
    '  ,EADRS_TEXT ' +
    ') ' +
    'VALUES ' +
    '(' +
    '  :OID ' +
    '  ,:OWNER_OID ' +
    '  ,:EADRS_TYPE ' +
    '  ,:EADRS_TEXT ' +
    ') ' +
    ' ' +
    ' ';
end;

procedure TVisEAdrsCreate.SetupParams;
var
  lData: TEAdrs;
begin
  lData:= TEAdrs(Visited);
  lData.OID.AssignToTIQuery(Query);
  lData.Owner.OID.AssignToTIQuery(Query);
  lData.AdrsType.OID.AssignToTIQuery('Adrs_Type',Query);
  Query.ParamAsString[  'EADRS_Text' ]       := lData.Text;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisEAdrsDelete
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisEAdrsDelete.AcceptVisitor: boolean;
begin
  result:= (Visited is TEAdrs) and
            (TEAdrs(Visited).ObjectState = posDelete);
end;

//------------------------------------------------------------------------------
procedure TVisEAdrsDelete.Init;
begin
  Query.SQL.Text:=
    'DELETE FROM EADRS ' +
    'WHERE OID =:OID ';
end;

//------------------------------------------------------------------------------
procedure TVisEAdrsDelete.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisEAdrsUpdate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisEAdrsUpdate.AcceptVisitor: boolean;
begin
  result:= (Visited is TEAdrs) and
            (Visited.ObjectState = posUpdate);
end;

//------------------------------------------------------------------------------
procedure TVisEAdrsUpdate.Init;
begin
  Query.SQL.Text:=
    'UPDATE EADRS ' +
    'SET ' +
    '  EADRS_TYPE =:EADRS_TYPE ' +
    '  ,EADRS_TEXT      =:EADRS_TEXT ' +
    'WHERE ' +
    '   OID        =:OID ';
end;

//------------------------------------------------------------------------------
procedure TVisEAdrsUpdate.SetupParams;
var
  lData: TEAdrs;
begin
  lData:= TEAdrs(Visited);
  lData.OID.AssignToTIQuery(Query);
  lData.AdrsType.OID.AssignToTIQuery('Adrs_Type',Query);
  Query.ParamAsString[  'EADRS_Text' ]       := lData.Text;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisAdrsCreate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisAdrsCreate.AcceptVisitor: boolean;
begin
  result:= (Visited is TAdrs) and
            (TEAdrs(Visited).ObjectState = posCreate);
end;

//------------------------------------------------------------------------------
procedure TVisAdrsCreate.Init;
begin
  Query.SQL.Text:=
    'INSERT INTO ADRS ' +
    '(' +
    '  OID ' +
    ' ,OWNER_OID ' +
    ' ,ADRS_TYPE ' +
    ' ,LINES ' +
    ' ,STATE ' +
    ' ,PCODE ' +
    ' ,COUNTRY ' +
    ') ' +
    'VALUES ' +
    '(' +
    ' :OID ' +
    ' ,:OWNER_OID ' +
    ' ,:ADRS_TYPE ' +
    ' ,:LINES ' +
    ' ,:STATE ' +
    ' ,:PCODE ' +
    ' ,:COUNTRY ' +
    ') ' +
    ' ' +
    ' ';
end;

//------------------------------------------------------------------------------
procedure TVisAdrsCreate.SetupParams;
var
  lData: TAdrs;
begin
  lData:= TAdrs(Visited);
  lData.OID.AssignToTIQuery(Query);
  lData.Owner.OID.AssignToTIQuery(Query);
  lData.AdrsType.OID.AssignToTIQuery('Adrs_Type',Query);
  Query.ParamAsString[ 'lines'      ]:= lData.Lines;
  Query.ParamAsString[ 'state'      ]:= lData.State;
  Query.ParamAsString[ 'pcode'      ]:= lData.PCode;
  Query.ParamAsString[ 'country'    ]:= lData.Country;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisAdrsUpdate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisAdrsUpdate.AcceptVisitor: boolean;
begin
  result:= (Visited is TAdrs) and
            (TEAdrs(Visited).ObjectState = posUpdate);
end;

//------------------------------------------------------------------------------
procedure TVisAdrsUpdate.Init;
begin
  Query.SQL.Text:=
    'UPDATE ADRS ' +
    'SET ' +
    ' ADRS_TYPE  =:ADRS_TYPE ' +
    ' ,LINES      =:LINES ' +
    ' ,STATE      =:STATE ' +
    ' ,PCODE      =:PCODE ' +
    ' ,COUNTRY    =:COUNTRY ' +
    'WHERE ' +
    '  OID        =:OID ';
end;

//------------------------------------------------------------------------------
procedure TVisAdrsUpdate.SetupParams;
var
  lData: TAdrs;
begin
  lData:= TAdrs(Visited);
  lData.OID.AssignToTIQuery(Query);
  lData.AdrsType.OID.AssignToTIQuery('Adrs_Type',Query);
  Query.ParamAsString[  'lines'     ]:= lData.Lines;
  Query.ParamAsString[  'state'     ]:= lData.State;
  Query.ParamAsString[  'pcode'     ]:= lData.PCode;
  Query.ParamAsString[  'country'   ]:= lData.Country;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisAdrsDelete
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisAdrsDelete.AcceptVisitor: boolean;
begin
  result:= (Visited is TAdrs) and
            (TEAdrs(Visited).ObjectState = posDelete);
end;

//------------------------------------------------------------------------------
procedure TVisAdrsDelete.Init;
begin
  Query.SQL.Text:=
    'DELETE FROM ADRS ' +
    'WHERE ' +
    '  OID        =:OID ';
end;

//------------------------------------------------------------------------------
procedure TVisAdrsDelete.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TVisPersonListObjectStateDeleted
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPersonListObjectStateDeleted.AcceptVisitor: boolean;
begin
  result:= (Inherited AcceptVisitor) and
            (Visited is TPeople);
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TVisAddressListObjectStateDeleted
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisAddressListObjectStateDeleted.AcceptVisitor: boolean;
begin
  result:= (Inherited AcceptVisitor) and
            (Visited is TAdrsList);
end;

//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
//*
//* TVisEAddressListObjectStateDeleted
//*
//* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisEAddressListObjectStateDeleted.AcceptVisitor: boolean;
begin
  result:= (Inherited AcceptVisitor) and
            (Visited is TEAdrsList);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCompanyRead_Detail
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisCompanyRead_Detail.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompany) and
            (Visited.ObjectState = posPK);
end;

//procedure TVisCompanyRead_Detail.Final;
//begin
//  // Do nothing
//end;

procedure TVisCompanyRead_Detail.Init;
begin
  Query.SQL.Text:=
    'SELECT ' +
    '    NOTES ' +
    'FROM ' +
    '    COMPANY ' +
    'WHERE ' +
    '    OID =:OID ';
end;

procedure TVisCompanyRead_Detail.MapRowToObject;
var
  lData: TCompany;
begin
  lData:= TCompany(Visited);
  lData.Notes   := Query.FieldAsString[ 'Notes'    ];
end;

procedure TVisCompanyRead_Detail.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCompanyDelete
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisCompanyDelete.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompany) and
            (Visited.ObjectState = posDelete);
end;

procedure TVisCompanyDelete.Init;
begin
  Query.SQL.Text:=
    'DELETE FROM COMPANY ' +
    'WHERE ' +
    '     OID =:OID ';
end;

procedure TVisCompanyDelete.SetupParams;
begin
  Visited.OID.AssignToTIQuery(Query);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCompanyCreate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisCompanyCreate.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompany) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisCompanyCreate.Init;
begin
  Query.SQL.Text:=
    'INSERT INTO COMPANY ' +
    '(' +
    '     OID ' +
    '    ,COMPANY_NAME ' +
    '    ,NOTES ' +
    ') ' +
    'VALUES ' +
    '(' +
    '    :OID ' +
    '    ,:COMPANY_NAME ' +
    '    ,:NOTES ' +
    ') ';
end;

procedure TVisCompanyCreate.SetupParams;
var
  lData: TCompany;
begin
  lData:= TCompany(Visited);
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsString[  'COMPANY_NAME' ]:= lData.CompanyName;
  Query.ParamAsString[  'NOTES' ]       := lData.Notes;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisCompanyUpdate
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisCompanyUpdate.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompany) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisCompanyUpdate.Init;
begin
  Query.SQL.Text:=
    'UPDATE COMPANY SET ' +
    '    COMPANY_NAME =:COMPANY_NAME ' +
    '    ,NOTES        =:NOTES ' +
    'WHERE ' +
    '    OID          =:OID ' +
    ' ';
end;

procedure TVisCompanyUpdate.SetupParams;
var
  lData: TCompany;
begin
  lData:= TCompany(Visited);
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsString[  'COMPANY_NAME' ]:= lData.CompanyName;
  Query.ParamAsString[  'NOTES' ]       := lData.Notes;
end;

initialization

  // Read PK visitors
//  GTIOPFManager.RegReadPKVisitor(TVisCompanyRead_PK);
//  GTIOPFManager.RegReadPKVisitor(TVisPersonRead_PK);
//  GTIOPFManager.RegReadPKVisitor(TVisCompanyEmployeeRead_PK);

  // Read detail visitors
  GTIOPFManager.RegReadVisitor(TVisLookupListRead);
  GTIOPFManager.RegReadVisitor(TVisCompanyRead_Detail);
  GTIOPFManager.RegReadVisitor(TVisPersonRead_Detail);
  GTIOPFManager.RegReadVisitor(TVisAdrsRead);
  GTIOPFManager.RegReadVisitor(TVisEAdrsRead);

  // Delete visitors
  GTIOPFManager.RegSaveVisitor(TVisEAdrsDelete);
  GTIOPFManager.RegSaveVisitor(TVisAdrsDelete);
  GTIOPFManager.RegSaveVisitor(TVisPersonDelete);
  GTIOPFManager.RegSaveVisitor(TVisCompanyDelete);

  // Update visitors
  GTIOPFManager.RegSaveVisitor(TVisCompanyUpdate);
  GTIOPFManager.RegSaveVisitor(TVisPersonUpdate);
  GTIOPFManager.RegSaveVisitor(TVisEAdrsUpdate);
  GTIOPFManager.RegSaveVisitor(TVisAdrsUpdate);

  // Create visitors
  GTIOPFManager.RegSaveVisitor(TVisCompanyCreate);
  GTIOPFManager.RegSaveVisitor(TVisPersonCreate);
  GTIOPFManager.RegSaveVisitor(TVisEAdrsCreate);
  GTIOPFManager.RegSaveVisitor(TVisAdrsCreate);

  // List clean up visitors
  GTIOPFManager.RegSaveVisitor(TVisPersonListObjectStateDeleted  );
  GTIOPFManager.RegSaveVisitor(TVisAddressListObjectStateDeleted );
  GTIOPFManager.RegSaveVisitor(TVisEAddressListObjectStateDeleted);

end.
