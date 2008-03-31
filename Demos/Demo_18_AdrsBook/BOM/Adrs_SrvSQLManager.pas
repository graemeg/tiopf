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

unit Adrs_SrvSQLManager;

interface
uses
  tiPtnVisSQL
  ,Adrs_BOM
 ;

type

  // Read the lookup lists
  //----------------------------------------------------------------------------
  TVisLookupListRead = class(TVisQrySelect)
  private
    FLastLookupList: TLookupList;
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read the primary keys of all the people in the database
  //----------------------------------------------------------------------------
  TVisCompanyRead_PK = class(TVisQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read the primary keys of all the people in the database
  //----------------------------------------------------------------------------
  TVisPersonRead_PK = class(TVisQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read the primary keys of all the people in the database
  //----------------------------------------------------------------------------
  TVisCompanyEmployeeRead_PK = class(TVisQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
    procedure   Final                  ; override;
  end;

  // Read the details of one person
  //----------------------------------------------------------------------------
  TVisPersonRead_Detail = class(TVisQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read the details of one company
  //----------------------------------------------------------------------------
  TVisCompanyRead_Detail = class(TVisQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
    procedure   Final                  ; override;
  end;

  // Insert a new company
  //----------------------------------------------------------------------------
  TVisCompanyCreate = class(TVisQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Update an existing company
  //----------------------------------------------------------------------------
  TVisCompanyUpdate = class(TVisQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Delete an existing company
  //----------------------------------------------------------------------------
  TVisCompanyDelete = class(TVisQryDelete)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
  end;

  // Insert a new person
  //----------------------------------------------------------------------------
  TVisPersonCreate = class(TVisQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Update a person
  //----------------------------------------------------------------------------
  TVisPersonUpdate = class(TVisQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Delete a person
  //----------------------------------------------------------------------------
  TVisPersonDelete = class(TVisQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Read all the address(es) for a person
  //----------------------------------------------------------------------------
  TVisAdrsRead = class(TVisQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Read all the EAddress(es) for a person
  //----------------------------------------------------------------------------
  TVisEAdrsRead = class(TVisQrySelect)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
    procedure   MapRowToObject         ; override;
  end;

  // Insert a new EAddress
  //----------------------------------------------------------------------------
  TVisEAdrsCreate = class(TVisQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Update an existing EAddress
  //----------------------------------------------------------------------------
  TVisEAdrsUpdate = class(TVisQryUpdate)
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Delete an EAddress
  //----------------------------------------------------------------------------
  TVisEAdrsDelete = class(TVisQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  // Insert a new address
  //----------------------------------------------------------------------------
  TVisAdrsCreate = class(TVisQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  //----------------------------------------------------------------------------
  TVisAdrsUpdate = class(TVisQryUpdate)
  private
  protected
    function    AcceptVisitor: boolean; override;
    procedure   Init                   ; override;
    procedure   SetupParams            ; override;
  end;

  //----------------------------------------------------------------------------
  TVisAdrsDelete = class(TVisQryUpdate)
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
  ,tiLog
  ,tiPerObjOIDAbs
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
  QueryName:= cgQryLookupListRead;
end;

procedure TVisLookupListRead.MapRowToObject;
var
  lData: TLookupListItem;
begin
  if (FLastLookupList = nil) or
     (not FLastLookupList.OID.EqualsQueryField('OID_LIST_NAME', Query)) then
  begin
    FLastLookupList            := TLookupList.Create;
//    FLastLookupList.OID.AsInteger        := Query.FieldAsInteger[  'OID_LIST_NAME'  ];
    FLastLookupList.OID.AssignFromTIQuery('OID_List_Name', Query);
    FLastLookupList.ListName   := Query.FieldAsString[   'LIST_NAME'      ];
    FLastLookupList.ObjectState:= posClean;
    TPerVisList(Visited).Add(FLastLookupList);
  end;

  lData            := TLookupListItem.Create;
  lData.OID.AssignFromTIQuery('Owner_OID', Query);
  lData.Text       := Query.FieldAsString[   'Item_TEXT'           ];
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
  QueryName:= cgQryCompanyReadPK;
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
  QueryName:= cgQryPersonReadPK;
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

procedure TVisCompanyEmployeeRead_PK.Final;
begin
  // Do nothing 
end;

procedure TVisCompanyEmployeeRead_PK.Init;
begin
  QueryName:= cgQryCompanyReadEmployeePK;
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
  QueryName:= cgQryPersonAddressRead;
end;

procedure TVisAdrsRead.MapRowToObject;
var
  lData: TAdrs;
  lOID: TtiOID;
  lAdrsType: TLookupListItem;
begin
  lOID:= GTIOPFManager.OIDFactory.CreateOID;
  try
    lOID.AssignFromTIQuery('Adrs_Type', Query);
    lAdrsType:=
      TAdrsBook(Visited.TopOfHierarchy).AdrsTypes.Find(lOID);
  finally
    lOID.Free;
  end;

  lData:= TAdrs.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.AdrsType:= lAdrsType;
  lData.Lines   := Query.FieldAsString[  'Lines'     ] ;
  lData.State   := Query.FieldAsString[  'State'     ] ;
  lData.PCode   := Query.FieldAsString[  'PCode'     ] ;
  lData.Country := Query.FieldAsString[  'Country'   ] ;
  lData.ObjectState:= posClean;
  TAdrsList(Visited).Add(lData);
end;

procedure TVisAdrsRead.SetupParams;
begin
//  Query.ParamAsInteger[ 'owner_oid' ]:= Visited.Owner.OID.AsInteger;
  Visited.Owner.OID.AssignToTIQuery('Owner_OID', Query);

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
  QueryName:= cgQryPersonEAddressRead;
end;

procedure TVisEAdrsRead.MapRowToObject;
var
  lData: TEAdrs;
  lOID: TtiOID;
  lAdrsType: TLookupListItem;
begin
  lOID:= GTIOPFManager.OIDFactory.CreateOID;
  try
    lOID.AssignFromTIQuery('EAdrs_Type', Query);
    lAdrsType:= TAdrsBook(Visited.TopOfHierarchy).AdrsTypes.Find(lOID)
  finally
    lOID.Free;
  end;

  lData:= TEAdrs.Create;
  lData.OID.AssignFromTIQuery(Query);
  lData.AdrsType:= lAdrsType;
  lData.Text     := Query.FieldAsString[  'EAdrs_Text'       ];
  lData.ObjectState:= posClean;
  TEAdrsList(Visited).Add(lData);
end;

procedure TVisEAdrsRead.SetupParams;
begin
//  Query.ParamAsInteger[ 'owner_oid' ]:= Visited.Owner.OID.AsInteger;
  Visited.Owner.OID.AssignToTIQuery('Owner_OID', Query);

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
  QueryName:= cgQryPersonReadDetail;
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
//  Query.ParamAsInteger[ 'OID' ]:= Visited.OID.AsInteger;
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
  QueryName:= cgQryPersonUpdate;
end;

procedure TVisPersonUpdate.SetupParams;
var
  lData: TPerson;
begin
  lData:= TPerson(Visited);
//  Query.ParamAsInteger[ 'OID' ]       := lData.OID.AsInteger;
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
  QueryName:= cgQryPersonDelete;
end;

procedure TVisPersonDelete.SetupParams;
begin
//  Query.ParamAsInteger[ 'OID' ]:= Visited.OID.AsInteger;
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
  QueryName:= cgQryPersonCreate;
end;

procedure TVisPersonCreate.SetupParams;
var
  lData: TPerson;
begin
  lData:= TPerson(Visited);
//  Query.ParamAsInteger[ 'OID' ]        := lData.OID.AsInteger;
  lData.OID.AssignToTIQuery(Query);
  Query.ParamAsString[  'First_Name' ] := lData.FirstName;
  Query.ParamAsString[  'Family_Name' ]:= lData.LastName;
  Query.ParamAsString[  'Title' ]      := lData.Title;
  Query.ParamAsString[  'Initials' ]   := lData.Initials;
  Query.ParamAsString[  'Notes' ]      := lData.Notes;
  lData.Owner.OID.AssignToTIQuery('Owner_OID', Query);
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
  QueryName:= cgQryPersonEAddressCreate;
end;

procedure TVisEAdrsCreate.SetupParams;
var
  lData: TEAdrs;
begin
  lData:= TEAdrs(Visited);
  lData.OID.AssignToTIQuery(Query);
  lData.Owner.OID.AssignToTIQuery('Owner_OID', Query);
//  Query.ParamAsInteger[  'EAdrs_Type' ]:= lData.AdrsType.OID.AsInteger;
  lData.AdrsType.OID.AssignToTIQuery('EAdrs_Type', Query);
  Query.ParamAsString[  'EAdrs_Text' ]       := lData.Text;
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
  QueryName:= cgQryPersonEAddressDelete;
end;

//------------------------------------------------------------------------------
procedure TVisEAdrsDelete.SetupParams;
begin
//  Query.ParamAsInteger[ 'OID' ]:= Visited.OID.AsInteger;
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
  QueryName:= cgQryPersonEAddressUpdate;
end;

//------------------------------------------------------------------------------
procedure TVisEAdrsUpdate.SetupParams;
var
  lData: TEAdrs;
begin
  lData:= TEAdrs(Visited);
  Visited.OID.AssignToTIQuery(Query);
//  Query.ParamAsInteger[ 'EAdrs_Type' ]:=
  lData.AdrsType.OID.AssignToTIQuery('EAdrs_Type', Query);
  Query.ParamAsString[  'EAdrs_Text' ]:= lData.Text;
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
  QueryName:= cgQryPersonAddressCreate;
end;

//------------------------------------------------------------------------------
procedure TVisAdrsCreate.SetupParams;
var
  lData: TAdrs;
begin
  lData:= TAdrs(Visited);
  lData.OID.AssignToTIQuery(Query);
//  Query.ParamAsInteger[ 'oid'       ]:= lData.OID.AsInteger;
//  Query.ParamAsInteger[ 'owner_oid' ]:= lData.Owner.OID.AsInteger;
//  Query.ParamAsInteger[ 'adrs_type' ]:= lData.AdrsType.OID.AsInteger;
  lData.Owner.OID.AssignToTIQuery('Owner_OID', Query);
  lData.AdrsType.OID.AssignToTIQuery('Adrs_Type', Query);
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
  QueryName:= cgQryPersonAddressUpdate;
end;

//------------------------------------------------------------------------------
procedure TVisAdrsUpdate.SetupParams;
var
  lData: TAdrs;
begin
  lData:= TAdrs(Visited);
  lData.OID.AssignToTIQuery(Query);
//  Query.ParamAsInteger[ 'adrs_type' ]:= lData.AdrsType.OID.AsInteger;
  lData.AdrsType.OID.AssignToTIQuery('adrs_type', Query);
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
  QueryName:= cgQryPersonAddressDelete;
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

{ TVisCompanyRead_Detail }

function TVisCompanyRead_Detail.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompany) and
            (Visited.ObjectState = posPK);
end;

procedure TVisCompanyRead_Detail.Final;
begin
  // Do nothing
end;

procedure TVisCompanyRead_Detail.Init;
begin
  QueryName:= cgQryCompanyReadDetail;
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

{ TVisCompanyDelete }

function TVisCompanyDelete.AcceptVisitor: boolean;
begin
  result:= (inherited AcceptVisitor) and
            (Visited is TCompany);
end;

procedure TVisCompanyDelete.Init;
begin
  QueryName:= cgQryCompanyDelete;
end;

{ TVisCompanyCreate }

function TVisCompanyCreate.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompany) and
            (Visited.ObjectState = posCreate);
end;

procedure TVisCompanyCreate.Init;
begin
  QueryName:= cgQryCompanyCreate;
end;

procedure TVisCompanyCreate.SetupParams;
var
  lData: TCompany;
begin
  lData:= TCompany(Visited);
  Visited.OID.AssignToTIQuery(Query);
  Query.ParamAsString[  'COMPANY_NAME' ]:= lData.CompanyName;
  Query.ParamAsString[  'NOTES' ]       := lData.Notes;
end;

{ TVisCompanyUpdate }

function TVisCompanyUpdate.AcceptVisitor: boolean;
begin
  result:= (Visited is TCompany) and
            (Visited.ObjectState = posUpdate);
end;

procedure TVisCompanyUpdate.Init;
begin
  QueryName:= cgQryCompanyUpdate;
end;

procedure TVisCompanyUpdate.SetupParams;
var
  lData: TCompany;
begin
  lData:= TCompany(Visited);
  Visited.OID.AssignToTIQuery(Query);
  Query.ParamAsString[  'COMPANY_NAME' ]:= lData.CompanyName;
  Query.ParamAsString[  'NOTES' ]       := lData.Notes;
end;

initialization

  // Read PK visitors
  GTIOPFManager.RegReadPKVisitor(TVisCompanyRead_PK);
  GTIOPFManager.RegReadPKVisitor(TVisPersonRead_PK);
  GTIOPFManager.RegReadPKVisitor(TVisCompanyEmployeeRead_PK);

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
