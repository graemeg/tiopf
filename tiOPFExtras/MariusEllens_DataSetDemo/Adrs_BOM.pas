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

  Change history
    Created: Jan 2000

  Notes:
    Business object model for the Address book demo

  ToDo:
    2. Refactor the TPersonList and TPerson classes to use better type casting
       to define their relatioship
    3. Refactor the TCompanyList and TCompany to use better type casting
       to define their relationship

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$I tiDefines.inc}

unit Adrs_BOM;

interface

uses
  tiPtnVisPerObj, Classes, tiPerObjOIDAbs, tiUtils, tiPersist, SysUtils;

type
  TAdrsBook = class;
  TLookupList = class;
  TLookupListItem = class;
  TPeople = class;
  TPerson = class;
  TCompanies = class;
  TCompany = class;
  TAdrsList = class;
  TEAdrsList = class;
  TAdrsAbs = class;
  TAdrs = class;
  TEAdrs = class;


  TLookupLists = class(TPerObjList)
  private
  protected
    function GetItems(i: integer): TLookupList;reintroduce;
    procedure SetItems(i: integer;const Value: TLookupList);reintroduce;
  public
    constructor Create;override;
    procedure Read;override;
    property Items[i: integer]: TLookupList read GetItems write SetItems;
    procedure Add(pObject: TLookupList;pbDefaultDispOrder: boolean = true);reintroduce;
    function Find(pOIDAsString: string): TLookupListItem;reintroduce;
    function FindByListName(const pListName: string): TLookupList;
  end;


  TLookupList = class(TPerObjList)
  private
    FListName: string;
  protected
    function GetItems(i: integer): TLookupListItem;reintroduce;
    procedure SetItems(i: integer;const Value: TLookupListItem);reintroduce;
    function GetOwner: TLookupLists;reintroduce;
    procedure SetOwner(const Value: TLookupLists);reintroduce;
  public
    procedure Read;override;
    property Items[i: integer]: TLookupListItem read GetItems write SetItems;
    procedure Add(pObject: TLookupListItem;pbDefaultDispOrder: boolean = true);reintroduce;
  published
    property ListName: string read FListName write FListName;
  end;


  TLookupListItem = class(TPerObjAbs)
  private
    FText: string;
    function GetID: string; //Added for TTiDataset Demo!
  protected
    function GetOwner: TLookupList;reintroduce;
    procedure SetOwner(const Value: TLookupList);reintroduce;
  public
    property Owner: TLookupList read GetOwner write SetOwner;
  published
    property ID: string read GetId; //Added for TTiDataset Demo!
    property Text: string read FText write FText;
  end;


  // The TAdrsBook class. Top of the tree of the Address Book BOM

  TAdrsBook = class(TPerObjAbs)
  private
    FPeople: TPeople;
    FCompanies: TCompanies;
    FAdrsTypes: TLookupLists;
  protected
    function GetCaption: string;override;
    procedure SetDeleted(const Value: boolean);override;
    // There are problems cloning the entire address book because the TAdrsBook
    // owns the list of valid address types (TLookupLists). Each address owned by
    // a person or a company has a pointer into this list. The AssignClassProps
    // method in TAdrsBookItemAbs has been overridden to copy pointers to these
    // objects so if we are cloning the entire address book, there will be a
    // duplicate of TLookupLists, but the TAdrsBookItemAbs will have pointers
    // into the wrong copy of this list.
    // procedure   AssignClassProps(pSource: TPerObjAbs); override ;
  published
    property People: TPeople read FPeople;
    property Companies: TCompanies read FCompanies;
    property AdrsTypes: TLookupLists read FAdrsTypes;
  public
    constructor Create;override;
    destructor Destroy;override;
    procedure Clear;
    procedure Read(const pDBConnectionName: string;pPerLayerName: string = '');overload;override;
    procedure Read;override;
  end;


  // A list of TPerson objects, with some published properties for display in a
  // TtiTreeView

  TPeople = class(TPerObjList)
  protected
    function GetCaption: string;override;
    function GetItems(i: integer): TPerson;reintroduce;
    procedure SetItems(i: integer;const Value: TPerson);reintroduce;
    function GetOID: TOID;override;
  public
    constructor Create;override;
    property Items[i: integer]: TPerson read GetItems write SetItems;
    procedure Add(pObject: TPerson;pbDefaultDispOrder: boolean = true);reintroduce;
  end;


  TCompanies = class(TPerObjList)
  protected
    function GetCaption: string;override;
    function GetItems(i: integer): TCompany;reintroduce;
    procedure SetItems(i: integer;const Value: TCompany);reintroduce;
  public
    property Items[i: integer]: TCompany read GetItems write SetItems;
    procedure Add(pObject: TCompany;pbDefaultDispOrder: boolean = true);reintroduce;
    function Last: TCompany;reintroduce;
  end;


  TAdrsBookItemAbs = class(TPerObjAbs)
  private
    FAddressList: TAdrsList;
    FEAddressList: TEAdrsList;
    FsNotes: string;
  protected
    procedure AssignClassProps(pSource: TPerObjAbs);override;
  public
    constructor Create;override;
    destructor Destroy;override;
  published
    property EAddressList: TEAdrsList read FEAddressList;
    property AddressList: TAdrsList read FAddressList;
    property Notes: string read FsNotes write FsNotes;
  end;


  TPerson = class(TAdrsBookItemAbs)
  private
    FsFirstname: string;
    FsLastName: string;
    FsInitials: string;
    FsTitle: string;
  protected
    function GetCaption: string;override;
    function GetOwner: TPeople;reintroduce;
    procedure SetOwner(const Value: TPeople);reintroduce;
  public
    property Owner: TPeople read GetOwner write SetOwner;
    function Clone: TPerson;reintroduce;
  published
    property Caption;
    property LastName: string read FsLastName write FsLastName;
    property FirstName: string read FsFirstname write FsFirstName;
    property Title: string read FsTitle write FsTitle;
    property Initials: string read FsInitials write FsInitials;
  end;


  TCompany = class(TAdrsBookItemAbs)
  private
    FCompanyName: string;
    FPersonList: TPeople;
  protected
    function GetCaption: string;override;
    function GetOwner: TCompanies;reintroduce;
    procedure SetOwner(const Value: TCompanies);reintroduce;
    procedure AssignClassProps(pSource: TPerObjAbs);override;
  public
    constructor Create;override;
    destructor Destroy;override;
    property Owner: TCompanies read GetOwner write SetOwner;
    function Clone: TCompany;reintroduce;
  published
    property CompanyName: string read FCompanyName write FCompanyName;
    property People: TPeople read FPersonList;
  end;


  TAdrsList = class(TPerObjList)
  protected
    function GetItems(i: integer): TAdrs;reintroduce;
    procedure SetItems(i: integer;const Value: TAdrs);reintroduce;
    function GetOID: TOID;override;
  public
    property Items[i: integer]: TAdrs read GetItems write SetItems;
    procedure Add(pObject: TAdrs;pbDefaultDispOrder: boolean = true);reintroduce;
  end;


  TEAdrsList = class(TPerObjList)
  protected
    function GetItems(i: integer): TEAdrs;reintroduce;
    procedure SetItems(i: integer;const Value: TEAdrs);reintroduce;
    function GetOID: TOID;override;
  public
    property Items[i: integer]: TEAdrs read GetItems write SetItems;
    procedure Add(pObject: TEAdrs;pbDefaultDispOrder: boolean = true);reintroduce;
  end;


  TAdrsAbs = class(TPerObjAbs)
  private
    FAdrsType: TLookupListItem;
    function GetLookupListItemAsString: string;
    function GetAdrsTypeOID: integer;
    procedure SetAdrsTypeOID(const pOIDAsString: integer);
  published
    property AdrsType: TLookupListItem read FAdrsType write FAdrsType;
    property AdrsTypeAsString: string read GeTLookupListItemAsString;
    //Corrected to integer because tiClassToDBMap_Srv accessed it as a integer
    property AdrsTypeOID: integer read GetAdrsTypeOID write SetAdrsTypeOID;
  public
    constructor Create;override;
    procedure AssignClassProps(pSource: TPerObjAbs);override;
  end;


  TAdrs = class(TAdrsAbs)
  private
    FsLines: string;
    FsState: string;
    FsCountry: string;
    FsPCode: string;
    FSuburb: string;
  protected
    function GetCaption: string;override;
    function GetOwner: TAdrsList;reintroduce;
    procedure SetOwner(const Value: TAdrsList);reintroduce;
  public
    property Owner: TAdrsList read GetOwner write SetOwner;
    function Clone: TAdrs;reintroduce;
  published
    property Country: string read FsCountry write FsCountry;
    property Lines: string read FsLines write FsLines;
    property Suburb: string read FSuburb write FSuburb;
    property State: string read FsState write FsState;
    property PCode: string read FsPCode write FsPCode;
  end;


  TEAdrs = class(TAdrsAbs)
  private
    FsText: string;
  protected
    function GetCaption: string;override;
    function GetOwner: TEAdrsList;reintroduce;
    procedure SetOwner(const Value: TEAdrsList);reintroduce;
  public
    property Owner: TEAdrsList read GetOwner write SetOwner;
    function Clone: TEAdrs;reintroduce;
  published
    property Text: string read FsText write FsText;
  end;

function gAdrsBook: TAdrsBook;
procedure FreeAndNilAdrsBook;
procedure PopulateAdrsBook;

implementation

var
  uAdrsBook: TAdrsBook;

function gAdrsBook: TAdrsBook;
begin
  if uAdrsBook = nil
  then uAdrsBook := TAdrsBook.Create;
  Result := uAdrsBook;
end;

procedure FreeAndNilAdrsBook;
begin
  FreeAndNil(uAdrsBook);
end;

procedure PopulateAdrsBook;
var lLookupList: TLookupList;
  lLookupListItem: TLookupListItem;
begin
  FreeAndNilAdrsbook;
  lLookupList := TLookupList.CreateNew;
  lLookupList.OID.AsString := '1';
  lLookupList.ObjectState := posCreate;
  lLookupList.ListName := 'ADRS';
  gAdrsBook.AdrsTypes.Add(lLookupList);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '3';
  lLookupListItem.Text := 'Work - Street Address';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '4';
  lLookupListItem.Text := 'Work - Postal';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '5';
  lLookupListItem.Text := 'Home - Street Address';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '6';
  lLookupListItem.Text := 'Home - Postal';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '7';
  lLookupListItem.Text := 'Other';
  lLookupList.Add(lLookupListItem);

  lLookupList := TLookupList.CreateNew;
  lLookupList.OID.AsString := '2';
  lLookupList.ObjectState := posCreate;
  lLookupList.ListName := 'EADRS';
  gAdrsBook.AdrsTypes.Add(lLookupList);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '8';
  lLookupListItem.Text := 'Web';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '9';
  lLookupListItem.Text := 'Phone - Home';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '10';
  lLookupListItem.Text := 'Phone - Work';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '11';
  lLookupListItem.Text := 'Phone - Mobile';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '13';
  lLookupListItem.Text := 'Fax';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '14';
  lLookupListItem.Text := 'EMail';
  lLookupList.Add(lLookupListItem);

  lLookupListItem := TLookupListItem.CreateNew;
  lLookupListItem.OID.AsString := '15';
  lLookupListItem.Text := 'Other';
  lLookupList.Add(lLookupListItem);

  gAdrsBook.AdrsTypes.Save;
end;



{ TLookupLists }

procedure TLookupLists.Add(pObject: TLookupList;pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

constructor TLookupLists.Create;
begin
  inherited;
  OID.AsString := '0'; // Because if it's -1, then find will fail when passed -1 as a param
end;

function TLookupLists.Find(pOIDAsString: string): TLookupListItem;
begin
  Result :=(inherited Find(pOIDAsString))as TLookupListItem;
end;

function TLookupLists.FindByListName(const pListName: string): TLookupList;
begin
  Result := TLookupList(FindByProps(['ListName'],[pListName], false));
  Assert(Result <> nil, 'Unable to find list name <' + pListName + '>');
end;

function TLookupLists.GetItems(i: integer): TLookupList;
begin
  Result := TLookupList(inherited GetItems(i));
end;

procedure TLookupLists.Read;
begin
  inherited;
  SortByOID;
end;

procedure TLookupLists.SetItems(i: integer;const Value: TLookupList);
begin
  inherited SetItems(i, Value);
end;



{ TLookupList }

procedure TLookupList.Add(pObject: TLookupListItem;pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TLookupList.GetItems(i: integer): TLookupListItem;
begin
  Result := TLookupListItem(inherited GetItems(i));
end;

function TLookupList.GetOwner: TLookupLists;
begin
  Result := TLookupLists(inherited GetOwner);
end;

procedure TLookupList.Read;
begin
  inherited;
  SortByOID;
end;

procedure TLookupList.SetItems(i: integer;const Value: TLookupListItem);
begin
  inherited SetItems(i, Value);
end;

procedure TLookupList.SetOwner(const Value: TLookupLists);
begin
  inherited SetOwner(Value);
end;



{ TLookupList }

function TLookupListItem.GetID: string;
begin
  Result := Oid.AsString;
end;

function TLookupListItem.GetOwner: TLookupList;
begin
  Result := TLookupList(inherited GetOwner);
end;

procedure TLookupListItem.SetOwner(const Value: TLookupList);
begin
  inherited SetOwner(Value);
end;


{ TAdrsBook }

procedure TAdrsBook.Clear;
begin
  FPeople.Clear;
  FCompanies.Clear;
  FAdrsTypes.Clear;

  FCompanies.ObjectState := posEmpty;
  FPeople.ObjectState := posEmpty;
  FAdrsTypes.ObjectState := posEmpty;

  ObjectState := posEmpty;
end;

constructor TAdrsBook.Create;
begin
  inherited;
  FAdrsTypes := TLookupLists.Create;
  FAdrsTypes.Owner := Self;

  FPeople := TPeople.Create;
  FPeople.Owner := Self;

  FCompanies := TCompanies.Create;
  FCompanies.Owner := Self;
end;

destructor TAdrsBook.Destroy;
begin
  FAdrsTypes.Free;
  FPeople.Free;
  FCompanies.Free;
  inherited;
end;

function TAdrsBook.GetCaption: string;
begin
  Result := 'Address book';
end;



{ TAdrsBookItemAbs }

procedure TAdrsBookItemAbs.AssignClassProps(pSource: TPerObjAbs);
begin
  FEAddressList.Assign(TAdrsBookItemAbs(pSource).EAddressList);
  FAddressList.Assign(TAdrsBookItemAbs(pSource).AddressList);
end;

constructor TAdrsBookItemAbs.Create;
begin
  inherited;
  FAddressList := TAdrsList.Create;
  FAddressList.Owner := Self;
  FAddressList.ItemOwner := Self;

  FEAddressList := TEAdrsList.Create;
  FEAddressList.Owner := Self;
  FEAddressList.ItemOwner := Self;
end;

destructor TAdrsBookItemAbs.Destroy;
begin
  FAddressList.Free;
  FEAddressList.Free;
  inherited;
end;

function TPerson.Clone: TPerson;
begin
  Result := TPerson(inherited Clone);
end;

function TPerson.GetCaption: string;
begin
  Result := LastName + ', ' + FirstName;
end;



{ TPeople }

procedure TPeople.Add(pObject: TPerson;pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

constructor TPeople.Create;
begin
  inherited;
  OID.AsString := '-1';
end;

function TPeople.GetCaption: string;
begin
  Result := 'People';
end;


{ TAdr }

function TAdrs.Clone: TAdrs;
begin
  Result := TAdrs(inherited Clone);
end;

function TAdrs.GetCaption: string;
begin
  Result := tiStrTran(Lines, CrLf, ' ') + ' ' +
  State + ' ' + PCode;
end;

function TAdrs.GetOwner: TAdrsList;
begin
  Result := TAdrsList(inherited GetOwner);
end;

procedure TAdrs.SetOwner(const Value: TAdrsList);
begin
  inherited SetOwner(Value);
end;

{ TEAdrs }

function TEAdrs.Clone: TEAdrs;
begin
  Result := TEAdrs(inherited Clone);
end;

function TEAdrs.GetCaption: string;
begin
  Result := Text;
end;

function TEAdrs.GetOwner: TEAdrsList;
begin
  Result := TEAdrsList(inherited GetOwner);
end;

procedure TEAdrs.SetOwner(const Value: TEAdrsList);
begin
  inherited SetOwner(Value);
end;

{ TAdrsList }

procedure TAdrsList.Add(pObject: TAdrs;pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TAdrsList.GetItems(i: integer): TAdrs;
begin
  Result := TAdrs(inherited GetItems(i));
end;

function TAdrsList.GetOID: TOID;
begin
  if Owner <> nil
  then Result := Owner.OID
  else Result := inherited GetOID;
end;

procedure TAdrsList.SetItems(i: integer;const Value: TAdrs);
begin
  inherited SetItems(i, Value);
end;

{ TEAdrsList }

procedure TEAdrsList.Add(pObject: TEAdrs;pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TPeople.GetItems(i: integer): TPerson;
begin
  Result := TPerson(inherited GetItems(i));
end;

function TEAdrsList.GetItems(i: integer): TEAdrs;
begin
  Result := TEAdrs(inherited GetItems(i));
end;

function TEAdrsList.GetOID: TOID;
begin
  if Owner <> nil
  then Result := Owner.OID
  else Result := inherited GetOID;
end;

procedure TEAdrsList.SetItems(i: integer;const Value: TEAdrs);
begin
  inherited SetItems(i, Value);
end;

{ TCompanies }

procedure TCompanies.Add(pObject: TCompany;pbDefaultDispOrder: boolean);
begin
  inherited Add(pObject, pbDefaultDispOrder);
end;

function TCompanies.GetCaption: string;
begin
  Result := 'Companies';
end;

function TCompanies.GetItems(i: integer): TCompany;
begin
  Result := TCompany(inherited GetItems(i));
end;

function TCompanies.Last: TCompany;
begin
  Result := TCompany(inherited Last);
end;

procedure TCompanies.SetItems(i: integer;const Value: TCompany);
begin
  inherited SetItems(i, Value);
end;

{ TCompany }

procedure TCompany.AssignClassProps(pSource: TPerObjAbs);
begin
  inherited AssignClassProps(pSource);
  FPersonList.Assign((pSource as TCompany).People);
end;

function TCompany.Clone: TCompany;
begin
  Result := TCompany(inherited clone);
end;

constructor TCompany.Create;
begin
  inherited;
  FPersonList := TPeople.Create;
  FPersonList.Owner := Self;
  FPersonList.ItemOwner := Self;
end;

destructor TCompany.Destroy;
begin
  FPersonList.Free;
  inherited;
end;

function TCompany.GetCaption: string;
begin
  Result := CompanyName;
end;

function TCompany.GetOwner: TCompanies;
begin
  Result := TCompanies(inherited GetOwner);
end;

procedure TCompany.SetOwner(const Value: TCompanies);
begin
  inherited SetOwner(Value);
end;

{ TAdrsAbs }

procedure TAdrsAbs.AssignClassProps(pSource: TPerObjAbs);
begin
  // Copy pointers...
  Self.AdrsType := TAdrsAbs(pSource).AdrsType;
end;

constructor TAdrsAbs.Create;
begin
  inherited;
  SelfIterate := false;
end;

function TAdrsAbs.GetAdrsTypeOID: integer;
begin
  if FAdrsType <> nil
  then Result := StrToInt(FAdrsType.OID.AsString)
  else Result := StrToInt(OID.NullOIDAsString);
end;

function TAdrsAbs.GeTLookupListItemAsString: string;
begin
  if FAdrsType <> nil
  then Result := FAdrsType.Text
  else Result := 'Unknown';
end;

procedure TAdrsAbs.SetAdrsTypeOID(const pOIDAsString: integer);
begin
  // Not sure that we have got this righ yet. Options:
  // a) A global LookupList, which can be accessed even if this
  //    instance of TAdrsAbs does not yet have an owner.
  // b) LookupList owned by AdrsBook, but AdrsBook is a global, singleton
  //    so the LookupList can be accessed by gAdrsBook.LookLists
  // c) A property on AdrsBook with access via Owner.Owner...
  //    Problem with this is that the TAdrsAbs may not have it's owner
  //    set yet.
  // The code below implements (b)
  if IntToStr(pOIDAsString) <> OID.NullOIDAsstring
  then FAdrsType := gAdrsBook.AdrsTypes.Find(IntToStr(pOIDAsstring))
  else FAdrsType := nil;

  // This code implements (c)
  //var
  //  lAdrsBook : TPerObjAbs ;
  //begin
  //  lAdrsBook := TopOfHierarchy ;
  //  if ( lAdrsBook is TAdrsBook ) and ( lAdrsBook <> nil ) then
  //    FAdrsType := TLookupListItem( TAdrsBook( lAdrsBook ).AdrsTypes.Find( Value ))
  //  else
  //    FAdrsType := nil ;
end;

{ TPeople }
// This is a hack to get the auto mapping working

function TPeople.GetOID: TOID;
begin
  if Owner <> nil
  then Result := Owner.OID
  else Result := inherited GetOID;
end;

procedure TPeople.SetItems(i: integer;const Value: TPerson);
begin
  inherited SetItems(i, Value);
end;

function TPerson.GetOwner: TPeople;
begin
  Result := TPeople(inherited GetOwner);
end;

procedure TPerson.SetOwner(const Value: TPeople);
begin
  inherited SetOwner(Value);
end;

{ TAdrsBook }

procedure TAdrsBook.Read(const pDBConnectionName: string;pPerLayerName: string = '');
begin
  gTIPerMgr.Read(AdrsTypes, pDBConnectionName, pPerLayerName);
  gTIPerMgr.ReadPK(Self, pDBConnectionName, pPerLayerName);
  gTIPerMgr.Read(Self, pDBConnectionName, pPerLayerName);
end;

procedure TAdrsBook.Read;
begin
  gTIPerMgr.Read(AdrsTypes);
  gTIPerMgr.ReadPK(Self);
  gTIPerMgr.Read(Self);
end;

procedure TAdrsBook.SetDeleted(const Value: boolean);
var lObjectState: TPerObjectState;
begin
  lObjectState := ObjectState;
  FPeople.Deleted := Value;
  FCompanies.Deleted := Value;
  ObjectState := lObjectState;
end;

initialization
  RegisterClasses([TPerson, TCompany, TAdrs, TEAdrs, TLookupListItem]);

finalization
  uAdrsBook.Free;
end.


