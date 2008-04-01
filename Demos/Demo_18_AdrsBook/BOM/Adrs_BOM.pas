unit Adrs_BOM;

{.$I tiDefines.inc}

interface
uses
  tiObject,
  Classes,
  tiOID,
  AdrsType_BOM;

const
  cErrorPersonNameNotAssigned = 'Please enter the person''s name';
  cErrorCompanyNameNotAssigned = 'Please enter the companies name';

type

  TAdrsBook       = class;
  TPersonList         = class;
  TPerson         = class;
  TCompanyList      = class;
  TCompany        = class;
  TAddressList       = class;
  TEAddressList      = class;
  TAdrsAbs        = class;
  TAdrs           = class;
  TEAdrs          = class;

  // The TAdrsBook class. Top of the tree of the Address Book BOM
  TAdrsBook = class(TtiObject)
  private
    FPeople   : TPersonList   ;
    FCompanies: TCompanyList;
    FEAdrsTypeList: TEAdrsTypeList;
  protected
    function    GetCaption: string; override;
    procedure   SetDeleted(const Value: boolean); override;
  published
    property    People   : TPersonList read FPeople;
    property    Companies: TCompanyList read FCompanies;
    property    EAdrsTypeList: TEAdrsTypeList read FEAdrsTypeList;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   Read; override;
    procedure   FindAllLike(const pLike: string; const pList: TList);
  end;

  TAdrsBookItemAbs = class(TtiObject)
  private
    FAddressList : TAddressList;
    FEAddressList: TEAddressList;
    FNotes: string;
  protected
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Save; override;
    procedure   Read; override;
  published
    property EAddressList: TEAddressList read FEAddressList;
    property AddressList : TAddressList  read FAddressList;
    property Notes: string read FNotes write FNotes;
  end;

  // TPerson class. Holds information about a person
  TPerson = class(TAdrsBookItemAbs)
  private
    FFirstname: string;
    FLastName: string;
    FInitials: string;
    FTitle: string;
  protected
    function  GetCaption: string; override;
    function  GetParent: TAdrsBook; reintroduce;
  public
    property  Parent: TAdrsBook read GetParent;
    function  Clone: TPerson; reintroduce;
    function  IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property LastName : string read FLastName  write FLastName;
    property FirstName: string read FFirstname write FFirstName;
    property Title    : string read FTitle     write FTitle;
    property Initials : string read FInitials  write FInitials;
  end;

  TPersonList   = class(TtiObjectList)
  protected
    function    GetItems(i: integer): TPerson; reintroduce;
    procedure   SetItems(i: integer; const AValue: TPerson); reintroduce;
  public
    property    Items[i:integer]: TPerson read GetItems write SetItems;
    procedure   Add(const AObject: TPerson); reintroduce;
  end;

  TCompanyList   = class(TtiObjectList)
  protected
    function    GetCaption: string; override;
    function    GetItems(i: integer): TCompany; reintroduce;
    procedure   SetItems(i: integer; const Value: TCompany); reintroduce;
  public
    property    Items[i:integer]: TCompany read GetItems write SetItems;
    procedure   Add(AObject: TCompany; pbDefaultDispOrder: boolean = true); reintroduce;
    function    Last: TCompany; reintroduce;
  end;

  TCompany = class(TAdrsBookItemAbs)
  private
    FCompanyName: string;
    FPersonList : TPersonList;
  protected
    function    GetCaption: string; override;
    function    GetOwner: TCompanyList; reintroduce;
    procedure   SetOwner(const Value: TCompanyList); reintroduce;
    procedure   AssignClassProps(pSource: TtiObject); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Owner      : TCompanyList  read GetOwner      write SetOwner;
    function    Clone: TCompany; reintroduce;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property    CompanyName: string read FCompanyName write FCompanyName;
    property    People   : TPersonList read FPersonList;
  end;

  // A list of TAddress objects
  TAddressList  = class(TtiObjectList)
  protected
    function    GetItems(i: integer): TAdrs; reintroduce;
    procedure   SetItems(i: integer; const Value: TAdrs); reintroduce;
    function    GetOID: TtiOID; override;
  public
    property    Items[i:integer]: TAdrs read GetItems write SetItems;
    procedure   Add(AObject: TAdrs  ; pbDefaultDispOrder: boolean = true); reintroduce;
  end;

  // A list of TEAddress objects
  TEAddressList = class(TtiObjectList)
  protected
    function    GetItems(i: integer): TEAdrs; reintroduce;
    procedure   SetItems(i: integer; const Value: TEAdrs); reintroduce;
    function    GetOID: TtiOID; override;
  public
    property    Items[i:integer]: TEAdrs read GetItems write SetItems;
    procedure   Add(AObject: TEAdrs ; pbDefaultDispOrder: boolean = true); reintroduce;
  end;

  TAdrsAbs = class(TtiObject)
  private
//    FAdrsType: TLookupListItem;
    function    GetLookupListItemAsString: string;
    function    GetAdrsTypeOID: String;
    procedure   SetAdrsTypeOID(const pOIDAsString: string);
  published
//    property    AdrsType: TLookupListItem read FAdrsType write FAdrsType;
    property    AdrsTypeAsString: string read GeTLookupListItemAsString ;
    property    AdrsTypeOID: String read GetAdrsTypeOID write SetAdrsTypeOID;
  public
    constructor Create; override;
    procedure   AssignClassProps(pSource: TtiObject); override;
  end;

  // The TAddress class. Holds conventional (street & postage) address info
  TAdrs = class(TAdrsAbs)
  private
    FsLines  : string;
    FsState  : string;
    FsCountry: string;
    FsPCode: string;
    FSuburb: string;
  protected
    function    GetCaption: string; override;
    function    GetOwner: TAddressList; reintroduce;
    procedure   SetOwner(const Value: TAddressList); reintroduce;
  public
    property    Owner      : TAddressList  read GetOwner      write SetOwner;
    function    Clone: TAdrs; reintroduce;
  published
    property Country: string read FsCountry write FsCountry;
    property Lines  : string read FsLines   write FsLines;
    property Suburb : string read FSuburb   write FSuburb;
    property State  : string read FsState   write FsState;
    property PCode  : string read FsPCode   write FsPCode;
  end;

  // The TEAddress class. Holds info about an EAddress like phone number or EMail
  TEAdrs = class(TAdrsAbs)
  private
    FsText: string;
  protected
    function    GetCaption: string; override;
    function    GetOwner: TEAddressList; reintroduce;
    procedure   SetOwner(const Value: TEAddressList); reintroduce;
  public
    property    Owner      : TEAddressList read GetOwner      write SetOwner;
    function    Clone: TEAdrs; reintroduce;
  published
    property Text: string read FsText write FsText;
  end;

function gAdrsBook: TAdrsBook;
procedure FreeAndNilAdrsBook;
procedure PopulateAdrsBook(const pAdrsBook: TAdrsBook);

implementation
uses
  tiUtils
  ,TypInfo
  ,tiOPFManager
  ,tiAutoMap
  ,SysUtils
  ,tiVisitorDB
 ;

var
  uAdrsBook: TAdrsBook;

function gAdrsBook: TAdrsBook;
begin
  if uAdrsBook = nil then
    uAdrsBook:= TAdrsBook.Create;
  result:= uAdrsBook;
end;

procedure FreeAndNilAdrsBook;
begin
  FreeAndNil(uAdrsBook);
end;

procedure PopulateAdrsBook(const pAdrsBook: TAdrsBook);
//var
//  lLookupList: TLookupList;
//  lLookupListItem: TLookupListItem;
begin
//Assert(False, 'Under construction');
////  pAdrsBook.AdrsTypes.MarkListItemsForDeletion;
//  lLookupList:= TLookupList.CreateNew;
//  lLookupList.ObjectState:= posCreate;
//  lLookupList.ListName:= 'ADRS';
//  pAdrsBook.AdrsTypes.Add(lLookupList);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Street Address';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Postal Address';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Home Address';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Work Address';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Other';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupList:= TLookupList.CreateNew;
//  lLookupList.ObjectState:= posCreate;
//  lLookupList.ListName:= 'EADRS';
//  pAdrsBook.AdrsTypes.Add(lLookupList);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Home phone';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Work phone';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Mobile phone';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Fax';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'EMail';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Web';
//  lLookupList.Add(lLookupListItem);
//
//  lLookupListItem:= TLookupListItem.CreateNew;
//  lLookupListItem.Text:= 'Other';
//  lLookupList.Add(lLookupListItem);
//
//  gAdrsBook.AdrsTypes.Save;
//
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAdrsBook
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TAdrsBook.Clear;
begin
  FPeople.Clear;
  FCompanies.Clear;
  FEAdrsTypeList.Clear;

  FCompanies.ObjectState:= posEmpty;
  FPeople.ObjectState:= posEmpty;
  FEAdrsTypeList.ObjectState:= posEmpty;

  ObjectState:= posEmpty;

end;

constructor TAdrsBook.Create;
begin
  inherited;
  FEAdrsTypeList:= TEAdrsTypeList.Create;
  FEAdrsTypeList.Owner:= self;

  FPeople := TPersonList.Create;
  FPeople.Owner:= Self;
  FPeople.OID.AsString:= 'AdrsBook';

  FCompanies:= TCompanyList.Create;
  FCompanies.Owner:= Self;

end;

destructor TAdrsBook.Destroy;
begin
  FEAdrsTypeList.Free;
  FPeople.Free;
  FCompanies.Free;
  inherited;
end;

procedure TAdrsBook.FindAllLike(const pLike: string; const pList: TList);
var
  i: integer;
begin
  pList.Clear;
  for i:= 0 to People.Count - 1 do
    if (not People.Items[i].Deleted) and
       (tiWildCardMatch(People.Items[i].Caption, '*' + pLike + '*' )) then
      pList.Add(People.Items[i]);
  for i:= 0 to Companies.Count - 1 do
    if (not Companies.Items[i].Deleted) and
       (tiWildCardMatch(Companies.Items[i].Caption, '*' + pLike + '*')) then
      pList.Add(Companies.Items[i]);
end;

function TAdrsBook.GetCaption: string;
begin
  result:= 'Address book';
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAdrsBookItemAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TAdrsBookItemAbs.AssignClassProps(ASource: TtiObject);
begin
  FEAddressList.Assign(TAdrsBookItemAbs(ASource).EAddressList);
  FAddressList.Assign(TAdrsBookItemAbs(ASource).AddressList);
end;

constructor TAdrsBookItemAbs.Create;
begin
  inherited;
  FAddressList := TAddressList.Create;
  FAddressList.Owner:= self;
  // ToDo: Refactor to remove need for ItemOwner. Use Parent instead
  FAddressList.ItemOwner:= self;

  FEAddressList:= TEAddressList.Create;
  FEAddressList.Owner:= self;
  // ToDo: Refactor to remove need for ItemOwner. Use Parent instead
  FEAddressList.ItemOwner:= self;

end;

destructor TAdrsBookItemAbs.Destroy;
begin
  FAddressList.Free;
  FEAddressList.Free;
  inherited;
end;

procedure TAdrsBookItemAbs.Read;
begin
  inherited;
end;

procedure TAdrsBookItemAbs.Save;
begin
  inherited;
end;

function TPerson.Clone: TPerson;
begin
  result:= TPerson(inherited Clone);
end;

function TPerson.GetCaption: string;
begin
  result:= LastName + ', ' + FirstName;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TEAddress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TPeople
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TPersonList.Add(const AObject: TPerson);
begin
  inherited Add(AObject);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAddress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{ TAddress }

function TAdrs.Clone: TAdrs;
begin
  result:= TAdrs(inherited Clone);
end;

function TAdrs.GetCaption: string;
begin
  result:= tiStrTran(Lines, CrLf, ' ') + ' ' +
            State + ' ' + PCode;
end;

function TAdrs.GetOwner: TAddressList;
begin
  result:= TAddressList(inherited GetOwner);
end;

procedure TAdrs.SetOwner(const Value: TAddressList);
begin
  inherited SetOwner(Value);
end;

{ TEAddress }

function TEAdrs.Clone: TEAdrs;
begin
  result:= TEAdrs(inherited Clone);
end;

function TEAdrs.GetCaption: string;
begin
  result:= Text;
end;


function TEAdrs.GetOwner: TEAddressList;
begin
    result:= TEAddressList(inherited GetOwner);
end;

procedure TEAdrs.SetOwner(const Value: TEAddressList);
begin
  inherited SetOwner(Value);
end;

{ TAdrsList }

procedure TAddressList.Add(AObject: TAdrs; pbDefaultDispOrder: boolean);
begin
  inherited Add(AObject, pbDefaultDispOrder);
end;

function TAddressList.GetItems(i: integer): TAdrs;
begin
  result:= TAdrs(inherited GetItems(i));
end;

function TAddressList.GetOID: TtiOID;
begin
  if Owner <> nil then
    result:= Owner.OID
  else
    result:= inherited GetOID;
end;

procedure TAddressList.SetItems(i: integer; const Value: TAdrs);
begin
  inherited SetItems(i, Value);
end;

{ TEAddressList }

procedure TEAddressList.Add(AObject: TEAdrs; pbDefaultDispOrder: boolean);
begin
  inherited Add(AObject, pbDefaultDispOrder);
end;

function TPersonList.GetItems(i: integer): TPerson;
begin
  result:= TPerson(inherited GetItems(i));
end;


function TEAddressList.GetItems(i: integer): TEAdrs;
begin
  result:= TEAdrs(inherited GetItems(i));
end;

function TEAddressList.GetOID: TtiOID;
begin
  if Owner <> nil then
    result:= Owner.OID
  else
    result:= inherited GetOID;
end;

procedure TEAddressList.SetItems(i: integer; const Value: TEAdrs);
begin
  inherited SetItems(i, Value);
end;

{ TCompanies }

procedure TCompanyList.Add(AObject: TCompany; pbDefaultDispOrder: boolean);
begin
  inherited Add(AObject, pbDefaultDispOrder);
end;

function TCompanyList.GetCaption: string;
begin
  result:= 'Companies';
end;

function TCompanyList.GetItems(i: integer): TCompany;
begin
  result:= TCompany(inherited GetItems(i));
end;

function TCompanyList.Last: TCompany;
begin
  result:= TCompany(inherited Last);
end;

procedure TCompanyList.SetItems(i: integer; const Value: TCompany);
begin
  inherited SetItems(i, Value);
end;

{ TCompany }

procedure TCompany.AssignClassProps(pSource: TtiObject);
begin
  inherited AssignClassProps(pSource);
  FPersonList.Assign((pSource as TCompany).People);
end;

function TCompany.Clone: TCompany;
begin
  result:= TCompany(inherited clone);
end;

constructor TCompany.Create;
begin
  inherited;
  FPersonList:= TPersonList.Create;
  FPersonList.Owner:= Self;
  // ToDo: Refactor to remove need for ItemOwner. Use Parent instead
  FPersonList.ItemOwner:= Self;
end;

destructor TCompany.Destroy;
begin
  FPersonList.Free;
  inherited;
end;

function TCompany.GetCaption: string;
begin
  result:= CompanyName;
end;

//function TCompany.GetEmployees: TList;
//begin
//  result:= People.List;
//end;

function TCompany.GetOwner: TCompanyList;
begin
  result:= TCompanyList(inherited GetOwner);
end;

function TCompany.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  if (CompanyName = '') then
    AErrors.AddError('CompanyName', cErrorCompanyNameNotAssigned);
  result:= AErrors.Count = 0;
end;

procedure TCompany.SetOwner(const Value: TCompanyList);
begin
  inherited SetOwner(Value);
end;

{ TAdrsAbs }

procedure TAdrsAbs.AssignClassProps(pSource: TtiObject);
begin
  // Copy pointers...
//  Self.AdrsType:= TAdrsAbs(pSource).AdrsType;
Assert(False, 'Under construction');
end;

constructor TAdrsAbs.Create;
begin
  inherited;
end;

function TAdrsAbs.GetAdrsTypeOID: String;
begin
Assert(False, 'Under construction');
//  if FAdrsType <> nil then
//    result:= FAdrsType.OID.AsString
//  else
//    result:= OID.NullOIDAsString;
end;

function TAdrsAbs.GeTLookupListItemAsString: string;
begin
Assert(False, 'Under construction');
//  if FAdrsType <> nil then
//    result:= FAdrsType.Text
//  else
//    result:= 'Unknown';
end;

procedure TPersonList.SetItems(i: integer; const AValue: TPerson);
begin
  inherited SetItems(i, AValue);
end;

function TPerson.GetParent: TAdrsBook;
begin
  result:= TAdrsBook(inherited GetParent);
end;

function TPerson.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  if (FirstName = '') and (LastName = '') then
    AErrors.AddError('', cErrorPersonNameNotAssigned);
  result:= AErrors.Count = 0;
end;

procedure TAdrsAbs.SetAdrsTypeOID(const pOIDAsString: string);
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

Assert(False, 'Under construction');
//  if pOIDAsString <> OID.NullOIDAsString then
//    FAdrsType:= gAdrsBook.AdrsTypes.Find(pOIDAsString)
//  else
//    FAdrsType:= nil;

// This code implements (c)
//var
//  lAdrsBook: TtiObject;
//begin
//  lAdrsBook:= TopOfHierarchy;
//  if (lAdrsBook is TAdrsBook) and (lAdrsBook <> nil) then
//    FAdrsType:= TLookupListItem(TAdrsBook(lAdrsBook).AdrsTypes.Find(Value))
//  else
//    FAdrsType:= nil;
end;

procedure TAdrsBook.Read;
begin
  EAdrsTypeList.Read;
//  GTIOPFManager.ReadPK(Self);
//GTIOPFManager.Read(  Self);
end;

procedure TAdrsBook.SetDeleted(const Value: boolean);
var
  lObjectState: TPerObjectState;
begin
  lObjectState:= ObjectState;
  FPeople.Deleted:= Value;
  FCompanies.Deleted:= Value;
  ObjectState:= lObjectState;
end;

initialization

finalization
  FreeAndNilAdrsBook;
  
end.








