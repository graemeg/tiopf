unit Adrs_BOM;

{.$I tiDefines.inc}

interface
uses
   tiObject
  ,Classes
  ,tiOID
  ;

const
  cErrorPersonNameNotAssigned = 'Please enter the person''s name' ;
  cErrorCompanyNameNotAssigned = 'Please enter the companies name' ;

type

  TAdrsBook       = class ;
  TLookupLists    = class ;
  TLookupList     = class ;
  TLookupListItem = class ;
  TPeople         = class ;
  TPerson         = class ;
  TCompanies      = class ;
  TCompany        = class ;
  TAdrsList       = class ;
  TEAdrsList      = class ;
  TAdrsAbs        = class ;
  TAdrs           = class ;
  TEAdrs          = class ;

  //----------------------------------------------------------------------------
  TLookupLists = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TLookupList ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TLookupList); reintroduce ;
  public
    constructor Create ; override ;
    procedure   Read ; override ;
    property    Items[i:integer] : TLookupList read GetItems write SetItems ;
    procedure   Add( pObject     : TLookupList   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    function    Find( pOIDAsString : String  ) : TLookupListItem ; reintroduce ;
    function    FindByListName( const pListName : string ) : TLookupList ;
  end ;

  //----------------------------------------------------------------------------
  TLookupList    = class( TtiObjectList )
  private
    FListName: string;
  protected
    function    GetItems(i: integer): TLookupListItem ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TLookupListItem); reintroduce ;
    function    GetOwner: TLookupLists; reintroduce ;
    procedure   SetOwner(const Value: TLookupLists); reintroduce ;
  public
    procedure   Read ; override ;
    property    Items[i:integer] : TLookupListItem read GetItems write SetItems ;
    procedure   Add( pObject     : TLookupListItem   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
  published
    property    ListName : string read FListName write FListName ;
  end ;

  //----------------------------------------------------------------------------
  TLookupListItem     = class( TtiObject )
  private
    FText: string;
  protected
    function    GetOwner: TLookupList; reintroduce ;
    procedure   SetOwner(const Value: TLookupList); reintroduce ;
  public
    property    Owner       : TLookupList             read GetOwner      write SetOwner ;
  published
    property    Text : string read FText write FText ;
  end ;

  // The TAdrsBook class. Top of the tree of the Address Book BOM
  //----------------------------------------------------------------------------
  TAdrsBook = class( TtiObject )
  private
    FPeople    : TPeople    ;
    FCompanies : TCompanies;
    FAdrsTypes: TLookupLists;
  protected
    function    GetCaption : string ; override ;
    procedure   SetDeleted(const Value: boolean); override ;
    // There are problems cloning the entire address book because the TAdrsBook
    // owns the list of valid address types (TLookupLists). Each address owned by
    // a person or a company has a pointer into this list. The AssignClassProps
    // method in TAdrsBookItemAbs has been overridden to copy pointers to these
    // objects so if we are cloning the entire address book, there will be a
    // duplicate of TLookupLists, but the TAdrsBookItemAbs will have pointers
    // into the wrong copy of this list.
    // procedure   AssignClassProps(pSource: TtiObject); override ;
  published
    property    People    : TPeople read FPeople ;
    property    Companies : TCompanies read FCompanies ;
    property    AdrsTypes : TLookupLists read FAdrsTypes ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Clear ;
    procedure   Read( const pDBConnectionName : string ; pPerLayerName : string = '' ) ; override ;
    procedure   FindAllLike( const pLike : string ; const pList : TList ) ;
  end ;

  // A list of TPerson objects, with some published properties for display in a
  // TtiTreeView
  //----------------------------------------------------------------------------
  TPeople   = class( TtiObjectList )
  protected
    function    GetCaption : string ; override ;
    function    GetItems(i: integer): TPerson ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TPerson); reintroduce ;
    function    GetOID : TOID ; override ;
  public
    constructor Create ; override ;
    property    Items[i:integer] : TPerson read GetItems write SetItems ;
    procedure   Add( pObject : TPerson ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
  end ;

  //----------------------------------------------------------------------------
  TCompanies   = class( TtiObjectList )
  protected
    function    GetCaption : string ; override ;
    function    GetItems(i: integer): TCompany ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TCompany); reintroduce ;
  public
    property    Items[i:integer] : TCompany read GetItems write SetItems ;
    procedure   Add( pObject : TCompany ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    function    Last : TCompany ; reintroduce ;
  end ;

  //----------------------------------------------------------------------------
  TAdrsBookItemAbs = class( TtiObject )
  private
    FAddressList  : TAdrsList ;
    FEAddressList : TEAdrsList ;
    FsNotes: string;
  protected
    procedure   AssignClassProps(pSource: TtiObject); override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
  published
    property EAddressList : TEAdrsList read FEAddressList ;
    property AddressList  : TAdrsList  read FAddressList ;
    property Notes : string read FsNotes write FsNotes ;
  end ;

  // TPerson class. Holds information about a person
  //----------------------------------------------------------------------------
  TPerson = class( TAdrsBookItemAbs )
  private
    FsFirstname: string;
    FsLastName: string;
    FsInitials : string ;
    FsTitle: string;
  protected
    function    GetCaption : string ; override ;
    function    GetOwner: TPeople; reintroduce ;
    procedure   SetOwner(const Value: TPeople); reintroduce ;
  public
    property    Owner       : TPeople read GetOwner      write SetOwner ;
    function    Clone : TPerson ; reintroduce ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property Caption ;
    property LastName  : string read FsLastName  write FsLastName ;
    property FirstName : string read FsFirstname write FsFirstName ;
    property Title     : string read FsTitle     write FsTitle ;
    property Initials  : string read FsInitials  write FsInitials ;
  end ;

  //----------------------------------------------------------------------------
  TCompany = class( TAdrsBookItemAbs )
  private
    FCompanyName : string;
    FPersonList  : TPeople;
  protected
    function    GetCaption : string ; override ;
    function    GetOwner: TCompanies; reintroduce ;
    procedure   SetOwner(const Value: TCompanies); reintroduce ;
    procedure   AssignClassProps(pSource: TtiObject); override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Owner       : TCompanies  read GetOwner      write SetOwner ;
    function    Clone : TCompany ; reintroduce ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; override ;
  published
    property    CompanyName : string read FCompanyName write FCompanyName ;
    property    People    : TPeople read FPersonList ;
  end ;

  // A list of TAddress objects
  //----------------------------------------------------------------------------
  TAdrsList  = class( TtiObjectList )
  protected
    function    GetItems(i: integer): TAdrs ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TAdrs); reintroduce ;
    function    GetOID : TOID ; override ;
  public
    property    Items[i:integer] : TAdrs read GetItems write SetItems ;
    procedure   Add( pObject : TAdrs   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
  end;

  // A list of TEAddress objects
  //----------------------------------------------------------------------------
  TEAdrsList = class( TtiObjectList )
  protected
    function    GetItems(i: integer): TEAdrs ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TEAdrs); reintroduce ;
    function    GetOID : TOID ; override ;
  public
    property    Items[i:integer] : TEAdrs read GetItems write SetItems ;
    procedure   Add( pObject : TEAdrs  ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
  end;

  //----------------------------------------------------------------------------
  TAdrsAbs = class( TtiObject )
  private
    FAdrsType: TLookupListItem ;
    function    GetLookupListItemAsString: string;
    function    GetAdrsTypeOID: String ;
    procedure   SetAdrsTypeOID(const pOIDAsString : string );
  published
    property    AdrsType : TLookupListItem read FAdrsType write FAdrsType ;
    property    AdrsTypeAsString : string read GeTLookupListItemAsString  ;
    property    AdrsTypeOID : String read GetAdrsTypeOID write SetAdrsTypeOID ;
  public
    constructor Create ; override ;
    procedure   AssignClassProps( pSource : TtiObject ) ; override ;
  end ;

  // The TAddress class. Holds conventional ( street & postage ) address info
  //----------------------------------------------------------------------------
  TAdrs = class( TAdrsAbs )
  private
    FsLines   : string ;
    FsState   : string;
    FsCountry: string;
    FsPCode: string;
    FSuburb: string;
  protected
    function    GetCaption : string ; override ;
    function    GetOwner: TAdrsList ; reintroduce ;
    procedure   SetOwner(const Value: TAdrsList); reintroduce ;
  public
    property    Owner       : TAdrsList  read GetOwner      write SetOwner ;
    function    Clone : TAdrs ; reintroduce ;
  published
    property Country : string read FsCountry write FsCountry ;
    property Lines   : string read FsLines   write FsLines ;
    property Suburb  : string read FSuburb   write FSuburb ;
    property State   : string read FsState   write FsState ;
    property PCode   : string read FsPCode   write FsPCode ;
  end ;

  // The TEAddress class. Holds info about an EAddress like phone number or EMail
  //----------------------------------------------------------------------------
  TEAdrs = class( TAdrsAbs )
  private
    FsText: string;
  protected
    function    GetCaption: string; override ;
    function    GetOwner: TEAdrsList ; reintroduce ;
    procedure   SetOwner(const Value: TEAdrsList); reintroduce ;
  public
    property    Owner       : TEAdrsList read GetOwner      write SetOwner ;
    function    Clone : TEAdrs ; reintroduce ;
  published
    property Text : string read FsText write FsText ;
  end ;

function gAdrsBook : TAdrsBook ;
procedure FreeAndNilAdrsBook;
procedure PopulateAdrsBook( const pAdrsBook : TAdrsBook ) ;

implementation
uses
  tiUtils
  ,TypInfo
  ,tiOPFManager
  ,tiClassToDBMap_BOM
  ,SysUtils
  ,tiVisitorDB
  ;

var
  uAdrsBook : TAdrsBook ;

function gAdrsBook : TAdrsBook ;
begin
  if uAdrsBook = nil then
    uAdrsBook := TAdrsBook.Create ;
  result := uAdrsBook ;
end ;

procedure FreeAndNilAdrsBook;
begin
  FreeAndNil( uAdrsBook ) ;
end ;

procedure PopulateAdrsBook( const pAdrsBook : TAdrsBook ) ;
var
  lLookupList : TLookupList ;
  lLookupListItem : TLookupListItem ;
begin
Assert(False, 'Under construction');
//  pAdrsBook.AdrsTypes.MarkListItemsForDeletion ;
  lLookupList := TLookupList.CreateNew ;
  lLookupList.ObjectState := posCreate ;
  lLookupList.ListName := 'ADRS' ;
  pAdrsBook.AdrsTypes.Add( lLookupList ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Street Address';
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Postal Address';
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Home Address';
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Work Address';
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Other';
  lLookupList.Add( lLookupListItem ) ;

  lLookupList := TLookupList.CreateNew ;
  lLookupList.ObjectState := posCreate ;
  lLookupList.ListName := 'EADRS' ;
  pAdrsBook.AdrsTypes.Add( lLookupList ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Home phone' ;
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Work phone' ;
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Mobile phone' ;
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Fax' ;
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'EMail' ;
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Web' ;
  lLookupList.Add( lLookupListItem ) ;

  lLookupListItem := TLookupListItem.CreateNew ;
  lLookupListItem.Text := 'Other' ;
  lLookupList.Add( lLookupListItem ) ;

  gAdrsBook.AdrsTypes.Save ;

end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLookupLists
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TLookupLists.Add(pObject: TLookupList; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject ) ;
end;

//------------------------------------------------------------------------------
constructor TLookupLists.Create;
begin
  inherited;
  OID.AsString := '0' ; // Because if it's -1, then find will fail when passed -1 as a param
end;

function TLookupLists.Find(pOIDAsString : string ): TLookupListItem;
begin
  result := ( inherited Find( pOIDAsString )) as TLookupListItem ;
end;

//------------------------------------------------------------------------------
function TLookupLists.FindByListName(const pListName: string): TLookupList;
begin
  result := TLookupList( FindByProps( ['ListName'], [pListName], false )) ;
  Assert( result <> nil, 'Unable to find list name <' + pListName + '>' ) ;
end;

//------------------------------------------------------------------------------
function TLookupLists.GetItems(i: integer): TLookupList;
begin
  result := TLookupList( inherited GetItems( i )) ;
end;

//------------------------------------------------------------------------------
procedure TLookupLists.Read;
begin
  inherited;
  SortByOID ;
end;

procedure TLookupLists.SetItems(i: integer; const Value: TLookupList);
begin
  inherited SetItems( i, Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLookupList
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TLookupList.Add(pObject: TLookupListItem; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject ) ;
end;

//------------------------------------------------------------------------------
function TLookupList.GetItems(i: integer): TLookupListItem;
begin
  result := TLookupListItem( inherited GetItems( i )) ;
end;

//------------------------------------------------------------------------------
function TLookupList.GetOwner: TLookupLists;
begin
  result := TLookupLists( inherited GetOwner ) ;
end;

//------------------------------------------------------------------------------
procedure TLookupList.Read;
begin
  inherited;
  SortByOID ;
end;

procedure TLookupList.SetItems(i: integer; const Value: TLookupListItem);
begin
  inherited SetItems( i, Value ) ;
end;

//------------------------------------------------------------------------------
procedure TLookupList.SetOwner(const Value: TLookupLists);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TLookupList
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TLookupListItem.GetOwner: TLookupList;
begin
  result := TLookupList( inherited GetOwner ) ;
end;

//------------------------------------------------------------------------------
procedure TLookupListItem.SetOwner(const Value: TLookupList);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAdrsBook
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TAdrsBook.Clear;
begin
  FPeople.Clear ;
  FCompanies.Clear ;
  FAdrsTypes.Clear ;

  FCompanies.ObjectState := posEmpty ;
  FPeople.ObjectState := posEmpty ;
  FAdrsTypes.ObjectState := posEmpty ;

  ObjectState := posEmpty ;

end;

constructor TAdrsBook.Create;
begin
  inherited;
  FAdrsTypes := TLookupLists.Create ;
  FAdrsTypes.Owner := self ;

  FPeople  := TPeople.Create ;
  FPeople.Owner := Self ;
  FPeople.OID.AsString := 'AdrsBook' ;

  FCompanies := TCompanies.Create ;
  FCompanies.Owner := Self ;

end;

//------------------------------------------------------------------------------
destructor TAdrsBook.Destroy;
begin
  FAdrsTypes.Free ;
  FPeople.Free ;
  FCompanies.Free ;
  inherited;
end;

//------------------------------------------------------------------------------
procedure TAdrsBook.FindAllLike(const pLike: string; const pList: TList);
var
  i : integer ;
begin
  pList.Clear ;
  for i := 0 to People.Count - 1 do
    if ( not People.Items[i].Deleted ) and
       ( tiWildCardMatch( People.Items[i].Caption, '*' + pLike + '*'  )) then
      pList.Add(People.Items[i]);
  for i := 0 to Companies.Count - 1 do
    if ( not Companies.Items[i].Deleted ) and
       ( tiWildCardMatch( Companies.Items[i].Caption, '*' + pLike + '*' )) then
      pList.Add(Companies.Items[i]);
end;

function TAdrsBook.GetCaption: string;
begin
  result := 'Address book' ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAdrsBookItemAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TAdrsBookItemAbs.AssignClassProps(pSource: TtiObject);
begin
  FEAddressList.Assign( TAdrsBookItemAbs( pSource ).EAddressList ) ;
  FAddressList.Assign( TAdrsBookItemAbs( pSource ).AddressList ) ;
end;

constructor TAdrsBookItemAbs.Create;
begin
  inherited;
  FAddressList  := TAdrsList.Create ;
  FAddressList.Owner := self ;
  FAddressList.ItemOwner := self ;

  FEAddressList := TEAdrsList.Create ;
  FEAddressList.Owner := self ;
  FEAddressList.ItemOwner := self ;

end;

//------------------------------------------------------------------------------
destructor TAdrsBookItemAbs.Destroy;
begin
  FAddressList.Free ;
  FEAddressList.Free ;
  inherited;
end;

//------------------------------------------------------------------------------
function TPerson.Clone: TPerson;
begin
  result := TPerson( inherited Clone ) ;
end;

function TPerson.GetCaption: string;
begin
  result := LastName + ', ' + FirstName ;
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
procedure TPeople.Add(pObject: TPerson; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

constructor TPeople.Create;
begin
  inherited;
  OID.AsString := '-1' ;
end;

function TPeople.GetCaption: string;
begin
  result := 'People' ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAddress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{ TAddress }

function TAdrs.Clone: TAdrs;
begin
  result := TAdrs( inherited Clone ) ;
end;

function TAdrs.GetCaption: string;
begin
  result := tiStrTran( Lines, CrLf, ' ' ) + ' ' +
            State + ' ' + PCode ;
end;

function TAdrs.GetOwner: TAdrsList;
begin
  result := TAdrsList( inherited GetOwner );
end;

procedure TAdrs.SetOwner(const Value: TAdrsList);
begin
  inherited SetOwner( Value ) ;
end;

{ TEAddress }

function TEAdrs.Clone: TEAdrs;
begin
  result := TEAdrs( inherited Clone ) ;
end;

function TEAdrs.GetCaption: string;
begin
  result := Text ;
end;


function TEAdrs.GetOwner: TEAdrsList;
begin
    result := TEAdrsList( inherited GetOwner );
end;

procedure TEAdrs.SetOwner(const Value: TEAdrsList);
begin
  inherited SetOwner( Value ) ;
end;

{ TAdrsList }

procedure TAdrsList.Add(pObject: TAdrs; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TAdrsList.GetItems(i: integer): TAdrs;
begin
  result := TAdrs( inherited GetItems( i )) ;
end;

function TAdrsList.GetOID: TOID;
begin
  if Owner <> nil then
    result := Owner.OID
  else
    result := inherited GetOID ;
end;

procedure TAdrsList.SetItems(i: integer; const Value: TAdrs);
begin
  inherited SetItems( i, Value ) ;
end;

{ TEAddressList }

procedure TEAdrsList.Add(pObject: TEAdrs; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TPeople.GetItems(i: integer): TPerson;
begin
  result := TPerson( inherited GetItems( i )) ;
end;


function TEAdrsList.GetItems(i: integer): TEAdrs;
begin
  result := TEAdrs( inherited GetItems( i )) ;
end;

function TEAdrsList.GetOID: TOID;
begin
  if Owner <> nil then
    result := Owner.OID
  else
    result := inherited GetOID ;
end;

procedure TEAdrsList.SetItems(i: integer; const Value: TEAdrs);
begin
  inherited SetItems( i, Value ) ;
end;

{ TCompanies }

procedure TCompanies.Add(pObject: TCompany; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TCompanies.GetCaption: string;
begin
  result := 'Companies' ;
end;

function TCompanies.GetItems(i: integer): TCompany;
begin
  result := TCompany( inherited GetItems( i )) ;
end;

function TCompanies.Last: TCompany;
begin
  result := TCompany( inherited Last ) ;
end;

procedure TCompanies.SetItems(i: integer; const Value: TCompany);
begin
  inherited SetItems( i, Value ) ;
end;

{ TCompany }

procedure TCompany.AssignClassProps(pSource: TtiObject);
begin
  inherited AssignClassProps( pSource ) ;
  FPersonList.Assign(( pSource as TCompany ).People ) ;
end;

function TCompany.Clone: TCompany;
begin
  result := TCompany( inherited clone ) ;
end;

constructor TCompany.Create;
begin
  inherited;
  FPersonList := TPeople.Create ;
  FPersonList.Owner := Self ;
  FPersonList.ItemOwner := Self ;
end;

destructor TCompany.Destroy;
begin
  FPersonList.Free ;
  inherited;
end;

function TCompany.GetCaption: string;
begin
  result := CompanyName ;
end;

//function TCompany.GetEmployees: TList;
//begin
//  result := People.List ;
//end;

function TCompany.GetOwner: TCompanies;
begin
  result := TCompanies( inherited GetOwner );
end;

function TCompany.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  inherited IsValid( pErrors ) ;
  if ( CompanyName = '' ) then
    pErrors.AddError('CompanyName', cErrorCompanyNameNotAssigned);
  result := pErrors.Count = 0 ;
end;

procedure TCompany.SetOwner(const Value: TCompanies);
begin
  inherited SetOwner( Value ) ;
end;

{ TAdrsAbs }

procedure TAdrsAbs.AssignClassProps(pSource: TtiObject);
begin
  // Copy pointers...
  Self.AdrsType := TAdrsAbs( pSource ).AdrsType ;
end;

constructor TAdrsAbs.Create;
begin
  inherited;
  SelfIterate := false ;
end;

function TAdrsAbs.GetAdrsTypeOID: String ;
begin
  if FAdrsType <> nil then
    result := FAdrsType.OID.AsString 
  else
    result := OID.NullOIDAsString ;
end;

function TAdrsAbs.GeTLookupListItemAsString: string;
begin
  if FAdrsType <> nil then
    result := FAdrsType.Text
  else
    result := 'Unknown' ;
end;

// This is a hack to get the auto mapping working
function TPeople.GetOID: TOID;
begin
  if Owner <> nil then
    result := Owner.OID
  else
    result := inherited GetOID ;

end;

procedure TPeople.SetItems(i: integer; const Value: TPerson);
begin
  inherited SetItems( i, Value ) ;
end;

function TPerson.GetOwner: TPeople;
begin
  result := TPeople( inherited GetOwner );
end;

function TPerson.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  inherited IsValid( pErrors ) ;
  if ( FirstName = '' ) and ( LastName = '' ) then
    pErrors.AddError('', cErrorPersonNameNotAssigned);
  result := pErrors.Count = 0 ;
end;

procedure TPerson.SetOwner(const Value: TPeople);
begin
  inherited SetOwner( Value ) ;
end;

procedure TAdrsAbs.SetAdrsTypeOID(const pOIDAsString : string );
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
  if pOIDAsString <> OID.NullOIDAsString then
    FAdrsType := gAdrsBook.AdrsTypes.Find( pOIDAsString )
  else
    FAdrsType := nil ;

// This code implements (c)
//var
//  lAdrsBook : TtiObject ;
//begin
//  lAdrsBook := TopOfHierarchy ;
//  if ( lAdrsBook is TAdrsBook ) and ( lAdrsBook <> nil ) then
//    FAdrsType := TLookupListItem( TAdrsBook( lAdrsBook ).AdrsTypes.Find( Value ))
//  else
//    FAdrsType := nil ;
end;

procedure TAdrsBook.Read(const pDBConnectionName : string ; pPerLayerName : string = '');
begin
  gTIOPFManager.Read(   AdrsTypes, pDBConnectionName, pPerLayerName ) ;
  gTIOPFManager.ReadPK( Self, pDBConnectionName, pPerLayerName ) ;
  gTIOPFManager.Read(   Self, pDBConnectionName, pPerLayerName ) ;
end;

procedure TAdrsBook.SetDeleted(const Value: boolean);
var
  lObjectState : TPerObjectState ;
begin
  lObjectState := ObjectState ;
  FPeople.Deleted := Value ;
  FCompanies.Deleted := Value ;
  ObjectState := lObjectState ;
end;

initialization

finalization
  FreeAndNilAdrsBook ;
  
end.








