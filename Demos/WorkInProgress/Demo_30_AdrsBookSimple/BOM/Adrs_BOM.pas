unit Adrs_BOM;

{$I tiDefines.inc}

interface
uses
   tiObject
  ,Classes
  ,tiOID
;

const
  cErrorPersonNameNotAssigned = 'Please enter the person''s name';
  cErrorCompanyNameNotAssigned = 'Please enter the companies name';

type

  TAdrsBook       = class;
  TPersonList     = class;
  TPerson         = class;
  TEAdrsList      = class;
  TEAdrs          = class;

  TAdrsBook = class(TtiObject)
  private
    FPeople   : TPersonList  ;
  protected
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Read; override;
    procedure   Save; override;
  published
    property    People   : TPersonList read FPeople;
  end;

  TPersonList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TPerson; reintroduce;
    procedure   SetItems(i: integer; const AValue: TPerson); reintroduce;
    function    GetOwner: TAdrsBook; reintroduce;
    procedure   SetOwner(const AValue: TAdrsBook); reintroduce;
  public
    property    Items[i:integer]: TPerson read GetItems write SetItems;
    procedure   Add(AObject: TPerson); reintroduce;
    property    Owner: TAdrsBook read GetOwner write SetOwner;
  published
  end;

  TPerson = class(TtiObject)
  private
    FLastName: string;
    FNotes: string;
    FInitials: string;
    FTitle: string;
    FFirstName: string;
    FEAdrsList: TEAdrsList;
  protected
    function    GetOwner: TPersonList; reintroduce;
    procedure   SetOwner(const AValue: TPersonList); reintroduce;
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Read; override;
    procedure   Save; override;
    property    Owner: TPersonList read GetOwner write SetOwner;
    function    Clone: TtiObject; override; // ToDo: Remove clone from template
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
  published
    property    FirstName: string read FFirstName write FFirstName;
    property    LastName: string read FLastName write FLastName;
    property    Title: string read FTitle write FTitle;
    property    Initials: string read FInitials write FInitials;
    property    Notes: string read FNotes write FNotes;

    property    EAdrsList: TEAdrsList read FEAdrsList;

  end;

  TEAdrsList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TEAdrs; reintroduce;
    procedure   SetItems(i: integer; const AValue: TEAdrs); reintroduce;
    function    GetOwner: TPerson; reintroduce;
    procedure   SetOwner(const AValue: TPerson); reintroduce;
    function    GetOID: TOID; override;
  public
    property    Items[i:integer]: TEAdrs read GetItems write SetItems;
    procedure   Add(AObject: TEAdrs); reintroduce;
    property    Owner: TPerson read GetOwner write SetOwner;
  published
  end;

  TEAdrs = class(TtiObject)
  private
    FAdrsType: string;
    FAdrsText: string;
  protected
    function    GetOwner: TEAdrsList; reintroduce;
    procedure   SetOwner(const AValue: TEAdrsList); reintroduce;
  public
    property    Owner: TEAdrsList read GetOwner write SetOwner;
    function    Clone: TtiObject; override;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
    procedure   Save; override;
  published
    property    AdrsType: string read FAdrsType write FAdrsType;
    property    AdrsText: string read FAdrsText write FAdrsText;
  end;

implementation
uses
   tiOPFManager
  ,tiConstants
  ,Adrs_Svr // To force linking
  ,Adrs_Constants
;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TAdrsBook
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TAdrsBook.Create;
begin
  inherited;
  FPeople:= TPersonList.Create;
  FPeople.Owner:= Self;
end;

//------------------------------------------------------------------------------
destructor TAdrsBook.Destroy;
begin
  FPeople.Free;
  inherited;
end;

procedure TAdrsBook.Read;
begin
  gTIOPFManager.VisitorManager.Execute(cVisAdrsReadPK, Self);
end;

procedure TAdrsBook.Save;
begin
  gTIOPFManager.VisitorManager.Execute(cVisAdrsSave, Self);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TEAddress
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

{ TPeople }

procedure TPersonList.Add(AObject: TPerson);
begin
  inherited Add(AObject);
end;

function TPersonList.GetItems(i: integer): TPerson;
begin
  result:= inherited GetItems(i) as TPerson;
end;

function TPersonList.GetOwner: TAdrsBook;
begin
  result:= inherited GetOwner as TAdrsBook;
end;

procedure TPersonList.SetItems(i: integer; const AValue: TPerson);
begin
  inherited SetItems(i, AValue);
end;

procedure TPersonList.SetOwner(const AValue: TAdrsBook);
begin
  inherited SetOwner(AValue);
end;

{ TPerson }

procedure TPerson.AssignClassProps(ASource: TtiObject);
begin
  Assert(ASource.TestValid(TPerson), cTIInvalidObjectError);
  EAdrsList.Assign((ASource as TPerson).EAdrsList);
end;

function TPerson.Clone: TtiObject;
begin
  result:= inherited Clone;
end;

constructor TPerson.Create;
begin
  inherited;
  FEAdrsList:= TEAdrsList.Create;
  FEAdrsList.Owner:= Self;
end;

destructor TPerson.Destroy;
begin
  FEAdrsList.Free;
  inherited;
end;

function TPerson.GetOwner: TPersonList;
begin
  result:= inherited GetOwner as TPersonList;
end;

function TPerson.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  result:= true;
end;

procedure TPerson.Read;
begin
  gTIOPFManager.VisitorManager.Execute(cVisAdrsRead, Self);
end;

procedure TPerson.Save;
begin
  gTIOPFManager.VisitorManager.Execute(cVisAdrsSave, Self);
end;

procedure TPerson.SetOwner(const AValue: TPersonList);
begin
  inherited SetOwner(AValue);
end;

{ TEAdrsList }

procedure TEAdrsList.Add(AObject: TEAdrs);
begin
  inherited Add(AObject);
end;

function TEAdrsList.GetItems(i: integer): TEAdrs;
begin
  result:= inherited GetItems(i) as TEAdrs;
end;

function TEAdrsList.GetOID: TOID;
begin
  Assert(Owner.TestValid, cTIInvalidObjectError);
  result:= Owner.OID;  
end;

function TEAdrsList.GetOwner: TPerson;
begin
  result:= inherited GetOwner as TPerson;
end;

procedure TEAdrsList.SetItems(i: integer; const AValue: TEAdrs);
begin
  inherited SetItems(i, AValue);
end;

procedure TEAdrsList.SetOwner(const AValue: TPerson);
begin
  inherited SetOwner(AValue);
end;

{ TEAdrs }

function TEAdrs.Clone: TtiObject;
begin
  result:= inherited Clone;
end;

function TEAdrs.GetOwner: TEAdrsList;
begin
  result:= inherited GetOwner as TEAdrsList;
end;

function TEAdrs.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  result:= AErrors.Count = 0;
end;

procedure TEAdrs.Save;
begin
  gTIOPFManager.VisitorManager.Execute(cVisAdrsSave, Self);
end;

procedure TEAdrs.SetOwner(const AValue: TEAdrsList);
begin
  inherited SetOwner(AValue);
end;

end.












