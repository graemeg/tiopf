unit tiAutoMap;

{$I tiDefines.inc}

interface

uses
  tiObject
  ,tiVisitor
  ,Classes
  ,tiVisitorDB
  ,tiQuery
  ,tiCriteria
  ;

const
  CErrorInconsistentTableNames      = 'Inconsistent table names found in DBColMap';
  CErrorQueryReturnedMoreThanOneRow = 'Query returned "%d" rows when 1 was expected';

type

  TtiClassDBMappingMgr = class;
  TtiClassMaps         = class;
  TtiDBMaps            = class;
  TtiAttrColMaps       = class;

  TtiClassMap   = class;
  TtiAttrMap    = class;
  TtiDBMap      = class;
  TtiDBTableMap = class;
  TtiDBColMap   = class;
  TtiAttrColMap = class;

  TtiClassDBCollections = class;
  TtiClassDBCollection  = class;

  //  TtiForeignKeyMaps          = class;
  //  TtiForeignKeyMap           = class;

  { Enumerated types used in mapping relationships. }
  TtiClassDBMapRelationshipType = (
      { Primary Key as defined in the DB table structure }
      pktDB,
      { Foreign Key as defined in the DB table structure }
      pktFK,
      { For human readable primary key info. If you want to load just enough
        information to populate a lookup list or some other list. A kind
        of 'light weight' object. }
      pktReadable
      );
  TPKInfo = set of TtiClassDBMapRelationshipType;


  //* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  //*
  //* TtiAutoMap Metadata
  //*
  //* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  TtiClassDBMappingMgr = class(TtiObject)
  private
    FAttrColMaps: TtiAttrColMaps;
    FClassMaps:   TtiClassMaps;
    FDBMaps:      TtiDBMaps;
    FCollections: TtiClassDBCollections;
    //    FForeignKeyMaps : TtiForeignKeyMaps;
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure RegisterMapping(const ADatabaseName: string; const AClass: TtiClass; const ATableName: string;
      const AAttrName: string; const AColName: string; const APKInfo: TPKInfo = []); overload;
    procedure RegisterMapping(const AClass: TtiClass; const ATableName: string; const AAttrName: string;
      const AColName: string; const APKInfo: TPKInfo = []);
      overload;
    procedure RegisterCollection(const ACollectionClass: TPerObjListClass; const ACollectionOfClass: TtiClass); overload;
    procedure RegisterInheritance(const AParentClass: TtiClass; const AChildClass: TtiClass);
  published
    property ClassMaps: TtiClassMaps read FClassMaps {write FClassMaps};
    property DBMaps: TtiDBMaps read FDBMaps    {write FDBMaps};
    property AttrColMaps: TtiAttrColMaps read FAttrColMaps {write FAttrColMaps};
    property Collections: TtiClassDBCollections read FCollections;
  end;


  TtiClassMaps = class(TtiObjectList)
  private
  protected
    function GetItems(i: integer): TtiClassMap; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiClassMap); reintroduce;
    function GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
    function FindByPerObjAbsClass(const AClass: TtiClass): TtiClassMap;
  public
    property Items[i: integer]: TtiClassMap read GetItems write SetItems;
    procedure Add(AObject: TtiClassMap); reintroduce;
    property Owner: TtiClassDBMappingMgr read GetOwner write SetOwner;
    function AddClassMap(const AClass: TtiClass): TtiClassMap;
    function FindCreate(const AClass: TtiClass): TtiClassMap;
    function IsClassReg(const AClass: TtiClass): boolean;
    procedure RegisterInheritance(const AParentClass: TtiClass; const AChildClass: TtiClass);
    function FindParent(const AClass: TtiClass): TtiClassMap;
    function HasParent(const AClass: TtiClass): boolean;
    procedure FindAllParents(const AClass: TtiClass; const AList: TtiClassMaps);
  end;


  TtiClassMap = class(TtiObjectList)
  private
    FPerObjAbsClass: TtiClass;
    FParentClassMap: TtiClassMap;
    function FindByAttrName(const AAttrName: string): TtiAttrMap;
    function _IsPublishedProp(const AAttrName: string): boolean;
    function _IsPublicProperty(const AAttrName: string): boolean;
  protected
    function GetCaption: string; override;
    function GetItems(i: integer): TtiAttrMap; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiAttrMap); reintroduce;
    function GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property Items[i: integer]: TtiAttrMap read GetItems write SetItems;
    procedure Add(AObject: TtiAttrMap); reintroduce;
    property Owner: TtiClassDBMappingMgr read GetOwner write SetOwner;
    property PerObjAbsClass: TtiClass read FPerObjAbsClass write FPerObjAbsClass;
    function AddAttrMap(const AAttrName: string): TtiAttrMap;
    property ParentClassMap: TtiClassMap read FParentClassMap write FParentClassMap;
    //    function    FindCreate(const AAttrName : string): TtiAttrMap;
  end;


  TtiAttrMap = class(TtiObject)
  private
    FAttrName: string;
  protected
    function GetCaption: string; override;
    function GetOwner: TtiClassMap; reintroduce;
    procedure SetOwner(const AValue: TtiClassMap); reintroduce;
  public
    property Owner: TtiClassMap read GetOwner write SetOwner;
  published
    property AttrName: string read FAttrName write FAttrName;
  end;


  TtiDBMaps = class(TtiObjectList)
  private
    function FindByDatabaseName(const ADatabaseName: string): TtiDBMap;
  protected
    function GetItems(i: integer): TtiDBMap; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiDBMap); reintroduce;
    function GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property Items[i: integer]: TtiDBMap read GetItems write SetItems;
    procedure Add(AObject: TtiDBMap); reintroduce;
    property Owner: TtiClassDBMappingMgr read GetOwner write SetOwner;
    function AddDBMap(const ADatabaseName: string): TtiDBMap;
    function FindCreate(const ADatabaseName: string): TtiDBMap;
  published
  end;


  TtiDBMap = class(TtiObjectList)
  private
    FDatabaseName: string;
    function FindByTableName(const ATableName: string): TtiDBTableMap;
  protected
    function GetItems(i: integer): TtiDBTableMap; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiDBTableMap); reintroduce;
    function GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property Items[i: integer]: TtiDBTableMap read GetItems write SetItems;
    procedure Add(AObject: TtiDBTableMap); reintroduce;
    property Owner: TtiClassDBMappingMgr read GetOwner write SetOwner;
    function AddTableMap(const ATableName: string): TtiDBTableMap;
    function FindCreate(const ATableName: string): TtiDBTableMap;
  published
    property DatabaseName: string read FDatabaseName write FDatabaseName;
  end;


  TtiDBTableMap = class(TtiObjectList)
  private
    FTableName: string;
    function FindByColName(const AColName: string): TtiDBColMap;
  protected
    function GetCaption: string; override;
    function GetItems(i: integer): TtiDBColMap; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiDBColMap); reintroduce;
    function GetOwner: TtiDBMap; reintroduce;
    procedure SetOwner(const AValue: TtiDBMap); reintroduce;
  public
    property Items[i: integer]: TtiDBColMap read GetItems write SetItems;
    procedure Add(AObject: TtiDBColMap); reintroduce;
    property Owner: TtiDBMap read GetOwner write SetOwner;
    function AddColMap(const AColName: string; APKInfo: TPKInfo): TtiDBColMap;
  published
    property TableName: string read FTableName write FTableName;
  end;


  TtiDBColMap = class(TtiObject)
  private
    FColName: string;
    FPKInfo:  TPKInfo;
  protected
    function GetCaption: string; override;
    function GetOwner: TtiDBTableMap; reintroduce;
    procedure SetOwner(const AValue: TtiDBTableMap); reintroduce;
  public
    property Owner: TtiDBTableMap read GetOwner write SetOwner;
  published
    property ColName: string read FColName write FColName;
    property PKInfo: TPKInfo read FPKInfo write FPKInfo;
  end;


  TtiAttrColMaps = class(TtiObjectList)
  private
  protected
    function GetItems(i: integer): TtiAttrColMap; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiAttrColMap); reintroduce;
    function GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property Items[i: integer]: TtiAttrColMap read GetItems write SetItems;
    procedure Add(AObject: TtiAttrColMap); reintroduce;
    property Owner: TtiClassDBMappingMgr read GetOwner write SetOwner;
    procedure AddMapping(const pAttrMap: TtiAttrMap; const pColMap: TtiDBColMap);
    procedure FindAllMappingsByMapToClass(const AClass: TtiClass; const AList: TtiAttrColMaps);
    procedure FindAllPKMappingsByMapToClass(const AClass: TtiClass; const AList: TtiAttrColMaps);
    function FindByClassAttrMap(const AClass: TtiClass; const AAttrName: string): TtiAttrColMap;
    function TableName: string;
  published
  end;


  TtiAttrColMap = class(TtiObject)
  private
    FAttrMap:  TtiAttrMap;
    FDBColMap: TtiDBColMap;
  protected
    function GetCaption: string; override;
    function GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property Owner: TtiClassDBMappingMgr read GetOwner write SetOwner;
    property AttrMap: TtiAttrMap read FAttrMap write FAttrMap;
    property DBColMap: TtiDBColMap read FDBColMap write FDBColMap;
  end;


  TtiClassDBCollections = class(TtiObjectList)
  protected
    function GetItems(i: integer): TtiClassDBCollection; reintroduce;
    procedure SetItems(i: integer; const AValue: TtiClassDBCollection); reintroduce;
    function GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property Items[i: integer]: TtiClassDBCollection read GetItems write SetItems;
    procedure Add(AObject: TtiClassDBCollection); reintroduce;
    property Owner: TtiClassDBMappingMgr read GetOwner write SetOwner;
    function AddClassCollectionMapping(const ACollectionClass: TPerObjListClass; const AClass: TtiClass): TtiClassDBCollection;
      overload;
    function FindByCollectionOf(const AClass: TtiClass): TtiClassDBCollection;
    procedure FindByCollection(const AClass: TtiClass; const AList: TList);
    function IsCollection(const AClass: TtiClass): boolean;
    function IsInCollection(const AClass: TtiClass): boolean;
  published
  end;


  TtiClassDBCollection = class(TtiObject)
  private
    FPerObjAbsClass:  TtiClass;
    FCollectionClass: TPerObjListClass;
    //    FForeignKeyCols : TStringList;
    FOwnerAttrMaps:   TtiAttrColMaps;
  protected
    function GetOwner: TtiClassDBCollections; reintroduce;
    procedure SetOwner(const AValue: TtiClassDBCollections); reintroduce;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Owner: TtiClassDBCollections read GetOwner write SetOwner;
    property CollectionClass: TPerObjListClass read FCollectionClass write FCollectionClass;
    property PerObjAbsClass: TtiClass read FPerObjAbsClass write FPerObjAbsClass;
    property OwnerAttrMaps: TtiAttrColMaps read FOwnerAttrMaps;
  end;


  //  TtiForeignKeyMaps = class(TtiObjectList)
  //  private
  //  protected
  //    function    GetItems(i: integer): TtiForeignKeyMap; reintroduce;
  //    procedure   SetItems(i: integer; const AValue: TtiForeignKeyMap); reintroduce;
  //    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
  //    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  //  public
  //    property    Items[i:integer]: TtiForeignKeyMap read GetItems write SetItems;
  //    procedure   Add(AObject : TtiForeignKeyMap  ; ADefaultDispOrder : boolean = true); reintroduce;
  //    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner;
  //    procedure   AddForeignKeyMap(const ACollectionOfClass : TPerObjListClass;
  //                                  const pFKProperty : string;
  //                                  const pFKField : string);

  ////    procedure   AddMapping(const pAttrMap : TtiAttrMap; const pColMap : TtiDBColMap);
  ////    procedure   FindAllMappingsByMapToClass(const AClass : TtiClass;
  ////                                             const AList : TtiAttrColMaps);
  ////    procedure   FindAllPKMappingsByMapToClass(const AClass : TtiClass;
  ////                                               const AList : TtiAttrColMaps);
  ////    function    FindByClassAttrMap(const AClass : TtiClass;
  ////                                    const AAttrName : string): TtiAttrColMap;
  ////    function    TableName : string;
  //  published
  //  end;

  //  TtiForeignKeyMap = class(TtiObject)
  //  private
  ////    FAttrMap: TtiAttrMap;
  ////    FDBColMap: TtiDBColMap;
  //  protected
  //    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
  //    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  //  public
  //    property    Owner : TtiClassDBMappingMgr read GetOwner write SetOwner;
  ////    property    AttrMap : TtiAttrMap read FAttrMap write FAttrMap;
  ////    property    DBColMap : TtiDBColMap read FDBColMap write FDBColMap;
  //  end;

  //* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  //*
  //* TtiAutoMap Visitors
  //*
  //* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

  TVisProAttributeToFieldName = class(TtiVisitor)
  private
    FAttrColMaps: TtiAttrColMaps;
    FClassType:   TtiClass;
  protected
    function AcceptVisitor: boolean; override;
  public
    constructor Create(AttrColMaps: TtiAttrColMaps; AClassType: TtiClass); reintroduce; virtual;
    procedure Execute(const pVisited: TtiVisited); override;
  end;

  TVisAutoAbs = class(TtiObjectVisitor)
  protected
    FWhereAttrColMaps: TtiAttrColMaps;
    FAttrColMaps: TtiAttrColMaps;
    FWhere:  TtiQueryParams;
    FParams: TtiQueryParams;
    FVisitedClassType: TtiClass;
    procedure AddToParams(const AParams: TtiQueryParams; const pAttrColMaps: TtiAttrColMaps; const AData: TtiObject); virtual;
    procedure QueryResultToObject(const ATarget: TtiObject; const pAttrColMaps: TtiAttrColMaps);
  protected
    procedure GetWhereAttrColMaps; virtual; abstract;
    procedure GetAttrColMaps; virtual; abstract;
    procedure SetupParams; override;
    function ParamsToString(const AParams: TtiQueryParams): string;
  public
    procedure Execute(const AData: TtiVisited); override;
    constructor Create; override;
    destructor Destroy; override;
  end;

  TVisAutoReadThis = class(TVisAutoAbs)
  protected
    FSetObjectState: boolean;
    procedure GetWhereAttrColMaps; override;
    procedure GetAttrColMaps; override;
    function AcceptVisitor: boolean; override;
    procedure MapRowToObject;
    procedure DoExecute;
    procedure Final(const AVisited: TtiObject); override;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;

  TVisAutoCollectionRead = class(TVisAutoAbs)
  private
    FClassDBCollection: TtiClassDBCollection;
    FClassToCreate:     TtiClass;
    FHasParent:         boolean;
    FClassesWithParent: TList;
    FCriteria:          TtiCriteria;
    procedure ReadDataForParentClass(ACollection: TtiClassDBCollection);
    procedure ReadDataForChildClasses(ACollection: TtiClassDBCollection);
    procedure SetUpCriteria;
  protected
    FSetObjectState: boolean;
    procedure GetWhereAttrColMaps; override;
    procedure GetAttrColMaps; override;
    function AcceptVisitor: boolean; override;
    procedure MapRowToObject(ACheckForDuplicates: boolean);
    procedure SetContinueVisiting; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(const AData: TtiVisited); override;
  end;

  TVisAutoCollectionPKRead = class(TVisAutoCollectionRead)
  protected
    procedure GetAttrColMaps; override;
    procedure SetContinueVisiting; override;
  end;

  TVisAutoUpdateAbs = class(TVisAutoAbs)
  protected
    procedure GetWhereAttrColMaps; override;
    procedure GetAttrColMaps; override;
    procedure DoExecuteQuery; virtual; abstract;
  public
    procedure Execute(const AData: TtiVisited); override;
  end;

  TVisAutoDelete = class(TVisAutoUpdateAbs)
  protected
    procedure GetAttrColMaps; override;
    function AcceptVisitor: boolean; override;
    procedure DoExecuteQuery; override;
  public
    constructor Create; override;
  end;

  TVisAutoUpdate = class(TVisAutoUpdateAbs)
  protected
    function AcceptVisitor: boolean; override;
    procedure DoExecuteQuery; override;
  end;

  TVisAutoCreate = class(TVisAutoUpdateAbs)
  protected
    procedure GetWhereAttrColMaps; override;
    procedure GetAttrColMaps; override;
    function AcceptVisitor: boolean; override;
    procedure DoExecuteQuery; override;
  end;

implementation

uses
   tiUtils
  ,tiOPFManager
  ,tiExcept
  ,tiOID
  ,tiRTTI
//##  ,tiFilteredObjectList
  ,TypInfo
  ,SysUtils
  ;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TtiClassDBMappingMgr
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
{
function TtiClassDBMappingMgr.AddAttrColMap(const pAttrMap: TtiAttrMap;
  const pDBColMap: TtiDBColMap): TtiAttrColMap;
begin
  result := TtiAttrColMap.Create;
  result.DBColMap := pDBColMap;
  result.AttrMap := pAttrMap;
  FAttrColMaps.Add(result);
end;
}

{
function TtiClassDBMappingMgr.AddClassMap(
  const AClass: TtiClass): TtiClassMap;
begin
  result := TtiClassMap.Create;
  result.PerObjAbsClass := AClass;
  FClassMaps.Add(result);
end;
}

{
function TtiClassDBMappingMgr.AddDBMap(const ADatabaseName : string): TtiDBMap;
begin
  result := TtiDBMap.Create;
  result.DatabaseName := ADatabaseName;
  FDBMaps.Add(result);
end;
}

procedure TtiClassDBMappingMgr.RegisterMapping(const ADatabaseName: string; const AClass: TtiClass;
  const ATableName: string; const AAttrName: string; const AColName: string; const APKInfo: TPKInfo = []);
var
  lDBMap:      TtiDBMap;
  lDBTableMap: TtiDBTableMap;
  lDBColMap:   TtiDBColMap;
  lClassMap:   TtiClassMap;
  lAttrMap:    TtiAttrMap;
begin
  lDBMap      := FDBMaps.FindCreate(ADatabaseName);
  lDBTableMap := lDBMap.FindCreate(ATableName);
  lDBColMap   := lDBTableMap.AddColMap(AColName, APKInfo);

  lClassMap := FClassMaps.FindCreate(AClass);
  lAttrMap  := lClassMap.AddAttrMap(AAttrName);
  lAttrMap.ObjectState := posClean;
  FAttrColMaps.AddMapping(lAttrMap, lDBColMap);
end;

procedure TtiClassDBMappingMgr.RegisterMapping(const AClass: TtiClass; const ATableName: string;
  const AAttrName: string; const AColName: string; const APKInfo: TPKInfo = []);
begin
       // ToDo: This will load before a persistence layer is loaded, so the call to
       //       DefautlDBConnectionName is invalid.
  RegisterMapping(
    '',{TtiOPFManager(Owner).DefaultDBConnectionName,}
    AClass,
    ATableName,
    AAttrName,
    AColName,
    APKInfo);
end;

constructor TtiClassDBMappingMgr.Create;
begin
  inherited;
  FAttrColMaps           := TtiAttrColMaps.Create;
  FAttrColMaps.Owner     := self;
  // ToDo: Refactor to remove need for ItemOwner. Use Parent instead
  FAttrColMaps.ItemOwner := self;

  FClassMaps           := TtiClassMaps.Create;
  FClassMaps.Owner     := self;
  // ToDo: Refactor to remove need for ItemOwner. Use Parent instead
  FClassMaps.ItemOwner := self;

  FDBMaps           := TtiDBMaps.Create;
  FDBMaps.Owner     := self;
  // ToDo: Refactor to remove need for ItemOwner. Use Parent instead
  FDBMaps.ItemOwner := self;

  FCollections           := TtiClassDBCollections.Create;
  FCollections.Owner     := self;
  // ToDo: Refactor to remove need for ItemOwner. Use Parent instead
  FCollections.ItemOwner := self;

end;

destructor TtiClassDBMappingMgr.Destroy;
begin
  FAttrColMaps.Free;
  FClassMaps.Free;
  FDBMaps.Free;
  FCollections.Free;
  inherited;
end;

{ TtiClassMaps }

procedure TtiClassMaps.Add(AObject: TtiClassMap);
begin
  inherited Add(AObject);
end;

function TtiClassMaps.AddClassMap(const AClass: TtiClass): TtiClassMap;
begin
  if FindByPerObjAbsClass(AClass) <> NIL then
    raise Exception.Create(
      'Attempt to register duplicate TtiClassMap' + Cr +
      'Classname: ' + AClass.ClassName + Cr +
      'Called in ' + ClassName + '.AddClassMap');
  //    tiTermError('Attempt to register duplicate TtiClassMap' + Cr +
  //                 'Classname: ' + AClass.ClassName + Cr +
  //                 'Called in ' + ClassName + '.AddClassMap');

  Result := TtiClassMap.Create;
  Result.PerObjAbsClass := AClass;
  Result.ObjectState := posClean;
  Add(Result);
end;

procedure TtiClassMaps.FindAllParents(const AClass: TtiClass; const AList: TtiClassMaps);
var
  lClassMap: TtiClassMap;
begin
  Assert(not AList.OwnsObjects, 'AList.OwnsObjects is true and it should be false');
  AList.Clear;
  lClassMap := FindByPerObjAbsClass(AClass);
  Assert(lClassMap <> NIL,
    'Request to find parent on class that is not registered <' +
    AClass.ClassName + '>');
  AList.Insert(0, lClassMap);
  while (lClassMap <> NIL) and
    (lClassMap.ParentClassMap <> NIL) do
  begin
    lClassMap := lClassMap.ParentClassMap;
    if lClassMap <> NIL then
      AList.Insert(0, lClassMap);
  end;
end;

function TtiClassMaps.FindByPerObjAbsClass(const AClass: TtiClass): TtiClassMap;
var
  i: integer;
begin
  Result := NIL;
  for i := 0 to Count - 1 do
    if Items[i].PerObjAbsClass = AClass then
    begin
      Result := Items[i];
      Exit; //==>
    end;
end;

function TtiClassMaps.FindCreate(const AClass: TtiClass): TtiClassMap;
begin
  Result := FindByPerObjAbsClass(AClass);
  if Result = NIL then
    Result := AddClassMap(AClass);
end;

function TtiClassMaps.FindParent(const AClass: TtiClass): TtiClassMap;
var
  lClassMap: TtiClassMap;
begin
  lClassMap := FindByPerObjAbsClass(AClass);
  Assert(lClassMap <> NIL,
    'Attempt to find parent on un-registered class <' +
    AClass.ClassName + '>');
  Result    := lClassMap.ParentClassMap;
end;

function TtiClassMaps.GetItems(i: integer): TtiClassMap;
begin
  Result := TtiClassMap(inherited GetItems(i));
end;

function TtiClassMaps.GetOwner: TtiClassDBMappingMgr;
begin
  Result := TtiClassDBMappingMgr(inherited GetOwner);
end;

function TtiClassMaps.HasParent(const AClass: TtiClass): boolean;
begin
  Result := (FindParent(AClass) <> NIL);
end;

function TtiClassMaps.IsClassReg(const AClass: TtiClass): boolean;
begin
  Result := FindByPerObjAbsClass(AClass) <> NIL;
end;

procedure TtiClassMaps.RegisterInheritance(const AParentClass: TtiClass; const AChildClass: TtiClass);
var
  lClassMapChild:  TtiClassMap;
  lClassMapParent: TtiClassMap;
begin

  if not AChildClass.InheritsFrom(AParentClass) then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a child class <' +
      AChildClass.ClassName +
      '> that does not inherit from <' +
      AParentClass.ClassName + '>');

  lClassMapChild := FindByPerObjAbsClass(AChildClass);
  if lClassMapChild = NIL then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a child class <' +
      AChildClass.ClassName +
      '> that has not yet been registered');

  lClassMapParent := FindByPerObjAbsClass(AParentClass);
  if lClassMapParent = NIL then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a parent class <' +
      AParentClass.ClassName +
      '> that has not yet been registered');

  lClassMapChild.ParentClassMap := lClassMapParent;
end;

procedure TtiClassMaps.SetItems(i: integer; const AValue: TtiClassMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiClassMaps.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

{ TtiClassMap }

procedure TtiClassMap.Add(AObject: TtiAttrMap);
begin
  inherited Add(AObject);
end;

function TtiClassMap.AddAttrMap(const AAttrName: string): TtiAttrMap;
begin
  if FindByAttrName(AAttrName) <> NIL then
    raise Exception.Create(
      'Attempt to register duplicate TtiAttrMap' + Cr +
      'ClassName: ' + PerObjAbsClass.ClassName + Cr +
      'AttrName:  ' + AAttrName + Cr +
      '. Called in ' + ClassName + '.AddAttrMap');
  if not _IsPublishedProp(AAttrName) then
    raise Exception.Create(
      AAttrName + ' is not a published property on ' +
      PerObjAbsClass.ClassName + Cr +
      '. Called in ' + ClassName + '.AddAttrMap');
  Result          := TtiAttrMap.Create;
  Result.AttrName := AAttrName;
  Result.ObjectState := posClean;
  Add(Result);
end;

function TtiClassMap._IsPublishedProp(const AAttrName: string): boolean;
begin
  Result :=
    _IsPublicProperty(AAttrName) or
    (IsPublishedProp(PerObjAbsClass, AAttrName));
end;

function TtiClassMap._IsPublicProperty(const AAttrName: string): boolean;
begin
  Result :=
    SameText(AAttrName, 'OID') or
    SameText(AAttrName, 'Owner.OID'); // Must do better than this
end;

function TtiClassMap.FindByAttrName(const AAttrName: string): TtiAttrMap;
var
  i: integer;
begin
  Result := NIL;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Attrname, AAttrName) then
    begin
      Result := Items[i];
      Break; //==>
    end;
end;

function TtiClassMap.GetCaption: string;
begin
  Result := FPerObjAbsClass.ClassName;
end;

function TtiClassMap.GetItems(i: integer): TtiAttrMap;
begin
  Result := TtiAttrMap(inherited GetItems(i));
end;

function TtiClassMap.GetOwner: TtiClassDBMappingMgr;
begin
  Result := TtiClassDBMappingMgr(inherited GetOwner);
end;

procedure TtiClassMap.SetItems(i: integer; const AValue: TtiAttrMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiClassMap.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

{ TtiDBMaps }

procedure TtiDBMaps.Add(AObject: TtiDBMap);
begin
  inherited Add(AObject);
end;

function TtiDBMaps.FindCreate(const ADatabaseName: string): TtiDBMap;
begin
  Result := FindByDatabaseName(ADatabaseName);
  if Result = NIL then
    Result := AddDBMap(ADatabaseName);
end;

function TtiDBMaps.FindByDatabaseName(const ADatabaseName: string): TtiDBMap;
var
  i: integer;
begin
  Result := NIL;
  for i := 0 to Count - 1 do
    if SameText(Items[i].DatabaseName, ADatabaseName) then
    begin
      Result := Items[i];
      Break; //==>
    end;
end;

function TtiDBMaps.GetItems(i: integer): TtiDBMap;
begin
  Result := TtiDBMap(inherited GetItems(i));
end;

function TtiDBMaps.GetOwner: TtiClassDBMappingMgr;
begin
  Result := TtiClassDBMappingMgr(inherited GetOwner);
end;

procedure TtiDBMaps.SetItems(i: integer; const AValue: TtiDBMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiDBMaps.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

{ TtiDBMap }

procedure TtiDBMap.Add(AObject: TtiDBTableMap);
begin
  inherited Add(AObject);
end;

function TtiDBMap.AddTableMap(const ATableName: string): TtiDBTableMap;
begin
  if FindByTableName(ATableName) <> NIL then
    raise Exception.Create(
      'Attempt to register duplicate TtiDBTableMap' + Cr +
      'DatabaseName: ' + DatabaseName + Cr +
      'TableName:    ' + ATableName + Cr +
      'Called in ' + ClassName + '.AddTableMap');
  //    tiTermError('Attempt to register duplicate TtiDBTableMap' + Cr +
  //                 'DatabaseName: ' + DatabaseName + Cr +
  //                 'TableName:    ' + ATableName + Cr +
  //                 'Called in ' + ClassName + '.AddTableMap');
  Result           := TtiDBTableMap.Create;
  Result.TableName := ATableName;
  Result.ObjectState := posClean;
  Add(Result);
end;

function TtiDBMap.FindCreate(const ATableName: string): TtiDBTableMap;
begin
  Result := FindByTableName(ATableName);
  if Result = NIL then
    Result := AddTableMap(ATableName);
end;

function TtiDBMap.FindByTableName(const ATableName: string): TtiDBTableMap;
var
  i: integer;
begin
  Result := NIL;
  for i := 0 to Count - 1 do
    if SameText(Items[i].TableName, ATableName) then
    begin
      Result := Items[i];
      Break; //==>
    end;
end;

function TtiDBMap.GetItems(i: integer): TtiDBTableMap;
begin
  Result := TtiDBTableMap(inherited GetItems(i));
end;

function TtiDBMap.GetOwner: TtiClassDBMappingMgr;
begin
  Result := TtiClassDBMappingMgr(inherited GetOwner);
end;

procedure TtiDBMap.SetItems(i: integer; const AValue: TtiDBTableMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiDBMap.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TtiDBColMap
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiDBColMap.GetCaption: string;
begin
  Result := FColName;
end;

function TtiDBColMap.GetOwner: TtiDBTableMap;
begin
  Result := TtiDBTableMap(inherited GetOwner);
end;

procedure TtiDBColMap.SetOwner(const AValue: TtiDBTableMap);
begin
  inherited SetOwner(AValue);
end;

{ TtiDBTableMap }

procedure TtiDBTableMap.Add(AObject: TtiDBColMap);
begin
  inherited Add(AObject);
end;

function TtiDBTableMap.AddColMap(const AColName: string; APKInfo: TPKInfo): TtiDBColMap;
begin
  Result := FindByColName(AColName);
  if Result <> NIL then
    Exit; //==>                     
          //    raise exception.Create(
          //                 'Attempt to register duplicate TtiDBColMap' + Cr +
          //                 'Table name: ' + TableName + Cr +
          //                 'Col name:   ' + AColName + Cr +
          //                 'Called in ' + ClassName + '.AddColMap');

  Result         := TtiDBColMap.Create;
  Result.ColName := AColName;
  Result.PKInfo  := APKInfo;
  Result.ObjectState := posClean;
  Add(Result);
end;

 //function TtiDBTableMap.FindCreate(const AColName: string; APKInfo : TPKInfo): TtiDBColMap;
 //begin
 //  result := FindByColName(AColName);
 //  if result = nil then
 //    result := AddColMap(AColName, APKInfo);
 //end;

function TtiDBTableMap.FindByColName(const AColName: string): TtiDBColMap;
var
  i: integer;
begin
  Result := NIL;
  for i := 0 to Count - 1 do
    if SameText(Items[i].ColName, AColName) then
    begin
      Result := Items[i];
      Break; //==>
    end;
end;

function TtiDBTableMap.GetCaption: string;
begin
  Result := FTableName;
end;

function TtiDBTableMap.GetItems(i: integer): TtiDBColMap;
begin
  Result := TtiDBColMap(inherited GetItems(i));
end;

function TtiDBTableMap.GetOwner: TtiDBMap;
begin
  Result := TtiDBMap(inherited GetOwner);
end;

procedure TtiDBTableMap.SetItems(i: integer; const AValue: TtiDBColMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiDBTableMap.SetOwner(const AValue: TtiDBMap);
begin
  inherited SetOwner(AValue);
end;

{ TtiAttrColMaps }

procedure TtiAttrColMaps.Add(AObject: TtiAttrColMap);
begin
  inherited Add(AObject);
end;

procedure TtiAttrColMaps.AddMapping(const pAttrMap: TtiAttrMap; const pColMap: TtiDBColMap);
var
  lData: TtiAttrColMap;
begin
  // ToDo: Check we are not adding duplicates in an assert()
  lData          := TtiAttrColMap.Create;
  lData.DBColMap := pColMap;
  lData.AttrMap  := pAttrMap;
  lData.ObjectState := posClean;
  Add(lData);
end;

procedure TtiAttrColMaps.FindAllMappingsByMapToClass(const AClass: TtiClass; const AList: TtiAttrColMaps);
var
  i: integer;
begin
  Assert(not AList.OwnsObjects, 'AList.OwnsObjects is true and it should be false');
  AList.Clear;
  for i := 0 to Count - 1 do
    if Items[i].AttrMap.Owner.PerObjAbsClass = AClass then
      AList.Add(Items[i]);
end;

procedure TtiAttrColMaps.FindAllPKMappingsByMapToClass(const AClass: TtiClass; const AList: TtiAttrColMaps);
var
  i: integer;
begin
  Assert(not AList.OwnsObjects, 'AList.OwnsObjects is true and it should be false');
  AList.Clear;
  for i := 0 to Count - 1 do
    if (Items[i].AttrMap.Owner.PerObjAbsClass = AClass) and
      (pktDB in Items[i].DBColMap.PKINfo) then
      AList.Add(Items[i]);
end;

function TtiAttrColMaps.FindByClassAttrMap(const AClass: TtiClass; const AAttrName: string): TtiAttrColMap;
var
  i: integer;
begin
  Result := NIL;
  for i := 0 to Count - 1 do
    if (Items[i].AttrMap.Owner.PerObjAbsClass = AClass) and
      (SameText(Items[i].AttrMap.AttrName, AAttrName)) then
    begin
      Result := Items[i];
      Break; //==>
    end;
end;

function TtiAttrColMaps.GetItems(i: integer): TtiAttrColMap;
begin
  Result := TtiAttrColMap(inherited GetItems(i));
end;

function TtiAttrColMaps.GetOwner: TtiClassDBMappingMgr;
begin
  Result := TtiClassDBMappingMgr(inherited GetOwner);
end;

procedure TtiAttrColMaps.SetItems(i: integer; const AValue: TtiAttrColMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiAttrColMaps.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

function TtiAttrMap.GetCaption: string;
begin
  Result := AttrName;
end;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TtiAttrMap
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiAttrMap.GetOwner: TtiClassMap;
begin
  Result := TtiClassMap(inherited GetOwner);
end;

procedure TtiAttrMap.SetOwner(const AValue: TtiClassMap);
begin
  inherited SetOwner(AValue);
end;

 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 // *
 // * TtiAttrColMap
 // *
 // * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiAttrColMap.GetCaption: string;
begin
  Result := FAttrMap.Caption + '/' + FDBColMap.Caption;
end;

function TtiAttrColMap.GetOwner: TtiClassDBMappingMgr;
begin
  Result := TtiClassDBMappingMgr(inherited GetOwner);
end;

procedure TtiAttrColMap.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

function TtiDBMaps.AddDBMap(const ADatabaseName: string): TtiDBMap;
begin
  if FindByDatabaseName(ADatabaseName) <> NIL then
    raise Exception.Create(
      'Attempt to register duplicate TtiDBMap' + Cr +
      'DatabaseName: ' + ADatabaseName + Cr +
      'Called in ' + ClassName + 'AddDBMap');
  //    tiTermError('Attempt to register duplicate TtiDBMap' + Cr +
  //                 'DatabaseName: ' + ADatabaseName + Cr +
  //                 'Called in ' + ClassName + 'AddDBMap');
  Result := TtiDBMap.Create;
  Result.DatabaseName := ADatabaseName;
  Result.ObjectState := posClean;
  Add(Result);
end;

procedure TtiClassDBMappingMgr.RegisterCollection(const ACollectionClass: TPerObjListClass; const ACollectionOfClass: TtiClass);
begin
  FCollections.AddClassCollectionMapping(ACollectionClass,
    ACollectionOfClass);
end;

{ TtiClassDBCollections }

procedure TtiClassDBCollections.Add(AObject: TtiClassDBCollection);
begin
  inherited Add(AObject);
end;

function TtiClassDBCollections.AddClassCollectionMapping(const ACollectionClass: TPerObjListClass;
  const AClass: TtiClass): TtiClassDBCollection;
begin
  Result := TtiClassDBCollection.Create;
  Result.CollectionClass := ACollectionClass;
  Result.PerObjAbsClass := AClass;
  Result.ObjectState := posClean;
  Add(Result);
end;

procedure TtiClassDBCollections.FindByCollection(const AClass: TtiClass; const AList: TList);
var
  i: integer;
begin
  AList.Clear;
  for i := 0 to Count - 1 do
    if Items[i].CollectionClass = AClass then
      AList.Add(Items[i]);
end;

function TtiClassDBCollections.FindByCollectionOf(const AClass: TtiClass): TtiClassDBCollection;
var
  i: integer;
begin
  Result := NIL;
  for i := 0 to Count - 1 do
    if Items[i].PerObjAbsClass = AClass then
    begin
      Result := Items[i];
      Exit; //==>
    end;
end;

function TtiClassDBCollections.GetItems(i: integer): TtiClassDBCollection;
begin
  Result := TtiClassDBCollection(inherited GetItems(i));
end;

function TtiClassDBCollections.GetOwner: TtiClassDBMappingMgr;
begin
  Result := TtiClassDBMappingMgr(inherited GetOwner);
end;

function TtiClassDBCollections.IsInCollection(const AClass: TtiClass): boolean;
begin
  Result := FindByCollectionOf(AClass) <> NIL;
end;

function TtiClassDBCollections.IsCollection(const AClass: TtiClass): boolean;
var
  lList: TList;
begin
  lList := TList.Create;
  try
    FindByCollection(AClass, lList);
    Result := lList.Count > 0;
  finally
    lList.Free;
  end;
end;

procedure TtiClassDBCollections.SetItems(i: integer; const AValue: TtiClassDBCollection);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiClassDBCollections.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

{ TtiClassDBCollection }

constructor TtiClassDBCollection.Create;
begin
  inherited;
  //  FForeignKeyCols := TStringList.Create;
  FOwnerAttrMaps := TtiAttrColMaps.Create;
  FOwnerAttrMaps.OwnsObjects := False;
end;

destructor TtiClassDBCollection.Destroy;
begin
  FOwnerAttrMaps.Free;
  //  FForeignKeyCols.Free;
  inherited;
end;

function TtiClassDBCollection.GetOwner: TtiClassDBCollections;
begin
  Result := TtiClassDBCollections(inherited GetOwner);
end;

procedure TtiClassDBCollection.SetOwner(const AValue: TtiClassDBCollections);
begin
  inherited SetOwner(AValue);
end;

function TtiAttrColMaps.TableName: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if Result = '' then
      Result := Items[i].DBColMap.Owner.TableName;
    if Result <> Items[i].DBColMap.Owner.TableName then
      raise EtiOPFProgrammerException.Create(cErrorInconsistentTableNames);
  end;
end;

procedure TtiClassDBMappingMgr.RegisterInheritance(const AParentClass: TtiClass; const AChildClass: TtiClass);
begin
  FClassMaps.RegisterInheritance(AParentClass, AChildClass);
end;

{ TVisProAttributeToFieldName }

function TVisProAttributeToFieldName.AcceptVisitor: boolean;
begin
  Result := Visited is TtiSelectionCriteriaAbs;
end;

constructor TVisProAttributeToFieldName.Create(AttrColMaps: TtiAttrColMaps; AClassType: TtiClass);
begin
  inherited Create;
  FAttrColMaps := AttrColMaps;
  FClassType   := AClassType;
end;

procedure TVisProAttributeToFieldName.Execute(const pVisited: TtiVisited);
var
  lCriteria: TtiSelectionCriteriaAbs;
  lMap:      TtiAttrColMap;
begin
  inherited Execute(pVisited);

  if not AcceptVisitor then
    Exit; //==>

  lCriteria := (Visited as TtiSelectionCriteriaAbs);
  Assert(Assigned(lCriteria), 'Invalid Visited in TVisProAttributeToFieldName.Execute');

  lMap := FAttrColMaps.FindByClassAttrMap(FClassType, lCriteria.Attribute);
  if assigned(lMap) then
    lCriteria.FieldName := lMap.DBColMap.ColName;

end;

procedure TVisAutoAbs.AddToParams(const AParams: TtiQueryParams; const pAttrColMaps: TtiAttrColMaps; const AData: TtiObject);

//-----------
  procedure _SetOIDParam(const AParams: TtiQueryParams; const AData: TtiObject; const AColName: string; const APropName: string);
  begin
    {$IFDEF OID_AS_INT64}
    if Pos('OWNER', UpperCase(APropName)) <> 0 then
    begin
      // If we are calling create (or update?) then we will want to set Owner.OID
      if Self is TVisAutoUpdateAbs then
      begin
        Assert(AData.Owner <> NIL,
          'Attempting to read a collection but the collections''s Visited.Owner is not assigned');
        AParams.SetValueAsInteger(AColName, AData.Owner.OID);
      end
      else
        AParams.SetValueAsInteger(AColName, AData.OID);
    end
    else
      AParams.SetValueAsInteger(AColName, AData.OID);
    {$ELSE}
    if Pos('OWNER', UpperCase(APropName)) <> 0 then
    begin
      // If we are calling create (or update?) then we will want to set Owner.OID
      if Self is TVisAutoUpdateAbs then
      begin
        Assert(AData.Owner <> NIL,
          'Attempting to read a collection but the collections''s Visited.Owner is not assigned');
        AData.Owner.OID.AssignToTIQueryParam(AColName, AParams);
      end
      else
      begin
        if not AData.OID.IsNull then
          AData.OID.AssignToTIQueryParam(AColName, AParams);
      end;
    end
    else
      AData.OID.AssignToTIQueryParam(AColName, AParams);
    {$ENDIF}
  end;

var
  lAttrColMap: TtiAttrColMap;
  i:           integer;
  lColName:    string;
  lPropName:   string;
  {$IFNDEF OID_AS_INT64}
  lOID:        TtiOID;
  {$ENDIF}
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');
  AParams.Clear;
  for i := 0 to pAttrColMaps.Count - 1 do
  begin
    lAttrColMap := pAttrColMaps.Items[i];
    lColName    := lAttrColMap.DBColMap.ColName;
    lPropName   := lAttrColMap.AttrMap.AttrName;

    { A little more fine grained. OID and Owner.OID will pass - not just any
      property with the letters OID in it. }
    if (SameText('OID', lPropName) or SameText('Owner.OID', lPropName)) and
      (Pos('_OID', UpperCase(lPropName)) = 0) then
      _SetOIDParam(AParams, AData, lColName, lPropName)
    {$IFNDEF OID_AS_INT64}
    else if tiPropertyInheritsFrom(AData.ClassType, lPropName, TtiOID) then
    begin
      lOID := TtiOID(GetObjectProp(AData, lPropName));
      if assigned(lOID) then
        lOID.AssignToTIQueryParam(lColName, AParams);
    end
    {$ENDIF}
    else
      AParams.SetValueFromProp(AData, lPropName, lColName);
  end;
end;

constructor TVisAutoAbs.Create;
begin
  inherited;
  FWhereAttrColMaps := TtiAttrColMaps.Create;
  FWhereAttrColMaps.OwnsObjects := False;
  FWhereAttrColMaps.AutoSetItemOwner := False;
  FWhere := TtiQueryParams.Create;

  FAttrColMaps := TtiAttrColMaps.Create;
  FAttrColMaps.OwnsObjects := False;
  FAttrColMaps.AutoSetItemOwner := False;
  FParams      := TtiQueryParams.Create;
end;

destructor TVisAutoAbs.Destroy;
begin
  FWhereAttrColMaps.Free;
  FWhere.Free;
  FParams.Free;
  FAttrColMaps.Free;
  inherited;
end;


{ TVisAutoCollectionRead }

function TVisAutoCollectionRead.AcceptVisitor: boolean;
begin
  Result :=
    ((Visited.ObjectState = posEmpty) or
    (Visited.ObjectState = posPK)) and
    (GTIOPFManager.ClassDBMappingMgr.Collections.IsCollection(
    TtiClass(Visited.ClassType)));
end;

procedure TVisAutoCollectionRead.Execute(const AData: TtiVisited);
var
  lCollections: TList;
  i: integer;
begin
  inherited Execute(AData);
  if not AcceptVisitor then
    Exit; //==>
  FSetObjectState := False;
  FClassesWithParent.Clear;
  lCollections    := TList.Create;
  try
    lCollections.Clear;
    GTIOPFManager.ClassDBMappingMgr.Collections.FindByCollection(
      FVisitedClassType,
      lCollections);
    ReadDataForParentClass(TtiClassDBCollection(lCollections.Items[0]));
    for i := 1 to lCollections.Count - 1 do
      ReadDataForChildClasses(TtiClassDBCollection(lCollections.Items[i]));
  finally
    lCollections.Free;
  end;
  SetContinueVisiting;
end;

procedure TVisAutoCollectionRead.ReadDataForParentClass(ACollection: TtiClassDBCollection);
begin
  FClassDBCollection := ACollection;
  SetupParams;
  SetUpCriteria; 

  // use 2 different SelectRow methods so that non-criteria-aware queries still work
  if Assigned(FCriteria) then
    Query.SelectRow(FAttrColMaps.TableName, FWhere, FCriteria)
  else
    Query.SelectRow(FAttrColMaps.TableName, FWhere);

  while not Query.EOF do
  begin
    MapRowToObject(False);
    Query.Next;
  end;
  Query.Close;
end;

procedure TVisAutoCollectionRead.ReadDataForChildClasses(ACollection: TtiClassDBCollection);

  procedure _GetWhereAttrColMaps(const AData: TtiObject);
  var
    i: integer;
  begin
    Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');

    GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
      TtiClass(FClassDBCollection.PerObjAbsClass), FWhereAttrColMaps);

    // Remove any mappings that are not foreign key mappings
    for i := FWhereAttrColMaps.Count - 1 downto 0 do
      if (not (pktFK in FWhereAttrColMaps.Items[i].DBColMap.PKInfo)) then
        FWhereAttrColMaps.Delete(i);

    AddToParams(FWhere, FWhereAttrColMaps, AData);

  end;

var
  i:          integer;
  lCount:     integer;
  lTableName: string;
begin
  FClassDBCollection := ACollection;
  GetAttrColMaps;
  for i := 0 to TtiObjectList(Visited).Count - 1 do
  begin
    _GetWhereAttrColMaps(TtiObjectList(Visited).Items[i]);
    lTableName := FWhereAttrColMaps.TableName;
    Assert(lTableName <> '', 'Unable to find table name. FWhereAttrColMaps.Count = ' +
      IntToStr(FWhereAttrColMaps.Count) + '. Suspect a missing [pktFK] value ' +
      'in the child classes RegisterMapping calls.');
    Query.SelectRow(lTableName, FWhere);
    lCount     := 0;
    while not Query.EOF do
    begin
      MapRowToObject(True);
      Query.Next;
      Inc(lCount);
    end;
    Query.Close;
    if lCount > 1 then
      raise EtiOPFDataException.CreateFmt(CErrorQueryReturnedMoreThanOneRow, [lCount]);
  end;
end;


{
procedure TVisAutoCollectionRead.PopulateIfChildClasses;
var
  i : integer;
  lVisAutoReadThis : TVisAutoReadThis;
begin
  // If a class has a registered parent, then only its PK info will
  // have been read. This will read remaining data on these classes.
  // It was tempting to just call ReadThis on each object in the list, but
  // that caused another database connection to be created which failed if
  // the dbConnectionPool.MaxCount value was 1
  // (as it is for the XML persistence layer.)
  if FClassesWithParent.Count <> 0 then
  begin
    lVisAutoReadThis := TVisAutoReadThis.Create;
    try
      lVisAutoReadThis.DBConnection := Self.DBConnection;
      for i := 0 to FClassesWithParent.Count - 1 do
        TtiObject(FClassesWithParent.Items[i]).Iterate(lVisAutoReadThis);

      // Force Final to be called (calling Final is usually handled by the visitor manager)
      for i := 0 to FClassesWithParent.Count - 1 do
      begin
        lVisAutoReadThis.Visited := TtiObject(FClassesWithParent.Items[i]);
        lVisAutoReadThis.Final;
      end;

    finally
      lVisAutoReadThis.Free;
    end;
  end;
end;
}

procedure TVisAutoCollectionRead.MapRowToObject(ACheckForDuplicates: boolean);

  function _DoesOwnObjects(AData: TtiObject): boolean;
  var
    lList: TList;
    i:     integer;
  begin
    Result := (AData.PropCount([tkClass]) > 0);
    if not Result then
      Exit; //==>
    Result := False;
    lList  := TList.Create;
    try
      AData.FindAllByClassType(TtiObject, lList);
      for i := 0 to lList.Count - 1 do
        if TtiObject(lList.Items[i]).Owner = AData then
        begin
          Result := True;
          Break; //==>
        end;
    finally
      lList.Free;
    end;
  end;

  function _DuplicateObject(var AIndex: integer): boolean;
  var
    lData:      TtiObject;
    lOID:       TtiOID;
    lPKColName: string;
    i:          integer;
  begin

    lPKColName := '';
    for i := 0 to FAttrColMaps.Count - 1 do
      if (pktDB in FAttrColMaps.Items[i].DBColMap.PKInfo) then
      begin
        lPKColName := FAttrColMaps.Items[i].DBColMap.ColName;
        Break; //==>
      end;

    Assert(lPKColName <> '', 'Can not determine primary key column. FAttrColMaps.Count <> 1');
    {$IFDEF OID_AS_INT64}
    lOID   := Query.FieldAsInteger[lPKColName];
    lData  := TtiObjectList(Visited).Find(lOID);
    Result := (lData <> NIL);
    if Result then
      AIndex := TtiObjectList(Visited).IndexOf(lData)
    else
      AIndex := -1;
    {$ELSE}
    lOID := GTIOPFManager.DefaultOIDGenerator.OIDClass.Create;
    try
      lOID.AssignFromTIQuery(lPKColName, Query);
      lData  := TtiObjectList(Visited).Find(lOID);
      Result := (lData <> NIL);
      if Result then
        AIndex := TtiObjectList(Visited).IndexOf(lData)
      else
        AIndex := -1;
    finally
      lOID.Free;
    end;
    {$ENDIF}
  end;

var
  lDataOld: TtiObject;
  lDataNew: TtiObject;
  lIndex:   integer;
  lList:    TtiObjectList;
begin
  if FAttrColMaps.Count = 0 then
    Exit; //==>

          // If we are working with a collection of objects of different types, there
          // is a chance that the object will be read more than once from more than one
          // table. For example, if we have a parent and child object, which are
          // persisted accross two tables, and the parent object is a valid object.
          // Say there are some entries in the parent table only, and some in both
          // the parent and child tables. The parent table will be read first so
          // an instance of the abstract class will be created. Next the child table
          // will be read and if there is a record there, then the first object will
          // be of the wrong type. To fix this, the original instance is removed and
          // a new copy created.
  lList := TtiObjectList(Visited);
  if ACheckForDuplicates and _DuplicateObject(lIndex) then
  begin
    lDataOld := lList.Items[lIndex];
    lDataNew := FClassToCreate.Create;
    lDataNew.Assign(lDataOld);
    lIndex   := lList.IndexOf(lDataOld);
    lList.Insert(lIndex, lDataNew);
    lList.Remove(lDataOld);
  end
  else
  begin
    lDataNew := FClassToCreate.Create;
    lList.Add(lDataNew);
  end;

  QueryResultToObject(lDataNew, FAttrColMaps);

  // Can do better than this rather messy call
  // (If its a collection, or it owns objects - not totally reliable
  if ((GTIOPFManager.ClassDBMappingMgr.Collections.IsCollection(FClassToCreate)) or
    (_DoesOwnObjects(lDataNew))) {or
      (FHasParent)} then
    lDataNew.ObjectState := posPK
  else
    lDataNew.ObjectState := posClean;

  if FHasParent then
    FClassesWithParent.Add(lDataNew);

end;

{ TVisAutoUpdateAbs }

procedure TVisAutoUpdateAbs.Execute(const AData: TtiVisited);
var
  lClassMaps: TtiClassMaps;
  i:          integer;
begin
  inherited Execute(AData);
  if not AcceptVisitor then
    Exit; //==>

          // Attached by the VisitorCtrlr
          //  Query.AttachDatabase(DBConnection.Database);
          //  try
  lClassMaps := TtiClassMaps.Create;
  try
    lClassMaps.OwnsObjects := False;
    GTIOPFManager.ClassDBMappingMgr.ClassMaps.FindAllParents(
      FVisitedClassType, lClassMaps);
    // For Create and Update
    // ToDo: This should be a case
    if IterationStyle = isTopDownRecurse then
      for i := 0 to lClassMaps.Count - 1 do
      begin
        FVisitedClassType := lClassMaps.Items[i].PerObjAbsClass;
        SetupParams;
        DoExecuteQuery;
      end
    else
      // For Delete
      for i := lClassMaps.Count - 1 downto 0 do
      begin
        FVisitedClassType := lClassMaps.Items[i].PerObjAbsClass;
        SetupParams;
        DoExecuteQuery;
      end;
  finally
    lClassMaps.Free;
  end;
  //  finally
  //    Query.DetachDatabase;
  //  end;
end;

procedure TVisAutoUpdateAbs.GetAttrColMaps;
var
  i: integer;
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');
  GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    FVisitedClassType, FAttrColMaps);

  // Remove any mappings that are primary key mappings
  for i := FAttrColMaps.Count - 1 downto 0 do
    if ((pktDB in FAttrColMaps.Items[i].DBColMap.PKInfo)) then
      FAttrColMaps.Delete(i);

  // Remove any mappings that are foreign key mappings
  for i := FAttrColMaps.Count - 1 downto 0 do
    if ((pktFK in FAttrColMaps.Items[i].DBColMap.PKInfo)) then
      FAttrColMaps.Delete(i);

  AddToParams(FParams, FAttrColMaps, Visited);
end;

procedure TVisAutoUpdateAbs.GetWhereAttrColMaps;
var
  i: integer;
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');
  GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    FVisitedClassType, FWhereAttrColMaps);
  // Remove any mappings that are not primary key mappings
  for i := FWhereAttrColMaps.Count - 1 downto 0 do
    if (not (pktDB in FWhereAttrColMaps.Items[i].DBColMap.PKInfo)) then
      FWhereAttrColMaps.Delete(i);
  AddToParams(FWhere, FWhereAttrColMaps, Visited);
end;

{ TVisAutoDelete }

function TVisAutoDelete.AcceptVisitor: boolean;
begin
  Result := (Visited.ObjectState = posDelete) and
    (GTIOPFManager.ClassDBMappingMgr.ClassMaps.IsClassReg(
    TtiClass(Visited.ClassType)));
end;

constructor TVisAutoDelete.Create;
begin
  inherited;
  IterationStyle:= isBottomUpSinglePass;
end;

procedure TVisAutoDelete.DoExecuteQuery;
begin
  Query.DeleteRow(FWhereAttrColMaps.TableName, FWhere);
end;

procedure TVisAutoDelete.GetAttrColMaps;
begin
  // For delete, we hav no AttrColMaps
end;

{ TVisAutoUpdate }

function TVisAutoUpdate.AcceptVisitor: boolean;
begin
  Result := (Visited.ObjectState = posUpdate) and
    (GTIOPFManager.ClassDBMappingMgr.ClassMaps.IsClassReg(
    TtiClass(Visited.ClassType)));
end;

{
procedure TVisAutoUpdate.DoSetupQuery;
begin
  Query.SetupForUpdate(FAttrColMaps, nil);
end;
}

procedure TVisAutoUpdate.DoExecuteQuery;
begin
  Query.UpdateRow(FWhereAttrColMaps.TableName, FParams, FWhere);
end;

{ TVisAutoCreate }

function TVisAutoCreate.AcceptVisitor: boolean;
begin
  Result := (Visited.ObjectState = posCreate) and
    (GTIOPFManager.ClassDBMappingMgr.ClassMaps.IsClassReg(
    TtiClass(Visited.ClassType)));
end;

procedure TVisAutoCreate.DoExecuteQuery;
begin
  Query.InsertRow(FAttrColMaps.TableName, FParams);
end;

procedure TVisAutoCreate.GetAttrColMaps;
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');
  GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    FVisitedClassType, FAttrColMaps);
  AddToParams(FParams, FAttrColMaps, Visited);
end;

procedure TVisAutoCreate.GetWhereAttrColMaps;
begin
  // Do nothing
end;

{ TVisAutoThisRead }

function TVisAutoReadThis.AcceptVisitor: boolean;
begin
  Result :=
    ((Visited.ObjectState = posEmpty) or
    (Visited.ObjectState = posPK)) and
    (GTIOPFManager.ClassDBMappingMgr.ClassMaps.IsClassReg(TtiClass(Visited.ClassType)));
end;

procedure TVisAutoReadThis.DoExecute;
var
  lCount:     integer;
  lTableName: string;
begin
  SetupParams;
  lTableName := FWhereAttrColMaps.TableName;
  Query.SelectRow(lTableName, FWhere);
  try
    lCount := 0;
    while not Query.EOF do
    begin
      if lCount > 0 then
        raise Exception.Create('Query returned more than one row');
      MapRowToObject;
      Query.Next;
      Inc(lCount);
    end;
  finally
    Query.Close;
  end;
end;

procedure TVisAutoReadThis.Execute(const AData: TtiVisited);
var
  lClassMaps: TtiClassMaps;
  i:          integer;
begin
  inherited Execute(AData);

  if not AcceptVisitor then
    Exit; //==>

  FSetObjectState := False;

  lClassMaps := TtiClassMaps.Create;
  try
    lClassMaps.OwnsObjects := False;
    GTIOPFManager.ClassDBMappingMgr.ClassMaps.FindAllParents(
      FVisitedClassType, lClassMaps);
    for i := 0 to lClassMaps.Count - 1 do
    begin
      FVisitedClassType := lClassMaps.Items[i].PerObjAbsClass;
      DoExecute;
    end;
  finally
    lClassMaps.Free;
  end;
  // If we are going to check object state in AcceptVisitor, then we will have to set
  // it in a reliable way after visiting. At the moment, it is set to
  // posClean or posDeleted in TVisPerObjAwareAbs. It will have to set it to
  // posPK if the object being added is itself a collection.
  // The check in AcceptVisitor for posEmpty will have to be extended to allow
  // for posPK too.
end;

procedure TVisAutoReadThis.Final(const AVisited: TtiObject);
begin
  if FSetObjectState then
  begin
    // Just a double check, the same as AcceptVisitor
    Assert((AVisited.ObjectState = posEmpty) or
      (AVisited.ObjectState = posPK),
      'Object state on ' + AVisited.ClassName +
      ' not posEmpty or posPK it''s ' +
      AVisited.ObjectStateAsString);
    if (GTIOPFManager.ClassDBMappingMgr.Collections.IsCollection(
      TtiClass(AVisited.ClassType))) then
      AVisited.ObjectState := posPK
    else
      AVisited.ObjectState := posClean;
  end;
end;

procedure TVisAutoReadThis.GetAttrColMaps;
var
  i: integer;
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');
  GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    FVisitedClassType, FAttrColMaps);
  for i := FAttrColMaps.Count - 1 downto 0 do
    if ((pktFK in FAttrColMaps.Items[i].DBColMap.PKInfo)) then
      FAttrColMaps.Delete(i);
end;

procedure TVisAutoReadThis.GetWhereAttrColMaps;
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');
  GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllPKMappingsByMapToClass(
    FVisitedClassType, FWhereAttrColMaps);
  AddToParams(FWhere, FWhereAttrColMaps, Visited);
end;

procedure TVisAutoReadThis.MapRowToObject;
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');
  QueryResultToObject(Visited, FAttrColMaps);
  FSetObjectState := True;
end;

procedure TVisAutoAbs.QueryResultToObject(const ATarget: TtiObject; const pAttrColMaps: TtiAttrColMaps);

  procedure _SetPropValue(const ATarget: TtiObject; const pAttrColMap: TtiAttrColMap);
  var
    lPropName:  string;
    lColName:   string;
    lFieldKind: TtiQueryFieldKind;
    lPropType:  TTypeKind;
    lInt:       int64;
    lStream:    TStream;
    lString:    string;
    {$IFNDEF OID_AS_INT64}
    lOID:       TtiOID;
    {$ENDIF}
  begin
    lColName  := pAttrColMap.DBColMap.ColName;
    lPropName := pAttrColMap.AttrMap.AttrName;

    // Some hacking around OIDs. Tidy this up
    if SameText(lPropName, 'OID') then
    begin
      {$IFDEF OID_AS_INT64}
      ATarget.OID := Query.FieldAsInteger[lColName];
       {$ELSE}
      ATarget.OID.AssignFromTIQuery(lColName, Query);
      {$ENDIF}
      Exit; //==>
    end;

    //    // handles published OIDs
    {$IFNDEF OID_AS_INT64}
    if tiPropertyInheritsFrom(ATarget.ClassType, lPropName, TtiOID) then
    begin
      lOID := TtiOID(GetObjectProp(ATarget, lPropName));
      if Assigned(lOID) then
      begin
        lOID.AssignFromTIQuery(lColName, Query);
        exit;
      end;
    end;
    {$ENDIF}

    //    if SameText(lPropName, 'DispOrder') then
    //    begin
    //      lInt := Query.FieldAsInteger[ lColName ];
    //      ATarget.DispOrder := lInt;
    //      Exit; //==>
    //    end;

    // ToDo: When setting a property in the auto map visitors, might be better to determine
    //       which set method to use based on the property type, rather than the db-field type.
    lFieldKind := Query.FieldKind(Query.FieldIndex(lColName));
    if (ATarget.IsReadWriteProp(lPropName)) or
      (lFieldKind = qfkBinary) then
      case lFieldKind of
        qfkString,
        qfkLongString:
        begin
          lString := Query.FieldAsString[lColName];
                      {$IFDEF BOOLEAN_CHAR_1}
          if ((UpperCase(lString) = 'T') or
            (UpperCase(lString) = 'F')) and
            (tiGetSimplePropType(ATarget, lPropName) = tiTKBoolean) then
            TypInfo.SetOrdProp(ATarget, lPropName, Ord(UpperCase(lString) = 'T'));
                      {$ELSE}
          if ((UpperCase(lString) = 'TRUE') or
            (UpperCase(lString) = 'FALSE')) and
            (tiGetSimplePropType(ATarget, lPropName) = tiTKBoolean) then
            TypInfo.SetOrdProp(ATarget, lPropName, Ord(UpperCase(lString) = 'TRUE'));
                      {$ENDIF}
          TypInfo.SetStrProp(ATarget, lPropName, lString);
        end;
        qfkInteger:
        begin
          lPropType := PropType(ATarget, lPropName);
          lInt      := Query.FieldAsInteger[lColName];
          if (lPropType = tkInt64) then
            // and ((lInt < Low(LongInt)) or (lInt > High(LongInt))) then
            TypInfo.SetInt64Prop(ATarget, lPropName, lInt)
          else
            TypInfo.SetOrdProp(ATarget, lPropName, lInt);
        end;
        qfkFloat: TypInfo.SetFloatProp(ATarget, lPropName, Query.FieldAsFloat[lColName]);
        qfkDateTime: TypInfo.SetFloatProp(ATarget, lPropName, Query.FieldAsDateTime[lColName]);
        qfkLogical: TypInfo.SetOrdProp(ATarget, lPropName, Ord(Query.FieldAsBoolean[lColName]));
        qfkBinary:
        begin
          lStream := TypInfo.GetObjectProp(ATarget, lPropName) as TStream;
          Query.AssignFieldAsStream(lColName, lStream);
        end;
        else
          raise EtiOPFInternalException.Create(cErrorInvalidQueryFieldKind);
      end;
  end;

var
  i:           integer;
  lAttrColMap: TtiAttrColMap;
begin
  for i := 0 to FAttrColMaps.Count - 1 do
  begin
    lAttrColMap := FAttrColMaps.Items[i];
    _SetPropValue(ATarget, lAttrColMap);
  end;
end;

procedure TVisAutoCollectionRead.GetAttrColMaps;
var
  i: integer;
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');

  GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    TtiClass(FClassDBCollection.PerObjAbsClass), FAttrColMaps);

  FClassToCreate := FAttrColMaps.Items[0].AttrMap.Owner.PerObjAbsClass;
  FHasParent     := GTIOPFManager.ClassDBMappingMgr.ClassMaps.HasParent(FClassToCreate);

  // If the class we are reading is a concrete class and it
  // has parents registered, the we should only read its
  // OID and read its data in a subsequent call.
  if (FHasParent) then
  {
    // Remove any mappings that are not PK mappings
    for i := FAttrColMaps.Count - 1 downto 0 do
      if not(pktDB in FAttrColMaps.Items[i].DBColMap.PKInfo) then
        FAttrColMaps.Delete(i);
}
  else
    for i := FAttrColMaps.Count - 1 downto 0 do
      if pktFK in FAttrColMaps.Items[i].DBColMap.PKInfo then
        FAttrColMaps.Delete(i)// Remove any foreign key mappings
  ;

end;

procedure TVisAutoCollectionRead.GetWhereAttrColMaps;
var
  i: integer;
begin
  Assert(FVisitedClassType <> NIL, 'FVisitedClassType = nil');

  GTIOPFManager.ClassDBMappingMgr.AttrColMaps.FindAllMappingsByMapToClass(
    TtiClass(FClassDBCollection.PerObjAbsClass), FWhereAttrColMaps);

  // Remove any mappings that are not foreign key mappings
  for i := FWhereAttrColMaps.Count - 1 downto 0 do
    if (not (pktFK in FWhereAttrColMaps.Items[i].DBColMap.PKInfo)) then
      FWhereAttrColMaps.Delete(i);

  AddToParams(FWhere, FWhereAttrColMaps, Visited);
end;

 { TVisAutoCollectionPKRead }

 //procedure TVisAutoCollectionPKRead.Final;
 //begin
 //  Visited.ObjectState := posPK;
 //end;

procedure TVisAutoCollectionPKRead.GetAttrColMaps;
var
  i: integer;
begin
  inherited;
  // Remove any mappings that are not primary key, or primary key -readable
  for i := FAttrColMaps.Count - 1 downto 0 do
    if (not (pktDB in FAttrColMaps.Items[i].DBColMap.PKInfo)) and
      (not (pktReadable in FAttrColMaps.Items[i].DBColMap.PKInfo)) then
      FAttrColMaps.Delete(i);
end;


procedure TVisAutoAbs.SetupParams;
begin
  GetWhereAttrColMaps;
  GetAttrColMaps;
end;

procedure TVisAutoAbs.Execute(const AData: TtiVisited);
begin
  inherited Execute(AData);
  FVisitedClassType := TtiClass(AData.ClassType);
end;

 //procedure TVisAutoCollectionRead.Final;
 //begin
 //  if FSetObjectState then
 //    if (GTIOPFManager.ClassDBMappingMgr.Collections.IsCollection(
 //       TtiClass(Visited.ClassType))) then
 //      Visited.ObjectState := posPK
 //    else
 //      Visited.ObjectState := posClean;
 //end;

procedure TVisAutoCollectionRead.SetContinueVisiting;
begin
  //  Do nothing, this method is used in the child class
end;

procedure TVisAutoCollectionRead.SetUpCriteria;
var
  lFiltered: ItiFiltered;
begin
  FCriteria := NIL;

  Supports(visited, ItiFiltered, lFiltered);

  if assigned(lFiltered) and (lFiltered.HasCriteria or lFiltered.HasOrderBy) then
  begin
    FCriteria := lFiltered.GetCriteria;

    FCriteria.MapFieldNames(TtiClass(FClassDBCollection.PerObjAbsClass));
  end;

end;

 //function TVisAutoCollectionPKRead.GetObjectState: TPerObjectState;
 //begin
 //  result := posPK;
 //end;

procedure TVisAutoCollectionPKRead.SetContinueVisiting;
begin
  ContinueVisiting := False;
end;

 //function TVisAutoCollectionRead.GetObjectState: TPerObjectState;
 //begin
 //  result := posClean;
 //end;

constructor TVisAutoCollectionRead.Create;
begin
  inherited;
  FClassesWithParent := TList.Create;
end;

destructor TVisAutoCollectionRead.Destroy;
begin
  FClassesWithParent.Free;
  inherited;
end;

function TVisAutoAbs.ParamsToString(const AParams: TtiQueryParams): string;
var
  i:      integer;
  lName:  string;
  lValue: string;
begin
  Result := '';
  for i := 0 to AParams.Count - 1 do
  begin
    lName  := AParams.ParamName(i);
    lValue := AParams.Items[i].GetValueAsString;
    Result := Result + lName + ' = ' + lValue + ', ';
  end;
end;

end.
