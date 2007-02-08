unit tiClassToDBMap_BOM;

{$I tiDefines.inc}

interface
uses
  tiObject
  ,tiVisitor
  ,Classes
 ;

const
  cErrorInconsistentTableNames = 'Inconsistent table names found in DBColMap';

type

  TtiClassDBMappingMgr       = class;
  TtiClassMaps               = class;
  TtiDBMaps                  = class;
  TtiAttrColMaps             = class;

  TtiClassMap                = class;
  TtiAttrMap                 = class;
  TtiDBMap                   = class;
  TtiDBTableMap              = class;
  TtiDBColMap                = class;
  TtiAttrColMap              = class;

  TtiClassDBCollections      = class;
  TtiClassDBCollection       = class;

//  TtiForeignKeyMaps          = class;
//  TtiForeignKeyMap           = class;

  TtiClassDBMapRelationshipType = (pktDB, pktFK, pktReadable);
  TPKInfo = set of TtiClassDBMapRelationshipType;


  TtiClassDBMappingMgr = class(TtiObject)
  private
    FAttrColMaps : TtiAttrColMaps;
    FClassMaps  : TtiClassMaps;
    FDBMaps     : TtiDBMaps;
    FCollections : TtiClassDBCollections;
//    FForeignKeyMaps : TtiForeignKeyMaps;
  protected
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   RegisterMapping(const ADatabaseName : string;
                                 const AClass : TtiClass;
                                 const ATableName : string;
                                 const AAttrName : string;
                                 const AColName  : string;
                                 const APKInfo   : TPKInfo = []); overload;
    procedure   RegisterMapping(const AClass : TtiClass;
                                 const ATableName : string;
                                 const AAttrName : string;
                                 const AColName  : string;
                                 const APKInfo   : TPKInfo = []); overload;
    procedure   RegisterCollection( const ACollectionClass : TPerObjListClass;
                                     const ACollectionOfClass : TtiClass); overload;
    procedure   RegisterInheritance(const AParentClass : TtiClass;
                                     const AChildClass : TtiClass);
  published
    property    ClassMaps  : TtiClassMaps read FClassMaps {write FClassMaps};
    property    DBMaps     : TtiDBMaps    read FDBMaps    {write FDBMaps};
    property    AttrColMaps : TtiAttrColMaps read FAttrColMaps {write FAttrColMaps};
    property    Collections : TtiClassDBCollections read FCollections;
  end;


  TtiClassMaps = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiClassMap; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiClassMap); reintroduce;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
    function    FindByPerObjAbsClass(const AClass: TtiClass): TtiClassMap;
  public
    property    Items[i:integer]: TtiClassMap read GetItems write SetItems;
    procedure   Add(AObject : TtiClassMap  ; ADefaultDispOrder : boolean = true); reintroduce;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner;
    function    AddClassMap(const AClass : TtiClass): TtiClassMap;
    function    FindCreate(const AClass : TtiClass): TtiClassMap;
    function    IsClassReg(const AClass : TtiClass): boolean;
    procedure   RegisterInheritance(const AParentClass : TtiClass;
                                     const AChildClass : TtiClass);
    function    FindParent(const AClass : TtiClass): TtiClassMap;
    function    HasParent( const AClass : TtiClass): Boolean;
    procedure   FindAllParents(const AClass : TtiClass;
                                const AList : TtiClassMaps);
  end;


  TtiClassMap = class(TtiObjectList)
  private
    FPerObjAbsClass: TtiClass;
    FParentClassMap: TtiClassMap;
    function FindByAttrName(const AAttrName: string): TtiAttrMap;
    function _IsPublishedProp(const AAttrName: string): Boolean;
    function _IsPublicProperty(const AAttrName: string): Boolean;
  protected
    function    GetCaption : string; override;
    function    GetItems(i: integer): TtiAttrMap; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiAttrMap); reintroduce;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property    Items[i:integer]: TtiAttrMap read GetItems write SetItems;
    procedure   Add(AObject : TtiAttrMap  ; ADefaultDispOrder : boolean = true); reintroduce;
    property    Owner : TtiClassDBMappingMgr read GetOwner write SetOwner;
    property    PerObjAbsClass : TtiClass read FPerObjAbsClass write FPerObjAbsClass;
    function    AddAttrMap(const AAttrName : string): TtiAttrMap;
    property    ParentClassMap : TtiClassMap read FParentClassMap write FParentClassMap;
//    function    FindCreate(const AAttrName : string): TtiAttrMap;
  end;


  TtiAttrMap = class(TtiObject)
  private
    FAttrName: string;
  protected
    function    GetCaption: string; override;
    function    GetOwner: TtiClassMap; reintroduce;
    procedure   SetOwner(const AValue: TtiClassMap); reintroduce;
  public
    property    Owner: TtiClassMap read GetOwner write SetOwner;
  published
    property    AttrName: string read FAttrName write FAttrName;
  end;


  TtiDBMaps = class(TtiObjectList)
  private
    function FindByDatabaseName(const ADatabaseName: string): TtiDBMap;
  protected
    function    GetItems(i: integer): TtiDBMap; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiDBMap); reintroduce;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property    Items[i:integer]: TtiDBMap read GetItems write SetItems;
    procedure   Add(AObject : TtiDBMap; ADefaultDispOrder : boolean = true); reintroduce;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner;
    function    AddDBMap(const ADatabaseName : string): TtiDBMap;
    function    FindCreate(const ADatabaseName : string): TtiDBMap;
  published
  end;


  TtiDBMap = class(TtiObjectList)
  private
    FDatabaseName: string;
    function FindByTableName(const ATableName: string): TtiDBTableMap;
  protected
    function    GetItems(i: integer): TtiDBTableMap; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiDBTableMap); reintroduce;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property    Items[i:integer]: TtiDBTableMap read GetItems write SetItems;
    procedure   Add(AObject : TtiDBTableMap; ADefaultDispOrder : boolean = true); reintroduce;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner;
    function    AddTableMap(const ATableName : string): TtiDBTableMap;
    function    FindCreate(const ATableName : string): TtiDBTableMap;
  published
    property    DatabaseName : string read FDatabaseName write FDatabaseName;
  end;


  TtiDBTableMap = class(TtiObjectList)
  private
    FTableName: string;
    function FindByColName(const AColName: string): TtiDBColMap;
  protected
    function    GetCaption : string; override;
    function    GetItems(i: integer): TtiDBColMap; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiDBColMap); reintroduce;
    function    GetOwner: TtiDBMap; reintroduce;
    procedure   SetOwner(const AValue: TtiDBMap); reintroduce;
  public
    property    Items[i:integer]: TtiDBColMap read GetItems write SetItems;
    procedure   Add(AObject : TtiDBColMap; ADefaultDispOrder : boolean = true); reintroduce;
    property    Owner : TtiDBMap read GetOwner      write SetOwner;
    function    AddColMap(const AColName : string; APKInfo : TPKInfo): TtiDBColMap;
  published
    property    TableName : string read FTableName write FTableName;
  end;


  TtiDBColMap = class(TtiObject)
  private
    FColName: string;
    FPKInfo: TPKInfo;
  protected
    function    GetCaption : string; override;
    function    GetOwner: TtiDBTableMap; reintroduce;
    procedure   SetOwner(const AValue: TtiDBTableMap); reintroduce;
  public
    property    Owner : TtiDBTableMap read GetOwner write SetOwner;
  published
    property    ColName : string read FColName write FColName;
    property    PKInfo : TPKInfo read FPKInfo write FPKInfo;
  end;


  TtiAttrColMaps = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiAttrColMap; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiAttrColMap); reintroduce;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property    Items[i:integer]: TtiAttrColMap read GetItems write SetItems;
    procedure   Add(AObject : TtiAttrColMap  ; ADefaultDispOrder : boolean = true); reintroduce;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner;
    procedure   AddMapping(const pAttrMap : TtiAttrMap; const pColMap : TtiDBColMap);
    procedure   FindAllMappingsByMapToClass(const AClass : TtiClass;
                                             const AList : TtiAttrColMaps);
    procedure   FindAllPKMappingsByMapToClass(const AClass : TtiClass;
                                               const AList : TtiAttrColMaps);
    function    FindByClassAttrMap(const AClass : TtiClass;
                                    const AAttrName : string): TtiAttrColMap;
    function    TableName : string;
  published
  end;


  TtiAttrColMap = class(TtiObject)
  private
    FAttrMap: TtiAttrMap;
    FDBColMap: TtiDBColMap;
  protected
    function    GetCaption : string; override;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property    Owner : TtiClassDBMappingMgr read GetOwner write SetOwner;
    property    AttrMap : TtiAttrMap read FAttrMap write FAttrMap;
    property    DBColMap : TtiDBColMap read FDBColMap write FDBColMap;
  end;


  TtiClassDBCollections = class(TtiObjectList)
  protected
    function    GetItems(i: integer): TtiClassDBCollection; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiClassDBCollection); reintroduce;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce;
    procedure   SetOwner(const AValue: TtiClassDBMappingMgr); reintroduce;
  public
    property    Items[i:integer]: TtiClassDBCollection read GetItems write SetItems;
    procedure   Add(AObject : TtiClassDBCollection  ; ADefaultDispOrder : boolean = true); reintroduce;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner;
    function    AddClassCollectionMapping(const ACollectionClass : TPerObjListClass;
                                           const AClass : TtiClass): TtiClassDBCollection; overload;
    function    FindByCollectionOf(const AClass: TtiClass): TtiClassDBCollection;
    procedure   FindByCollection(const AClass: TtiClass; const AList : TList);
    function    IsCollection(const AClass : TtiClass): boolean;
    function    IsInCollection(const AClass : TtiClass): boolean;
  published
  end;


  TtiClassDBCollection = class(TtiObject)
  private
    FPerObjAbsClass : TtiClass;
    FCollectionClass : TPerObjListClass;
//    FForeignKeyCols : TStringList;
    FOwnerAttrMaps  : TtiAttrColMaps;
  protected
    function    GetOwner: TtiClassDBCollections; reintroduce;
    procedure   SetOwner(const AValue: TtiClassDBCollections); reintroduce;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Owner      : TtiClassDBCollections             read GetOwner      write SetOwner;
    property    CollectionClass : TPerObjListClass read FCollectionClass write FCollectionClass;
    property    PerObjAbsClass : TtiClass read FPerObjAbsClass write FPerObjAbsClass;
    property    OwnerAttrMaps  : TtiAttrColMaps read FOwnerAttrMaps;
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
//
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

implementation
uses
   tiLog
  ,tiUtils
  ,tiClassToDBMap_Srv
  ,tiOPFManager
  ,tiExcept
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

procedure TtiClassDBMappingMgr.RegisterMapping(
    const ADatabaseName : string;
    const AClass       : TtiClass;
    const ATableName   : string;
    const AAttrName    : string;
    const AColName     : string;
    const APKInfo      : TPKInfo = []);
var
  lDBMap : TtiDBMap;
  lDBTableMap : TtiDBTableMap;
  lDBColMap  : TtiDBColMap;
  lClassMap  : TtiClassMap;
  lAttrMap   : TtiAttrMap ;
begin
  lDBMap     := FDBMaps.FindCreate(     ADatabaseName);
  lDBTableMap := lDBMap.FindCreate(      ATableName);
  lDBColMap  := lDBTableMap.AddColMap( AColName, APKInfo);

  lClassMap  := FClassMaps.FindCreate(  AClass);
  lAttrMap   := lClassMap.AddAttrMap(   AAttrName);
  lAttrMap.ObjectState := posClean;
  FAttrColMaps.AddMapping(lAttrMap, lDBColMap);
end;

procedure TtiClassDBMappingMgr.RegisterMapping(
    const AClass       : TtiClass;
    const ATableName   : string;
    const AAttrName    : string;
    const AColName     : string;
    const APKInfo      : TPKInfo = []);
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
  FAttrColMaps := TtiAttrColMaps.Create;
  FAttrColMaps.Owner := self;
  FAttrColMaps.ItemOwner := self;

  FClassMaps  := TtiClassMaps.Create;
  FClassMaps.Owner    := self;
  FClassMaps.ItemOwner := self;

  FDBMaps     := TtiDBMaps.Create;
  FDBMaps.Owner := self;
  FDBMaps.ItemOwner := self;

  FCollections := TtiClassDBCollections.Create;
  FCollections.Owner := self;
  FCollections.ItemOwner := self;

//  FForeignKeyMaps := TtiForeignKeyMaps.Create;
//  FForeignKeyMaps.Owner := self;
//  FForeignKeyMaps.ItemOwner := self;

end;

destructor TtiClassDBMappingMgr.destroy;
begin
  FAttrColMaps.Free;
  FClassMaps.Free;
  FDBMaps.Free;
  FCollections.Free;
  inherited;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiClassMaps
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiClassMaps.Add(AObject: TtiClassMap;ADefaultDispOrder: boolean);
begin
  inherited Add(AObject, ADefaultDispOrder);
end;

function TtiClassMaps.AddClassMap(const AClass: TtiClass): TtiClassMap;
begin
  if FindByPerObjAbsClass(AClass) <> nil then
    raise Exception.Create(
                 'Attempt to register duplicate TtiClassMap' + Cr +
                 'Classname: ' + AClass.ClassName + Cr +
                 'Called in ' + ClassName + '.AddClassMap');
//    tiTermError('Attempt to register duplicate TtiClassMap' + Cr +
//                 'Classname: ' + AClass.ClassName + Cr +
//                 'Called in ' + ClassName + '.AddClassMap');

  result := TtiClassMap.Create;
  result.PerObjAbsClass := AClass;
  result.ObjectState := posClean;
  Add(result);
end;

procedure TtiClassMaps.FindAllParents(const AClass: TtiClass; const AList: TtiClassMaps);
var
  lClassMap : TtiClassMap;
begin
  Assert(not AList.OwnsObjects, 'AList.OwnsObjects is true and it should be false');
  AList.Clear;
  lClassMap := FindByPerObjAbsClass(AClass);
  Assert(lClassMap <> nil,
         'Request to find parent on class that is not registered <' +
         AClass.ClassName + '>');
  AList.Insert(0, lClassMap);
  while (lClassMap<>nil)and
        (lClassMap.ParentClassMap <> nil) do
  begin
    lClassMap := lClassMap.ParentClassMap;
    if lClassMap <> nil then
      AList.Insert(0, lClassMap);
  end;
end;

function TtiClassMaps.FindByPerObjAbsClass(const AClass: TtiClass): TtiClassMap;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].PerObjAbsClass = AClass then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;

function TtiClassMaps.FindCreate(const AClass: TtiClass): TtiClassMap;
begin
  result := FindByPerObjAbsClass(AClass);
  if result = nil then
    result := AddClassMap(AClass);
end;

function TtiClassMaps.FindParent(const AClass: TtiClass): TtiClassMap;
var
  lClassMap : TtiClassMap;
begin
  lClassMap := FindByPerObjAbsClass(AClass);
  Assert(lClassMap <> nil,
          'Attempt to find parent on un-registered class <' +
          AClass.ClassName + '>');
  result := lClassMap.ParentClassMap;
end;

function TtiClassMaps.GetItems(i: integer): TtiClassMap;
begin
  result := TtiClassMap(inherited GetItems(i));
end;

function TtiClassMaps.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr(inherited GetOwner);
end;

function TtiClassMaps.HasParent(const AClass: TtiClass): Boolean;
begin
  result := (FindParent(AClass) <> nil);
end;

function TtiClassMaps.IsClassReg(const AClass: TtiClass): boolean;
begin
  result := FindByPerObjAbsClass(AClass) <> nil;
end;

procedure TtiClassMaps.RegisterInheritance(const AParentClass : TtiClass;
                                            const AChildClass : TtiClass);
var
  lClassMapChild : TtiClassMap;
  lClassMapParent : TtiClassMap;
begin

  if not AChildClass.InheritsFrom(AParentClass) then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a child class <' +
      AChildClass.ClassName +
      '> that does not inherit from <' +
      AParentClass.ClassName + '>');

  lClassMapChild := FindByPerObjAbsClass(AChildClass);
  if lClassMapChild = nil then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a child class <' +
      AChildClass.ClassName +
      '> that has not yet been registered');

  lClassMapParent := FindByPerObjAbsClass(AParentClass);
  if lClassMapParent = nil then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a parent class <'+
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

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiClassMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiClassMap.Add(AObject: TtiAttrMap; ADefaultDispOrder: boolean);
begin
  inherited Add(AObject, ADefaultDispOrder);
end;

function TtiClassMap.AddAttrMap(const AAttrName: string): TtiAttrMap;
begin
  if FindByAttrName(AAttrName) <> nil then
    raise exception.Create(
                 'Attempt to register duplicate TtiAttrMap' + Cr +
                 'ClassName: ' + PerObjAbsClass.ClassName + Cr +
                 'AttrName:  ' + AAttrName + Cr +
                 '. Called in ' + ClassName + '.AddAttrMap');
  if not _IsPublishedProp(AAttrName) then
    raise exception.Create(
                 AAttrName + ' is not a published property on ' +
                 PerObjAbsClass.ClassName + Cr +
                 '. Called in ' + ClassName + '.AddAttrMap');
  result := TtiAttrMap.Create;
  result.AttrName := AAttrName;
  result.ObjectState := posClean;
  Add(result);
end;

function TtiClassMap._IsPublishedProp(const AAttrName: string): Boolean;
begin
  result :=
    _IsPublicProperty(AAttrName) or
    (IsPublishedProp(PerObjAbsClass, AAttrName));
end;

function TtiClassMap._IsPublicProperty(const AAttrName: string): Boolean;
begin
  result :=
    SameText(AAttrName, 'OID') or
    SameText(AAttrName, 'Owner.OID') // Must do better than this
end;

function TtiClassMap.FindByAttrName(const AAttrName: string): TtiAttrMap;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Attrname, AAttrName) then
    begin
      result := Items[i];
      Break; //==>
    end;
end;

function TtiClassMap.GetCaption: string;
begin
  result := FPerObjAbsClass.ClassName;
end;

function TtiClassMap.GetItems(i: integer): TtiAttrMap;
begin
  result := TtiAttrMap(inherited GetItems(i));
end;

function TtiClassMap.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr(inherited GetOwner);
end;

procedure TtiClassMap.SetItems(i: integer; const AValue: TtiAttrMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiClassMap.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBMaps
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBMaps.Add(AObject: TtiDBMap; ADefaultDispOrder: boolean);
begin
  inherited Add(AObject, ADefaultDispOrder);
end;

function TtiDBMaps.FindCreate(const ADatabaseName: string): TtiDBMap;
begin
  result := FindByDatabaseName(ADatabaseName);
  if result = nil then
    result := AddDBMap(ADatabaseName);
end;

function TtiDBMaps.FindByDatabaseName(const ADatabaseName: string): TtiDBMap;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].DatabaseName, ADatabaseName) then
    begin
      result := Items[i];
      Break; //==>
    end;
end;

function TtiDBMaps.GetItems(i: integer): TtiDBMap;
begin
  result := TtiDBMap(inherited GetItems(i));
end;

function TtiDBMaps.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr(inherited GetOwner);
end;

procedure TtiDBMaps.SetItems(i: integer; const AValue: TtiDBMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiDBMaps.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBMap.Add(AObject: TtiDBTableMap; ADefaultDispOrder: boolean);
begin
  inherited Add(AObject, ADefaultDispOrder);
end;

function TtiDBMap.AddTableMap(const ATableName: string): TtiDBTableMap;
begin
  if FindByTableName(ATableName) <> nil then
    raise exception.Create(
                 'Attempt to register duplicate TtiDBTableMap' + Cr +
                 'DatabaseName: ' + DatabaseName + Cr +
                 'TableName:    ' + ATableName + Cr +
                 'Called in ' + ClassName + '.AddTableMap');
//    tiTermError('Attempt to register duplicate TtiDBTableMap' + Cr +
//                 'DatabaseName: ' + DatabaseName + Cr +
//                 'TableName:    ' + ATableName + Cr +
//                 'Called in ' + ClassName + '.AddTableMap');
  result := TtiDBTableMap.Create;
  result.TableName := ATableName;
  result.ObjectState := posClean;
  Add(result);
end;

function TtiDBMap.FindCreate(const ATableName: string): TtiDBTableMap;
begin
  result := FindByTableName(ATableName);
  if result = nil then
    result := AddTableMap(ATableName);
end;

function TtiDBMap.FindByTableName(const ATableName: string): TtiDBTableMap;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].TableName, ATableName) then
    begin
      result := Items[i];
      Break; //==>
    end;
end;

function TtiDBMap.GetItems(i: integer): TtiDBTableMap;
begin
  result := TtiDBTableMap(inherited GetItems(i));
end;

function TtiDBMap.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr(inherited GetOwner);
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
  result := FColName;
end;

function TtiDBColMap.GetOwner: TtiDBTableMap;
begin
  result := TtiDBTableMap(inherited GetOwner);
end;

procedure TtiDBColMap.SetOwner(const AValue: TtiDBTableMap);
begin
  inherited SetOwner(AValue);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBTableMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBTableMap.Add(AObject: TtiDBColMap; ADefaultDispOrder: boolean);
begin
  inherited Add(AObject, ADefaultDispOrder);
end;

function TtiDBTableMap.AddColMap(const AColName: string; APKInfo : TPKInfo): TtiDBColMap;
begin
  result := FindByColName(AColName);
  if result <> nil then
    Exit; //==>                     
//    raise exception.Create(
//                 'Attempt to register duplicate TtiDBColMap' + Cr +
//                 'Table name: ' + TableName + Cr +
//                 'Col name:   ' + AColName + Cr +
//                 'Called in ' + ClassName + '.AddColMap');

  result := TtiDBColMap.Create;
  result.ColName := AColName;
  result.PKInfo := APKInfo;
  result.ObjectState := posClean;
  Add(result);
end;

//function TtiDBTableMap.FindCreate(const AColName: string; APKInfo : TPKInfo): TtiDBColMap;
//begin
//  result := FindByColName(AColName);
//  if result = nil then
//    result := AddColMap(AColName, APKInfo);
//end;

function TtiDBTableMap.FindByColName(const AColName : string): TtiDBColMap;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].ColName, AColName) then
    begin
      result := Items[i];
      Break; //==>
    end;
end;

function TtiDBTableMap.GetCaption: string;
begin
  result := FTableName;
end;

function TtiDBTableMap.GetItems(i: integer): TtiDBColMap;
begin
  result := TtiDBColMap(inherited GetItems(i));
end;

function TtiDBTableMap.GetOwner: TtiDBMap;
begin
  result := TtiDBMap(inherited GetOwner);
end;

procedure TtiDBTableMap.SetItems(i: integer; const AValue: TtiDBColMap);
begin
  inherited SetItems(i, AValue);
end;

procedure TtiDBTableMap.SetOwner(const AValue: TtiDBMap);
begin
  inherited SetOwner(AValue);
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiAttrColMaps
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiAttrColMaps.Add(AObject: TtiAttrColMap; ADefaultDispOrder: boolean);
begin
  inherited Add(AObject, ADefaultDispOrder);
end;

procedure TtiAttrColMaps.AddMapping(const pAttrMap: TtiAttrMap;
  const pColMap: TtiDBColMap);
var
  lData : TtiAttrColMap;
begin
  // ToDo: Check we are not adding duplicates in an assert()
  lData := TtiAttrColMap.Create;
  lData.DBColMap := pColMap;
  lData.AttrMap := pAttrMap;
  lData.ObjectState := posClean;
  Add(lData);
end;

procedure TtiAttrColMaps.FindAllMappingsByMapToClass(
  const AClass: TtiClass;
  const AList: TtiAttrColMaps);
var
  i : integer;
begin
  Assert(not AList.OwnsObjects, 'AList.OwnsObjects is true and it should be false');
  AList.Clear;
  for i := 0 to Count - 1 do
    if Items[i].AttrMap.Owner.PerObjAbsClass = AClass then
      AList.Add(Items[i]);
end;

procedure TtiAttrColMaps.FindAllPKMappingsByMapToClass(
  const AClass: TtiClass; const AList: TtiAttrColMaps);
var
  i : integer;
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
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if (Items[i].AttrMap.Owner.PerObjAbsClass = AClass) and
       (SameText(Items[i].AttrMap.AttrName, AAttrName)) then
    begin
      result := Items[i];
      Break; //==>
    end;
end;

function TtiAttrColMaps.GetItems(i: integer): TtiAttrColMap;
begin
  result := TtiAttrColMap(inherited GetItems(i));
end;

function TtiAttrColMaps.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr(inherited GetOwner);
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
  result := TtiClassMap(inherited GetOwner);
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
  result := FAttrMap.Caption + '/' + FDBColMap.Caption;
end;

function TtiAttrColMap.GetOwner: TtiClassDBMappingMgr;
begin
    result := TtiClassDBMappingMgr(inherited GetOwner);
end;

procedure TtiAttrColMap.SetOwner(const AValue: TtiClassDBMappingMgr);
begin
  inherited SetOwner(AValue);
end;

function TtiDBMaps.AddDBMap(const ADatabaseName: string): TtiDBMap;
begin
  if FindByDatabaseName(ADatabaseName) <> nil then
    raise exception.Create(
                 'Attempt to register duplicate TtiDBMap' + Cr +
                 'DatabaseName: ' + ADatabaseName + Cr +
                 'Called in ' + ClassName + 'AddDBMap');
//    tiTermError('Attempt to register duplicate TtiDBMap' + Cr +
//                 'DatabaseName: ' + ADatabaseName + Cr +
//                 'Called in ' + ClassName + 'AddDBMap');
  result := TtiDBMap.Create;
  result.DatabaseName := ADatabaseName;
  result.ObjectState := posClean;
  Add(result);
end;

procedure TtiClassDBMappingMgr.RegisterCollection(
  const ACollectionClass: TPerObjListClass;
  const ACollectionOfClass: TtiClass);
begin
  FCollections.AddClassCollectionMapping(ACollectionClass,
                                          ACollectionOfClass);
end;

{ TtiClassDBCollections }

procedure TtiClassDBCollections.Add(AObject: TtiClassDBCollection; ADefaultDispOrder: boolean);
begin
  inherited Add(AObject, ADefaultDispOrder);
end;

function TtiClassDBCollections.AddClassCollectionMapping(
  const ACollectionClass: TPerObjListClass;
  const AClass: TtiClass): TtiClassDBCollection;
begin
  result := TtiClassDBCollection.Create;
  result.CollectionClass := ACollectionClass;
  result.PerObjAbsClass := AClass;
  result.ObjectState := posClean;
  Add(result);
end;

procedure TtiClassDBCollections.FindByCollection(
  const AClass: TtiClass;
  const AList : TList);
var
  i : integer;
begin
  AList.Clear;
  for i := 0 to Count - 1 do
    if Items[i].CollectionClass = AClass then
      AList.Add(Items[i]);
end;

function TtiClassDBCollections.FindByCollectionOf(const AClass: TtiClass): TtiClassDBCollection;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if Items[i].PerObjAbsClass = AClass then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;

function TtiClassDBCollections.GetItems(i: integer): TtiClassDBCollection;
begin
  result := TtiClassDBCollection(inherited GetItems(i));
end;

function TtiClassDBCollections.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr(inherited GetOwner);
end;

function TtiClassDBCollections.IsInCollection(const AClass: TtiClass): boolean;
begin
  result := FindByCollectionOf(AClass) <> nil;
end;

function TtiClassDBCollections.IsCollection(const AClass: TtiClass): boolean;
var
  lList : TList;
begin
  lList := TList.Create;
  try
    FindByCollection(AClass, lList);
    result := lList.Count > 0;
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
  FOwnerAttrMaps.OwnsObjects := false;
end;

destructor TtiClassDBCollection.Destroy;
begin
  FOwnerAttrMaps.Free;
//  FForeignKeyCols.Free;
  inherited;
end;

function TtiClassDBCollection.GetOwner: TtiClassDBCollections;
begin
  result := TtiClassDBCollections(inherited GetOwner);
end;

procedure TtiClassDBCollection.SetOwner(const AValue: TtiClassDBCollections);
begin
  inherited SetOwner(AValue);
end;

function TtiAttrColMaps.TableName: string;
var
  i : integer;
begin
  result := '';
  for i := 0 to Count - 1 do
  begin
    if result = '' then
      result := Items[i].DBColMap.Owner.TableName;
    if result <> Items[i].DBColMap.Owner.TableName then
      raise EtiOPFProgrammerException.Create(cErrorInconsistentTableNames);
  end;
end;

procedure TtiClassDBMappingMgr.RegisterInheritance(const AParentClass : TtiClass;
                                                    const AChildClass: TtiClass);
begin
  FClassMaps.RegisterInheritance(AParentClass, AChildClass);
end;

end.














