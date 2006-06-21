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

  TtiClassDBMappingMgr       = class ;
  TtiClassMaps               = class ;
  TtiDBMaps                  = class ;
  TtiAttrColMaps             = class ;

  TtiClassMap                = class ;
  TtiAttrMap                 = class ;
  TtiDBMap                   = class ;
  TtiDBTableMap              = class ;
  TtiDBColMap                = class ;
  TtiAttrColMap              = class ;

  TtiClassDBCollections      = class ;
  TtiClassDBCollection       = class ;

//  TtiForeignKeyMaps          = class ;
//  TtiForeignKeyMap           = class ;

  TtiClassDBMapRelationshipType = ( pktDB, pktFK, pktReadable ) ;
  TPKInfo = set of TtiClassDBMapRelationshipType ;

  TtiClassDBMappingMgr = class( TtiObject )
  private
    FAttrColMaps : TtiAttrColMaps;
    FClassMaps   : TtiClassMaps;
    FDBMaps      : TtiDBMaps;
    FCollections : TtiClassDBCollections ;
//    FForeignKeyMaps : TtiForeignKeyMaps ;
  protected
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   RegisterMapping( const pDatabaseName : string ;
                                 const pClass : TtiClass ;
                                 const pTableName : string ;
                                 const pAttrName  : string ;
                                 const pColName   : string ;
                                 const pPKInfo    : TPKInfo = [] ) ; overload ;
    procedure   RegisterMapping( const pClass : TtiClass ;
                                 const pTableName : string ;
                                 const pAttrName  : string ;
                                 const pColName   : string ;
                                 const pPKInfo    : TPKInfo = [] ) ; overload ;
    procedure   RegisterCollection(  const pCollectionClass : TPerObjListClass ;
                                     const pCollectionOfClass : TtiClass ) ; overload ;
    procedure   RegisterInheritance( const pParentClass : TtiClass ;
                                     const pChildClass  : TtiClass ) ;
  published
    property    ClassMaps   : TtiClassMaps read FClassMaps {write FClassMaps} ;
    property    DBMaps      : TtiDBMaps    read FDBMaps    {write FDBMaps} ;
    property    AttrColMaps : TtiAttrColMaps read FAttrColMaps {write FAttrColMaps} ;
    property    Collections : TtiClassDBCollections read FCollections ;
  end ;

  TtiClassMaps = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TtiClassMap ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiClassMap); reintroduce ;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
    function    FindByPerObjAbsClass( const pClass: TtiClass): TtiClassMap;
  public
    property    Items[i:integer] : TtiClassMap read GetItems write SetItems ;
    procedure   Add( pObject : TtiClassMap   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner ;
    function    AddClassMap( const pClass : TtiClass ) : TtiClassMap ;
    function    FindCreate( const pClass : TtiClass ) : TtiClassMap ;
    function    IsClassReg( const pClass : TtiClass ) : boolean ;
    procedure   RegisterInheritance( const pParentClass : TtiClass ;
                                     const pChildClass  : TtiClass ) ;
    function    FindParent( const pClass : TtiClass ) : TtiClassMap ;
    function    HasParent(  const pClass : TtiClass ) : Boolean ;
    procedure   FindAllParents( const pClass : TtiClass ;
                                const pList : TtiClassMaps ) ;
  end ;

  TtiClassMap = class( TtiObjectList )
  private
    FPerObjAbsClass: TtiClass;
    FParentClassMap: TtiClassMap;
    function FindByAttrName(const pAttrName: string): TtiAttrMap;
    function _IsPublishedProp(const pAttrName: string): Boolean;
    function _IsPublicProperty(const pAttrName: string): Boolean;
  protected
    function    GetCaption : string ; override ;
    function    GetItems(i: integer): TtiAttrMap ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiAttrMap); reintroduce ;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
  public
    property    Items[i:integer] : TtiAttrMap read GetItems write SetItems ;
    procedure   Add( pObject : TtiAttrMap   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner : TtiClassDBMappingMgr read GetOwner write SetOwner ;
    property    PerObjAbsClass : TtiClass read FPerObjAbsClass write FPerObjAbsClass ;
    function    AddAttrMap( const pAttrName : string ) : TtiAttrMap ;
    property    ParentClassMap : TtiClassMap read FParentClassMap write FParentClassMap ;
//    function    FindCreate( const pAttrName : string ) : TtiAttrMap ;
  end ;

  TtiAttrMap = class( TtiObject )
  private
    FAttrName: string;
  protected
    function    GetOwner: TtiClassMap; reintroduce ;
    procedure   SetOwner(const Value: TtiClassMap); reintroduce ;
  public
    property    Owner       : TtiClassMap read GetOwner      write SetOwner ;
  published
    property    AttrName : string read FAttrName write FAttrName ;
  end ;

  TtiDBMaps = class( TtiObjectList )
  private
    function FindByDatabaseName(const pDatabaseName: string): TtiDBMap;
  protected
    function    GetItems(i: integer): TtiDBMap ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDBMap); reintroduce ;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
  public
    property    Items[i:integer] : TtiDBMap read GetItems write SetItems ;
    procedure   Add( pObject : TtiDBMap ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner ;
    function    AddDBMap( const pDatabaseName : string ) : TtiDBMap ;
    function    FindCreate( const pDatabaseName : string ) : TtiDBMap ;
  published
  end ;

  TtiDBMap = class( TtiObjectList )
  private
    FDatabaseName: string;
    function FindByTableName(const pTableName: string): TtiDBTableMap;
  protected
    function    GetItems(i: integer): TtiDBTableMap ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDBTableMap); reintroduce ;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
  public
    property    Items[i:integer] : TtiDBTableMap read GetItems write SetItems ;
    procedure   Add( pObject : TtiDBTableMap ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner ;
    function    AddTableMap( const pTableName : string ) : TtiDBTableMap ;
    function    FindCreate( const pTableName : string ) : TtiDBTableMap ;
  published
    property    DatabaseName : string read FDatabaseName write FDatabaseName ;
  end ;

  TtiDBTableMap = class( TtiObjectList )
  private
    FTableName: string;
    function FindByColName(const pColName: string): TtiDBColMap;
  protected
    function    GetCaption : string ; override ;
    function    GetItems(i: integer): TtiDBColMap ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiDBColMap); reintroduce ;
    function    GetOwner: TtiDBMap; reintroduce ;
    procedure   SetOwner(const Value: TtiDBMap); reintroduce ;
  public
    property    Items[i:integer] : TtiDBColMap read GetItems write SetItems ;
    procedure   Add( pObject : TtiDBColMap ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner : TtiDBMap read GetOwner      write SetOwner ;
    function    AddColMap( const pColName : string ; pPKInfo : TPKInfo ) : TtiDBColMap ;
  published
    property    TableName : string read FTableName write FTableName ;
  end ;

  TtiDBColMap = class( TtiObject )
  private
    FColName: string;
    FPKInfo: TPKInfo;
  protected
    function    GetCaption : string ; override ;
    function    GetOwner: TtiDBTableMap; reintroduce ;
    procedure   SetOwner(const Value: TtiDBTableMap); reintroduce ;
  public
    property    Owner : TtiDBTableMap read GetOwner write SetOwner ;
  published
    property    ColName : string read FColName write FColName ;
    property    PKInfo  : TPKInfo read FPKInfo write FPKInfo ;
  end ;

  TtiAttrColMaps = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TtiAttrColMap ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiAttrColMap); reintroduce ;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
  public
    property    Items[i:integer] : TtiAttrColMap read GetItems write SetItems ;
    procedure   Add( pObject : TtiAttrColMap   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner ;
    procedure   AddMapping( const pAttrMap : TtiAttrMap ; const pColMap : TtiDBColMap ) ;
    procedure   FindAllMappingsByMapToClass( const pClass : TtiClass ;
                                             const pList : TtiAttrColMaps ) ;
    procedure   FindAllPKMappingsByMapToClass( const pClass : TtiClass ;
                                               const pList : TtiAttrColMaps ) ;
    function    FindByClassAttrMap( const pClass : TtiClass ;
                                    const pAttrName : string ) : TtiAttrColMap ;
    function    TableName : string ;
  published
  end ;

  TtiAttrColMap = class( TtiObject )
  private
    FAttrMap: TtiAttrMap;
    FDBColMap: TtiDBColMap;
  protected
    function    GetCaption : string ; override ;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
  public
    property    Owner : TtiClassDBMappingMgr read GetOwner write SetOwner ;
    property    AttrMap : TtiAttrMap read FAttrMap write FAttrMap ;
    property    DBColMap : TtiDBColMap read FDBColMap write FDBColMap ;
  end ;

  TtiClassDBCollections = class( TtiObjectList )
  protected
    function    GetItems(i: integer): TtiClassDBCollection; reintroduce ;
    procedure   SetItems(i: integer; const Value: TtiClassDBCollection); reintroduce ;
    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
  public
    property    Items[i:integer] : TtiClassDBCollection read GetItems write SetItems ;
    procedure   Add( pObject : TtiClassDBCollection   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner ;
    function    AddClassCollectionMapping( const pCollectionClass : TPerObjListClass ;
                                           const pPerObjAbsClass : TtiClass ) : TtiClassDBCollection ; overload ;
    function    FindByCollectionOf( const pClass: TtiClass): TtiClassDBCollection;
    procedure   FindByCollection( const pClass: TtiClass ; const pList : TList );
    function    IsCollection( const pClass : TtiClass): boolean ;
    function    IsInCollection( const pClass : TtiClass ) : boolean ;
  published
  end ;

  TtiClassDBCollection = class( TtiObject )
  private
    FPerObjAbsClass  : TtiClass;
    FCollectionClass : TPerObjListClass;
//    FForeignKeyCols  : TStringList ;
    FOwnerAttrMaps   : TtiAttrColMaps ;
  protected
    function    GetOwner: TtiClassDBCollections; reintroduce ;
    procedure   SetOwner(const Value: TtiClassDBCollections); reintroduce ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Owner       : TtiClassDBCollections             read GetOwner      write SetOwner ;
    property    CollectionClass : TPerObjListClass read FCollectionClass write FCollectionClass ;
    property    PerObjAbsClass  : TtiClass read FPerObjAbsClass write FPerObjAbsClass ;
    property    OwnerAttrMaps   : TtiAttrColMaps read FOwnerAttrMaps ;
  end ;

//  TtiForeignKeyMaps = class( TtiObjectList )
//  private
//  protected
//    function    GetItems(i: integer): TtiForeignKeyMap ; reintroduce ;
//    procedure   SetItems(i: integer; const Value: TtiForeignKeyMap); reintroduce ;
//    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
//    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
//  public
//    property    Items[i:integer] : TtiForeignKeyMap read GetItems write SetItems ;
//    procedure   Add( pObject : TtiForeignKeyMap   ; pbDefaultDispOrder : boolean = true ) ; reintroduce ;
//    property    Owner : TtiClassDBMappingMgr read GetOwner      write SetOwner ;
//    procedure   AddForeignKeyMap( const pCollectionOfClass : TPerObjListClass ;
//                                  const pFKProperty : string ;
//                                  const pFKField : string ) ;
//
////    procedure   AddMapping( const pAttrMap : TtiAttrMap ; const pColMap : TtiDBColMap ) ;
////    procedure   FindAllMappingsByMapToClass( const pClass : TtiClass ;
////                                             const pList : TtiAttrColMaps ) ;
////    procedure   FindAllPKMappingsByMapToClass( const pClass : TtiClass ;
////                                               const pList : TtiAttrColMaps ) ;
////    function    FindByClassAttrMap( const pClass : TtiClass ;
////                                    const pAttrName : string ) : TtiAttrColMap ;
////    function    TableName : string ;
//  published
//  end ;

//  TtiForeignKeyMap = class( TtiObject )
//  private
////    FAttrMap: TtiAttrMap;
////    FDBColMap: TtiDBColMap;
//  protected
//    function    GetOwner: TtiClassDBMappingMgr; reintroduce ;
//    procedure   SetOwner(const Value: TtiClassDBMappingMgr); reintroduce ;
//  public
//    property    Owner : TtiClassDBMappingMgr read GetOwner write SetOwner ;
////    property    AttrMap : TtiAttrMap read FAttrMap write FAttrMap ;
////    property    DBColMap : TtiDBColMap read FDBColMap write FDBColMap ;
//  end ;

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
  result := TtiAttrColMap.Create ;
  result.DBColMap := pDBColMap ;
  result.AttrMap  := pAttrMap ;
  FAttrColMaps.Add( result ) ;
end;
}

{
function TtiClassDBMappingMgr.AddClassMap(
  const pClass: TtiClass): TtiClassMap;
begin
  result := TtiClassMap.Create ;
  result.PerObjAbsClass := pClass ;
  FClassMaps.Add( result ) ;
end;
}

{
function TtiClassDBMappingMgr.AddDBMap( const pDatabaseName : string ) : TtiDBMap ;
begin
  result := TtiDBMap.Create ;
  result.DatabaseName := pDatabaseName ;
  FDBMaps.Add( result ) ;
end;
}

procedure TtiClassDBMappingMgr.RegisterMapping(
    const pDatabaseName : string ;
    const pClass        : TtiClass ;
    const pTableName    : string ;
    const pAttrName     : string ;
    const pColName      : string ;
    const pPKInfo       : TPKInfo = [] ) ;
var
  lDBMap : TtiDBMap ;
  lDBTableMap : TtiDBTableMap ;
  lDBColMap   : TtiDBColMap ;
  lClassMap   : TtiClassMap ;
  lAttrMap    : TtiAttrMap  ;
begin
  lDBMap      := FDBMaps.FindCreate(      pDatabaseName ) ;
  lDBTableMap := lDBMap.FindCreate(       pTableName ) ;
  lDBColMap   := lDBTableMap.AddColMap(  pColName, pPKInfo ) ;

  lClassMap   := FClassMaps.FindCreate(   pClass ) ;
  lAttrMap    := lClassMap.AddAttrMap(    pAttrName ) ;
  lAttrMap.ObjectState := posClean ;
  FAttrColMaps.AddMapping( lAttrMap, lDBColMap ) ;
end;

procedure TtiClassDBMappingMgr.RegisterMapping(
    const pClass        : TtiClass ;
    const pTableName    : string ;
    const pAttrName     : string ;
    const pColName      : string ;
    const pPKInfo       : TPKInfo = [] ) ;
begin
  // ToDo: This will load before a persistence layer is loaded, so the call to
  //       DefautlDBConnectionName is invalid.
  RegisterMapping(
    '',{TtiOPFManager( Owner ).DefaultDBConnectionName,}
    pClass,
    pTableName,
    pAttrName,
    pColName,
    pPKInfo ) ;
end;

constructor TtiClassDBMappingMgr.Create;
begin
  inherited;
  FAttrColMaps := TtiAttrColMaps.Create ;
  FAttrColMaps.Owner := self ;
  FAttrColMaps.ItemOwner := self ;

  FClassMaps   := TtiClassMaps.Create ;
  FClassMaps.Owner     := self ;
  FClassMaps.ItemOwner := self ;

  FDBMaps      := TtiDBMaps.Create ;
  FDBMaps.Owner := self ;
  FDBMaps.ItemOwner := self ;

  FCollections := TtiClassDBCollections.Create ;
  FCollections.Owner := self ;
  FCollections.ItemOwner := self ;

//  FForeignKeyMaps := TtiForeignKeyMaps.Create ;
//  FForeignKeyMaps.Owner := self ;
//  FForeignKeyMaps.ItemOwner := self ;

end;

destructor TtiClassDBMappingMgr.destroy;
begin
  FAttrColMaps.Free ;
  FClassMaps.Free ;
  FDBMaps.Free ;
  FCollections.Free ;
  inherited;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiClassMaps
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiClassMaps.Add(pObject: TtiClassMap;pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiClassMaps.AddClassMap( const pClass: TtiClass): TtiClassMap;
begin
  if FindByPerObjAbsClass( pClass ) <> nil then
    raise Exception.Create(
                 'Attempt to register duplicate TtiClassMap' + Cr +
                 'Classname: ' + pClass.ClassName + Cr +
                 'Called in ' + ClassName + '.AddClassMap' ) ;
//    tiTermError( 'Attempt to register duplicate TtiClassMap' + Cr +
//                 'Classname: ' + pClass.ClassName + Cr +
//                 'Called in ' + ClassName + '.AddClassMap' ) ;

  result := TtiClassMap.Create ;
  result.PerObjAbsClass := pClass ;
  result.ObjectState := posClean ;
  Add( result ) ;
end;

procedure TtiClassMaps.FindAllParents(const pClass: TtiClass; const pList: TtiClassMaps);
var
  lClassMap : TtiClassMap ;
begin
  Assert( not pList.OwnsObjects, 'pList.OwnsObjects is true and it should be false' ) ;
  pList.Clear ;
  lClassMap := FindByPerObjAbsClass( pClass ) ;
  Assert( lClassMap <> nil,
         'Request to find parent on class that is not registered <' +
         pClass.ClassName + '>' ) ;
  pList.Insert( 0, lClassMap ) ;
  while ( lClassMap<>nil )and
        ( lClassMap.ParentClassMap <> nil ) do
  begin
    lClassMap := lClassMap.ParentClassMap;
    if lClassMap <> nil then
      pList.Insert( 0, lClassMap ) ;
  end ;
end;

function TtiClassMaps.FindByPerObjAbsClass( const pClass: TtiClass): TtiClassMap;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if Items[i].PerObjAbsClass = pClass then
    begin
      result := Items[i] ;
      Exit ; //==>
    end ;
end;

function TtiClassMaps.FindCreate( const pClass: TtiClass): TtiClassMap;
begin
  result := FindByPerObjAbsClass( pClass ) ;
  if result = nil then
    result := AddClassMap( pClass ) ;
end;

function TtiClassMaps.FindParent( const pClass: TtiClass): TtiClassMap;
var
  lClassMap : TtiClassMap ;
begin
  lClassMap := FindByPerObjAbsClass( pClass ) ;
  Assert( lClassMap <> nil,
          'Attempt to find parent on un-registered class <' +
          pClass.ClassName + '>' ) ;
  result := lClassMap.ParentClassMap ;
end;

function TtiClassMaps.GetItems(i: integer): TtiClassMap;
begin
  result := TtiClassMap( inherited GetItems( i )) ;
end;

function TtiClassMaps.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr( inherited GetOwner ) ;
end;

function TtiClassMaps.HasParent( const pClass: TtiClass): Boolean;
begin
  result := (FindParent( pClass ) <> nil) ;
end;

function TtiClassMaps.IsClassReg(const pClass: TtiClass): boolean;
begin
  result := FindByPerObjAbsClass( pClass ) <> nil ;
end;

procedure TtiClassMaps.RegisterInheritance( const pParentClass : TtiClass ;
                                            const pChildClass  : TtiClass );
var
  lClassMapChild : TtiClassMap ;
  lClassMapParent : TtiClassMap ;
begin

  if not pChildClass.InheritsFrom( pParentClass ) then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a child class <' +
      pChildClass.ClassName +
      '> that does not inherit from <' +
      pParentClass.ClassName + '>') ;

  lClassMapChild := FindByPerObjAbsClass( pChildClass ) ;
  if lClassMapChild = nil then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a child class <' +
      pChildClass.ClassName +
      '> that has not yet been registered');

  lClassMapParent := FindByPerObjAbsClass( pParentClass ) ;
  if lClassMapParent = nil then
    raise EtiOPFProgrammerException.Create(
      'Attempt to register inheritance on a parent class <'+
      pParentClass.ClassName +
      '> that has not yet been registered');

  lClassMapChild.ParentClassMap := lClassMapParent ;
end;

procedure TtiClassMaps.SetItems(i: integer; const Value: TtiClassMap);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiClassMaps.SetOwner(const Value: TtiClassDBMappingMgr);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiClassMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiClassMap.Add(pObject: TtiAttrMap; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiClassMap.AddAttrMap(const pAttrName: string): TtiAttrMap;
begin
  if FindByAttrName( pAttrName ) <> nil then
    raise exception.Create(
                 'Attempt to register duplicate TtiAttrMap' + Cr +
                 'ClassName: ' + PerObjAbsClass.ClassName + Cr +
                 'AttrName:  ' + pAttrName + Cr +
                 '. Called in ' + ClassName + '.AddAttrMap' ) ;
  if not _IsPublishedProp( pAttrName ) then
    raise exception.Create(
                 pAttrName + ' is not a published property on ' +
                 PerObjAbsClass.ClassName + Cr +
                 '. Called in ' + ClassName + '.AddAttrMap' ) ;
  result := TtiAttrMap.Create ;
  result.AttrName := pAttrName ;
  result.ObjectState := posClean ;
  Add( result ) ;
end;

function TtiClassMap._IsPublishedProp(const pAttrName: string): Boolean;
begin
  result :=
    _IsPublicProperty(pAttrName) or
    ( IsPublishedProp( PerObjAbsClass, pAttrName )) ;
end ;

function TtiClassMap._IsPublicProperty(const pAttrName: string): Boolean;
begin
  result :=
    SameText( pAttrName, 'OID' ) or
    SameText( pAttrName, 'Owner.OID' ) // Must do better than this
end;

function TtiClassMap.FindByAttrName(const pAttrName: string): TtiAttrMap;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].Attrname, pAttrName ) then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
end ;

function TtiClassMap.GetCaption: string;
begin
  result := FPerObjAbsClass.ClassName ;
end;

function TtiClassMap.GetItems(i: integer): TtiAttrMap;
begin
  result := TtiAttrMap( inherited GetItems( i )) ;
end;

function TtiClassMap.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr( inherited GetOwner ) ;
end;

procedure TtiClassMap.SetItems(i: integer; const Value: TtiAttrMap);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiClassMap.SetOwner(const Value: TtiClassDBMappingMgr);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBMaps
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBMaps.Add(pObject: TtiDBMap; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiDBMaps.FindCreate(const pDatabaseName: string): TtiDBMap;
begin
  result := FindByDatabaseName( pDatabaseName ) ;
  if result = nil then
    result := AddDBMap( pDatabaseName ) ;
end;

function TtiDBMaps.FindByDatabaseName(const pDatabaseName: string): TtiDBMap;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].DatabaseName, pDatabaseName ) then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
end ;

function TtiDBMaps.GetItems(i: integer): TtiDBMap;
begin
  result := TtiDBMap( inherited GetItems( i )) ;
end;

function TtiDBMaps.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr( inherited GetOwner ) ;
end;

procedure TtiDBMaps.SetItems(i: integer; const Value: TtiDBMap);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDBMaps.SetOwner(const Value: TtiClassDBMappingMgr);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBMap.Add(pObject: TtiDBTableMap; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiDBMap.AddTableMap(const pTableName: string): TtiDBTableMap;
begin
  if FindByTableName( pTableName ) <> nil then
    raise exception.Create(
                 'Attempt to register duplicate TtiDBTableMap' + Cr +
                 'DatabaseName: ' + DatabaseName + Cr +
                 'TableName:    ' + pTableName + Cr +
                 'Called in ' + ClassName + '.AddTableMap' ) ;
//    tiTermError( 'Attempt to register duplicate TtiDBTableMap' + Cr +
//                 'DatabaseName: ' + DatabaseName + Cr +
//                 'TableName:    ' + pTableName + Cr +
//                 'Called in ' + ClassName + '.AddTableMap' ) ;
  result := TtiDBTableMap.Create ;
  result.TableName := pTableName ;
  result.ObjectState := posClean ;
  Add( result ) ;
end;

function TtiDBMap.FindCreate(const pTableName: string): TtiDBTableMap;
begin
  result := FindByTableName( pTableName ) ;
  if result = nil then
    result := AddTableMap( pTableName ) ;
end;

function TtiDBMap.FindByTableName(const pTableName: string): TtiDBTableMap;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].TableName, pTableName ) then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
end ;

function TtiDBMap.GetItems(i: integer): TtiDBTableMap;
begin
  result := TtiDBTableMap( inherited GetItems( i )) ;
end;

function TtiDBMap.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr( inherited GetOwner ) ;
end;

procedure TtiDBMap.SetItems(i: integer; const Value: TtiDBTableMap);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDBMap.SetOwner(const Value: TtiClassDBMappingMgr);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBColMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiDBColMap.GetCaption: string;
begin
  result := FColName ;
end;

function TtiDBColMap.GetOwner: TtiDBTableMap;
begin
  result := TtiDBTableMap( inherited GetOwner ) ;
end;

procedure TtiDBColMap.SetOwner(const Value: TtiDBTableMap);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiDBTableMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiDBTableMap.Add(pObject: TtiDBColMap; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiDBTableMap.AddColMap(const pColName: string ; pPKInfo : TPKInfo ): TtiDBColMap;
begin
  result := FindByColName( pColName ) ;
  if result <> nil then
    Exit ; //==>                     
//    raise exception.Create(
//                 'Attempt to register duplicate TtiDBColMap' + Cr +
//                 'Table name: ' + TableName + Cr +
//                 'Col name:   ' + pColName + Cr +
//                 'Called in ' + ClassName + '.AddColMap') ;

  result := TtiDBColMap.Create ;
  result.ColName := pColName ;
  result.PKInfo  := pPKInfo ;
  result.ObjectState := posClean ;
  Add( result ) ;
end;

//function TtiDBTableMap.FindCreate(const pColName: string ; pPKInfo : TPKInfo ): TtiDBColMap;
//begin
//  result := FindByColName( pColName ) ;
//  if result = nil then
//    result := AddColMap( pColName, pPKInfo ) ;
//end;

function TtiDBTableMap.FindByColName( const pColName : string ) : TtiDBColMap ;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].ColName, pColName ) then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
end ;

function TtiDBTableMap.GetCaption: string;
begin
  result := FTableName ;
end;

function TtiDBTableMap.GetItems(i: integer): TtiDBColMap;
begin
  result := TtiDBColMap( inherited GetItems( i )) ;
end;

function TtiDBTableMap.GetOwner: TtiDBMap;
begin
  result := TtiDBMap( inherited GetOwner ) ;
end;

procedure TtiDBTableMap.SetItems(i: integer; const Value: TtiDBColMap);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiDBTableMap.SetOwner(const Value: TtiDBMap);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiAttrColMaps
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TtiAttrColMaps.Add(pObject: TtiAttrColMap; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

procedure TtiAttrColMaps.AddMapping(const pAttrMap: TtiAttrMap;
  const pColMap: TtiDBColMap);
var
  lData : TtiAttrColMap ;
begin
  // ToDo: Check we are not adding duplicates in an assert( )
  lData := TtiAttrColMap.Create ;
  lData.DBColMap := pColMap ;
  lData.AttrMap := pAttrMap ;
  lData.ObjectState := posClean ;
  Add( lData ) ;
end;

procedure TtiAttrColMaps.FindAllMappingsByMapToClass(
  const pClass: TtiClass;
  const pList: TtiAttrColMaps );
var
  i : integer ;
begin
  Assert( not pList.OwnsObjects, 'pList.OwnsObjects is true and it should be false' ) ;
  pList.Clear ;
  for i := 0 to Count - 1 do
    if Items[i].AttrMap.Owner.PerObjAbsClass = pClass then
      pList.Add( Items[i] ) ;
end;

procedure TtiAttrColMaps.FindAllPKMappingsByMapToClass(
  const pClass: TtiClass; const pList: TtiAttrColMaps);
var
  i : integer ;
begin
  Assert( not pList.OwnsObjects, 'pList.OwnsObjects is true and it should be false' ) ;
  pList.Clear ;
  for i := 0 to Count - 1 do
    if ( Items[i].AttrMap.Owner.PerObjAbsClass = pClass ) and
       ( pktDB in Items[i].DBColMap.PKINfo ) then
      pList.Add( Items[i] ) ;
end;

function TtiAttrColMaps.FindByClassAttrMap(const pClass: TtiClass; const pAttrName: string): TtiAttrColMap;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if ( Items[i].AttrMap.Owner.PerObjAbsClass = pClass ) and
       ( SameText( Items[i].AttrMap.AttrName, pAttrName )) then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
end;

function TtiAttrColMaps.GetItems(i: integer): TtiAttrColMap;
begin
  result := TtiAttrColMap( inherited GetItems( i )) ;
end;

function TtiAttrColMaps.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr( inherited GetOwner ) ;
end;

procedure TtiAttrColMaps.SetItems(i: integer; const Value: TtiAttrColMap);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiAttrColMaps.SetOwner(const Value: TtiClassDBMappingMgr);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiAttrMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiAttrMap.GetOwner: TtiClassMap;
begin
  result := TtiClassMap( inherited GetOwner ) ;
end;

procedure TtiAttrMap.SetOwner(const Value: TtiClassMap);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtiAttrColMap
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TtiAttrColMap.GetCaption: string;
begin
  result := FAttrMap.Caption + '/' + FDBColMap.Caption ;
end;

function TtiAttrColMap.GetOwner: TtiClassDBMappingMgr;
begin
    result := TtiClassDBMappingMgr( inherited GetOwner ) ;
end;

procedure TtiAttrColMap.SetOwner(const Value: TtiClassDBMappingMgr);
begin
  inherited SetOwner( Value ) ;
end;

function TtiDBMaps.AddDBMap(const pDatabaseName: string): TtiDBMap;
begin
  if FindByDatabaseName( pDatabaseName ) <> nil then
    raise exception.Create(
                 'Attempt to register duplicate TtiDBMap' + Cr +
                 'DatabaseName: ' + pDatabaseName + Cr +
                 'Called in ' + ClassName + 'AddDBMap' ) ;
//    tiTermError( 'Attempt to register duplicate TtiDBMap' + Cr +
//                 'DatabaseName: ' + pDatabaseName + Cr +
//                 'Called in ' + ClassName + 'AddDBMap' ) ;
  result := TtiDBMap.Create ;
  result.DatabaseName := pDatabaseName ;
  result.ObjectState := posClean ;
  Add( result ) ;
end;

procedure TtiClassDBMappingMgr.RegisterCollection(
  const pCollectionClass: TPerObjListClass;
  const pCollectionOfClass: TtiClass);
begin
  FCollections.AddClassCollectionMapping( pCollectionClass,
                                          pCollectionOfClass ) ;
end;

{ TtiClassDBCollections }

procedure TtiClassDBCollections.Add(pObject: TtiClassDBCollection; pbDefaultDispOrder: boolean);
begin
  inherited Add( pObject, pbDefaultDispOrder ) ;
end;

function TtiClassDBCollections.AddClassCollectionMapping(
  const pCollectionClass: TPerObjListClass;
  const pPerObjAbsClass: TtiClass ) : TtiClassDBCollection ;
begin
  result := TtiClassDBCollection.Create ;
  result.CollectionClass := pCollectionClass ;
  result.PerObjAbsClass  := pPerObjAbsClass ;
  result.ObjectState := posClean ;
  Add( result ) ;
end;

procedure TtiClassDBCollections.FindByCollection(
  const pClass: TtiClass ;
  const pList : TList );
var
  i : integer ;
begin
  pList.Clear ;
  for i := 0 to Count - 1 do
    if Items[i].CollectionClass = pClass then
      pList.Add( Items[i] );
end;

function TtiClassDBCollections.FindByCollectionOf( const pClass: TtiClass): TtiClassDBCollection;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if Items[i].PerObjAbsClass = pClass then
    begin
      result := Items[i] ;
      Exit ; //==>
    end ;
end;

function TtiClassDBCollections.GetItems(i: integer): TtiClassDBCollection;
begin
  result := TtiClassDBCollection( inherited GetItems( i )) ;
end;

function TtiClassDBCollections.GetOwner: TtiClassDBMappingMgr;
begin
  result := TtiClassDBMappingMgr( inherited GetOwner ) ;
end;

function TtiClassDBCollections.IsInCollection( const pClass: TtiClass): boolean;
begin
  result := FindByCollectionOf( pClass ) <> nil ;
end;

function TtiClassDBCollections.IsCollection( const pClass: TtiClass): boolean;
var
  lList : TList ;
begin
  lList := TList.Create ;
  try
    FindByCollection( pClass, lList );
    result := lList.Count > 0 ;
  finally
    lList.Free ;
  end ;
end;

procedure TtiClassDBCollections.SetItems(i: integer; const Value: TtiClassDBCollection);
begin
  inherited SetItems( i, Value ) ;
end;

procedure TtiClassDBCollections.SetOwner( const Value: TtiClassDBMappingMgr);
begin
  inherited SetOwner( Value ) ;
end;

{ TtiClassDBCollection }

constructor TtiClassDBCollection.Create;
begin
  inherited;
//  FForeignKeyCols := TStringList.Create ;
  FOwnerAttrMaps  := TtiAttrColMaps.Create ;
  FOwnerAttrMaps.OwnsObjects := false ;
end;

destructor TtiClassDBCollection.Destroy;
begin
  FOwnerAttrMaps.Free ;
//  FForeignKeyCols.Free ;
  inherited;
end;

function TtiClassDBCollection.GetOwner: TtiClassDBCollections;
begin
  result := TtiClassDBCollections( inherited GetOwner ) ;
end;

procedure TtiClassDBCollection.SetOwner(const Value: TtiClassDBCollections);
begin
  inherited SetOwner( Value ) ;
end;

function TtiAttrColMaps.TableName: string;
var
  i : integer;
begin
  result := '' ;
  for i := 0 to Count - 1 do
  begin
    if result = '' then
      result := Items[i].DBColMap.Owner.TableName ;
    if result <> Items[i].DBColMap.Owner.TableName then
      raise EtiOPFProgrammerException.Create(cErrorInconsistentTableNames) ;
  end ;
end;

procedure TtiClassDBMappingMgr.RegisterInheritance( const pParentClass : TtiClass ;
                                                    const pChildClass: TtiClass);
begin
  FClassMaps.RegisterInheritance( pParentClass, pChildClass ) ;
end;

end.
