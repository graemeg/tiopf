{:@summary The base unit for tiOPF.
   @desc Contains all the base classes used throughout the framework.}

unit tiObject ;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiVisitor
  ,tiOID
  ,Classes
  ,TypInfo
  ,Contnrs
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,SyncObjs   // This must come after the Windows unit!
  ;

const

  cErrorFieldNotAssigned = 'Field <%s> is null. OID=%d';
  cErrorFieldTooLong = 'Field <%s> field too long. Allowed length %d, current length %d';
  cErrorUnableToDetermineFieldName = 'Unable to determine field name on <%s>';
  cErrorTIPerObjAbsTestValid = 'tiOPF Internal Error: TtiBaseObject.TestValid failed' ;
  cErrorSettingProperty      = 'Error setting property %s.%s Message %s';
  cErrorGettingProperty      = 'Error getting property %s.%s Message %s';
  cErrorInvalidObjectState   = 'Invalid ObjectState';
  cErrorNoFindMethodAssigned = 'No find method assigned';
  cErrorAttemptToSetNonPublishedProperty = 'Attempt to set non-published property %s.%s to %s';
  cErrorInvalidSortType = 'Invalid TtiPerObjListSortType';

type
  {: The possible states of a TtiObjector descendant in memory.
   @enum posEmpty The object has been created, but not filled with data from the DB
   @enum posPK The object has been created, but only it's primary key has been read
   @enum posCreate The object has been created and populated with data and must be saved to the DB
   @enum posUpdate The object has been changed, the DB must be updated
   @enum posDelete The object has been deleted, it must be deleted from the DB
   @enum posDeleted The object was marked for deletion, and has been deleted in the database
   @enum posClean The object is 'Clean' no DB update necessary
   }
  TPerObjectState = (
                      posEmpty,
                      posPK,
                      posCreate,
                      posUpdate,
                      posDelete,
                      posDeleted,
                      posClean
                     ) ;

  TtiObject = class ;
  TtiObjectList = class ;
  TPerObjErrors = class ;
  TPerObjError  = class ;

  TPerObjFindMethod = procedure( pPerObjAbs : TtiObject; var pbFound : boolean ) of object ;
  TPerObjFindMethodExt = procedure( pPerObjAbs : TtiObject; var pbFound : boolean; pUserContext: Pointer ) of object ;
  TPerObjFindMethodData = procedure( pPerObjAbs : TtiObject; var pbFound : boolean; pData : TtiObject ) of object ;
  TPerObjForEachMethod        = procedure( pPerObjAbs : TtiObject) of object ;
  TPerObjForEachMethodRegular = procedure( pPerObjAbs : TtiObject) ;
  TtiObjectEvent = procedure( const pData: TtiObject ) of object ;

{
  // Template for creating TPerVisList and TtiObjectstubs...
  TMyClasses = class ;
  TMyClass   = class ;

  TMyClasses = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TMyClass ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TMyClass); reintroduce ;
    function    GetOwner: TMyClasses; reintroduce ;
    procedure   SetOwner(const Value: TMyClasses); reintroduce ;
  public
    property    Items[i:integer] : TMyClass read GetItems write SetItems ;
    procedure   Add( pObject : TMyClass   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    property    Owner : TMyClass read GetOwner      write SetOwner ;
  published
  end ;

  TMyClass = class( TtiObject)
  private
  protected
    function    GetOwner: TMyClasses; reintroduce ;
    procedure   SetOwner(const Value: TMyClasses ); reintroduce ;
  public
    property    Owner       : TMyClasses             read GetOwner      write SetOwner ;
  published
  end ;
}

  {: Does the field allow null values. This is one of the possibly many tests
    that the field will make when the TestValidValue function is called. }
  TtiNullValidation = (
                        nvAllowNull = 0
                       ,nvNotNull = 1
                      );

  {: Is the TtiObjectList sorted by OID or not sorted at all?}
  TtiPerObjListSortType =
                       (  stNone
                         ,stOID
                       );


  {: Abstract persistent field for use with TtiObject The usual way to add
     data to TtiObjectis with published properties. These can be simple data
     types (String, Integer, Float, DateTime) or object data types that you
     define. Simple data types will not handle NULL values so if NULL management
     is required, you should use persistent fields. Using persistent fields
     however, makes the code more complex and harder to maintain.}
  TtiFieldAbs = class( TtiBaseObject )
  private
    FOwner: TtiObject;
    FFieldName: string;
    FNullValidation: TtiNullValidation;
    FIsNull: Boolean ;
    procedure SetIsNull(const Value: Boolean);
  protected
    procedure   Clear; virtual ;
    procedure   SetValue; virtual ;
    procedure   SetAsString(const pValue: string); virtual ; abstract ;
    function    GetAsString: string;               virtual ; abstract ;
    function    GetFieldName: string; virtual ;
  public
    constructor Create(const pOwner: TtiObject); overload ; virtual ;
    constructor Create(const pOwner: TtiObject; const pNullValidation: TtiNullValidation); overload ; virtual ;
    function    IsValidValue(const pErrors : TPerObjErrors = nil): Boolean ; virtual ;
    function    Equals(pCompareWith: TtiFieldAbs): Boolean ; virtual ; abstract ;

    property    Owner:            TtiObject        read FOwner ;
    property    FieldName:        string            read GetFieldName;
    property    NullValidation:   TtiNullValidation read FNullValidation write FNullValidation;
    property    AsString:         string            read GetAsString Write SetAsString;
    property    IsNull:           Boolean           read FIsNull write SetIsNull;
  end ;


  {: Concrete persistent string field}
  TtiFieldString = class(TtiFieldAbs)
  private
    FValue: string;
    FMaxLength: Integer;
  protected
    procedure   Clear; override ;
    procedure   SetAsString(const pValue: string); override ;
    function    GetAsString: string;              override ;
  public
    // A maximum length of 0 (zero) means no maximum length
    constructor Create(const pOwner: TtiObject;
                       const pNullValidation: TtiNullValidation = nvAllowNull;
                       const pMaxLength: Integer = 0); reintroduce; overload ;
    function    IsValidValue(const pErrors : TPerObjErrors = nil): Boolean ; override ;
    function    Equals(pCompareWith: TtiFieldAbs): Boolean ; override ;

    property    MaxLength: Integer read FMaxLength;
  end;


  {: Concrete persistent Int64 field}
  TtiFieldInteger = class(TtiFieldAbs)
  private
    FValue: Int64;
    FMaxDigits: Integer;
    procedure   SetAsInteger(const pValue: Int64);
  protected
    procedure   Clear; override ;
    procedure   SetAsString(const pValue: string);  override ;
    function    GetAsString: string;               override ;
  public
    // A maximum digits of 0 (zero) means no maximum digits
    constructor Create(const pOwner: TtiObject;
                       const pNullValidation: TtiNullValidation = nvAllowNull;
                       const pMaxDigits: Integer = 0); reintroduce; overload ;
    function    IsValidValue(const pErrors : TPerObjErrors = nil): Boolean ; override ;
    function    Equals(pCompareWith: TtiFieldAbs): Boolean ; override ;

    property    AsInteger : Int64 read FValue Write SetAsInteger;
    property    MaxDigits : Integer read FMaxDigits;
  end;


  {: Concrete persistent float field}
  TtiFieldFloat = class(TtiFieldAbs)
  private
    FValue: Extended;
    FPrecision: Integer;
    FEpsilon: Extended;
  protected
    procedure   Clear; override ;
    procedure   SetAsString(const pValue: string);  override ;
    function    GetAsString: string;               override ;
    procedure   SetAsFloat(const pValue: Extended); virtual ;
    procedure   SetPrecision(const Value: Integer); virtual ;
  public
    constructor Create(const pOwner: TtiObject); overload; override;
    constructor Create(const pOwner: TtiObject;
                       const pNullValidation: TtiNullValidation); overload; override;
    constructor Create(const pOwner: TtiObject;
                       const pNullValidation: TtiNullValidation;
                       const pPrecision: Integer); reintroduce; overload ;
    function    Equals(pCompareWith: TtiFieldAbs): Boolean ; override ;
    property    AsFloat: Extended read FValue Write SetAsFloat;
    property    Precision: Integer Read FPrecision Write SetPrecision;
    property    Epsilon: Extended Read FEpsilon Write FEpsilon;
  end;


  {: Concrete persistent boolean field}
  TtiFieldBoolean = class(TtiFieldAbs)
  private
    FValue: Boolean;
    procedure   SetAsBoolean(const pValue: Boolean);
  protected
    procedure   Clear; override ;
    procedure   SetAsString(const pValue: string);  override ;
    function    GetAsString: string;               override ;
  public
    function    Equals(pCompareWith: TtiFieldAbs): Boolean ; override ;
    property    AsBoolean: Boolean read FValue Write SetAsBoolean;
  end;


  {: Concrete persistent TDateTime field}
  TtiFieldDateTime = class(TtiFieldAbs)
  private
    FValue: TDateTime;
    procedure   SetAsDateTime(const pValue: TDateTime);
  protected
    procedure   Clear; override ;
    procedure   SetAsString(const pValue: string);  override ;
    function    GetAsString: string;               override ;
  public
    function    Equals(pCompareWith: TtiFieldAbs): Boolean ; override ;
    property    AsDateTime: TDateTime read FValue Write SetAsDateTime;
  end;

  
  TtiFieldList = class(TObjectlist)
  protected
    function  GetItem(Index: Integer): TtiFieldAbs;
    procedure SetItem(Index: Integer; AObject: TtiFieldAbs);
  public
    constructor Create;
    function Add(AObject: TtiFieldAbs): Integer;
    property Items[Index: Integer]: TtiFieldAbs read GetItem write SetItem; default;
  end;


  TtiObject = class( TtiVisited )
  private
    FOID : TOID ;
    FObjectState : TPerObjectState ;
    FObserverList: TList;
    FUpdateCount: Integer;
    function GetObserverList: TList;
  protected
    FOwner: TtiObject; // Want FOwner here as there are time when we may want
                        // go get access without going through the GetOwner and
                        // SetOwner as these may have been typecase in a way
                        // which causes incompatabilities.
    function    GetDeleted: boolean; virtual ;
    procedure   SetDeleted(const Value: boolean); virtual ;
    procedure   SetDirty(const Value: boolean); virtual ;
    procedure   SetObjectState(const pValue: TPerObjectState) ; virtual ;
    function    GetOID: TOID; virtual ;
    procedure   SetOID(const Value: TOID); virtual;
    function    GetDirty : boolean ; virtual ;
    function    GetOwner: TtiObject; reintroduce ; virtual ;
    procedure   SetOwner(const Value: TtiObject); virtual ;
    procedure   AssignPublicProps(pSource: TtiObject);virtual ;
    procedure   AssignPublishedProp(pSource: TtiObject; psPropName: string);
    function    CountPropsByType(pSource: TtiObject; pPropFilter: TTypeKinds): integer;
    procedure   AssignPublishedProps(pSource: TtiObject; pPropFilter: TTypeKinds = [] );
    // You must override this in the concrete if there are class properties
    procedure   AssignClassProps(pSource: TtiObject); virtual ;
    function    GetIndex : integer ;
//    function    GetDispOrder: integer; virtual ;
    procedure   DoFindAllNotUnique( pPerObjAbs: TtiObject; var pbFound: boolean; pData: TtiObject ); virtual;
    function    GetPropValue(const pPropName: string): Variant; virtual ;
    procedure   SetPropValue(const pPropName: string; const pPropValue: Variant); virtual ;

  public
    {: Creates a new instance of the class}
    constructor Create  ; override ;
    {: Creates a new instance of the class and initialises it's OID with next available value}
    constructor CreateNew( const pOwner : TtiObject ; const pDatabaseName : string = '' ; const pPerLayerName : string = '' ) ; overload ; virtual ;
    constructor CreateNew( const pDatabaseName : string = '' ; const pPerLayerName : string = '' ) ; overload ; virtual ;
    destructor  Destroy ; override ;
    {: Dos this object equal another? }
    function    Equals( const pData : TtiObject ) : boolean ; virtual ;
    {: The OID of this object }
   {$IFDEF OID_AS_INT64}
      property    OID         : TOID                   read GetOID write SetOID ;
   {$ELSE}
      property    OID         : TOID                   read GetOID        ;
   {$ENDIF}
//    {: The display order of this object}
//    property    DispOrder   : integer                read GetDispOrder  write FiDispOrder ;
    {: The current state of this object}
    property    ObjectState: TPerObjectState read FObjectState write SetObjectState;
    {: The type of class that owns this object}
    property    Owner: TtiObject read GetOwner write SetOwner ;
    {: Returns the value of the property specified in pPropName.}
    property    PropValue[const pPropName: string]: Variant read GetPropValue write SetPropValue;
    {: Is the specified property read and write?}
    function    IsReadWriteProp( const pPropName : string ) : boolean ; virtual ;
    {: Return the TtiTypeKind (simple property type) of the property}
    function    PropType(const pPropName: string): TtiTypeKind;
    {: Is this object deleted? }
    property    Deleted : boolean                    read GetDeleted    write SetDeleted ;
    {: Is this object out of sync with the persistence layer?}
    property    Dirty   : boolean                    read GetDirty      write SetDirty ;

    property    Index : integer                      read GetIndex ;
    {: Returns this objects state as a string value.}
    function    ObjectStateAsString : string ;
    {: Find an object in the hierarchy by OID with the OID passed as a string}
    function    Find( pOIDToFindAsString : string ) : TtiObject ;  overload ; virtual ;
    {: Find an object in the hierarchy by OID with the OID passed as a TOID object}
    function    Find( pOIDToFind : TOID ) : TtiObject ;  overload ; virtual ;
    {: Find an object in the hierarchy using the find method passed}
    function    Find( pPerObjFindMethod : TPerObjFindMethod ) : TtiObject ; overload ;
    {: Find an object in the hierarchy using the extended find method passed}
    function    Find( pPerObjFindMethodExt : TPerObjFindMethodExt; pUserContext: Pointer ) : TtiObject ; overload;
    {: Find all objects in the hierarchy using the find method passed}
    function    FindAll( pPerObjFindMethod : TPerObjFindMethod; pList : TList ) : integer ; overload ;
    {: Find all objects in the hierarchy using the find method passed}
    function    FindAll( pPerObjFindMethodExt : TPerObjFindMethodExt; pList : TList; pUserContext: Pointer ) : integer ; overload ;
    {: Find all objects in the hierarchy using the find method passed}
    function    FindAll( pPerObjFindMethodData : TPerObjFindMethodData; pList : TList; pData : TtiObject ) : integer ; overload ;
    {:Is this obect unique in the hierarchy?}
    function    IsUnique( const pPerObjAbs : TtiObject ) : boolean ; virtual ;
    {: Creates a cloned instance of this object. }
    function    Clone : TtiObject ; virtual ; // Must override and typecast if to be used
    {: Copy this object to another.}
    procedure   Assign( const pSource : TtiObject ) ; reintroduce ; virtual ;
    {: returns the object at the top of the hierarchy}
    function    TopOfHierarchy : TtiObject ; virtual ;
    {: Set every object in the hierarchy's ObjectState to pObjectState }
    procedure   SetAllObjectStates(const pObjectState: TPerObjectState); virtual;
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid(const pErrors: TPerObjErrors): boolean; overload; virtual;
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid(var pErrorMessage: string): boolean; overload; // Don't override this one
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid(const pStrings: TStrings): boolean; overload; // Don't override this one
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid(const pStrings: TStrings; pAppend: boolean): boolean; overload; // Don't override this one
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid: boolean; overload; // Don't override this one
    {: Read in the primary Key values only from the database for this object.}
    procedure   ReadPK(const pDBConnectionName: string; pPerLayerName: string = ''); overload; virtual;
    {: Read in the primary Key values only from the database for this object.}
    procedure   ReadPK; overload; virtual;
    {: Read this object, but no owned objects from the database }
    procedure   ReadThis(const pDBConnectionName: string; pPerLayerName: string = ''); overload; virtual;
    {: Read this object, but no owned objects from the database }
    procedure   ReadThis; overload;  virtual;
    {: Read this object, along with any owned objects from the database }
    procedure   Read(const pDBConnectionName: string; pPerLayerName: string = ''); overload; virtual;
    {: Read this object, along with any owned objects from the database }
    procedure   Read; overload;  virtual;
    {: Updates the database with the current property values for this object.}
    procedure   Save(const pDBConnectionName: string; pPerLayerName: string = ''); overload; virtual;
    procedure   Save; overload; virtual;

    procedure   AssignFieldList(var pFieldList: TtiFieldList);
    {: ForceAsCreate will get a new OID, and set ObjectState := posCreate}
    procedure   ForceAsCreate(const pDatabaseName : string = '' ; const pPerLayerName : string = '');
    {: Display the object tree as a string for debugging (will show all published properties.)}
    function    AsDebugString: string; virtual ;
    {:Assign the published property names to a TStringList}
//    procedure   GetPropNames(AList: TStringList; APropFilter: TtiPropTypes = []);
    {: Return the propery count filter by pPropFilter }
    function    PropCount(pPropFilter: TTypeKinds = ctkSimple ): integer;

    { Observer pattern implementation below }
    {: Attach a new observer }
    procedure   AttachObserver(pObserver: TtiObject); virtual;
    {: Detach a existing observer }
    procedure   DetachObserver(pObserver: TtiObject); virtual;
    {: Start a update process. This will allow us to update multiple things,
      before we notify the observers of the updates. }
    procedure   BeginUpdate;
    {: End a update process }
    procedure   EndUpdate;
    {: Only needed if performing a observing role }
    procedure   Update(pSubject: TtiObject); virtual;
    {: Notify all the attached observers about a change }
    procedure   NotifyObservers; virtual;
    {: Used to get access to the internal observer list. This has been surfaced
       so that the MGM List Views can atttach/detach observers to the selected
       object. Not a great way of doing it - we need a different design. }
    property    ObserverList: TList read GetObserverList write FObserverList;
  end ;


  TtiObjectListCompareEvent = procedure(AItem1, AItem2: TtiObject) of object ;


  TtiObjectList = class( TtiObject )
  private
    FList : TObjectList ;
    FItemOwner: TtiObject;
    FbAutoSetItemOwner: boolean;
    function    GetList: TList;
//    procedure   AssignDispOrder(pData: TtiObject);
    function    GetCountNotDeleted: integer;
    function    GetOwnsObjects: boolean;
    procedure   SetOwnsObjects(const Value: boolean);
    function    DoCompareByProps( pItem1   : Pointer ; pItem2   : Pointer ;
                const pSortProps : array of string; pAscendingOrder : Boolean = True) : integer ;
    procedure   QuickSortByProps(SortList: PPointerList; L, R: Integer;
                const pSortProps : array of string; pAscendingOrder : Boolean = True);
  protected
    function    GetCount: integer; virtual ;
    function    GetItems(i: integer): TtiObject ; virtual ;
    procedure   SetItems(i: integer; const Value: TtiObject); virtual ;
    procedure   SetItemOwner(const Value: TtiObject); virtual ;
    procedure   AssignPublicProps( pSource : TtiObject ) ; override ;
    procedure   AssignClassProps(pSource: TtiObject); override ;
    function    IndexOfBinary(AOIDToFind : TOID) : integer ; virtual;
    function    IndexOfFullScan(AOIDToFind : TOID) : integer ; virtual;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;

    procedure   Assign( const pSource : TtiObject ) ; override ;
    {: The number of items in the list.}
    property    Count : integer read GetCount ;
    {: The number of items in the list that are not marked as deleted.}
    property    CountNotDeleted : integer read GetCountNotDeleted ;
    property    Items[i:integer] : TtiObject read GetItems write SetItems ; default ;

    {: Does the list own the objects it contains? i.e. Will the objects be freed when the list is cleared/destroyed?}
    property    OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects ;
    property    ItemOwner      : TtiObject read FItemOwner    write SetItemOwner ;
    property    AutoSetItemOwner : boolean read FbAutoSetItemOwner write FbAutoSetItemOwner ;

    {: Finds the object in the list whose OID value matches.}
    function    Find( pOIDToFindAsString : string ) : TtiObject ;  override ;
    {: Finds the object in the list whose OID value matches.}
    function    Find( pOIDToFind : TOID ) : TtiObject ; override ;
    {: Finds the object in the list whose OID value matches. Faster search if sorted by OID. }
    function    Find( pOIDToFind : TOID; pSortType: TtiPerObjListSortType ) : TtiObject ; overload ;
    {: Finds the object in the list whose OID value matches. Will search the list, and if not found, will search all owned objects }
    function    FindInHierarchy( pOIDToFind : TOID) : TtiObject ; overload ;
    {: Performs the method pMethod on every object in the list.}
    procedure   ForEach( pMethod : TPerObjForEachMethod        ; pbIncludeDeleted : boolean = false ) ; overload ; virtual ;
    {: Performs the method pMethod on every object in the list.}
    procedure   ForEach( pMethod : TPerObjForEachMethodRegular ; pbIncludeDeleted : boolean = false ) ; overload ; virtual ;
    {: Add an object to the list.
       Don't override Add( pObject : TtiObject  ; pDefDispOrdr : boolean = true ),
       It's an old method for backward compatibility . Override Add(const pObject:TtiObject).}
    procedure   Add( pObject : TtiObject  ; pDefDispOrdr : Boolean ) ; overload ; virtual ;
    {: Add an object to the list.}
    procedure   Add( const pObject : TtiObject ) ; overload ; virtual ;
    {: Empty list and delete all owned objects}
    procedure   Clear ; virtual ;
    {: Empty list, but do not delete owned objects }
    procedure   Empty ; virtual ;
    {: The index of the specified object in the list.}
    function    IndexOf( pData : TObject ) : integer ; overload ; virtual ;
    {: The index of the object in the list whose OID value matches. Faster search if sorted by OID. }
    function    IndexOf( pOIDToFind : TOID; pSortType: TtiPerObjListSortType = stNone) : integer ; overload ; virtual ;
    {: The last object in the list.}
    function    Last  : TtiObject ; virtual ;
    {: The first object in the list.}
    function    First : TtiObject ; virtual ;
    {: Returns the first item in the list that hasn't been marked deleted.}
    function    FirstExcludeDeleted : TtiObject ; virtual ;
    {: Returns the last item in the list that hasn't been marked deleted.}
    function    LastExcludeDeleted : TtiObject ; virtual ;
    {: Removes the object at a specified position and (if OwnsObject is True) frees the object.}
    procedure   Delete( i : integer ) ; virtual ;
    {: Removes the specified item from the list and (if OwnsObject is True) frees the object.}
    function    Remove( pData : TtiObject ) : integer ; virtual ;
    {: Removes the specified object from the list without freeing the object.}
    procedure   Extract( pData : TtiObject ) ; virtual ;
    {: Adds an object to the list at the position specified by Index.}
    procedure   Insert( const piIndex : integer ; pData : TtiObject ) ; overload ; virtual ;
    procedure   Insert( pInsertBefore : TtiObject ; pData : TtiObject ) ; overload ; virtual ;
    {: Sets all items in the list as ready for deletion}
    procedure   MarkListItemsForDeletion ; virtual ;
    {: Sets all items in the list as needing updating to the database}
    procedure   MarkListItemsDirty ; virtual ;
    procedure   PropToStrings(const pStrings: TStrings ; const pPropName : string = 'caption' ); virtual ;
    {: Finds the first object whose properties passed in pProps match the values specified.}
    function    FindByProps( const pProps : array of string ;
                             const pVals  : array of variant;
                             pCaseSensitive : boolean = true  ) : TtiObject ; virtual ;
    {: Sorts the list by the properties specified}
    procedure   SortByProps( const pSortProps : array of string; pAscendingOrder : Boolean = True) ; virtual ;
    {: Sorts the list by each member's OID value.}
    procedure   SortByOID ; virtual ;

    {: Compare Self with AList. Fire an event for each object depending on the differences}
    procedure   CompareWith(AList: TtiObjectList;
                            AInBothAndEquals: TtiObjectListCompareEvent;
                            AInBothAndNotEquals: TtiObjectListCompareEvent;
                            AIn1Only: TtiObjectListCompareEvent;
                            AIn2Only: TtiObjectListCompareEvent); virtual;

    {: Compare Self with AList. Add each object to the appropriate list depending on the differences}
//    procedure   CompareWith(AList: TtiObjectList;
//                            AInBothAndEquals: TtiObjectList;
//                            AInBothAndNotEquals: TtiObjectList;
//                            AIn1Only: TtiObjectList;
//                            AIn2Only: TtiObjectList); overload;
  published
    // This must be published so it can be used by the tiPerAware controls.
    property    List  : TList read GetList ;
  end ;

  TtiClass  = class of TtiObject ;
  TtiObjectClass = class of TtiObject;
  TPerObjListClass = class of TtiObjectList ;
  TtiObjectListClass = class of TtiObjectList ;


  TPerObjErrors = class( TtiObjectList )
  private
  protected
    function    GetItems(i: integer): TPerObjError ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TPerObjError); reintroduce ;
    function    GetAsString: string; virtual ;
  public
    property    Items[i:integer] : TPerObjError read GetItems write SetItems ;
    procedure   Add( pObject : TPerObjError   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    procedure   AddError( const pErrorProperty : string ; const pErrorMessage : string ; pErrorCode : integer ) ; overload ;
    procedure   AddError( const pErrorProperty : string ; const pErrorMessage : string ) ; overload ;
    procedure   AddError( const pErrorMessage : string ) ; overload ;
    function    FindByMessage( const pMessage : string ) : TPerObjError ;
    property    AsString: string Read GetAsString;
  published
  end ;

  
  TPerObjError = class( TtiObject )
  private
    FErrorMessage: string;
    FErrorProperty: string;
    FErrorCode: Word;
  protected
    function    GetOwner: TPerObjErrors; reintroduce ;
    procedure   SetOwner(const Value: TPerObjErrors ); reintroduce ;
  public
    property    Owner       : TPerObjErrors             read GetOwner      write SetOwner ;
  published
    property    ErrorProperty : string read FErrorProperty write FErrorProperty ;
    property    ErrorMessage  : string read FErrorMessage  write FErrorMessage ;
    property    ErrorCode     : Word   read FErrorCode     write FErrorCode ;
  end ;


  TPerObjClassMapping = class( TtiBaseObject )
  private
    FPerObjAbsClassName: string;
    FPerObjAbsClass: TtiClass;
  public
    constructor Create ; virtual ;
    property PerObjAbsClassName : string read FPerObjAbsClassName write FPerObjAbsClassName ;
    property PerObjAbsClass : TtiClass read FPerObjAbsClass write FPerObjAbsClass ;
  end ;


  TPerObjFactory = class( TtiBaseObject )
  private
    FList : TObjectList ;
  protected
    function FindByClassName( const pClassName : string ) : TPerObjClassMapping ; virtual ;
    function GetItems(pIndex: integer): TPerObjClassMapping; virtual ;
  public
    constructor Create ; virtual ;
    destructor  Destroy ; override ;
    procedure   RegisterClass( const pClassName : string ; pClass : TtiClass ) ;
    procedure   UnRegisterClass( const pClassName : string ) ;
    function    CreateInstance( const pClassName : string ) : TtiObject ;
    function    CreateNewInstance( const pClassName : string; pOwner : TtiObject = nil ) : TtiObject ;
    function    IsRegistered( const pClassName : string ) : boolean ;
    property    Items[pIndex : integer] : TPerObjClassMapping read GetItems ;
    function    Count : integer ;
  end ;

  
  TPerStream = class( TtiObject )
  private
    FStream : TMemoryStream ;
    function  GetSize     : integer;
    function  GetAsString : string;
    procedure SetAsString(const Value: string);
    procedure SetSize(const Value: integer);
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Stream : TMemoryStream read FStream write FStream ;
    property    Size   : integer       read GetSize write SetSize ;

    procedure   SaveToFile( const psFileName : string ) ;
    procedure   LoadFromFile( const psFileName : string ) ;
    procedure   Clear ;
    property    AsString : string read GetAsString write SetAsString ;
  end ;


  TPerStringStream = class( TtiObject )
  private
    FStream : TStringStream ;
    function GetAsString: string;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    property    Stream : TStringStream read FStream write FStream ;
    procedure   Write( const psValue : string ) ;
    procedure   WriteLn( const psValue : string ) ;
    property    AsString : string read GetAsString ;
  end ;


  {:The thread-safe version of <See Class="TtiObjectList">}
  TPerObjThreadList = class( TtiObjectList )
  private
    FCriticalSection: TCriticalSection;
    //FRaiseLockException : Boolean;
  protected
    function    GetCount: integer; override ;
    procedure   SetItems(i: integer; const Value: TtiObject); override ;
    procedure   SetItemOwner(const Value: TtiObject); override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    {: Locks the list to prevent other threads gaining access.}
    procedure   Lock ;
    {: Unlocks the list to allow other threads access.}
    procedure   UnLock ;

    procedure   Delete( i : integer ) ; override ;
    procedure   Add( pObject : TtiObject  ; pDefDispOrdr : boolean = true ) ; override ;
    procedure   Clear ; override ; // Empty list and delete all owned objects
    procedure   Empty ; override ; // Empty list, but do not delete owned objects
    function    IndexOf( pData : TObject ) : integer ; override ;
    function    Last  : TtiObject ; override ;
    function    First : TtiObject ; override ;
    function    FirstExcludeDeleted : TtiObject ; override ;
    function    LastExcludeDeleted : TtiObject ; override ;
    function    Remove( pData : TtiObject ) : integer ; override ;
    procedure   MarkListItemsForDeletion ; override ;
    procedure   MarkListItemsDirty ; override ;
    procedure   SortByOID ; override ;
    procedure   SortByProps( const pSortProps : array of string; pAscendingOrder : Boolean = True ) ; override ;
    {:Does a lock failure raise an exception or just log an error?}
    //Property RaiseLockException : Boolean Read FRaiseLockException Write FRaiseLockException;
  end ;


  // TPerVisList is here for backward compatibility.
  // Do not use. Use TtiObjectList instead.
  TPerVisList = class( TtiObjectList ) ;


  TVisPerObjFind = class( TtiVisitor )
  private
    FUserContext: Pointer;
    FFound: TtiObject;
    FPerObjFindMethod: TPerObjFindMethod;
    FPerObjFindMethodExt: TPerObjFindMethodExt;
    FFoundList: TList;
    FData: TtiObject;
    FPerObjFindMethodData: TPerObjFindMethodData;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    constructor Create ; override ;
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
    property    Found : TtiObject read FFound ;
    property    PerObjFindMethod : TPerObjFindMethod read FPerObjFindMethod write FPerObjFindMethod ;
    property    PerObjFindMethodExt: TPerObjFindMethodExt read FPerObjFindMethodExt write FPerObjFindMethodExt;
    property    PerObjFindMethodData: TPerObjFindMethodData read FPerObjFindMethodData write FPerObjFindMethodData;
    property    FoundList : TList read FFoundList write FFoundList ;
    property    UserContext : Pointer read FUserContext write FUserContext ;
    property    Data : TtiObject read FData write FData ;
  end ;

  
  TVisPerObjFindByOID = class( TtiVisitor )
  private
    FOIDToFind: TOID;
    FFound: TtiObject;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    constructor Create ; override ;
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
    property    Found : TtiObject read FFound ;
    property    OIDToFind : TOID read FOIDToFind write FOIDToFind ;
  end ;


  TVisPerObjDel = class( TtiVisitor )
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
  end ;


  TVisSetAllObjectStates = class( TtiVisitor )
  private
    FObjectState: TPerObjectState;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
    property    ObjectState : TPerObjectState read FObjectState write FObjectState ;
  end ;


  TVisPerObjIsDirty = class( TtiVisitor )
  private
    FbDirty: boolean;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
    property    Dirty : boolean read FbDirty write FbDirty ;
  end ;


  // Stream a TtiObject out as text
  TVisTIObjectAsDebugString = class( TVisStringStream )
  private
    FbIncludeDeleted: Boolean;
    FbDataOnly: Boolean;
  protected
    function    AcceptVisitor: Boolean; override;
  public
    constructor Create; overload; override;
    constructor Create(pIncludeDeleted: Boolean; pDataOnly: Boolean = False); overload;
    procedure   Execute(const pVisited: TtiVisited); override;
    function    Indent: string;
    property    IncludeDeleted: Boolean read FbIncludeDeleted write FbIncludeDeleted ;
    property    DataOnly: Boolean read FbDataOnly write FbDataOnly ;
  end ;


const
  cgNullDBInteger            = -1  ;
  cgNullDBString             = ''  ;
  cgNullDBDate               = 0.0 ;


function ObjectStateToString( pObjectState : TPerObjectState ) : string ;


implementation
uses
  // tiOPF
   tiConstants
  ,tiLog
  ,tiOPFManager
  ,tiExcept
  ,tiUtils
  // Delphi
  ,SysUtils
  ,Math
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}
  ;


function ObjectStateToString( pObjectState : TPerObjectState ) : string ;
begin
  result := GetEnumName( TypeInfo( TPerObjectState ),
                         Ord( pObjectState )) ;
end ;


{ TtiObject } 

constructor TtiObject.Create ;
begin
  inherited Create ;
  FObjectState := posEmpty ;
  {$IFDEF OID_AS_INT64}
  FOID := cNullOIDInteger;
  {$ENDIF}
  FOwner       := nil ;
  FUpdateCount := 0;
end;

{: @ToDo We have to address two issues with assign:
  i)  What if our object contains a pointer to another object that it owns
  ii) What if our object contains a list of other objects
  If we are to be creating new instances of owned objects, we must work out
  what to do about ObjectState and OIDs 

  Creates a cloned instance of the object. This method must be overridden and
  typecast if you are going to use it. }
function TtiObject.Clone: TtiObject;
var
  lClass : TtiClass ;
begin
  lClass := TtiClass( ClassType ) ;
  result := TtiObject( lClass.Create );
  result.Assign( self ) ;
end;


{: When you create a concrete class that contains object type properties
  you will have to override AssignClassProps( ) and implement the necessary
  behaviour to copy, clone or create new instances of these properties. }
procedure TtiObject.Assign( const pSource: TtiObject);
begin
  Assert(( pSource is Self.ClassType ) or
          ( Self is pSource.ClassType ),
          pSource.ClassName +
          ' and ' +
          ClassName +
          ' are not assignment compatable' ) ;

  AssignPublicProps(    pSource ) ;
  AssignPublishedProps( pSource ) ;
  AssignClassProps(     pSource ) ;

  // When you create a concrete class that contains object type properties
  // you will have to override AssignClassProps( ) and implement
  // the necessary behaviour to copy, clone or create new instances
  // of these properties.

end;


procedure TtiObject.AssignPublishedProps( pSource : TtiObject ;
                                           pPropFilter : TTypeKinds = [] ) ;
var
  lsl : TStringList ;
  i : integer ;
  lsPropName : string ;
  lPropFilter : TTypeKinds ;
begin
  if pPropFilter = [] then
    lPropFilter := ctkSimple + [tkEnumeration, tkVariant]
  else
    lPropFilter := pPropFilter ;

  lsl := TStringList.Create ;
  try
    tiGetPropertyNames( self, lsl, lPropFilter ) ;
    for i := 0 to lsl.Count - 1 do
    begin
      lsPropName := lsl.Strings[i] ;
      try
        // Only clone read/write properties
        if ( tiIsReadWriteProp( Self, lsPropName )) and
           ( IsPublishedProp( pSource, lsPropName )) then
          AssignPublishedProp( pSource, lsPropName );
      except
        on e:exception do
          raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
            [ClassName, lsPropName, e.Message]);
      end ;
    end ;
  finally
    lsl.Free ;
  end ;
end ;


function TtiObject.CountPropsByType( pSource : TtiObject ;
                                      pPropFilter : TTypeKinds ) : integer ;
var
  lsl : TStringList ;
begin
  lsl := TStringList.Create ;
  try
    tiGetPropertyNames( self, lsl, pPropFilter ) ;
    result := lsl.Count ;
  finally
    lsl.Free ;
  end ;
end ;


procedure TtiObject.AssignPublicProps( pSource : TtiObject ) ;
begin
  Assert( pSource.TestValid(TtiObject), cTIInvalidObjectError );
  Assert( pSource.Owner.TestValid(TtiObject, true), cTIInvalidObjectError );
  {$IFDEF OID_AS_INT64}
    OID := pSource.OID ;
  {$ELSE}
    OID.Assign( pSource.OID ) ;
  {$ENDIF}
  ObjectState := pSource.ObjectState ;
  // 1. If we are cloning a list element to edit, then we will probably
  //    want it's Owner property set to the list. This will be done here.
  // 2. If we are editing a list, then we want each list element to be cloned
  //    too and we want their owner properties to be set to the cloned list,
  //    this will be done in TtiObjectList.AssignClassProps.
  // 3. If we are editing a compound object, then Owner will be set in
  //    the classes constructor.
  if pSource.Owner is TtiObjectList then
    Owner := pSource.Owner;
end;


procedure TtiObject.AssignPublishedProp(pSource: TtiObject; psPropName: string);
var
  lPropType: TTypeKind;
begin
  lPropType := TypInfo.PropType(pSource, psPropName);
  if lPropType in ctkSimple + [tkVariant, tkEnumeration] then
  begin
    case lPropType of
      tkChar        : SetOrdProp(Self, psPropName, GetOrdProp(pSource, psPropName));
      tkWChar       : SetOrdProp(Self, psPropName, GetOrdProp(pSource, psPropName));
      tkString      : SetStrProp(Self, psPropName, GetStrProp(pSource, psPropName));
      tkLString     : SetStrProp(Self, psPropName, GetStrProp(pSource, psPropName));
      tkWString     : SetWideStrProp(Self, psPropName, GetWideStrProp(pSource, psPropName));
      {$IFDEF FPC}
      tkAString     : SetStrProp(Self, psPropName, GetStrProp(pSource, psPropName));
      {$ENDIF}
      tkInteger     : SetOrdProp(Self, psPropName, GetOrdProp(pSource, psPropName));
      tkInt64       : SetInt64Prop(Self, psPropName, GetInt64Prop(pSource, psPropName));
      tkFloat       : SetFloatProp(Self, psPropName, GetFloatProp(pSource, psPropName));
      tkVariant     : SetVariantProp(Self, psPropName, GetVariantProp(pSource, psPropName));
      tkEnumeration : SetOrdProp(Self, psPropName, GetOrdProp(pSource, psPropName));
      {$IFDEF FPC}
      tkBool        : SetInt64Prop(Self, psPropName, GetInt64Prop(pSource, psPropName));
      {$ENDIF}
    end;
  end
  else
    raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
      [ClassName, psPropName, 'Unknown property type']);
end;


procedure TtiObject.AssignClassProps( pSource : TtiObject ) ;
begin
  Assert( CountPropsByType( pSource, [tkClass] ) = 0,
          'Trying to call ' + ClassName + '.Assign( ) on a class that contains ' +
          'object type properties. AssignClassProps( ) must be overridden in the concrete class.' ) ;
end;


{ Note: This functionality has not been tested fully. Bit of a hack really :(
        Talk about thrashing the CPU with out need. :( :( :( !  }
function TtiObject.Equals(const pData: TtiObject): boolean;
var
  lVisComp : TVisTIObjectAsDebugString;
  lVisSelf : TVisTIObjectAsDebugString;
begin
  // ToDo: Better to do this as a field by field compare...
  lVisComp := TVisTIObjectAsDebugString.Create(False, True);
  try
    lVisSelf := TVisTIObjectAsDebugString.Create(False, True);
    try
      Self.Iterate( lVisSelf ) ;
      pData.Iterate( lVisComp ) ;
      result := lVisSelf.Text = lVisComp.Text ;
    finally
      lVisSelf.Free ;
    end ;
  finally
    lVisComp.Free ;
  end ;
end;


function TtiObject.GetDeleted: boolean;
begin
  result := ( ObjectState = posDelete ) or
            ( ObjectState = posDeleted ) ;
end;


function TtiObject.GetDirty: boolean;
var
  lVis : TVisPerObjIsDirty ;
begin
  lVis := TVisPerObjIsDirty.Create ;
  try
    self.Iterate( lVis ) ;
    result := lVis.Dirty ;
  finally
    lVis.Free ;
  end ;
end;


procedure TtiObject.SetDeleted(const Value: boolean);
var
  lVis : TVisPerObjDel ;
begin
  if Value and not Deleted then
  begin
    lVis := TVisPerObjDel.Create ;
    try
      self.Iterate( lVis ) ;
    finally
      lVis.Free ;
    end ;
  end ;
end;


procedure TtiObject.SetDirty(const Value: boolean);
begin
  if Value then
  begin   // Dirty set to True
    case ObjectState of
      posEmpty   : begin
                     ObjectState := posCreate    ;
                     {$IFDEF OID_AS_INT64}
                       if OID = cNullOIDInteger then
                         OID := gTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID ;
                     {$ELSE}
                       if OID.IsNull then
                           Assert( false, 'Under construction' ) ;
                         // OID.GetNextValue ;
                     {$ENDIF}
                   end ;
      posPK      : ObjectState := posUpdate    ;
      posCreate  : ; // Do nothing
      posUpdate  : ; // Do nothing
      posDelete  : ; // Do nothing
      posDeleted : ; // Do nothing
      posClean   : ObjectState := posUpdate ;
    else
      raise EtiOPFInternalException.Create(cErrorInvalidObjectState);
    end;
  end
  else
  begin   // Dirty set to False
    ObjectState := posClean;
  end;
end;


{ TVisPerObjDel } 

function TVisPerObjDel.AcceptVisitor : boolean;
begin
  result := ( Visited is TtiObject );
  if not result then
    Exit ; //==>

  result := ( TtiObject(Visited).ObjectState <> posDeleted );
  if not result then
    Exit ; //==>

  if VisitedsOwner = nil then
    Exit ; //==>

  if VisitedsOwner = Visited then
    Exit ; //==>

  // Check to see that the object currently being visited  actually
  // has an owner of the next object up the tree.
  // If it does, then assume it is owned by the previous object,
  // if it does not, then assume it is just a pointer to this object.
  result := ( VisitedsOwner = TtiObject(Visited).Owner );
  if result then
    Exit ; //==>

  if VisitedsOwner is TtiObjectList then
    result := ( TtiObjectList(VisitedsOwner).ItemOwner =
                TtiObject(Visited).Owner );

end;


procedure TVisPerObjDel.Execute(const pVisited: TtiVisited) ;
begin
  Inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

  TtiObject(pVisited).ObjectState := posDelete ;
end;


{ TVisPerObjIsDirty } 

function TVisPerObjIsDirty.AcceptVisitor : boolean;
begin
  result := (( Visited is TtiObject ) or
            ( Visited is TtiObjectList )) and
            ( not Dirty ) ;
end;


procedure TVisPerObjIsDirty.Execute(const pVisited: TtiVisited);
begin
  inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

  if Visited is TtiObject then
    Dirty := TtiObject( pVisited ).ObjectState in
              [ posCreate,  // The object is new and must be created in the DB
                posUpdate,  // The object has been changed, the DB must be updated
                posDelete   // The object has been deleted, it must be deleted from the DB
              ]
  else
    Assert( false, 'Invalid visited type' ) ;

  // Use this to debug problems when you think you have set an object's state
  // to posClean or posDeleted, but it's parent object is still showing dirty.
{
  if Dirty then
    Log([ 'Dirty',
          Dirty,
          Visited.ClassName,
          Visited.Caption,
          GetEnumName( TypeInfo( TPerObjectState ),
                       ord( TtiObject( pVisited ).ObjectState ))]) ;
}
end;


{ TtiObjectList } 

constructor TtiObjectList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
  FItemOwner := Self;
  FbAutoSetItemOwner := true;
end;


destructor TtiObjectList.Destroy;
begin
  FList.Free ;
  inherited;
end;


procedure TtiObjectList.Add(pObject: TtiObject ; pDefDispOrdr : boolean);
begin
  Add(pObject);
end;


procedure TtiObjectList.Add( const pObject : TtiObject ) ;
begin
  if FbAutoSetItemOwner then
    pObject.Owner := FItemOwner ;
  FList.Add( pObject ) ;
end ;


procedure TtiObjectList.Clear;
begin
  FList.Clear ;
  // Should Clear set the ObjectState to posEmpty. Originally we thought yes,
  // then got burnt by side effects, so this was removed 26/09/2001
  { 2005-08-25 graemeg: I thought I would take my chances as it makes sence
    to set the ObjectState }
  ObjectState := posEmpty ;
end;


{: Call Delete to remove the object at Index from the list. (The first object 
  is indexed as 0, the second object is indexed as 1, and so forth.) After an 
  object is deleted, all the objects that follow it are moved up in index 
  position and Count is decremented.

  To use an object reference (rather than an index position) to specify the 
  object to be removed, call Remove.

  If OwnsObjects is True, Delete frees the object in addition to removing it 
  from the list. To remove an object from the list without freeing it, call 
  Extract. }
procedure TtiObjectList.Delete(i: integer);
begin
  if AutoSetItemOwner then
    TtiObject( Items[i] ).Owner := nil ;
  FList.Delete( i ) ;
end;


{ TVisTIObjectAsDebugString }

function TVisTIObjectAsDebugString.AcceptVisitor : boolean;
begin
  result := ( Visited is TtiObject ) and
            (( not TtiObject( Visited ).Deleted ) or
             ( TtiObject( Visited ).Deleted and IncludeDeleted )) ;
end;

constructor TVisTIObjectAsDebugString.Create;
begin
  inherited;
  FbIncludeDeleted  := False ;
  FbDataOnly        := False ;
end;

constructor TVisTIObjectAsDebugString.Create(pIncludeDeleted: Boolean;
  pDataOnly: Boolean);
begin
  Create;
  FbIncludeDeleted  := pIncludeDeleted;
  FbDataOnly        := pDataOnly;
end;


procedure TVisTIObjectAsDebugString.Execute(const pVisited: TtiVisited);
var
  i : integer ;
  lslProps : TStringList ;
  lsValue : string ;
  lsPropName : string ;
begin
  inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    Exit ; //==>

  Write( Indent + pVisited.Caption ) ;
  if not FbDataOnly then
  begin
    Write( ', ' ) ;
    Write( TtiObject( pVisited ).ObjectStateAsString ) ;
    Write( ', ' ) ;
    if pVisited.ClassName <> ( pVisited ).Caption then
    begin
      Write( TtiObject( pVisited ).Caption ) ;
      Write( ', ' ) ;
    end;
    {$IFDEF OID_AS_INT64}
      Write( IntToStr( TtiObject( pVisited ).OID )) ;
    {$ELSE}
      if gTIOPFManager.DefaultOIDClassName <> '' then
        Write( 'OID=' +TtiObject( pVisited ).OID.AsString )
      else
        Write('OID=Null');
    {$ENDIF}
    if TtiObject( pVisited ).Dirty then
      Write( ', *< Dirty >*' ) ;
  end ;

  WriteLn( '' ) ;

  lslProps := TStringList.Create ;
  try
    tiGetPropertyNames( TtiBaseObject( pVisited ),
                        lslProps,
                        ctkSimple + [tkVariant, tkEnumeration] ) ;

    // dean.millam@duffersgreens.com, modified to format TDateTime
    for i := 0 to lslProps.Count - 1 do
    begin
      lsPropName := lslProps.Strings[i] ;
      if not SameText( lsPropName, 'Caption' ) then
      begin
        try
          lsValue := TtiObject( pVisited ).PropValue[lsPropName];
          if TtiObject(pVisited).PropType(lsPropName) = tiTKDateTime then
            lsValue := tiDateTimeAsIntlDateDisp(TtiObject(pVisited).PropValue[lsPropName])
          else
            lsValue := TtiObject(pVisited).PropValue[lsPropName];
        except
          on e:exception do
            lsValue := 'Error: ' + e.Message ;
        end ;

        lsValue := '  ' +
                   lslProps.Strings[i] +
                   ' = ' +
                   lsValue ;
        WriteLn( Indent + lsValue ) ;
      end ;
    end ;
  finally
    lslProps.Free ;
  end ;
end;


procedure TtiObject.SetObjectState(const pValue: TPerObjectState);
begin
  if FObjectState = pValue then exit;
  FObjectState := pValue;
end;


procedure TtiObjectList.Empty;
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    FList.Extract(FList.Items[i]);
end;


function TtiObjectList.GetCount: integer;
begin
  result := FList.Count;
end;


function TtiObjectList.GetItems(i: integer): TtiObject ;
begin
  result := TtiObject( FList.Items[ i ] ) ;
end;


function TtiObjectList.GetList: TList;
begin
  result := FList ;
end;


function TtiObjectList.IndexOf(pData: TObject): integer;
begin
  result := FList.IndexOf( pData ) ;
end;


function TtiObjectList.Last: TtiObject;
begin
  // TList can't handle this. Strange!
  if FList.Count > 0 then
// Under some circumstances, this will AV. Why?  
//    result := TtiObject( FList.Last )
    result := TtiObject( FList.Items[FList.Count-1] )
  else
    result := nil ;
end;


function TtiObjectList.LastExcludeDeleted: TtiObject;
var
  i : integer ;
begin
  for i := Count-1 downto 0 do
    if not Items[i].Deleted then
    begin
      result := Items[i] ;
      Exit ; //==>
    end ;
  result := nil ;
end;


procedure TtiObjectList.SetItems(i: integer; const Value: TtiObject );
begin
  FList.Items[ i ] := Value ;
end;


function TtiObject.ObjectStateAsString: string;
begin
  result := GetEnumName( TypeInfo( TPerObjectState ),
                         Ord( ObjectState )) ;
end;


function TVisTIObjectAsDebugString.Indent: string;
begin
  result := tiSpace(( Depth - 1 ) * 2) ;
end;


{ TPerStream }
 
constructor TPerStream.Create;
begin
  inherited;
  FStream := nil ;
  Clear ;
end;


destructor TPerStream.Destroy;
begin
  FStream.Free ;
  inherited;
end;


procedure TPerStream.Clear;
begin
  if FStream <> nil then
    FStream.Free ;
  FStream := TMemoryStream.Create ;
end;


function TPerStream.GetSize: integer;
begin
  result := FStream.Size ;
end;


procedure TPerStream.LoadFromFile(const psFileName: string);
begin
  FStream.LoadFromFile( psFileName ) ;
end;


procedure TPerStream.SaveToFile( const psFileName: string);
begin
  FStream.Position := 0 ;
  FStream.SaveToFile( psFileName ) ;
end;


function TPerStream.GetAsString: string;
var
  ls : string ;
begin
  FStream.Seek( 0,soFromBeginning ) ;
  SetString( ls, nil, FStream.Size );
  Stream.Read( Pointer( ls )^, FStream.Size);
  Result := ls ;
end;


procedure TPerStream.SetAsString(const Value: string);
var
  lpcText : PChar ;
begin
  FStream.Clear ;
  lpcText := PChar( Value ) ;
  FStream.WriteBuffer( lpcText^, length( lpcText )) ;
end;


{: Call Remove to delete a specific object from the list when its index is 
  unknown. The value returned is the index of the object in the Items array 
  before it was removed. If the specified object is not found on the list, 
  Remove returns -1. If OwnsObjects is True, Remove frees the object in 
  addition to removing it from the list. After an object is deleted, all the 
  objects that follow it are moved up in index position and Count is 
  decremented. If an object appears more than once on the list, Remove deletes 
  only the first appearance. Hence, if OwnsObjects is True, removing an object 
  that appears more than once results in empty object references later in the 
  list. To use an index position (rather than an object reference) to specify 
  the object to be removed, call Delete. To remove an object from the list 
  without freeing it, call Extract. }
function TtiObjectList.Remove(pData: TtiObject):integer;
begin
  if AutoSetItemOwner then
    pData.Owner := nil ;
  result := FList.Remove( pData ) ;
end;


{: Call Extract to remove an object from the list without freeing the object
  itself. After an object is removed, all the objects that follow it are moved 
  up in index position and Count is decremented.}
procedure TtiObjectList.Extract(pData: TtiObject);
begin
  if AutoSetItemOwner then
    pData.Owner := nil ;
  FList.Extract( pData ) ;
end;


{: Call Insert to add an object at a specified position in the list, shifting 
  the item that previously occupied that position (and all subsequent items) up.
  Insert increments Count and, if necessary, allocates memory by increasing the 
  value of Capacity. The Index parameter is zero-based, so the first position 
  in the list has an index of 0. To replace a nil reference with a new object 
  without growing the array, set the Items property directly. }
procedure TtiObjectList.Insert(const piIndex: integer; pData: TtiObject);
begin
  FList.Insert( piIndex, pData ) ;
  pData.Owner := self ;
//  AssignDispOrder( pData ) ;
end;

//{: @TODO AssignDispOrder logic requires work. }
//procedure TtiObjectList.AssignDispOrder( pData: TtiObject ) ;
//var
//  i : integer ;
//  lBefore : TtiObject ;
//  lAfter  : TtiObject ;
//begin
//  i := IndexOf( pData ) ;
//  if i > 0 then
//    lBefore := Items[i-1]
//  else
//    lBefore := nil ;
//
//  if i < Count-1 then
//    lAfter := Items[i+1]
//  else
//    lAfter := nil ;
//
//  if      ( lBefore = nil ) and ( lAfter = nil ) then
//  begin
//    pData.DispOrder := cuiDispOrderInc ;
//  end
//  else if ( lBefore <> nil ) and ( lAfter = nil ) then
//  begin
//    pData.DispOrder := ( lBefore.DispOrder div cuiDispOrderInc + 1 ) * cuiDispOrderInc ;
//  end
//  else if ( lBefore = nil ) and ( lAfter <> nil ) then
//  begin
//    pData.DispOrder := ( lAfter.DispOrder div cuiDispOrderInc - 1 ) * cuiDispOrderInc ;
//  end
//  else if ( lBefore <> nil ) and ( lAfter <> nil ) then
//  begin
//    pData.DispOrder := ( lBefore.DispOrder + lAfter.DispOrder ) div 2 ;
//  end ;
//  pData.Dirty := true ;
//end ;


procedure TtiObjectList.Insert(pInsertBefore, pData: TtiObject);
var
  i : integer ;
begin
  i := FList.IndexOf( pInsertBefore ) ;
  if i >= 0 then
    Insert( i, pData )
  else
  begin
    Add( pData ) ;
  end ;
end;


{ TVisPerObjFindByOID }
 
function TVisPerObjFindByOID.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiObject ) and
            ( FFound = nil ) ;
end;


constructor TVisPerObjFindByOID.Create;
begin
  inherited;
  FFound := nil ;
end;


procedure TVisPerObjFindByOID.Execute(const pVisited: TtiVisited);
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  if OIDEquals( TtiObject( Visited ).OID ,FOIDToFind ) then
    FFound := TtiObject( Visited ) ;
end;


function TtiObject.Find(pPerObjFindMethod: TPerObjFindMethod): TtiObject;
var
  lVis : TVisPerObjFind ;
begin
  lVis := TVisPerObjFind.Create ;
  try
    lVis.PerObjFindMethod := pPerObjFindMethod ;
    self.Iterate( lVis ) ;
    result := lVis.Found ;
  finally
    lVis.Free ;
  end ;
end ;


function TtiObject.Find( pPerObjFindMethodExt : TPerObjFindMethodExt; pUserContext: Pointer ) : TtiObject ;
var
  lVis : TVisPerObjFind ;
begin
  lVis := TVisPerObjFind.Create ;
  try
    lVis.PerObjFindMethodExt := pPerObjFindMethodExt ;
    lVis.UserContext := pUserContext;
    self.Iterate( lVis ) ;
    result := lVis.Found ;
  finally
    lVis.Free ;
  end ;
end ;


function TtiObject.FindAll( pPerObjFindMethod: TPerObjFindMethod ; pList : TList ): integer;
var
  lVis : TVisPerObjFind ;
begin
  if pList <> nil then
    pList.Clear ;
  lVis := TVisPerObjFind.Create ;
  try
    lVis.FoundList := pList ;
    lVis.PerObjFindMethod := pPerObjFindMethod ;
    self.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
  result := pList.Count ;
end;


function TtiObject.Find( pOIDToFind : TOID ) : TtiObject;
var
  lVis : TVisPerObjFindByOID ;
begin
  lVis := TVisPerObjFindByOID.Create ;
  try
    lVis.OIDToFind := pOIDToFind ;
    self.Iterate( lVis ) ;
    result := lVis.Found ;
  finally
    lVis.Free ;
  end ;
end;


procedure TtiObjectList.MarkListItemsForDeletion;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    Items[i].Deleted := true ;
end;


procedure TtiObjectList.MarkListItemsDirty;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    Items[i].Dirty := true ;
end;


{ TVisPerObjFind }

function TVisPerObjFind.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiObject ) and
            (( FFound = nil ) or ( FFoundList <> nil )) ;
end;


constructor TVisPerObjFind.Create;
begin
  inherited;
end;


procedure TVisPerObjFind.Execute(const pVisited: TtiVisited);
var
  lbFound : boolean ;
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  lbFound := false ;
  if Assigned(FPerObjFindMethod) then
    FPerObjFindMethod( TtiObject( Visited ), lbFound )
  else if Assigned( FPerObjFindMethodExt ) then
    PerObjFindMethodExt( TtiObject( Visited ), lbFound, FUserContext )
  else if Assigned( FPerObjFindMethodData ) then
    PerObjFindMethodData( TtiObject( Visited ), lbFound, Data )
  else
    raise EtiOPFProgrammerException.Create(cErrorNoFindMethodAssigned);

  if lbFound then
    if FoundList = nil then
      FFound := TtiObject( Visited )
    else
      FoundList.Add( Visited ) ;
end;


function TtiObject.GetOwner: TtiObject;
begin
  //Assert( FOwner <> Nil, 'Owner has not been assigned in ' + ClassName ) ;
  Result := FOwner;
end;


procedure TtiObject.SetOwner(const Value: TtiObject);
begin
  FOwner := Value ;
end;


{ By default, IsUnique will check all objects in the collection (from
  the current object down the tree) for uniqueness by OID. Override
  DoFindAllNotUnique to change the properties that are tested. } 
function TtiObject.IsUnique(const pPerObjAbs: TtiObject ) : boolean ;
var
  lList : TList ;
  i : integer ;
begin
  Assert( Assigned( pPerObjAbs ), 'pPerObjAbs not assigned' ) ;
  lList := TList.Create ;
  try
    FindAll( DoFindAllNotUnique, lList, pPerObjAbs ) ;
    result := true ;
    for i := 0 to lList.Count - 1 do
    begin
      if ( TtiObject(lList.Items[i]) <> pPerObjAbs ) {and
         ( lList.Items[i] <> Self )} and
         ( OIDEquals( TtiObject( lList.Items[i] ).OID, pPerObjAbs.OID )) then
      begin
        result := false ;
        Exit ; //==>
      end ;
    end ;
  finally
    lList.Free ;
  end ;
end;


procedure TtiObjectList.ForEach(pMethod: TPerObjForEachMethod; pbIncludeDeleted: boolean=false);
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    if not Items[i].Deleted or pbIncludeDeleted then
      pMethod( Items[i] ) ;
end;


procedure TtiObjectList.ForEach(pMethod: TPerObjForEachMethodRegular; pbIncludeDeleted: boolean);
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    if not Items[i].Deleted or pbIncludeDeleted then
      pMethod( Items[i] ) ;
end;


procedure TtiObjectList.PropToStrings(const pStrings: TStrings ; const pPropName : string = 'caption' );
var
  i : integer ;
  lPropValue : string ;
begin
  pStrings.Clear ;
  for i := 0 to Count - 1 do
  begin
    lPropValue := TypInfo.GetPropValue( Items[i], pPropName );
    pStrings.AddObject( lPropValue, Items[i] ) ;
  end ;
end;


function TtiObjectList.GetCountNotDeleted: integer;
var
  i : integer ;
begin
  result := 0 ;
  for i := 0 to Count - 1 do
    if not Items[i].Deleted then
      Inc( result ) ;
end;


{ TPerStringStream }

constructor TPerStringStream.Create;
begin
  FStream := TStringStream.Create( '' ) ;
  inherited;
end;


destructor TPerStringStream.Destroy;
begin
  FStream.Free ;
  inherited;
end;


function TPerStringStream.GetAsString: string;
begin
//  FStream.Position := 0 ;
  result := FStream.DataString ;
end;


procedure TPerStringStream.Write(const psValue: string);
begin
  FStream.WriteString( psValue ) ;
end;


procedure TPerStringStream.WriteLn(const psValue: string);
begin
  FStream.WriteString( psValue + CrLf ) ;
end;


procedure TtiObjectList.SetItemOwner(const Value: TtiObject);
var
  i : integer ;
begin
  FItemOwner := Value;
  for I := 0 to Count - 1 do
    Items[ I ].Owner := FItemOwner ;
end ;


procedure TtiObjectList.AssignClassProps(pSource: TtiObject);
var
  i : integer ;
  lClass : TtiClass ;
  lSource : TtiObject ;
  lTarget : TtiObject ;
begin
  Assert( pSource is TtiObjectList,
          'pSource not a TtiObjectList' ) ;

  if OwnsObjects then
  begin
    for i := 0 to TtiObjectList( pSource ).Count - 1 do
    begin
      lSource := TtiObjectList( pSource ).Items[i] ;
      lClass := TtiClass( lSource.ClassType ) ;
      lTarget  := TtiObject( lClass.Create );
      Add( lTarget ) ;
      lTarget.Assign( lSource ) ;
      if AutoSetItemOwner then
        lTarget.Owner := ItemOwner ;

    end ;
  end
  else
    for i := 0 to TtiObjectList( pSource ).Count - 1 do
      Add( TtiObjectList( pSource ).Items[i] );
end;


function TtiObjectList.GetOwnsObjects: boolean;
begin
  result := FList.OwnsObjects ;
end;


procedure TtiObjectList.SetOwnsObjects(const Value: boolean);
begin
  FList.OwnsObjects := Value ;
end;


function TtiObject.GetIndex: integer;
begin
  Assert( Owner <> nil,
          'Owner not assigned' ) ;
  Assert( Owner is TtiObjectList,              
          'Owner not a TtiObjectList, its a ' + Owner.ClassName ) ;
  result := TtiObjectList( Owner ).IndexOf( self ) ;
end;


function TtiObjectList.FindByProps( const pProps : array of string ;
                                  const pVals  : array of variant;
                                  pCaseSensitive : boolean = true ): TtiObject;
var
  j: Integer;
  i : integer ;
  lFound : boolean;

  function PropertyMatch(Idx: Integer; PropName: string; PropValue: variant): boolean;
  var
    lSearch, lItem : variant;
    lVarType : TVarType;
    tiFieldAbs: TtiFieldAbs;
  begin
    lSearch := PropValue;
    // If the property is a TtiFieldAbs then compare the value with the field
    // objects AsString, else compare the value with the property itself
    if GetPropInfo(Items[Idx], PropName, [tkClass]) <> nil then begin
      tiFieldAbs := GetObjectProp(Items[Idx], PropName, TtiFieldAbs) as TtiFieldAbs;
      if Assigned(tiFieldAbs) then
        lItem := tiFieldAbs.AsString
      else
        lItem := TypInfo.GetPropValue( Items[Idx], PropName );
    end else
      lItem := TypInfo.GetPropValue( Items[Idx], PropName );
    lVarType := VarType(lItem);

    // This part should be wrote in a better way (more efficient...)
    // But I'm not sure it is really important here
    // Just to be sure that I'm comparing the SAME kind of values
    if (VarType(PropValue)=varBoolean) then
      lItem:=VarAsType(lItem,varBoolean)
    else
    begin
      lSearch := VarAsType(lSearch,lVarType);
      lItem := VarAsType(lItem, lVarType);
    end;

    // PWH Changed for D5 compat
    if (tiIsVariantOfType(lSearch,varOleStr) or
        tiIsVariantOfType(lSearch,varString)) and not pCaseSensitive then
      result := SameText(lSearch,lItem)
    else
      result := (lSearch = lItem);
  end;

begin
  Assert( High( pProps ) = High( pVals ),
          'Props and Vals must have the same number of elements' ) ;

  result := nil;
  lFound := False;
  for i := 0 to Count - 1 do
  begin
    // Iterate over each property and value to see if it matches
    for j := 0 to High(pProps) do    // Iterate
    begin
      lFound := PropertyMatch(i,pProps[j],pVals[j]);

      if not lFound then
        break;
    end;    // for j

    if lFound then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
  end; // for i
end;


procedure TPerStream.SetSize(const Value: integer);
begin
  FStream.Size := Value ;
end;


function TtiObject.TopOfHierarchy: TtiObject;
  function _TopOfHierarchy( Value : TtiObject ) : TtiObject ;
  begin
    if Value.Owner = nil then
      result := Value
    else
      result := _TopOfHierarchy( Value.Owner ) ;
  end ;
begin
  result := _TopOfHierarchy( Self ) ;
end;


function TtiObject.GetOID: TOID;
begin
  // Create OID on demand
  {$IFNDEF OID_AS_INT64}
    if FOID = nil then
      FOID := gTIOPFManager.OIDFactory.CreateOID ;
  {$ENDIF}
  result := FOID ;
end;


constructor TtiObject.CreateNew( const pOwner : TtiObject ; const pDatabaseName : string = '' ; const pPerLayerName : string = '' );
begin
  Create ;
  if pOwner <> nil then
    Owner := pOwner ;
  ObjectState := posCreate ;
  {$IFDEF OID_AS_INT64}
    OID := gTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID ;
  {$ELSE}
    OID.GetNextValue( pDatabaseName, pPerLayerName ) ;
  {$ENDIF}
end;


constructor TtiObject.CreateNew( const pDatabaseName : string = '' ; const pPerLayerName : string = '' );
begin
  Create ;
  ObjectState := posCreate ;
  {$IFDEF OID_AS_INT64}
    OID := gTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID ;
  {$ELSE}
    OID.GetNextValue( pDatabaseName, pPerLayerName ) ;
  {$ENDIF}
end;


function TtiObjectList.First: TtiObject;
begin
  // TList can't handle this. Strange!
  if FList.Count > 0 then
    result := TtiObject( FList.First )
  else
    result := nil ;
end;


function _DoSortByOID( Item1, Item2 : pointer ) : integer ;
begin
  {$IFDEF OID_AS_INT64}
    if TtiObject( Item1 ).OID < TtiObject( Item2 ).OID then
      result := -1
    else if TtiObject( Item1 ).OID > TtiObject( Item2 ).OID then
      result := 1
    else
      result := 0 ;
  {$ELSE}
    result := TtiObject( Item1 ).OID.Compare( TtiObject( Item2 ).OID ) ;
  {$ENDIF}
end ;


procedure TtiObjectList.SortByOID;
begin
  List.Sort(_DoSortByOID);
end;


//procedure TtiObjectList.SortByDispOrder;
//begin
//  SortByProps(['DispOrder']);
//end;


function TtiObjectList.DoCompareByProps(
  pItem1   : Pointer ;
  pItem2   : Pointer ;
  const pSortProps : array of string;
  pAscendingOrder : Boolean = True) : integer ;
var
  i : integer ;
  lsPropName : string ;
  lValue1 : variant ;
  lValue2 : variant ;
begin
  result := 0 ;
  try
    for i := Low( pSortProps ) to High( pSortProps ) do
    begin
      lsPropName := pSortProps[i] ;
      lValue1 := TtiObject( pItem1 ).GetPropValue( lsPropName ) ;
      lValue2 := TtiObject( pItem2 ).GetPropValue( lsPropName ) ;

      if lValue1 < lValue2 then
      Begin
        If (pAscendingOrder) Then
          result := -1
        Else
          Result := 1;
      End
      else if lValue1 > lValue2 then
      Begin
        If (pAscendingOrder) Then
          result := 1
        Else
          Result := -1;
      End;
      if result <> 0 then
        Break ; //==>
    end ;

  except
    on e:exception do
      raise exception.Create(
        'Error in TtiObjectList._DoCompare PropName <' +
        lsPropName + '>' + e.Message ) ;
  end ;

end ;

// This method was cloned from Classes.TList.Sort and
// was added here to introduce an array of strings to
// hold property names for the sort.
procedure TtiObjectList.QuickSortByProps(
  SortList: PPointerList;
  L, R: Integer;
  const pSortProps: array of string;
  pAscendingOrder : Boolean = True);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while DoCompareByProps(SortList^[I], P, pSortProps, pAscendingOrder) < 0 do
        Inc(I);
      while DoCompareByProps(SortList^[J], P, pSortProps, pAscendingOrder) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortByProps(SortList, L, J, pSortProps, pAscendingOrder);
    L := I;
  until I >= R;
end;


procedure TtiObjectList.SortByProps(const pSortProps: array of string; pAscendingOrder : Boolean = True);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSortByProps(FList.List, 0, Count - 1, pSortProps, pAscendingOrder);
end;


procedure TtiObject.Read(const pDBConnectionName: string ; pPerLayerName : string = '' );
begin
  gTIOPFManager.Read( Self, pDBConnectionName, pPerlayerName ) ;
end;


procedure TtiObject.Read;
begin
  Read( '', '' ) ;
end;


procedure TtiObject.ReadPK(const pDBConnectionName: string ; pPerLayerName : string = '' );
begin
  gTIOPFManager.ReadPK( Self, pDBConnectionName, pPerLayerName ) ;
end;


procedure TtiObject.ReadPK;
begin
  ReadPK( '', '' ) ;
end;


procedure TtiObject.Save(const pDBConnectionName: string ; pPerLayerName : string = '' );
begin
  gTIOPFManager.Save( Self, pDBConnectionName, pPerLayerName ) ;
end;


procedure TtiObject.Save;
begin
  Save( '', '' ) ;
end;


procedure TtiObject.AssignFieldList(var pFieldList: TtiFieldList);
var
  i, lCount: Integer;
  lPropInfo: PPropInfo;
  lTempList: PPropList;
  lObject: TObject;
begin
  Assert(Assigned(pFieldList), 'Stringlist passed as parameter to GetPropListAsStrings is nil');
  pFieldList.Clear;
  lCount := GetPropList(PTypeInfo(Self.ClassInfo), lTempList);
  try
    for i := 0 to lCount - 1 do
    begin
      lPropInfo := lTempList^[i];
      if (lPropInfo^.PropType^.Kind = tkClass) and
         Assigned(lPropInfo^.GetProc) then
      begin
        lObject := GetObjectProp(self, lPropInfo);
        if (lObject <> nil) and (lObject is TtiFieldAbs) then
        begin
          pFieldList.Add(TtiFieldAbs(lObject));
        end;
      end;
    end;
  finally
    FreeMem(lTempList);
  end;
end;

{ TPerObjThreadList }

procedure TPerObjThreadList.Add(pObject: TtiObject; pDefDispOrdr: boolean);
begin
  Lock;
  try
    inherited Add( pObject, pDefDispOrdr ) ;
  finally
    Unlock;
  end ;
end;

procedure TPerObjThreadList.Clear;
begin
  Lock;
  try
    inherited Clear ;
  finally
    Unlock;
  end ;
end;

constructor TPerObjThreadList.Create;
begin
  inherited;
  FCriticalSection := TCriticalSection.Create;
end;

procedure TPerObjThreadList.Delete(i: integer);
begin
  Lock;
  try
    inherited Delete( i ) ;
  finally
    Unlock;
  end ;
end;

destructor TPerObjThreadList.Destroy;
begin
  FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TPerObjThreadList.Empty;
begin
  Lock;
  try
    inherited Empty ;
  finally
    Unlock;
  end ;
end;

function TPerObjThreadList.First: TtiObject;
begin
  Lock;
  try
    result := inherited First ;
  finally
    Unlock;
  end ;
end;

function TPerObjThreadList.GetCount: integer;
begin
  Lock;
  try
    result := inherited GetCount ;
  finally
    Unlock;
  end ;
end;

function TPerObjThreadList.IndexOf(pData: TObject): integer;
begin
  Lock;
  try
    result := inherited IndexOf( pData ) ;
  finally
    Unlock;
  end ;
end;

function TPerObjThreadList.Last: TtiObject;
begin
  Lock;
  try
    result := inherited Last ;
  finally
    Unlock;
  end ;
end;

function TPerObjThreadList.LastExcludeDeleted: TtiObject;
begin
  Lock;
  try
    result := inherited LastExcludeDeleted ;
  finally
    Unlock;
  end ;
end;

procedure TPerObjThreadList.Lock;
begin
  FCriticalSection.Enter;  // If you want timeout capability, use a mutex
end;

procedure TPerObjThreadList.MarkListItemsDirty;
begin
  Lock;
  try
    inherited MarkListItemsDirty ;
  finally
    Unlock;
  end ;
end;

procedure TPerObjThreadList.MarkListItemsForDeletion;
begin
  Lock;
  try
    inherited MarkListItemsForDeletion ;
  finally
    Unlock;
  end ;
end;

function TPerObjThreadList.Remove(pData: TtiObject): integer;
begin
  Lock;
  try
    result := inherited Remove( pData ) ;
  finally
    Unlock;
  end ;
end;

procedure TPerObjThreadList.SetItemOwner(const Value: TtiObject);
begin
  Lock;
  try
    inherited SetItemOwner( Value ) ;
  finally
    Unlock;
  end ;
end;

procedure TPerObjThreadList.SetItems(i: integer; const Value: TtiObject);
begin
  Lock;
  try
    inherited SetItems( i, Value ) ;
  finally
    Unlock;
  end ;
end;

procedure TPerObjThreadList.SortByProps(const pSortProps: array of string; pAscendingOrder : Boolean = True);
begin
  Lock;
  try
    inherited SortByProps( pSortProps, pAscendingOrder ) ;
  finally
    Unlock;
  end ;
end;


procedure TPerObjThreadList.SortByOID;
begin
  Lock;
  try
    inherited SortByOID ;
  finally
    Unlock;
  end ;
end;


procedure TPerObjThreadList.UnLock;
begin
  FCriticalSection.Leave;
end;


function TPerObjThreadList.FirstExcludeDeleted: TtiObject;
begin
  Lock;
  try
    result := inherited FirstExcludeDeleted ;
  finally
    Unlock;
  end ;
end;


{ TPerObjFactory }

function TPerObjFactory.Count: integer;
begin
  result := FList.Count ;
end;


constructor TPerObjFactory.Create;
begin
  inherited ;
  FList := TObjectList.Create ;
end;


function TPerObjFactory.CreateInstance( const pClassName: string): TtiObject;
var
  lPerObjClassMapping : TPerObjClassMapping ;
begin
  lPerObjClassMapping := FindByClassName( pClassName ) ;
  Assert( lPerObjClassMapping <> nil,
          'Request for unrgister class <' +
          pClassName + '>' ) ;
  result := lPerObjClassMapping.PerObjAbsClass.Create;
end;


function TPerObjFactory.CreateNewInstance(const pClassName: string; pOwner : TtiObject = nil ): TtiObject;
var
  lPerObjClassMapping : TPerObjClassMapping ;
begin
  lPerObjClassMapping := FindByClassName( pClassName ) ;
  Assert( lPerObjClassMapping <> nil,
          'Request for unrgister class <' +
          pClassName + '>' ) ;
  result := lPerObjClassMapping.PerObjAbsClass.CreateNew(pOwner) ;
end;


destructor TPerObjFactory.destroy;
begin
  FList.Free ;
  inherited;
end;


function TPerObjFactory.FindByClassName( const pClassName: string): TPerObjClassMapping;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to FList.Count - 1 do
    if SameText( TPerObjClassMapping( FList.Items[i] ).PerObjAbsClassName,
                 pClassName ) then
    begin
      result := TPerObjClassMapping( FList.Items[i] ) ;
      Break ; //==>
    end ;
end;


function TPerObjFactory.GetItems(pIndex: integer): TPerObjClassMapping;
begin
  result := TPerObjClassMapping( FList.Items[pIndex] ) ;
end;


function TPerObjFactory.IsRegistered(const pClassName: string): boolean;
begin
  result := FindByClassName( pClassName ) <> nil ;
end;


procedure TPerObjFactory.RegisterClass(const pClassName: string ; pClass : TtiClass);
var
  lPerObjClassMapping : TPerObjClassMapping ;
begin
  lPerObjClassMapping := FindByClassName( pClassName ) ;
  Assert( lPerObjClassMapping = nil,
          'Attempt to register duplicate class mapping <' +
          pClassName + '>' ) ;
  lPerObjClassMapping := TPerObjClassMapping.Create ;
  lPerObjClassMapping.PerObjAbsClassName := pClassName ;
  lPerObjClassMapping.PerObjAbsClass := pClass ;
  FList.Add( lPerObjClassMapping ) ;
end;


procedure TtiObjectList.Assign(const pSource: TtiObject);
begin
  Clear ;
  inherited Assign( pSource ) ;
end;


//function TtiObject.GetDispOrder: integer;
//begin
//  Result := FiDispOrder;
//end;


procedure TtiObject.SetAllObjectStates(const pObjectState: TPerObjectState);
var
  lVis : TVisSetAllObjectStates ;
begin
  lVis := TVisSetAllObjectStates.Create ;
  try
    lVis.ObjectState := pObjectState ;
    Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end;


procedure TPerObjFactory.UnRegisterClass(const pClassName: string);
var
  lPerObjClassMapping : TPerObjClassMapping ;
begin
  lPerObjClassMapping := FindByClassName( pClassName ) ;
  Assert( lPerObjClassMapping <> nil, 'Attempt to unregister class that is not retistered.' ) ;
  FList.Remove( lPerObjClassMapping ) ;
end;


{ TVisSetAllObjectStates }

function TVisSetAllObjectStates.AcceptVisitor: boolean;
begin
  result := ( Visited is TtiObject ) and
            ( TtiObject( Visited ).ObjectState in [ posClean, posUpdate ]) ;
end;


procedure TVisSetAllObjectStates.Execute(const pVisited: TtiVisited);
begin
  Inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

  TtiObject( Visited ).ObjectState := FObjectState ;
end;


procedure TtiObject.ReadThis(const pDBConnectionName: string ; pPerLayerName : string = '' );
begin
  {$IFDEF OID_AS_INT64}
    Assert( OID <> cNullOIDInteger, 'OID not assigned');
  {$ELSE}
    Assert( not OID.IsNull, 'OID not assigned');
  {$ENDIF}
  gTIOPFManager.ReadThis( Self, pDBConnectionName, pPerLayerName ) ;
end;


procedure TtiObject.ReadThis;
begin
  ReadThis( '', '' ) ;
end;


procedure TtiObjectList.AssignPublicProps(pSource: TtiObject);
begin
  inherited AssignPublicProps(pSource) ;
  
// Don't set these here, they will be set in a classes constructor  
//  OwnsObjects := TtiObjectList(pSource).OwnsObjects;
//  AutoSetItemOwner := TtiObjectList(pSource).AutoSetItemOwner;
end;


function TtiObjectList.Find(pOIDToFind: TOID): TtiObject;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    if OIDEquals( Items[i].OID, pOIDToFind ) then
    begin
      result := Items[i];
      Exit ; //==>
    end ;
  result := nil;
end;


function TtiObjectList.Find(pOIDToFind: TOID; pSortType: TtiPerObjListSortType): TtiObject;
var
  FindIndex: Integer;
begin
  FindIndex := IndexOf(pOIDToFind, pSortType);
  if FindIndex >= 0 then
    Result := Items[FindIndex]
  else
    Result := nil ;
end;


function TtiObjectList.IndexOf(pOIDToFind: TOID; pSortType: TtiPerObjListSortType = stNone): integer;
begin
  case pSortType of
  stOID : Result := IndexOfBinary(pOIDToFind);
  stNone: Result := IndexOfFullScan(pOIDToFind);
  else
    raise EtiOPFInternalException.Create(cErrorInvalidSortType);
  end;
end;


{ TPerObjErrors }

procedure TPerObjErrors.Add(pObject: TPerObjError; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;


procedure TPerObjErrors.AddError(
  const pErrorProperty : string ;
  const pErrorMessage: string;
  pErrorCode: integer);
var
  lError : TPerObjError ;
begin
  lError := TPerObjError.Create ;
  lError.ErrorProperty := pErrorProperty ;
  lError.ErrorMessage  := pErrorMessage ;
  lError.ErrorCode     := pErrorCode ;
  Add( lError ) ;
end;


procedure TPerObjErrors.AddError(const pErrorMessage: string);
begin
  AddError('', pErrorMessage, -1);
end;


procedure TPerObjErrors.AddError(
  const pErrorProperty : string ;
  const pErrorMessage: string);
begin
  AddError(pErrorProperty, pErrorMessage, -1);
end;


function TPerObjErrors.FindByMessage(const pMessage: string): TPerObjError;
var
  i : integer ;
begin
  result := nil ;
  for i := 0 to Count - 1 do
    if SameText( Items[i].ErrorMessage, pMessage ) then
    begin
      result := Items[i] ;
      Exit ; //==>
    end ;
end;

function TPerObjErrors.GetAsString: string;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    if Result <> '' then
      Result := Result + CrLf;
    Result := Result + Items[i].ErrorMessage;
  end;
end;


function TPerObjErrors.GetItems(i: integer): TPerObjError;
begin
  result := TPerObjError( inherited GetItems( i )) ;
end;


procedure TPerObjErrors.SetItems(i: integer; const Value: TPerObjError);
begin
  inherited SetItems( i, Value ) ;
end;


{ TPerObjError }

function TPerObjError.GetOwner: TPerObjErrors;
begin
  result := TPerObjErrors( inherited GetOwner );
end;


procedure TPerObjError.SetOwner(const Value: TPerObjErrors);
begin
  inherited SetOwner( Value ) ;
end;


function TtiObject.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  Assert( pErrors.TestValid(TPerObjErrors), cTIInvalidObjectError ) ;
  result := true ;
  pErrors.Clear ;
end;


function TtiObject.IsValid(var pErrorMessage: string): boolean;
var
  lErrors : TPerObjErrors ;
  i : integer ;
begin
  lErrors := TPerObjErrors.Create ;
  try
    result := IsValid( lErrors ) ;
    for i := 0 to lErrors.Count - 1 do
    begin
      pErrorMessage := tiAddTrailingValue( pErrorMessage, Cr ) ;
      pErrorMessage := pErrorMessage + lErrors.Items[i].ErrorMessage ;
    end ;
  finally
    lErrors.Free ;
  end ;
end;


function TtiObject.IsValid: boolean;
var
  lErrors : TPerObjErrors ;
begin
  lErrors := TPerObjErrors.Create ;
  try
    result := IsValid( lErrors ) ;
  finally
    lErrors.Free ;
  end ;
end;


function TtiObject.IsValid(const pStrings: TStrings): boolean;
var
  lMessage : string ;
begin
  result := IsValid( lMessage ) ;
  pStrings.Text := lMessage ;
end;


function TtiObject.IsValid( const pStrings : TStrings ; pAppend : boolean ) : boolean ; 
var
  lsl : TStringList ;
  i   : integer ;
begin
  lsl := TStringList.Create ;
  try
    result := IsValid( lsl ) ;
    if not pAppend then
      pStrings.Clear ;
    for i := 0 to lsl.count - 1 do
      pStrings.Add(lsl.strings[i]);
  finally
    lsl.Free ;
  end;
end;


destructor TtiObject.Destroy;
begin
  {$IFNDEF OID_AS_INT64}
    FOID.Free ;
  {$ENDIF}
  FObserverList.Free;
  FObserverList := nil;
  inherited;
end;


function TtiObject.Find(pOIDToFindAsString: string): TtiObject;
{$IFNDEF OID_AS_INT64}
  var
    lOID : TOID ;
{$ENDIF}
begin
  {$IFDEF OID_AS_INT64}
    result := Find( StrToInt( pOIDToFindAsString )) ;
  {$ELSE}
    lOID := gTIOPFManager.OIDFactory.CreateOID ;
    try
      lOID.AsString := pOIDToFindAsString ;
     result := Find( lOID ) ;
    finally
      lOID.Free ;
    end ;
  {$ENDIF}
end;


function TtiObjectList.Find(pOIDToFindAsString: string): TtiObject;
var
  i : integer ;
{$IFNDEF OID_AS_INT64}
  lOIDToFind : TOID ;
{$ENDIF}
begin
  {$IFDEF OID_AS_INT64}
    for i := 0 to Count - 1 do
      if Items[i].OID = StrToInt( pOIDToFindAsString ) then
      begin
        result := Items[i];
        Exit ; //==>
      end ;
    result := nil ;
  {$ELSE}
    lOIDToFind := gTIOPFManager.OIDFactory.CreateOID ;
    try
      lOIDToFind.AsString := pOIDToFindAsString ;
      for i := 0 to Count - 1 do
        if Items[i].OID.Equals( lOIDToFind ) then
        begin
          result := Items[i];
          Exit ; //==>
        end ;
      result := nil;
    finally
      lOIDToFind.Free ;
    end ;
  {$ENDIF}
end;


function TtiObject.FindAll(pPerObjFindMethodExt: TPerObjFindMethodExt; pList: TList; pUserContext: Pointer ): integer;
var
  lVis : TVisPerObjFind ;
begin
  if pList <> nil then
    pList.Clear ;
  lVis := TVisPerObjFind.Create ;
  try
    lVis.FoundList := pList ;
    lVis.UserContext := pUserContext ;
    lVis.FPerObjFindMethodExt := pPerObjFindMethodExt ;
    self.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
  result := pList.Count ;
end;


procedure TtiObject.DoFindAllNotUnique(pPerObjAbs: TtiObject;
  var pbFound: boolean; pData : TtiObject);
begin
  Assert( Assigned( pData ), 'pData not assigned' ) ;
  pbFound :=
    ( OIDEquals( pPerObjAbs.OID, pData.OID )) and
    ( pPerObjAbs <> pData ) and
    ( not pPerObjAbs.Deleted ) ;
end;


function TtiObject.FindAll(pPerObjFindMethodData: TPerObjFindMethodData;
  pList: TList; pData: TtiObject): integer;
var
  lVis : TVisPerObjFind ;
begin
  if pList <> nil then
    pList.Clear ;
  lVis := TVisPerObjFind.Create ;
  try
    lVis.FoundList := pList ;
    lVis.Data := pData ;
    lVis.PerObjFindMethodData := pPerObjFindMethodData ;
    self.Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
  result := pList.Count ;
end;


function TtiObject.GetPropValue(const pPropName: string): Variant;
var
  lDate : TDateTime ;
  lbValue : boolean ;
  lTypeKind : TtiTypeKind ;
begin
  if SameText( pPropName, 'OID' ) then
  {$IFDEF OID_AS_INT64}
    result := Integer(OID)
  {$ELSE}
    result := OID.AsVariant
  {$ENDIF}
//  else if SameText( pPropName, 'DispOrder' ) then
//    result := DispOrder
  else
  begin
    Assert( IsPublishedProp( Self, pPropName ), pPropName + ' is not a published property on ' + ClassName ) ;
    try
      lTypeKind := tiGetSimplePropType( Self, pPropName ) ;
      case lTypeKind of
      tiTKDateTime : begin
                       lDate := TypInfo.GetPropValue( Self, pPropName ) ;
                       result := lDate ;
                     end;
      tiTKBoolean :  begin
                       lbValue := TypInfo.GetPropValue( Self, pPropName ) ;
                       result := lbValue ;
                     end;
      else
        result := TypInfo.GetPropValue( Self, pPropName ) ;
      end ;
    except
      on e:exception do
        raise EtiOPFProgrammerException.CreateFmt(
          cErrorGettingProperty, [pPropName, ClassName, e.Message]);
    end ;
  end ;
end;


procedure TtiObject.SetPropValue(const pPropName: string; const pPropValue: Variant);
var
  ldtValue : TDateTime ;
  lsValue : string ;
  liValue : integer ;
  lbValue : boolean ;
  lPropTypeKind : TtiTypeKind ;
  lValueTypeKind : TtiTypeKind ;
begin
  // OID is a special case. Should we publish OID?
  if SameText( pPropName, 'OID' ) then
  begin
    {$IFDEF OID_AS_INT64}
      OID := Integer(pPropValue) ;
    {$ELSE}
      OID.AsVariant := pPropValue ;
    {$ENDIF}
    Exit ; //==>
  end ;

  if not IsPublishedProp( Self, pPropName ) then
    raise EtiOPFProgrammerException.CreateFmt(cErrorAttemptToSetNonPublishedProperty,
      [pPropName,ClassName, VarToStr(pPropValue) ]);

  try

    lPropTypeKind := tiGetSimplePropType( Self, pPropName ) ;
    case lPropTypeKind of
    tiTKDateTime : begin
                     ldtValue := VarToDateTime( pPropValue ) ;
                     TypInfo.SetPropValue( Self, pPropName, ldtValue ) ;
                   end;
    tiTKBoolean : begin
                    lValueTypeKind := tiVarSimplePropType( pPropValue ) ;
                    case lValueTypeKind of
                    tiTKString : begin
                                   lsValue := pPropValue ;
                                   if SameText( lsValue, 'true' ) or
                                     ( lsValue = '1' ) or
                                     SameText( lsValue, 't' ) then
                                     TypInfo.SetPropValue( Self, pPropName, 1 )
                                   else
                                     TypInfo.SetPropValue( Self, pPropName, 0 );
                                 end ;
                    tiTKInteger : begin
                                    liValue := pPropValue ;
                                    if liValue = 0 then
                                      TypInfo.SetPropValue( Self, pPropName, 0 )
                                    else
                                      TypInfo.SetPropValue( Self, pPropName, 1 );
                                  end ;
                    tiTKBoolean : begin
                                    lbValue := pPropValue ;
                                    if lbValue then
                                      TypInfo.SetPropValue( Self, pPropName, 1 )
                                    else
                                      TypInfo.SetPropValue( Self, pPropName, 0 );
                                  end ;
                    else
                      raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
                        [ClassName, pPropName, 'Unknown type' ]);
                    end
                  end ;
    else
      TypInfo.SetPropValue( Self, pPropName, pPropValue ) ;
    end ;

  except
    on e:exception do
      raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
        [ClassName, pPropName, e.message ]);
  end ;
end;


function TtiObject.IsReadWriteProp(const pPropName: string): boolean;
begin
  result :=
    SameText( pPropName, 'OID' ) or
    tiIsReadWriteProp( Self, pPropName ) ;   
end;


{ TPerObjClassMapping }

constructor TPerObjClassMapping.Create;
begin
  inherited ;
end;


{ TPerObjFieldAbs }

constructor TtiFieldAbs.Create(const pOwner: TtiObject; const pNullValidation: TtiNullValidation);
begin
  inherited Create;
  FOwner := pOwner ;
  FNullValidation := pNullValidation;
  Clear;
end;


constructor TtiFieldAbs.Create(const pOwner: TtiObject);
begin
  Create(pOwner, nvAllowNull);
end;


procedure TtiFieldAbs.Clear;
begin
  FIsNull := True ;
end;


procedure TtiFieldAbs.SetValue;
begin
  FIsNull := False;
end;


function TtiFieldAbs.IsValidValue(const pErrors: TPerObjErrors): Boolean;
begin
  Assert( Owner.TestValid(TtiObject), cTIInvalidObjectError );
  Result := (NullValidation = nvAllowNull) or (not IsNull);
  if Assigned(pErrors) and (not Result) then
    pErrors.AddError(FieldName,
                     Format(cErrorFieldNotAssigned, [Owner.ClassName + '.' + FieldName, Owner.OID]));
end;

{ TtiFieldString }

constructor TtiFieldString.Create(
  const pOwner: TtiObject;
  const pNullValidation: TtiNullValidation;
  const pMaxLength: Integer);
begin
  inherited Create(pOwner, pNullValidation);
  FMaxLength := pMaxLength;
end;

procedure TtiFieldString.Clear;
begin
  inherited;
  FValue := '';
end;

function TtiFieldString.GetAsString: string;
begin
  Result := FValue;
end;

procedure TtiFieldString.SetAsString(const pValue: string);
begin
  FValue := pValue;
  SetValue;
end;

function TtiFieldString.IsValidValue(const pErrors : TPerObjErrors): Boolean;
begin
  Result := inherited IsValidValue(pErrors);
  if Result then begin
    Result := (MaxLength = 0) or (Length(AsString) <= MaxLength);
    if Assigned(pErrors) and (not Result) then
      pErrors.AddError(FieldName,
                       Format(cErrorFieldTooLong,
                              [ClassName + '.' + FieldName, MaxLength, Length(AsString)]));
  end;
end;

function TtiFieldString.Equals(pCompareWith: TtiFieldAbs): Boolean;
begin
  Assert( pCompareWith.TestValid(TtiFieldString), cErrorTIPerObjAbsTestValid );
  Result := AsString = pCompareWith.AsString;
end;

{ TtiFieldInteger }

constructor TtiFieldInteger.Create(
  const pOwner: TtiObject;
  const pNullValidation: TtiNullValidation;
  const pMaxDigits: Integer);
begin
  inherited Create(pOwner, pNullValidation);
  FMaxDigits := pMaxDigits;
end;

procedure TtiFieldInteger.Clear;
begin
  inherited;
  FValue := 0;
end;

function TtiFieldInteger.GetAsString: string;
begin
  Result := IntToStr(FValue);
end;

procedure TtiFieldInteger.SetAsString(const pValue: string);
begin
  if pValue <> '' then
    FValue := StrToInt(pValue)
  else
    FValue := 0;
  SetValue;
end;

procedure TtiFieldInteger.SetAsInteger(const pValue: Int64);
begin
  FValue := pValue;
  SetValue;
end;

function TtiFieldInteger.IsValidValue(const pErrors : TPerObjErrors): Boolean;
begin
  Result := inherited IsValidValue(pErrors);
  if Result then begin
    // The sign of the number is not considered to be a digit. ie. A maximum
    // digits = 3 can validly store 999 and -999 but not 9999 or -9999
    Result := (MaxDigits = 0) or
              ((AsInteger >= 0) and (Length(AsString) <= MaxDigits)) or
              ((AsInteger < 0) and (Length(AsString) <= (MaxDigits+1)));
    if Assigned(pErrors) and (not Result) then
      pErrors.AddError(FieldName,
                       Format(cErrorFieldTooLong,
                              [ClassName + '.' + FieldName, MaxDigits, Length(AsString)]));
  end;
end;

function TtiFieldInteger.Equals(pCompareWith: TtiFieldAbs): Boolean;
begin
  Assert( pCompareWith.TestValid(TtiFieldAbs), cErrorTIPerObjAbsTestValid );
  Result := AsInteger = (pCompareWith as TtiFieldInteger).AsInteger;
end;

{ TtiFieldFloat }

procedure TtiFieldFloat.Clear;
begin
  inherited;
  FValue := 0 ;
end;

constructor TtiFieldFloat.Create(const pOwner: TtiObject;
  const pNullValidation: TtiNullValidation; const pPrecision: Integer);
begin
  inherited Create(pOwner, pNullValidation);
  SetPrecision(pPrecision);
end;

constructor TtiFieldFloat.Create(const pOwner: TtiObject);
begin
  Create(pOwner, nvAllowNull);
end;

constructor TtiFieldFloat.Create(const pOwner: TtiObject;
  const pNullValidation: TtiNullValidation);
begin
  // Precision of 0 will return a string to what ever precision the float values is stored
  Create(pOwner, pNullValidation, 0);
end;

function TtiFieldFloat.Equals(pCompareWith: TtiFieldAbs): Boolean;
var
  lF1: extended;
  lF2: extended;
begin
  Assert( pCompareWith.TestValid(TtiFieldFloat), cErrorTIPerObjAbsTestValid );
  lF1 := AsFloat;
  lF2 := (pCompareWith as TtiFieldFloat).AsFloat;
  Result := SameValue(lF1, lF2, Epsilon);
end;

function TtiFieldFloat.GetAsString: string;
begin
  if FPrecision = 0 then
    Result := FloatToStr(FValue)
  else
    Result := tiFloatToStr(FValue, FPrecision);
end;

procedure TtiFieldFloat.SetAsFloat(const pValue: Extended);
begin
  FValue := pValue;
  SetValue;
end;

procedure TtiFieldFloat.SetAsString(const pValue: string);
begin
  if pValue <> '' then
    FValue := StrToFloat(pValue)
  else
    FValue := 0;
  SetValue;
end;

procedure TtiFieldFloat.SetPrecision(const Value: Integer);
begin
  FPrecision := Value;
  FEpsilon := Power(10, -(Value+1))*2;
end;

{ TtiFieldBoolean }

procedure TtiFieldBoolean.Clear;
begin
  inherited;
  FValue := False ;
end;

function TtiFieldBoolean.GetAsString: string;
begin
{$IFDEF BOOLEAN_CHAR_1}
  if FValue then
    result := 'T'
  else
    result := 'F' ;
{$ELSE}
  if FValue then
    result := 'TRUE'
  else
    result := 'FALSE' ;
{$ENDIF}
end;


procedure TtiFieldBoolean.SetAsString(const pValue: string);
begin
  if SameText( pValue, 'TRUE' ) or
     SameText( pValue, 'T' ) or
     SameText( pValue, 'Y' ) or
     ( pValue = '1' ) then
  begin
    FValue := True;
    SetValue;
  end
  else if SameText( pValue, 'FALSE' ) or
     SameText( pValue, 'F' ) or
     SameText( pValue, 'N' ) or
     ( pValue = '0' ) then
  begin
    FValue := False;
    SetValue;
  end
  else
    Clear; //Value is null
end;


procedure TtiFieldBoolean.SetAsBoolean(const pValue: Boolean);
begin
  FValue := pValue;
  SetValue;
end;


function TtiFieldBoolean.Equals(pCompareWith: TtiFieldAbs): Boolean;
begin
  Assert( pCompareWith.TestValid(TtiFieldBoolean), cErrorTIPerObjAbsTestValid );
  Result := AsBoolean = (pCompareWith as TtiFieldBoolean).AsBoolean;
end;


{ TtiFieldDateTime }

procedure TtiFieldDateTime.Clear;
begin
  inherited;
  FValue := 0;
end;


function TtiFieldDateTime.GetAsString: string;
begin
  Result := tiDateTimeAsXMLString(FValue);
end;


procedure TtiFieldDateTime.SetAsString(const pValue: string);
begin
  FValue := tiXMLStringToDateTime(pValue);
  SetValue;
end;


procedure TtiFieldDateTime.SetAsDateTime(const pValue: TDateTime);
begin
  FValue := pValue;
  SetValue;
end;


function TtiFieldAbs.GetFieldName: string;
var
  lsl: TStringList;
  i : Integer;
begin
  if FFieldName = '' then
  begin
    Assert( Owner.TestValid(TtiObject), cTIInvalidObjectError );
    lsl:= TStringList.Create;
    try
      tiGetPropertyNames(Owner,lsl, [tkClass]);
      for i := 0 to lsl.Count - 1 do
        if GetObjectProp(Owner, lsl.Strings[i]) = Self then
        begin
          FFieldName := lsl.Strings[i];
          Break ; //==>
        end;
        if FFieldName = '' then
          raise Exception.CreateFmt(cErrorUnableToDetermineFieldName, [Owner.ClassName]);
    finally
      lsl.Free;
    end;
  end ;
  Result := FFieldName;
end;


function TtiFieldDateTime.Equals(pCompareWith: TtiFieldAbs): Boolean;
begin
  Assert( pCompareWith.TestValid(TtiFieldDateTime), cErrorTIPerObjAbsTestValid );
  Result := SameValue(AsDateTime, (pCompareWith as TtiFieldDateTime).AsDateTime, cdtOneSecond/2);
end;


{ TtiFieldList }

constructor TtiFieldList.Create;
begin
  inherited;
  {NB: Overriding the default here so that child objects will not automatically be
  freed when the list is freed. If you want the standard behaviour of an Objectlist
  then just set the OwnsObjects property to True after Create.}
  OwnsObjects := False;
end;


function TtiFieldList.Add(AObject: TtiFieldAbs): Integer;
begin
  Result := inherited Add(AObject);
end;


function TtiFieldList.GetItem(Index: Integer): TtiFieldAbs;
begin
  Result := TtiFieldAbs(inherited Items[Index]);
end;


procedure TtiFieldList.SetItem(Index: Integer; AObject: TtiFieldAbs);
begin
  inherited Items[Index] := AObject;
end;


procedure TtiObject.ForceAsCreate(const pDatabaseName : string = '' ; const pPerLayerName : string = '');
begin
  {$IFDEF OID_AS_INT64}
    OID := gTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID ;
  {$ELSE}
    OID.GetNextValue( pDatabaseName, pPerLayerName ) ;
  {$ENDIF}
  ObjectState := posCreate;
end;


function TtiObject.PropType(const pPropName: string): TtiTypeKind;
begin
  Assert(pPropName <> '', 'pPropName not assigned');
  Result := tiGetSimplePropType(Self, pPropName);
end;


function TtiObjectList.FindInHierarchy(pOIDToFind: TOID): TtiObject;
begin
  Result := Find(pOIDToFind);
  if Result = nil then
    Result := inherited Find(pOIDToFind);
end;


function TtiObject.AsDebugString: string;
var
  lVisitor : TVisTIObjectAsDebugString ;
begin
  lVisitor := TVisTIObjectAsDebugString.Create(True);
  try
    Self.Iterate(lVisitor);
    result := lVisitor.Text;
  finally
    lVisitor.Free;
  end;
end;


procedure TtiObject.SetOID(const Value: TOID);
begin
  FOID := Value;
end;


function TtiObject.PropCount(pPropFilter: TTypeKinds = ctkSimple): integer;
var
  lsl : TStringList ;
begin
  lsl := TStringList.Create ;
  try
    tiGetPropertyNames( Self, lsl, pPropFilter ) ;
    result := lsl.Count ;
  finally
    lsl.Free ;
  end ;
end;


procedure TtiFieldAbs.SetIsNull(const Value: Boolean);
begin
  if Value then
    Clear
  else
    FIsNull := false;
end;


function TtiObjectList.IndexOfBinary(AOIDToFind: TOID): integer;
var
  LLow: Integer;
  LHigh: Integer;
  LMid: Integer;
  LCompareResult: Integer;
begin
  LLow := 0;
  LHigh := Count - 1;
  while LLow <= LHigh do
  begin
    LMid := ((LHigh + LLow) div 2);    // Floor((LHigh + LLow) / 2);
    LCompareResult := OIDCompare( Items[LMid].OID, AOIDToFind );
    if LCompareResult > 0 then
      LHigh := LMid - 1
    else if LCompareResult < 0 then
      LLow := LMid + 1
    else begin
      Result := LMid;
      Exit ; //==>
    end;
  end;
  Result := -1;
end;


function TtiObjectList.IndexOfFullScan(AOIDToFind: TOID): integer;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    if OIDEquals( Items[i].OID, AOIDToFind ) then
    begin
      Result := i;
      Exit ; //==>
    end ;
  Result := -1;
end;


function TtiObjectList.FirstExcludeDeleted: TtiObject;
var
  i : integer ;
begin
  for i := 0 to Count-1 do
    if not Items[i].Deleted then
    begin
      result := Items[i] ;
      Exit ; //==>
    end ;
  result := nil ;
end;


procedure TtiObjectList.CompareWith(AList: TtiObjectList; AInBothAndEquals,
  AInBothAndNotEquals, AIn1Only, AIn2Only: TtiObjectListCompareEvent);
var
  i: Integer;
  L: TtiObject;
begin
  Assert(AList.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(Assigned(AInBothAndEquals),    'AInBothAndEquals not assigned');
  Assert(Assigned(AInBothAndNotEquals), 'AInBothAndNotEqualsnot assigned');
  Assert(Assigned(AIn1Only),            'AIn1Onlynot assigned');
  Assert(Assigned(AIn2Only),            'AIn2Onlynot assigned');

  for i:= 0 to Count - 1 do
  begin
    L:= AList.Find(Items[i].OID);
    if L = nil then
      AIn1Only(Items[i], nil)
    else if Items[i].Equals(L) then
      AInBothAndEquals(Items[i], L)
    else
      AInBothAndNotEquals(Items[i], L);
  end;

  for i:= 0 to AList.Count - 1 do
  begin
    L:= Find(AList.Items[i].OID);
    if L = nil then
      AIn2Only(nil, AList.Items[i]);
  end;

end;


procedure TtiObject.AttachObserver(pObserver: TtiObject);
begin
  { To conserve memory, we only create FObserverList when needed }
  if not Assigned(FObserverList) then
      FObserverList := TList.Create;
  if FObserverList.IndexOf( pObserver ) = -1 then
    FObserverList.Add( pObserver );
end;


procedure TtiObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;


procedure TtiObject.DetachObserver(pObserver: TtiObject);
begin
  FObserverList.Remove( pObserver );
  { To conserve memory, we free FObserverList when not used anymore }
  if FObserverList.Count = 0 then
  begin
    FObserverList.Free;
    FObserverList := nil;
  end;
end;


procedure TtiObject.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    NotifyObservers;
end;


procedure TtiObject.NotifyObservers;
var
  ObjectIndex: Integer;
  Observer: TtiObject;
begin
  if not Assigned(FObserverList) then
    Exit; //==>

  for ObjectIndex := 0 to FObserverList.Count - 1 do
  begin
    Observer := TtiObject(FObserverList.Items[ObjectIndex]);
    Observer.Update( self );
  end;
end;


procedure TtiObject.Update(pSubject: TtiObject);
begin
  { Do nothing here. This will be implemented in decendant classes when needed }
end;


function TtiObject.GetObserverList: TList;
begin
  if not Assigned(FObserverList) then
    FObserverList := TList.Create;
  Result := FObserverList;
end;


end.

