{:@summary The base unit for tiOPF.
   @desc Contains all the base classes used throughout the framework.}

unit tiObject;

{$I tiDefines.inc}

interface
uses
   tiBaseObject
  ,tiVisitor
  ,tiOID
  ,tiRTTI
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
  cErrorSettingProperty      = 'Error setting property %s.%s Message %s';
  cErrorGettingProperty      = 'Error getting property %s.%s Message %s';
  cErrorInvalidObjectState   = 'Invalid ObjectState';
  cErrorNoFindMethodAssigned = 'No find method assigned';
  cErrorAttemptToSetNonPublishedProperty = 'Attempt to set non-published property %s.%s to %s';
  cErrorInvalidSortType = 'Invalid TtiPerObjListSortType';
  CErrorDefaultOIDGeneratorNotAssigned = 'Default OIDGenerator not assigned. You must register an instance of TOIDGenerator with the global GTIOPFManager.';

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
                    );

  {: The possible values to be displayed by TtiObjectAsDebugString}
  TtiObjectAsDebugStringValues =
    (adsData, adsClassName, adsObjectState, adsOID, adsCaption, adsDeleted, adsChildren);

  TtiObjectAsDebugStringValuesToShow = set of TtiObjectAsDebugStringValues;

const
  CTIAsDebugStringDataAll =
    [adsData, adsClassName, adsObjectState, adsOID, adsCaption, adsDeleted, adsChildren];

  CTIAsDebugStringDataAndChildren =
    [adsData, adsChildren];

type

  TtiObject = class;
  TtiObjectList = class;
  TtiObjectErrors = class;
  TtiObjectError  = class;

  TObjForEachMethod        = procedure(AObject: TObject) of object;
  TObjForEachMethodRegular = procedure(AObject: TObject);
  TPerObjFindMethod = procedure(AObject : TtiObject; var AFound : boolean) of object;
  TPerObjFindMethodExt = procedure(AObject : TtiObject; var AFound : boolean; AUserContext: Pointer) of object;
  TPerObjFindMethodData = procedure(AObject : TtiObject; var AFound : boolean; AData : TtiObject) of object;
  TPerObjForEachMethod        = procedure(AObject : TtiObject) of object;
  TPerObjForEachMethodRegular = procedure(AObject : TtiObject);
  TtiObjectEvent = procedure(const AData: TtiObject) of object;

  {: Does the field allow null values. This is one of the possibly many tests
    that the field will make when the TestValidValue function is called. }
  TtiNullValidation = (
                        nvAllowNull = 0
                       ,nvNotNull = 1
                     );

  {: Is the TtiObjectList sorted by OID or not sorted at all?}
  TtiPerObjListSortType =
                       ( stNone
                         ,stOID
                      );

  {: Abstract persistent field for use with TtiObject The usual way to add
     data to TtiObject is with published properties. These can be simple data
     types (String, Integer, Float, DateTime) or object data types that you
     define. Simple data types will not handle NULL values so if NULL management
     is required, you should use persistent fields. Using persistent fields
     however, makes the code more complex and harder to maintain.}
  TtiFieldAbs = class(TtiBaseObject)
  private
    FOwner: TtiObject;
    FFieldName: string;
    FNullValidation: TtiNullValidation;
    FIsNull: Boolean;
    procedure SetIsNull(const AValue: Boolean);
  protected
    procedure   Clear; virtual;
    procedure   SetValue; virtual;
    procedure   SetAsString(const AValue: string); virtual; abstract;
    function    GetAsString: string;               virtual; abstract;
    function    GetFieldName: string; virtual;
    procedure   SetFieldName(const AValue: string); virtual;
  public
    constructor Create(const AOwner: TtiObject); overload; virtual;
    constructor Create(const AOwner: TtiObject; const ANullValidation: TtiNullValidation); overload; virtual;
    function    IsValidValue(const AErrors : TtiObjectErrors = nil): Boolean; virtual;
    function    Equals(ACompareWith: TtiFieldAbs): Boolean; virtual; abstract;
    procedure   Assign(AAssignFrom: TtiFieldAbs); virtual; abstract;

    property    Owner:            TtiObject         read FOwner;
    property    FieldName:        string            read GetFieldName write SetFieldName;
    property    NullValidation:   TtiNullValidation read FNullValidation write FNullValidation;
    property    AsString:         string            read GetAsString Write SetAsString;
    property    IsNull:           Boolean           read FIsNull write SetIsNull;

  end;

  {: Concrete persistent string field}
  TtiFieldString = class(TtiFieldAbs)
  private
    FValue: string;
    FMaxLength: Integer;
  protected
    procedure   Clear; override;
    procedure   SetAsString(const AValue: string); override;
    function    GetAsString: string;              override;
  public
    // A maximum length of 0 (zero) means no maximum length
    constructor Create(const AOwner: TtiObject;
                       const ANullValidation: TtiNullValidation = nvAllowNull;
                       const AMaxLength: Integer = 0); reintroduce; overload; virtual;
    function    IsValidValue(const AErrors : TtiObjectErrors = nil): Boolean; override;
    function    Equals(ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(AAssignFrom: TtiFieldAbs); override;

    property    MaxLength: Integer read FMaxLength;
  end;

  TtiFieldStringMethod = class;
  TtiStringFieldMethodReadEvent = procedure(ASender: TtiFieldStringMethod; var AValue: string) of object;

  {: Concrete persistent string field that uses a user defined method to access it's data}
  TtiFieldStringMethod = class(TtiFieldString)
  private
    FReadEvent: TtiStringFieldMethodReadEvent;
  protected
    procedure   Clear; override;
    procedure   SetAsString(const AValue: string); override;
    function    GetAsString: string;              override;
  public
    constructor Create(const AOwner: TtiObject;
                       const AReadMethod: TtiStringFieldMethodReadEvent); reintroduce; overload; virtual;
    function    IsValidValue(const AErrors : TtiObjectErrors = nil): Boolean; override;
    function    Equals(ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(AAssignFrom: TtiFieldAbs); override;
  end;

  // ToDo: Implement checking for null in GetAsString
  {: Concrete persistent Int64 field}
  TtiFieldInteger = class(TtiFieldAbs)
  private
    FValue: Int64;
    FMaxDigits: Integer;
  protected
    procedure   Clear; override;
    procedure   SetAsString(const AValue: string);  override;
    function    GetAsString: string;               override;
    procedure   SetAsInteger(const AValue: Int64); virtual;
  public
    // A maximum digits of 0 (zero) means no maximum digits
    constructor Create(const AOwner: TtiObject;
                       const ANullValidation: TtiNullValidation = nvAllowNull;
                       const AMaxDigits: Integer = 0); reintroduce; overload; virtual;
    function    IsValidValue(const AErrors : TtiObjectErrors = nil): Boolean; override;
    function    Equals(ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(AAssignFrom: TtiFieldAbs); override;

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
    procedure   Clear; override;
    procedure   SetAsString(const AValue: string);  override;
    function    GetAsString: string;               override;
    procedure   SetAsFloat(const AValue: Extended); virtual;
    procedure   SetPrecision(const AValue: Integer); virtual;
  public
    constructor Create(const AOwner: TtiObject); overload; override;
    constructor Create(const AOwner: TtiObject;
                       const ANullValidation: TtiNullValidation); overload; override;
    constructor Create(const AOwner: TtiObject;
                       const ANullValidation: TtiNullValidation;
                       const APrecision: Integer); reintroduce; overload;
    function    Equals(ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(AAssignFrom: TtiFieldAbs); override;
    property    AsFloat: Extended read FValue Write SetAsFloat;
    property    Precision: Integer Read FPrecision Write SetPrecision;
    property    Epsilon: Extended Read FEpsilon Write FEpsilon;
  end;

  {: Concrete persistent boolean field}
  TtiFieldBoolean = class(TtiFieldAbs)
  private
    FValue: Boolean;
    procedure   SetAsBoolean(const AValue: Boolean);
  protected
    procedure   Clear; override;
    procedure   SetAsString(const AValue: string);  override;
    function    GetAsString: string;               override;
  public
    function    Equals(ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(AAssignFrom: TtiFieldAbs); override;
    property    AsBoolean: Boolean read FValue Write SetAsBoolean;
  end;

  {: Concrete persistent TDateTime field}
  TtiFieldDateTime = class(TtiFieldAbs)
  private
    FValue: TDateTime;
    function    GetDays: Word;
    function    GetHours: Word;
    function    GetMinutes: Word;
    function    GetMonths: Word;
    function    GetSeconds: Word;
    function    GetYears: Word;
    procedure   SetAsDateTime(const AValue: TDateTime);
  protected
    procedure   Clear; override;
    procedure   SetAsString(const AValue: string);  override;
    function    GetAsString: string;               override;
  public
    function    Equals(ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(AAssignFrom: TtiFieldAbs); override;
    property    AsDateTime: TDateTime read FValue Write SetAsDateTime;
    property    Days: Word read GetDays;
    property    Months: Word read GetMonths;
    property    Years: Word read GetYears;
    property    Hours: Word read GetHours;
    property    Minutes: Word read GetMinutes;
    property    Seconds: Word read GetSeconds;
  end;

  TtiFieldList = class(TObjectlist)
  protected
    function  GetItem(AIndex: Integer): TtiFieldAbs;
    procedure SetItem(AIndex: Integer; AObject: TtiFieldAbs);
  public
    constructor Create;
    function Add(AObject: TtiFieldAbs): Integer;
    property Items[Index: Integer]: TtiFieldAbs read GetItem write SetItem; default;
  end;

  TtiObject = class(TtiVisited)
  private
    FOID : TtiOID;
    FObjectState : TPerObjectState;
    FObserverList: TList;
    FUpdateCount: Integer;
    function GetObserverList: TList;
  protected
    FOwner: TtiObject; // Want FOwner here as there are time when we may want
                        // go get access without going through the GetOwner and
                        // SetOwner as these may have been typecase in a way
                        // which causes incompatabilities.
    function    GetDeleted: boolean; virtual;
    procedure   SetDeleted(const AValue: boolean); virtual;
    procedure   SetDirty(const AValue: boolean); virtual;
    procedure   SetObjectState(const AValue: TPerObjectState); virtual;
    function    OIDGenerator: TtiOIDGenerator; virtual;
    {$IFDEF OID_AS_INT64}
    procedure   SetOID(const AValue: TtiOID); virtual;
    {$ENDIF}
    function    GetOID: TtiOID; virtual;
    function    GetDirty : boolean; virtual;
    function    GetOwner: TtiObject; reintroduce; virtual;
    procedure   SetOwner(const AValue: TtiObject); virtual;
    function    GetParent: TtiObject; virtual;
    procedure   AssignPublicProps(ASource: TtiObject);virtual;
    procedure   AssignPublishedProp(ASource: TtiObject; APropName: string);
    function    CountPropsByType(ASource: TtiObject; APropFilter: TTypeKinds): integer;
    procedure   AssignPublishedProps(ASource: TtiObject; APropFilter: TTypeKinds = []);
    // You must override this in the concrete if there are class properties
    procedure   AssignClassProps(ASource: TtiObject); virtual;
    function    GetIndex : integer;
    procedure   DoFindAllNotUnique(AObject: TtiObject; var AFound: boolean; AData: TtiObject); virtual;
    function    GetPropValue(const APropName: string): Variant; virtual;
    procedure   SetPropValue(const APropName: string; const APropValue: Variant); virtual;

    {: Read in the primary Key values only from the database for this object.
       You must overide ReadPK and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inheried (if you are using automap)).}
    procedure   ReadPK(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    {: Read in the primary Key values only from the database for this object.
       You must overide ReadPK and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inheried (if you are using automap)).}
    procedure   ReadPK; overload; virtual;
    {: Read this object, but no owned objects from the database
       You must overide ReadPK and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inheried (if you are using automap)).}
    procedure   ReadThis(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    {: Read this object, but no owned objects from the database
       You must overide ReadThis and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inheried (if you are using automap)).}
    procedure   ReadThis; overload;  virtual;
    {: Read this object, along with any owned objects from the database
       You must overide ReadThis and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inheried (if you are using automap)).}
    procedure   Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    {: Read this object, along with any owned objects from the database
       You must overide Read and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inheried (if you are using automap)).}
    procedure   Read; overload;  virtual;
    {: Updates the database with the current property values for this object.
       You must overide Save and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inheried (if you are using automap)).}
    procedure   Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    {: Updates the database with the current property values for this object.
       You must overide Save and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inheried (if you are using automap)).}
    procedure   Save; overload; virtual;

  public
    {: Creates a new instance of the class}
    constructor Create ; override;
    {: Creates a new instance of the class and initialises it's OID with next available value}
    constructor CreateNew(const AOwner : TtiObject; const ADatabaseName : string = ''; const APersistenceLayerName : string = ''); overload; virtual;
    constructor CreateNew(const ADatabaseName : string = ''; const APersistenceLayerName : string = ''); overload; virtual;
    destructor  Destroy; override;
    {: Dos this object equal another? }
    function    Equals(const AData : TtiObject): boolean; virtual;
    {: The OID of this object }
   {$IFDEF OID_AS_INT64}
      property    OID        : TtiOID                   read GetOID write SetOID;
   {$ELSE}
      property    OID        : TtiOID                   read GetOID       ;
   {$ENDIF}
    {: The current state of this object}
    property    ObjectState: TPerObjectState read FObjectState write SetObjectState;
    {: The type of class that owns this object. If I am owned by a TtiObjectList,
       then my owner is the object responsible for my destruction.}
    property    Owner: TtiObject read GetOwner write SetOwner;
    {: The parent of this object. By default, my parent is the same as my object,
       except if I'm owned by a list when my parent will be the owner of the list.
       Parent is used to help determine foreign key relationships.}
    property    Parent: TtiObject read GetParent;
    {: Returns the value of the property specified in APropName.}
    property    PropValue[const APropName: string]: Variant read GetPropValue write SetPropValue;
    {: Is the specified property read and write?}
    function    IsReadWriteProp(const APropName : string): boolean; virtual;
    {: Return the TtiTypeKind (simple property type) of the property}
    function    PropType(const APropName: string): TtiTypeKind;
    {: Is this object deleted? }
    property    Deleted : boolean                    read GetDeleted    write SetDeleted;
    {: Is this object out of sync with the persistence layer?}
    property    Dirty  : boolean                    read GetDirty      write SetDirty;

    property    Index : integer                      read GetIndex;
    {: Returns this objects state as a string value.}
    function    ObjectStateAsString : string;
    {: Find an object in the hierarchy by OID with the OID passed as a string}
    function    Find(AOIDToFindAsString : string): TtiObject;  overload; virtual;
    {: Find an object in the hierarchy by OID with the OID passed as a TtiOID object}
    function    Find(AOIDToFind : TtiOID): TtiObject;  overload; virtual;
    {: Find an object in the hierarchy using the find method passed}
    function    Find(AtiObjectFindMethod : TPerObjFindMethod): TtiObject; overload;
    {: Find an object in the hierarchy using the extended find method passed}
    function    Find(ATIObjectFindMethodExt : TPerObjFindMethodExt; AUserContext: Pointer): TtiObject; overload;
    {: Find all objects in the hierarchy using the find method passed}
    function    FindAll(AtiObjectFindMethod : TPerObjFindMethod; AList : TList): integer; overload;
    {: Find all objects in the hierarchy using the find method passed}
    function    FindAll(ATIObjectFindMethodExt : TPerObjFindMethodExt; AList : TList; AUserContext: Pointer): integer; overload;
    {: Find all objects in the hierarchy using the find method passed}
    function    FindAll(ATIObjectFindMethodData : TPerObjFindMethodData; AList : TList; AData : TtiObject): integer; overload;
    {:Is this obect unique in the hierarchy?}
    function    IsUnique(const AObject : TtiObject): boolean; virtual;
    {: Creates a cloned instance of this object. }
    function    Clone : TtiObject; virtual; // Must override and typecast if to be used
    {: Copy this object to another.}
    procedure   Assign(const ASource : TtiObject); reintroduce; virtual;
    {: returns the object at the top of the hierarchy}
    function    TopOfHierarchy : TtiObject; virtual;
    {: Set every object in the hierarchy's ObjectState to AObjectState }
    procedure   SetAllObjectStates(const AObjectState: TPerObjectState); virtual;
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid(const AErrors: TtiObjectErrors): boolean; overload; virtual;
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid(var AErrorMessage: string): boolean; overload; // Don't override this one
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid(const AStrings: TStrings): boolean; overload; // Don't override this one
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid(const AStrings: TStrings; AAppend: boolean): boolean; overload; // Don't override this one
    {: Is the Object a valid one. Does it adhere to all the business rules you defined? }
    function    IsValid: boolean; overload; // Don't override this one

    procedure   AssignFieldList(var AFieldList: TtiFieldList);
    {: ForceAsCreate will get a new OID, and set ObjectState := posCreate}
    procedure   ForceAsCreate(const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
    {: Display the object tree as a string for debugging (will show all published properties.)}
    function    AsDebugString(
                 const AValuesToShow: TtiObjectAsDebugStringValuesToShow
                         = CTIAsDebugStringDataAll): string; virtual;
    {:Assign the published property names to a TStringList}
//    procedure   GetPropNames(AList: TStringList; APropFilter: TtiPropTypes = []);
    {: Return the propery count filter by APropFilter }
    function    PropCount(APropFilter: TTypeKinds = ctkSimple): integer;

    { Observer pattern implementation below }
    {: Attach a new observer }
    procedure   AttachObserver(AObserver: TtiObject); virtual;
    {: Detach a existing observer }
    procedure   DetachObserver(AObserver: TtiObject); virtual;
    {: Start a update process. This will allow us to update multiple things,
      before we notify the observers of the updates. }
    procedure   BeginUpdate;
    {: End a update process }
    procedure   EndUpdate;
    {: Only needed if performing a observing role }
    procedure   Update(ASubject: TtiObject); virtual;
    {: Notify all the attached observers about a change }
    procedure   NotifyObservers; virtual;
    {: Used to get access to the internal observer list. This has been surfaced
       so that the MGM List Views can atttach/detach observers to the selected
       object. Not a great way of doing it - we need a different design. }
    property    ObserverList: TList read GetObserverList write FObserverList;
  end;


  TtiObjectListCompareEvent = procedure(AItem1, AItem2: TtiObject) of object;

  TtiEnumerator = class(TtiBaseObject)
  private
    FIndex: Integer;
    FList: TtiObjectList;
  public
    constructor Create(AList: TtiObjectList);
    function GetCurrent: TtiObject;
    function MoveNext: Boolean;
    property Current: TtiObject read GetCurrent;
  end;

  TtiObjectList = class(TtiObject)
  private
    FList : TObjectList;
    FItemOwner: TtiObject;
    FbAutoSetItemOwner: boolean;
    function    GetList: TList;
    function    GetCountNotDeleted: integer;
    function    GetOwnsObjects: boolean;
    procedure   SetOwnsObjects(const AValue: boolean);
    function    DoCompareByProps(AItem1  : Pointer; AItem2  : Pointer;
                const ASortProps : array of string; AAscendingOrder : Boolean = True): integer;
    procedure   QuickSortByProps(SortList: PPointerList; L, R: Integer;
                const ASortProps : array of string; AAscendingOrder : Boolean = True);
  protected
    function    GetCount: integer; virtual;
    function    GetItems(i: integer): TtiObject; virtual;
    procedure   SetItems(i: integer; const AValue: TtiObject); virtual;
    procedure   SetItemOwner(const AValue: TtiObject); virtual;
    procedure   AssignPublicProps(ASource : TtiObject); override;
    procedure   AssignClassProps(ASource: TtiObject); override;
    function    IndexOfBinary(AOIDToFind : TtiOID): integer; virtual;
    function    IndexOfFullScan(AOIDToFind : TtiOID): integer; virtual;
  public
    constructor Create; override;
    destructor  Destroy; override;

    {: Assign all data from ASource into Self}
    procedure   Assign(const ASource : TtiObject); override;
    {: Assign the captions from all objects to the TStrings AList}
    procedure   AssignCaptions(const AStrings: TStrings);
    {: The number of items in the list.}
    property    Count : integer read GetCount;
    {: The number of items in the list that are not marked as deleted.}
    property    CountNotDeleted : integer read GetCountNotDeleted;
    property    Items[i:integer]: TtiObject read GetItems write SetItems; default;

    {: Does the list own the objects it contains? i.e. Will the objects be freed when the list is cleared/destroyed?}
    property    OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
    property    ItemOwner     : TtiObject read FItemOwner    write SetItemOwner;
    property    AutoSetItemOwner : boolean read FbAutoSetItemOwner write FbAutoSetItemOwner;

    {: Finds the object in the list whose OID value matches.}
    function    Find(AOIDToFindAsString : string): TtiObject;  override;
    {: Finds the object in the list whose OID value matches.}
    function    Find(AOIDToFind : TtiOID): TtiObject; override;
    {: Finds the object in the list whose OID value matches. Faster search if sorted by OID. }
    function    Find(AOIDToFind : TtiOID; ASortType: TtiPerObjListSortType): TtiObject; overload;
    {: Finds the object in the list whose OID value matches. Will search the list, and if not found, will search all owned objects }
    function    FindInHierarchy(AOIDToFind : TtiOID): TtiObject; overload;
    {: Performs the method AMethod on every object in the list.}
    procedure   ForEach(AMethod : TPerObjForEachMethod       ; AIncludeDeleted : boolean = false); overload; virtual;
    {: Performs the method AMethod on every object in the list.}
    procedure   ForEach(AMethod : TPerObjForEachMethodRegular; AIncludeDeleted : boolean = false); overload; virtual;
    {: Add an object to the list.
       Don't override Add(AObject : TtiObject ; ADefDispOrdr : boolean = true),
       It's an old method for backward compatibility . Override Add(const AObject:TtiObject).}
    function    Add(const AObject: TtiObject; ADefDispOrdr: Boolean): integer; overload; virtual;
    {: Add an object to the list.}
    function    Add(const AObject : TtiObject): integer; overload; virtual;
    {: Empty list and delete all owned objects}
    procedure   Clear; virtual;
    {: Empty list, but do not delete owned objects }
    procedure   Empty; virtual;
    {: The index of the specified object in the list.}
    function    IndexOf(const AObject: TtiObject): integer; overload; virtual;
    {: The index of the object in the list whose OID value matches. Faster search if sorted by OID. }
    function    IndexOf(AOIDToFind : TtiOID; ASortType: TtiPerObjListSortType = stNone): integer; overload; virtual;
    {: The last object in the list.}
    function    Last : TtiObject; virtual;
    {: The first object in the list.}
    function    First : TtiObject; virtual;
    {: Returns the first item in the list that hasn't been marked deleted.}
    function    FirstExcludeDeleted : TtiObject; virtual;
    {: Returns the last item in the list that hasn't been marked deleted.}
    function    LastExcludeDeleted : TtiObject; virtual;
    {: Removes the object at a specified position and (if OwnsObject is True) frees the object.}
    procedure   Delete(i : integer); virtual;
    {: Removes the specified item from the list and (if OwnsObject is True) frees the object.}
    function    Remove(const AObject: TtiObject): integer; virtual;
    {: Removes the specified object from the list without freeing the object.}
    procedure   Extract(const AObject: TtiObject); virtual;
    {: Adds an object to the list at the position specified by Index.}
    procedure   Insert(const AIndex: integer; const AObject: TtiObject); overload; virtual;
    procedure   Insert(const AInsertBefore: TtiObject; const AObject: TtiObject); overload; virtual;
    {: Sets all items in the list as ready for deletion}
    procedure   MarkListItemsForDeletion; virtual;
    {: Scan the list and remove, then free all objects with ObjectState = posDeleted}
    procedure   FreeDeleted;
    {: Sets all items in the list as needing updating to the database}
    procedure   MarkListItemsDirty; virtual;
    procedure   PropToStrings(const AStrings: TStrings; const APropName : string = 'caption'); virtual;
    {: Finds the first object whose properties passed in AProps match the values specified.}
    function    FindByProps(const AProps : array of string;
                             const AVals : array of variant;
                             ACaseSensitive : boolean = true ): TtiObject; virtual;
    {: Sorts the list by the properties specified}
    procedure   SortByProps(const ASortProps : array of string; AAscendingOrder : Boolean = True); virtual;
    {: Sorts the list by each member's OID value.}
    procedure   SortByOID; virtual;

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
    {: Allows the use of the for-in loop in Delphi 2005+}
    function GetEnumerator: TtiEnumerator;
  published
    // This must be published so it can be used by the tiPerAware controls.
    property    List : TList read GetList;
  end;

  TtiClass  = class of TtiObject;
  TtiObjectClass = class of TtiObject;
  TPerObjListClass = class of TtiObjectList;
  TtiObjectListClass = class of TtiObjectList;

  TtiObjectErrors = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiObjectError; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiObjectError); reintroduce;
    function    GetAsString: string; virtual;
  public
    property    Items[i:integer]: TtiObjectError read GetItems write SetItems;
    function    Add(const AObject: TtiObjectError): integer; reintroduce;
    procedure   AddError(const AErrorProperty: string; const AErrorMessage: string; AErrorCode: Word); overload;
    procedure   AddError(const AErrorProperty: string; const AErrorMessage: string); overload;
    procedure   AddError(const AErrorMessage: string); overload;
    function    FindByMessage(const AMessage: string): TtiObjectError;
    function    FindByErrorCode(const AErrorCode: Word): TtiObjectError;
    function    FindByErrorProperty(const AErrorProperty: string): TtiObjectError;
    property    AsString: string Read GetAsString;
  published
  end;

  TtiObjectError = class(TtiObject)
  private
    FErrorMessage: string;
    FErrorProperty: string;
    FErrorCode: Word;
  protected
    function    GetOwner: TtiObjectErrors; reintroduce;
    procedure   SetOwner(const AValue: TtiObjectErrors); reintroduce;
  public
    property    Owner: TtiObjectErrors read GetOwner write SetOwner;
  published
    property    ErrorProperty: string read FErrorProperty write FErrorProperty;
    property    ErrorMessage: string read FErrorMessage write FErrorMessage;
    property    ErrorCode: Word read FErrorCode write FErrorCode;
  end;

  TPerObjClassMapping = class(TtiBaseObject)
  private
    FPerObjAbsClassName: string;
    FPerObjAbsClass: TtiClass;
  public
    constructor Create; virtual;
    property PerObjAbsClassName : string read FPerObjAbsClassName write FPerObjAbsClassName;
    property PerObjAbsClass : TtiClass read FPerObjAbsClass write FPerObjAbsClass;
  end;

  TPerObjFactory = class(TtiBaseObject)
  private
    FList : TObjectList;
  protected
    function FindByClassName(const AClassName : string): TPerObjClassMapping; virtual;
    function GetItems(AIndex: integer): TPerObjClassMapping; virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   RegisterClass(const AClassName : string; AClass : TtiClass);
    procedure   UnRegisterClass(const AClassName : string);
    function    CreateInstance(const AClassName : string): TtiObject;
    function    CreateNewInstance(const AClassName : string; AOwner : TtiObject = nil): TtiObject;
    function    IsRegistered(const AClassName : string): boolean;
    property    Items[AIndex : integer]: TPerObjClassMapping read GetItems;
    function    Count : integer;
  end;

  TPerStream = class(TtiObject)
  private
    FStream : TMemoryStream;
    function  GetSize    : integer;
    function  GetAsString : string;
    procedure SetAsString(const AValue: string);
    procedure SetSize(const AValue: integer);
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Stream : TMemoryStream read FStream write FStream;
    property    Size  : integer       read GetSize write SetSize;

    procedure   SaveToFile(const AFileName : string);
    procedure   LoadFromFile(const AFileName : string);
    procedure   Clear;
    property    AsString : string read GetAsString write SetAsString;
  end;

  TPerStringStream = class(TtiObject)
  private
    FStream : TStringStream;
    function GetAsString: string;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Stream : TStringStream read FStream write FStream;
    procedure   Write(const AValue : string);
    procedure   WriteLn(const AValue : string);
    property    AsString : string read GetAsString;
  end;

  {:The thread-safe version of <See Class="TtiObjectList">}
  TPerObjThreadList = class(TtiObjectList)
  private
    FCriticalSection: TCriticalSection;
    //FRaiseLockException : Boolean;
  protected
    function    GetCount: integer; override;
    procedure   SetItems(i: integer; const AValue: TtiObject); override;
    procedure   SetItemOwner(const AValue: TtiObject); override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    {: Locks the list to prevent other threads gaining access.}
    procedure   Lock;
    {: Unlocks the list to allow other threads access.}
    procedure   UnLock;

    procedure   Delete(i : integer); override;
    function    Add(const AObject: TtiObject; ADefDispOrdr: boolean = true): integer; override;
    procedure   Clear; override; // Empty list and delete all owned objects
    procedure   Empty; override; // Empty list, but do not delete owned objects
    function    IndexOf(const AObject: TtiObject): integer; override;
    function    Last : TtiObject; override;
    function    First : TtiObject; override;
    function    FirstExcludeDeleted : TtiObject; override;
    function    LastExcludeDeleted : TtiObject; override;
    function    Remove(const AObject: TtiObject): integer; override;
    procedure   MarkListItemsForDeletion; override;
    procedure   MarkListItemsDirty; override;
    procedure   SortByOID; override;
    procedure   SortByProps(const ASortProps : array of string; AAscendingOrder : Boolean = True); override;
    {:Does a lock failure raise an exception or just log an error?}
    //Property RaiseLockException : Boolean Read FRaiseLockException Write FRaiseLockException;
  end;

  // TPerVisList is here for backward compatibility.
  // Do not use. Use TtiObjectList instead.
  TPerVisList = class(TtiObjectList);

  TVisPerObjFind = class(TtiVisitor)
  private
    FUserContext: Pointer;
    FFound: TtiObject;
    FPerObjFindMethod: TPerObjFindMethod;
    FPerObjFindMethodExt: TPerObjFindMethodExt;
    FFoundList: TList;
    FData: TtiObject;
    FPerObjFindMethodData: TPerObjFindMethodData;
  protected
    function    AcceptVisitor : boolean; override;
  public
    constructor Create; override;
    procedure   Execute(const AVisited : TtiVisited); override;
    property    Found : TtiObject read FFound;
    property    PerObjFindMethod : TPerObjFindMethod read FPerObjFindMethod write FPerObjFindMethod;
    property    PerObjFindMethodExt: TPerObjFindMethodExt read FPerObjFindMethodExt write FPerObjFindMethodExt;
    property    PerObjFindMethodData: TPerObjFindMethodData read FPerObjFindMethodData write FPerObjFindMethodData;
    property    FoundList : TList read FFoundList write FFoundList;
    property    UserContext : Pointer read FUserContext write FUserContext;
    property    Data : TtiObject read FData write FData;
  end;

  TVisPerObjFindByOID = class(TtiVisitor)
  private
    FOIDToFind: TtiOID;
    FFound: TtiObject;
  protected
    function    AcceptVisitor : boolean; override;
  public
    constructor Create; override;
    procedure   Execute(const AVisited : TtiVisited); override;
    property    Found : TtiObject read FFound;
    property    OIDToFind : TtiOID read FOIDToFind write FOIDToFind;
  end;

  TVisSetAllObjectStates = class(TtiVisitor)
  private
    FObjectState: TPerObjectState;
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AVisited : TtiVisited); override;
    property    ObjectState : TPerObjectState read FObjectState write FObjectState;
  end;

  TVisPerObjIsDirty = class(TtiVisitor)
  private
    FbDirty: boolean;
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AVisited : TtiVisited); override;
    property    Dirty : boolean read FbDirty write FbDirty;
  end;

const
  cgNullDBInteger            = -1 ;
  cgNullDBString             = '' ;
  cgNullDBDate               = 0.0;

function ObjectStateToString(AObjectState : TPerObjectState): string;

{:Copy a TtiObjectList of TtiObject(s) data to a TStream using CSV format}
procedure tiListToStream(AStream: TStream;
                         AList: TtiObjectList;
                         AFieldDelim: string;
                         ARowDelim: string;
                         AColsSelected: TStringList); overload;

procedure tiListToStream(AStream : TStream;
                         AList : TtiObjectList); overload;

// Copy a TList of TtiBaseObject's to a CSV file (Prompt the user for the file name)
procedure tiListToCSV(AList: TtiObjectList;
                       const AFileName: string;
                       AColsSelected: TStringList); overload;

procedure tiListToCSV(AList: TtiObjectList;
                       const AFileName: string); overload;

implementation
uses
  // tiOPF
   tiConstants
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

function ObjectStateToString(AObjectState : TPerObjectState): string;
begin
  result := GetEnumName(TypeInfo(TPerObjectState),
                         Ord(AObjectState));
end;

procedure tiListToStream(AStream : TStream; AList : TtiObjectList);
var
  lFields : TStringList;
begin
  Assert(AStream<>nil, 'AStream not assigned');
  Assert(AList.TestValid, CTIErrorInvalidObject);
  Assert(AList.Count > 0, 'AList.Count = 0');
  lFields := TStringList.Create;
  try
    tiGetPropertyNames(AList.Items[0], lFields);
    tiListToStream(AStream, AList, ',', CrLf, lFields);
  finally
    lFields.Free;
  end;
end;


procedure tiListToCSV(AList: TtiObjectList;
                       const AFileName: string;
                       AColsSelected: TStringList);
var
  lStream   : TFileStream;
begin
  Assert(AList.TestValid, CTIErrorInvalidObject);
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(AColsSelected<>nil, 'AColsSelected not assigned');

  lStream := TFileStream.Create(AFileName, fmCreate);
  try
    tiListToStream(lStream, AList, ',', CrLf, AColsSelected);
  finally
    lStream.Free;
  end;
end;


procedure tiListToCSV(AList: TtiObjectList; const AFileName: string);
var
  lStream   : TFileStream;
  lFields: TStringList;
begin
  Assert(AList.TestValid, CTIErrorInvalidObject);
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(AList.Count > 0, 'AList.Count = 0');
  lFields:= TStringList.Create;
  try
    lStream := TFileStream.Create(AFileName, fmCreate);
    try
      tiGetPropertyNames(AList.Items[0], lFields);
      tiListToStream(lStream, AList, ',', CrLf, lFields);
    finally
      lStream.Free;
    end;
  finally
    lFields.Free;
  end;
end;

procedure tiListToStream(AStream : TStream;
                         AList : TtiObjectList;
                         AFieldDelim : string;
                         ARowDelim: string;
                         AColsSelected : TStringList);
var
  i, j      : integer;
  lsValue   : string;
  lFieldName : string;
  AData     : TtiBaseObject;
  lLine     : string;
  lPropType: TTypeKind;
begin
  // Write column headings
  for i := 0 to AColsSelected.Count - 1 do begin
    tiAppendStringToStream(AColsSelected.Strings[i], AStream);
    if i < AColsSelected.Count - 1 then
      tiAppendStringToStream(AFieldDelim, AStream)
    else
      tiAppendStringToStream(ARowDelim, AStream);
  end;

  // Write the data
  for i := 0 to AList.Count - 1 do
  begin
    AData := (TObject(AList.Items[i]) as TtiBaseObject);
    lLine := '';
    for j := 0 to AColsSelected.Count - 1 do
    begin
      if lLine <> '' then
        lLine := lLine + AFieldDelim;
      lFieldName := AColsSelected.Strings[j];
      if GetPropInfo(AData,lFieldName)^.PropType^.Name = 'TDateTime' then
        lsValue := tiDateTimeToStr(GetPropValue(AData,lFieldName))
      else
      begin
        lPropType := TypInfo.PropType(AData, lFieldName);
        case lPropType of
          tkChar       : lsValue := IntToStr(GetOrdProp(AData, lFieldName));
          tkWChar      : lsValue := IntToStr(GetOrdProp(AData, lFieldName));
          tkString     : lsValue := GetStrProp(AData, lFieldName);
          tkLString    : lsValue := GetStrProp(AData, lFieldName);
          tkWString    : lsValue := GetWideStrProp(AData, lFieldName);
          {$IFDEF FPC}
          tkAString    : lsValue := GetStrProp(AData, lFieldName);
          {$ENDIF}
          tkInteger    : lsValue := IntToStr(GetInt64Prop(AData, lFieldName));
          tkInt64      : lsValue := IntToStr(GetInt64Prop(AData, lFieldName));
          tkFloat      : lsValue := FloatToStr(GetFloatProp(AData, lFieldName));
          tkEnumeration : lsValue := IntToStr(GetOrdProp(AData, lFieldName));
          {$IFDEF FPC}
          tkBool       : lsValue := IntToStr(GetInt64Prop(AData, lFieldName));
          {$ENDIF}
        end;
      end;
      lLine := lLine + lsValue;
    end;
    if i <> 0 then
      lLine := ARowDelim + lLine;
    tiAppendStringToStream(lLine, AStream)
  end;
end;

{ TtiObject }

constructor TtiObject.Create;
begin
  inherited Create;
  FObjectState := posEmpty;
  {$IFDEF OID_AS_INT64}
  FOID := cNullOIDInteger;
  {$ENDIF}
  FOwner      := nil;
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
  lClass : TtiClass;
begin
  lClass := TtiClass(ClassType);
  result := TtiObject(lClass.Create);
  result.Assign(self);
end;

{: When you create a concrete class that contains object type properties
  you will have to override AssignClassProps() and implement the necessary
  behaviour to copy, clone or create new instances of these properties. }
procedure TtiObject.Assign(const ASource: TtiObject);
begin
  Assert((ASource is Self.ClassType) or
          (Self is ASource.ClassType),
          ASource.ClassName +
          ' and ' +
          ClassName +
          ' are not assignment compatable');

  AssignPublicProps(   ASource);
  AssignPublishedProps(ASource);
  AssignClassProps(    ASource);

  // When you create a concrete class that contains object type properties
  // you will have to override AssignClassProps() and implement
  // the necessary behaviour to copy, clone or create new instances
  // of these properties.

end;

procedure TtiObject.AssignPublishedProps(ASource : TtiObject;
                                           APropFilter : TTypeKinds = []);
var
  lsl : TStringList;
  i : integer;
  lsPropName : string;
  lPropFilter : TTypeKinds;
begin
  if APropFilter = [] then
    lPropFilter := ctkSimple + [tkEnumeration, tkVariant]
  else
    lPropFilter := APropFilter;

  lsl := TStringList.Create;
  try
    tiGetPropertyNames(self, lsl, lPropFilter);
    for i := 0 to lsl.Count - 1 do
    begin
      lsPropName := lsl.Strings[i];
      try
        // Only clone read/write properties
        if (tiIsReadWriteProp(Self, lsPropName)) and
           (IsPublishedProp(ASource, lsPropName)) then
          AssignPublishedProp(ASource, lsPropName);
      except
        on e:exception do
          raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
            [ClassName, lsPropName, e.Message]);
      end;
    end;
  finally
    lsl.Free;
  end;
end;

function TtiObject.CountPropsByType(ASource : TtiObject;
                                      APropFilter : TTypeKinds): integer;
var
  lsl : TStringList;
begin
  lsl := TStringList.Create;
  try
    tiGetPropertyNames(self, lsl, APropFilter);
    result := lsl.Count;
  finally
    lsl.Free;
  end;
end;

procedure TtiObject.AssignPublicProps(ASource : TtiObject);
begin
  Assert(ASource.TestValid(TtiObject), CTIErrorInvalidObject);
  Assert(ASource.Owner.TestValid(TtiObject, true), CTIErrorInvalidObject);
  {$IFDEF OID_AS_INT64}
    OID := ASource.OID;
  {$ELSE}
    OID.Assign(ASource.OID);
  {$ENDIF}
  FObjectState := ASource.ObjectState;
  // 1. If we are cloning a list element to edit, then we will probably
  //    want it's Owner property set to the list. This will be done here.
  // 2. If we are editing a list, then we want each list element to be cloned
  //    too and we want their owner properties to be set to the cloned list,
  //    this will be done in TtiObjectList.AssignClassProps.
  // 3. If we are editing a compound object, then Owner will be set in
  //    the classes constructor.
  if ASource.Owner is TtiObjectList then
    Owner := ASource.Owner;
end;


procedure TtiObject.AssignPublishedProp(ASource: TtiObject; APropName: string);
var
  lPropType: TTypeKind;
begin
  lPropType := TypInfo.PropType(ASource, APropName);
  if lPropType in ctkSimple + [tkVariant, tkEnumeration] then
  begin
    case lPropType of
      tkChar       : SetOrdProp(Self, APropName, GetOrdProp(ASource, APropName));
      tkWChar      : SetOrdProp(Self, APropName, GetOrdProp(ASource, APropName));
      tkString     : SetStrProp(Self, APropName, GetStrProp(ASource, APropName));
      tkLString    : SetStrProp(Self, APropName, GetStrProp(ASource, APropName));
      tkWString    : SetWideStrProp(Self, APropName, GetWideStrProp(ASource, APropName));
      {$IFDEF FPC}
      tkAString    : SetStrProp(Self, APropName, GetStrProp(ASource, APropName));
      {$ENDIF}
      tkInteger    : SetOrdProp(Self, APropName, GetOrdProp(ASource, APropName));
      tkInt64      : SetInt64Prop(Self, APropName, GetInt64Prop(ASource, APropName));
      tkFloat      : SetFloatProp(Self, APropName, GetFloatProp(ASource, APropName));
      tkVariant    : SetVariantProp(Self, APropName, GetVariantProp(ASource, APropName));
      tkEnumeration : SetOrdProp(Self, APropName, GetOrdProp(ASource, APropName));
      {$IFDEF FPC}
      tkBool       : SetInt64Prop(Self, APropName, GetInt64Prop(ASource, APropName));
      {$ENDIF}
    end;
  end
  else
    raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
      [ClassName, APropName, 'Unknown property type']);
end;


procedure TtiObject.AssignClassProps(ASource : TtiObject);
begin
  Assert(CountPropsByType(ASource, [tkClass]) = 0,
          'Trying to call ' + ClassName + '.Assign() on a class that contains ' +
          'object type properties. AssignClassProps() must be overridden in the concrete class.');
end;

function TtiObject.GetDeleted: boolean;
begin
  result := (ObjectState = posDelete) or
            (ObjectState = posDeleted);
end;

function TtiObject.GetDirty: boolean;
var
  lVis : TVisPerObjIsDirty;
begin
  lVis := TVisPerObjIsDirty.Create;
  try
    self.Iterate(lVis);
    result := lVis.Dirty;
  finally
    lVis.Free;
  end;
end;

type

  TtiObjectVisitorSetObjectStateToDelete = class(TtiVisitor)
  protected
    function AcceptVisitor(const AVisited: TtiVisited): boolean; override;
    function VisitBranch(const ADerivedParent, AVisited: TtiVisited) : boolean; override;
  public
    procedure Execute(const AVisited : TtiVisited); override;
  end;

  function TtiObjectVisitorSetObjectStateToDelete.AcceptVisitor(const AVisited: TtiVisited): boolean;
  var
    LVisited: TtiObject;
  begin
    Assert(AVisited.TestValid(TtiObject), CTIErrorInvalidObject);
    LVisited:= AVisited as TtiObject;
    result := LVisited.ObjectState <> posDeleted;
  end;

  procedure TtiObjectVisitorSetObjectStateToDelete.Execute(const AVisited: TtiVisited);
  begin
    Inherited Execute(AVisited);
    TtiObject(AVisited).ObjectState := posDelete;
  end;

  function TtiObjectVisitorSetObjectStateToDelete.VisitBranch(
    const ADerivedParent, AVisited: TtiVisited): boolean;
  var
    LVisited: TtiObject;
  begin
    Assert(ADerivedParent.TestValid(TtiObject, True), CTIErrorInvalidObject);
    Assert(AVisited.TestValid(TtiObject), CTIErrorInvalidObject);
    LVisited:= AVisited as TtiObject;
    result:= (ADerivedParent = nil) or
      (ADerivedParent = LVisited.Owner)
  end;

procedure TtiObject.SetDeleted(const AValue: boolean);
var
  lVis : TtiObjectVisitorSetObjectStateToDelete;
begin
  if AValue and not Deleted then
  begin
    lVis := TtiObjectVisitorSetObjectStateToDelete.Create;
    try
      self.Iterate(lVis);
    finally
      lVis.Free;
    end;
  end;
end;

procedure TtiObject.SetDirty(const AValue: boolean);
begin
  if AValue then
  begin   // Dirty set to True
    case ObjectState of
      posEmpty  : begin
                     ObjectState := posCreate   ;
                     {$IFDEF OID_AS_INT64}
                       if OID = cNullOIDInteger then
                         OID := GTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID;
                     {$ELSE}
                       if OID.IsNull then
                           Assert(false, 'Under construction');
                         // OID.GetNextValue;
                     {$ENDIF}
                   end;
      posPK     : ObjectState := posUpdate   ;
      posCreate :; // Do nothing
      posUpdate :; // Do nothing
      posDelete :; // Do nothing
      posDeleted :; // Do nothing
      posClean  : ObjectState := posUpdate;
    else
      raise EtiOPFInternalException.Create(cErrorInvalidObjectState);
    end;
  end
  else
  begin   // Dirty set to False
    ObjectState := posClean;
  end;
end;


{ TVisPerObjIsDirty }

function TVisPerObjIsDirty.AcceptVisitor : boolean;
begin
  result := ((Visited is TtiObject) or
            (Visited is TtiObjectList)) and
            (not Dirty);
end;

procedure TVisPerObjIsDirty.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);

  if not AcceptVisitor then
    exit; //==>

  if Visited is TtiObject then
    Dirty := TtiObject(AVisited).ObjectState in
              [ posCreate,  // The object is new and must be created in the DB
                posUpdate,  // The object has been changed, the DB must be updated
                posDelete   // The object has been deleted, it must be deleted from the DB
              ]
  else
    Assert(false, 'Invalid visited type');

  // Use this to debug problems when you think you have set an object's state
  // to posClean or posDeleted, but it's parent object is still showing dirty.
{
  if Dirty then
    Log([ 'Dirty',
          Dirty,
          Visited.ClassName,
          Visited.Caption,
          GetEnumName(TypeInfo(TPerObjectState),
                       ord(TtiObject(AVisited).ObjectState))]);
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
  FList.Free;
  inherited;
end;

function TtiObjectList.Add(const AObject: TtiObject; ADefDispOrdr: boolean): integer;
begin
  result := Add(AObject);
end;

function TtiObjectList.Add(const AObject : TtiObject): integer;
begin
  if FbAutoSetItemOwner then
    AObject.Owner := FItemOwner;
  result := FList.Add(AObject);
end;

procedure TtiObjectList.Clear;
begin
  FList.Clear;
  // Should Clear set the ObjectState to posEmpty. Originally we thought yes,
  // then got burnt by side effects, so this was removed 26/09/2001
  { 2005-08-25 graemeg: I thought I would take my chances as it makes sense
    to set the ObjectState }
  ObjectState := posEmpty;
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
    TtiObject(Items[i]).Owner := nil;
  FList.Delete(i);
end;


procedure TtiObject.SetObjectState(const AValue: TPerObjectState);
begin
  if FObjectState = AValue then exit;
  FObjectState := AValue;
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

function TtiObjectList.GetItems(i: integer): TtiObject;
begin
  result := TtiObject(FList.Items[ i ]);
end;

function TtiObjectList.GetList: TList;
begin
  result := FList;
end;

function TtiObjectList.IndexOf(const AObject: TtiObject): integer;
begin
  result := FList.IndexOf(AObject);
end;

function TtiObjectList.Last: TtiObject;
begin
  // TList can't handle this. Strange!
  if FList.Count > 0 then
// Under some circumstances, this will AV. Why?  
//    result := TtiObject(FList.Last)
    result := TtiObject(FList.Items[FList.Count-1])
  else
    result := nil;
end;

function TtiObjectList.LastExcludeDeleted: TtiObject;
var
  i : integer;
begin
  for i := Count-1 downto 0 do
    if not Items[i].Deleted then
    begin
      result := Items[i];
      Exit; //==>
    end;
  result := nil;
end;

procedure TtiObjectList.SetItems(i: integer; const AValue: TtiObject);
begin
  FList.Items[ i ]:= AValue;
end;

function TtiObject.ObjectStateAsString: string;
begin
  result := GetEnumName(TypeInfo(TPerObjectState),
                         Ord(ObjectState));
end;

function TtiObject.OIDGenerator: TtiOIDGenerator;
begin
  if not Assigned(GTIOPFManager.DefaultOIDGenerator) then
    raise EtiOPFProgrammerException.Create(CErrorDefaultOIDGeneratorNotAssigned);
  result:= GTIOPFManager.DefaultOIDGenerator;
end;

{ TPerStream }
 
constructor TPerStream.Create;
begin
  inherited;
  FStream := nil;
  Clear;
end;

destructor TPerStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

procedure TPerStream.Clear;
begin
  if FStream <> nil then
    FStream.Free;
  FStream := TMemoryStream.Create;
end;

function TPerStream.GetSize: integer;
begin
  result := FStream.Size;
end;

procedure TPerStream.LoadFromFile(const AFileName: string);
begin
  FStream.LoadFromFile(AFileName);
end;

procedure TPerStream.SaveToFile(const AFileName: string);
begin
  FStream.Position := 0;
  FStream.SaveToFile(AFileName);
end;

function TPerStream.GetAsString: string;
var
  ls : string;
begin
  FStream.Seek(0,soFromBeginning);
  SetString(ls, nil, FStream.Size);
  Stream.Read(Pointer(ls)^, FStream.Size);
  Result := ls;
end;

procedure TPerStream.SetAsString(const AValue: string);
var
  lpcText : PChar;
begin
  FStream.Clear;
  lpcText := PChar(AValue);
  FStream.WriteBuffer(lpcText^, length(lpcText));
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
function TtiObjectList.Remove(const AObject: TtiObject):integer;
begin
  if AutoSetItemOwner then
    AObject.Owner := nil;
  result := FList.Remove(AObject);
end;

{: Call Extract to remove an object from the list without freeing the object
  itself. After an object is removed, all the objects that follow it are moved 
  up in index position and Count is decremented.}
procedure TtiObjectList.Extract(const AObject: TtiObject);
begin
  if AutoSetItemOwner then
    AObject.Owner := nil;
  FList.Extract(AObject);
end;

{: Call Insert to add an object at a specified position in the list, shifting
  the item that previously occupied that position (and all subsequent items) up.
  Insert increments Count and, if necessary, allocates memory by increasing the
  value of Capacity. The Index parameter is zero-based, so the first position
  in the list has an index of 0. To replace a nil reference with a new object
  without growing the array, set the Items property directly. }
procedure TtiObjectList.Insert(const AIndex: integer; const AObject: TtiObject);
begin
  FList.Insert(AIndex, AObject);
  if FbAutoSetItemOwner then
    AObject.Owner := FItemOwner;
end;

procedure TtiObjectList.Insert(const AInsertBefore: TtiObject; const AObject: TtiObject);
var
  i : integer;
begin
  i := FList.IndexOf(AInsertBefore);
  if i >= 0 then
    Insert(i, AObject)
  else
  begin
    Add(AObject);
  end;
end;


{ TVisPerObjFindByOID }
 
function TVisPerObjFindByOID.AcceptVisitor: boolean;
begin
  result := (Visited is TtiObject) and
            (FFound = nil);
end;

constructor TVisPerObjFindByOID.Create;
begin
  inherited;
  FFound := nil;
end;

procedure TVisPerObjFindByOID.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  if not AcceptVisitor then
    Exit; //==>

  if OIDEquals(TtiObject(Visited).OID ,FOIDToFind) then
    FFound := TtiObject(Visited);
end;

function TtiObject.Find(AtiObjectFindMethod: TPerObjFindMethod): TtiObject;
var
  lVis : TVisPerObjFind;
begin
  lVis := TVisPerObjFind.Create;
  try
    lVis.PerObjFindMethod := AtiObjectFindMethod;
    self.Iterate(lVis);
    result := lVis.Found;
  finally
    lVis.Free;
  end;
end;

function TtiObject.Find(ATIObjectFindMethodExt : TPerObjFindMethodExt; AUserContext: Pointer): TtiObject;
var
  lVis : TVisPerObjFind;
begin
  lVis := TVisPerObjFind.Create;
  try
    lVis.PerObjFindMethodExt := ATIObjectFindMethodExt;
    lVis.UserContext := AUserContext;
    self.Iterate(lVis);
    result := lVis.Found;
  finally
    lVis.Free;
  end;
end;

function TtiObject.FindAll(AtiObjectFindMethod: TPerObjFindMethod; AList : TList): integer;
var
  lVis : TVisPerObjFind;
begin
  if AList <> nil then
    AList.Clear;
  lVis := TVisPerObjFind.Create;
  try
    lVis.FoundList := AList;
    lVis.PerObjFindMethod := AtiObjectFindMethod;
    self.Iterate(lVis);
  finally
    lVis.Free;
  end;
  result := AList.Count;
end;

function TtiObject.Find(AOIDToFind : TtiOID): TtiObject;
var
  lVis : TVisPerObjFindByOID;
begin
  lVis := TVisPerObjFindByOID.Create;
  try
    lVis.OIDToFind := AOIDToFind;
    self.Iterate(lVis);
    result := lVis.Found;
  finally
    lVis.Free;
  end;
end;

procedure TtiObjectList.MarkListItemsForDeletion;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Deleted := true;
end;

procedure TtiObjectList.MarkListItemsDirty;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Dirty := true;
end;

{ TVisPerObjFind }

function TVisPerObjFind.AcceptVisitor: boolean;
begin
  result := (Visited is TtiObject) and
            ((FFound = nil) or (FFoundList <> nil));
end;

constructor TVisPerObjFind.Create;
begin
  inherited;

end;

procedure TVisPerObjFind.Execute(const AVisited: TtiVisited);
var
  lbFound : boolean;
begin
  inherited Execute(AVisited);
  if not AcceptVisitor then
    Exit; //==>

  lbFound := false;
  if Assigned(FPerObjFindMethod) then
    FPerObjFindMethod(TtiObject(Visited), lbFound)
  else if Assigned(FPerObjFindMethodExt) then
    PerObjFindMethodExt(TtiObject(Visited), lbFound, FUserContext)
  else if Assigned(FPerObjFindMethodData) then
    PerObjFindMethodData(TtiObject(Visited), lbFound, Data)
  else
    raise EtiOPFProgrammerException.Create(cErrorNoFindMethodAssigned);

  if lbFound then
    if FoundList = nil then
      FFound := TtiObject(Visited)
    else
      FoundList.Add(Visited);

end;

function TtiObject.GetOwner: TtiObject;
begin
  //Assert(FOwner <> Nil, 'Owner has not been assigned in ' + ClassName);
  Result := FOwner;
end;

procedure TtiObject.SetOwner(const AValue: TtiObject);
begin
  FOwner := AValue;
end;

{ By default, IsUnique will check all objects in the collection (from
  the current object down the tree) for uniqueness by OID. Override
  DoFindAllNotUnique to change the properties that are tested. } 
function TtiObject.IsUnique(const AObject: TtiObject): boolean;
var
  lList : TList;
  i : integer;
begin
  Assert(Assigned(AObject), 'AObject not assigned');
  lList := TList.Create;
  try
    FindAll(DoFindAllNotUnique, lList, AObject);
    result := true;
    for i := 0 to lList.Count - 1 do
    begin
      if (TtiObject(lList.Items[i]) <> AObject) {and
         (lList.Items[i] <> Self)} and
         (OIDEquals(TtiObject(lList.Items[i]).OID, AObject.OID)) then
      begin
        result := false;
        Exit; //==>
      end;
    end;
  finally
    lList.Free;
  end;
end;

procedure TtiObjectList.ForEach(AMethod: TPerObjForEachMethod; AIncludeDeleted: boolean=false);
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if not Items[i].Deleted or AIncludeDeleted then
      AMethod(Items[i]);
end;

procedure TtiObjectList.ForEach(AMethod: TPerObjForEachMethodRegular; AIncludeDeleted: boolean);
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if not Items[i].Deleted or AIncludeDeleted then
      AMethod(Items[i]);
end;

procedure TtiObjectList.PropToStrings(const AStrings: TStrings; const APropName : string = 'caption');
var
  i : integer;
  lPropValue : string;
begin
  AStrings.Clear;
  for i := 0 to Count - 1 do
  begin
    lPropValue := TypInfo.GetPropValue(Items[i], APropName);
    AStrings.AddObject(lPropValue, Items[i]);
  end;
end;

function TtiObjectList.GetCountNotDeleted: integer;
var
  i : integer;
begin
  result := 0;
  for i := 0 to Count - 1 do
    if not Items[i].Deleted then
      Inc(result);
end;

function TtiObjectList.GetEnumerator: TtiEnumerator;
begin
  result:= TtiEnumerator.Create(self);
end;

{ TPerStringStream }

constructor TPerStringStream.Create;
begin
  FStream := TStringStream.Create('');
  inherited;
end;

destructor TPerStringStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TPerStringStream.GetAsString: string;
begin
//  FStream.Position := 0;
  result := FStream.DataString;
end;

procedure TPerStringStream.Write(const AValue: string);
begin
  FStream.WriteString(AValue);
end;

procedure TPerStringStream.WriteLn(const AValue: string);
begin
  FStream.WriteString(AValue + CrLf);
end;

procedure TtiObjectList.SetItemOwner(const AValue: TtiObject);
var
  i : integer;
begin
  FItemOwner := AValue;
  for I := 0 to Count - 1 do
    Items[ I ].Owner := FItemOwner;
end;

procedure TtiObjectList.AssignCaptions(const AStrings: TStrings);
var
  i: Integer;
begin
  AStrings.Clear;
  for i := 0 to count - 1 do
    AStrings.Add(Items[i].Caption);
end;

procedure TtiObjectList.AssignClassProps(ASource: TtiObject);
var
  i : integer;
  lClass : TtiClass;
  lSource : TtiObject;
  lTarget : TtiObject;
begin
  Assert(ASource is TtiObjectList,
          'ASource not a TtiObjectList');

  if OwnsObjects then
  begin
    for i := 0 to TtiObjectList(ASource).Count - 1 do
    begin
      lSource := TtiObjectList(ASource).Items[i];
      lClass := TtiClass(lSource.ClassType);
      lTarget := TtiObject(lClass.Create);
      Add(lTarget);
      lTarget.Assign(lSource);
      if AutoSetItemOwner then
        lTarget.Owner := ItemOwner;

    end;
  end
  else
    for i := 0 to TtiObjectList(ASource).Count - 1 do
      Add(TtiObjectList(ASource).Items[i]);
end;

function TtiObjectList.GetOwnsObjects: boolean;
begin
  result := FList.OwnsObjects;
end;

procedure TtiObjectList.SetOwnsObjects(const AValue: boolean);
begin
  FList.OwnsObjects := AValue;
end;

function TtiObject.GetIndex: integer;
begin
  Assert(Owner <> nil,
          'Owner not assigned');
  Assert(Owner is TtiObjectList,              
          'Owner not a TtiObjectList, its a ' + Owner.ClassName);
  result := TtiObjectList(Owner).IndexOf(self);
end;

function TtiObjectList.FindByProps(const AProps: array of string;
                                  const AVals: array of variant;
                                  ACaseSensitive: boolean = true): TtiObject;
var
  j: Integer;
  i: integer;
  lFound: boolean;

  function PropertyMatch(Idx: Integer; PropName: string; PropValue: variant): boolean;
  var
    lSearch, lItem: variant;
    lVarType: TVarType;
    tiFieldAbs: TtiFieldAbs;
    lPropInfo: PPropInfo;
  begin
    lSearch := PropValue;
    
    // If the property is a TtiFieldAbs then compare the value with the field
    // objects AsString, else compare the value with the property itself
    lPropInfo := GetPropInfo(Items[Idx], PropName, [tkClass]);
    if Assigned(lPropInfo) then
    begin
      tiFieldAbs := GetObjectProp(Items[Idx], PropName, TtiFieldAbs) as TtiFieldAbs;
      if Assigned(tiFieldAbs) then
        lItem := tiFieldAbs.AsString
      else
        lItem := TypInfo.GetPropValue(Items[Idx], PropName);
    end
    else
      lItem := TypInfo.GetPropValue(Items[Idx], PropName);

    // Just to be sure that I'm comparing the SAME kind of values,
    // plus Boolean types need some extra help under FPC
    if VarIsType(PropValue, varBoolean) then
      lItem := Boolean(GetOrdProp(Items[Idx], PropName))
    else
    begin
      lVarType  := VarType(lItem);
      lSearch   := VarAsType(lSearch, lVarType);
      lItem     := VarAsType(lItem, lVarType);
    end;

    // PWH Changed for D5 compat
    if (tiIsVariantOfType(lSearch, varOleStr) or
        tiIsVariantOfType(lSearch, varString)) and not ACaseSensitive then
    begin
      result := SameText(lSearch, lItem);
    end
    else
    begin
      result := (lSearch = lItem);
    end;
  end;

begin
  Assert(High(AProps) = High(AVals),
          'Props and Vals must have the same number of elements');

  result := nil;
  lFound := False;
  for i := 0 to Count - 1 do
  begin
    // Iterate over each property and value to see if it matches
    for j := Low(AProps) to High(AProps) do    // Iterate
    begin
      lFound := PropertyMatch(i, AProps[j], AVals[j]);

      if not lFound then
        break;
    end;    // for j

    if lFound then
    begin
      result := Items[i];
      Break; //==>
    end;
  end; // for i
end;

procedure TPerStream.SetSize(const AValue: integer);
begin
  FStream.Size := AValue;
end;

function TtiObject.TopOfHierarchy: TtiObject;
  function _TopOfHierarchy(AValue : TtiObject): TtiObject;
  begin
    if AValue.Owner = nil then
      result := AValue
    else
      result := _TopOfHierarchy(AValue.Owner);
  end;
begin
  result := _TopOfHierarchy(Self);
end;


function TtiObject.GetOID: TtiOID;
begin
  // Create OID on demand
  {$IFNDEF OID_AS_INT64}
    if FOID = nil then
      FOID := OIDGenerator.OIDClass.Create;
  {$ENDIF}
  result := FOID;
end;


constructor TtiObject.CreateNew(const AOwner : TtiObject; const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
begin
  Create;
  if AOwner <> nil then
    Owner := AOwner;
  ObjectState := posCreate;
  {$IFDEF OID_AS_INT64}
    OID := GTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID;
  {$ELSE}
    OIDGenerator.AssignNextOID(OID);
  {$ENDIF}
end;

constructor TtiObject.CreateNew(const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
begin
  Create;
  ObjectState := posCreate;
  {$IFDEF OID_AS_INT64}
    OID := GTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID;
  {$ELSE}
    OIDGenerator.AssignNextOID(OID);
  {$ENDIF}
end;

function TtiObjectList.First: TtiObject;
begin
  // TList can't handle this. Strange!
  if FList.Count > 0 then
    result := TtiObject(FList.First)
  else
    result := nil;
end;

function _DoSortByOID(Item1, Item2 : pointer): integer;
begin
  {$IFDEF OID_AS_INT64}
    if TtiObject(Item1).OID < TtiObject(Item2).OID then
      result := -1
    else if TtiObject(Item1).OID > TtiObject(Item2).OID then
      result := 1
    else
      result := 0;
  {$ELSE}
    result := TtiObject(Item1).OID.Compare(TtiObject(Item2).OID);
  {$ENDIF}
end;

procedure TtiObjectList.SortByOID;
begin
  List.Sort(_DoSortByOID);
end;

//procedure TtiObjectList.SortByDispOrder;
//begin
//  SortByProps(['DispOrder']);
//end;

function TtiObjectList.DoCompareByProps(
  AItem1  : Pointer;
  AItem2  : Pointer;
  const ASortProps : array of string;
  AAscendingOrder : Boolean = True): integer;
var
  i : integer;
  lsPropName : string;
  lValue1 : variant;
  lValue2 : variant;
begin
  result := 0;
  try
    for i := Low(ASortProps) to High(ASortProps) do
    begin
      lsPropName := ASortProps[i];
      lValue1 := TtiObject(AItem1).GetPropValue(lsPropName);
      lValue2 := TtiObject(AItem2).GetPropValue(lsPropName);

      if lValue1 < lValue2 then
      Begin
        If (AAscendingOrder) Then
          result := -1
        Else
          Result := 1;
      End
      else if lValue1 > lValue2 then
      Begin
        If (AAscendingOrder) Then
          result := 1
        Else
          Result := -1;
      End;
      if result <> 0 then
        Break; //==>
    end;

  except
    on e:exception do
      raise exception.Create(
        'Error in TtiObjectList._DoCompare PropName <' +
        lsPropName + '>' + e.Message);
  end;

end;

// This method was cloned from Classes.TList.Sort and
// was added here to introduce an array of strings to
// hold property names for the sort.
procedure TtiObjectList.QuickSortByProps(
  SortList: PPointerList;
  L, R: Integer;
  const ASortProps: array of string;
  AAscendingOrder : Boolean = True);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while DoCompareByProps(SortList^[I], P, ASortProps, AAscendingOrder) < 0 do
        Inc(I);
      while DoCompareByProps(SortList^[J], P, ASortProps, AAscendingOrder) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I]:= SortList^[J];
        SortList^[J]:= T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortByProps(SortList, L, J, ASortProps, AAscendingOrder);
    L := I;
  until I >= R;
end;

procedure TtiObjectList.SortByProps(const ASortProps: array of string; AAscendingOrder : Boolean = True);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSortByProps(FList.List, 0, Count - 1, ASortProps, AAscendingOrder);
end;

procedure TtiObject.Read(const ADBConnectionName: string; APersistenceLayerName : string = '');
begin
  GTIOPFManager.Read(Self, ADBConnectionName, APersistenceLayerName);
end;

procedure TtiObject.Read;
begin
  Read('', '');
end;

procedure TtiObject.ReadPK(const ADBConnectionName: string; APersistenceLayerName : string = '');
begin
  GTIOPFManager.ReadPK(Self, ADBConnectionName, APersistenceLayerName);
end;

procedure TtiObject.ReadPK;
begin
  ReadPK('', '');
end;

procedure TtiObject.Save(const ADBConnectionName: string; APersistenceLayerName : string = '');
begin
  GTIOPFManager.Save(Self, ADBConnectionName, APersistenceLayerName);
end;

procedure TtiObject.Save;
begin
  Save('', '');
end;

procedure TtiObject.AssignFieldList(var AFieldList: TtiFieldList);
var
  i, lCount: Integer;
  lPropInfo: PPropInfo;
  lTempList: PPropList;
  lObject: TObject;
begin
  Assert(Assigned(AFieldList), 'Stringlist passed as parameter to GetPropListAsStrings is nil');
  AFieldList.Clear;
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
          AFieldList.Add(TtiFieldAbs(lObject));
        end;
      end;
    end;
  finally
    FreeMem(lTempList);
  end;
end;

{ TPerObjThreadList }

function TPerObjThreadList.Add(const AObject: TtiObject; ADefDispOrdr: boolean): integer;
begin
  Lock;
  try
    result := inherited Add(AObject, ADefDispOrdr);
  finally
    Unlock;
  end;
end;

procedure TPerObjThreadList.Clear;
begin
  Lock;
  try
    inherited Clear;
  finally
    Unlock;
  end;
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
    inherited Delete(i);
  finally
    Unlock;
  end;
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
    inherited Empty;
  finally
    Unlock;
  end;
end;

function TPerObjThreadList.First: TtiObject;
begin
  Lock;
  try
    result := inherited First;
  finally
    Unlock;
  end;
end;

function TPerObjThreadList.GetCount: integer;
begin
  Lock;
  try
    result := inherited GetCount;
  finally
    Unlock;
  end;
end;

function TPerObjThreadList.IndexOf(const AObject: TtiObject): integer;
begin
  Lock;
  try
    result := inherited IndexOf(AObject);
  finally
    Unlock;
  end;
end;

function TPerObjThreadList.Last: TtiObject;
begin
  Lock;
  try
    result := inherited Last;
  finally
    Unlock;
  end;
end;

function TPerObjThreadList.LastExcludeDeleted: TtiObject;
begin
  Lock;
  try
    result := inherited LastExcludeDeleted;
  finally
    Unlock;
  end;
end;

procedure TPerObjThreadList.Lock;
begin
  FCriticalSection.Enter;  // If you want timeout capability, use a mutex
end;

procedure TPerObjThreadList.MarkListItemsDirty;
begin
  Lock;
  try
    inherited MarkListItemsDirty;
  finally
    Unlock;
  end;
end;

procedure TPerObjThreadList.MarkListItemsForDeletion;
begin
  Lock;
  try
    inherited MarkListItemsForDeletion;
  finally
    Unlock;
  end;
end;

function TPerObjThreadList.Remove(const AObject: TtiObject): integer;
begin
  Lock;
  try
    result := inherited Remove(AObject);
  finally
    Unlock;
  end;
end;

procedure TPerObjThreadList.SetItemOwner(const AValue: TtiObject);
begin
  Lock;
  try
    inherited SetItemOwner(AValue);
  finally
    Unlock;
  end;
end;

procedure TPerObjThreadList.SetItems(i: integer; const AValue: TtiObject);
begin
  Lock;
  try
    inherited SetItems(i, AValue);
  finally
    Unlock;
  end;
end;

procedure TPerObjThreadList.SortByProps(const ASortProps: array of string; AAscendingOrder : Boolean = True);
begin
  Lock;
  try
    inherited SortByProps(ASortProps, AAscendingOrder);
  finally
    Unlock;
  end;
end;

procedure TPerObjThreadList.SortByOID;
begin
  Lock;
  try
    inherited SortByOID;
  finally
    Unlock;
  end;
end;

procedure TPerObjThreadList.UnLock;
begin
  FCriticalSection.Leave;
end;

function TPerObjThreadList.FirstExcludeDeleted: TtiObject;
begin
  Lock;
  try
    result := inherited FirstExcludeDeleted;
  finally
    Unlock;
  end;
end;

{ TPerObjFactory }

function TPerObjFactory.Count: integer;
begin
  result := FList.Count;
end;

constructor TPerObjFactory.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

function TPerObjFactory.CreateInstance(const AClassName: string): TtiObject;
var
  lPerObjClassMapping : TPerObjClassMapping;
begin
  lPerObjClassMapping := FindByClassName(AClassName);
  Assert(lPerObjClassMapping <> nil,
          'Request for unrgister class <' +
          AClassName + '>');
  result := lPerObjClassMapping.PerObjAbsClass.Create;
end;

function TPerObjFactory.CreateNewInstance(const AClassName: string; AOwner : TtiObject = nil): TtiObject;
var
  lPerObjClassMapping : TPerObjClassMapping;
begin
  lPerObjClassMapping := FindByClassName(AClassName);
  Assert(lPerObjClassMapping <> nil,
          'Request for unrgister class <' +
          AClassName + '>');
  result := lPerObjClassMapping.PerObjAbsClass.CreateNew(AOwner);
end;

destructor TPerObjFactory.destroy;
begin
  FList.Free;
  inherited;
end;

function TPerObjFactory.FindByClassName(const AClassName: string): TPerObjClassMapping;
var
  i : integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
    if SameText(TPerObjClassMapping(FList.Items[i]).PerObjAbsClassName,
                 AClassName) then
    begin
      result := TPerObjClassMapping(FList.Items[i]);
      Break; //==>
    end;
end;

function TPerObjFactory.GetItems(AIndex: integer): TPerObjClassMapping;
begin
  result := TPerObjClassMapping(FList.Items[AIndex]);
end;

function TPerObjFactory.IsRegistered(const AClassName: string): boolean;
begin
  result := FindByClassName(AClassName) <> nil;
end;

procedure TPerObjFactory.RegisterClass(const AClassName: string; AClass : TtiClass);
var
  lPerObjClassMapping : TPerObjClassMapping;
begin
  lPerObjClassMapping := FindByClassName(AClassName);
  Assert(lPerObjClassMapping = nil,
          'Attempt to register duplicate class mapping <' +
          AClassName + '>');
  lPerObjClassMapping := TPerObjClassMapping.Create;
  lPerObjClassMapping.PerObjAbsClassName := AClassName;
  lPerObjClassMapping.PerObjAbsClass := AClass;
  FList.Add(lPerObjClassMapping);
end;


procedure TtiObjectList.Assign(const ASource: TtiObject);
begin
  Clear;
  inherited Assign(ASource);
end;

//function TtiObject.GetDispOrder: integer;
//begin
//  Result := FiDispOrder;
//end;

procedure TtiObject.SetAllObjectStates(const AObjectState: TPerObjectState);
var
  lVis : TVisSetAllObjectStates;
begin
  lVis := TVisSetAllObjectStates.Create;
  try
    lVis.ObjectState := AObjectState;
    Iterate(lVis);
  finally
    lVis.Free;
  end;
end;

procedure TPerObjFactory.UnRegisterClass(const AClassName: string);
var
  lPerObjClassMapping : TPerObjClassMapping;
begin
  lPerObjClassMapping := FindByClassName(AClassName);
  Assert(lPerObjClassMapping <> nil, 'Attempt to unregister class that is not retistered.');
  FList.Remove(lPerObjClassMapping);
end;

{ TVisSetAllObjectStates }

function TVisSetAllObjectStates.AcceptVisitor: boolean;
begin
  result := (Visited is TtiObject) and
            (TtiObject(Visited).ObjectState in [ posClean, posUpdate ]);
end;

procedure TVisSetAllObjectStates.Execute(const AVisited: TtiVisited);
begin
  Inherited Execute(AVisited);

  if not AcceptVisitor then
    exit; //==>

  TtiObject(Visited).ObjectState := FObjectState;
end;

procedure TtiObject.ReadThis(const ADBConnectionName: string; APersistenceLayerName : string = '');
begin
  {$IFDEF OID_AS_INT64}
    Assert(OID <> cNullOIDInteger, 'OID not assigned');
  {$ELSE}
    Assert(not OID.IsNull, 'OID not assigned');
  {$ENDIF}
  GTIOPFManager.ReadThis(Self, ADBConnectionName, APersistenceLayerName);
end;

procedure TtiObject.ReadThis;
begin
  ReadThis('', '');
end;

procedure TtiObjectList.AssignPublicProps(ASource: TtiObject);
begin
  inherited AssignPublicProps(ASource);
  
// Don't set these here, they will be set in a classes constructor  
//  OwnsObjects := TtiObjectList(ASource).OwnsObjects;
//  AutoSetItemOwner := TtiObjectList(ASource).AutoSetItemOwner;
end;

function TtiObjectList.Find(AOIDToFind: TtiOID): TtiObject;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if OIDEquals(Items[i].OID, AOIDToFind) then
    begin
      result := Items[i];
      Exit; //==>
    end;
  result := nil;
end;

function TtiObjectList.Find(AOIDToFind: TtiOID; ASortType: TtiPerObjListSortType): TtiObject;
var
  FindIndex: Integer;
begin
  FindIndex := IndexOf(AOIDToFind, ASortType);
  if FindIndex >= 0 then
    Result := Items[FindIndex]
  else
    Result := nil;
end;

function TtiObjectList.IndexOf(AOIDToFind: TtiOID; ASortType: TtiPerObjListSortType = stNone): integer;
begin
  case ASortType of
    stOID : Result := IndexOfBinary(AOIDToFind);
    stNone: Result := IndexOfFullScan(AOIDToFind);
  else
    raise EtiOPFInternalException.Create(cErrorInvalidSortType);
  end;
end;

{ TtiObjectErrors }

function TtiObjectErrors.Add(const AObject: TtiObjectError): Integer;
begin
  Result := inherited Add(AObject);
end;

procedure TtiObjectErrors.AddError(const AErrorProperty: string;
  const AErrorMessage: string; AErrorCode: Word);
var
  lError: TtiObjectError;
begin
  lError := TtiObjectError.Create;
  lError.ErrorProperty  := AErrorProperty;
  lError.ErrorMessage   := AErrorMessage;
  lError.ErrorCode      := AErrorCode;
  Add(lError);
end;

procedure TtiObjectErrors.AddError(const AErrorMessage: string);
begin
  AddError('', AErrorMessage, 0);
end;

procedure TtiObjectErrors.AddError(
  const AErrorProperty : string;
  const AErrorMessage: string);
begin
  AddError(AErrorProperty, AErrorMessage, 0);
end;

function TtiObjectErrors.FindByMessage(const AMessage: string): TtiObjectError;
var
  i : integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if SameText(Items[i].ErrorMessage, AMessage) then
    begin
      result := Items[i];
      Exit; //==>
    end;
end;

function TtiObjectErrors.FindByErrorCode(const AErrorCode: Word): TtiObjectError;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].ErrorCode = AErrorCode then
    begin
      Result := Items[i];
      Exit; //==>
    end;
  end;
end;

function TtiObjectErrors.FindByErrorProperty(const AErrorProperty: string): TtiObjectError;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].ErrorProperty, AErrorProperty) then
    begin
      Result := Items[i];
      Exit; //==>
    end;
  end;
end;

function TtiObjectErrors.GetAsString: string;
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

function TtiObjectErrors.GetItems(i: integer): TtiObjectError;
begin
  result := TtiObjectError(inherited GetItems(i));
end;

procedure TtiObjectErrors.SetItems(i: integer; const AValue: TtiObjectError);
begin
  inherited SetItems(i, AValue);
end;

{ TtiObjectError }

function TtiObjectError.GetOwner: TtiObjectErrors;
begin
  result := TtiObjectErrors(inherited GetOwner);
end;

procedure TtiObjectError.SetOwner(const AValue: TtiObjectErrors);
begin
  inherited SetOwner(AValue);
end;

function TtiObject.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  Assert(AErrors.TestValid(TtiObjectErrors), CTIErrorInvalidObject);
  result := true;
  AErrors.Clear;
end;

function TtiObject.IsValid(var AErrorMessage: string): boolean;
var
  lErrors : TtiObjectErrors;
  i : integer;
begin
  lErrors := TtiObjectErrors.Create;
  try
    result := IsValid(lErrors);
    for i := 0 to lErrors.Count - 1 do
    begin
      AErrorMessage := tiAddTrailingValue(AErrorMessage, Cr);
      AErrorMessage := AErrorMessage + lErrors.Items[i].ErrorMessage;
    end;
  finally
    lErrors.Free;
  end;
end;

function TtiObject.IsValid: boolean;
var
  lErrors : TtiObjectErrors;
begin
  lErrors := TtiObjectErrors.Create;
  try
    result := IsValid(lErrors);
  finally
    lErrors.Free;
  end;
end;

function TtiObject.IsValid(const AStrings: TStrings): boolean;
var
  lMessage : string;
begin
  result := IsValid(lMessage);
  AStrings.Text := lMessage;
end;

function TtiObject.IsValid(const AStrings : TStrings; AAppend : boolean): boolean; 
var
  lsl : TStringList;
  i  : integer;
begin
  lsl := TStringList.Create;
  try
    result := IsValid(lsl);
    if not AAppend then
      AStrings.Clear;
    for i := 0 to lsl.count - 1 do
      AStrings.Add(lsl.strings[i]);
  finally
    lsl.Free;
  end;
end;

destructor TtiObject.Destroy;
begin
  {$IFNDEF OID_AS_INT64}
    FOID.Free;
  {$ENDIF}
  FObserverList.Free;
  FObserverList := nil;
  inherited;
end;

function TtiObject.Find(AOIDToFindAsString: string): TtiObject;
{$IFNDEF OID_AS_INT64}
  var
    lOID : TtiOID;
{$ENDIF}
begin
  {$IFDEF OID_AS_INT64}
    result := Find(StrToInt(AOIDToFindAsString));
  {$ELSE}
    lOID := OIDGenerator.OIDClass.Create;
    try
      lOID.AsString := AOIDToFindAsString;
     result := Find(lOID);
    finally
      lOID.Free;
    end;
  {$ENDIF}
end;

function TtiObjectList.Find(AOIDToFindAsString: string): TtiObject;
var
  i : integer;
{$IFNDEF OID_AS_INT64}
  lOIDToFind : TtiOID;
{$ENDIF}
begin
  {$IFDEF OID_AS_INT64}
    for i := 0 to Count - 1 do
      if Items[i].OID = StrToInt(AOIDToFindAsString) then
      begin
        result := Items[i];
        Exit; //==>
      end;
    result := nil;
  {$ELSE}
    lOIDToFind := OIDGenerator.OIDClass.Create;
    try
      lOIDToFind.AsString := AOIDToFindAsString;
      for i := 0 to Count - 1 do
        if Items[i].OID.Equals(lOIDToFind) then
        begin
          result := Items[i];
          Exit; //==>
        end;
      result := nil;
    finally
      lOIDToFind.Free;
    end;
  {$ENDIF}
end;

function TtiObject.FindAll(ATIObjectFindMethodExt: TPerObjFindMethodExt; AList: TList; AUserContext: Pointer): integer;
var
  lVis : TVisPerObjFind;
begin
  if AList <> nil then
    AList.Clear;
  lVis := TVisPerObjFind.Create;
  try
    lVis.FoundList := AList;
    lVis.UserContext := AUserContext;
    lVis.FPerObjFindMethodExt := ATIObjectFindMethodExt;
    self.Iterate(lVis);
  finally
    lVis.Free;
  end;
  result := AList.Count;
end;

procedure TtiObject.DoFindAllNotUnique(AObject: TtiObject;
  var AFound: boolean; AData : TtiObject);
begin
  Assert(Assigned(AData), 'AData not assigned');
  AFound :=
    (OIDEquals(AObject.OID, AData.OID)) and
    (AObject <> AData) and
    (not AObject.Deleted);
end;

function TtiObject.FindAll(ATIObjectFindMethodData: TPerObjFindMethodData;
  AList: TList; AData: TtiObject): integer;
var
  lVis : TVisPerObjFind;
begin
  if AList <> nil then
    AList.Clear;
  lVis := TVisPerObjFind.Create;
  try
    lVis.FoundList := AList;
    lVis.Data := AData;
    lVis.PerObjFindMethodData := ATIObjectFindMethodData;
    self.Iterate(lVis);
  finally
    lVis.Free;
  end;
  result := AList.Count;
end;

function TtiObject.GetParent: TtiObject;
begin
  if (Owner is TtiObjectList) and
     Assigned(Owner.Owner) then
    result:= Owner.Owner
  else
    result:= Owner;
end;

function TtiObject.GetPropValue(const APropName: string): Variant;
var
  lDate : TDateTime;
  lbValue : boolean;
  lTypeKind : TtiTypeKind;
begin
  if SameText(APropName, 'OID') then
  {$IFDEF OID_AS_INT64}
    result := Integer(OID)
  {$ELSE}
    result := OID.AsVariant
  {$ENDIF}
//  else if SameText(APropName, 'DispOrder') then
//    result := DispOrder
  else
  begin
    Assert(IsPublishedProp(Self, APropName), APropName + ' is not a published property on ' + ClassName);
    try
      lTypeKind := tiGetSimplePropType(Self, APropName);
      case lTypeKind of
      tiTKDateTime : begin
                       lDate := TypInfo.GetPropValue(Self, APropName);
                       result := lDate;
                     end;
      tiTKBoolean :  begin
                       lbValue := TypInfo.GetPropValue(Self, APropName);
                       result := lbValue;
                     end;
      else
        result := TypInfo.GetPropValue(Self, APropName);
      end;
    except
      on e:exception do
        raise EtiOPFProgrammerException.CreateFmt(
          cErrorGettingProperty, [APropName, ClassName, e.Message]);
    end;
  end;
end;

procedure TtiObject.SetPropValue(const APropName: string; const APropValue: Variant);
var
  ldtValue : TDateTime;
  lsValue : string;
  liValue : integer;
  lbValue : boolean;
  lPropTypeKind : TtiTypeKind;
  lValueTypeKind : TtiTypeKind;
begin

  // OID is a special case. Should we publish OID?
  if SameText(APropName, 'OID') then
  begin
    {$IFDEF OID_AS_INT64}
      OID := Integer(APropValue);
    {$ELSE}
      OID.AsVariant := APropValue;
    {$ENDIF}
    Exit; //==>
  end;

  if not IsPublishedProp(Self, APropName) then
    raise EtiOPFProgrammerException.CreateFmt(cErrorAttemptToSetNonPublishedProperty,
      [APropName,ClassName, VarToStr(APropValue) ]);

  try

    lPropTypeKind := tiGetSimplePropType(Self, APropName);
    case lPropTypeKind of
    tiTKDateTime : begin
                     ldtValue := VarToDateTime(APropValue);
                     TypInfo.SetPropValue(Self, APropName, ldtValue);
                   end;
    tiTKBoolean : begin
                    lValueTypeKind := tiVarSimplePropType(APropValue);
                    case lValueTypeKind of
                    tiTKString : begin
                                   lsValue := APropValue;
                                   if SameText(lsValue, 'true') or
                                     (lsValue = '1') or
                                     SameText(lsValue, 't') then
                                     TypInfo.SetPropValue(Self, APropName, 1)
                                   else
                                     TypInfo.SetPropValue(Self, APropName, 0);
                                 end;
                    tiTKInteger : begin
                                    liValue := APropValue;
                                    if liValue = 0 then
                                      TypInfo.SetPropValue(Self, APropName, 0)
                                    else
                                      TypInfo.SetPropValue(Self, APropName, 1);
                                  end;
                    tiTKBoolean : begin
                                    lbValue := APropValue;
                                    if lbValue then
                                      TypInfo.SetPropValue(Self, APropName, 1)
                                    else
                                      TypInfo.SetPropValue(Self, APropName, 0);
                                  end;
                    else
                      raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
                        [ClassName, APropName, 'Unknown type' ]);
                    end
                  end;
    else
      TypInfo.SetPropValue(Self, APropName, APropValue);
    end;

  except
    on e:exception do
      raise EtiOPFProgrammerException.CreateFmt(cErrorSettingProperty,
        [ClassName, APropName, e.message ]);
  end;
end;

function TtiObject.IsReadWriteProp(const APropName: string): boolean;
begin
  result :=
    SameText(APropName, 'OID') or
    tiIsReadWriteProp(Self, APropName);   
end;

{ TPerObjClassMapping }

constructor TPerObjClassMapping.Create;
begin
  inherited;
end;

{ TPerObjFieldAbs }

constructor TtiFieldAbs.Create(const AOwner: TtiObject; const ANullValidation: TtiNullValidation);
begin
  inherited Create;
  FOwner := AOwner;
  FNullValidation := ANullValidation;
  Clear;
end;

constructor TtiFieldAbs.Create(const AOwner: TtiObject);
begin
  Create(AOwner, nvAllowNull);
end;


procedure TtiFieldAbs.Clear;
begin
  FIsNull := True;
end;

procedure TtiFieldAbs.SetValue;
begin
  FIsNull := False;
end;

function TtiFieldAbs.IsValidValue(const AErrors: TtiObjectErrors): Boolean;
begin
  Assert(Owner.TestValid(TtiObject), CTIErrorInvalidObject);
  Result := (NullValidation = nvAllowNull) or (not IsNull);
  if Assigned(AErrors) and (not Result) then
    AErrors.AddError(FieldName,
                     Format(cErrorFieldNotAssigned, [Owner.ClassName + '.' + FieldName, Owner.OID]));
end;

{ TtiFieldString }

constructor TtiFieldString.Create(
  const AOwner: TtiObject;
  const ANullValidation: TtiNullValidation;
  const AMaxLength: Integer);
begin
  inherited Create(AOwner, ANullValidation);
  FMaxLength := AMaxLength;
end;

procedure TtiFieldString.Assign(AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldString), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsString:= AAssignFrom.AsString;
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

procedure TtiFieldString.SetAsString(const AValue: string);
begin
  FValue := AValue;
  SetValue;
end;

function TtiFieldString.IsValidValue(const AErrors : TtiObjectErrors): Boolean;
begin
  Result := inherited IsValidValue(AErrors);
  if Result then begin
    Result := (MaxLength = 0) or (Length(AsString) <= MaxLength);
    if Assigned(AErrors) and (not Result) then
      AErrors.AddError(FieldName,
                       Format(cErrorFieldTooLong,
                              [ClassName + '.' + FieldName, MaxLength, Length(AsString)]));
  end;
end;

function TtiFieldString.Equals(ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldString), CTIErrorInvalidObject);
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (AsString = ACompareWith.AsString);
end;

{ TtiFieldInteger }

constructor TtiFieldInteger.Create(
  const AOwner: TtiObject;
  const ANullValidation: TtiNullValidation;
  const AMaxDigits: Integer);
begin
  inherited Create(AOwner, ANullValidation);
  FMaxDigits := AMaxDigits;
end;

procedure TtiFieldInteger.Assign(AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldInteger), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsInteger:= (AAssignFrom as TtiFieldInteger).AsInteger;
end;

procedure TtiFieldInteger.Clear;
begin
  inherited;
  FValue := 0;
end;

function TtiFieldInteger.GetAsString: string;
begin
  if not IsNull then
    Result := IntToStr(FValue)
  else
    Result:= '';
end;

procedure TtiFieldInteger.SetAsString(const AValue: string);
begin
  if AValue <> '' then
    FValue := StrToInt(AValue)
  else
    FValue := 0;
  SetValue;
end;

procedure TtiFieldInteger.SetAsInteger(const AValue: Int64);
begin
  FValue := AValue;
  SetValue;
end;

function TtiFieldInteger.IsValidValue(const AErrors : TtiObjectErrors): Boolean;
begin
  Result := inherited IsValidValue(AErrors);
  if Result then begin
    // The sign of the number is not considered to be a digit. ie. A maximum
    // digits = 3 can validly store 999 and -999 but not 9999 or -9999
    Result := (MaxDigits = 0) or
              ((AsInteger >= 0) and (Length(AsString) <= MaxDigits)) or
              ((AsInteger < 0) and (Length(AsString) <= (MaxDigits+1)));
    if Assigned(AErrors) and (not Result) then
      AErrors.AddError(FieldName,
                       Format(cErrorFieldTooLong,
                              [ClassName + '.' + FieldName, MaxDigits, Length(AsString)]));
  end;
end;

function TtiFieldInteger.Equals(ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldAbs), CTIErrorInvalidObject);
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (AsInteger = (ACompareWith as TtiFieldInteger).AsInteger);
end;

{ TtiFieldFloat }

procedure TtiFieldFloat.Assign(AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldFloat), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsFloat:= (AAssignFrom as TtiFieldFloat).AsFloat;
end;

procedure TtiFieldFloat.Clear;
begin
  inherited;
  FValue := 0;
end;

constructor TtiFieldFloat.Create(const AOwner: TtiObject;
  const ANullValidation: TtiNullValidation; const APrecision: Integer);
begin
  inherited Create(AOwner, ANullValidation);
  SetPrecision(APrecision);
end;

constructor TtiFieldFloat.Create(const AOwner: TtiObject);
begin
  Create(AOwner, nvAllowNull);
end;

constructor TtiFieldFloat.Create(const AOwner: TtiObject;
  const ANullValidation: TtiNullValidation);
begin
  // Precision of 0 will return a string to what ever precision the float values is stored
  Create(AOwner, ANullValidation, 0);
end;

function TtiFieldFloat.Equals(ACompareWith: TtiFieldAbs): Boolean;
var
  lF1: extended;
  lF2: extended;
begin
  Assert(ACompareWith.TestValid(TtiFieldFloat), CTIErrorInvalidObject);
  lF1 := AsFloat;
  lF2 := (ACompareWith as TtiFieldFloat).AsFloat;
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (SameValue(lF1, lF2, Epsilon));
end;

function TtiFieldFloat.GetAsString: string;
begin
  if FPrecision = 0 then
    Result := FloatToStr(FValue)
  else
    Result := tiFloatToStr(FValue, FPrecision);
end;

procedure TtiFieldFloat.SetAsFloat(const AValue: Extended);
begin
  FValue := AValue;
  SetValue;
end;

procedure TtiFieldFloat.SetAsString(const AValue: string);
begin
  if AValue <> '' then
    FValue := StrToFloat(AValue)
  else
    FValue := 0;
  SetValue;
end;

procedure TtiFieldFloat.SetPrecision(const AValue: Integer);
begin
  FPrecision := AValue;
  FEpsilon := Power(10, -(AValue+1))*2;
end;

{ TtiFieldBoolean }

procedure TtiFieldBoolean.Assign(AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldBoolean), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsBoolean:= (AAssignFrom as TtiFieldBoolean).AsBoolean;
end;

procedure TtiFieldBoolean.Clear;
begin
  inherited;
  FValue := False;
end;

function TtiFieldBoolean.GetAsString: string;
begin
{$IFDEF BOOLEAN_CHAR_1}
  if FValue then
    result := 'T'
  else
    result := 'F';
{$ELSE}
  if FValue then
    result := 'TRUE'
  else
    result := 'FALSE';
{$ENDIF}
end;

procedure TtiFieldBoolean.SetAsString(const AValue: string);
begin
  if SameText(AValue, 'TRUE') or
     SameText(AValue, 'T') or
     SameText(AValue, 'Y') or
     (AValue = '1') then
  begin
    FValue := True;
    SetValue;
  end
  else if SameText(AValue, 'FALSE') or
     SameText(AValue, 'F') or
     SameText(AValue, 'N') or
     (AValue = '0') then
  begin
    FValue := False;
    SetValue;
  end
  else
    Clear; //AValue is null
end;

procedure TtiFieldBoolean.SetAsBoolean(const AValue: Boolean);
begin
  FValue := AValue;
  SetValue;
end;

function TtiFieldBoolean.Equals(ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldBoolean), CTIErrorInvalidObject);
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (AsBoolean = (ACompareWith as TtiFieldBoolean).AsBoolean);
end;

{ TtiFieldDateTime }

procedure TtiFieldDateTime.Assign(AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldDateTime), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsDateTime:= (AAssignFrom as TtiFieldDateTime).AsDateTime;
end;

procedure TtiFieldDateTime.Clear;
begin
  inherited;
  FValue := 0;
end;

function TtiFieldDateTime.GetAsString: string;
begin
  Result := tiDateTimeAsXMLString(FValue);
end;

procedure TtiFieldDateTime.SetAsString(const AValue: string);
begin
  FValue := tiXMLStringToDateTime(AValue);
  SetValue;
end;

procedure TtiFieldDateTime.SetAsDateTime(const AValue: TDateTime);
begin
  FValue := AValue;
  SetValue;
end;

function TtiFieldDateTime.GetDays: Word;
var
  Year, Month, Day: word;
begin
  if IsNull then
    Result := 0
  else
  begin
    DecodeDate(FValue, Year, Month, Day);
    Result := Day;
  end;
end;

function TtiFieldDateTime.GetHours: Word;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(FValue, Hour, Min, Sec, MSec);
  Result := Hour;
end;

function TtiFieldDateTime.GetMinutes: Word;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(FValue, Hour, Min, Sec, MSec);
  Result := Min;
end;

function TtiFieldDateTime.GetMonths: Word;
var
  Year, Month, Day: word;
begin
  if IsNull then
    Result := 0
  else
  begin
    DecodeDate(FValue, Year, Month, Day);
    Result := Month;
  end;
end;

function TtiFieldDateTime.GetSeconds: Word;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(FValue, Hour, Min, Sec, MSec);
  Result := Sec;
end;

function TtiFieldDateTime.GetYears: Word;
var
  Year, Month, Day: word;
begin
  if IsNull then
    Result := 0
  else
  begin
    DecodeDate(FValue, Year, Month, Day);
    Result := Year;
  end;
end;

function TtiFieldAbs.GetFieldName: string;
var
  lsl: TStringList;
  i : Integer;
begin
  if FFieldName = '' then
  begin
    Assert(Owner.TestValid(TtiObject), CTIErrorInvalidObject);
    lsl:= TStringList.Create;
    try
      tiGetPropertyNames(Owner,lsl, [tkClass]);
      for i := 0 to lsl.Count - 1 do
        if GetObjectProp(Owner, lsl.Strings[i]) = Self then
        begin
          FFieldName := lsl.Strings[i];
          Break; //==>
        end;
        if FFieldName = '' then
          raise Exception.CreateFmt(cErrorUnableToDetermineFieldName, [Owner.ClassName]);
    finally
      lsl.Free;
    end;
  end;
  Result := FFieldName;
end;

function TtiFieldDateTime.Equals(ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldDateTime), CTIErrorInvalidObject);
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (SameValue(AsDateTime, (ACompareWith as TtiFieldDateTime).AsDateTime, cdtOneSecond/2));
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

function TtiFieldList.GetItem(AIndex: Integer): TtiFieldAbs;
begin
  Result := TtiFieldAbs(inherited Items[AIndex]);
end;

procedure TtiFieldList.SetItem(AIndex: Integer; AObject: TtiFieldAbs);
begin
  inherited Items[AIndex]:= AObject;
end;

procedure TtiObject.ForceAsCreate(const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
begin
  {$IFDEF OID_AS_INT64}
    OID := GTIOPFManager.DefaultPerLayer.NextOIDMgr.NextOID;
  {$ELSE}
    OIDGenerator.AssignNextOID(OID);
  {$ENDIF}
  ObjectState := posCreate;
end;

function TtiObject.PropType(const APropName: string): TtiTypeKind;
begin
  Assert(APropName <> '', 'APropName not assigned');
  Result := tiGetSimplePropType(Self, APropName);
end;

function TtiObjectList.FindInHierarchy(AOIDToFind: TtiOID): TtiObject;
begin
  Result := Find(AOIDToFind);
  if Result = nil then
    Result := inherited Find(AOIDToFind);
end;

type

  {: Stream a TtiObject out as text for debugging.
     A typical use may be tiShowMessage(FMyObject.AsDebugString)}
  TVisTIObjectAsDebugString = class(TVisStringStream)
  private
    FToShow: TtiObjectAsDebugStringValuesToShow;
  protected
    function    AcceptVisitor: Boolean; override;
  public
    constructor Create; overload; override;
    constructor Create(const AIncludeDeleted: Boolean; const ADataOnly: Boolean = False); reintroduce; overload;
    constructor Create(const AValuesToShow: TtiObjectAsDebugStringValuesToShow); reintroduce; overload;
    procedure   Execute(const AVisited: TtiVisited); override;
    function    Indent: string;
    property    ToShow:TtiObjectAsDebugStringValuesToShow read FToShow write FToShow;
  end;

{ TVisTIObjectAsDebugString }

function TVisTIObjectAsDebugString.AcceptVisitor : boolean;
begin
  result := (Visited is TtiObject) and
            ((adsDeleted in ToShow) or
             ((not TtiObject(Visited).Deleted) and
              (not TtiObject(Visited).Deleted))) and
            ((Depth <= 1) or (adsData in ToShow));
end;

constructor TVisTIObjectAsDebugString.Create;
begin
  inherited;
  FToShow:= CTIAsDebugStringDataAll;
end;

constructor TVisTIObjectAsDebugString.Create(
  const AIncludeDeleted: Boolean;
  const ADataOnly: Boolean);
begin
  Create;
  if ADataOnly then
    FToShow:= [adsData, adsChildren];
  if not AIncludeDeleted then
    FToShow:= FToShow - [adsDeleted];
end;

// ToDo: TVisTIObjectAsDebugString.Execute is too long. Refactor & unit test
procedure TVisTIObjectAsDebugString.Execute(const AVisited: TtiVisited);
var
  i: integer;
  LProperties: TStringList;
  LPropValue: string;
  LPropName: string;
  LNewLineRequired: boolean;
begin
  inherited Execute(AVisited);

  if not AcceptVisitor then
    Exit; //==>

  LNewLineRequired:= false;

  if (adsClassName in ToShow) and
     (AVisited.ClassName <> (AVisited).Caption) then
  begin
    Write(Indent + TtiObject(AVisited).ClassName);
    Write(', ');
    LNewLineRequired:= True;
  end
  else
    Write(Indent);

  if adsCaption in ToShow then
  begin
    Write(AVisited.Caption);
    Write(', ');
    LNewLineRequired:= True;
  end;

  if adsObjectState in ToShow then
  begin
    Write(TtiObject(AVisited).ObjectStateAsString);
    Write(', ');
    LNewLineRequired:= True;
  end;

  if adsOID in ToShow then
  begin
    {$IFDEF OID_AS_INT64}
      Write(IntToStr(TtiObject(AVisited).OID));
    {$ELSE}
      if (GTIOPFManager.DefaultOIDGenerator.OIDClass <> nil) then
        Write('OID=' +TtiObject(AVisited).OID.AsString)
      else
        Write('OID=Null');
    {$ENDIF}
    Write(', ');
    LNewLineRequired:= True;
  end;

  if (TtiObject(AVisited).Dirty) and
     (adsObjectState in ToShow) then
  begin
    Write('**Dirty**');
    LNewLineRequired:= True;
  end;

  if LNewLineRequired then
    WriteLn('');

  if adsData in ToShow then
  begin
    LProperties := TStringList.Create;
    try
      tiGetPropertyNames(TtiBaseObject(AVisited),
                          LProperties,
                          ctkSimple + [tkVariant, tkEnumeration]);

      for i := 0 to LProperties.Count - 1 do
      begin
        LPropName := LProperties.Strings[i];
        if not SameText(LPropName, 'Caption') then
        begin
          try
            LPropValue := TtiObject(AVisited).PropValue[LPropName];
            if TtiObject(AVisited).PropType(LPropName) = tiTKDateTime then
              LPropValue := tiDateTimeAsIntlDateDisp(TtiObject(AVisited).PropValue[LPropName])
            else
              LPropValue := TtiObject(AVisited).PropValue[LPropName];
          except
            on e:exception do
              LPropValue := 'Error: ' + e.Message;
          end;
          WriteLn(Indent + '  ' +
                LPropName +
                ' = ' +
                LPropValue);
        end;
      end;
    finally
      LProperties.Free;
    end;
  end;

  if not( adsChildren in ToShow) then
    ContinueVisiting:= False;

end;


function TVisTIObjectAsDebugString.Indent: string;
begin
  result := tiSpace((Depth - 1) * 2);
end;


constructor TVisTIObjectAsDebugString.Create(
  const AValuesToShow: TtiObjectAsDebugStringValuesToShow);
begin
  Create;
  ToShow:= AValuesToShow;
end;                           

function TtiObject.AsDebugString(
  const AValuesToShow: TtiObjectAsDebugStringValuesToShow
                         = CTIAsDebugStringDataAll): string;
var
  lVisitor : TVisTIObjectAsDebugString;
begin
  lVisitor := TVisTIObjectAsDebugString.Create(AValuesToShow);
  try
    Self.Iterate(lVisitor);
    result := Trim(lVisitor.Text);
    result:= tiTrim(result, ',');
  finally
    lVisitor.Free;
  end;
end;

{ Note: This functionality has not been tested fully. Bit of a hack really :(
        Talk about thrashing the CPU with out need.:(:(:(!  }
function TtiObject.Equals(const AData: TtiObject): boolean;
var
  lVisComp : TVisTIObjectAsDebugString;
  lVisSelf : TVisTIObjectAsDebugString;
begin
  // ToDo: Better to do this as a field by field compare...
  lVisComp := TVisTIObjectAsDebugString.Create(False, True);
  try
    lVisSelf := TVisTIObjectAsDebugString.Create(False, True);
    try
      Self.Iterate(lVisSelf);
      AData.Iterate(lVisComp);
      result := lVisSelf.Text = lVisComp.Text;
    finally
      lVisSelf.Free;
    end;
  finally
    lVisComp.Free;
  end;
end;

{$IFDEF OID_AS_INT64}
procedure TtiObject.SetOID(const AValue: TtiOID);
begin
  FOID := AValue;
end;
{$ENDIF}

function TtiObject.PropCount(APropFilter: TTypeKinds = ctkSimple): integer;
var
  lsl : TStringList;
begin
  lsl := TStringList.Create;
  try
    tiGetPropertyNames(Self, lsl, APropFilter);
    result := lsl.Count;
  finally
    lsl.Free;
  end;
end;

procedure TtiFieldAbs.SetFieldName(const AValue: string);
begin
  FFieldName:= AValue;
end;

procedure TtiFieldAbs.SetIsNull(const AValue: Boolean);
begin
  if AValue then
    Clear
  else
    FIsNull := false;
end;

function TtiObjectList.IndexOfBinary(AOIDToFind: TtiOID): integer;
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
    LCompareResult := OIDCompare(Items[LMid].OID, AOIDToFind);
    if LCompareResult > 0 then
      LHigh := LMid - 1
    else if LCompareResult < 0 then
      LLow := LMid + 1
    else begin
      Result := LMid;
      Exit; //==>
    end;
  end;
  Result := -1;
end;

function TtiObjectList.IndexOfFullScan(AOIDToFind: TtiOID): integer;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if OIDEquals(Items[i].OID, AOIDToFind) then
    begin
      Result := i;
      Exit; //==>
    end;
  Result := -1;
end;

function TtiObjectList.FirstExcludeDeleted: TtiObject;
var
  i : integer;
begin
  for i := 0 to Count-1 do
    if not Items[i].Deleted then
    begin
      result := Items[i];
      Exit; //==>
    end;
  result := nil;
end;

procedure TtiObjectList.CompareWith(AList: TtiObjectList; AInBothAndEquals,
  AInBothAndNotEquals, AIn1Only, AIn2Only: TtiObjectListCompareEvent);
var
  i: Integer;
  L: TtiObject;
begin
  Assert(AList.TestValid, CTIErrorInvalidObject);
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

procedure TtiObject.AttachObserver(AObserver: TtiObject);
begin
  { To conserve memory, we only create FObserverList when needed }
  if ObserverList.IndexOf(AObserver) = -1 then
    FObserverList.Add(AObserver);
end;

procedure TtiObject.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TtiObject.DetachObserver(AObserver: TtiObject);
begin
  if not Assigned(FObserverList) then
    Exit; //==>

  FObserverList.Remove(AObserver);
  { To conserve memory, we free FObserverList when not used anymore }
  if FObserverList.Count = 0 then
  begin
    FObserverList.Free;
    FObserverList := nil;
  end;
end;

procedure TtiObject.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      NotifyObservers;
  end;
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
    if Assigned(Observer) then
      Observer.Update(self);
  end;
end;

procedure TtiObject.Update(ASubject: TtiObject);
begin
  { Do nothing here. This will be implemented in decendant classes when needed }
end;

function TtiObject.GetObserverList: TList;
begin
  if not Assigned(FObserverList) then
    FObserverList := TList.Create;
  Result := FObserverList;
end;

procedure TtiObjectList.FreeDeleted;
var
  i: Integer;
begin
  // ToDo: Should really be handled through a visitor that iterates,
  //       bottom up, freeing all nested objects
  for i:= Pred(Count) downto 0 do
    if Items[i].ObjectState = posDeleted then
      Delete(i);
end;

{ TtiFieldStringMethod }

procedure TtiFieldStringMethod.Assign(AAssignFrom: TtiFieldAbs);
begin
  Assert(False, 'Assign not implemented');
end;

procedure TtiFieldStringMethod.Clear;
begin
  // Do nothing
end;

constructor TtiFieldStringMethod.Create(const AOwner: TtiObject;
  const AReadMethod: TtiStringFieldMethodReadEvent);
begin
  Assert(Assigned(AOwner), 'AOwner not assigned');
  Assert(Assigned(AReadMethod), 'AReadMethod not assigned');
  inherited Create(AOwner);
  FReadEvent:= AReadMethod;
end;

function TtiFieldStringMethod.Equals(ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldString), CTIErrorInvalidObject);
  Result :=(AsString = ACompareWith.AsString);
end;

function TtiFieldStringMethod.GetAsString: string;
var
  LS: string;
begin
  Assert(Assigned(FReadEvent), 'FReadEvent not assigned');
  LS:= '';
  FReadEvent(Self, LS);
  Result:= LS;
end;

function TtiFieldStringMethod.IsValidValue(const AErrors: TtiObjectErrors): Boolean;
begin
  result:= false;
  Assert(False, 'Not implemented');
end;

procedure TtiFieldStringMethod.SetAsString(const AValue: string);
begin
  Assert(False, 'Not implemented');
end;

{ TtiEnumerator }

constructor TtiEnumerator.Create(AList: TtiObjectList);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

function TtiEnumerator.GetCurrent: TtiObject;
begin
  Result := FList[FIndex];
end;

function TtiEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

end.


