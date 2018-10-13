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
{$IFDEF IOS}
  ,System.Generics.Defaults
  ,Generics.Collections
{$ELSE}
  ,Contnrs
{$ENDIF IOS}
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,SyncObjs   // This must come after the Windows unit!
  ,tiExcept
 ;

resourcestring
  cErrorFieldNotAssigned = 'Field <%s> is null. OID=%d';
  cErrorFieldTooLong = 'Field <%s> field too long. Allowed length %d, current length %d';
  cErrorUnableToDetermineFieldName = 'Unable to determine field name on <%s>';
  cErrorInvalidObjectState   = 'Invalid ObjectState';
  cErrorNoFindMethodAssigned = 'No find method assigned';
  cErrorAttemptToSetNonPublishedProperty = 'Attempt to set non-published property %s.%s to %s';
  cErrorInvalidSortType = 'Invalid TtiPerObjListSortType';
  CErrorDefaultOIDGeneratorNotAssigned = 'Default OIDGenerator not assigned. You must register an instance of TOIDGenerator with the global GTIOPFManager.';
  cError = 'Error: ';
  CErrorInvalidDate = 'A DateTime was passed when a Date was expected. DateTime="%s"';
  
type

  TPerObjectState = (
                      posEmpty,
                      posPK,
                      posCreate,
                      posUpdate,
                      posDelete,
                      posDeleted,
                      posClean,
                      posLoading
                    );

  {: The possible values to be displayed by TtiObjectAsDebugString}
  TtiObjectAsDebugStringValues =
    (adsData, adsClassName, adsObjectState, adsOID, adsCaption, adsDeleted, adsChildren, adsChildCount);

  TtiObjectAsDebugStringValuesToShow = set of TtiObjectAsDebugStringValues;

  {: Various observer Notifications }
  TNotifyOperation = (noChanged, noAddItem, noDeleteItem, noFree, noCustom, noReSort);

const
  CTIAsDebugStringDataAll =
    [adsData, adsClassName, adsObjectState, adsOID, adsCaption, adsDeleted, adsChildren, adsChildCount];

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

  TtiObjectEventRegular = procedure(const AObject: TtiObject);
  TtiObjectEvent = procedure(const AObject: TtiObject) of object;
  TtiObjectUpdateEvent = procedure(ASubject: TtiObject; AOperation: TNotifyOperation) of object;

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
{$M+}
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
    function    Equals(const ACompareWith: TtiFieldAbs): Boolean; reintroduce; virtual; abstract;
    procedure   Assign(const AAssignFrom: TtiFieldAbs); virtual; abstract;

    property    Owner:            TtiObject         read FOwner;
    property    FieldName:        string            read GetFieldName write SetFieldName;
    property    NullValidation:   TtiNullValidation read FNullValidation write FNullValidation;
    property    IsNull:           Boolean           read FIsNull write SetIsNull;
  published
    property    AsString:         string            read GetAsString Write SetAsString;
  end;
{$M-}

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
    function    Equals(const ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(const AAssignFrom: TtiFieldAbs); override;

    property    MaxLength: Integer read FMaxLength;
  end;

  TtiFieldStringMethod = class;
  TtiStringFieldMethodReadEvent = procedure(const ASender: TtiFieldStringMethod; var AValue: string) of object;

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
    function    Equals(const ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(const AAssignFrom: TtiFieldAbs); override;
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
    function    Equals(const ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(const AAssignFrom: TtiFieldAbs); override;

    property    MaxDigits : Integer read FMaxDigits;
  published
    property    AsInteger : Int64 read FValue Write SetAsInteger;
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
    function    Equals(const ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(const AAssignFrom: TtiFieldAbs); override;
    property    Precision: Integer Read FPrecision Write SetPrecision;
    property    Epsilon: Extended Read FEpsilon Write FEpsilon;
  published
    property    AsFloat: Extended read FValue Write SetAsFloat;
  end;

  {: Concrete persistent currency field}
  TtiFieldCurrency = class(TtiFieldAbs)
  private
    FValue: Int64;
    procedure SetAsInteger(const Value: Int64);
  protected
    procedure   Clear; override;
    procedure   SetAsString(const AValue: string);  override;
    function    GetAsString: string;               override;
    procedure   SetAsFloat(const AValue: Extended); virtual;
    function    GetAsFloat: Extended; virtual;
    function    GetAsCurrencyString: string;
    procedure   SetAsCurrencyString(const AValue: string);
  public
    constructor Create(const AOwner: TtiObject); overload; override;
    constructor Create(const AOwner: TtiObject;
                       const ANullValidation: TtiNullValidation); overload; override;
    function    Equals(const ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(const AAssignFrom: TtiFieldAbs); override;
    property    AsFloat: Extended read GetAsFloat Write SetAsFloat;
    property    AsInteger: Int64 read FValue write SetAsInteger;
    procedure   Inc(const AField: TtiFieldCurrency); overload;
    procedure   Inc(const AValue: integer); overload;
  published
    property    AsCurrencyString: string read GetAsCurrencyString write SetAsCurrencyString;
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
    function    Equals(const ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(const AAssignFrom: TtiFieldAbs); override;
  published
    property    AsBoolean: Boolean read FValue Write SetAsBoolean;
  end;

  {: Concrete persistent TDateTime field, but for Date portion only}
  TtiFieldDate = class(TtiFieldAbs)
  private
    FValue: TDateTime;
    function    GetDays: Word;
    function    GetMonths: Word;
    function    GetYears: Word;
  protected
    procedure   Clear; override;
    procedure   SetAsDateTime(const AValue: TDateTime); virtual;
    procedure   SetAsString(const AValue: string);  override;
    function    GetAsString: string;               override;
    property    Value: TDateTime read FValue write SetAsDateTime;
  public
    function    Equals(const ACompareWith: TtiFieldAbs): Boolean; override;
    procedure   Assign(const AAssignFrom: TtiFieldAbs); override;
    property    Days: Word read GetDays;
    property    Months: Word read GetMonths;
    property    Years: Word read GetYears;
  published
    property    AsDateTime: TDateTime read FValue Write SetAsDateTime;
  end;

  {: Concrete persistent TDateTime field}
  TtiFieldDateTime = class(TtiFieldDate)
  private
    function    GetHours: Word;
    function    GetMinutes: Word;
    function    GetSeconds: Word;
  protected
    procedure   SetAsDateTime(const AValue: TDateTime); override;
    procedure   SetAsString(const AValue: string);  override;
    function    GetAsString: string;               override;
  public
    procedure   Assign(const AAssignFrom: TtiFieldAbs); override;
    property    Hours: Word read GetHours;
    property    Minutes: Word read GetMinutes;
    property    Seconds: Word read GetSeconds;
  end;


{$IFDEF IOS}
  TtiFieldList = class(TObjectlist<TtiFieldAbs>)
{$ELSE}
  TtiFieldList = class(TObjectlist)
{$ENDIF IOS}
  protected
    function  GetItem(AIndex: Integer): TtiFieldAbs;
    procedure SetItem(AIndex: Integer; AObject: TtiFieldAbs);
  public
    constructor Create;
    function Add(AObject: TtiFieldAbs): Integer;
    property Items[Index: Integer]: TtiFieldAbs read GetItem write SetItem; default;
  end;


  ItiNotifyObserversHelper = interface(IInterface)
  ['{43F29C17-5E49-4C62-B68F-522A1C80C17A}']
  end;

  TtiNotifyObserversHelper = class(TInterfacedObject, ItiNotifyObserversHelper)
  private
    FObserved: TtiObject;
  public
    constructor Create(const AObserved: TtiObject; const ATopic: string = '');
    destructor Destroy; override;
  end;

  TtiObjectListCompareEvent = procedure(AItem1, AItem2: TtiObject) of object;
  TtiObjectListCompareWithDifferenceMessageEvent = procedure(const AItem1, AItem2: TtiObject; var ADifferenceMessage: string) of object;
  TtiObjectListFindCompareFunc = function(const AObject: TtiObject; const AValue): integer of object;
  TtiObjectListItemMatchFunc = reference to function(const AItem1, AItem2: TtiObject): Boolean;
  TtiObjectListItemCompareFunc = reference to function(const AItem1, AItem2: TtiObject): Boolean;
  TtiObjectCompareEvent = function(const AItem1, AItem2: TtiObject): integer of object;

  ItiObserverHandlesErrorState = interface(IInterface)
  ['{766D38E6-0366-4317-9F27-D3CB73BA19CA}']
    procedure ProcessErrorState(const ASubject: TtiObject; const AOperation: TNotifyOperation; const AErrors: TtiObjectErrors);
  end;


  TtiObject = class(TtiVisited)
  private
    FOID: TtiOID;
    FObjectState : TPerObjectState;
    FObserverList: TList;
    FUpdateCount: Integer;
    FUpdateTopicList: TStringList;
    function GetObserverList: TList;
    function GetUpdateTopicList: TStringList;
  protected
    FOwner: TtiObject; // Want FOwner here as there are time when we may want
                        // go get access without going through the GetOwner and
                        // SetOwner as these may have been typecase in a way
                        // which causes incompatabilities.
    function    GetDeleted: boolean; virtual;
    procedure   SetDeleted(const AValue: boolean); virtual;
    procedure   SetDirty(const AValue: boolean); virtual;
    procedure   SetObjectState(const AValue: TPerObjectState); virtual;
    function    GetObjectState: TPerObjectState; virtual;
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
    procedure   DoGetFieldBounds(const AFieldName: String; var MinValue, MaxValue: Integer; var HasBounds: Boolean); overload; virtual;
    procedure   DoGetFieldBounds(const AFieldName: String; var MinValue, MaxValue: Extended; var HasBounds: Boolean); overload; virtual;
    procedure   DoGetFieldBounds(const AFieldName: String; var MinValue, MaxValue: TDateTime; var HasBounds: Boolean); overload; virtual;
    {: Read in the primary Key values only from the database for this object.
       You must override ReadPK and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inherited (if you are using automap)).}
    procedure   ReadPK(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    {: Read in the primary Key values only from the database for this object.
       You must override ReadPK and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inherited (if you are using automap)).}
    procedure   ReadPK; overload; virtual;
    {: Read this object, but no owned objects from the database
       You must override ReadThis and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inherited (if you are using automap)).}
    procedure   ReadThis(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    {: Read this object, but no owned objects from the database
       You must override ReadThis and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inherited (if you are using automap)).}
    procedure   ReadThis; overload;  virtual;
    {: Read this object, along with any owned objects from the database
       You must override Read and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inherited (if you are using automap)).}
    procedure   Read(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    {: Read this object, along with any owned objects from the database
       You must override Read and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inherited (if you are using automap)).}
    procedure   Read; overload;  virtual;
    {: Updates the database with the current property values for this object.
       You must override Save and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inherited (if you are using automap)).}
    procedure   Save(const ADBConnectionName: string; APersistenceLayerName: string = ''); overload; virtual;
    {: Updates the database with the current property values for this object.
       You must override Save and implement a call to the visitor manager (if you
       are using hard coded visitors, or call inherited (if you are using automap)).}
    procedure   Save; overload; virtual;
    {: Only needed if performing a observing role: called when an object under
       observation is freed. The observer should stop observing it. It may or may not
       perform a DetachObserver. }
    procedure   StopObserving(ASubject: TtiObject); virtual;
  public
    {: Creates a new instance of the class setting ObjectState property to posEmpty.}
    constructor Create; override;
    {: Creates a new instance of the class and initialises it's OID with next available value
       setting ObjectState property to posCreate. }
    constructor CreateNew(const AOwner: TtiObject; const ADatabaseName: string = ''; const APersistenceLayerName: string = ''); overload; virtual;
    constructor CreateNew(const ADatabaseName: string = ''; const APersistenceLayerName: string = ''); overload; virtual;
    destructor  Destroy; override;
    {: Does this object equal another? }
    function    Equals(const AData : TtiObject): boolean; reintroduce; overload; virtual;
    function    Equals(const AData : TtiObject; var ADifferenceMessage: string): boolean; reintroduce; overload; virtual;
    {: The OID of this object }
   {$IFDEF OID_AS_INT64}
      property    OID        : TtiOID                   read GetOID write SetOID;
   {$ELSE}
      property    OID        : TtiOID                   read GetOID       ;
   {$ENDIF}
    {: The current state of this object}
    property    ObjectState: TPerObjectState read GetObjectState write SetObjectState;
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
    function    Find(const AOIDToFind : TtiOID): TtiObject;  overload; virtual;
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
    {: Is this object unique in the hierarchy?}
    function    IsUnique(const AObject : TtiObject): boolean; overload; virtual;

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
    {: Get the minimum and maximum values for a property. Returns false if there are no bounds.
       For string fields, this is the min and max length. }
    function    GetFieldBounds(const AFieldName: string; out MinValue, MaxValue: Integer): Boolean; overload;
    function    GetFieldBounds(const AFieldName: string; out MinValue, MaxValue: Extended): Boolean; overload;
    function    GetFieldBounds(const AFieldName: string; out MinValue, MaxValue: TDateTime): Boolean; overload;

    procedure   AssignFieldList(var AFieldList: TtiFieldList);
    {: ForceAsCreate will get a new OID, and set ObjectState := posCreate}
    procedure   ForceAsCreate(const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
    {: Display the object tree as a string for debugging (will show all published properties.)}
    function    AsDebugString(const AValuesToShow: TtiObjectAsDebugStringValuesToShow): string; overload; virtual;
    function    AsDebugString: string; overload; virtual;
    {:Assign the published property names to a TStringList}
//    procedure   GetPropNames(AList: TStringList; APropFilter: TtiPropTypes = []);
    {: Return the propery count filter by APropFilter }
    function    PropCount(APropFilter: TTypeKinds = ctkSimple): integer;

    {: Answer a CSV formatted string of object properties}
    function    PropToCSV(const AFieldDelim: string; AColsSelected: TStringList): string;
    { Observer pattern implementation below }
    {: Attach a new observer }
    procedure   AttachObserver(AObserver: TtiObject); virtual;
    {: Detach a existing observer }
    procedure   DetachObserver(AObserver: TtiObject); virtual;
    {: Start a update process. This will allow us to update multiple things,
      before we notify the observers of the updates. Topic allows for
      identification of specific changes. }
    procedure   BeginUpdate(const ATopic: string = '');
    {: End an update process }
    procedure   EndUpdate;
    {: BeginUpdate when called, EndUpdate when out of scope. Topic allows for
      identification of specific changes. }
    function    NotifyObserversHelper(const ATopic: string = ''): ItiNotifyObserversHelper;
    {: Only needed if performing a observing role }
    procedure   Update(ASubject: TtiObject); overload; virtual;
    {: Only needed if performing a observing role where other events than changed need to be observed }
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation); overload; virtual;
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation; AData: TtiObject); overload; virtual;
    {: Notify all the attached observers about a change }
    procedure   NotifyObservers; overload; virtual;
    {: Notify all the attached observers about a change for a specific topic }
    procedure   NotifyObservers(const ATopic: string); overload; virtual;
    {: Notify all the attached observers about a change operation}
    procedure   NotifyObservers(ASubject : TtiObject; AOperation : TNotifyOperation); overload; virtual;
    {: Notify all the attached observers about a change operation for a specific topic }
    procedure   NotifyObservers(ASubject : TtiObject; AOperation : TNotifyOperation; const ATopic: string); overload; virtual;
    procedure   NotifyObservers(ASubject: TtiObject; AOperation: TNotifyOperation; AData: TtiObject; const ATopic: string); overload; virtual;
    {: Used to get access to the internal observer list. This has been surfaced
       so that the MGM List Views can atttach/detach observers to the selected
       object. Not a great way of doing it - we need a different design. }
    property    ObserverList: TList read GetObserverList write FObserverList;
    {: Update notifications can be for specific topics (such as one or more
       specific properties changed. }
    property    UpdateTopicList: TStringList read GetUpdateTopicList;
  end;

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
{$IFDEF IOS}
    FList : TObjectList<TtiObject>;
{$ELSE}
    FList : TObjectList;
{$ENDIF IOS}
    FItemOwner: TtiObject;
    FbAutoSetItemOwner: boolean;
{$IFDEF IOS}
    function    GetList: TList<TtiObject>;
{$ELSE}
    function    GetList: TList;
{$ENDIF IOS}
    function    GetCountNotDeleted: integer;
    function    GetOwnsObjects: boolean;
    procedure   SetOwnsObjects(const AValue: boolean);
    function    DoCompareByProps(AItem1  : Pointer; AItem2  : Pointer;
                const ASortProps : array of string; AAscendingOrder : Boolean = True): integer;
    procedure   QuickSortByProps(SortList: {$IFDEF DELPHIXE2ORABOVE}TPointerList{$ELSE}PPointerList{$ENDIF}; L, R: Integer;
                const ASortProps : array of string; AAscendingOrder : Boolean = True);
  protected
    function    GetCount: integer; virtual;
    function    GetCapacity: integer;
    procedure   SetCapacity(const AValue: integer);
    function    GetItems(i: integer): TtiObject; virtual;
    procedure   SetItems(i: integer; const AValue: TtiObject); virtual;
    procedure   SetItemOwner(const AValue: TtiObject); virtual;
    function    CreateAssignItem(const ASource: TtiObject): TtiObject; virtual;
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
    {: Assign the captions & objects from all objects to the TStrings AList}
    procedure   AssignCaptionsAndObjects(const AStrings: TStrings);
    {: The number of items in the list.}
    property    Count : integer read GetCount;
    {: The capacity (number of items with allocated memory) of the list.}
    property    Capacity : integer read GetCapacity write SetCapacity;
    {: The number of items in the list that are not marked as deleted.}
    property    CountNotDeleted : integer read GetCountNotDeleted;
    property    Items[i:integer]: TtiObject read GetItems write SetItems; default;

    {: Does the list own the objects it contains? i.e. Will the objects be
      freed when the list is cleared/destroyed? The default is True. }
    property    OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects;
    property    ItemOwner     : TtiObject read FItemOwner    write SetItemOwner;
    property    AutoSetItemOwner : boolean read FbAutoSetItemOwner write FbAutoSetItemOwner;

    {: Finds the object in the list whose OID value matches.}
    function    Find(AOIDToFindAsString : string): TtiObject;  override;
    {: Finds the object in the list whose OID value matches.}
    function    Find(const AOIDToFind : TtiOID): TtiObject; override;
    {: Finds the object in the list whose OID value matches. Faster search if sorted by OID. }
    function    Find(AOIDToFind : TtiOID; ASortType: TtiPerObjListSortType): TtiObject; overload;
    {: Finds the first object in the list that matches the given object according to the called match function. }
    function    Find(const AObject: TtiObject; const AItemMatchFunc: TtiObjectListItemMatchFunc): TtiObject; overload;
    {: Finds the object in the list whose OID value matches. Will search the list, and if not found, will search all owned objects }
    function    FindInHierarchy(AOIDToFind : TtiOID): TtiObject; overload;
    {: Finds the object in the sorted list given a comparison function. If not found the index is the insertion point.
       This provides a convenient way to search for an existing object in a sorted list and if not found insert it at
       the correct position to keep the list in sorted order. }
    function    FindSortedUntyped(const AFindCompare: TtiObjectListFindCompareFunc; const AValue; out AObject: TtiObject; out AIndex: integer): boolean;
    {: Performs the method AMethod on every object in the list.}
    procedure   ForEach(const AMethod: TtiObjectEvent; const AIncludeDeleted: boolean = false); overload; virtual;
    {: Performs the method AMethod on every object in the list.}
    procedure   ForEach(const AMethod: TtiObjectEventRegular; const AIncludeDeleted: boolean = false); overload; virtual;
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
    procedure   CompareWith(const AList: TtiObjectList;
                            const AInBothAndEquals: TtiObjectListCompareEvent;
                            const AInBothAndNotEquals: TtiObjectListCompareWithDifferenceMessageEvent;
                            const AIn1Only: TtiObjectListCompareEvent;
                            const AIn2Only: TtiObjectListCompareEvent;
                            const AItemMatchFunc: TtiObjectListItemMatchFunc = nil;
                            const AItemCompareFunc: TtiObjectListItemCompareFunc = nil); virtual;

    {: Compare Self with AList. Add each object to the appropriate list depending on the differences}
//    procedure   CompareWith(AList: TtiObjectList;
//                            AInBothAndEquals: TtiObjectList;
//                            AInBothAndNotEquals: TtiObjectList;
//                            AIn1Only: TtiObjectList;
//                            AIn2Only: TtiObjectList); overload;
    {: Allows the use of the for-in loop in Delphi 2005+}
    function GetEnumerator: TtiEnumerator;
    function IsUnique(const AObject : TtiObject; const ACompareEvent: TtiObjectCompareEvent): boolean; overload;

  published
    // This must be published so it can be used by the tiPerAware controls.
{$IFDEF IOS}
    property    List : TList<TtiObject> read GetList;
{$ELSE}
    property    List : TList read GetList;
{$ENDIF IOS}
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
    function    Equals(const AData : TtiObject): boolean; override;
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
{$IFDEF IOS}
    FList : TObjectList<TtiBaseObject>;
{$ELSE}
    FList : TObjectList;
{$ENDIF IOS}
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


{$IFDEF IOS}
  AnsiString = Array of Byte;
{$ENDIF IOS}

{$IFNDEF IOS}
  TPerStream = class(TtiObject)
  private
    FStream : TMemoryStream;
    function  GetSize    : integer;
    function  GetAsString : AnsiString;
    procedure SetAsString(const AValue: AnsiString);
    procedure SetSize(const AValue: integer);
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Stream : TMemoryStream read FStream write FStream;
    property    Size  : integer       read GetSize write SetSize;

    procedure   SaveToFile(const AFileName : string);
    procedure   LoadFromFile(const AFileName : string);
    procedure   Clear;
    property    AsString : AnsiString read GetAsString write SetAsString;
  end;
{$ENDIF IOS}


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
    function    Add(const AObject: TtiObject): integer; override;
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

  { TtiObserverProxy }
  {: Event based notification for observation of a single object }

  TtiObserverProxy = class(TtiObject)
  private
    FSubject: TtiObject;
    FOnUpdate: TtiObjectUpdateEvent;
    procedure   SetSubject(const AValue: TtiObject);
  protected
    procedure   StopObserving(ASubject: TtiObject); override;
  public
    constructor Create; overload; override;
    constructor Create(const ASubject: TtiObject; const AOnUpdate: TtiObjectUpdateEvent); reintroduce; overload; virtual;
    destructor  Destroy; override;
    procedure   Update(ASubject: TtiObject; AOperation: TNotifyOperation; AChild: TtiObject=nil); overload; override;
    property    Subject: TtiObject read FSubject write SetSubject;
    property    OnUpdate: TtiObjectUpdateEvent read FOnUpdate write FOnUpdate;
  end;

  { TtiObserversProxy }
  {: Event based notification for observation of multiple objects }

  TtiObserversProxy = class(TtiObject)
  private
    FSubjects: TtiObjectList;
    FOnUpdate: TtiObjectUpdateEvent;
  protected
    procedure StopObserving(ASubject: TtiObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure AttachTo(ASubject: TtiObject);
    procedure DetachFrom(ASubject: TtiObject); overload;
    procedure DetachFrom(ASubjectClass: TtiObjectClass); overload;
    procedure DetachAll;
    procedure Update(ASubject: TtiObject; AOperation: TNotifyOperation); override;
    property OnUpdate: TtiObjectUpdateEvent read FOnUpdate write FOnUpdate;
  end;

const
  cgNullDBInteger            = -1 ;
  cgNullDBString             = '' ;
  cgNullDBDate               = 0.0;

function ObjectStateToString(AObjectState: TPerObjectState): string;

function NotifyOperationToString(AOperation: TNotifyOperation): string;

{:Copy a TtiObjectList of TtiObject(s) data to a TStream using CSV format}
procedure tiListToStream(AStream: TStream;
                         AList: TtiObjectList;
                         AFieldDelim: string;
                         ARowDelim: string;
                         AColsSelected: TStringList;
                         AColsHeader: TStringList = nil); overload;

procedure tiListToStream(AStream : TStream;
                         AList : TtiObjectList); overload;

// Copy a TList of TtiBaseObject's to a CSV file (Prompt the user for the file name)
procedure tiListToCSV(AList: TtiObjectList;
                       const AFileName: string;
                       AColsSelected: TStringList;
                       AColsHeader: TStringList = nil); overload;

procedure tiListToCSV(AList: TtiObjectList;
                       const AFileName: string); overload;

implementation
uses
  // tiOPF
   tiConstants
  ,tiOPFManager
  ,tiUtils
  // Delphi
  ,SysUtils
  ,Math
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}
  {$IFNDEF FPC}
  ,Types  // gets rid of Delphi H2443 compiler hint
  {$ENDIF}
 ;

function ObjectStateToString(AObjectState : TPerObjectState): string;
begin
  result := GetEnumName(TypeInfo(TPerObjectState),
                         Ord(AObjectState));
end;

function NotifyOperationToString(AOperation: TNotifyOperation): string;
begin
  result := GetEnumName(TypeInfo(TNotifyOperation),
                         Ord(AOperation));
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
    tiListToStream(AStream, AList, ',', tiLineEnd, lFields);
  finally
    lFields.Free;
  end;
end;

procedure tiListToCSV(AList: TtiObjectList;
                       const AFileName: string;
                       AColsSelected: TStringList;
                       AColsHeader: TStringList = nil);
var
  lStream   : TFileStream;
begin
  Assert(AList.TestValid, CTIErrorInvalidObject);
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(AColsSelected<>nil, 'AColsSelected not assigned');

  lStream := TFileStream.Create(AFileName, fmCreate);
  try
    tiListToStream(lStream, AList, ',', tiLineEnd, AColsSelected, AColsHeader);
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
      tiListToStream(lStream, AList, ',', tiLineEnd, lFields);
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
                         AColsSelected : TStringList;
                         AColsHeader: TStringList = nil);
var
  i, j, LIntValue : integer;
  LValue   : string;
  LFieldName : string;
  LData     : TtiBaseObject;
  LLine     : string;
  LPropType: TTypeKind;
  LColsHeader: TStringList;
begin
  // Write column headings
  if Assigned(AColsHeader) then
    lColsHeader := AColsHeader
  else
    lColsHeader := AColsSelected;
  for i := 0 to LColsHeader.Count - 1 do begin
    tiAppendStringToStream(LColsHeader.Strings[i], AStream);
    if i < LColsHeader.Count - 1 then
      tiAppendStringToStream(AFieldDelim, AStream)
    else
      tiAppendStringToStream(ARowDelim, AStream);
  end;

  // Write the data
  for i := 0 to AList.Count - 1 do
  begin
    LData := (TObject(AList.Items[i]) as TtiBaseObject);
    LLine := '';
    for j := 0 to AColsSelected.Count - 1 do
    begin
      if LLine <> '' then
        LLine := LLine + AFieldDelim;
      LFieldName := AColsSelected.Strings[j];
{$IFDEF IOS}
      if GetPropInfo(LData,lFieldName).NameFld.ToString = 'TDateTime' then
{$ELSE}
      if GetPropInfo(LData,lFieldName)^.PropType^.Name = 'TDateTime' then
{$ENDIF IOS}
        LValue := tiDateTimeToStr(GetPropValue(LData, LFieldName))
      else
      begin
        LPropType := TypInfo.PropType(LData, lFieldName);
        case lPropType of
          tkChar       : LValue := IntToStr(GetOrdProp(LData, LFieldName));
          tkWChar      : LValue := IntToStr(GetOrdProp(LData, LFieldName));
          tkString     : LValue := GetStrProp(LData, LFieldName);
          tkLString    : LValue := GetStrProp(LData, LFieldName);
          tkWString    : LValue := GetStrProp(LData, LFieldName);
          {$IFDEF FPC}
          tkAString    : LValue := GetStrProp(LData, LFieldName);
          {$ENDIF}
          tkInteger    : begin  // Two step extract required to avoid strange errors with Int props that have getters
                           LIntValue  := GetInt64Prop(LData, LFieldName);
                           LValue := IntToStr(LIntValue);
                         end;
          tkInt64      : LValue := IntToStr(GetInt64Prop(LData, LFieldName));
          tkFloat      : LValue := FloatToStr(GetFloatProp(LData, LFieldName));
          tkEnumeration: LValue := IntToStr(GetOrdProp(LData, LFieldName));
          {$IFDEF FPC}
          tkBool       : LValue := IntToStr(GetInt64Prop(LData, LFieldName));
          {$ENDIF}
          {$IFDEF UNICODE}
          tkUString    : LValue := GetStrProp(LData, LFieldName);
          {$ENDIF}
        end;
      end;
      LLine := LLine + LValue;
    end;
    if i <> 0 then
      LLine := ARowDelim + LLine;
    tiAppendStringToStream(LLine, AStream)
  end;
end;

function GetFieldAbsProp(Instance: TTiObject; APropName: string): TTiFieldAbs;
var
  tk: TtiTypeKind;
begin
  Result := Nil;
  tk := tiGetSimplePropType(Instance, APropName);
  if (tk = tiTKBinary)  then
    Result := TTiFieldAbs(GetObjectProp(Instance, APropName, TtiFieldAbs));
end;


{ TtiNotifyObserversHelper }

constructor TtiNotifyObserversHelper.Create(const AObserved: TtiObject;
  const ATopic: string);
begin
  Assert(AObserved.TestValid, CTIErrorInvalidObject);
  inherited Create;
  FObserved := AObserved;
  FObserved.BeginUpdate(ATopic);
end;

destructor TtiNotifyObserversHelper.Destroy;
begin
  FObserved.EndUpdate;
  inherited Destroy;
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

function TtiObject.AsDebugString: string;
begin
  result:= AsDebugString(CTIAsDebugStringDataAll);
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

  AssignClassProps(    ASource);
  AssignPublicProps(   ASource);
  AssignPublishedProps(ASource);

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
      tkWString    : SetStrProp(Self, APropName, GetStrProp(ASource, APropName));
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
      {$IFDEF UNICODE}
      tkUString : SetStrProp(Self, APropName, GetStrProp(ASource, APropName));
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
                         OID:= OIDGenerator.NextOID;
                     {$ELSE}
                       if OID.IsNull then
                         OIDGenerator.AssignNextOID(OID);
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
{$IFDEF IOS}
  FList := TObjectList<TtiObject>.Create;
{$ELSE}
  FList := TObjectList.Create;
{$ENDIF IOS}
  FItemOwner := Self;
  FbAutoSetItemOwner := true;
end;


destructor TtiObjectList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TtiObjectList.Add(const AObject : TtiObject): integer;
begin
  if FbAutoSetItemOwner then
    AObject.Owner := FItemOwner;
  result := FList.Add(AObject);
  if FUpdateCount <= 0 then
    NotifyObservers(self, noAddItem, AObject, '');
end;

procedure TtiObjectList.Clear;
begin
  BeginUpdate;
  Try
    FList.Clear;
    FObjectState := posEmpty;
  Finally
    EndUpdate;
  end;
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
  if FUpdateCount <= 0 then
    NotifyObservers(self, noDeleteItem, TtiObject(Items[i]), '');
  if AutoSetItemOwner then
    TtiObject(Items[i]).Owner := nil;
  FList.Delete(i);
end;


procedure TtiObject.SetObjectState(const AValue: TPerObjectState);
begin
  if FObjectState = AValue then exit;
  FObjectState := AValue;
  NotifyObserversHelper;
end;

procedure TtiObjectList.Empty;
var
  i: integer;
begin
  BeginUpdate;
  try
    for i := Count - 1 downto 0 do
      FList.Extract(FList.Items[i]);
  finally
    EndUpdate;
  end;
end;

function TtiObjectList.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TtiObjectList.GetCount: integer;
begin
  result := FList.Count;
end;

function TtiObjectList.GetItems(i: integer): TtiObject;
begin
  result := TtiObject(FList.Items[ i ]);
end;

{$IFDEF IOS}
function TtiObjectList.GetList: TList<TtiObject>;
begin
  result := FList;
end;
{$ELSE}
function TtiObjectList.GetList: TList;
begin
  result := FList;
end;
{$ENDIF IOS}

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

{$IFNDEF IOS}
constructor TPerStream.Create;
begin
  inherited Create;
  FStream := nil;
  Clear;
end;

destructor TPerStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
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

procedure TPerStream.SetSize(const AValue: integer);
begin
  FStream.Size := AValue;
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

function TPerStream.GetAsString: AnsiString;
var
  ls : AnsiString;
begin
  FStream.Seek(0,soFromBeginning);
  SetString(ls, nil, FStream.Size);
  Stream.Read(Pointer(ls)^, FStream.Size);
  Result := ls;
end;

procedure TPerStream.SetAsString(const AValue: AnsiString);
var
  lpcText : PAnsiChar;
begin
  FStream.Clear;
  lpcText := PAnsiChar(AValue);
  FStream.WriteBuffer(lpcText^, length(lpcText));
end;
{$ENDIF IOS}

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
  if FUpdateCount <= 0 then
    NotifyObservers(self, noDeleteItem, AObject, '');
  if AutoSetItemOwner then
    AObject.Owner := nil;
  result := FList.Remove(AObject);
end;

{: Call Extract to remove an object from the list without freeing the object
  itself. After an object is removed, all the objects that follow it are moved 
  up in index position and Count is decremented.}
procedure TtiObjectList.Extract(const AObject: TtiObject);
begin
  if FUpdateCount <= 0 then
    NotifyObservers(self, noDeleteItem, AObject, '');
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
  if FUpdateCount <= 0 then
    NotifyObservers(self, noAddItem, AObject, '');
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


function TtiObjectList.IsUnique(const AObject: TtiObject;
  const ACompareEvent: TtiObjectCompareEvent): boolean;
var
  i : integer;
  LItem: TtiObject;
begin
  Assert(Assigned(AObject), 'AObject not assigned');
  Assert(Assigned(ACompareEvent), 'ACompareEvent not assigned');
  for i := 0 to Count-1 do
  begin
    LItem:= Items[i];
    if (ACompareEvent(AObject, LItem) = 0) and
       (not LItem.Deleted) and
       (LItem <> AObject) then
      Exit(false);
  end;
  Exit(true);
end;

{ TVisPerObjFindByOID }
 
function TVisPerObjFindByOID.AcceptVisitor: boolean;
begin
  result := (Visited is TtiObject) and
            (FFound = nil);
end;

constructor TVisPerObjFindByOID.Create;
begin
  inherited Create;
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

function TtiObject.Find(const AOIDToFind : TtiOID): TtiObject;
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
  begin
    if FoundList = nil then
      FFound := TtiObject(Visited)
    else
      FoundList.Add(Visited);
  end;
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

procedure TtiObjectList.ForEach(const AMethod: TtiObjectEvent;
  const AIncludeDeleted: boolean=false);
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    if not Items[i].Deleted or AIncludeDeleted then
      AMethod(Items[i]);
end;

procedure TtiObjectList.ForEach(const AMethod: TtiObjectEventRegular;
  const AIncludeDeleted: boolean);
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
  inherited Create;
end;

destructor TPerStringStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
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
  FStream.WriteString(AValue + tiLineEnd);
end;

procedure TtiObjectList.SetCapacity(const AValue: integer);
begin
  FList.Capacity := AValue;
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

procedure TtiObjectList.AssignCaptionsAndObjects(const AStrings: TStrings);
var
  i: Integer;
begin
  AStrings.Clear;
  for i := 0 to count - 1 do
    AStrings.AddObject(Items[i].Caption, Items[i]);
end;

// Uses the parameterless constructor
function TtiObjectList.CreateAssignItem(const ASource: TtiObject): TtiObject;
var
  LClass: TtiClass;
begin
  LClass := TtiClass(ASource.ClassType);
  Result := TtiObject(LClass.Create);
end;

procedure TtiObjectList.AssignClassProps(ASource: TtiObject);
var
  i: integer;
  LSource: TtiObject;
  LTarget: TtiObject;
begin
  Assert(ASource is TtiObjectList, 'ASource not a TtiObjectList');

  if OwnsObjects then
  begin
    for i := 0 to TtiObjectList(ASource).Count - 1 do
    begin
      LSource := TtiObjectList(ASource).Items[i];
      LTarget := CreateAssignItem(LSource);
      Add(LTarget);
      LTarget.Assign(LSource);
      if AutoSetItemOwner then
        LTarget.Owner := ItemOwner;
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
          'Owner not a TtiObjectList, it''s a ' + Owner.ClassName);
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
    AObject: TObject;
  begin
    lSearch := PropValue;

    // If the property is a TtiFieldAbs then compare the value with the field
    // objects AsString, else compare the value with the property itself
    AObject := Items[Idx];
    lPropInfo := tiGetPropInfo(Items[Idx].ClassType, PropName, @AObject);
    if Assigned(lPropInfo) and (tiGetTypeInfo(lPropInfo).Kind in [tkClass,tkEnumeration]) then
    begin
      if lPropInfo.PropType^.Kind=tkClass then
      begin
        tiFieldAbs := GetObjectProp(AObject, PropName, TtiFieldAbs) as TtiFieldAbs;
        if Assigned(tiFieldAbs) then
          lItem := tiFieldAbs.AsString
        else
{$IFDEF IOS}
          lItem := TypInfo.GetPropValue(AObject, LPropInfo.NameFld.ToString);
{$ELSE}
          lItem := TypInfo.GetPropValue(AObject, string(LPropInfo^.Name));
{$ENDIF IOS}
      end
      else // tkEnumeration
      begin
        // If the property is an enumeration and the search is on a numeric value,
        // compare the ordinary values of the enumeration, instead of it' s name
{$IFDEF IOS}
        if VarIsNumeric(lSearch) then
          lItem := TypInfo.GetOrdProp(AObject, LPropInfo.NameFld.ToString)
        else
          lItem := TypInfo.GetPropValue(AObject, LPropInfo.NameFld.ToString);
{$ELSE}
        if VarIsNumeric(lSearch) then
          lItem := TypInfo.GetOrdProp(AObject, string(LPropInfo^.Name))
        else
          lItem := TypInfo.GetPropValue(AObject, string(LPropInfo^.Name));
{$ENDIF IOS}
      end;
    end
    else
      lItem := tiGetProperty(Items[Idx], PropName);

    // Just to be sure that I'm comparing the SAME kind of values,
    // plus Boolean types need some extra help under FPC
    if VarIsType(PropValue, varBoolean) then
{$IFDEF IOS}
      lItem := Boolean(TypInfo.GetOrdProp(AObject, LPropInfo.NameFld.ToString))
{$ELSE}
      lItem := Boolean(TypInfo.GetOrdProp(AObject, string(LPropInfo^.Name)))
{$ENDIF IOS}
    else
    begin
      lVarType  := VarType(lItem);
      lSearch   := VarAsType(lSearch, lVarType);
      lItem     := VarAsType(lItem, lVarType);
    end;

    // PWH Changed for D5 compat
    if (tiIsVariantOfType(lSearch, varOleStr)
       or tiIsVariantOfType(lSearch, varString)
       {$IFDEF UNICODE} or tiIsVariantOfType(lSearch, varUString) {$ENDIF}
        ) and not ACaseSensitive then
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

constructor TtiObject.CreateNew(const AOwner: TtiObject; const ADatabaseName: string = ''; const APersistenceLayerName: string = '');
begin
  Create;
  if AOwner <> nil then
    Owner := AOwner;
  ObjectState := posCreate;
  {$IFDEF OID_AS_INT64}
    OID := OIDGenerator.NextOID;
  {$ELSE}
    OIDGenerator.AssignNextOID(OID, ADatabaseName, APersistenceLayerName);
  {$ENDIF}
end;

constructor TtiObject.CreateNew(const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
begin
  CreateNew(nil, ADatabaseName, APersistenceLayerName);
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
  BeginUpdate;
{$IFDEF IOS}
  FList.Sort(TComparer<TtiObject>.Construct(
   function (const L, R: TtiObject): integer
   begin
     result := _DoSortByOID(L, R);
   end
   ));
{$ELSE}
  FList.Sort(_DoSortByOID);
{$ENDIF IOS}
  NotifyObservers(self, noReSort);
  EndUpdate;
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

      if VarType(lValue1)=varString then
      begin
        If (AAscendingOrder)
        then Result:=AnsiCompareText(lValue1,lValue2)
        else Result:=AnsiCompareText(lValue2,lValue1)
      end
      else begin
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
      end;

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
  SortList: {$IFDEF DELPHIXE2ORABOVE}TPointerList{$ELSE}PPointerList{$ENDIF};
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
    P := SortList{$IFNDEF DELPHIXE2ORABOVE}^{$ENDIF}[(L + R) shr 1];
    repeat
      while DoCompareByProps(SortList{$IFNDEF DELPHIXE2ORABOVE}^{$ENDIF}[I], P, ASortProps, AAscendingOrder) < 0 do
        Inc(I);
      while DoCompareByProps(SortList{$IFNDEF DELPHIXE2ORABOVE}^{$ENDIF}[J], P, ASortProps, AAscendingOrder) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList{$IFNDEF DELPHIXE2ORABOVE}^{$ENDIF}[I];
        SortList{$IFNDEF DELPHIXE2ORABOVE}^{$ENDIF}[I]:= SortList{$IFNDEF DELPHIXE2ORABOVE}^{$ENDIF}[J];
        SortList{$IFNDEF DELPHIXE2ORABOVE}^{$ENDIF}[J]:= T;
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
{$IFDEF IOS}
  Assert(False, 'TtiObjectList.SortByProps not implemented');
{$ELSE}
  BeginUpdate;
  if (FList <> nil) and (Count > 0) then
    QuickSortByProps(FList.List, 0, Count - 1, ASortProps, AAscendingOrder);
  NotifyObservers(self, noReSort);
  EndUpdate;
{$ENDIF IOS}
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

function TPerObjThreadList.Add(const AObject: TtiObject): integer;
begin
  Lock;
  try
    result := inherited Add(AObject);
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
  inherited Create;
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
  inherited Destroy;
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
  inherited Create;
{$IFDEF IOS}
  FList := TObjectList<TtiBaseObject>.Create;
{$ELSE}
  FList := TObjectList.Create;
{$ENDIF IOS}
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

destructor TPerObjFactory.Destroy;
begin
  FList.Free;
  inherited Destroy;
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
  Assert(FindByClassName(AClassName) = nil,
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

function TtiObjectList.Find(const AOIDToFind: TtiOID): TtiObject;
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

function TtiObjectList.Find(const AObject: TtiObject;
  const AItemMatchFunc: TtiObjectListItemMatchFunc): TtiObject;
var
  i: Integer;
begin
  Assert(Assigned(AItemMatchFunc), 'AItemMatchFunc must be assigned');
  for i := 0 to Count - 1 do
    if AItemMatchFunc(AObject, Items[i]) then
      Exit(Items[i]); //==>
  result := nil;
end;

function TtiObjectList.FindInHierarchy(AOIDToFind: TtiOID): TtiObject;
begin
  Result := Find(AOIDToFind);
  if Result = nil then
    Result := inherited Find(AOIDToFind);
end;

function TtiObjectList.FindSortedUntyped(
  const AFindCompare: TtiObjectListFindCompareFunc; const AValue;
  out AObject: TtiObject; out AIndex: integer): boolean;
var
  LLow: Integer;
  LHigh: Integer;
  LMid: Integer;
  LCompareResult: Integer;
begin
  Assert(@AFindCompare <> nil, 'FindCompare function is required');

  result := false;
  AObject := nil;
  AIndex := -1;

  // Binary search
  LLow := 0;
  LHigh := Count - 1;
  while LLow <= LHigh do
  begin
    LMid := ((LHigh + LLow) div 2);
    LCompareResult := AFindCompare(Items[LMid], AValue);
    if LCompareResult > 0 then
      LHigh := LMid - 1
    else if LCompareResult < 0 then
      LLow := LMid + 1
    else begin
      result := true;
      AObject := Items[LMid];
      AIndex := LMid;
      Exit; //==>
    end;
  end;

  AIndex := LLow; // Insertion point
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
var
  i : integer;
begin
  // Check for uniqueness before adding
  for i := 0 to Count - 1 do
    if AObject.Equals(Items[i]) then
    begin
      // This list owns its children, so free orphaned child.
      AObject.Free;
      Exit(-1); //==>
    end;
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
      Result := Result + tiLineEnd;
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

function TtiObjectError.Equals(const AData: TtiObject): boolean;
var
  LError: TtiObjectError;
begin
  Assert(AData.TestValid(TtiObjectError), CTIErrorInvalidObject);
  LError := AData as TtiObjectError;
  Result := (LError.ErrorCode = ErrorCode) and
    SameStr(LError.ErrorMessage, ErrorMessage) and
    SameStr(LError.ErrorProperty, ErrorProperty);
end;

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

function TtiObject.GetFieldBounds(const AFieldName: string; out MinValue,
  MaxValue: Integer): Boolean;
begin
  Result := False;
  DoGetFieldBounds(AFieldName, MinValue, MaxValue, Result);
end;

function TtiObject.GetFieldBounds(const AFieldName: string; out MinValue,
  MaxValue: Extended): Boolean;
begin
  Result := False;
  DoGetFieldBounds(AFieldName, MinValue, MaxValue, Result);
end;

function TtiObject.GetFieldBounds(const AFieldName: string; out MinValue,
  MaxValue: TDateTime): Boolean;
begin
  Result := False;
  DoGetFieldBounds(AFieldName, MinValue, MaxValue, Result);
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
  NotifyObservers(Self,noFree);
  FreeAndNil(FObserverList);
  FreeAndNil(FUpdateTopicList);
  inherited Destroy;
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
begin
  // ToDo: OID is a special case. We should publish OID so it can be accessed
  //       without special code, but that will have side effects.
  //       (We will also have to modify tiRTTI to handle object properties.)
  if SameText(APropName, 'OID') then
  {$IFDEF OID_AS_INT64}
    result := PtrInt(OID)
  {$ELSE}
    result := OID.AsVariant
  {$ENDIF}
  else
    result:= tiGetProperty(Self, APropName);
end;

procedure TtiObject.SetPropValue(const APropName: string; const APropValue: Variant);
begin
  // ToDo: OID is a special case. We should publish OID so it can be accessed
  //       without special code, but that will have side effects.
  //       (We will also have to modify tiRTTI to handle object properties.)
  if SameText(APropName, 'OID') then
    {$IFDEF OID_AS_INT64}
      OID := Integer(APropValue)
    {$ELSE}
      OID.AsVariant := APropValue
    {$ENDIF}
  else
    tiSetProperty(Self, APropName, APropValue);
end;

procedure TtiObject.DoGetFieldBounds(const AFieldName: String; var MinValue,
    MaxValue: Integer; var HasBounds: Boolean);
var
  F: TTiFieldAbs;
  FS: TtiFieldString;
  FI: TtiFieldInteger;
  I: integer;
begin
  HasBounds := False;
  F := GetFieldAbsProp(Self, AFieldName);
  if Assigned(F) then
    if F is TtiFieldString then
    begin
      FS := F as TtiFieldString;
      MinValue := Ord(FS.NullValidation);
      MaxValue := FS.MaxLength;
      HasBounds := (MinValue>0) or (MaxValue>0);
    end
    else if F is TtiFieldInteger then
    begin
      FI := F as TtiFieldInteger;
      HasBounds := (FI.MaxDigits>0);
      if HasBounds then
      begin
        MaxValue := 10;
        For I:=2 to FI.MaxDigits do
          MaxValue := MaxValue*10;
        Dec(MaxValue);
        MinValue := -MaxValue;
      end;
    end;
end;

procedure TtiObject.DoGetFieldBounds(const AFieldName: String; var MinValue,
    MaxValue: Extended; var HasBounds: Boolean);
begin
  HasBounds := False;
end;

procedure TtiObject.DoGetFieldBounds(const AFieldName: String; var MinValue,
    MaxValue: TDateTime; var HasBounds: Boolean);
begin
  HasBounds := False;
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
  inherited Create;
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

procedure TtiFieldString.Assign(const AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldString), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsString:= AAssignFrom.AsString;
end;

procedure TtiFieldString.Clear;
begin
  inherited Clear;
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

function TtiFieldString.Equals(const ACompareWith: TtiFieldAbs): Boolean;
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

procedure TtiFieldInteger.Assign(const AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldInteger), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsInteger:= (AAssignFrom as TtiFieldInteger).AsInteger;
end;

procedure TtiFieldInteger.Clear;
begin
  inherited Clear;
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

function TtiFieldInteger.Equals(const ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldAbs), CTIErrorInvalidObject);
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (AsInteger = (ACompareWith as TtiFieldInteger).AsInteger);
end;

{ TtiFieldFloat }

procedure TtiFieldFloat.Assign(const AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldFloat), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsFloat:= (AAssignFrom as TtiFieldFloat).AsFloat;
end;

procedure TtiFieldFloat.Clear;
begin
  inherited Clear;
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

function TtiFieldFloat.Equals(const ACompareWith: TtiFieldAbs): Boolean;
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

{ TtiFieldCurrency }

procedure TtiFieldCurrency.Assign(const AAssignFrom: TtiFieldAbs);
begin
  if AAssignFrom.IsNull then
    IsNull:= True
  else
  begin
    IsNull:= False;
    FValue:= (AAssignFrom as TtiFieldCurrency).FValue;
  end;
end;

procedure TtiFieldCurrency.Clear;
begin
  inherited Clear;
  FValue := 0;
end;

constructor TtiFieldCurrency.Create(const AOwner: TtiObject);
begin
  Create(AOwner, nvAllowNull);
end;

constructor TtiFieldCurrency.Create(const AOwner: TtiObject;
  const ANullValidation: TtiNullValidation);
begin
  inherited Create(AOwner, ANullValidation);
  FValue:= 0;
end;

function TtiFieldCurrency.Equals(const ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldCurrency), CTIErrorInvalidObject);
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (AsInteger = (ACompareWith as TtiFieldCurrency).AsInteger);
end;

function TtiFieldCurrency.GetAsCurrencyString: string;
begin
  if not IsNull then
    Result:= tiFloatToCurrency(FValue / 100)
  else
    Result:= ''
end;

function TtiFieldCurrency.GetAsFloat: Extended;
begin
  Result:= FValue / 100;
end;

function TtiFieldCurrency.GetAsString: string;
var
  LWhole: integer;
  LFrac: integer;
  LValue: integer;
  LSign: integer;
begin
  if not IsNull then
  begin
    LSign:= Sign(FValue);
    LValue:= Abs(FValue);
    LWhole:= LValue div 100;
    LFrac:= LValue - (LWhole * 100);
    Result:= IntToStr(LWhole) + '.' + tiPad0(IntToStr(LFrac), 2);
    if LSign = -1 then
      Result:= '-' + Result;
  end else
    Result:= ''
end;

procedure TtiFieldCurrency.Inc(const AValue: integer);
begin
  AsInteger:= AsInteger + AValue;
end;

procedure TtiFieldCurrency.Inc(const AField: TtiFieldCurrency);
begin
  AsInteger:= AsInteger + AField.AsInteger;
end;

procedure TtiFieldCurrency.SetAsCurrencyString(const AValue: string);
var
  LValue: string;
  LSign: Shortint;
  LWhole: string;
  LFrac: string;
  {$IF Defined(DELPHI2009) or Defined(DELPHI2010)}
    FormatSettings: TFormatSettings;
  {$IFEND}
begin
  {$IF Defined(DELPHI2009) or Defined(DELPHI2010)}
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  {$IFEND}
  LValue:= UpperCase(Trim(AValue));
  if (LValue = '') or (LValue = 'NIL') then
  begin
    IsNull:= True;
    Exit; //==>
  end;
  IsNull:= False;

  if Pos('DR', LValue) <> 0 then
    LSign:= -1
  else
    LSign:= 1;

  LValue:= tiStrTran(LValue, FormatSettings.ThousandSeparator, '');
  LValue:= tiStrTran(LValue, FormatSettings.CurrencyString, '');
  LValue:= tiStrTran(LValue, '$', '');
  LValue:= tiStrTran(LValue, 'DR', '');
  LValue:= tiStrTran(LValue, 'CR', '');
  LValue:= tiStrTran(LValue, ' ', '');
  LValue:= Trim(LValue);
  if (Copy(LValue, Length(LValue), 1) = '-') or
     (Copy(LValue, 1, 1) = '-') then
  begin
    LSign:= -1;
    LValue:= tiStrTran(LValue, '-', '');
  end;
  LWhole:= tiToken(LValue, '.', 1);
  LFrac:= tiToken(LValue, '.', 2);
  FValue:= (StrToIntDef(LWhole, 0) * 100 + StrToIntDef(LFrac, 0)) * LSign;
end;

procedure TtiFieldCurrency.SetAsFloat(const AValue: Extended);
begin
  IsNull:= False;
  FValue:= tiRound(AValue * 100);
end;

procedure TtiFieldCurrency.SetAsInteger(const Value: Int64);
begin
  IsNull:= false;
  FValue := Value;
end;

procedure TtiFieldCurrency.SetAsString(const AValue: string);
begin
  SetAsCurrencyString(AValue);
end;

{ TtiFieldBoolean }

procedure TtiFieldBoolean.Assign(const AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldBoolean), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsBoolean:= (AAssignFrom as TtiFieldBoolean).AsBoolean;
end;

procedure TtiFieldBoolean.Clear;
begin
  inherited Clear;
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

function TtiFieldBoolean.Equals(const ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldBoolean), CTIErrorInvalidObject);
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (AsBoolean = (ACompareWith as TtiFieldBoolean).AsBoolean);
end;

{ TtiFieldDateTime }

procedure TtiFieldDateTime.Assign(const AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldDateTime), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsDateTime := (AAssignFrom as TtiFieldDateTime).AsDateTime;
end;

function TtiFieldDateTime.GetAsString: string;
begin
  Result := tiDateTimeAsXMLString(Value);
end;

procedure TtiFieldDateTime.SetAsDateTime(const AValue: TDateTime);
begin
  FValue := AValue;
  SetValue;
end;

procedure TtiFieldDateTime.SetAsString(const AValue: string);
begin
  FValue := tiXMLStringToDateTime(AValue);
  SetValue;
end;

function TtiFieldDateTime.GetHours: Word;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  Result := Hour;
end;

function TtiFieldDateTime.GetMinutes: Word;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  Result := Min;
end;

function TtiFieldDateTime.GetSeconds: Word;
var
  Hour, Min, Sec, MSec: word;
begin
  DecodeTime(Value, Hour, Min, Sec, MSec);
  Result := Sec;
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

{ TtiFieldList }

constructor TtiFieldList.Create;
begin
  inherited Create;
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
    OID := OIDGenerator.NextOID;
  {$ELSE}
    OIDGenerator.AssignNextOID(OID, ADatabaseName, APersistenceLayerName);
  {$ENDIF}
  ObjectState := posCreate;
end;

function TtiObject.PropToCSV(const AFieldDelim: string; AColsSelected: TStringList): string;
var
  j, LIntValue      : integer;
  LValue   : string;
  LFieldName : string;
  LPropType: TTypeKind;
begin
  Result := '';
  for j := 0 to AColsSelected.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + AFieldDelim;
    LFieldName := AColsSelected.Strings[j];
{$IFDEF IOS}
    if GetPropInfo(Self,LFieldName).NameFld.ToString = 'TDateTime' then
{$ELSE}
    if GetPropInfo(Self,LFieldName)^.PropType^.Name = 'TDateTime' then
{$ENDIF IOS}
      LValue := tiDateTimeToStr(PropValue[LFieldName])
    else
    begin
      lPropType := TypInfo.PropType(Self, LFieldName);
      case lPropType of
        tkChar       : LValue := IntToStr(GetOrdProp(Self, LFieldName));
        tkWChar      : LValue := IntToStr(GetOrdProp(Self, LFieldName));
        tkString     : LValue := GetStrProp(Self, LFieldName);
        tkLString    : LValue := GetStrProp(Self, LFieldName);
        tkWString    : LValue := GetStrProp(Self, LFieldName);
        {$IFDEF FPC}
        tkAString    : LValue := GetStrProp(Self, LFieldName);
        {$ENDIF}
        tkInteger    : begin  // Two step extract required to avoid strange errors with Int props that have getters
                         LIntValue  := GetInt64Prop(Self, LFieldName);
                         LValue := IntToStr(LIntValue);
                       end;
        tkInt64      : LValue := IntToStr(GetInt64Prop(Self, LFieldName));
        tkFloat      : LValue := FloatToStr(GetFloatProp(Self, LFieldName));
        tkEnumeration : LValue := IntToStr(GetOrdProp(Self, LFieldName));
        {$IFDEF FPC}
        tkBool       : LValue := IntToStr(GetInt64Prop(Self, LFieldName));
        {$ENDIF}
        {$IFDEF UNICODE}
        tkUString    : LValue := GetStrProp(Self, LFieldName);
        {$ENDIF}
      end;
    end;
    if Pos(AFieldDelim,LValue) > 0 then
      LValue := tiQuote(LValue); 
    Result := Result + LValue;
  end;
end;

function TtiObject.PropType(const APropName: string): TtiTypeKind;
begin
  Assert(APropName <> '', 'APropName not assigned');
  Result := tiGetSimplePropType(Self, APropName);
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
            ((Depth <= 1) or (adsChildren in ToShow));    //IPK Was adsData - why I don't know
end;

constructor TVisTIObjectAsDebugString.Create;
begin
  inherited Create;
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

  if adsChildCount in ToShow then
  begin
    if (AVisited is TtiObjectList) then
    begin
      Write('Count=' + IntToStr(TtiObjectList(AVisited).Count));
      Write(', ');
    end;
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
        Write('OID=(nil)');
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
              LPropValue := cError + e.Message;
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
    ContinueVisiting := False;
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
  const AValuesToShow: TtiObjectAsDebugStringValuesToShow): string;
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
  if FOID = AValue then Exit; //==>
  FOID := AValue;
  NotifyObserversHelper;
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

procedure TtiObjectList.CompareWith(
  const AList: TtiObjectList;
  const AInBothAndEquals: TtiObjectListCompareEvent;
  const AInBothAndNotEquals: TtiObjectListCompareWithDifferenceMessageEvent;
  const AIn1Only: TtiObjectListCompareEvent;
  const AIn2Only: TtiObjectListCompareEvent;
  const AItemMatchFunc: TtiObjectListItemMatchFunc;
  const AItemCompareFunc: TtiObjectListItemCompareFunc);
var
  i: Integer;
  L: TtiObject;
  LDifferenceMessage: string;
  LEquals: Boolean;
begin
  Assert(AList.TestValid, CTIErrorInvalidObject);
  Assert(Assigned(AInBothAndEquals),    'AInBothAndEquals not assigned');
  Assert(Assigned(AInBothAndNotEquals), 'AInBothAndNotEqualsnot assigned');
  Assert(Assigned(AIn1Only),            'AIn1Onlynot assigned');
  Assert(Assigned(AIn2Only),            'AIn2Onlynot assigned');

  for i := 0 to Count - 1 do
  begin
    if Assigned(AItemMatchFunc) then
      L := AList.Find(Items[i], AItemMatchFunc)
    else
      L := AList.Find(Items[i].OID);
    if L = nil then
      AIn1Only(Items[i], nil)
    else
    begin
      if Assigned(AItemCompareFunc) then
        LEquals := AItemCompareFunc(Items[i], L)
      else
        LEquals := Items[i].Equals(L, LDifferenceMessage);
      if LEquals then
        AInBothAndEquals(Items[i], L)
      else
        AInBothAndNotEquals(Items[i], L, LDifferenceMessage);
    end;
  end;

  for i := 0 to AList.Count - 1 do
  begin
    if Assigned(AItemMatchFunc) then
      L := Find(AList.Items[i], AItemMatchFunc)
    else
      L := Find(AList.Items[i].OID);
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

procedure TtiObject.BeginUpdate(const ATopic: string);
begin
  Inc(FUpdateCount);
  if ATopic <> '' then
    UpdateTopicList.Add(ATopic);
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

function TtiObject.Equals(const AData: TtiObject;
  var ADifferenceMessage: string): boolean;
begin
  result:= Equals(AData);
  if not result then
  begin
    if ADifferenceMessage <> '' then
      ADifferenceMessage:= ADifferenceMessage + '; ';
    ADifferenceMessage :=
      ADifferenceMessage + '"' + ClassName + ': ' +
      Caption + ' <> ' + AData.Caption + '"';
  end;
end;

function TtiObject.NotifyObserversHelper(const ATopic: string): ItiNotifyObserversHelper;
begin
  result := TtiNotifyObserversHelper.Create(Self, ATopic);
end;

procedure TtiObject.NotifyObservers;
begin
  NotifyObservers(Self, noChanged, nil, '');
end;

procedure TtiObject.NotifyObservers(const ATopic: string);
begin
  NotifyObservers(Self, noChanged, nil, ATopic);
end;

procedure TtiObject.NotifyObservers(ASubject: TTiObject;
  AOperation: TNotifyOperation);
begin
  NotifyObservers(ASubject, AOperation, nil, '');
end;

procedure TtiObject.NotifyObservers(ASubject: TTiObject;
  AOperation: TNotifyOperation; const ATopic: string);
begin
  NotifyObservers(ASubject, AOperation, nil, ATopic);
end;

procedure TtiObject.NotifyObservers(ASubject: TTiObject;
  AOperation: TNotifyOperation; AData: TTiObject; const ATopic: string);
var
  ObjectIndex: Integer;
  Observer: TtiObject;
  LObserverList: TList;
  LErrors: TtiObjectErrors;
  ObsIntf: ItiObserverHandlesErrorState;
  NeedsErrorList: Boolean;
begin
  if not Assigned(FObserverList) then
    Exit; //==>
  LErrors := nil;

  if ATopic <> '' then
    UpdateTopicList.Add(ATopic);

  // Allow observers to be removed during notification.
  LObserverList := TList.Create;
  try
    LObserverList.Assign(FObserverList);

    { First find out if we need to call IsValid. Also IsValid should only be
      called if AOperation <> noFree. ie: not during a destructor. }
    NeedsErrorList := False;
    ObjectIndex := 0;
    if (AOperation <> noFree) then
    begin
      while (not NeedsErrorList) and (ObjectIndex < LObserverList.Count) do
      begin
        Observer := TtiObject(LObserverList.Items[ObjectIndex]);
        if Assigned(Observer) and Supports(Observer, ItiObserverHandlesErrorState, ObsIntf) then
          NeedsErrorList := True;
        inc(ObjectIndex);
      end;
    end;

    { collect possible errors }
    if NeedsErrorList then
    begin
      LErrors := TtiObjectErrors.Create;
      ASubject.IsValid(LErrors);
    end;

    for ObjectIndex := 0 to LObserverList.Count - 1 do
    begin
      // FObserverList is freed when empty.
      if not Assigned(FObserverList) then
        break;
      Observer := TtiObject(LObserverList.Items[ObjectIndex]);
      if Assigned(Observer) and
         ((ObjectIndex = 0) or (FObserverList.IndexOf(Observer) <> -1)) then
      begin
        Observer.Update(ASubject, AOperation, AData);
        if NeedsErrorList then
        begin
          if Supports(Observer, ItiObserverHandlesErrorState, ObsIntf) then
            ObsIntf.ProcessErrorState(ASubject, AOperation, LErrors);
        end;
      end;
    end;
  finally
    if Assigned(LErrors) then
      LErrors.Free;
    LObserverList.Free;
  end;

  // We free the topic list to conserve memory and to clear the topics to
  // start a fresh list for future notifications.
  if Assigned(FUpdateTopicList) then
    FreeAndNil(FUpdateTopicList);
end;

procedure TtiObject.Update(ASubject: TtiObject);
begin
  { Do nothing here. This will be implemented in decendant classes when needed }
end;

procedure TtiObject.Update(ASubject: TtiObject; AOperation: TNotifyOperation);
begin
  Update(ASubject, AOperation, nil);
end;

procedure TtiObject.Update(ASubject: TtiObject; AOperation: TNotifyOperation;
  AData: TtiObject);
begin
  if (AOperation=noChanged) then
    Update(ASubject)
  else if (AOperation=noFree) then
    StopObserving(ASubject)
end;

procedure TtiObject.StopObserving(ASubject: TtiObject);
begin
  { Do nothing here. This will be implemented in decendant classes when needed }
end;

function TtiObject.GetObjectState: TPerObjectState;
begin
  result:= FObjectState;
end;

function TtiObject.GetObserverList: TList;
begin
  if not Assigned(FObserverList) then
    FObserverList := TList.Create;
  Result := FObserverList;
end;

function TtiObject.GetUpdateTopicList: TStringList;
begin
  if not Assigned(FUpdateTopicList) then
  begin
    FUpdateTopicList := TStringList.Create;
    FUpdateTopicList.Duplicates := dupIgnore;
  end;
  result := FUpdateTopicList;
end;

procedure TtiObjectList.FreeDeleted;
var
  i: Integer;
begin
  BeginUpdate;
  Try
    // ToDo: Should really be handled through a visitor that iterates,
    //       bottom up, freeing all nested objects
    for i:= Pred(Count) downto 0 do
      if Items[i].ObjectState = posDeleted then
        Delete(i);
  finally
    EndUpdate;
  end;
end;

{ TtiFieldStringMethod }

procedure TtiFieldStringMethod.Assign(const AAssignFrom: TtiFieldAbs);
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

function TtiFieldStringMethod.Equals(const ACompareWith: TtiFieldAbs): Boolean;
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

{ TtiFieldDate }

procedure TtiFieldDate.Assign(const AAssignFrom: TtiFieldAbs);
begin
  Assert(AAssignFrom.TestValid(TtiFieldDate), CTIErrorInvalidObject);
  if AAssignFrom.IsNull then
    IsNull:= true
  else
    AsDateTime := (AAssignFrom as TtiFieldDate).AsDateTime;
end;

procedure TtiFieldDate.Clear;
begin
  inherited Clear;
  FValue := 0;
end;

function TtiFieldDate.Equals(const ACompareWith: TtiFieldAbs): Boolean;
begin
  Assert(ACompareWith.TestValid(TtiFieldDate), CTIErrorInvalidObject);
  Result :=
    (IsNull = ACompareWith.IsNull) and
    (SameValue(AsDateTime, (ACompareWith as TtiFieldDate).AsDateTime, cdtOneSecond/2));
end;

function TtiFieldDate.GetAsString: string;
begin
  Result := tiDateToStr(FValue);
end;

function TtiFieldDate.GetDays: Word;
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

function TtiFieldDate.GetMonths: Word;
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

function TtiFieldDate.GetYears: Word;
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

procedure TtiFieldDate.SetAsDateTime(const AValue: TDateTime);
var
  LValue: TDateTime;
begin
  LValue:=Trunc(AValue);
  if not tiIsNearEnough(LValue, AValue) then
    raise EtiOPFDataException.CreateFmt(CErrorInvalidDate, [tiDateTimeToStr(AValue)]);
  FValue := AValue;
  if FValue <> CNullDate then
    SetValue
  else
    Clear;
end;

procedure TtiFieldDate.SetAsString(const AValue: string);
begin
  Value := tiXMLStringToDateTime(AValue);
end;

{ TtiObserverProxy }

constructor TtiObserverProxy.Create;
begin
  inherited Create;
  Subject := nil;
  FOnUpdate := nil;
end;

constructor TtiObserverProxy.Create(const ASubject: TtiObject;
  const AOnUpdate: TtiObjectUpdateEvent);
begin
  Assert(Assigned(AOnUpdate), 'AOnUpdate should be assigned');
  inherited Create;

  Subject := ASubject;
  FOnUpdate := AOnUpdate;
end;

destructor TtiObserverProxy.Destroy;
begin
  Subject := nil;
  inherited Destroy;
end;

procedure TtiObserverProxy.SetSubject(const AValue: TtiObject);
begin
  Assert(AValue.TestValid(TtiObject, true), CTIErrorInvalidObject);

  if AValue = FSubject then
    Exit; //==>

  if Assigned(FSubject) then
    FSubject.DetachObserver(Self);
  FSubject := AValue;
  if Assigned(FSubject) then
    FSubject.AttachObserver(Self);
end;

procedure TtiObserverProxy.StopObserving(ASubject: TtiObject);
begin
  inherited StopObserving(ASubject);
  Subject := nil;
end;

procedure TtiObserverProxy.Update(ASubject: TtiObject;
  AOperation: TNotifyOperation; AChild: TtiObject);
begin
  inherited Update(ASubject, AOperation, AChild);
  if (ASubject = Subject) and (AOperation = noFree) then
    Subject := nil;
  if Assigned(FOnUpdate) then
    FOnUpdate(ASubject, AOperation);
end;

{ TtiObserversProxy }

constructor TtiObserversProxy.Create;
begin
  inherited;
  FSubjects := TtiObjectList.Create;
  FSubjects.OwnsObjects := false;
  FSubjects.AutoSetItemOwner := false;
end;

destructor TtiObserversProxy.Destroy;
begin
  DetachAll;
  FSubjects.Free;
  inherited;
end;

procedure TtiObserversProxy.AttachTo(ASubject: TtiObject);
begin
  if Assigned(ASubject) and (FSubjects.IndexOf(ASubject) = -1) then
  begin
    FSubjects.Add(ASubject);
    ASubject.AttachObserver(Self);
  end;
end;

procedure TtiObserversProxy.DetachFrom(ASubject: TtiObject);
var
  i: integer;
begin
  i := FSubjects.IndexOf(ASubject);
  if i >= 0 then
  begin
    ASubject.DetachObserver(Self);
    FSubjects.Delete(i);
  end;
end;

procedure TtiObserversProxy.DetachFrom(ASubjectClass: TtiObjectClass);
var
  i: integer;
begin
  for i := FSubjects.Count - 1 downto 0 do
    if FSubjects.Items[i] is ASubjectClass then
    begin
      FSubjects.Items[i].DetachObserver(Self);
      FSubjects.Delete(i);
    end;
end;

procedure TtiObserversProxy.DetachAll;
var
  i: integer;
begin
  for i := FSubjects.Count - 1 downto 0 do
  begin
    FSubjects.Items[i].DetachObserver(Self);
    FSubjects.Delete(i);
  end;
end;

procedure TtiObserversProxy.StopObserving(ASubject: TtiObject);
begin
  inherited;
  DetachFrom(ASubject);
end;

procedure TtiObserversProxy.Update(ASubject: TtiObject;
  AOperation: TNotifyOperation);
begin
  inherited;
  if Assigned(FOnUpdate) then
    FOnUpdate(ASubject, AOperation);
end;

end.

