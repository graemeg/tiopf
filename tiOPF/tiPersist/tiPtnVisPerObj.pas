{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

  Purpose: Add some properties to the TVisitedAbs to turn it into a
           persistent object

  Revision History:
    Sept 1999, Peter Hinrichsen, Created
    Nov 2000, SM, Added TPerObjFindMethodExt procedure declaration to take
                  UserContext pointer as a parameter.
    Nov 2000, SM, Added overloaded Find to TPerObjAbs class to take UserContext
                  pointer as a parameter - allows thread safe searching as no
                  global parameters need to be defined.
    Feb 2001, Ian Krigsman, Added ItemParent property.
    Sept 2001, PH & IK, changed Assign method to raise an exception if an
                  class type property is to be assigned. This forces the
                  developer to override assign if the object is not flat.
    Dec 2001, PH, Added ReadPK, Read and Save methods.
    Aug 2002, IK  Added SortByDispOrder;
    Apr 2003, AD  Added ability for lists to be sorted in both ascending and
                  descending order.
    Aug 2003, AD  Added RaiseLockException property to TPerObjThreadList.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{:@summary The base unit for tiOPF.
   @desc Contains all the base classes used throughout the framework.}

{$I tiDefines.inc}

unit tiPtnVisPerObj ;

interface
uses
  tiPtnVis
  ,Classes
  ,TypInfo
  ,Contnrs
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,Libc
  {$ENDIF LINUX}
  ,tiPerObjOIDAbs
  ,tiObjAbs
  ;


type
  {: The possible states of a TPerObjAbs or descendant in memory.
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

  TPerObjAbs = class ;
  TPerObjList = class ;
  TPerObjErrors = class ;
  TPerObjError  = class ;

  TPerObjFindMethod = procedure( pPerObjAbs : TPerObjAbs ; var pbFound : boolean ) of object ;
  TPerObjFindMethodExt = procedure( pPerObjAbs : TPerObjAbs ; var pbFound : boolean; pUserContext: Pointer ) of object ;
  TPerObjFindMethodData = procedure( pPerObjAbs : TPerObjAbs ; var pbFound : boolean; pData : TPerObjAbs ) of object ;
  TPerObjForEachMethod        = procedure( pPerObjAbs : TPerObjAbs ) of object ;
  TPerObjForEachMethodRegular = procedure( pPerObjAbs : TPerObjAbs ) ;
  TPerObjAbsEvent = procedure( const pData: TPerObjAbs ) of object ;

{
  // Template for creating TPerVisList and TPerObjAbs stubs...
  TMyClasses = class ;
  TMyClass   = class ;

  //----------------------------------------------------------------------------
  TMyClasses = class( TPerObjList )
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

  //----------------------------------------------------------------------------
  TMyClass = class( TPerObjAbs )
  private
  protected
    function    GetOwner: TMyClasses; reintroduce ;
    procedure   SetOwner(const Value: TMyClasses ); reintroduce ;
  public
    property    Owner       : TMyClasses             read GetOwner      write SetOwner ;
  published
  end ;
}

  //----------------------------------------------------------------------------
  TPerObjAbs = class( TVisitedAbs )
  private
    FOID : TOID ;
    FObjectState : TPerObjectState ;
    FiDispOrder : integer ;
  protected
    FOwner: TPerObjAbs; // Want FOwner here as there are time when we may want
                        // go get access without going through the GetOwner and
                        // SetOwner as these may have been typecase in a way
                        // which causes incompatabilities.
    function    GetDeleted: boolean; virtual ;
    procedure   SetDeleted(const Value: boolean); virtual ;
    procedure   SetDirty(const Value: boolean); virtual ;
    procedure   SetObjectState(const Value: TPerObjectState) ; virtual ;
    function    GetOID: TOID; virtual ;
    function    GetDirty : boolean ; virtual ;
    function    GetOwner: TPerObjAbs; reintroduce ; virtual ;
    procedure   SetOwner(const Value: TPerObjAbs); virtual ;
    procedure   AssignPublicProps(pSource: TPerObjAbs);virtual ;
    procedure   AssignPublishedProp(pSource: TPerObjAbs; psPropName: string);
    function    CountPropsByType(pSource: TPerObjAbs; pPropFilter: TTypeKinds): integer;
    procedure   AssignPublishedProps(pSource: TPerObjAbs; pPropFilter: TTypeKinds = [] );
    // You must override this in the concrete if there are class properties
    procedure   AssignClassProps(pSource: TPerObjAbs); virtual ;
    function    GetIndex : integer ;
    function    GetDispOrder: integer; virtual ;
    procedure   DoFindAllNotUnique(  pPerObjAbs : TPerObjAbs ; var pbFound : boolean; pData : TPerObjAbs ) ; virtual ;
    function    GetPropValue(const pPropName: string): Variant; virtual ;
    procedure   SetPropValue(const pPropName: string; const pPropValue: Variant); virtual ;

  public
    {: Creates a new instance of the class}
    constructor Create  ; override ;
    {: Creates a new instance of the class and initialises it's OID with next available value}
    constructor CreateNew( const pOwner : TPerObjAbs ; const pDatabaseName : string = '' ; const pPerLayerName : string = '' ) ; overload ; virtual ;
    constructor CreateNew( const pDatabaseName : string ; const pPerLayerName : string ) ; overload ; virtual ;
    constructor CreateNew ; overload ; virtual ;
    destructor  Destroy ; override ;
    {: Dos this object equal another? }
    function    Equals( pData : TPerObjAbs ) : boolean ; virtual ;
    {: The OID of this object }
   {$IFDEF OID_AS_INT64}
      property    OID         : TOID                   read GetOID write FOID ;
   {$ELSE}
      property    OID         : TOID                   read GetOID        ;
   {$ENDIF}
    {: The display order of this object}

    property    DispOrder   : integer                read GetDispOrder  write FiDispOrder ;
    {: The current state of this object}
    property    ObjectState : TPerObjectState        read FObjectState  write SetObjectState ;
    {: The type of class that owns this object}
    property    Owner       : TPerObjAbs             read GetOwner      write SetOwner ;
    {: Returns the value of the property specified in pPropName.}
    property    PropValue[const pPropName : string]   : Variant                read GetPropValue  write SetPropValue ;
    {: Is the specified property read and write?}
    function    IsReadWriteProp( const pPropName : string ) : boolean ; virtual ;
    {: Is this object deleted? }
    property    Deleted : boolean                    read GetDeleted    write SetDeleted ;
    {: Is this object out of sync with the persistence layer?}
    property    Dirty   : boolean                    read GetDirty      write SetDirty ;

    property    Index : integer                      read GetIndex ;
    {: Returns this objects state as a string value.}
    function    ObjectStateAsString : string ;
    {: Find an object in the hierarchy by OID with the OID passed as a string}
    function    Find( pOIDToFindAsString : string ) : TPerObjAbs ;  overload ; virtual ;
    {: Find an object in the hierarchy by OID with the OID passed as a TOID object}
    function    Find( pOIDToFind : TOID ) : TPerObjAbs ;  overload ; virtual ;
    {: Find an object in the hierarchy using the find method passed}
    function    Find( pPerObjFindMethod : TPerObjFindMethod ) : TPerObjAbs ; overload ;
    {: Find an object in the hierarchy using the extended find method passed}
    function    Find( pPerObjFindMethodExt : TPerObjFindMethodExt; pUserContext: Pointer ) : TPerObjAbs ; overload;
    {: Find all objects in the hierarchy using the find method passed}
    function    FindAll( pPerObjFindMethod : TPerObjFindMethod; pList : TList ) : integer ; overload ;
    function    FindAll( pPerObjFindMethodExt : TPerObjFindMethodExt; pList : TList; pUserContext: Pointer ) : integer ; overload ;
    function    FindAll( pPerObjFindMethodData : TPerObjFindMethodData; pList : TList; pData : TPerObjAbs ) : integer ; overload ;
    {:Is this obect unique in the hierarchy?}
    function    IsUnique( const pPerObjAbs : TPerObjAbs ) : boolean ; virtual ;
    {: Creates a cloned instance of this object. }
    function    Clone : TPerObjAbs ; virtual ; // Must override and typecast if to be used
    {: Copy this object to another.}
    procedure   Assign( pSource : TPerObjAbs ) ; reintroduce ; virtual ;
    {: returns the object at the top of the hierarchy}
    function    TopOfHierarchy : TPerObjAbs ; virtual ;
    procedure   SetAllObjectStates( const pObjectState : TPerObjectState ) ;
    function    IsValid( const pErrors : TPerObjErrors ) : boolean ; overload ; virtual ;
    function    IsValid( var pErrorMessage : string ) : boolean ; overload ; // Don't override this one
    function    IsValid( const pStrings : TStrings ) : boolean ; overload ; // Don't override this one
    function    IsValid( const pStrings : TStrings ; pAppend : boolean ) : boolean ; overload ; // Don't override this one
    function    IsValid : boolean ; overload ; // Don't override this one
    {: Read in the primary Key values only from the database for this object.}
    procedure   ReadPK(   const pDBConnectionName : string ; pPerLayerName : string = '' ) ; overload ; virtual ;
    procedure   ReadPK ; overload ; virtual ;
    {: Read this object, but no owned objects from the database }
    procedure   ReadThis( const pDBConnectionName : string ; pPerLayerName : string = ''  ) ; overload ;  virtual ;
    procedure   ReadThis ; overload ;  virtual ;
    {: Read this object, along with any owned objects from the database }
    procedure   Read(     const pDBConnectionName : string ; pPerLayerName : string = ''  ) ; overload ;  virtual ;
    procedure   Read ; overload ;  virtual ;
    {: Updates the database with the current property values for this object.}
    procedure   Save(     const pDBConnectionName : string ; pPerLayerName : string = ''  ) ; overload ;  virtual ;
    procedure   Save ; overload ;  virtual ;


  end ;

  //----------------------------------------------------------------------------
  TPerObjList = class( TPerObjAbs )
  private
    FList : TObjectList ;
    FItemOwner: TPerObjAbs;
    FbAutoSetItemOwner: boolean;
    function    GetList: TList;
    procedure   AssignDispOrder(pData: TPerObjAbs);
    function    GetCountNotDeleted: integer;
    function    GetOwnsObjects: boolean;
    procedure   SetOwnsObjects(const Value: boolean);
    function    DoCompareByProps( pItem1   : Pointer ; pItem2   : Pointer ;
                const pSortProps : array of string; pAscendingOrder : Boolean = True) : integer ;
    procedure   QuickSortByProps(SortList: PPointerList; L, R: Integer;
                const pSortProps : array of string; pAscendingOrder : Boolean = True);
  protected
    function    GetCount: integer; virtual ;
    function    GetItems(i: integer): TPerObjAbs ; virtual ;
    procedure   SetItems(i: integer; const Value: TPerObjAbs); virtual ;
    procedure   SetItemOwner(const Value: TPerObjAbs); virtual ;
    procedure   AssignPublicProps( pSource : TPerObjAbs ) ; override ;
    procedure   AssignClassProps(pSource: TPerObjAbs); override ;
  public
    constructor Create ; override ;
    destructor  destroy ; override ;

    procedure   Assign( pSource : TPerObjAbs ) ; override ;
    {: The number of items in the list.}
    property    Count : integer read GetCount ;
    {: The number of items in the list that are not marked as deleted.}
    property    CountNotDeleted : integer read GetCountNotDeleted ;
    property    Items[i:integer] : TPerObjAbs read GetItems write SetItems ; default ;

    {: Does the list own the objects it contains? i.e. Will the objects be freed when the list is cleared/destroyed?}
    property    OwnsObjects : boolean read GetOwnsObjects write SetOwnsObjects ;
    property    ItemOwner      : TPerObjAbs read FItemOwner    write SetItemOwner ;
    property    AutoSetItemOwner : boolean read FbAutoSetItemOwner write FbAutoSetItemOwner ;

    {: Finds the object in the list whose OID value matches.}
    function    Find( pOIDToFindAsString : string ) : TPerObjAbs ;  override ;
    {: Finds the object in the list whose OID value matches.}
    function    Find( pOIDToFind : TOID ) : TPerObjAbs ; override ;
    {: Performs the method pMethod on every object in the list.}
    procedure   ForEach( pMethod : TPerObjForEachMethod        ; pbIncludeDeleted : boolean = false ) ; overload ; virtual ;
    {: Performs the method pMethod on every object in the list.}
    procedure   ForEach( pMethod : TPerObjForEachMethodRegular ; pbIncludeDeleted : boolean = false ) ; overload ; virtual ;
    {: Add an object to the list.}
    procedure   Add( pObject : TPerObjAbs  ; pDefDispOrdr : boolean = true ) ; virtual ;
    {: Empty list and delete all owned objects}
    procedure   Clear ; virtual ;
    {: Empty list, but do not delete owned objects }
    procedure   Empty ; virtual ;
    {: The index of the specified object in the list.}
    function    IndexOf( pData : TObject ) : integer ; virtual ;
    {: The last object in the list.}
    function    Last  : TPerObjAbs ; virtual ;
    {: The first object in the list.}
    function    First : TPerObjAbs ; virtual ;
    {: Returns the last item in the list that hasn't been marked deleted.}
    function    LastExcludeDeleted : TPerObjAbs ; virtual ;
    {: Removes the object at a specified position and (if OwnsObject is True) frees the object.}
    procedure   Delete( i : integer ) ; virtual ;
    {: Removes the specified item from the list and (if OwnsObject is True) frees the object.}
    function    Remove( pData : TPerObjAbs ) : integer ; virtual ;
    {: Removes the specified object from the list without freeing the object.}
    procedure   Extract( pData : TPerObjAbs ) ; virtual ;
    {: Adds an object to the list at the position specified by Index.}
    procedure   Insert( const piIndex : integer ; pData : TPerObjAbs ) ; overload ; virtual ;
    procedure   Insert( pInsertBefore : TPerObjAbs ; pData : TPerObjAbs ) ; overload ; virtual ;
    {: Sets all items in the list as ready for deletion}
    procedure   MarkListItemsForDeletion ; virtual ;
    {: Sets all items in the list as needing updating to the database}
    procedure   MarkListItemsDirty ; virtual ;
    procedure   PropToStrings(const pStrings: TStrings ; const pPropName : string = 'caption' ); virtual ;
    {: Finds the first object whose properties passed in pProps match the values specified.}
    function    FindByProps( const pProps : array of string ;
                             const pVals  : array of variant;
                             pCaseSensitive : boolean = true  ) : TPerObjAbs ; virtual ;
    {: Sorts the list by the properties specified}
    procedure   SortByProps( const pSortProps : array of string; pAscendingOrder : Boolean = True) ; virtual ;
    {: Sorts the list by each member's OID value.}
    procedure   SortByOID ; virtual ;
    {: Sorts the list by each member's DispOrder value.}
    procedure   SortByDispOrder; virtual;

  published
    // This must be published so it can be used by the tiPerAware controls.
    property    List  : TList read GetList ;
  end ;

  TPerObjAbsClass  = class of TPerObjAbs ;
  TPerObjListClass = class of TPerObjList ;

  //----------------------------------------------------------------------------
  TPerObjErrors = class( TPerObjList )
  private
  protected
    function    GetItems(i: integer): TPerObjError ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TPerObjError); reintroduce ;
  public
    property    Items[i:integer] : TPerObjError read GetItems write SetItems ;
    procedure   Add( pObject : TPerObjError   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
    procedure   AddError( const pErrorProperty : string ; const pErrorMessage : string ; pErrorCode : integer = -1 ) ;
    function    FindByMessage( const pMessage : string ) : TPerObjError ;
  published
  end ;

  //----------------------------------------------------------------------------
  TPerObjError = class( TPerObjAbs )
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

  //----------------------------------------------------------------------------
  TPerObjClassMapping = class( TtiObjAbs )
  private
    FPerObjAbsClassName: string;
    FPerObjAbsClass: TPerObjAbsClass;
  public
    constructor Create ; virtual ;
    property PerObjAbsClassName : string read FPerObjAbsClassName write FPerObjAbsClassName ;
    property PerObjAbsClass : TPerObjAbsClass read FPerObjAbsClass write FPerObjAbsClass ;
  end ;

  //----------------------------------------------------------------------------
  TPerObjFactory = class( TtiObjAbs )
  private
    FList : TObjectList ;
  protected
    function FindByClassName( const pClassName : string ) : TPerObjClassMapping ; virtual ;
    function GetItems(pIndex: integer): TPerObjClassMapping; virtual ;
  public
    constructor create ; virtual ;
    destructor  destroy ; override ;
    procedure   RegisterClass( const pClassName : string ; pClass : TPerObjAbsClass ) ;
    procedure   UnRegisterClass( const pClassName : string ) ;
    function    CreateInstance( const pClassName : string ) : TPerObjAbs ;
    function    CreateNewInstance( const pClassName : string; pOwner : TPerObjAbs = nil ) : TPerObjAbs ;
    function    IsRegistered( const pClassName : string ) : boolean ;
    property    Items[pIndex : integer] : TPerObjClassMapping read GetItems ;
    function    Count : integer ;
  end ;

  //----------------------------------------------------------------------------
  TPerStream = class( TPerObjAbs )
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

  //----------------------------------------------------------------------------
  TPerStringStream = class( TPerObjAbs )
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

  //----------------------------------------------------------------------------
  {:The thread-safe version of <See Class="TPerObjList">}
  TPerObjThreadList = class( TPerObjList )
  private
    {$IFDEF MSWINDOWS}
    FSemaphore : THandle ;
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
    FSemaphore  : TSemaphore ;
    {$ENDIF LINUX}
    FLockCount : integer ;
    FRaiseLockException : Boolean;
    procedure InternalLock( pExternalLock : boolean  ) ;
    procedure InternalUnLock( pExternalLock : boolean ) ;
  protected
    function    GetCount: integer; override ;
    procedure   SetItems(i: integer; const Value: TPerObjAbs); override ;
    procedure   SetItemOwner(const Value: TPerObjAbs); override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    {: Locks the list to prevent other threads gaining access.}
    procedure   Lock ;
    {: Unlocks the list to allow other threads access.}
    procedure   UnLock ;

    procedure   Delete( i : integer ) ; override ;
    procedure   Add( pObject : TPerObjAbs  ; pDefDispOrdr : boolean = true ) ; override ;
    procedure   Clear ; override ; // Empty list and delete all owned objects
    procedure   Empty ; override ; // Empty list, but do not delete owned objects
    function    IndexOf( pData : TObject ) : integer ; override ;
    function    Last  : TPerObjAbs ; override ;
    function    First : TPerObjAbs ; override ;
    function    LastExcludeDeleted : TPerObjAbs ; override ;
    function    Remove( pData : TPerObjAbs ) : integer ; override ;
    procedure   MarkListItemsForDeletion ; override ;
    procedure   MarkListItemsDirty ; override ;
    procedure   SortByOID ; override ;
    procedure   SortByProps( const pSortProps : array of string; pAscendingOrder : Boolean = True ) ; override ;
    {:Does a lock failure raise an exception or just log an error?}
    Property RaiseLockException : Boolean Read FRaiseLockException Write FRaiseLockException;
  end ;

  // TPerVisList is here for backward compatibility.
  // Do not use. Use TPerObjList instead.
  TPerVisList = class( TPerObjList ) ;

  //----------------------------------------------------------------------------
  TVisPerObjFind = class( TVisitorAbs )
  private
    FUserContext: Pointer;
    FFound: TPerObjAbs;
    FPerObjFindMethod: TPerObjFindMethod;
    FPerObjFindMethodExt: TPerObjFindMethodExt;
    FFoundList: TList;
    FData: TPerObjAbs;
    FPerObjFindMethodData: TPerObjFindMethodData;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    constructor Create ; override ;
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    property    Found : TPerObjAbs read FFound ;
    property    PerObjFindMethod : TPerObjFindMethod read FPerObjFindMethod write FPerObjFindMethod ;
    property    PerObjFindMethodExt: TPerObjFindMethodExt read FPerObjFindMethodExt write FPerObjFindMethodExt;
    property    PerObjFindMethodData: TPerObjFindMethodData read FPerObjFindMethodData write FPerObjFindMethodData;
    property    FoundList : TList read FFoundList write FFoundList ;
    property    UserContext : Pointer read FUserContext write FUserContext ;
    property    Data : TPerObjAbs read FData write FData ;
  end ;

  //----------------------------------------------------------------------------
  TVisPerObjFindByOID = class( TVisitorAbs )
  private
    FOIDToFind: TOID;
    FFound: TPerObjAbs;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    constructor Create ; override ;
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    property    Found : TPerObjAbs read FFound ;
    property    OIDToFind : TOID read FOIDToFind write FOIDToFind ;
  end ;

  //----------------------------------------------------------------------------
  TVisPerObjDel = class( TVisitorAbs )
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

  //----------------------------------------------------------------------------
  TVisSetAllObjectStates = class( TVisitorAbs )
  private
    FObjectState: TPerObjectState;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    property    ObjectState : TPerObjectState read FObjectState write FObjectState ;
  end ;

  //----------------------------------------------------------------------------
  TVisPerObjIsDirty = class( TVisitorAbs )
  private
    FbDirty: boolean;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    property    Dirty : boolean read FbDirty write FbDirty ;
  end ;

  // Stream a TPerObjAbs out as text
  //----------------------------------------------------------------------------
  TVisPerObjToText = class( TVisStringStream )
  private
    FbIncludeDeleted: boolean;
    FbDataOnly: boolean;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    Constructor Create ; override ;
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    Function    Indent : string ;
    property    IncludeDeleted : boolean read FbIncludeDeleted write FbIncludeDeleted ;
    property    DataOnly : boolean read FbDataOnly write FbDataOnly ;
  end ;

const
  cgNullDBInteger            = -1  ;
  cgNullDBString             = ''  ;
  cgNullDBDate               = 0.0 ;
  cuiDispOrderInc            = 1000 ;

function ObjectStateToString( pObjectState : TPerObjectState ) : string ;

implementation
uses
   tiLog
  ,tiUtils
  ,tiDialogs
  ,SysUtils
  ,tiPersist
  {$IFNDEF VER130}
   ,Variants
  {$ENDIF}
  ;
  
//var
//  uSortProps : array of string ;

function ObjectStateToString( pObjectState : TPerObjectState ) : string ;
begin
  result := GetEnumName( TypeInfo( TPerObjectState ),
                         Ord( pObjectState )) ;
end ;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TPerObjAbs
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TPerObjAbs.Create ;
begin
  inherited Create ;
  FObjectState := posEmpty ;
  FiDispOrder  := 0 ;
  FOwner       := nil ;
end;

{: @ToDo We have to address two issues with assign:
 i)  What if our object contains a pointer to another object that it owns
 ii) What if our object contains a list of other objects
 If we are to be creating new instances of owned objects, we must work out
 what to do about ObjectState and OIDs }
// -----------------------------------------------------------------------------
{: Creates a cloned instance of the object. This method must be overridden and
typecast if you are going to use it.}
function TPerObjAbs.Clone: TPerObjAbs;
var
  lClass : TPerObjAbsClass ;
begin
  lClass := TPerObjAbsClass( ClassType ) ;
  result := TPerObjAbs( lClass.Create );
  result.Assign( self ) ;
end;

// -----------------------------------------------------------------------------
{: When you create a concrete class that contains object type properties
you will have to override AssignClassProps( ) and implement the necessary
behaviour to copy, clone or create new instances of these properties.}
procedure TPerObjAbs.Assign(pSource: TPerObjAbs);
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

// -----------------------------------------------------------------------------
procedure TPerObjAbs.AssignPublishedProps( pSource : TPerObjAbs ;
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
          AssignPublishedProp( pSource, lsPropName ) ;
      except
        on e:exception do
          tiFmtException( e, 'Error setting property <' + lsPropName + '>',
                          ClassName, 'Assign' ) ;
      end ;
    end ;
  finally
    lsl.Free ;
  end ;
end ;

// -----------------------------------------------------------------------------
function TPerObjAbs.CountPropsByType( pSource : TPerObjAbs ;
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

// -----------------------------------------------------------------------------
procedure TPerObjAbs.AssignPublicProps( pSource : TPerObjAbs ) ;
begin
  {$IFDEF OID_AS_INT64}
    OID := pSource.OID ;
  {$ELSE}
    OID.Assign( pSource.OID ) ;
  {$ENDIF}
  ObjectState := pSource.ObjectState ;
  DispOrder   := pSource.DispOrder ;
  // 1. If we are cloning a list element to edit, then we will probably
  //    want it's Owner property set to the list. This will be done here.
  // 2. If we are editing a list, then we want each list element to be cloned
  //    too and we want their owner properties to be set to the cloned list,
  //    this will be done in TPerObjList.AssignClassProps.
  // 3. If we are editing a compound object, then Owner will be set in
  //    the classes constructor.
  if pSource.Owner is TPerObjList then
    Owner       := pSource.Owner ;
end ;

// -----------------------------------------------------------------------------
procedure TPerObjAbs.AssignPublishedProp( pSource : TPerObjAbs ; psPropName : string) ;
var
  lPropType : TTypeKind ;
begin
  lPropType := PropType( pSource, psPropName ) ;
  // This will copy pointers so if we are assigning an object, and it has a
  // property which points to another object, A, they will both end up pointing
  // to the same instance of A. This may be what we want, but then if our
  // object owns A, then we probably want another copy of A. This requires some
  // thought.
//  if lPropType in [tkClass] then
//    SetObjectProp( Self,
//                   psPropName,
//                   GetObjectProp( pSource, psPropName ))
//  else // lPropType in ctkSimple + [tkVariant, tkEnumeration]
  if lPropType in ctkSimple + [tkVariant, tkEnumeration] then
    TypInfo.SetPropValue( Self,
                          psPropName,
                          TypInfo.GetPropValue( pSource, psPropName ))
  else
    tiFmtException( 'Unable to assign property because it is not a valid type',
                    ClassName,
                    'AssignPublishedProp' ) ;

end ;

// -----------------------------------------------------------------------------
procedure TPerObjAbs.AssignClassProps( pSource : TPerObjAbs ) ;
begin
  Assert( CountPropsByType( pSource, [tkClass] ) = 0,
          'Trying to call ' + ClassName + '.Assign( ) on a class that contains ' +
          'object type properties. AssignClassProps( ) must be overridden in the concrete class.' ) ;
end ;

// Note: This functionality has not been tested fully. Bit of a hack really :(
//       Talk about thrashing the CPU with out need. :( :( :( !
// -----------------------------------------------------------------------------
function TPerObjAbs.Equals(pData: TPerObjAbs): boolean;
var
  lVisComp : TVisPerObjToText ;
  lVisSelf : TVisPerObjToText ;
begin
  // ToDo: Better to do this as a field by field compare...
  lVisComp := TVisPerObjToText.Create ;
  try
    lVisComp.DataOnly := true ;
    lVisSelf := TVisPerObjToText.Create ;
    try
      lVisSelf.DataOnly := true ;
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

// -----------------------------------------------------------------------------
function TPerObjAbs.GetDeleted: boolean;
begin
  result := ( ObjectState = posDelete ) or
            ( ObjectState = posDeleted ) ;
end;

// -----------------------------------------------------------------------------
function TPerObjAbs.GetDirty: boolean;
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

// -----------------------------------------------------------------------------
procedure TPerObjAbs.SetDeleted(const Value: boolean);
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

// -----------------------------------------------------------------------------
procedure TPerObjAbs.SetDirty(const Value: boolean);
begin
  case ObjectState of
    posEmpty   : begin
                   FObjectState := posCreate    ;
                   {$IFDEF OID_AS_INT64}
                     if OID = -1 then
                       OID := gTIPerMgr.DefaultPerLayer.NextOIDMgr.NextOID ;
                   {$ELSE}
                     if OID.IsNull then
Assert( false, 'Under construction' ) ;
//                       OID.GetNextValue ;
                   {$ENDIF}
                 end ;
    posPK      : FObjectState := posUpdate    ;
    posCreate  : FObjectState := FObjectState ; // Do nothing
    posUpdate  : FObjectState := FObjectState ; // Do nothing
    posDelete  : FObjectState := FObjectState ; // Do nothing
    posDeleted : FObjectState := FObjectState ; // Do nothing
    posClean   : FObjectState := posUpdate ;
  else
    tiFmtException( 'Invalid ObjectState <' + ObjectStateAsString + '>',
                    ClassName, 'SetDirty' ) ;
  end ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPerObjDel
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPerObjDel.AcceptVisitor : boolean;
begin
//  result :=
//    (( Visited is TPerObjAbs ) and
//     ( TPerObjAbs( Visited ).ObjectState <> posDelete )) or
//    (( Visited is TPerObjList ) and
//     ( TPerObjList( Visited ).ObjectState <> posDelete )) ;


  LogArray([ 'Visited.Caption', Visited.Caption ]);
  LogArray([ 'Visited.OID', TPerObjAbs(Visited).OID ]);
  LogArray([ 'Visited is TPerObjAbs', Visited is TPerObjAbs ]);
  LogArray([ 'TPerObjAbs(Visited).ObjectState <> posDeleted', TPerObjAbs(Visited).ObjectState <> posDeleted ]);
  LogArray([ 'if VisitedsOwner = nil', VisitedsOwner = nil ]);
  LogArray([ 'VisitedsOwner = Visited', VisitedsOwner = Visited ]);
  LogArray([ 'VisitedsOwner = TPerObjAbs(Visited).Owner', VisitedsOwner = TPerObjAbs(Visited).Owner ]);
  LogArray([ '' ]);

  result := ( Visited is TPerObjAbs );
  if not result then
    Exit ; //==>

  result := ( TPerObjAbs(Visited).ObjectState <> posDeleted );
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
  result := ( VisitedsOwner = TPerObjAbs(Visited).Owner );
  if result then
    Exit ; //==>

  if VisitedsOwner is TPerObjList then
    result := ( TPerObjList(VisitedsOwner).ItemOwner =
                TPerObjAbs(Visited).Owner );

end;

// -----------------------------------------------------------------------------
procedure TVisPerObjDel.Execute(const pVisited: TVisitedAbs) ;
begin
  Inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

//  if pVisited is TPerObjAbs then
//    TPerObjAbs( pVisited ).ObjectState := posDelete
//  else if pVisited is TPerObjList then
//    TPerObjList( pVisited ).ObjectState := posDelete ;
  TPerObjAbs(pVisited).ObjectState := posDelete ;


end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPerObjIsDirty
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPerObjIsDirty.AcceptVisitor : boolean;
begin
  result := (( Visited is TPerObjAbs ) or
            ( Visited is TPerObjList )) and
            ( not Dirty ) ;
end;

//------------------------------------------------------------------------------
procedure TVisPerObjIsDirty.Execute(const pVisited: TVisitedAbs);
begin

  Inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

  if Visited is TPerObjAbs then
    Dirty := TPerObjAbs( pVisited ).ObjectState in
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
                       ord( TPerObjAbs( pVisited ).ObjectState ))]) ;
}
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TPerObjList
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// -----------------------------------------------------------------------------
constructor TPerObjList.create ;
begin
  inherited Create ;
  FList := TObjectList.Create  ;
  FItemOwner := Self ;
  FbAutoSetItemOwner := true ;
end;

// -----------------------------------------------------------------------------
destructor TPerObjList.destroy;
begin
  FList.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TPerObjList.Add(pObject: TPerObjAbs ; pDefDispOrdr : boolean = true );
var
  l : TPerObjAbs ;
begin
  if FbAutoSetItemOwner then
    pObject.Owner := FItemOwner ;
  if pDefDispOrdr and
     ( pObject.ObjectState = posCreate ) then
  begin
    l := Last ;
    if l <> nil then
      pObject.DispOrder := ( l.DispOrder div cuiDispOrderInc + 1 ) * cuiDispOrderInc
    else
      pObject.DispOrder := cuiDispOrderInc ;
  end ;
  FList.Add( pObject ) ;
end;

// -----------------------------------------------------------------------------
procedure TPerObjList.Clear;
begin
  FList.Clear ;
  // Should Clear set the ObjectState to posEmpty. Originally we thought yes,
  // then got burnt by side effects, so this was removed 26/09/2001
  //ObjectState := posEmpty ;
end;

// -----------------------------------------------------------------------------
{: Call Delete to remove the object at Index from the list. (The first object is
indexed as 0, the second object is indexed as 1, and so forth.) After an object
is deleted, all the objects that follow it are moved up in index position and
Count is decremented.

To use an object reference (rather than an index position) to specify the object
to be removed, call Remove.

If OwnsObjects is True, Delete frees the object in addition to removing it from
the list. To remove an object from the list without freeing it, call Extract.}
procedure TPerObjList.Delete(i: integer);
begin
  TPerObjAbs( Items[i] ).Owner := nil ;
  FList.Delete( i ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPerObjToText
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPerObjToText.AcceptVisitor : boolean;
begin
  result := ( Visited is TPerObjAbs ) and
            (( not TPerObjAbs( Visited ).Deleted ) or
             ( TPerObjAbs( Visited ).Deleted and IncludeDeleted )) ;
end;

// -----------------------------------------------------------------------------
constructor TVisPerObjToText.Create;
begin
  inherited;
  FbIncludeDeleted := false ;
  FbDataOnly := false ;
end;

// -----------------------------------------------------------------------------
procedure TVisPerObjToText.Execute(const pVisited: TVisitedAbs);
var
  i : integer ;
  lslProps : TStringList ;
  lsValue : string ;
  lsPropName : string ;
begin

  Inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

  Write( Indent + pVisited.ClassName ) ;
  if not FbDataOnly then
  begin
    Write( ', ' ) ;
    Write( TPerObjAbs( pVisited ).ObjectStateAsString ) ;
    Write( ', ' ) ;
    Write( TPerObjAbs( pVisited ).Caption ) ;
    Write( ', ' ) ;
    {$IFDEF OID_AS_INT64}
      Write( IntToStr( TPerObjAbs( pVisited ).OID )) ;
    {$ELSE}
      Write( TPerObjAbs( pVisited ).OID.AsString ) ;
    {$ENDIF}
    if TPerObjAbs( pVisited ).Dirty then
      Write( ', *< Dirty >*' ) ;
  end ;

  WriteLn( '' ) ;

  lslProps := TStringList.Create ;
  try
    tiGetPropertyNames( TPersistent( pVisited ),
                        lslProps,
                        ctkSimple + [tkVariant, tkEnumeration] ) ;

    // dean.millam@duffersgreens.com, modified to format TDateTime
    for i := 0 to lslProps.Count - 1 do
    begin
      lsPropName := lslProps.Strings[i] ;
      if not SameText( lsPropName, 'Caption' ) then
      begin
        try
          lsValue := TPerObjAbs( pVisited ).PropValue[lsPropName];
        except
          on e:exception do
            lsValue := 'Error: ' + e.Message ;
        end ;


{        // Properly format TDateTime types
        if GetPropInfo(pVisited,lsPropName).PropType^.Name = 'TDateTime' then
          lsValue := DateTimeToStr(TypInfo.GetPropValue(pVisited,lsPropName))
        else
          lsValue := GetPropValue( pVisited, lsPropName, true ) ;
}
        lsValue := '  ' +
                   lslProps.Strings[i] +
                   ' = ' +
                   lsValue ;
        WriteLn( Indent + lsValue ) ;
      end ;
    end ;

//    for i := 0 to lslProps.Count - 1 do begin
//      lsPropName := lslProps.Strings[i] ;
//      if not SameText( lsPropName, 'Caption' ) then
//      begin
//        lsValue := GetPropValue( pVisited, lsPropName, true ) ;
//        lsValue := '  ' +
//                   lslProps.Strings[i] +
//                   ' = ' +
//                   lsValue ;
//        WriteLn( Indent + lsValue ) ;
//      end ;
//    end ;
  finally
    lslProps.Free ;
  end ;
{
}


end;

// -----------------------------------------------------------------------------
procedure TPerObjAbs.SetObjectState(const Value: TPerObjectState);
begin
  FObjectState := Value;
end;

// -----------------------------------------------------------------------------
procedure TPerObjList.Empty;
var
  i : integer ;
begin
  for i := Count - 1 downto 0 do
    FList.Extract( FList.Items[i] ) ;
end;

// -----------------------------------------------------------------------------
function TPerObjList.GetCount: integer;
begin
  result := FList.Count ;
end;

// -----------------------------------------------------------------------------
function TPerObjList.GetItems(i: integer): TPerObjAbs ;
begin
  result := TPerObjAbs( FList.Items[ i ] ) ;
end;

// -----------------------------------------------------------------------------
function TPerObjList.GetList: TList;
begin
  result := FList ;
end;

// -----------------------------------------------------------------------------
function TPerObjList.IndexOf(pData: TObject): integer;
begin
  result := FList.IndexOf( pData ) ;
end;

// -----------------------------------------------------------------------------
function TPerObjList.Last: TPerObjAbs;
begin
  // TList can't handle this. Strange!
  if FList.Count > 0 then
// Under some circumstances, this will AV. Why?  
//    result := TPerObjAbs( FList.Last )
    result := TPerObjAbs( FList.Items[FList.Count-1] )
  else
    result := nil ;
end;

// -----------------------------------------------------------------------------
function TPerObjList.LastExcludeDeleted: TPerObjAbs;
var
  i : integer ;
begin
  result := nil ;
  for i := Count-1 downto 0 do
    if not Items[i].Deleted then
    begin
      result := Items[i] ;
      Break ; //==>
    end ;
end;

// -----------------------------------------------------------------------------
procedure TPerObjList.SetItems(i: integer; const Value: TPerObjAbs );
begin
  FList.Items[ i ] := Value ;
end;

// -----------------------------------------------------------------------------
function TPerObjAbs.ObjectStateAsString: string;
begin
  result := GetEnumName( TypeInfo( TPerObjectState ),
                         Ord( ObjectState )) ;
end;

// -----------------------------------------------------------------------------
function TVisPerObjToText.Indent: string;
begin
  result := tiSpace(( Depth - 1 ) * 2) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TPerStream
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TPerStream.Create;
begin
  inherited;
  FStream := nil ;
  Clear ;
end;

// -----------------------------------------------------------------------------
destructor TPerStream.Destroy;
begin
  FStream.Free ;
  inherited;
end;

// -----------------------------------------------------------------------------
procedure TPerStream.Clear;
begin
  if FStream <> nil then
    FStream.Free ;
  FStream := TMemoryStream.Create ;
end;

// -----------------------------------------------------------------------------
function TPerStream.GetSize: integer;
begin
  result := FStream.Size ;
end;

// -----------------------------------------------------------------------------
procedure TPerStream.LoadFromFile(const psFileName: string);
begin
  FStream.LoadFromFile( psFileName ) ;
end;

// -----------------------------------------------------------------------------
procedure TPerStream.SaveToFile( const psFileName: string);
begin
  FStream.Position := 0 ;
  FStream.SaveToFile( psFileName ) ;
end;

// -----------------------------------------------------------------------------
function TPerStream.GetAsString: string;
var
  ls : string ;
begin
  FStream.Seek( 0,soFromBeginning ) ;
  SetString( ls, nil, FStream.Size );
  Stream.Read( Pointer( ls )^, FStream.Size);
  Result := ls ;
end;

// -----------------------------------------------------------------------------
procedure TPerStream.SetAsString(const Value: string);
var
  lpcText : PChar ;
begin
  FStream.Clear ;
  lpcText := PChar( Value ) ;
  FStream.WriteBuffer( lpcText^, length( lpcText )) ;
end;

{: Call Remove to delete a specific object from the list when its index is unknown.
The value returned is the index of the object in the Items array before it was
removed. If the specified object is not found on the list, Remove returns –1.
If OwnsObjects is True, Remove frees the object in addition to removing it from
the list. After an object is deleted, all the objects that follow it are moved
up in index position and Count is decremented. If an object appears more than
once on the list, Remove deletes only the first appearance. Hence, if OwnsObjects
is True, removing an object that appears more than once results in empty object
references later in the list. To use an index position (rather than an object
reference) to specify the object to be removed, call Delete.
To remove an object from the list without freeing it, call Extract.}
function TPerObjList.Remove(pData: TPerObjAbs):integer;
begin
  if FbAutoSetItemOwner then
    pData.Owner := nil ;
  result := FList.Remove( pData ) ;
end;

{:Call Extract to remove an object from the list without freeing the object
itself. After an object is removed, all the objects that follow it are moved up
in index position and Count is decremented.}
procedure TPerObjList.Extract(pData: TPerObjAbs);
begin
  pData.Owner := nil ;
  FList.Extract( pData ) ;
end;

{: Call Insert to add an object at a specified position in the list, shifting the
item that previously occupied that position (and all subsequent items) up. Insert
increments Count and, if necessary, allocates memory by increasing the value of
Capacity.
The Index parameter is zero-based, so the first position in the list has an
index of 0.
To replace a nil reference with a new object without growing the array, set the
Items property directly.}
procedure TPerObjList.Insert(const piIndex: integer; pData: TPerObjAbs);
begin
  FList.Insert( piIndex, pData ) ;
  pData.Owner := self ;
  AssignDispOrder( pData ) ;
end;

{: @TODO AssignDispOrder logic requires work. }
procedure TPerObjList.AssignDispOrder( pData: TPerObjAbs ) ;
var
  i : integer ;
  lBefore : TPerObjAbs ;
  lAfter  : TPerObjAbs ;
begin
  i := IndexOf( pData ) ;
  if i > 0 then
    lBefore := Items[i-1]
  else
    lBefore := nil ;

  if i < Count-1 then
    lAfter := Items[i+1]
  else
    lAfter := nil ;

  if      ( lBefore = nil ) and ( lAfter = nil ) then
  begin
    pData.DispOrder := cuiDispOrderInc ;
  end
  else if ( lBefore <> nil ) and ( lAfter = nil ) then
  begin
    pData.DispOrder := ( lBefore.DispOrder div cuiDispOrderInc + 1 ) * cuiDispOrderInc ;
  end
  else if ( lBefore = nil ) and ( lAfter <> nil ) then
  begin
    pData.DispOrder := ( lAfter.DispOrder div cuiDispOrderInc - 1 ) * cuiDispOrderInc ;
  end
  else if ( lBefore <> nil ) and ( lAfter <> nil ) then
  begin
    pData.DispOrder := ( lBefore.DispOrder + lAfter.DispOrder ) div 2 ;
  end ;
  pData.Dirty := true ;
end ;

procedure TPerObjList.Insert(pInsertBefore, pData: TPerObjAbs);
var
  i : integer ;
begin
  i := FList.IndexOf( pInsertBefore ) ;
  if i >= 0 then
    Insert( i, pData )
  else
  begin
    Add( pData ) ;
    AssignDispOrder( pData ) ;
  end ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TVisPerObjFindByOID
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TVisPerObjFindByOID.AcceptVisitor: boolean;
begin
  result := ( Visited is TPerObjAbs ) and
            ( FFound = nil ) ;
end;

constructor TVisPerObjFindByOID.Create;
begin
  inherited;
  FFound := nil ;
end;

procedure TVisPerObjFindByOID.Execute(const pVisited: TVisitedAbs);
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  if OIDEquals( TPerObjAbs( Visited ).OID ,FOIDToFind ) then
    FFound := TPerObjAbs( Visited ) ;

end;

function TPerObjAbs.Find(pPerObjFindMethod: TPerObjFindMethod): TPerObjAbs;
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

function TPerObjAbs.Find( pPerObjFindMethodExt : TPerObjFindMethodExt; pUserContext: Pointer ) : TPerObjAbs ;
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

function TPerObjAbs.FindAll( pPerObjFindMethod: TPerObjFindMethod ; pList : TList ): integer;
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

function TPerObjAbs.Find( pOIDToFind : TOID ) : TPerObjAbs;
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

procedure TPerObjList.MarkListItemsForDeletion;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    Items[i].Deleted := true ;
end;

procedure TPerObjList.MarkListItemsDirty;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    Items[i].Dirty := true ;
end;

{ TVisPerObjFind }

function TVisPerObjFind.AcceptVisitor: boolean;
begin
  result := ( Visited is TPerObjAbs ) and
            (( FFound = nil ) or ( FFoundList <> nil )) ;
end;

constructor TVisPerObjFind.Create;
begin
  inherited;

end;

procedure TVisPerObjFind.Execute(const pVisited: TVisitedAbs);
var
  lbFound : boolean ;
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>

  lbFound := false ;
  if Assigned(FPerObjFindMethod) then
    FPerObjFindMethod( TPerObjAbs( Visited ), lbFound )
  else if Assigned( FPerObjFindMethodExt ) then
    PerObjFindMethodExt( TPerObjAbs( Visited ), lbFound, FUserContext )
  else if Assigned( FPerObjFindMethodData ) then
    PerObjFindMethodData( TPerObjAbs( Visited ), lbFound, Data )
  else
    tiFmtException( 'No find method assigned', ClassName, 'Execute' ) ;

  if lbFound then
    if FoundList = nil then
      FFound := TPerObjAbs( Visited )
    else
      FoundList.Add( Visited ) ;

end;

function TPerObjAbs.GetOwner: TPerObjAbs;
begin
  //Assert( FOwner <> Nil, 'Owner has not been assigned in ' + ClassName ) ;
  Result := FOwner;
end;

procedure TPerObjAbs.SetOwner(const Value: TPerObjAbs);
begin
  FOwner := Value ;
end;

// By default, IsUnique will check all objects in the collection (from
// the current object down the tree) for uniqueness by OID. Override
// DoFindAllNotUnique to change the properties that are tested.
// -----------------------------------------------------------------------------
function TPerObjAbs.IsUnique(const pPerObjAbs: TPerObjAbs ) : boolean ;
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
      if ( lList.Items[i] <> pPerObjAbs ) {and
         ( lList.Items[i] <> Self )} and
         ( OIDEquals( TPerObjAbs( lList.Items[i] ).OID, pPerObjAbs.OID )) then
      begin
        result := false ;
        Exit ; //==>
      end ;
    end ;
  finally
    lList.Free ;
  end ;
end;

// -----------------------------------------------------------------------------
procedure TPerObjList.ForEach(pMethod: TPerObjForEachMethod; pbIncludeDeleted: boolean=false);
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    if not Items[i].Deleted or pbIncludeDeleted then
      pMethod( Items[i] ) ;
end;

procedure TPerObjList.ForEach(pMethod: TPerObjForEachMethodRegular; pbIncludeDeleted: boolean);
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    if not Items[i].Deleted or pbIncludeDeleted then
      pMethod( Items[i] ) ;
end;

procedure TPerObjList.PropToStrings(const pStrings: TStrings ; const pPropName : string = 'caption' );
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

function TPerObjList.GetCountNotDeleted: integer;
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

procedure TPerObjList.SetItemOwner(const Value: TPerObjAbs);
var
  i : integer ;
begin
  FItemOwner := Value;
  for I := 0 to Count - 1 do
    Items[ I ].Owner := FItemOwner ;
end ;

//------------------------------------------------------------------------------
procedure TPerObjList.AssignClassProps(pSource: TPerObjAbs);
var
  i : integer ;
  lClass : TPerObjAbsClass ;
  lSource : TPerObjAbs ;
  lTarget : TPerObjAbs ;
begin
  Assert( pSource is TPerObjList,
          'pSource not a TPerObjList' ) ;

  if OwnsObjects then
  begin
    for i := 0 to TPerObjList( pSource ).Count - 1 do
    begin
      lSource := TPerObjList( pSource ).Items[i] ;
      lClass := TPerObjAbsClass( lSource.ClassType ) ;
      lTarget  := TPerObjAbs( lClass.Create );
      Add( lTarget ) ;
      lTarget.Assign( lSource ) ;
      if AutoSetItemOwner then
        lTarget.Owner := ItemOwner ;

    end ;
  end
  else
    for i := 0 to TPerObjList( pSource ).Count - 1 do
      Add( TPerObjList( pSource ).Items[i] );
end;

function TPerObjList.GetOwnsObjects: boolean;
begin
  result := FList.OwnsObjects ;
end;

procedure TPerObjList.SetOwnsObjects(const Value: boolean);
begin
  FList.OwnsObjects := Value ;
end;

function TPerObjAbs.GetIndex: integer;
begin
  Assert( Owner <> nil,
          'Owner not assigned' ) ;
  Assert( Owner is TPerObjList,              
          'Owner not a TPerObjList, its a ' + Owner.ClassName ) ;
  result := TPerObjList( Owner ).IndexOf( self ) ;
end;

function TPerObjList.FindByProps( const pProps : array of string ;
                                  const pVals  : array of variant;
                                  pCaseSensitive : boolean = true ): TPerObjAbs;
var
  j: Integer;
  i : integer ;
  lFound : boolean;

  function PropertyMatch(Idx: Integer; PropName: string; PropValue: variant): boolean;
  var
    lSearch, lItem : variant;
    lVarType : TVarType;
  begin
    lSearch := PropValue;
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
    if tiIsVariantOfType(lSearch,varOleStr) and not pCaseSensitive then
      result := SameText(lSearch,lItem)
    else
      result := (lSearch = lItem);
  end;

begin
  Assert( High( pProps ) = High( pVals ),
          'Props and Vals must have the same number of elements' ) ;

  result := nil;
  lFound := False;
  try
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
  except
    on e:exception do
      tiFmtException( e.message, className, 'FindByProps' ) ;
  end ;
end;

procedure TPerStream.SetSize(const Value: integer);
begin
  FStream.Size := Value ;
end;

function TPerObjAbs.TopOfHierarchy: TPerObjAbs;
  function _TopOfHierarchy( Value : TPerObjAbs ) : TPerObjAbs ;
  begin
    if Value.Owner = nil then
      result := Value
    else
      result := _TopOfHierarchy( Value.Owner ) ;
  end ;
begin
  result := _TopOfHierarchy( Self ) ;
end;


function TPerObjAbs.GetOID: TOID;
begin
  // Create OID on demand
  {$IFNDEF OID_AS_INT64}
    if FOID = nil then
      FOID := gTIPerMgr.OIDFactory.CreateOID ;
  {$ENDIF}
  result := FOID ;
end;


constructor TPerObjAbs.CreateNew( const pOwner : TPerObjAbs ; const pDatabaseName : string = '' ; const pPerLayerName : string = '' );
begin
  Create ;
  if pOwner <> nil then
    Owner := pOwner ;
  ObjectState := posCreate ;
  {$IFDEF OID_AS_INT64}
    OID := gTIPerMgr.DefaultPerLayer.NextOIDMgr.NextOID ;
  {$ELSE}
    OID.GetNextValue( pDatabaseName, pPerLayerName ) ;
  {$ENDIF}
end;

constructor TPerObjAbs.CreateNew( const pDatabaseName : string ; const pPerLayerName : string );
begin
  Create ;
  ObjectState := posCreate ;
  {$IFDEF OID_AS_INT64}
    OID := gTIPerMgr.DefaultPerLayer.NextOIDMgr.NextOID ;
  {$ELSE}
    OID.GetNextValue( pDatabaseName, pPerLayerName ) ;
  {$ENDIF}
end;

constructor TPerObjAbs.CreateNew;
begin
  CreateNew(gTIPerMgr.DefaultDBConnectionName, gTIPerMgr.DefaultPerLayerName);
end;

function TPerObjList.First: TPerObjAbs;
begin
  // TList can't handle this. Strange!
  if FList.Count > 0 then
    result := TPerObjAbs( FList.First )
  else
    result := nil ;
end;

function _DoSortByOID( Item1, Item2 : pointer ) : integer ;
begin
  {$IFDEF OID_AS_INT64}
    if TPerObjAbs( Item1 ).OID < TPerObjAbs( Item2 ).OID then
      result := -1
    else if TPerObjAbs( Item1 ).OID > TPerObjAbs( Item2 ).OID then
      result := 1
    else
      result := 0 ;
  {$ELSE}
    result := TPerObjAbs( Item1 ).OID.Compare( TPerObjAbs( Item2 ).OID ) ;
  {$ENDIF}
end ;

procedure TPerObjList.SortByOID;
begin
  List.Sort( _DoSortByOID ) ;
end;

procedure TPerObjList.SortByDispOrder;
begin
  SortByProps(['DispOrder']);
end;

function TPerObjList.DoCompareByProps(
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
      lValue1 := TPerObjAbs( pItem1 ).GetPropValue( lsPropName ) ;
      lValue2 := TPerObjAbs( pItem2 ).GetPropValue( lsPropName ) ;

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
      raise exception.create(
        'Error in TPerObjList._DoCompare PropName <' +
        lsPropName + '>' + e.Message ) ;
  end ;

end ;

// This method was cloned from Classes.TList.Sort and
// was added here to introduce an array of strings to
// hold property names for the sort.
procedure TPerObjList.QuickSortByProps(
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

procedure TPerObjList.SortByProps(const pSortProps: array of string; pAscendingOrder : Boolean = True);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSortByProps(FList.List, 0, Count - 1, pSortProps, pAscendingOrder);
end;

procedure TPerObjAbs.Read(const pDBConnectionName: string ; pPerLayerName : string = '' );
begin
  gTIPerMgr.Read( Self, pDBConnectionName, pPerlayerName ) ;
end;

procedure TPerObjAbs.Read;
begin
  Read( '', '' ) ;
end;

procedure TPerObjAbs.ReadPK(const pDBConnectionName: string ; pPerLayerName : string = '' );
begin
  gTIPerMgr.ReadPK( Self, pDBConnectionName, pPerLayerName ) ;
end;

procedure TPerObjAbs.ReadPK;
begin
  ReadPK( '', '' ) ;
end;

procedure TPerObjAbs.Save(const pDBConnectionName: string ; pPerLayerName : string = '' );
begin
  gTIPerMgr.Save( Self, pDBConnectionName, pPerLayerName ) ;
end;

procedure TPerObjAbs.Save;
begin
  Save( '', '' ) ;
end;

{ TPerObjThreadList }

procedure TPerObjThreadList.Add(pObject: TPerObjAbs; pDefDispOrdr: boolean);
begin
  InternalLock( false ) ;
  try
    inherited Add( pObject, pDefDispOrdr ) ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

procedure TPerObjThreadList.Clear;
begin
  InternalLock( false ) ;
  try
    inherited Clear ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

constructor TPerObjThreadList.Create;
var
  lsSemaphoreName : string ;
  {$IFDEF LINUX}
  error: integer;
  {$ENDIF LINUX}
begin
  inherited;
  lsSemaphoreName := ClassName ;
  {$IFDEF MSWINDOWS}
  FSemaphore := CreateSemaphore( nil, 1, 1, PChar( lsSemaphoreName )) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_init(FSemaphore, false, 0);
  if error <> 0 then
    raise Exception.Create('Failed to create the semaphore');
  {$ENDIF LINUX}

  FRaiseLockException := False;
  FLockCount := 0 ;
end;

procedure TPerObjThreadList.Delete(i: integer);
begin
  InternalLock( false ) ;
  try
    inherited Delete( i ) ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

destructor TPerObjThreadList.Destroy;
  {$IFDEF LINUX}
var
  error: integer;
  {$ENDIF LINUX}
begin
  {$IFDEF MSWINDOWS}
  CloseHandle( FSemaphore ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_destroy( FSemaphore );
  if error <> 0 then
    raise Exception.Create('Failed to destroy the semaphore');
  {$ENDIF LINUX}
  inherited;
end;

procedure TPerObjThreadList.Empty;
begin
  InternalLock( false ) ;
  try
    inherited Empty ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

function TPerObjThreadList.First: TPerObjAbs;
begin
  InternalLock( false ) ;
  try
    result := inherited First ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

function TPerObjThreadList.GetCount: integer;
begin
  InternalLock( false ) ;
  try
    result := inherited GetCount ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

function TPerObjThreadList.IndexOf(pData: TObject): integer;
begin
  InternalLock( false ) ;
  try
    result := inherited IndexOf( pData ) ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

function TPerObjThreadList.Last: TPerObjAbs;
begin
  InternalLock( false ) ;
  try
    result := inherited Last ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

function TPerObjThreadList.LastExcludeDeleted: TPerObjAbs;
begin
  InternalLock( false ) ;
  try
    result := inherited LastExcludeDeleted ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

procedure TPerObjThreadList.Lock;
begin
  //LogArray([ 'Lock', FLockCount ]);
  InternalLock( true ) ;
end;

procedure TPerObjThreadList.MarkListItemsDirty;
begin
  InternalLock( false ) ;
  try
    inherited MarkListItemsDirty ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

procedure TPerObjThreadList.MarkListItemsForDeletion;
begin
  InternalLock( false ) ;
  try
    inherited MarkListItemsForDeletion ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

function TPerObjThreadList.Remove(pData: TPerObjAbs): integer;
begin
  InternalLock( false ) ;
  try
    result := inherited Remove( pData ) ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

procedure TPerObjThreadList.SetItemOwner(const Value: TPerObjAbs);
begin
  InternalLock( false ) ;
  try
    inherited SetItemOwner( Value ) ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

procedure TPerObjThreadList.SetItems(i: integer; const Value: TPerObjAbs);
begin
  InternalLock( false ) ;
  try
    inherited SetItems( i, Value ) ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

procedure TPerObjThreadList.SortByProps(const pSortProps: array of string; pAscendingOrder : Boolean = True);
begin
  InternalLock( false ) ;
  try
    inherited SortByProps( pSortProps, pAscendingOrder ) ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

procedure TPerObjThreadList.SortByOID;
begin
  InternalLock( false ) ;
  try
    inherited SortByOID ;
  finally
    InternalUnLock( false ) ;
  end ;
end;

procedure TPerObjThreadList.UnLock;
begin
  //LogArray([ 'UnLock', FLockCount ]);
  InternalUnLock( true ) ;
end;

procedure TPerObjThreadList.InternalLock( pExternalLock : boolean ) ;
begin
  //LogArray([ 'InternalLock', pExternalLock, FLockCount ]);
  if ( not pExternalLock ) and
     ( FLockCount > 0 ) then
  begin
    Inc( FLockCount ) ;
    Exit ; //==>
  end ;

  {$IFDEF MSWINDOWS}
  if WaitForSingleObject( FSemaphore, 60000 ) = WAIT_TIMEOUT then
  begin
    If FRaiseLockException Then
      Raise Exception.CreateFmt('Timed out waiting to lock %s.', [ClassName])
    Else
    Begin
      LogError( 'Timed out waiting to lock ' + ClassName ) ;
      exit ; //==>
    End;
  end ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  // no need to test return value, it always returns 0
  sem_wait( FSemaphore );
  {$ENDIF LINUX}
  Inc( FLockCount ) ;

end;

procedure TPerObjThreadList.InternalUnLock( pExternalLock : boolean ) ;
{$IFDEF LINUX}
var
  error: integer;
{$ENDIF LINUX}
begin
  //.LogArray([ 'InternalUnLock', pExternalLock, FLockCount ]);
  if ( not pExternalLock ) and
     ( FLockCount > 1 ) then
  begin
    Dec( FLockCount ) ;
    Exit ; //==>
  end ;

  {$IFDEF MSWINDOWS}
  ReleaseSemaphore( FSemaphore, 1, nil ) ;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  error := sem_post( FSemaphore );
  if error <> 0 then
    raise Exception.Create('Failed to unlock the semaphore');
  {$ENDIF LINUX}
  Dec( FLockCount ) ;

end;

{ TPerObjFactory }

function TPerObjFactory.Count: integer;
begin
  result := FList.Count ;
end;

constructor TPerObjFactory.create;
begin
  inherited ;
  FList := TObjectList.Create ;
end;

function TPerObjFactory.CreateInstance( const pClassName: string): TPerObjAbs;
var
  lPerObjClassMapping : TPerObjClassMapping ;
begin
  lPerObjClassMapping := FindByClassName( pClassName ) ;
  Assert( lPerObjClassMapping <> nil,
          'Request for unrgister class <' +
          pClassName + '>' ) ;
  result := lPerObjClassMapping.PerObjAbsClass.Create;
end;

function TPerObjFactory.CreateNewInstance(const pClassName: string; pOwner : TPerObjAbs = nil ): TPerObjAbs;
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

procedure TPerObjFactory.RegisterClass(const pClassName: string ; pClass : TPerObjAbsClass);
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


procedure TPerObjList.Assign(pSource: TPerObjAbs);
begin
  Clear ;
  inherited Assign( pSource ) ;
end;

function TPerObjAbs.GetDispOrder: integer;
begin
  Result := FiDispOrder;
end;

procedure TPerObjAbs.SetAllObjectStates(const pObjectState: TPerObjectState);
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
  result := ( Visited is TPerObjAbs ) and
            ( TPerObjAbs( Visited ).ObjectState in [ posClean, posUpdate ]) ;
end;

procedure TVisSetAllObjectStates.Execute(const pVisited: TVisitedAbs);
begin
  Inherited Execute( pVisited ) ;

  if not AcceptVisitor then
    exit ; //==>

  TPerObjAbs( Visited ).ObjectState := FObjectState ;

end;          

procedure TPerObjAbs.ReadThis(const pDBConnectionName: string ; pPerLayerName : string = '' );
begin
  {$IFDEF OID_AS_INT64}
    if OID = cNullOID then
  {$ELSE}
    if OID.IsNull then
  {$ENDIF}
    tiFmtException( 'OID not assigned on ' + ClassName +
                    ' when ReadThis was called.',
                    ClassName, 'ReadThis' );
  gTIPerMgr.ReadThis( Self, pDBConnectionName, pPerLayerName ) ;
end;

procedure TPerObjAbs.ReadThis;
begin
  ReadThis( '', '' ) ;
end;

procedure TPerObjList.AssignPublicProps(pSource: TPerObjAbs);
begin
  inherited AssignPublicProps(pSource) ;
  
// Don't set these here, they will be set in a classes constructor  
//  OwnsObjects := TPerObjList(pSource).OwnsObjects;
//  AutoSetItemOwner := TPerObjList(pSource).AutoSetItemOwner;
end;

function TPerObjList.Find(pOIDToFind: TOID): TPerObjAbs;
var
  i : integer ;
begin
  for i := 0 to Count - 1 do
    if OIDEquals( Items[i].OID, pOIDToFind ) then
    begin
      result := Items[i];
      Exit ; //==>
    end ;
  result := ( Inherited Find( pOIDToFind )) ;
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

function TPerObjAbs.IsValid(const pErrors: TPerObjErrors): boolean;
begin
  Assert( pErrors <> nil, 'pErrors not assigned' ) ;
  result := true ;
  pErrors.Clear ;
end;

function TPerObjAbs.IsValid(var pErrorMessage: string): boolean;
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

function TPerObjAbs.IsValid: boolean;
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

function TPerObjAbs.IsValid(const pStrings: TStrings): boolean;
var
  lMessage : string ;
begin
  result := IsValid( lMessage ) ;
  pStrings.Text := lMessage ;
end;

function TPerObjAbs.IsValid( const pStrings : TStrings ; pAppend : boolean ) : boolean ; 
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

destructor TPerObjAbs.Destroy;
begin
  {$IFNDEF OID_AS_INT64}
    FOID.Free ;
  {$ENDIF}
  inherited;
end;

function TPerObjAbs.Find(pOIDToFindAsString: string): TPerObjAbs;
{$IFNDEF OID_AS_INT64}
  var
    lOID : TOID ;
{$ENDIF}
begin
  {$IFDEF OID_AS_INT64}
    result := Find( StrToInt( pOIDToFindAsString )) ;
  {$ELSE}
    lOID := gTIPerMgr.OIDFactory.CreateOID ;
    try
      lOID.AsString := pOIDToFindAsString ;
     result := Find( lOID ) ;
    finally
      lOID.Free ;
    end ;
  {$ENDIF}
end;

function TPerObjList.Find(pOIDToFindAsString: string): TPerObjAbs;
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
    result := ( Inherited Find( pOIDToFindAsString )) ;
  {$ELSE}
    lOIDToFind := gTIPerMgr.OIDFactory.CreateOID ;
    try
      lOIDToFind.AsString := pOIDToFindAsString ;
      for i := 0 to Count - 1 do
        if Items[i].OID.Equals( lOIDToFind ) then
        begin
          result := Items[i];
          Exit ; //==>
        end ;
      result := ( Inherited Find( lOIDToFind )) ;
    finally
      lOIDToFind.Free ;
    end ;
  {$ENDIF}
end;

function TPerObjAbs.FindAll(pPerObjFindMethodExt: TPerObjFindMethodExt; pList: TList; pUserContext: Pointer ): integer;
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

procedure TPerObjAbs.DoFindAllNotUnique(pPerObjAbs: TPerObjAbs;
  var pbFound: boolean; pData : TPerObjAbs);
begin
  Assert( Assigned( pData ), 'pData not assigned' ) ;
  pbFound :=
    ( OIDEquals( pPerObjAbs.OID, pData.OID )) and
    ( pPerObjAbs <> pData ) and
    ( not pPerObjAbs.Deleted ) ;
end;

function TPerObjAbs.FindAll(pPerObjFindMethodData: TPerObjFindMethodData;
  pList: TList; pData: TPerObjAbs): integer;
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

function TPerObjAbs.GetPropValue(const pPropName: string): Variant;
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
  else if SameText( pPropName, 'DispOrder' ) then
    result := DispOrder
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
        tiFmtException( e,
                        'Error getting property value for <' + pPropName + '> on <' + ClassName + '>',
                        ClassName, 'DoGetPropValue' ) ;
    end ;
  end ;
end;

procedure TPerObjAbs.SetPropValue(const pPropName: string; const pPropValue: Variant);
var
  ldtValue : TDateTime ;
  lsValue : string ;
  liValue : integer ;
  lbValue : boolean ;
  lPropTypeKind : TtiTypeKind ;
  lValueTypeKind : TtiTypeKind ;
begin

  // OID is a special case. Should we publishe OID?
  if SameText( pPropName, 'OID' ) then
  begin
    {$IFDEF OID_AS_INT64}
      OID := Integer(pPropValue) ;
    {$ELSE}
      OID.AsVariant := pPropValue ;
    {$ENDIF}
    Exit ; //==>
  end ;

  if SameText( pPropName, 'DispOrder' ) then
  begin
    DispOrder := pPropValue ;
    Exit ; //==>
  end ;

  if not IsPublishedProp( Self, pPropName ) then
    tiFmtException( 'Attempting to set non-published property <' +
                    pPropName + '> to <' + VarToStr(pPropValue) + '>',
                    ClassName, 'MapRowToObject' ) ;
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
                        tiFmtException( 'pPropValue of unknown variant type', ClassName, 'SetPropValue' ) ;
                    end
                  end ;
    else
      TypInfo.SetPropValue( Self, pPropName, pPropValue ) ;
    end ;

  except
    on e:exception do
      tiFmtException( e,
                      'Error setting property <' +
                      pPropName +
                      '> on <' + ClassName + '> to <' +
                      VarToStr(pPropValue) + '>',
                      ClassName, 'MapRowToObject'  ) ;
  end ;
end;

function TPerObjAbs.IsReadWriteProp(const pPropName: string): boolean;
begin
  result :=
    SameText( pPropName, 'OID' ) or
    SameText( pPropName, 'DispOrder' ) or
    tiIsReadWriteProp( Self, pPropName ) ;   
end;

{ TPerObjClassMapping }

constructor TPerObjClassMapping.Create;
begin
  inherited ;
end;

end.
