unit tiVisitor ;

{$I tiDefines.inc}

interface
uses
  Classes
  ,tiBaseObject
  ,TypInfo
  ,tiStreams
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF MSWINDOWS}
  ,Controls   // For Cusror - watch for GUI dependencies
  ,SyncObjs   // This unit must always appear after the Windows unit!
  ;

const
  cErrorInVisitorExecute = 'Error in %s.Execute(%s) Message: %s';

const

  // Type kinds for use with tiGetPropertyNames
  // All string type properties
  ctkString = [ tkChar, tkString, tkWChar, tkLString, tkWString {$IFDEF FPC},tkAString{$ENDIF} ] ;
  // Integer type properties
  ctkInt    = [ tkInteger, tkInt64 {$IFDEF FPC},tkBool{$ENDIF}] ;
  // Float type properties
  ctkFloat  = [ tkFloat ] ;
  // Numeric type properties
  ctkNumeric = [tkInteger, tkInt64, tkFloat];
  // All simple types (string, int, float)
  ctkSimple = ctkString + ctkInt + ctkFloat ;

  // These are the leftovers
  // tkUnknown
  // tkClass, tkMethod,
  // tkEnumeration, tkSet, tkVariant, tkArray, tkRecord, tkInterface, tkDynArray

  // These are all the possibilities
  // tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat,
  // tkString, tkSet, tkClass, tkMethod, tkWChar, tkLString, tkWString,
  // tkVariant, tkArray, tkRecord, tkInterface, tkInt64, tkDynArray);

  cErrorInvalidTtiTypeKind = 'Invalid TtiTypeKind' ;

type
  // Simple TypeKinds, as summary of the TTypeKinds available in TypInfo
  TtiTypeKind =  ( tiTKInteger, tiTKFloat , tiTKString, tiTKDateTime, tiTKBoolean, tiTKBinary ) ;

  // Convert a property from Delphi's TTypeKind to TtiSimpleTypeKind
  // EG: Change tkInteger, tkInt64 and tkEnumeration to tkInteger
  function tiGetSimplePropType( const pPersistent : TtiBaseObject ; const psPropName : string ) : TtiTypeKind ;
  function tiVarSimplePropType( pValue : Variant ) : TtiTypeKind ;

  // Is this a numeric property ?
  function tiIsNumericProp( pPersistent : TtiBaseObject ; psPropName : string ) : boolean ;

  // Read a TtiBaseObject's published properties into a TStringList
  procedure tiGetPropertyNames( pPersistent : TtiBaseObject ;
                                pSL : TStringList ;
                                pPropFilter : TTypeKinds = ctkSimple ) ; overload ;

  procedure tiGetPropertyNames( pPersistent : TtiBaseObjectClass ;
                                pSL : TStringList ;
                                pPropFilter : TTypeKinds = ctkSimple ) ; overload ;

  // Is a property a read & write property
  function tiIsReadWriteProp( const pData : TtiBaseObject ; const psPropName : string ) : boolean ; overload ;
  function tiIsReadWriteProp( const pData : TtiBaseObjectClass ; const psPropName : string ) : boolean ; overload ;

type
  {$M+}
  TtiVisited = class ;
  {$M-}
  TtiVisitor = class ;
                       
  TtiVisitorCtrlr = class( TtiBaseObject )
  private
    FDBConnectionName: string;
    FSQLMgrDataSource: string;
    FPerLayerName    : string ;
  protected
    procedure SetPerLayerName(const Value: string); virtual ;
  public
    constructor Create ; virtual ;
    procedure BeforeExecuteAll( pVisitors : TList )      ; virtual ;
    procedure BeforeExecuteOne( pVisitor : TtiVisitor ) ; virtual ;
    // Visitors are executed here...
    procedure AfterExecuteOne( pVisitor : TtiVisitor  ) ; virtual ;
    procedure AfterExecuteAll( pVisitors : TList )       ; virtual ;
    // Executed if there was an error
    procedure AfterExecuteError( pVisitors : TList )     ; virtual ;
    // The property DBConnectionName is really only required in DBVisitors, but
    // must be introduce here so it can be set at a generic level by the
    // VisitorMgr. The alternative is to use RTTI or TypeInfo and only set the
    // property on DBVisitorMgr(s), but that would be an ever worse hack.
    property  PerLayerName     : string read FPerLayerName     write SetPerLayerName ;
    property  DBConnectionName : string read FDBConnectionName write FDBConnectionName ;
    // ToDo: Remove SQLMgrDataSource from TVisitorController
    property  SQLMgrDataSource : string read FSQLMgrDataSource write FSQLMgrDataSource ;
  end ;

  TtiVisitorControllerClass = class of TtiVisitorCtrlr ;

  TtiVisitorIterateDirection = ( vidTopDown, vidBottomUp ) ;

  // TtiVisitor: The class that does the visiting
  TtiVisitor = class( TtiBaseObject )
  private
    FVisited           : TtiVisited ;
    FbContinueVisiting : boolean;
    FVisitorController : TtiVisitorCtrlr;
    FiDepth: integer;
    FIterateDirection: TtiVisitorIterateDirection;
    FVisitedsOwner: TtiVisited;
  protected
    function    AcceptVisitor : boolean ; overload ; virtual ;
    function    GetVisited: TtiVisited ; virtual ;
    procedure   SetVisited(const Value: TtiVisited); virtual ;
  public
    constructor Create ; virtual;

    procedure   Execute( const pVisited : TtiVisited ) ; virtual ;
    function    VisitorControllerClass : TtiVisitorControllerClass ; virtual ;

    property    Visited : TtiVisited read GetVisited write SetVisited ;
    property    ContinueVisiting : boolean read FbContinueVisiting write FbContinueVisiting ;
    property    VisitorController : TtiVisitorCtrlr read FVisitorController write FVisitorController ;
    property    Depth : integer read FiDepth write FiDepth ;
    property    IterateDirection : TtiVisitorIterateDirection
                  read  FIterateDirection
                  write FIterateDirection ;
    property    VisitedsOwner : TtiVisited read FVisitedsOwner write FVisitedsOwner ;
  end ;

  TtiVisGetAllToVisit = class( TtiVisitor )
  private
    FList : TList ;
    FVisitor : TtiVisitor ;
  protected
    function AcceptVisitor : boolean ; override ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
    property    Visitor : TtiVisitor read FVisitor write FVisitor ;
    property    List : TList read FList ;
  end ;

  // TVisitorClass reference
  TtiVisitorClass = class of TtiVisitor ;

  // TtiVisited class reference
  TtiVisitedClass = class of TtiVisited ;

  // TtiVisited
  // The class that gets visited.
  TtiVisited = class( TtiBaseObject )
  private
    FbSelfIterate: boolean;
  protected
    function    GetCaption : string ; virtual ;
  published
    property    Caption    : string  read GetCaption ;
  public
    constructor Create ; virtual ;
    procedure   Iterate( pVisitor : TtiVisitor ) ; virtual ;
    procedure   IterateBottomUp( pVisitor: TtiVisitor ) ; virtual ;
    property    SelfIterate : boolean read FbSelfIterate write FbSelfIterate ;
    procedure   FindAllByClassType( pClass : TtiVisitedClass ; pList : TList ) ;
    function    CountByClass( pClass : TtiVisitedClass ) : integer ;
  end ;

  // A wrapper for the TtiPreSizedStream which allows text to be written to the stream
  // with each visit.
  TVisStream = class( TtiVisitor )
  private
    FStream : TtiPreSizedStream ;
  protected
    procedure Write( const pValue : string ) ; virtual ;
    procedure WriteLn( const pValue : string = '' ) ; virtual ;
    procedure SetStream(const Value: TtiPreSizedStream) ; virtual ;
  public
    property  Stream : TtiPreSizedStream read FStream write SetStream ;
  end ;

  TVisStringStream = class( TVisStream )
  protected
    function    GetText: string; virtual;
  public
    Constructor Create ; override ;
    Destructor  Destroy ; override ;
    Property    Text : string read GetText ;
  end ;

  // A visitor to count the number of instances of each class owned by the
  // passed object
  TVisClassCount = class( TtiVisitor )
  private
    FList: TStringList;
    function GetClassCount(pClass : TClass): integer;
    procedure SetClassCount(pClass : TClass; const Value: integer);
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
    property    ClassCount[ pClass : TClass]: integer
                  read GetClassCount
                  write SetClassCount ;
  end ;

  // A visitor to find all owned objects of a given class
  TVisFindAllByClass = class( TtiVisitor )
  private
    FList: TList;
    FClassTypeToFind: TtiVisitedClass;
  protected
    function    AcceptVisitor : boolean ; override ;
  public
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
    property    ClassTypeToFind : TtiVisitedClass read FClassTypeToFind write FClassTypeToFind ;
    property    List : TList read FList write FList ;
  end ;

  TVisStreamClass = class of TVisStream ;

  TVisClassRef = class of TtiVisitor ;

  TVisMapping = class( TObject )
  private
    FsGroupName : string;
    FClassRef   : TVisClassRef;
  public
    constructor CreateExt( const psGroupName : string ;
                           const pClassRef : TVisClassRef ) ;
    property    GroupName : string read FsGroupName write FsGroupName ;
    property    ClassRef  : TVisClassRef read FClassRef write FClassRef ;
  end ;

  // A procedural type to define the signature used for
  // BeforeExecute, AfterExecute and AfterExecuteError
  TProcessVisitorMgrs = procedure( pVisitorController : TtiVisitorCtrlr ;
                                   pVisitors   : TList ) of object ;

  // The Visitor Manager
  TtiVisitorManager = class( TtiBaseObject )
  private
    FVisMappings : TStringList ;
    FHourGlassCount : integer ;
    FCritSec: TCriticalSection;
    FThreadIDList : TList ;
    FBreakOnException: boolean;
    FSavedCursor : TCursor ;
    procedure GetVisitors(       pVisitors : TList ; const psGroupName : string ) ;
    procedure GetVisitorControllers( const pVisitors         : TList ;
                                     const pVisitorMgrs      : TList ;
                                     const pDBConnectionName : string ;
                                     const pPerLayerName     : string ) ;
    procedure ProcessVisitorControllers(
        pVisitors, pVisitorControllers : TList ;
        pProc : TProcessVisitorMgrs ;
        psMethodName : string ) ;
    // These call ProcessVisitorMgrs to scan for visitors and visitorMgrs
    // by passing the appropriate method of VisitorMgr to execute.
    procedure DoBeforeExecute(     pVisitorMgr : TtiVisitorCtrlr ; pVisitors   : TList ) ;
    procedure DoAfterExecute(      pVisitorMgr : TtiVisitorCtrlr ; pVisitors   : TList ) ;
    procedure DoAfterExecuteError( pVisitorMgr : TtiVisitorCtrlr ; pVisitors   : TList ) ;
    procedure ExecuteVisitors(   pVisitors    : TList ; pVisited : TtiVisited ) ;
    procedure ProcessVisitors( const pGroupName : string ;
                               const pVisited : TtiVisited ;
                               const pDBConnectionName : string;
                               const pPerLayerName      : string) ;
    procedure Lock ;
    procedure UnLock ;
    procedure AddThreadID(    pThreadID : LongWord ) ;
    procedure RemoveThreadID( pThreadID : LongWord ) ;
    function  GetThreadCount : LongWord ;
  public
    constructor Create ; virtual ;
    destructor  Destroy ; override ;
    procedure   RegisterVisitor( const psGroupName : string ;
                                 const pClassRef   : TVisClassRef ) ;
    procedure   UnRegisterVisitors( const psGroupName : string ) ;
    function    Execute( const psGroupName       : string ;
                         const pVisited          : TtiVisited ;
                         const pDBConnectionName : string = '' ;
                         const pPerLayerName     : string = '' ) : string ;
    property    ThreadCount : LongWord read GetThreadCount ;
    property    BreakOnException : boolean read FBreakOnException write FBreakOnException ;
  end ;


// Global proc to write a apply a TVisStream (as a TFileStream) to a TtiVisited.
procedure VisStreamToFile( pData        : TtiVisited ;
                           pFileName   : string ;
                           pVisClassRef : TtiVisitorClass );


implementation
uses
   tiLog     // Logging
  ,tiOPFManager
  ,tiConstants
  ,tiPersistenceLayers
  ,tiExcept
  ,SysUtils  // Exception
  ,Contnrs
  ,tiUtils
  ,Forms     // ToDo: Application.MainForm - UI dependency :(
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
  ;


procedure VisStreamToFile( pData : TtiVisited ;
                           pFileName : string ;
                           pVisClassRef : TtiVisitorClass );
var
  lVisitor : TVisStream ;
  lStream  : TtiPreSizedStream ;
  lDir     : string ;
begin
  lDir := ExtractFilePath( pFileName ) ;
  tiForceDirectories(pFileName);
  lStream := TtiPreSizedStream.Create( cStreamStartSize, cStreamGrowBy );
  try
    lVisitor   := TVisStream( pVisClassRef.Create ) ;
    try
      lVisitor.Stream := lStream ;
      pData.Iterate( lVisitor ) ;
    finally
      lVisitor.Free ;
    end ;
    lStream.SaveToFile(pFileName);
  finally
     lStream.Free ;
  end ;
end ;


{ TtiVisited }

function TtiVisited.CountByClass(pClass: TtiVisitedClass): integer;
var
  lList : TList ;
begin
  lList := TList.Create ;
  try
    FindAllByClassType( pClass, lList ) ;
    result := lList.Count ;
  finally
    lList.Free ;
  end ;
end;


constructor TtiVisited.Create ;
begin
  inherited create ;
  FbSelfIterate := true ;
end;


procedure TtiVisited.FindAllByClassType(pClass: TtiVisitedClass; pList: TList);
var
  lVis : TVisFindAllByClass ;
begin
  Assert( pList <> nil, 'pList not assigned' ) ;
  pList.Clear ;
  lVis := TVisFindAllByClass.Create ;
  try
    lVis.ClassTypeToFind := pClass ;
    lVis.List := pList ;
    Iterate( lVis ) ;
  finally
    lVis.Free ;
  end ;
end;


function TtiVisited.GetCaption: string;
begin
  result := className ;
end;


procedure TtiVisited.Iterate(pVisitor: TtiVisitor) ;
var
  lClassPropNames : TStringList ;
  i        : integer ;
  j        : integer ;
  lVisited : TObject ;
  lVisitedsOwner : TtiVisited ;
begin
  Assert( pVisitor <> nil, 'Visitor unassigned' ) ;

  // Don't go any further if terminated
  if gTIOPFManager.Terminated then
    Exit ; //==>

  if not pVisitor.ContinueVisiting then
    Exit ; //==>

  pVisitor.Depth := pVisitor.Depth + 1 ;
  try

    pVisitor.Execute( self ) ;
    lVisitedsOwner := pVisitor.VisitedsOwner ;
    pVisitor.VisitedsOwner := Self ;

    // If SelfIterate is true, then use RTTI to scan through all the
    // properties of type TtiVisited
    if SelfIterate and
       ( not gTIOPFManager.Terminated ) then
    begin
      // Create a string list to hold the property names
      lClassPropNames := TStringList.Create ;
      try
        // Get all property names of type tkClass
        tiGetPropertyNames( self, lClassPropNames, [tkClass] ) ;

        // Scan through these properties
        for i := 0 to lClassPropNames.Count - 1 do
        begin

          // Get a pointer to the property
          lVisited := GetObjectProp( self, lClassPropNames.Strings[i] ) ;

          // If the property is a TtiVisited, then visit it.
          if ( lVisited is TtiVisited ) and
             ( pVisitor.ContinueVisiting ) and
             ( not gTIOPFManager.Terminated ) then
          begin
            TtiVisited( lVisited ).Iterate( pVisitor ) ;
            continue ; //==>
          end ;

          // If the property is a TList, then visit it's items
          if (lVisited is TList ) then
          begin
            for j := 0 to TList( lVisited ).Count - 1 do
              if ( TObject( TList( lVisited ).Items[j] ) is TtiVisited ) and
                 ( pVisitor.ContinueVisiting ) and
                 ( not gTIOPFManager.Terminated ) then
              begin
                TtiVisited( TList( lVisited ).Items[j] ).Iterate( pVisitor ) ;
              end ;
            continue ; //==>
          end ;

        end ;
        pVisitor.VisitedsOwner := lVisitedsOwner ;
      finally
        lClassPropNames.Free ;
      end ;
    end ;

  finally
    pVisitor.Depth := pVisitor.Depth - 1 ;
  end ;
end ;


{ TtiVisitor }

constructor TtiVisitor.Create;
begin
  inherited create ;
  FbContinueVisiting  := true ;
  FVisitorController  := nil ;
  FiDepth             := 0 ;
  FIterateDirection   := vidTopDown ;
end;


function TtiVisitor.AcceptVisitor: boolean;
begin
  result := true ;
end;


procedure TVisStream.SetStream(const Value: TtiPreSizedStream);
begin
  Assert( Value.TestValid(TtiPreSizedStream), cTIInvalidObjectError );
  FStream := Value;
end;


procedure TVisStream.Write(const pValue: string);
begin
  Assert( FStream.TestValid(TtiPreSizedStream), cTIInvalidObjectError );
  FStream.Write(pValue);
end;


procedure TVisStream.WriteLn(const pValue: string = '' );
begin
  Assert( FStream.TestValid(TtiPreSizedStream), cTIInvalidObjectError );
  FStream.WriteLn(pValue);
end ;


procedure TtiVisitor.Execute( const pVisited: TtiVisited);
begin
  FVisited := pVisited ;
end;


function TtiVisitor.VisitorControllerClass : TtiVisitorControllerClass ;
begin
  result := TtiVisitorCtrlr ;
end;


{ TtiVisitorCtrlr }

procedure TtiVisitorCtrlr.AfterExecuteAll( pVisitors : TList );
begin
  // Do nothing
end;


procedure TtiVisitorCtrlr.AfterExecuteError( pVisitors : TList );
begin
  // Do nothing
end;


procedure TtiVisitorCtrlr.AfterExecuteOne(pVisitor : TtiVisitor);
begin
  // Do nothing
end;


procedure TtiVisitorCtrlr.BeforeExecuteAll( pVisitors : TList );
begin
  // Do nothing
end;


procedure TtiVisitorCtrlr.BeforeExecuteOne(pVisitor : TtiVisitor);
begin
  // Do nothing
end;


constructor TtiVisitorCtrlr.Create;
begin
  // So we can create an instance ot TVisitorMgr from a class reference var.
  inherited ;
end;


function TtiVisitor.GetVisited: TtiVisited;
begin
  result := FVisited ;
end;


procedure TtiVisitor.SetVisited(const Value: TtiVisited);
begin
  FVisited := Value ;
end;


{ TVisClassCount }

constructor TVisClassCount.Create;
begin
  inherited;
  FList := TStringList.Create;
end;


destructor TVisClassCount.Destroy;
begin
  FList.Free;
  inherited;
end;


procedure TVisClassCount.Execute(const pVisited: TtiVisited);
begin
  inherited Execute(pVisited);
  ClassCount[ pVisited.ClassType ] := ClassCount[ pVisited.ClassType ] + 1 ;
end;


function TVisClassCount.GetClassCount(pClass : TClass): integer;
begin
  Result := StrToIntDef( FList.Values[ pClass.ClassName ], 0 ) ;
end;


procedure TVisClassCount.SetClassCount(pClass : TClass; const Value: integer);
begin
  FList.Values[ pClass.ClassName ] := IntToStr( value ) ;
end;


{ TVisStringStream }

constructor TVisStringStream.Create;
begin
  inherited;
  Stream := TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
end;


destructor TVisStringStream.Destroy;
begin
  Stream.Free ;
  inherited;
end;


function TVisStringStream.GetText: string;
begin
  result := FStream.AsString;
end;


procedure TtiVisited.IterateBottomUp(pVisitor: TtiVisitor);
var
  lVisitor : TtiVisGetAllToVisit ;
  i : integer ;
begin
  lVisitor := TtiVisGetAllToVisit.Create ;
  try
    lVisitor.Visitor := pVisitor ;
    Self.Iterate( lVisitor ) ;
    for i := lVisitor.List.Count - 1 downto 0 do
      pVisitor.Execute( TtiVisited( lVisitor.List.Items[i] )) ;
  finally
    lVisitor.Free ;
  end ;
end;


{ TtiVisGetAllToVisit }

function TtiVisGetAllToVisit.AcceptVisitor: boolean;
begin
  result := FVisitor.AcceptVisitor ;
end;


constructor TtiVisGetAllToVisit.Create;
begin
  inherited;
  FList := TList.Create ;
end;


destructor TtiVisGetAllToVisit.Destroy;
begin
  FList.Free ;
  inherited;
end;


procedure TtiVisGetAllToVisit.Execute(const pVisited: TtiVisited);
begin
  inherited Execute( pVisited ) ;
  FVisitor.Visited := pVisited ;
  if AcceptVisitor then
    List.Add( pVisited ) ;
end;


{ TVisFindAllByClass }

function TVisFindAllByClass.AcceptVisitor: boolean;
begin
  result := Visited is FClassTypeToFind ;
end;


procedure TVisFindAllByClass.Execute(const pVisited: TtiVisited);
begin
  inherited Execute( pVisited ) ;
  if not AcceptVisitor then
    Exit ; //==>
  FList.Add( pVisited ) ;
end;


procedure TtiVisitorCtrlr.SetPerLayerName(const Value: string);
begin
  FPerLayerName := Value ;
end;


{ TtiVisitorManager }

procedure TtiVisitorManager.AddThreadID(pThreadID: LongWord);
begin
  if ( pThreadID = MainThreadID ) then
    Exit ; //==>
  Lock ;
  try
    if FThreadIDList.IndexOf( TObject( pThreadID )) = -1 then
      FThreadIDList.Add( TObject( pThreadID )) ;
  finally
    UnLock ;
  end;
end;


constructor TtiVisitorManager.Create;
begin
  inherited ;
  FThreadIDList     := TList.Create ;
  FCritSec          := TCriticalSection.Create;
  FVisMappings      := TStringList.Create ;
  FHourGlassCount   := 0 ;
  FBreakOnException := True ;
end;


destructor TtiVisitorManager.destroy;
var
  i : integer ;
begin
  for i := FVisMappings.Count-1 downto 0 do
    TObject( FVisMappings.Objects[i] ).Free ;
  FVisMappings.Free ;
  FreeAndNil(FCritSec);
  FThreadIDList.Free ;
  inherited;
end;


procedure TtiVisitorManager.DoAfterExecute(pVisitorMgr: TtiVisitorCtrlr;
    pVisitors: TList);
begin
  pVisitorMgr.AfterExecuteAll(pVisitors);
end;


procedure TtiVisitorManager.DoAfterExecuteError(pVisitorMgr: TtiVisitorCtrlr;
    pVisitors: TList);
begin
  pVisitorMgr.AfterExecuteError(pVisitors);
end;


procedure TtiVisitorManager.DoBeforeExecute(pVisitorMgr: TtiVisitorCtrlr;
    pVisitors: TList);
begin
  pVisitorMgr.BeforeExecuteAll(pVisitors);
end;


function TtiVisitorManager.Execute( const psGroupName       : string;
                            const pVisited          : TtiVisited ;
                            const pDBConnectionName : string = '' ;
                            const pPerLayerName     : string = '' ) : string ;
var
  lbHourGlassRequired : boolean ;
  lPerLayerName       : string ;
  lDBConnectionName   : string ;
begin
  // Don't go any further if terminated
  if gTIOPFManager.Terminated then
    Exit ; //==>

  AddThreadID( GetCurrentThreadID ) ;

  try
    Log( 'About to process visitors for <' + psGroupName + '>', lsVisitor ) ;

    if pPerLayerName = '' then
    begin
      Assert( gTIOPFManager.DefaultPerLayer.TestValid(TtiPersistenceLayer), cTIInvalidObjectError );
      lPerLayerName := gTIOPFManager.DefaultPerLayer.PerLayerName
    end else
      lPerLayerName := pPerLayerName ;

    if pDBConnectionName = '' then
      lDBConnectionName := gTIOPFManager.DefaultDBConnectionName
    else
      lDBConnectionName := pDBConnectionName ;

    Assert( lDBConnectionName <> '',
            'Either the gTIOPFManager.DefaultDBConnectionName must be set, ' +
            'or the DBConnectionName must be passed as a parameter to ' +
            'gVisMgr.Execute( )' ) ;

    // If we are in the main thread, and Application.MainForm <> nil,
    // then we require an hourglass
    lbHourGlassRequired :=
      ( GetCurrentThreadID = MainThreadID ) and
      ( Application.MainForm <> nil ) ;

    // If an hourglass is required, then turn it on and inc the counter
    if lbHourGlassRequired then
    begin
      if ( FHourGlassCount = 0 ) then
      begin
        FSavedCursor := Screen.Cursor ;
        Screen.Cursor := crHourGlass ;
      end ;
      Inc( FHourGlassCount ) ;
    end ;

    try
      Result := '' ;
      try
        ProcessVisitors( psGroupName, pVisited, lDBConnectionName, lPerLayerName ) ;
      finally
        // If an hourglass was required, then dec the counter and turn it off
        if lbHourGlassRequired then
        begin
          Dec( FHourGlassCount ) ;
          if ( FHourGlassCount = 0 ) then
            Screen.Cursor := FSavedCursor ;
        end ;
      end ;

    except
      // Log and display any error messages
      on e:exception do
      begin
        Result := e.message ;
        LogError( e.message, false ) ;
        if BreakOnException then
          raise ;
      end ;
    end ;

    Log( 'Finished process visitors for <' + psGroupName + '>', lsVisitor );
  finally
    RemoveThreadID(GetCurrentThreadID);
  end;
end;


procedure TtiVisitorManager.ExecuteVisitors(pVisitors: TList; pVisited: TtiVisited);
  procedure _RunBeforeExecuteOne( pVisitor : TtiVisitor ) ;
  var
    lsVisitor : string ;
  begin
    lsVisitor := pVisitor.ClassName ;
    pVisitor.VisitorController.BeforeExecuteOne( pVisitor ) ;
  end ;


  procedure _RunAfterExecuteOne( pVisitor : TtiVisitor ) ;
  var
    lsVisitor : string ;
  begin
    // Don't go any further if terminated
    if gTIOPFManager.Terminated then
      Exit ; //==>
    lsVisitor := pVisitor.ClassName ;
    pVisitor.VisitorController.AfterExecuteOne( pVisitor ) ;
  end ;


  procedure _RunIterate( pVisited : TtiVisited ; pVisitor : TtiVisitor );
  begin
    if pVisitor.IterateDirection = vidTopDown then
      pVisited.Iterate( pVisitor )
    else
      pVisited.IterateBottomUp( pVisitor ) ;
  end ;
var
  lVisitor : TtiVisitor ;
  i : integer ;
begin
  for i := 0 to pVisitors.Count - 1 do
  begin
    // Don't go any further if terminated
    if gTIOPFManager.Terminated then
      Exit ; //==>
    lVisitor  := TtiVisitor( pVisitors.Items[i] ) ;
    _RunBeforeExecuteOne( lVisitor ) ;
    try
      if pVisited <> nil then
        _RunIterate( pVisited, lVisitor )
      else
        lVisitor.Execute( nil ) ;
    finally
      _RunAfterExecuteOne( lVisitor ) ;
    end ;
  end ;
end ;


// Search for the appropriate VisitorController for each visitor
procedure TtiVisitorManager.GetVisitorControllers( const pVisitors    : TList ;
                                           const pVisitorMgrs : TList ;
                                           const pDBConnectionName : string ;
                                           const pPerLayerName : string );
var
  i, j : integer ;
  lVisitor : TtiVisitor ;
begin
  Log( 'Getting visitor controllers', lsVisitor ) ;
  // Scan all the visitors
  for i := 0 to pVisitors.Count - 1 do begin
    // Get a local pointer to the visitor
    lVisitor := TtiVisitor( pVisitors.Items[i] ) ;

    // Search the list of visitor controllers already created for a match
    // with this visitor.
    for j := 0 to pVisitorMgrs.Count - 1 do begin
      if (lVisitor.VisitorControllerClass.ClassName = TObject( pVisitorMgrs.Items[j] ).ClassName ) then begin
        lVisitor.VisitorController := TtiVisitorCtrlr( pVisitorMgrs.Items[j] ) ;
        break ; //==>
      end ;
    end ;

    // The visitor controller was not found, so add a new one.
    if lVisitor.VisitorController = nil then begin
      lVisitor.VisitorController := lVisitor.VisitorControllerClass.Create ;
      lVisitor.VisitorController.PerLayerName := pPerLayerName ;
      lVisitor.VisitorController.DBConnectionName := pDBConnectionName ;
      // ToDo: Remove the need to set lVisitor.VisitorController.SQLMgrDataSource
      lVisitor.VisitorController.SQLMgrDataSource := pDBConnectionName ;
      pVisitorMgrs.Add( lVisitor.VisitorController ) ;
    end ;
  end ;
  Log( 'Done getting visitor controllers', lsVisitor ) ;
end ;


procedure TtiVisitorManager.GetVisitors(pVisitors: TList;const psGroupName: string);
var
  i : integer ;
  lsGroupName : string ;
begin
  pVisitors.Clear ;
  lsGroupName := upperCase( psGroupName ) ;
  for i := 0 to FVisMappings.Count - 1 do
    if FVisMappings.Strings[i] = lsGroupName then
      pVisitors.Add( TVisMapping( FVisMappings.Objects[i] ).ClassRef.Create ) ;
end;


procedure TtiVisitorManager.Lock;
begin
  FCritSec.Enter;
end;


procedure TtiVisitorManager.ProcessVisitorControllers(pVisitors, pVisitorControllers: TList;
  pProc: TProcessVisitorMgrs ; psMethodName : string );
var
  i, j : integer ;
  lVisitorController : TtiVisitorCtrlr ;
  lVisitors   : TList ;
begin

  if gTIOPFManager.Terminated then
    Exit ; //==>

  lVisitors := TList.Create;
  try
    // Scan all the visitor controllers in the list
    for i := 0 to pVisitorControllers.Count - 1 do
    begin

      // Don't go any further if terminated
      if gTIOPFManager.Terminated then
        Exit ; //==>

      lVisitorController := TtiVisitorCtrlr( pVisitorControllers.Items[i] ) ;
      for j := 0 to pVisitors.Count-1 do
        if ( TtiVisitor( pVisitors.Items[j] ).VisitorControllerClass =
           lVisitorController.ClassType ) and
           ( not gTIOPFManager.Terminated ) then
          lVisitors.Add( pVisitors.Items[j] ) ;
        pProc( lVisitorController, lVisitors ) ;

    end ;
  finally
    lVisitors.Free ;
  end ;
end;


procedure TtiVisitorManager.ProcessVisitors( const pGroupName        : string;
                                     const pVisited          : TtiVisited;
                                     const pDBConnectionName : string;
                                     const pPerLayerName     : string );
var
  lVisitors           : TObjectList ;
  lVisitorMgrs        : TObjectList ;
begin
  lVisitors := TObjectList.Create ;
  try
    lVisitorMgrs := TObjectList.Create ;
    try
      GetVisitors(    lVisitors, pGroupName  ) ;
      GetVisitorControllers( lVisitors, lVisitorMgrs, pDBConnectionName, pPerLayerName ) ;
      Log( 'Visitor count: ' +
           IntToStr( lVisitors.Count ) +
           ' VisitorMgr count: ' +
           IntToStr( lVisitorMgrs.Count ), lsVisitor ) ;
      ProcessVisitorControllers( lVisitors, lVisitorMgrs, DoBeforeExecute, 'DoBeforeExecute' ) ;
      try
        ExecuteVisitors( lVisitors, pVisited ) ;
        ProcessVisitorControllers( lVisitors, lVisitorMgrs, DoAfterExecute, 'DoAfterExecute' ) ;
      except
        on e:exception do
        begin
          ProcessVisitorControllers( lVisitors, lVisitorMgrs, DoAfterExecuteError, 'DoAfterExecuteError ' ) ;
          raise ;
        end ;
      end ;
    finally
      lVisitorMgrs.Free ;
    end ;
  finally
    lVisitors.Free ;
  end ;
end;


procedure TtiVisitorManager.RegisterVisitor( const psGroupName : string ;
                                         const pClassRef : TVisClassRef ) ;
var
  lVisMapping : TVisMapping ;
  lsGroupName : string ;
begin
  lsGroupName := UpperCase( psGroupName ) ;
  lVisMapping := TVisMapping.CreateExt( lsGroupName, pClassRef ) ;
  FVisMappings.AddObject( lsGroupName, lVisMapping ) ;
end;


procedure TtiVisitorManager.RemoveThreadID(pThreadID: LongWord);
var
  i : integer ;
begin
  if ( pThreadID = MainThreadID ) then
    Exit ; //==>
  if gTIOPFManager.Terminated then
    Exit ; //==>
  Lock ;
  try
    if not gTIOPFManager.Terminated then
    begin
      i := -1 ;
      try
        i := FThreadIDList.IndexOf( TObject( pThreadID )) ;
      except end ;
      if i <> -1 then
       FThreadIDList.Delete( i ) ;
    end ;
  finally
    UnLock ;
  end;
end;


function TtiVisitorManager.GetThreadCount: LongWord;
begin
  Lock ;
  try
    result := FThreadIDList.Count ;
  finally
    UnLock ;
  end;
end;


procedure TtiVisitorManager.UnLock;
begin
  FCritSec.Leave;
end;


procedure TtiVisitorManager.UnRegisterVisitors(const psGroupName: string);
var
  i : integer ;
  lsGroupName : string ;
begin
  lsGroupName := upperCase( psGroupName ) ;
  for i := FVisMappings.Count - 1 downto 0 do
    if FVisMappings.Strings[i] = lsGroupName then
    begin
      TVisMapping( FVisMappings.Objects[i] ).Free ;
      FVisMappings.Delete( i ) ;
    end ;
end;


{ TVisMapping }

constructor TVisMapping.CreateExt(const psGroupName: string;
  const pClassRef: TVisClassRef);
begin
  Create ;
  FClassRef   := pClassRef ;
  FsGroupName := upperCase( psGroupName ) ;
end;


procedure tiGetPropertyNames( pPersistent : TtiBaseObject ; pSL : TStringList ;
                              pPropFilter : TTypeKinds = ctkSimple ) ;
begin
  Assert( pPersistent <> nil, 'pPersistent not assigned.' ) ;
  tiGetPropertyNames( TtiBaseObjectClass( pPersistent.ClassType ),
                      pSL,
                      pPropFilter ) ;
end ;


procedure tiGetPropertyNames( pPersistent : TtiBaseObjectClass ;
                              pSL : TStringList ;
                              pPropFilter : TTypeKinds = ctkSimple ) ;
var
  lCount : integer ;
  lSize  : integer ;
  lList  : PPropList ;
  i : integer ;
  lPropFilter : TTypeKinds ;
begin
  Assert( pSL <> nil, 'pSL not assigned.' ) ;
  lPropFilter := pPropFilter ;

  pSL.Clear ;

  lCount  := GetPropList(pPersistent.ClassInfo
                         ,lPropFilter
                         ,nil
                         {$ifdef Delphi6OrAbove},false{$endif});
  lSize   := lCount * SizeOf(Pointer);
  GetMem(lList, lSize);
  try
     GetPropList(pPersistent.ClassInfo
                 ,lPropFilter
                 ,LList
                 {$ifdef Delphi6OrAbove},false{$endif});
    for i := 0 to lcount - 1 do
      pSL.Add( lList^[i]^.Name ) ;
  finally
    FreeMem( lList, lSize ) ;
  end ;
end ;


function tiIsReadWriteProp(const pData: TtiBaseObject;
    const psPropName: string): boolean;
begin
  result := tiIsReadWriteProp(TtiBaseObjectClass(pData.ClassType), psPropName);
end;


function tiIsReadWriteProp(const pData: TtiBaseObjectClass;
    const psPropName: string): boolean;
var
  lPropInfo : PPropInfo ;
begin
  Assert(pData <> nil, 'pData not assigned');
  Assert(IsPublishedProp(pData, psPropName), psPropName
      + ' not a published property on ' + pData.ClassName);
  try
    lPropInfo := GetPropInfo( pData, psPropName ) ;
    result    := (lPropInfo^.GetProc <> nil) and (lPropInfo^.SetProc <> nil);
  except
    on e:exception do
      raise exception.CreateFmt(
          'Error calling tiIsReadWriteProp with class: %s and property %s',
          [pData.ClassName, psPropName]);
  end;
end;


function tiGetSimplePropType(const pPersistent: TtiBaseObject;
    const psPropName: string): TtiTypeKind;
var
  lPropType : TTypeKind ;
  lPropTypeName : string ;
begin

  Assert( pPersistent <> nil, 'pPersistent is nil' ) ;

  lPropTypeName := GetPropInfo(pPersistent, psPropName)^.PropType^.Name ;

  // Check for a TDateTime
  if SameText( lPropTypeName, 'TDateTime' ) then
  begin
    result := tiTKDateTime ;
    Exit ; //==>
  end ;

  // Check for a Boolean
  if SameText( lPropTypeName, 'Boolean' ) then
  begin
    result := tiTKBoolean ;
    Exit ; //==>
  end ;

  try
    lPropType := PropType( pPersistent, psPropName ) ;
  except
    on e:exception do
      raise exception.Create( 'Error in tiGetSimpleTypeKind ' + Cr +
                              'Property name: ' + psPropName + Cr +
                              'Message: ' + e.message ) ;
  end ;

  // ToDo: Detection of stream properties could be better
  if ( lPropType = tkClass ) and
     (( SameText( 'TStream', lPropTypeName )) or
      ( SameText( 'TMemoryStream', lPropTypeName )) or
      ( SameText( 'TFileStream', lPropTypeName )) or
      ( SameText( 'TStringStream', lPropTypeName ))) then
  begin
    result := tiTKBinary ;
    Exit ; //==>
  end ;

  case lPropType of
  tkInteger,
  tkInt64,
  tkEnumeration : result := tiTKInteger ;

  tkFloat       : result := tiTKFloat ;

  tkString,
  tkChar,
  tkWChar,
  tkLString,
  {$IFDEF FPC}
  tkAString,
  {$ENDIF}
  tkWString     : result := tiTKString ;

  {$IFDEF FPC}
  tkBool        : result := tiTKBoolean;
  {$ENDIF}
  else
    raise exception.Create( 'Invalid property type passed to ' +
                            'tiGetSimplePropType. ClassName <' +
                            pPersistent.ClassName +
                            '> Property name <' +
                            psPropName + '>' ) ;
  end;
end;


function tiVarSimplePropType( pValue : Variant ) : TtiTypeKind ;
begin
{
varEmpty        The variant is Unassigned.
varNull	        The variant is Null.
VarSmallint     16-bit signed integer (type Smallint).
varInteger      32-bit signed integer (type Integer).
varSingle       Single-precision floating-point value (type Single).
varDouble       Double-precision floating-point value (type Double).
varCurrency     Currency floating-point value (type Currency).
varDate         Date and time value (type TDateTime).
varOLEStr       Reference to a dynamically allocated UNICODE string.
varDispatch     Reference to an Automation object (an IDispatch interface pointer).
varError        Operating system error code.
varBoolean      16-bit boolean (type WordBool).
varUnknown      Reference to an unknown COM object (an IUnknown interface pointer).
varByte         8-bit unsigned integer (type Byte).
varString       Reference to a dynamically allocated Pascal string (type AnsiString).
varTypeMask     Bit mask for extracting type code.
varArray        Bit indicating variant array.
varByRef        Bit indicating variant contains a reference (rather than a value).
}

  if tiIsVariantOfType( pValue, varSmallint ) or
     tiIsVariantOfType( pValue, varInteger ) or
     {$ifdef Delphi6OrAbove}
     tiIsVariantOfType( pValue, varWord ) or
     tiIsVariantOfType( pValue, varLongWord ) or
     tiIsVariantOfType( pValue, varInt64 ) or
     tiIsVariantOfType( pValue, varShortInt ) or
     {$endif}
     tiIsVariantOfType( pValue, varByte ) then
    Result := tiTKInteger
  else if tiIsVariantOfType( pValue, varSingle ) or
          tiIsVariantOfType( pValue, varDouble ) or
          tiIsVariantOfType( pValue, varCurrency ) then
    Result := tiTKFloat
  else if tiIsVariantOfType( pValue, varString ) or
          tiIsVariantOfType( pValue, varOLEStr ) then
    Result := tiTKString
  else if tiIsVariantOfType( pValue, varDate ) then
    Result := tiTKDateTime
  else if tiIsVariantOfType( pValue, varBoolean ) then
    Result := tiTKBoolean
  else
  begin
    raise EtiOPFInternalException.Create(cErrorInvalidVariantType ) ;
    Result := tiTKInteger ; // Just to shut the compiler up. Won't get here.
  end ;
end;


function tiIsNumericProp( pPersistent : TtiBaseObject ; psPropName : string ) : boolean ;
var
  lPropType : TTypeKind ;
begin
  try
    lPropType := PropType( pPersistent, psPropName ) ;
  except
    on e:exception do
      raise exception.Create( 'Error in tiGetSimpleTypeKind ' +
                              'Message: ' + e.message ) ;
  end ;
  result := lPropType in [ tkInteger, tkInt64,tkEnumeration, tkFloat ] ;
end;


end.

