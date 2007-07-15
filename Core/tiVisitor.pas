unit tiVisitor;

{$I tiDefines.inc}

// ToDo:
//    Refactor ineration to allow TopDownSinglePass,
//      BottomUpSinglePass, TopDownRecurse
//    Remove
//      SelfIterate
//      FindAllByClassType
//      CountByClass
//    Audit for const params
//    Group visitors by registered name
//    Audit for unit tests
//    Add VisitBranch method

interface
uses
   tiBaseObject
  ,tiStreams
  ,Classes
  ,TypInfo
  ,SyncObjs
  ,SysUtils
 ;

const
  cErrorInVisitorExecute = 'Error in %s.Execute(%s) Message: %s';

type
  {$M+}
  TtiVisited = class;
  {$M-}
  TtiVisitor = class;

  // TVisitorClass reference
  TtiVisitorClass = class of TtiVisitor;

  // TtiVisited class reference
  TtiVisitedClass = class of TtiVisited;

  // TtiVisited
  // The class that gets visited.
  TtiVisited = class(TtiBaseObject)
  protected
    function    GetCaption: string; virtual;
    procedure   BuildListToVisit(const AVisitor : TtiVisitor);
  published
    property    Caption   : string  read GetCaption;
  public
    constructor Create; virtual;
    procedure   Iterate(AVisitor : TtiVisitor); virtual;
    procedure   IterateBottomUp(AVisitor: TtiVisitor); virtual;
    procedure   FindAllByClassType(AClass : TtiVisitedClass; AList : TList);
  end;

  TtiVisitorCtrlr = class(TtiBaseObject)
  private
    FDBConnectionName: string;
    FPerLayerName   : string;
  protected
    procedure SetPerLayerName(const AValue: string); virtual;
  public
    constructor Create; virtual;
    procedure BeforeExecuteAll(AVisitors : TList)     ; virtual;
    procedure BeforeExecuteOne(AVisitor : TtiVisitor); virtual;
    // Visitors are executed here...
    procedure AfterExecuteOne(AVisitor : TtiVisitor ); virtual;
    procedure AfterExecuteAll(AVisitors : TList)      ; virtual;
    // Executed if there was an error
    procedure AfterExecuteError(AVisitors : TList)    ; virtual;
    // The property DBConnectionName is really only required in DBVisitors, but
    // must be introduce here so it can be set at a generic level by the
    // VisitorMgr. The alternative is to use RTTI or TypeInfo and only set the
    // property on DBVisitorMgr(s), but that would be an ever worse hack.
    property  PerLayerName    : string read FPerLayerName     write SetPerLayerName;
    property  DBConnectionName : string read FDBConnectionName write FDBConnectionName;
  end;

  TtiVisitorControllerClass = class of TtiVisitorCtrlr;

  // Requre vidTopDownSinglePass, vidBottomUpSinglePass, vidTopDownRecurse
  TtiVisitorIterateDirection = (vidTopDown, vidBottomUp);

  // TtiVisitor: The class that does the visiting
  TtiVisitor = class(TtiBaseObject)
  private
    FVisited          : TtiVisited;
    FContinueVisiting : boolean;
    FVisitorController : TtiVisitorCtrlr;
    FDepth: integer;
    FIterateDirection: TtiVisitorIterateDirection;
    FVisitedsOwner: TtiVisited;
  protected
    function    AcceptVisitor : boolean; overload; virtual;
    function    GetVisited: TtiVisited; virtual;
    procedure   SetVisited(const AValue: TtiVisited); virtual;
  public
    constructor Create; virtual;

    procedure   Execute(const AVisited : TtiVisited); virtual;
    function    VisitorControllerClass : TtiVisitorControllerClass; virtual;

    property    Visited : TtiVisited read GetVisited write SetVisited;
    property    ContinueVisiting : boolean read FContinueVisiting write FContinueVisiting;
    property    VisitorController : TtiVisitorCtrlr read FVisitorController write FVisitorController;
    property    Depth : integer read FDepth write FDepth;
    property    IterateDirection : TtiVisitorIterateDirection
                  read  FIterateDirection
                  write FIterateDirection;
    property    VisitedsOwner : TtiVisited read FVisitedsOwner write FVisitedsOwner;
  end;

  TtiVisGetAllToVisit = class(TtiVisitor)
  private
    FList : TList;
    FVisitor : TtiVisitor;
  protected
    function AcceptVisitor : boolean; override;
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Execute(const AVisited : TtiVisited); override;
    property    Visitor : TtiVisitor read FVisitor write FVisitor;
    property    List : TList read FList;
  end;

  // A wrapper for the TtiPreSizedStream which allows text to be written to the stream
  // with each visit.
  TVisStream = class(TtiVisitor)
  private
    FStream : TtiPreSizedStream;
  protected
    procedure Write(const AValue : string); virtual;
    procedure WriteLn(const AValue : string = ''); virtual;
    procedure SetStream(const AValue: TtiPreSizedStream); virtual;
  public
    property  Stream : TtiPreSizedStream read FStream write SetStream;
  end;

  TVisStringStream = class(TVisStream)
  protected
    function    GetText: string; virtual;
  public
    Constructor Create; override;
    Destructor  Destroy; override;
    Property    Text : string read GetText;
  end;

  // A visitor to count the number of instances of each class owned by the
  // passed object
  TVisClassCount = class(TtiVisitor)
  private
    FList: TStringList;
    function GetClassCount(AClass : TClass): integer;
    procedure SetClassCount(AClass : TClass; const AValue: integer);
  public
    constructor Create; override;
    destructor  Destroy; override;
    procedure   Execute(const AVisited : TtiVisited); override;
    property    ClassCount[ AClass : TClass]: integer
                  read GetClassCount
                  write SetClassCount;
  end;

  // A visitor to find all owned objects of a given class
  TVisFindAllByClass = class(TtiVisitor)
  private
    FList: TList;
    FClassTypeToFind: TtiVisitedClass;
  protected
    function    AcceptVisitor : boolean; override;
  public
    procedure   Execute(const AVisited : TtiVisited); override;
    property    ClassTypeToFind : TtiVisitedClass read FClassTypeToFind write FClassTypeToFind;
    property    List : TList read FList write FList;
  end;

  TVisStreamClass = class of TVisStream;

  TVisClassRef = class of TtiVisitor;

  TVisMapping = class(TObject)
  private
    FGroupName : string;
    FClassRef  : TVisClassRef;
  public
    constructor CreateExt(const AGroupName : string;
                           const AClassRef : TVisClassRef);
    property    GroupName : string read FGroupName write FGroupName;
    property    ClassRef : TVisClassRef read FClassRef write FClassRef;
  end;

  // A procedural type to define the signature used for
  // BeforeExecute, AfterExecute and AfterExecuteError
  TProcessVisitorMgrs = procedure(AVisitorController : TtiVisitorCtrlr;
                                   AVisitors  : TList) of object;

  // The Visitor Manager
  TtiVisitorManager = class(TtiBaseObject)
  private
    FVisMappings : TStringList;
    FHourGlassCount : integer;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
    FBreakOnException: boolean;
    procedure GetVisitors(      AVisitors : TList; const AGroupName : string);
    procedure GetVisitorControllers(const AVisitors        : TList;
                                     const AVisitorMgrs     : TList;
                                     const ADBConnectionName : string;
                                     const APersistenceLayerName    : string);
    procedure ProcessVisitorControllers(
        AVisitors, pVisitorControllers : TList;
        pProc : TProcessVisitorMgrs;
        psMethodName : string);
    // These call ProcessVisitorMgrs to scan for visitors and visitorMgrs
    // by passing the appropriate method of VisitorMgr to execute.
    procedure DoBeforeExecute(    AVisitorMgr : TtiVisitorCtrlr; AVisitors  : TList);
    procedure DoAfterExecute(     AVisitorMgr : TtiVisitorCtrlr; AVisitors  : TList);
    procedure DoAfterExecuteError(AVisitorMgr : TtiVisitorCtrlr; AVisitors  : TList);
    procedure ExecuteVisitors(  AVisitors   : TList; AVisited : TtiVisited);
    procedure ProcessVisitors(const AGroupName : string;
                               const AVisited : TtiVisited;
                               const ADBConnectionName : string;
                               const APersistenceLayerName     : string);
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   RegisterVisitor(const AGroupName : string;
                                 const AClassRef  : TVisClassRef);
    procedure   UnRegisterVisitors(const AGroupName : string);
    function    Execute(const AGroupName      : string;
                         const AVisited         : TtiVisited;
                         const ADBConnectionName : string = '';
                         const APersistenceLayerName    : string = ''): string;
    property    BreakOnException : boolean read FBreakOnException write FBreakOnException;
  end;


// Global proc to write a apply a TVisStream (as a TFileStream) to a TtiVisited.
procedure VisStreamToFile(AData       : TtiVisited;
                          AFileName  : string;
                          AVisClassRef : TtiVisitorClass);


implementation
uses
   tiLog     // Logging
  ,tiOPFManager
  ,tiConstants
  ,tiPersistenceLayers
  ,tiExcept
  ,Contnrs
  ,tiUtils
  ,tiRTTI
  {$IFDEF DELPHI5}
  ,FileCtrl
  {$ENDIF}
 ;


procedure VisStreamToFile(AData : TtiVisited;
                           AFileName : string;
                           AVisClassRef : TtiVisitorClass);
var
  lVisitor : TVisStream;
  lStream : TtiPreSizedStream;
  lDir    : string;
begin
  lDir := ExtractFilePath(AFileName);
  tiForceDirectories(AFileName);
  lStream := TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
  try
    lVisitor  := TVisStream(AVisClassRef.Create);
    try
      lVisitor.Stream := lStream;
      AData.Iterate(lVisitor);
    finally
      lVisitor.Free;
    end;
    lStream.SaveToFile(AFileName);
  finally
     lStream.Free;
  end;
end;


{ TtiVisited }

//function TtiVisited.CountByClass(AClass: TtiVisitedClass): integer;
//var
//  lList : TList;
//begin
//  lList := TList.Create;
//  try
//    FindAllByClassType(AClass, lList);
//    result := lList.Count;
//  finally
//    lList.Free;
//  end;
//end;


procedure TtiVisited.BuildListToVisit(const AVisitor: TtiVisitor);
begin
end;

constructor TtiVisited.Create;
begin
  inherited create;
  //FSelfIterate := true;
end;


procedure TtiVisited.FindAllByClassType(AClass: TtiVisitedClass; AList: TList);
var
  lVis : TVisFindAllByClass;
begin
  Assert(AList <> nil, 'AList not assigned');
  AList.Clear;
  lVis := TVisFindAllByClass.Create;
  try
    lVis.ClassTypeToFind := AClass;
    lVis.List := AList;
    Iterate(lVis);
  finally
    lVis.Free;
  end;
end;


function TtiVisited.GetCaption: string;
begin
  result := className;
end;


procedure TtiVisited.Iterate(AVisitor: TtiVisitor);
var
  lClassPropNames : TStringList;
  i       : integer;
  j       : integer;
  lVisited : TObject;
  lVisitedsOwner : TtiVisited;
begin
  Assert(AVisitor <> nil, 'Visitor unassigned');

  // Don't go any further if terminated
  if gTIOPFManager.Terminated then
    Exit; //==>

  if not AVisitor.ContinueVisiting then
    Exit; //==>

  AVisitor.Depth := AVisitor.Depth + 1;
  try

    AVisitor.Execute(self);
    lVisitedsOwner := AVisitor.VisitedsOwner;
    AVisitor.VisitedsOwner := Self;

    // If SelfIterate is true, then use RTTI to scan through all the
    // properties of type TtiVisited
    if not gTIOPFManager.Terminated then
    begin
      // Create a string list to hold the property names
      lClassPropNames := TStringList.Create;
      try
        // Get all property names of type tkClass
        tiGetPropertyNames(self, lClassPropNames, [tkClass]);

        // Scan through these properties
        for i := 0 to lClassPropNames.Count - 1 do
        begin

          // Get a pointer to the property
          lVisited := GetObjectProp(self, lClassPropNames.Strings[i]);

          // If the property is a TtiVisited, then visit it.
          if (lVisited is TtiVisited) and
             (AVisitor.ContinueVisiting) and
             (not gTIOPFManager.Terminated) then
          begin
            TtiVisited(lVisited).Iterate(AVisitor);
            continue; //==>
          end;

          // If the property is a TList, then visit it's items
          if (lVisited is TList) then
          begin
            for j := 0 to TList(lVisited).Count - 1 do
              if (TObject(TList(lVisited).Items[j]) is TtiVisited) and
                 (AVisitor.ContinueVisiting) and
                 (not gTIOPFManager.Terminated) then
              begin
                TtiVisited(TList(lVisited).Items[j]).Iterate(AVisitor);
              end;
            continue; //==>
          end;

        end;
        AVisitor.VisitedsOwner := lVisitedsOwner;
      finally
        lClassPropNames.Free;
      end;
    end;

  finally
    AVisitor.Depth := AVisitor.Depth - 1;
  end;
end;


{ TtiVisitor }

constructor TtiVisitor.Create;
begin
  inherited create;
  FContinueVisiting := true;
  FVisitorController := nil;
  FDepth            := 0;
  FIterateDirection  := vidTopDown;
end;


function TtiVisitor.AcceptVisitor: boolean;
begin
  result := true;
end;


procedure TVisStream.SetStream(const AValue: TtiPreSizedStream);
begin
  Assert(AValue.TestValid(TtiPreSizedStream), cTIInvalidObjectError);
  FStream := AValue;
end;


procedure TVisStream.Write(const AValue: string);
begin
  Assert(FStream.TestValid(TtiPreSizedStream), cTIInvalidObjectError);
  FStream.Write(AValue);
end;


procedure TVisStream.WriteLn(const AValue: string = '');
begin
  Assert(FStream.TestValid(TtiPreSizedStream), cTIInvalidObjectError);
  FStream.WriteLn(AValue);
end;


procedure TtiVisitor.Execute(const AVisited: TtiVisited);
begin
  FVisited := AVisited;
end;


function TtiVisitor.VisitorControllerClass : TtiVisitorControllerClass;
begin
  result := TtiVisitorCtrlr;
end;


{ TtiVisitorCtrlr }

procedure TtiVisitorCtrlr.AfterExecuteAll(AVisitors : TList);
begin
  Assert(AVisitors = AVisitors);  // Getting rid of compiler hints, param not used.
  // Do nothing
end;


procedure TtiVisitorCtrlr.AfterExecuteError(AVisitors : TList);
begin
  Assert(AVisitors = AVisitors);  // Getting rid of compiler hints, param not used.
  // Do nothing
end;


procedure TtiVisitorCtrlr.AfterExecuteOne(AVisitor : TtiVisitor);
begin
  Assert(AVisitor = AVisitor);  // Getting rid of compiler hints, param not used.
  // Do nothing
end;


procedure TtiVisitorCtrlr.BeforeExecuteAll(AVisitors : TList);
begin
  Assert(AVisitors = AVisitors);  // Getting rid of compiler hints, param not used.
  // Do nothing
end;


procedure TtiVisitorCtrlr.BeforeExecuteOne(AVisitor : TtiVisitor);
begin
  Assert(AVisitor = AVisitor);  // Getting rid of compiler hints, param not used.
  // Do nothing
end;


constructor TtiVisitorCtrlr.Create;
begin
  // So we can create an instance ot TVisitorMgr from a class reference var.
  inherited;
end;


function TtiVisitor.GetVisited: TtiVisited;
begin
  result := FVisited;
end;


procedure TtiVisitor.SetVisited(const AValue: TtiVisited);
begin
  FVisited := AValue;
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


procedure TVisClassCount.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  ClassCount[ AVisited.ClassType ]:= ClassCount[ AVisited.ClassType ] + 1;
end;


function TVisClassCount.GetClassCount(AClass : TClass): integer;
begin
  Result := StrToIntDef(FList.Values[ AClass.ClassName ], 0);
end;


procedure TVisClassCount.SetClassCount(AClass : TClass; const AValue: integer);
begin
  FList.Values[ AClass.ClassName ]:= IntToStr(AValue);
end;


{ TVisStringStream }

constructor TVisStringStream.Create;
begin
  inherited;
  Stream := TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
end;


destructor TVisStringStream.Destroy;
begin
  Stream.Free;
  inherited;
end;


function TVisStringStream.GetText: string;
begin
  result := FStream.AsString;
end;


procedure TtiVisited.IterateBottomUp(AVisitor: TtiVisitor);
var
  lVisitor : TtiVisGetAllToVisit;
  i : integer;
begin
  lVisitor := TtiVisGetAllToVisit.Create;
  try
    lVisitor.Visitor := AVisitor;
    Self.Iterate(lVisitor);
    for i := lVisitor.List.Count - 1 downto 0 do
      AVisitor.Execute(TtiVisited(lVisitor.List.Items[i]));
  finally
    lVisitor.Free;
  end;
end;


{ TtiVisGetAllToVisit }

function TtiVisGetAllToVisit.AcceptVisitor: boolean;
begin
  result := FVisitor.AcceptVisitor;
end;


constructor TtiVisGetAllToVisit.Create;
begin
  inherited;
  FList := TList.Create;
end;


destructor TtiVisGetAllToVisit.Destroy;
begin
  FList.Free;
  inherited;
end;


procedure TtiVisGetAllToVisit.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  FVisitor.Visited := AVisited;
  if AcceptVisitor then
    List.Add(AVisited);
end;


{ TVisFindAllByClass }

function TVisFindAllByClass.AcceptVisitor: boolean;
begin
  result := Visited is FClassTypeToFind;
end;


procedure TVisFindAllByClass.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  if not AcceptVisitor then
    Exit; //==>
  FList.Add(AVisited);
end;


procedure TtiVisitorCtrlr.SetPerLayerName(const AValue: string);
begin
  FPerLayerName := AValue;
end;


{ TtiVisitorManager }

constructor TtiVisitorManager.Create;
begin
  inherited;
  FSynchronizer         := TMultiReadExclusiveWriteSynchronizer.Create;
  FVisMappings     := TStringList.Create;
  FHourGlassCount  := 0;
  FBreakOnException := True;
end;


destructor TtiVisitorManager.destroy;
var
  i : integer;
begin
  for i := FVisMappings.Count-1 downto 0 do
    TObject(FVisMappings.Objects[i]).Free;
  FVisMappings.Free;
  FreeAndNil(FSynchronizer);
  inherited;
end;


procedure TtiVisitorManager.DoAfterExecute(AVisitorMgr: TtiVisitorCtrlr;
    AVisitors: TList);
begin
  AVisitorMgr.AfterExecuteAll(AVisitors);
end;


procedure TtiVisitorManager.DoAfterExecuteError(AVisitorMgr: TtiVisitorCtrlr;
    AVisitors: TList);
begin
  AVisitorMgr.AfterExecuteError(AVisitors);
end;


procedure TtiVisitorManager.DoBeforeExecute(AVisitorMgr: TtiVisitorCtrlr;
    AVisitors: TList);
begin
  AVisitorMgr.BeforeExecuteAll(AVisitors);
end;


function TtiVisitorManager.Execute(const AGroupName      : string;
                            const AVisited         : TtiVisited;
                            const ADBConnectionName : string = '';
                            const APersistenceLayerName    : string = ''): string;
var
  lPerLayerName      : string;
  lDBConnectionName  : string;
begin
  // Don't go any further if terminated
  if gTIOPFManager.Terminated then
    Exit; //==>

  Log('About to process visitors for <' + AGroupName + '>', lsVisitor);

  if APersistenceLayerName = '' then
  begin
    Assert(gTIOPFManager.DefaultPerLayer.TestValid(TtiPersistenceLayer), cTIInvalidObjectError);
    lPerLayerName := gTIOPFManager.DefaultPerLayer.PerLayerName
  end else
    lPerLayerName := APersistenceLayerName;

  if ADBConnectionName = '' then
    lDBConnectionName := gTIOPFManager.DefaultDBConnectionName
  else
    lDBConnectionName := ADBConnectionName;

  Assert(lDBConnectionName <> '',
          'Either the gTIOPFManager.DefaultDBConnectionName must be set, ' +
          'or the DBConnectionName must be passed as a parameter to ' +
          'gVisMgr.Execute()');

  try
    Result := '';
    ProcessVisitors(AGroupName, AVisited, lDBConnectionName, lPerLayerName);
  except
    // Log and display any error messages
    on e:exception do
    begin
      Result := e.message;
      LogError(e.message, false);
      if BreakOnException then
        raise;
    end;
  end;

  Log('Finished process visitors for <' + AGroupName + '>', lsVisitor);
end;


procedure TtiVisitorManager.ExecuteVisitors(AVisitors: TList; AVisited: TtiVisited);
  procedure _RunBeforeExecuteOne(AVisitor : TtiVisitor);
  var
    lsVisitor : string;
  begin
    lsVisitor := AVisitor.ClassName;
    AVisitor.VisitorController.BeforeExecuteOne(AVisitor);
  end;


  procedure _RunAfterExecuteOne(AVisitor : TtiVisitor);
  var
    lsVisitor : string;
  begin
    // Don't go any further if terminated
    if gTIOPFManager.Terminated then
      Exit; //==>
    lsVisitor := AVisitor.ClassName;
    AVisitor.VisitorController.AfterExecuteOne(AVisitor);
  end;


  procedure _RunIterate(AVisited : TtiVisited; AVisitor : TtiVisitor);
  begin
    if AVisitor.IterateDirection = vidTopDown then
      AVisited.Iterate(AVisitor)
    else
      AVisited.IterateBottomUp(AVisitor);
  end;
var
  lVisitor : TtiVisitor;
  i : integer;
begin
  for i := 0 to AVisitors.Count - 1 do
  begin
    // Don't go any further if terminated
    if gTIOPFManager.Terminated then
      Exit; //==>
    lVisitor := TtiVisitor(AVisitors.Items[i]);
    _RunBeforeExecuteOne(lVisitor);
    try
      if AVisited <> nil then
        _RunIterate(AVisited, lVisitor)
      else
        lVisitor.Execute(nil);
    finally
      _RunAfterExecuteOne(lVisitor);
    end;
  end;
end;


// Search for the appropriate VisitorController for each visitor
procedure TtiVisitorManager.GetVisitorControllers(const AVisitors   : TList;
                                           const AVisitorMgrs : TList;
                                           const ADBConnectionName : string;
                                           const APersistenceLayerName : string);
var
  i, j : integer;
  lVisitor : TtiVisitor;
begin
  Log('Getting visitor controllers', lsVisitor);
  // Scan all the visitors
  for i := 0 to AVisitors.Count - 1 do begin
    // Get a local pointer to the visitor
    lVisitor := TtiVisitor(AVisitors.Items[i]);

    // Search the list of visitor controllers already created for a match
    // with this visitor.
    for j := 0 to AVisitorMgrs.Count - 1 do begin
      if (lVisitor.VisitorControllerClass.ClassName = TObject(AVisitorMgrs.Items[j]).ClassName) then begin
        lVisitor.VisitorController := TtiVisitorCtrlr(AVisitorMgrs.Items[j]);
        break; //==>
      end;
    end;

    // The visitor controller was not found, so add a new one.
    if lVisitor.VisitorController = nil then begin
      lVisitor.VisitorController := lVisitor.VisitorControllerClass.Create;
      lVisitor.VisitorController.PerLayerName := APersistenceLayerName;
      lVisitor.VisitorController.DBConnectionName := ADBConnectionName;
      AVisitorMgrs.Add(lVisitor.VisitorController);
    end;
  end;
  Log('Done getting visitor controllers', lsVisitor);
end;


procedure TtiVisitorManager.GetVisitors(AVisitors: TList;const AGroupName: string);
var
  i : integer;
  lsGroupName : string;
begin
  AVisitors.Clear;
  lsGroupName := upperCase(AGroupName);
  for i := 0 to FVisMappings.Count - 1 do
    if FVisMappings.Strings[i] = lsGroupName then
      AVisitors.Add(TVisMapping(FVisMappings.Objects[i]).ClassRef.Create);
end;

procedure TtiVisitorManager.ProcessVisitorControllers(AVisitors, pVisitorControllers: TList;
  pProc: TProcessVisitorMgrs; psMethodName : string);
var
  i, j : integer;
  lVisitorController : TtiVisitorCtrlr;
  lVisitors  : TList;
begin
  Assert(psMethodName = psMethodName);  // Getting rid of compiler hints, param not used.

  if gTIOPFManager.Terminated then
    Exit; //==>

  lVisitors := TList.Create;
  try
    // Scan all the visitor controllers in the list
    for i := 0 to pVisitorControllers.Count - 1 do
    begin

      // Don't go any further if terminated
      if gTIOPFManager.Terminated then
        Exit; //==>

      lVisitorController := TtiVisitorCtrlr(pVisitorControllers.Items[i]);
      for j := 0 to AVisitors.Count-1 do
        if (TtiVisitor(AVisitors.Items[j]).VisitorControllerClass =
           lVisitorController.ClassType) and
           (not gTIOPFManager.Terminated) then
          lVisitors.Add(AVisitors.Items[j]);
        pProc(lVisitorController, lVisitors);

    end;
  finally
    lVisitors.Free;
  end;
end;


procedure TtiVisitorManager.ProcessVisitors(const AGroupName       : string;
                                     const AVisited         : TtiVisited;
                                     const ADBConnectionName : string;
                                     const APersistenceLayerName    : string);
var
  LVisitors          : TObjectList;
  LVisitorMgrs       : TObjectList;
begin
  LVisitors := TObjectList.Create;
  try
    LVisitorMgrs := TObjectList.Create;
    try
      FSynchronizer.BeginRead;
      try
        GetVisitors(   LVisitors, AGroupName );
        GetVisitorControllers(LVisitors, LVisitorMgrs, ADBConnectionName, APersistenceLayerName);
        Log('Visitor count: ' +
             IntToStr(LVisitors.Count) +
             ' VisitorMgr count: ' +
             IntToStr(LVisitorMgrs.Count), lsVisitor);
        ProcessVisitorControllers(LVisitors, LVisitorMgrs, DoBeforeExecute, 'DoBeforeExecute');
        try
          ExecuteVisitors(LVisitors, AVisited);
          ProcessVisitorControllers(LVisitors, LVisitorMgrs, DoAfterExecute, 'DoAfterExecute');
        except
          on e:exception do
          begin
            ProcessVisitorControllers(LVisitors, LVisitorMgrs, DoAfterExecuteError, 'DoAfterExecuteError ');
            raise;
          end;
        end;
      finally
        FSynchronizer.EndRead;
      end;
    finally
      LVisitorMgrs.Free;
    end;
  finally
    LVisitors.Free;
  end;
end;


procedure TtiVisitorManager.RegisterVisitor(const AGroupName : string;
                                         const AClassRef : TVisClassRef);
var
  lVisMapping : TVisMapping;
  lsGroupName : string;
begin
  FSynchronizer.BeginWrite;
  try
    lsGroupName := UpperCase(AGroupName);
    lVisMapping := TVisMapping.CreateExt(lsGroupName, AClassRef);
    FVisMappings.AddObject(lsGroupName, lVisMapping);
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TtiVisitorManager.UnRegisterVisitors(const AGroupName: string);
var
  i : integer;
  lsGroupName : string;
begin
  FSynchronizer.BeginWrite;
  try
  lsGroupName := upperCase(AGroupName);
  for i := FVisMappings.Count - 1 downto 0 do
    if FVisMappings.Strings[i] = lsGroupName then
    begin
      TVisMapping(FVisMappings.Objects[i]).Free;
      FVisMappings.Delete(i);
    end;
  finally
    FSynchronizer.EndWrite;
  end;
end;


{ TVisMapping }

constructor TVisMapping.CreateExt(const AGroupName: string;
  const AClassRef: TVisClassRef);
begin
  Create;
  FClassRef  := AClassRef;
  FGroupName := upperCase(AGroupName);
end;


end.












