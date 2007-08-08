unit tiVisitor;

{$I tiDefines.inc}

// ToDo:

//    Audit for const params
//    Audit for unit tests
//    Format with JCF
//    Audit for Used Units
//    Audit for TestValid calls
//    Rename TtiPerObjVisitor to TtiObjectVisitor
//    Add comments in PasDoc format
//    Test compile demos
//    Remove deprecated instance of VisMgr on tiOPFManager

//    Change signature of
//      AcceptVisitor
//      Init
//      SetupParams
//      MapRowToObject
//    Remove reference to Visited in TtiObjectVisitor

//    Refactor tiTestFramework to remove duplication
//    Refactor the DBConnectionPool so Lock returns a TtiDatabase

interface
uses
   tiBaseObject
  ,tiStreams
  ,Classes
  ,TypInfo
  ,SyncObjs
  ,SysUtils
  ,Contnrs
 ;

const
  CErrorInVisitorExecute = 'Error in %s.Execute(%s) Message: %s';
  CErrorInvalidIterationStyle = 'Invalid TtiIterationStyle';
  CErrorAttemptToRegisterDuplicateVisitor = 'Attempt to register duplicate visitor "%s"';
  CErrorInvalidVisitorGroup = 'Attempt to execute visitors for an unknown visitor group "%s"';
  CErrorIncompatibleVisitorController = 'VisitorControllerClass not compatible. Required type "%s", Actual type "%s"';

type
  TtiIterationStyle = (isTopDownRecurse, isTopDownSinglePass, isBottomUpSinglePass);

  {$M+}
  TtiVisited = class;
  {$M-}
  TtiVisitor = class;
  TtiTouchedByVisitor = class;
  TtiTouchedByVisitorList = class;
  TtiVisitorManager = class;

  // TVisitorClass reference
  TtiVisitorClass = class of TtiVisitor;

  // TtiVisited class reference
  TtiVisitedClass = class of TtiVisited;

  {: Counter for the depth of iteration. There is no theoretical limit, however
     a limit is set as High(Word) = 64435 as it's unlikely that the depth
     will ever reach that limit. If it does, this type can be changed to Cardinal.}
  TIterationDepth = word;

  // Method that is called when each Visited is touched.
  TtiVisitedTouchMethod =   procedure (const ACandidates: TtiVisited;
                                     const AVisitor : TtiVisitor;
                                     const AList: TtiTouchedByVisitorList;
                                     const AIterationDepth: TIterationDepth) of object;

  TtiTouchedByVisitor = class(TtiBaseObject)
  private
    FVisitor: TtiVisitor;
    FVisited: TtiVisited;
    FIterationDepth: TIterationDepth;
  public
    constructor Create(const AVisitor: TtiVisitor; const AVisited: TtiVisited;
      const AIterationDepth: TIterationDepth);
    property Visited: TtiVisited read FVisited;
    property Visitor: TtiVisitor read FVisitor;
    property IterationDepth: TIterationDepth read FIterationDepth;
  end;

  TtiTouchedByVisitorList = class(TtiBaseObject)
  private
    FList: TObjectList;
    function GetItems(const AIndex: Integer): TtiTouchedByVisitor;
    function GetCount: integer;
  public
    constructor Create(const AOwnsObjects: Boolean);
    destructor Destroy; override;
    procedure   Add(const AItem: TtiTouchedByVisitor);
    procedure   AppendTopDown(const AList: TtiTouchedByVisitorList);
    procedure   AppendBottomUp(const AList: TtiTouchedByVisitorList);
    property    Items[const AIndex: Integer]: TtiTouchedByVisitor read GetItems;
    property    Count: integer read GetCount;
  end;

  // TtiVisited
  // The class that gets visited.
  TtiVisited = class(TtiBaseObject)
  protected
    function    GetCaption: string; virtual;
    procedure   IterateTopDownRecurse(const AVisitor : TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
    procedure   IterateTopDownSinglePass(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
    procedure   IterateBottomUpSinglePass(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
    procedure   IterateAssignTouched(const AVisitor : TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList); virtual;
    procedure   IterateRecurse(const AVisitor : TtiVisitor;
                        const ADerivedParent: TtiVisited;
                        const ATouchedByVisitorList: TtiTouchedByVisitorList;
                        const ATouchMethod: TtiVisitedTouchMethod;
                        const AIterationDepth: TIterationDepth); virtual;
    procedure   IterateOverList(const AVisitor: TtiVisitor;
                        const ACandidates: TList;
                        const ADerivedParent: TtiVisited;
                        const ATouchedByVisitorList: TtiTouchedByVisitorList;
                        const ATouchMethod: TtiVisitedTouchMethod;
                        const AIterationDepth: TIterationDepth);
    procedure   TouchMethodAddToList(const ACandidates: TtiVisited;
                                      const AVisitor : TtiVisitor;
                                      const ATouchedByVisitorList: TtiTouchedByVisitorList;
                                      const AIterationDepth: TIterationDepth);
    procedure   TouchMethodExecuteVisitor(const ACandidates: TtiVisited;
                                      const AVisitor : TtiVisitor;
                                      const ATouchedByVisitorList: TtiTouchedByVisitorList;
                                      const AIterationDepth: TIterationDepth);
    procedure   ExecuteVisitor(const AVisitor: TtiVisitor; const AVisitedCandidate: TtiTouchedByVisitor);
    function    GetTerminated: boolean; virtual;
    function    ContinueVisiting(const AVisitor: TtiVisitor): boolean; virtual;
    function    CheckContinueVisitingIfTopDownRecurse(const AVisitor: TtiVisitor): boolean; virtual;
    function    TIOPFManager: TObject; virtual;
  published
    property    Caption   : string  read GetCaption;
  public
    constructor Create; virtual;
    procedure   Iterate(const AVisitor : TtiVisitor); overload; virtual;
    procedure   FindAllByClassType(AClass : TtiVisitedClass; AList : TList);
    property    Terminated: Boolean read GetTerminated;
  end;

  TtiVisitorControllerConfig = class(TtiBaseObject)
  private
    FVisitorManager: TtiVisitorManager;
  protected
    property VisitorManager: TtiVisitorManager read FVisitorManager;
  public
    constructor Create(const AVisitorManager: TtiVisitorManager);
  end;

  TtiVisitorController = class(TtiBaseObject)
  private
    FConfig: TtiVisitorControllerConfig;
    FTouchedByVisitorList: TtiTouchedByVisitorList;
    FVisitorManager: TtiVisitorManager;
  protected
    property  Config: TtiVisitorControllerConfig read FConfig;
    property  VisitorManager: TtiVisitorManager read FVisitorManager;
  public
    constructor Create(const AVisitorManager: TtiVisitorManager; const AConfig: TtiVisitorControllerConfig); virtual;
    destructor  Destroy; override;
    property    TouchedByVisitorList: TtiTouchedByVisitorList read FTouchedByVisitorList;
    procedure BeforeExecuteVisitorGroup; virtual;
    procedure BeforeExecuteVisitor(const AVisitor : TtiVisitor); virtual;
    procedure AfterExecuteVisitor(const AVisitor : TtiVisitor ); virtual;
    procedure AfterExecuteVisitorGroup(const ATouchedByVisitorList : TtiTouchedByVisitorList); virtual;
    procedure AfterExecuteVisitorGroupError; virtual;
  end;

  TtiVisitorControllerClass = class of TtiVisitorController;

  // TtiVisitor: The class that does the visiting
  TtiVisitor = class(TtiBaseObject)
  private
    FVisited          : TtiVisited;
    FContinueVisiting : boolean;
    FDepth: TIterationDepth;
    FIterationStyle: TtiIterationStyle;
    FVisitedsOwner: TtiVisited;
  protected
    function    AcceptVisitor : boolean; overload; virtual;
    function    AcceptVisitor(const AVisited: TtiVisited) : boolean; overload; virtual;
    function    VisitBranch(const ADerivedParent, AVisited: TtiVisited) : boolean; virtual;
    function    GetVisited: TtiVisited; virtual;
    procedure   SetVisited(const AValue: TtiVisited);
    procedure   SetDepth(const ADepth: TIterationDepth);
  public
    constructor Create; virtual;
    class function VisitorControllerClass : TtiVisitorControllerClass; virtual;

    procedure   Execute(const AVisited : TtiVisited); virtual;
    property    Visited : TtiVisited read FVisited; // ToDo: Can this be protected?

    property    ContinueVisiting : boolean read FContinueVisiting write FContinueVisiting;
    property    Depth : TIterationDepth read FDepth;
    property    IterationStyle : TtiIterationStyle
                  read  FIterationStyle
                  write FIterationStyle;
    property    VisitedsOwner : TtiVisited read FVisitedsOwner write FVisitedsOwner;

  end;

  TtiVisitorMappingGroup = class(TtiBaseObject)
  private
    FMappings: TClassList;
    FGroupName: string;
    FVisitorControllerClass: TtiVisitorControllerClass;
  public
    constructor Create(const AGroupName: string;
      const AVisitorControllerClass: TtiVisitorControllerClass);
    destructor Destroy; override;
    procedure Add(const AVisitorClass: TtiVisitorClass);
    procedure AssignVisitorInstances(const AVisitorList: TObjectList);
    property GroupName: string read FGroupName;
    property VisitorControllerClass: TtiVisitorControllerClass read FVisitorControllerClass;
  end;

  // A procedural type to define the signature used for
  // BeforeExecute, AfterExecute and AfterExecuteError
  TOnProcessVisitorController = procedure(
    const AVisitorController : TtiVisitorController;
    const AVisitors  : TList) of object;

  // The Visitor Manager
  TtiVisitorManager = class(TtiBaseObject)
  private
    FTIOPFManager: TtiBaseObject;
    FVisitorMappings : TObjectList;
    FSynchronizer: TMultiReadExclusiveWriteSynchronizer;
    FBreakOnException: boolean;
    procedure ExecuteVisitors(const AVisitorController: TtiVisitorController; const AVisitors: TList; const AVisited : TtiVisited);
    function GetVisitorMappings: TList;
  protected
    property    VisitorMappings: TList read GetVisitorMappings;
    function    FindVisitorMappingGroup(const AGroupName: string): TtiVisitorMappingGroup; virtual;
    procedure ProcessVisitors(const AGroupName : string;
                const AVisited : TtiVisited;
                const AVisitorControllerConfig: TtiVisitorControllerConfig); virtual;
    function GetTIOPFManager: TtiBaseObject; virtual;
  public
    constructor Create(const ATIOPFManager: TtiBaseObject); virtual;
    destructor  Destroy; override;
    property    TIOPFManager: TtiBaseObject read GetTIOPFManager;
    procedure   RegisterVisitor(const AGroupName : string;
                                 const AVisitorClass  : TtiVisitorClass);
    procedure   UnRegisterVisitors(const AGroupName : string);
    function    Execute(const AGroupName      : string;
                         const AVisited         : TtiVisited): string; overload; virtual;
    property    BreakOnException : boolean read FBreakOnException write FBreakOnException;
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

procedure TtiVisited.IterateRecurse(
  const AVisitor: TtiVisitor;
  const ADerivedParent: TtiVisited;
  const ATouchedByVisitorList: TtiTouchedByVisitorList;
  const ATouchMethod: TtiVisitedTouchMethod;
  const AIterationDepth: TIterationDepth);
var
  LClassPropNames : TStringList;
  LCandidate : TObject;
  i       : integer;
  LIterationDepth: TIterationDepth;
begin
  if AVisitor.VisitBranch(ADerivedParent, Self) and
     CheckContinueVisitingIfTopDownRecurse(AVisitor) then
  begin
    LIterationDepth:= AIterationDepth+1;
    if AVisitor.AcceptVisitor(Self) then
      ATouchMethod(Self, AVisitor, ATouchedByVisitorList, LIterationDepth);
    LClassPropNames := TStringList.Create;
    try
      tiGetPropertyNames(Self, LClassPropNames, [tkClass]);
      i:= 0;
      while (i <= LClassPropNames.Count - 1) do
      begin
        LCandidate := GetObjectProp(Self, LClassPropNames.Strings[i]);
        if (LCandidate is TtiVisited) then
          (LCandidate as TtiVisited).IterateRecurse(AVisitor, Self, ATouchedByVisitorList, ATouchMethod, LIterationDepth)
        else if (LCandidate is TList) then
          IterateOverList(AVisitor, (LCandidate as TList), Self, ATouchedByVisitorList, ATouchMethod, LIterationDepth);
        inc(i);
      end;
    finally
      LClassPropNames.Free;
    end;
  end;
end;

function TtiVisited.CheckContinueVisitingIfTopDownRecurse(
  const AVisitor: TtiVisitor): boolean;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  if AVisitor.IterationStyle <> isTopDownRecurse then
    result:= true
  else
    result:= ContinueVisiting(AVisitor);
end;

function TtiVisited.ContinueVisiting(const AVisitor: TtiVisitor): boolean;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  result:= AVisitor.ContinueVisiting and not Terminated;
end;

constructor TtiVisited.Create;
begin
  inherited create;
end;


procedure TtiVisited.ExecuteVisitor(const AVisitor: TtiVisitor;
  const AVisitedCandidate: TtiTouchedByVisitor);
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(AVisitedCandidate.TestValid, cTIInvalidObjectError);
  AVisitor.SetVisited(AVisitedCandidate.Visited);
  AVisitor.SetDepth(AVisitedCandidate.IterationDepth);
  AVisitor.Execute(AVisitedCandidate.Visited);
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


procedure TtiVisited.IterateOverList(
  const AVisitor: TtiVisitor;
  const ACandidates: TList;
  const ADerivedParent: TtiVisited;
  const ATouchedByVisitorList: TtiTouchedByVisitorList;
  const ATouchMethod: TtiVisitedTouchMethod;
  const AIterationDepth: TIterationDepth);
var
  i: integer;
begin
  i:= 0;
  while (i <= ACandidates.Count - 1) do
  begin
    if (TObject(ACandidates.Items[i]) is TtiVisited) then
      TtiVisited(ACandidates.Items[i]).IterateRecurse(AVisitor, ADerivedParent,
              ATouchedByVisitorList, ATouchMethod, AIterationDepth);
    inc(i);
  end;
end;

function TtiVisited.GetCaption: string;
begin
  result := className;
end;


function TtiVisited.GetTerminated: boolean;
begin
  result:= TtiOPFManager(TIOPFManager).Terminated;
end;

procedure TtiVisited.IterateTopDownRecurse(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
var
  LTouchedObjectList: TtiTouchedByVisitorList;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  LTouchedObjectList:= TtiTouchedByVisitorList.Create(False);
  try
    IterateRecurse(AVisitor, nil, LTouchedObjectList, TouchMethodExecuteVisitor, 0);
  finally
    ATouchedByVisitorList.AppendTopDown(LTouchedObjectList);
    LTouchedObjectList.Free;
  end;
end;


procedure TtiVisited.IterateTopDownSinglePass(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
var
  LTouchedObjectList: TtiTouchedByVisitorList;
  i : integer;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  LTouchedObjectList:= TtiTouchedByVisitorList.Create(False);
  try
    IterateRecurse(AVisitor, nil, LTouchedObjectList, TouchMethodAddToList, 0);
    i:= 0;
    while (i <= LTouchedObjectList.Count-1) and
      ContinueVisiting(AVisitor) do
    begin
      ExecuteVisitor(AVisitor, LTouchedObjectList.Items[i]);
      Inc(i);
    end;
  finally
    ATouchedByVisitorList.AppendTopDown(LTouchedObjectList);
    LTouchedObjectList.Free;
  end;
end;

function TtiVisited.TIOPFManager: TObject;
begin
  result:= gTIOPFManager;
end;

procedure TtiVisited.TouchMethodAddToList(const ACandidates: TtiVisited;
  const AVisitor: TtiVisitor; const ATouchedByVisitorList: TtiTouchedByVisitorList; const AIterationDepth: TIterationDepth);
var
  LVisitedCandidate: TtiTouchedByVisitor;
begin
  LVisitedCandidate:= TtiTouchedByVisitor.Create(AVisitor, ACandidates, AIterationDepth);
  ATouchedByVisitorList.Add(LVisitedCandidate);
end;

procedure TtiVisited.TouchMethodExecuteVisitor(const ACandidates: TtiVisited;
  const AVisitor: TtiVisitor; const ATouchedByVisitorList: TtiTouchedByVisitorList; const AIterationDepth: TIterationDepth);
var
  LVisitedCandidate: TtiTouchedByVisitor;
begin
  LVisitedCandidate:= TtiTouchedByVisitor.Create(AVisitor, ACandidates, AIterationDepth);
  ATouchedByVisitorList.Add(LVisitedCandidate);
  ExecuteVisitor(AVisitor, LVisitedCandidate);
end;

{ TtiVisitor }

function TtiVisitor.AcceptVisitor(const AVisited: TtiVisited): boolean;
begin
  SetVisited(AVisited);
  result:= AcceptVisitor;
end;

constructor TtiVisitor.Create;
begin
  inherited create;
  FContinueVisiting := true;
  FDepth            := 0;
  FIterationStyle  := isTopDownRecurse;
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


function TtiVisitor.VisitBranch(const ADerivedParent,
  AVisited: TtiVisited): boolean;
begin
  result:= True;
end;

class function TtiVisitor.VisitorControllerClass : TtiVisitorControllerClass;
begin
  result := TtiVisitorController;
end;


{ TtiVisitorCtrlr }

procedure TtiVisitorController.AfterExecuteVisitorGroup(const ATouchedByVisitorList : TtiTouchedByVisitorList);
begin
  Assert(ATouchedByVisitorList = ATouchedByVisitorList);  // Getting rid of compiler hints, param not used.
  // Do nothing
end;


procedure TtiVisitorController.AfterExecuteVisitorGroupError;
begin
  // Do nothing
end;


procedure TtiVisitorController.AfterExecuteVisitor(const AVisitor : TtiVisitor);
begin
  Assert(AVisitor = AVisitor);  // Getting rid of compiler hints, param not used.
  // Do nothing
end;


procedure TtiVisitorController.BeforeExecuteVisitorGroup;
begin
  // Do nothing
end;


procedure TtiVisitorController.BeforeExecuteVisitor(const AVisitor : TtiVisitor);
begin
  Assert(AVisitor = AVisitor);  // Getting rid of compiler hints, param not used.
  // Do nothing
end;


constructor TtiVisitorController.Create(const AVisitorManager: TtiVisitorManager;
  const AConfig: TtiVisitorControllerConfig);
begin
  Assert(AVisitorManager.TestValid(TtiVisitorManager, True), cTIInvalidObjectError);
  Assert(AConfig.TestValid, cTIInvalidObjectError);
  inherited Create;
  FVisitorManager:= AVisitorManager;
  FConfig:= AConfig;
  FTouchedByVisitorList:= TtiTouchedByVisitorList.Create(True);
end;


destructor TtiVisitorController.Destroy;
begin
  FTouchedByVisitorList.Free;
  inherited;
end;

function TtiVisitor.GetVisited: TtiVisited;
begin
  result := FVisited;
end;


procedure TtiVisitor.SetDepth(const ADepth: TIterationDepth);
begin
  FDepth:= ADepth;
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


procedure TtiVisited.Iterate(const AVisitor : TtiVisitor);
var
  LTouchedByVisitorList: TtiTouchedByVisitorList;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  LTouchedByVisitorList:= TtiTouchedByVisitorList.Create(True);
  try
    IterateAssignTouched(AVisitor, LTouchedByVisitorList);
  finally
    LTouchedByVisitorList.Free;
  end;
end;

procedure TtiVisited.IterateAssignTouched(const AVisitor: TtiVisitor;
  const ATouchedByVisitorList: TtiTouchedByVisitorList);
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  case AVisitor.IterationStyle of
  isTopDownRecurse:     IterateTopDownRecurse(AVisitor, ATouchedByVisitorList);
  isTopDownSinglePass:  IterateTopDownSinglePass(AVisitor, ATouchedByVisitorList);
  isBottomUpSinglePass: IterateBottomUpSinglePass(AVisitor, ATouchedByVisitorList);
  else
    raise EtiOPFProgrammerException.Create(CErrorInvalidIterationStyle);
  end;
end;

procedure TtiVisited.IterateBottomUpSinglePass(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
var
  LTouchedObjectList: TtiTouchedByVisitorList;
  i : integer;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  LTouchedObjectList:= TtiTouchedByVisitorList.Create(False);
  try
    IterateRecurse(AVisitor, nil, LTouchedObjectList, TouchMethodAddToList, 0);
    i:= LTouchedObjectList.Count-1;
    while (i >= 0) and
      ContinueVisiting(AVisitor) do
    begin
      ExecuteVisitor(AVisitor, LTouchedObjectList.Items[i]);
      Dec(i);
    end;
  finally
    ATouchedByVisitorList.AppendBottomUp(LTouchedObjectList);
    LTouchedObjectList.Free;
  end;
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

{ TtiVisitorManager }

constructor TtiVisitorManager.Create(const ATIOPFManager: TtiBaseObject);
begin
  Assert(ATIOPFManager.TestValid(TtiOPFManager, True), cTIInvalidObjectError);
  inherited Create;
  FTIOPFManager:= ATIOPFManager;
  FSynchronizer:= TMultiReadExclusiveWriteSynchronizer.Create;
  FVisitorMappings:= TObjectList.Create;
  FBreakOnException:= True;
end;


destructor TtiVisitorManager.destroy;
begin
  FVisitorMappings.Free;
  FreeAndNil(FSynchronizer);
  inherited;
end;

function TtiVisitorManager.Execute(const AGroupName      : string;
                            const AVisited         : TtiVisited): string;
var
  LVisitorControllerConfig: TtiVisitorControllerConfig;
begin
  LVisitorControllerConfig:= TtiVisitorControllerConfig.Create(Self);
  try
    ProcessVisitors(AGroupName, AVisited, LVisitorControllerConfig);
  finally
    LVisitorControllerConfig.Free;
  end;
end;

procedure TtiVisitorManager.ExecuteVisitors(
  const AVisitorController: TtiVisitorController;
  const AVisitors: TList; const AVisited : TtiVisited);
var
  LVisitor : TtiVisitor;
  i : integer;
begin
  for i := 0 to AVisitors.Count - 1 do
  begin
    LVisitor := TtiVisitor(AVisitors.Items[i]);
    AVisitorController.BeforeExecuteVisitor(LVisitor);
    try
      if AVisited <> nil then
        AVisited.IterateAssignTouched(LVisitor, AVisitorController.TouchedByVisitorList)
      else
        LVisitor.Execute(nil);
    finally
      AVisitorController.AfterExecuteVisitor(LVisitor);
    end;
  end;
end;


function TtiVisitorManager.FindVisitorMappingGroup(
  const AGroupName: string): TtiVisitorMappingGroup;
var
  i : integer;
  LGroupName : string;
begin
  result:= nil;
  LGroupName := UpperCase(AGroupName);
  for i := 0 to FVisitorMappings.Count - 1 do
    if (FVisitorMappings.Items[i] as TtiVisitorMappingGroup).GroupName = LGroupName then
    begin
      Result:= FVisitorMappings.Items[i] as TtiVisitorMappingGroup;
      Exit; //==>
    end;
end;

function TtiVisitorManager.GetTIOPFManager: TtiBaseObject;
begin
  Assert(FTIOPFManager.TestValid, cTIInvalidObjectError);
  result:= FTIOPFManager;
end;

function TtiVisitorManager.GetVisitorMappings: TList;
begin
  result:= FVisitorMappings;
end;

procedure TtiVisitorManager.ProcessVisitors(const AGroupName       : string;
  const AVisited: TtiVisited;
  const AVisitorControllerConfig: TtiVisitorControllerConfig);
var
  LVisitorMappingGroup: TtiVisitorMappingGroup;
  LVisitorController: TtiVisitorController;
  LVisitors : TObjectList;
begin
  Log('About to process visitors for <' + AGroupName + '>', lsVisitor);
  LVisitors := TObjectList.Create;
  try
    FSynchronizer.BeginRead;
    try
      LVisitorMappingGroup:= FindVisitorMappingGroup(AGroupName);
      if LVisitorMappingGroup = nil then
        raise EtiOPFProgrammerException.CreateFmt(CErrorInvalidVisitorGroup, [AGroupName]);
      LVisitorController:= LVisitorMappingGroup.VisitorControllerClass.Create(Self, AVisitorControllerConfig);
      LVisitorMappingGroup.AssignVisitorInstances(LVisitors);
      try
        LVisitorController.BeforeExecuteVisitorGroup;
        try
          ExecuteVisitors(LVisitorController, LVisitors, AVisited);
          LVisitorController.AfterExecuteVisitorGroup(LVisitorController.TouchedByVisitorList);
        except
          on e:exception do
          begin
            LVisitorController.AfterExecuteVisitorGroupError;
            raise;
          end;
        end;
      finally
        LVisitorController.Free;
      end;
    finally
      FSynchronizer.EndRead;
    end;
  finally
    LVisitors.Free;
  end;
  Log('Finished process visitors for <' + AGroupName + '>', lsVisitor);
end;


procedure TtiVisitorManager.RegisterVisitor(const AGroupName : string;
                                         const AVisitorClass : TtiVisitorClass);
var
  LVisitorMappingGroup: TtiVisitorMappingGroup;
begin
  FSynchronizer.BeginWrite;
  try
    LVisitorMappingGroup:= FindVisitorMappingGroup(AGroupName);
    if LVisitorMappingGroup = nil then
    begin
      LVisitorMappingGroup:= TtiVisitorMappingGroup.Create(AGroupName,
        AVisitorClass.VisitorControllerClass);
      FVisitorMappings.Add(LVisitorMappingGroup);
    end;
    LVisitorMappingGroup.Add(AVisitorClass);
  finally
    FSynchronizer.EndWrite;
  end;
end;

procedure TtiVisitorManager.UnRegisterVisitors(const AGroupName: string);
var
  LVisitorMappingGroup: TtiVisitorMappingGroup;
begin
  FSynchronizer.BeginWrite;
  try
    LVisitorMappingGroup:= FindVisitorMappingGroup(AGroupName);
    Assert(Assigned(LVisitorMappingGroup),
           'Request to UnRegister visitor group that''s not registered "' +
           AGroupName + '"');
    FVisitorMappings.Remove(LVisitorMappingGroup);
  finally
    FSynchronizer.EndWrite;
  end;
end;

{ TVisitorMappingGroup }

procedure TtiVisitorMappingGroup.Add(const AVisitorClass: TtiVisitorClass);
var
  i: integer;
begin
  Assert(Assigned(AVisitorClass), 'AVisitorClass not assigned');
  if AVisitorClass.VisitorControllerClass <> VisitorControllerClass then
    raise EtiOPFProgrammerException.CreateFmt(CErrorIncompatibleVisitorController,
      [VisitorControllerClass.ClassName, AVisitorClass.VisitorControllerClass.ClassName]);
  for i := 0 to FMappings.Count-1 do
    if FMappings.Items[i] = AVisitorClass then
      raise EtiOPFProgrammerException.CreateFmt(CErrorAttemptToRegisterDuplicateVisitor, [AVisitorClass.ClassName]);
  FMappings.Add(AVisitorClass);
end;

procedure TtiVisitorMappingGroup.AssignVisitorInstances(const AVisitorList: TObjectList);
var
  i: integer;
begin
  Assert(Assigned(AVisitorList), 'AVisitors not assigned');
  for i := 0 to FMappings.Count-1 do
    AVisitorList.Add(TtiVisitorClass(FMappings.Items[i]).Create);
end;

constructor TtiVisitorMappingGroup.Create(const AGroupName: string;
  const AVisitorControllerClass: TtiVisitorControllerClass);
begin
  inherited Create;
  FGroupName:= UpperCase(AGroupName);
  FMappings:= TClassList.Create;
  FVisitorControllerClass:= AVisitorControllerClass;
end;

destructor TtiVisitorMappingGroup.Destroy;
begin
  FMappings.Free;
  inherited;
end;

{ TtiTouchedByVisitor }

constructor TtiTouchedByVisitor.Create(const AVisitor: TtiVisitor;
  const AVisited: TtiVisited;
  const AIterationDepth: TIterationDepth);
begin
  inherited Create;
  FVisitor:= AVisitor;
  FVisited:= AVisited;
  FIterationDepth:= AIterationDepth;
end;

{ TtiTouchedByVisitorList }

procedure TtiTouchedByVisitorList.Add(const AItem: TtiTouchedByVisitor);
begin
  FList.Add(AItem);
end;

procedure TtiTouchedByVisitorList.AppendBottomUp(
  const AList: TtiTouchedByVisitorList);
var
  i: Integer;
begin
  Assert(AList.TestValid, cTIInvalidObjectError);
  Assert(not AList.FList.OwnsObjects, 'AList.FList.OwnsObjects is True. Can not append from a list with OwnsObjects = True');
  Assert(FList.OwnsObjects, 'FList.OwnsObjects is False. Can not append to a list with OwnsObjects = False');
  for i := AList.Count-1 downto 0 do
    Add(AList.Items[i]);
end;

procedure TtiTouchedByVisitorList.AppendTopDown(const AList: TtiTouchedByVisitorList);
var
  i: Integer;
begin
  Assert(AList.TestValid, cTIInvalidObjectError);
  Assert(not AList.FList.OwnsObjects, 'AList.FList.OwnsObjects is True. Can not append from a list with OwnsObjects = True');
  Assert(FList.OwnsObjects, 'FList.OwnsObjects is False. Can not append to a list with OwnsObjects = False');
  for i := 0 to AList.Count-1 do
    Add(AList.Items[i]);
end;

constructor TtiTouchedByVisitorList.Create(const AOwnsObjects: Boolean);
begin
  inherited Create; 
  FList:= TObjectList.Create(AOwnsObjects);
end;

destructor TtiTouchedByVisitorList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TtiTouchedByVisitorList.GetCount: integer;
begin
  result:= FList.Count;
end;

function TtiTouchedByVisitorList.GetItems(
  const AIndex: Integer): TtiTouchedByVisitor;
begin
  result:= FList.Items[AIndex] as TtiTouchedByVisitor;
end;

{ TtiVisitorControllerConfig }

constructor TtiVisitorControllerConfig.Create(
  const AVisitorManager: TtiVisitorManager);
begin
  Assert(AVisitorManager.TestValid, cTIInvalidObjectError);
  inherited Create;
  FVisitorManager:= AVisitorManager;
end;

end.












