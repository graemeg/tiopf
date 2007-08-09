{** Unit containing the two classes TtiVisitor, TtiVisited and TtiVisitorManager,
    which form the basis of the tiOPF's object persistence mechanism.}
unit tiVisitor;

{$I tiDefines.inc}

 // ToDo:

 //    Add comments in PasDoc format

 //    Change signature of
 //      AcceptVisitor
 //      Init
 //      SetupParams
 //      MapRowToObject
 //    Remove reference to Visited in TtiObjectVisitor
 //    Update code templates

 //    Refactor tiTestFramework to remove duplication
 //    Refactor the DBConnectionPool so Lock returns a TtiDatabase

interface

uses
  tiBaseObject,
  tiStreams,
  Classes,
  TypInfo,
  SyncObjs,
  SysUtils,
  Contnrs;

const
  CErrorInVisitorExecute      = 'Error in %s.Execute(%s) Message: %s';
  CErrorInvalidIterationStyle = 'Invalid TtiIterationStyle';
  CErrorAttemptToRegisterDuplicateVisitor =
    'Attempt to register duplicate visitor "%s"';
  CErrorInvalidVisitorGroup   =
    'Attempt to execute visitors for an unknown visitor group "%s"';
  CErrorIncompatibleVisitorController =
    'VisitorControllerClass not compatible. Required type "%s", Actual type "%s"';

type
  {** A TtiVisitor can iterate over a TtiVisited in three ways:

      isTopDownRecurse - Touch all nodes in the graph of objects as they
        appear. As new objects are written to the graph, iterate over these too.

      isTopDownSinglePass - Read all objects in the graph into a list and
        iterate over these. Objects that are read into the graph as part of
        the iteration process are not touched as part of this iteration.
        isTopDownSinglePass is useful when a large number of objects are being
        read into a flat list. If isTopDownRecurse where used, each object
        read into the list would be touched by the visitor and this can be
        very time consuming.

      isBottomUpSinglePass - Same as isTopDownSinglePass, except iteration is
        from the bottom up. isBottomUpSinglePass is used for deleting objects.}
  TtiIterationStyle = (isTopDownRecurse, isTopDownSinglePass,
    isBottomUpSinglePass);

  {$M+}
  TtiVisited          = class;
  {$M-}
  TtiVisitor          = class;
  TtiTouchedByVisitor = class;
  TtiTouchedByVisitorList = class;
  TtiVisitorManager       = class;

  {** A class reference for holding TtiVisitor}
  TtiVisitorClass = class of TtiVisitor;

  {** A class reference for holding TtiVisited}
  TtiVisitedClass = class of TtiVisited;

  {** @exclude
    Counter for the depth of iteration. There is no theoretical limit, however
    a limit is set as High(Word) = 64435 as it's unlikely that the depth
    will ever reach that limit. If it does, this type can be changed to Cardinal.}
  TIterationDepth = word;

  {** @exclude Method that is called when each Visited is touched.}
  TtiVisitedTouchMethod = procedure(const ACandidates: TtiVisited;
    const AVisitor: TtiVisitor; const AList: TtiTouchedByVisitorList;
    const AIterationDepth: TIterationDepth) of object;

  {** @exclude A container for holding Visitor/Visited pairs.
      TtiTouchedByVisitor is used by TtiObjectVisitor(s) when Final must
      be executed against each TtiVisited that was touched by the iteration
      cycle.}
  TtiTouchedByVisitor = class(TtiBaseObject)
  private
    FVisitor:        TtiVisitor;
    FVisited:        TtiVisited;
    FIterationDepth: TIterationDepth;
  public
    constructor Create(const AVisitor: TtiVisitor; const AVisited: TtiVisited;
      const AIterationDepth: TIterationDepth);
    property Visited: TtiVisited read FVisited;
    property Visitor: TtiVisitor read FVisitor;
    property IterationDepth: TIterationDepth read FIterationDepth;
  end;

  {** @exclude A list of TtiTouchedByVisitor}
  TtiTouchedByVisitorList = class(TtiBaseObject)
  private
    FList: TObjectList;
    function GetItems(const AIndex: integer): TtiTouchedByVisitor;
    function GetCount: integer;
  public
    constructor Create(const AOwnsObjects: boolean);
    destructor Destroy; override;
    procedure Add(const AItem: TtiTouchedByVisitor);
    procedure AppendTopDown(const AList: TtiTouchedByVisitorList);
    procedure AppendBottomUp(const AList: TtiTouchedByVisitorList);
    property Items[const AIndex: integer]: TtiTouchedByVisitor read GetItems;
    property Count: integer read GetCount;
  end;

  {** TtiVisited implements the Iterate method, which will pass a TtiVisitor over
      every node in the graph of objects. Object instances exposed as
      published properties will be touched by the TtiVisitor. Objects contained
      in a published TList will also be touched.}
  TtiVisited = class(TtiBaseObject)
  protected
    {** @exclude}
    function GetCaption: string; virtual;
    {** @exclude}
    procedure IterateTopDownRecurse(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
    {** @exclude}
    procedure IterateTopDownSinglePass(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
    {** @exclude}
    procedure IterateBottomUpSinglePass(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
    {** @exclude}
    procedure IterateAssignTouched(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList);
      virtual;
    {** @exclude}
    procedure IterateRecurse(const AVisitor: TtiVisitor; const ADerivedParent: TtiVisited;
      const ATouchedByVisitorList: TtiTouchedByVisitorList;
      const ATouchMethod: TtiVisitedTouchMethod; const AIterationDepth: TIterationDepth);
      virtual;
    {** @exclude}
    procedure IterateOverList(const AVisitor: TtiVisitor; const ACandidates: TList;
      const ADerivedParent: TtiVisited; const ATouchedByVisitorList: TtiTouchedByVisitorList;
      const ATouchMethod: TtiVisitedTouchMethod; const AIterationDepth: TIterationDepth);
    {** @exclude}
    procedure TouchMethodAddToList(const ACandidates: TtiVisited;
      const AVisitor: TtiVisitor; const ATouchedByVisitorList: TtiTouchedByVisitorList;
      const AIterationDepth: TIterationDepth);
    {** @exclude}
    procedure TouchMethodExecuteVisitor(const ACandidates: TtiVisited;
      const AVisitor: TtiVisitor; const ATouchedByVisitorList: TtiTouchedByVisitorList;
      const AIterationDepth: TIterationDepth);
    {** @exclude}
    procedure ExecuteVisitor(const AVisitor: TtiVisitor;
      const AVisitedCandidate: TtiTouchedByVisitor);
    {** @exclude}
    function GetTerminated: boolean; virtual;
    {** @exclude}
    function ContinueVisiting(const AVisitor: TtiVisitor): boolean; virtual;
    {** @exclude}
    function CheckContinueVisitingIfTopDownRecurse(const AVisitor: TtiVisitor): boolean;
      virtual;
    {** @exclude}
    function TIOPFManager: TObject; virtual;
  published
    {** A short text description of the object. By default, the classname but
        can be customised by overriding GetCaption.}
    property Caption: string read GetCaption;
  public
    {** @exclude}
    constructor Create; virtual;
    {** Iterate will cause an instance of TtiVisitor to be passed over all
        objects that are accessable by RTTI as published.
        @param AVisitor: An instance of the TtiVisitor to be passed over the
        object graph.}
    procedure Iterate(const AVisitor: TtiVisitor); overload; virtual;
    {** Find all the objects that are of a given class type.
        @param AClass The class type to find
        @param AList An empty TList that will be populated with the instances
               found.}
    procedure FindAllByClassType(const AClass: TtiVisitedClass; const AList: TList);
    {** @exclude}
    property Terminated: boolean read GetTerminated;
  end;

  {** @exclude}
  TtiVisitorControllerConfig = class(TtiBaseObject)
  private
    FVisitorManager: TtiVisitorManager;
  protected
    property VisitorManager: TtiVisitorManager read FVisitorManager;
  public
    constructor Create(const AVisitorManager: TtiVisitorManager);
  end;

  {** @exclude}
  TtiVisitorController = class(TtiBaseObject)
  private
    FConfig:         TtiVisitorControllerConfig;
    FTouchedByVisitorList: TtiTouchedByVisitorList;
    FVisitorManager: TtiVisitorManager;
  protected
    property Config: TtiVisitorControllerConfig read FConfig;
    property VisitorManager: TtiVisitorManager read FVisitorManager;
  public
    constructor Create(const AVisitorManager: TtiVisitorManager;
      const AConfig: TtiVisitorControllerConfig); virtual;
    destructor Destroy; override;
    property TouchedByVisitorList: TtiTouchedByVisitorList read FTouchedByVisitorList;
    procedure BeforeExecuteVisitorGroup; virtual;
    procedure BeforeExecuteVisitor(const AVisitor: TtiVisitor); virtual;
    procedure AfterExecuteVisitor(const AVisitor: TtiVisitor); virtual;
    procedure AfterExecuteVisitorGroup(const ATouchedByVisitorList: TtiTouchedByVisitorList);
      virtual;
    procedure AfterExecuteVisitorGroupError; virtual;
  end;

  {** @exclude}
  TtiVisitorControllerClass = class of TtiVisitorController;

  {** TtiVisitor: The class that does the visiting}
  TtiVisitor = class(TtiBaseObject)
  private
    FVisited:        TtiVisited;
    FContinueVisiting: boolean;
    FDepth:          TIterationDepth;
    FIterationStyle: TtiIterationStyle;
    FVisitedsOwner:  TtiVisited;
  protected
    function AcceptVisitor: boolean; overload; virtual;
    function AcceptVisitor(const AVisited: TtiVisited): boolean;
      overload; virtual;
    function VisitBranch(const ADerivedParent, AVisited: TtiVisited): boolean;
      virtual;
    function GetVisited: TtiVisited; virtual;
    procedure SetVisited(const AValue: TtiVisited);
    procedure SetDepth(const ADepth: TIterationDepth);
  public
    constructor Create; virtual;
    class function VisitorControllerClass: TtiVisitorControllerClass; virtual;

    procedure Execute(const AVisited: TtiVisited); virtual;
    property Visited: TtiVisited read FVisited; // ToDo: Can this be protected?

    property ContinueVisiting: boolean read FContinueVisiting write FContinueVisiting;
    property Depth: TIterationDepth read FDepth;
    property IterationStyle: TtiIterationStyle read FIterationStyle write FIterationStyle;
    property VisitedsOwner: TtiVisited read FVisitedsOwner write FVisitedsOwner;

  end;

  {** @exclude}
  TtiVisitorMappingGroup = class(TtiBaseObject)
  private
    FMappings:  TClassList;
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

  {** @exclude
      A procedural type to define the signature used for
      BeforeExecute, AfterExecute and AfterExecuteError}
  TOnProcessVisitorController = procedure(const AVisitorController: TtiVisitorController;
    const AVisitors: TList) of object;

  {** The Visitor Manager }
  TtiVisitorManager = class(TtiBaseObject)
  private
    FTIOPFManager:     TtiBaseObject;
    FVisitorMappings:  TObjectList;
    FSynchronizer:     TMultiReadExclusiveWriteSynchronizer;
    procedure ExecuteVisitors(const AVisitorController: TtiVisitorController;
      const AVisitors: TList; const AVisited: TtiVisited);
    function GetVisitorMappings: TList;
  protected
    property VisitorMappings: TList read GetVisitorMappings;
    function FindVisitorMappingGroup(const AGroupName: string): TtiVisitorMappingGroup;
      virtual;
    procedure ProcessVisitors(const AGroupName: string; const AVisited: TtiVisited;
      const AVisitorControllerConfig: TtiVisitorControllerConfig); virtual;
    function GetTIOPFManager: TtiBaseObject; virtual;
  public
    constructor Create(const ATIOPFManager: TtiBaseObject); virtual;
    destructor Destroy; override;
    property TIOPFManager: TtiBaseObject read GetTIOPFManager;
    procedure RegisterVisitor(const AGroupName: string; const AVisitorClass: TtiVisitorClass);
    procedure UnRegisterVisitors(const AGroupName: string);
    function Execute(const AGroupName: string; const AVisited: TtiVisited): string;
      overload; virtual;
  end;


  {** A wrapper for the TtiPreSizedStream which allows text to be written to
      the stream with each visit.}
  TVisStream = class(TtiVisitor)
  private
    FStream: TtiPreSizedStream;
  protected
    procedure Write(const AValue: string); virtual;
    procedure WriteLn(const AValue: string = ''); virtual;
    procedure SetStream(const AValue: TtiPreSizedStream); virtual;
  public
    property Stream: TtiPreSizedStream read FStream write SetStream;
  end;

  {** A wrapper for the TtiPreSizedStream which allows text to be written to
      the stream with each visit. The stream can be accessed as a string.}
  TVisStringStream = class(TVisStream)
  protected
    function GetText: string; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Text: string read GetText;
  end;

  // A visitor to count the number of instances of each class owned by the
  // passed object
  TVisClassCount = class(TtiVisitor)
  private
    FList: TStringList;
    function GetClassCount(const AClass: TClass): integer;
    procedure SetClassCount(const AClass: TClass; const AValue: integer);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(const AVisited: TtiVisited); override;
    property ClassCount[const AClass: TClass]: integer read GetClassCount write SetClassCount;
  end;

  // A visitor to find all owned objects of a given class
  TVisFindAllByClass = class(TtiVisitor)
  private
    FList: TList;
    FClassTypeToFind: TtiVisitedClass;
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AVisited: TtiVisited); override;
    property ClassTypeToFind: TtiVisitedClass read FClassTypeToFind write FClassTypeToFind;
    property List: TList read FList write FList;
  end;

  {** @exclude}
  TVisStreamClass = class of TVisStream;

{** Global proc to write a apply a TVisStream (as a TFileStream) to a TtiVisited.}
procedure VisStreamToFile(const AData: TtiVisited; const AFileName: string;
  const AVisClassRef: TtiVisitorClass);

implementation

uses
  tiLog,
  tiOPFManager,
  tiConstants,
  tiPersistenceLayers,
  tiExcept,
  tiUtils,
  tiRTTI;

procedure VisStreamToFile(const AData: TtiVisited; const AFileName: string;
  const AVisClassRef: TtiVisitorClass);
var
  lVisitor: TVisStream;
  lStream:  TtiPreSizedStream;
  lDir:     string;
begin
  Assert(AData.TestValid, cTIInvalidObjectError);
  Assert(AFileName<>'', 'AFileName not assigned');
  Assert(Assigned(AVisClassRef), 'AVisClassRef not assigned');
  lDir    := ExtractFilePath(AFileName);
  tiForceDirectories(AFileName);
  lStream := TtiPreSizedStream.Create(cStreamStartSize, cStreamGrowBy);
  try
    lVisitor := TVisStream(AVisClassRef.Create);
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

procedure TtiVisited.IterateRecurse(const AVisitor: TtiVisitor;
  const ADerivedParent: TtiVisited; const ATouchedByVisitorList: TtiTouchedByVisitorList;
  const ATouchMethod: TtiVisitedTouchMethod; const AIterationDepth: TIterationDepth);
var
  LClassPropNames: TStringList;
  LCandidate: TObject;
  i: integer;
  LIterationDepth: TIterationDepth;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(ADerivedParent.TestValid(True), cTIInvalidObjectError);
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  if AVisitor.VisitBranch(ADerivedParent, Self) and
    CheckContinueVisitingIfTopDownRecurse(AVisitor) then
  begin
    LIterationDepth := AIterationDepth + 1;
    if AVisitor.AcceptVisitor(Self) then
      ATouchMethod(Self, AVisitor, ATouchedByVisitorList, LIterationDepth);
    LClassPropNames := TStringList.Create;
    try
      tiGetPropertyNames(Self, LClassPropNames, [tkClass]);
      i := 0;
      while (i <= LClassPropNames.Count - 1) do
      begin
        LCandidate := GetObjectProp(Self, LClassPropNames.Strings[i]);
        if (LCandidate is TtiVisited) then
          (LCandidate as TtiVisited).IterateRecurse(AVisitor,
            Self, ATouchedByVisitorList, ATouchMethod, LIterationDepth)
        else if (LCandidate is TList) then
          IterateOverList(AVisitor, (LCandidate as TList), Self,
            ATouchedByVisitorList, ATouchMethod, LIterationDepth);
        Inc(i);
      end;
    finally
      LClassPropNames.Free;
    end;
  end;
end;

function TtiVisited.CheckContinueVisitingIfTopDownRecurse(const AVisitor: TtiVisitor): boolean;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  if AVisitor.IterationStyle <> isTopDownRecurse then
    Result := True
  else
    Result := ContinueVisiting(AVisitor);
end;

function TtiVisited.ContinueVisiting(const AVisitor: TtiVisitor): boolean;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Result := AVisitor.ContinueVisiting and not Terminated;
end;

constructor TtiVisited.Create;
begin
  inherited Create;
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

procedure TtiVisited.FindAllByClassType(const AClass: TtiVisitedClass; const AList: TList);
var
  lVis: TVisFindAllByClass;
begin
  Assert(Assigned(AClass), 'AClass not assigned');
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


procedure TtiVisited.IterateOverList(const AVisitor: TtiVisitor; const ACandidates: TList;
  const ADerivedParent: TtiVisited; const ATouchedByVisitorList: TtiTouchedByVisitorList;
  const ATouchMethod: TtiVisitedTouchMethod; const AIterationDepth: TIterationDepth);
var
  i: integer;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(Assigned(ACandidates), 'ACandidates not assigned');
  Assert(ADerivedParent.TestValid, cTIInvalidObjectError);
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  Assert(Assigned(ATouchMethod), 'ATouchMethod not assigned');
  i := 0;
  while (i <= ACandidates.Count - 1) do
  begin
    if (TObject(ACandidates.Items[i]) is TtiVisited) then
      TtiVisited(ACandidates.Items[i]).IterateRecurse(AVisitor, ADerivedParent,
        ATouchedByVisitorList, ATouchMethod, AIterationDepth);
    Inc(i);
  end;
end;

function TtiVisited.GetCaption: string;
begin
  Result := ClassName;
end;


function TtiVisited.GetTerminated: boolean;
begin
  Result := TtiOPFManager(TIOPFManager).Terminated;
end;

procedure TtiVisited.IterateTopDownRecurse(const AVisitor: TtiVisitor;
  const ATouchedByVisitorList: TtiTouchedByVisitorList);
var
  LTouchedObjectList: TtiTouchedByVisitorList;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  LTouchedObjectList := TtiTouchedByVisitorList.Create(False);
  try
    IterateRecurse(AVisitor, nil, LTouchedObjectList,
      TouchMethodExecuteVisitor, 0);
  finally
    ATouchedByVisitorList.AppendTopDown(LTouchedObjectList);
    LTouchedObjectList.Free;
  end;
end;


procedure TtiVisited.IterateTopDownSinglePass(const AVisitor: TtiVisitor;
  const ATouchedByVisitorList: TtiTouchedByVisitorList);
var
  LTouchedObjectList: TtiTouchedByVisitorList;
  i: integer;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  LTouchedObjectList := TtiTouchedByVisitorList.Create(False);
  try
    IterateRecurse(AVisitor, nil, LTouchedObjectList, TouchMethodAddToList, 0);
    i := 0;
    while (i <= LTouchedObjectList.Count - 1) and
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
  Assert(GTIOPFManager.TestValid, cTIInvalidObjectError);
  Result := GTIOPFManager;
end;

procedure TtiVisited.TouchMethodAddToList(const ACandidates: TtiVisited;
  const AVisitor: TtiVisitor; const ATouchedByVisitorList: TtiTouchedByVisitorList;
  const AIterationDepth: TIterationDepth);
var
  LVisitedCandidate: TtiTouchedByVisitor;
begin
  Assert(ACandidates.TestValid, cTIInvalidObjectError);
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  LVisitedCandidate := TtiTouchedByVisitor.Create(AVisitor,
    ACandidates, AIterationDepth);
  ATouchedByVisitorList.Add(LVisitedCandidate);
end;

procedure TtiVisited.TouchMethodExecuteVisitor(const ACandidates: TtiVisited;
  const AVisitor: TtiVisitor; const ATouchedByVisitorList: TtiTouchedByVisitorList;
  const AIterationDepth: TIterationDepth);
var
  LVisitedCandidate: TtiTouchedByVisitor;
begin
  Assert(ACandidates.TestValid, cTIInvalidObjectError);
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  LVisitedCandidate := TtiTouchedByVisitor.Create(AVisitor,
    ACandidates, AIterationDepth);
  ATouchedByVisitorList.Add(LVisitedCandidate);
  ExecuteVisitor(AVisitor, LVisitedCandidate);
end;

{ TtiVisitor }

function TtiVisitor.AcceptVisitor(const AVisited: TtiVisited): boolean;
begin
  Assert(AVisited.TestValid, cTIInvalidObjectError);
  SetVisited(AVisited);
  Result := AcceptVisitor;
end;

constructor TtiVisitor.Create;
begin
  inherited Create;
  FContinueVisiting := True;
  FDepth          := 0;
  FIterationStyle := isTopDownRecurse;
end;

function TtiVisitor.AcceptVisitor: boolean;
begin
  Result := True;
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
  Assert(AVisited.TestValid, cTIInvalidObjectError);
  FVisited := AVisited;
end;


function TtiVisitor.VisitBranch(const ADerivedParent, AVisited: TtiVisited): boolean;
begin
  Assert(ADerivedParent.TestValid(True), cTIInvalidObjectError);
  Assert(AVisited.TestValid, cTIInvalidObjectError);
  Result := True;
end;

class function TtiVisitor.VisitorControllerClass: TtiVisitorControllerClass;
begin
  Result := TtiVisitorController;
end;


{ TtiVisitorCtrlr }

procedure TtiVisitorController.AfterExecuteVisitorGroup(
  const ATouchedByVisitorList: TtiTouchedByVisitorList);
begin
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  // Getting rid of compiler hints, param not used here.
  // Do nothing
end;


procedure TtiVisitorController.AfterExecuteVisitorGroupError;
begin
  // Do nothing
end;


procedure TtiVisitorController.AfterExecuteVisitor(const AVisitor: TtiVisitor);
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  // Getting rid of compiler hints, param not used here.
  // Do nothing
end;


procedure TtiVisitorController.BeforeExecuteVisitorGroup;
begin
  // Do nothing
end;


procedure TtiVisitorController.BeforeExecuteVisitor(const AVisitor: TtiVisitor);
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  // Getting rid of compiler hints, param not used here.
  // Do nothing
end;


constructor TtiVisitorController.Create(const AVisitorManager: TtiVisitorManager;
  const AConfig: TtiVisitorControllerConfig);
begin
  Assert(AVisitorManager.TestValid(TtiVisitorManager, True),
    cTIInvalidObjectError);
  Assert(AConfig.TestValid, cTIInvalidObjectError);
  inherited Create;
  FVisitorManager := AVisitorManager;
  FConfig         := AConfig;
  FTouchedByVisitorList := TtiTouchedByVisitorList.Create(True);
end;


destructor TtiVisitorController.Destroy;
begin
  FTouchedByVisitorList.Free;
  inherited;
end;

function TtiVisitor.GetVisited: TtiVisited;
begin
  Result := FVisited;
end;


procedure TtiVisitor.SetDepth(const ADepth: TIterationDepth);
begin
  FDepth := ADepth;
end;

procedure TtiVisitor.SetVisited(const AValue: TtiVisited);
begin
  Assert(AValue.TestValid(TtiVisited, True), cTIInvalidObjectError);
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
  ClassCount[AVisited.ClassType] := ClassCount[AVisited.ClassType] + 1;
end;


function TVisClassCount.GetClassCount(const AClass: TClass): integer;
begin
  Assert(Assigned(AClass), 'AClass not assigned');
  Result := StrToIntDef(FList.Values[AClass.ClassName], 0);
end;


procedure TVisClassCount.SetClassCount(const AClass: TClass; const AValue: integer);
begin
  Assert(Assigned(AClass), 'AClass not assigned');
  FList.Values[AClass.ClassName] := IntToStr(AValue);
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
  Result := FStream.AsString;
end;


procedure TtiVisited.Iterate(const AVisitor: TtiVisitor);
var
  LTouchedByVisitorList: TtiTouchedByVisitorList;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  LTouchedByVisitorList := TtiTouchedByVisitorList.Create(True);
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
    isTopDownRecurse: IterateTopDownRecurse(AVisitor, ATouchedByVisitorList);
    isTopDownSinglePass: IterateTopDownSinglePass(AVisitor,
        ATouchedByVisitorList);
    isBottomUpSinglePass: IterateBottomUpSinglePass(AVisitor,
        ATouchedByVisitorList);
    else
      raise EtiOPFProgrammerException.Create(CErrorInvalidIterationStyle);
  end;
end;

procedure TtiVisited.IterateBottomUpSinglePass(const AVisitor: TtiVisitor;
  const ATouchedByVisitorList: TtiTouchedByVisitorList);
var
  LTouchedObjectList: TtiTouchedByVisitorList;
  i: integer;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(ATouchedByVisitorList.TestValid, cTIInvalidObjectError);
  LTouchedObjectList := TtiTouchedByVisitorList.Create(False);
  try
    IterateRecurse(AVisitor, nil, LTouchedObjectList, TouchMethodAddToList, 0);
    i := LTouchedObjectList.Count - 1;
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
  Result := Visited is FClassTypeToFind;
end;


procedure TVisFindAllByClass.Execute(const AVisited: TtiVisited);
begin
  Assert(AVisited.TestValid, cTIInvalidObjectError);
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
  FTIOPFManager     := ATIOPFManager;
  FSynchronizer     := TMultiReadExclusiveWriteSynchronizer.Create;
  FVisitorMappings  := TObjectList.Create;
end;


destructor TtiVisitorManager.Destroy;
begin
  FVisitorMappings.Free;
  FreeAndNil(FSynchronizer);
  inherited;
end;

function TtiVisitorManager.Execute(const AGroupName: string; const AVisited: TtiVisited): string;
var
  LVisitorControllerConfig: TtiVisitorControllerConfig;
begin
  Assert(AGroupName<>'', 'AGroupName not assigned');
  Assert(AVisited.TestValid, cTIInvalidObjectError);
  LVisitorControllerConfig := TtiVisitorControllerConfig.Create(Self);
  try
    ProcessVisitors(AGroupName, AVisited, LVisitorControllerConfig);
  finally
    LVisitorControllerConfig.Free;
  end;
end;

procedure TtiVisitorManager.ExecuteVisitors(const AVisitorController: TtiVisitorController;
  const AVisitors: TList; const AVisited: TtiVisited);
var
  LVisitor: TtiVisitor;
  i:        integer;
begin
  Assert(AVisitorController.TestValid, cTIInvalidObjectError);
  Assert(Assigned(AVisitors), 'AVisitors not assigned');
  Assert(AVisited.TestValid, cTIInvalidObjectError);
  for i := 0 to AVisitors.Count - 1 do
  begin
    LVisitor := TtiVisitor(AVisitors.Items[i]);
    AVisitorController.BeforeExecuteVisitor(LVisitor);
    try
      if AVisited <> nil then
        AVisited.IterateAssignTouched(LVisitor,
          AVisitorController.TouchedByVisitorList)
      else
        LVisitor.Execute(nil);
    finally
      AVisitorController.AfterExecuteVisitor(LVisitor);
    end;
  end;
end;


function TtiVisitorManager.FindVisitorMappingGroup(const AGroupName: string):
TtiVisitorMappingGroup;
var
  i:          integer;
  LGroupName: string;
begin
  Assert(AGroupName<>'', 'AGroupName not assigned');
  Result     := nil;
  LGroupName := UpperCase(AGroupName);
  for i := 0 to FVisitorMappings.Count - 1 do
    if (FVisitorMappings.Items[i] as TtiVisitorMappingGroup).GroupName =
      LGroupName then
    begin
      Result := FVisitorMappings.Items[i] as TtiVisitorMappingGroup;
      Exit; //==>
    end;
end;

function TtiVisitorManager.GetTIOPFManager: TtiBaseObject;
begin
  Assert(FTIOPFManager.TestValid, cTIInvalidObjectError);
  Result := FTIOPFManager;
end;

function TtiVisitorManager.GetVisitorMappings: TList;
begin
  Result := FVisitorMappings;
end;

procedure TtiVisitorManager.ProcessVisitors(const AGroupName: string;
  const AVisited: TtiVisited; const AVisitorControllerConfig: TtiVisitorControllerConfig);
var
  LVisitorMappingGroup: TtiVisitorMappingGroup;
  LVisitorController: TtiVisitorController;
  LVisitors: TObjectList;
begin
  Assert(AGroupName<>'', 'AGroupName not assigned');
  Assert(AVisited.TestValid, cTIInvalidObjectError);
  Assert(AVisitorControllerConfig.TestValid, cTIInvalidObjectError);
  Log('About to process visitors for <' + AGroupName + '>', lsVisitor);
  LVisitors := TObjectList.Create;
  try
    FSynchronizer.BeginRead;
    try
      LVisitorMappingGroup := FindVisitorMappingGroup(AGroupName);
      if LVisitorMappingGroup = nil then
        raise EtiOPFProgrammerException.CreateFmt(CErrorInvalidVisitorGroup,
          [AGroupName]);
      LVisitorController :=
        LVisitorMappingGroup.VisitorControllerClass.Create(Self,
        AVisitorControllerConfig);
      LVisitorMappingGroup.AssignVisitorInstances(LVisitors);
      try
        LVisitorController.BeforeExecuteVisitorGroup;
        try
          ExecuteVisitors(LVisitorController, LVisitors, AVisited);
          LVisitorController.AfterExecuteVisitorGroup(
            LVisitorController.TouchedByVisitorList);
        except
          on e: Exception do
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


procedure TtiVisitorManager.RegisterVisitor(const AGroupName: string;
  const AVisitorClass: TtiVisitorClass);
var
  LVisitorMappingGroup: TtiVisitorMappingGroup;
begin
  Assert(AGroupName<>'', 'AGroupName not assigned');
  Assert(Assigned(AVisitorClass), 'AVisitorClass not assigned');
  FSynchronizer.BeginWrite;
  try
    LVisitorMappingGroup := FindVisitorMappingGroup(AGroupName);
    if LVisitorMappingGroup = nil then
    begin
      LVisitorMappingGroup := TtiVisitorMappingGroup.Create(AGroupName,
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
  Assert(AGroupName<>'', 'AGroupName not assigned');
  FSynchronizer.BeginWrite;
  try
    LVisitorMappingGroup := FindVisitorMappingGroup(AGroupName);
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
    raise EtiOPFProgrammerException.CreateFmt(
      CErrorIncompatibleVisitorController,
      [VisitorControllerClass.ClassName,
      AVisitorClass.VisitorControllerClass.ClassName]);
  for i := 0 to FMappings.Count - 1 do
    if FMappings.Items[i] = AVisitorClass then
      raise EtiOPFProgrammerException.CreateFmt(
        CErrorAttemptToRegisterDuplicateVisitor, [AVisitorClass.ClassName]);
  FMappings.Add(AVisitorClass);
end;

procedure TtiVisitorMappingGroup.AssignVisitorInstances(const AVisitorList: TObjectList);
var
  i: integer;
begin
  Assert(Assigned(AVisitorList), 'AVisitors not assigned');
  for i := 0 to FMappings.Count - 1 do
    AVisitorList.Add(TtiVisitorClass(FMappings.Items[i]).Create);
end;

constructor TtiVisitorMappingGroup.Create(const AGroupName: string;
  const AVisitorControllerClass: TtiVisitorControllerClass);
begin
  Assert(AGroupName <> '', 'AGroupName not assigned');
  inherited Create;
  FGroupName := UpperCase(AGroupName);
  FMappings  := TClassList.Create;
  FVisitorControllerClass := AVisitorControllerClass;
end;

destructor TtiVisitorMappingGroup.Destroy;
begin
  FMappings.Free;
  inherited;
end;

{ TtiTouchedByVisitor }

constructor TtiTouchedByVisitor.Create(const AVisitor: TtiVisitor;
  const AVisited: TtiVisited; const AIterationDepth: TIterationDepth);
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Assert(AVisited.TestValid, cTIInvalidObjectError);
  inherited Create;
  FVisitor        := AVisitor;
  FVisited        := AVisited;
  FIterationDepth := AIterationDepth;
end;

{ TtiTouchedByVisitorList }

procedure TtiTouchedByVisitorList.Add(const AItem: TtiTouchedByVisitor);
begin
  Assert(AItem.TestValid, cTIInvalidObjectError);
  FList.Add(AItem);
end;

procedure TtiTouchedByVisitorList.AppendBottomUp(const AList: TtiTouchedByVisitorList);
var
  i: integer;
begin
  Assert(AList.TestValid, cTIInvalidObjectError);
  Assert(not AList.FList.OwnsObjects,
    'AList.FList.OwnsObjects is True. Can not append from a list with OwnsObjects = True');
  Assert(FList.OwnsObjects,
    'FList.OwnsObjects is False. Can not append to a list with OwnsObjects = False');
  for i := AList.Count - 1 downto 0 do
    Add(AList.Items[i]);
end;

procedure TtiTouchedByVisitorList.AppendTopDown(const AList: TtiTouchedByVisitorList);
var
  i: integer;
begin
  Assert(AList.TestValid, cTIInvalidObjectError);
  Assert(not AList.FList.OwnsObjects,
    'AList.FList.OwnsObjects is True. Can not append from a list with OwnsObjects = True');
  Assert(FList.OwnsObjects,
    'FList.OwnsObjects is False. Can not append to a list with OwnsObjects = False');
  for i := 0 to AList.Count - 1 do
    Add(AList.Items[i]);
end;

constructor TtiTouchedByVisitorList.Create(const AOwnsObjects: boolean);
begin
  inherited Create;
  FList := TObjectList.Create(AOwnsObjects);
end;

destructor TtiTouchedByVisitorList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TtiTouchedByVisitorList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TtiTouchedByVisitorList.GetItems(const AIndex: integer): TtiTouchedByVisitor;
begin
  Result := FList.Items[AIndex] as TtiTouchedByVisitor;
end;

{ TtiVisitorControllerConfig }

constructor TtiVisitorControllerConfig.Create(const AVisitorManager: TtiVisitorManager);
begin
  Assert(AVisitorManager.TestValid, cTIInvalidObjectError);
  inherited Create;
  FVisitorManager := AVisitorManager;
end;

end.
