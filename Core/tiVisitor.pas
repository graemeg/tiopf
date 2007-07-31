unit tiVisitor;

{$I tiDefines.inc}

// ToDo:
//    Implement AcceptVisitor in Iterate method
//      Have I done this correctly?
//      Can calls to AcceptVisitor be removed from Execute?

//    Refactor so GetAllToVisit recurses into Iterate

//    Implement Terminated

//    Audit for const params
//    Group visitors by registered name
//    Refactor VisitorController to remove DB smell
//    Audit & test BreakOnException code
//    Audit for unit tests

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
  CErrorInVisitorExecute = 'Error in %s.Execute(%s) Message: %s';
  CErrorInvalidIterationStyle = 'Invalid TtiIterationStyle';

type
  TtiIterationStyle = (isTopDownRecurse, isTopDownSinglePass, isBottomUpSinglePass);

  {$M+}
  TtiVisited = class;
  {$M-}
  TtiVisitor = class;

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
                                     const AList: TList;
                                     const AIterationDepth: TIterationDepth) of object;

  TtiVisitedCandidate = class(TtiBaseObject)
  private
    FVisited: TtiVisited;
    FApparentOwner: TtiVisited;
    FIterationDepth: TIterationDepth;
  public
    property ApparentOwner: TtiVisited read FApparentOwner write FApparentOwner;
    property Visited: TtiVisited read FVisited write FVisited;
    property IterationDepth: TIterationDepth read FIterationDepth write FIterationDepth;
  end;

  // TtiVisited
  // The class that gets visited.
  TtiVisited = class(TtiBaseObject)
  protected
    function    GetCaption: string; virtual;
    procedure   Iterate(const AVisitor : TtiVisitor;
                        const ADerivedParent: TtiVisited;
                        const ATouchedObjectList: TList;
                        const ATouchMethod: TtiVisitedTouchMethod;
                        const AIterationDepth: TIterationDepth); overload; virtual;
    procedure   IterateOverList(const AVisitor: TtiVisitor;
                        const ACandidates: TList;
                        const ADerivedParent: TtiVisited;
                        const ATouchecdObjectList: TList;
                        const ATouchMethod: TtiVisitedTouchMethod;
                        const AIterationDepth: TIterationDepth);
    procedure   IterateTopDownRecurse(AVisitor : TtiVisitor); virtual;
    procedure   IterateTopDownSinglePass(AVisitor: TtiVisitor); virtual;
    procedure   IterateBottomUpSinglePass(AVisitor: TtiVisitor); virtual;
    procedure   TouchMethodAddToList(const ACandidates: TtiVisited;
                                      const AVisitor : TtiVisitor;
                                      const AList: TList;
                                      const AIterationDepth: TIterationDepth);
    procedure   TouchMethodExecuteVisitor(const ACandidates: TtiVisited;
                                      const AVisitor : TtiVisitor;
                                      const AList: TList;
                                      const AIterationDepth: TIterationDepth);
    procedure   ExecuteVisitor(const AVisitor: TtiVisitor; const AVisitedCandidate: TtiVisitedCandidate);
    function    GetTerminated: boolean; virtual;
    function    ContinueVisiting(const AVisitor: TtiVisitor): boolean; virtual;
    function    CheckContinueVisitingIfTopDownRecurse(const AVisitor: TtiVisitor): boolean; virtual;
  published
    property    Caption   : string  read GetCaption;
  public
    constructor Create; virtual;
    procedure   Iterate(const AVisitor : TtiVisitor); overload;
    procedure   FindAllByClassType(AClass : TtiVisitedClass; AList : TList);
    property    Terminated: Boolean read GetTerminated;
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

  // TtiVisitor: The class that does the visiting
  TtiVisitor = class(TtiBaseObject)
  private
    FVisited          : TtiVisited;
    FContinueVisiting : boolean;
    FVisitorController : TtiVisitorCtrlr;
    FDepth: TIterationDepth;
    FIterationStyle: TtiIterationStyle;
    FVisitedsOwner: TtiVisited;
  protected
    function    AcceptVisitor : boolean; overload; virtual;
    function    AcceptVisitor(AVisited: TtiVisited) : boolean; overload; virtual;
    function    VisitBranch(const ADerivedParent, AVisited: TtiVisited) : boolean; virtual;
    function    GetVisited: TtiVisited; virtual;
    procedure   SetVisited(const AValue: TtiVisited);
    procedure   SetDepth(const ADepth: TIterationDepth);
  public
    constructor Create; virtual;

    procedure   Execute(const AVisited : TtiVisited); virtual;
    function    VisitorControllerClass : TtiVisitorControllerClass; virtual;
    property    Visited : TtiVisited read FVisited;

    property    ContinueVisiting : boolean read FContinueVisiting write FContinueVisiting;
    property    VisitorController : TtiVisitorCtrlr read FVisitorController write FVisitorController;
    property    Depth : TIterationDepth read FDepth;
    property    IterationStyle : TtiIterationStyle
                  read  FIterationStyle
                  write FIterationStyle;
    property    VisitedsOwner : TtiVisited read FVisitedsOwner write FVisitedsOwner;

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

procedure TtiVisited.Iterate(
  const AVisitor: TtiVisitor;
  const ADerivedParent: TtiVisited;
  const ATouchedObjectList: TList;
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
      ATouchMethod(Self, AVisitor, ATouchedObjectList, LIterationDepth);
    LClassPropNames := TStringList.Create;
    try
      tiGetPropertyNames(Self, LClassPropNames, [tkClass]);
      i:= 0;
      while (i <= LClassPropNames.Count - 1) do
      begin
        LCandidate := GetObjectProp(Self, LClassPropNames.Strings[i]);
        if (LCandidate is TtiVisited) then
          (LCandidate as TtiVisited).Iterate(AVisitor, (LCandidate as TtiVisited), ATouchedObjectList, ATouchMethod, LIterationDepth)
        else if (LCandidate is TList) then
          IterateOverList(AVisitor, (LCandidate as TList), Self, ATouchedObjectList, ATouchMethod, LIterationDepth);
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
  const AVisitedCandidate: TtiVisitedCandidate);
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
  const ATouchecdObjectList: TList;
  const ATouchMethod: TtiVisitedTouchMethod;
  const AIterationDepth: TIterationDepth);
var
  i: integer;
begin
  i:= 0;
  while (i <= ACandidates.Count - 1) do
  begin
    if (TObject(ACandidates.Items[i]) is TtiVisited) then
      TtiVisited(ACandidates.Items[i]).Iterate(AVisitor, ADerivedParent,
              ATouchecdObjectList, ATouchMethod, AIterationDepth);
    inc(i);
  end;
end;

function TtiVisited.GetCaption: string;
begin
  result := className;
end;


function TtiVisited.GetTerminated: boolean;
begin
  result:= gTIOPFManager.Terminated;
end;

procedure TtiVisited.IterateTopDownRecurse(AVisitor: TtiVisitor);
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  Iterate(AVisitor, nil, nil, TouchMethodExecuteVisitor, 0);
end;


procedure TtiVisited.IterateTopDownSinglePass(AVisitor: TtiVisitor);
var
  LList: TObjectList;
  i : integer;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  LList:= TObjectList.Create(True);
  try
    Iterate(AVisitor, nil, LList, TouchMethodAddToList, 0);
    i:= 0;
    while (i <= LList.Count-1) and
      ContinueVisiting(AVisitor) do
    begin
      ExecuteVisitor(AVisitor, TtiVisitedCandidate(LList.Items[i]));
      Inc(i);
    end;
  finally
    LList.Free;
  end;
end;

procedure TtiVisited.TouchMethodAddToList(const ACandidates: TtiVisited;
  const AVisitor: TtiVisitor; const AList: TList; const AIterationDepth: TIterationDepth);
var
  LVisitedCandidate: TtiVisitedCandidate;
begin
  LVisitedCandidate:= TtiVisitedCandidate.Create;
  LVisitedCandidate.Visited:= ACandidates;
//  LVisitedCandidate.ApparentOwner:= AApparentOwner;
  LVisitedCandidate.IterationDepth:= AIterationDepth;
  AList.Add(LVisitedCandidate);
end;

procedure TtiVisited.TouchMethodExecuteVisitor(const ACandidates: TtiVisited;
  const AVisitor: TtiVisitor; const AList: TList; const AIterationDepth: TIterationDepth);
var
  LVisitedCandidate: TtiVisitedCandidate;
begin
  LVisitedCandidate:= TtiVisitedCandidate.Create;
  try
    LVisitedCandidate.Visited:= ACandidates;
  //  LVisitedCandidate.ApparentOwner:= AApparentOwner;
    LVisitedCandidate.IterationDepth:= AIterationDepth;
    ExecuteVisitor(AVisitor, LVisitedCandidate);
  finally
    LVisitedCandidate.Free;
  end;
end;

{ TtiVisitor }

function TtiVisitor.AcceptVisitor(AVisited: TtiVisited): boolean;
begin
  SetVisited(AVisited);
  result:= AcceptVisitor;
end;

constructor TtiVisitor.Create;
begin
  inherited create;
  FContinueVisiting := true;
  FVisitorController := nil;
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
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  case AVisitor.IterationStyle of
  isTopDownRecurse:     IterateTopDownRecurse(AVisitor);
  isTopDownSinglePass:  IterateTopDownSinglePass(AVisitor);
  isBottomUpSinglePass: IterateBottomUpSinglePass(AVisitor);
  else
    raise EtiOPFProgrammerException.Create(CErrorInvalidIterationStyle);
  end;
end;

procedure TtiVisited.IterateBottomUpSinglePass(AVisitor: TtiVisitor);
var
  LList: TObjectList;
  i : integer;
begin
  Assert(AVisitor.TestValid, cTIInvalidObjectError);
  LList:= TObjectList.Create(True);
  try
    Iterate(AVisitor, nil, LList, TouchMethodAddToList, 0);
    i:= LList.Count-1;
    while (i >= 0) and
      ContinueVisiting(AVisitor) do
    begin
      ExecuteVisitor(AVisitor, TtiVisitedCandidate(LList.Items[i]));
      Dec(i);
    end;
  finally
    LList.Free;
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

var
  LVisitor : TtiVisitor;
  i : integer;
begin
  for i := 0 to AVisitors.Count - 1 do
  begin
    // Don't go any further if terminated
    if gTIOPFManager.Terminated then
      Exit; //==>
    LVisitor := TtiVisitor(AVisitors.Items[i]);
    _RunBeforeExecuteOne(LVisitor);
    try
      if AVisited <> nil then
        AVisited.Iterate(LVisitor)
      else
        LVisitor.Execute(nil);
    finally
      _RunAfterExecuteOne(LVisitor);
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












