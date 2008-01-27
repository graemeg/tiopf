unit tiVisitor_TST;

{$I tiDefines.inc}

interface

uses
  Classes
  ,tiTestFramework
  ,tiVisitor
  ,Contnrs
  ;

type
  TTestVisitedList         = class;
  TTestVisitedOwned        = class;
  TTestVisitedListAndOwned = class;

  TTestTIVisitor = class(TtiTestCase)
  private
    function CreateList: TTestVisitedList;
    function CreateOwned: TTestVisitedOwned;
    procedure Visitor_ContinueVisiting(const AIterationStyle: TtiIterationStyle);
    procedure Visited_Iterate_Single(const AIterationStyle: TtiIterationStyle);
    procedure Visited_Iterate_List(const AIterationStyle: TtiIterationStyle);
    procedure Visited_Iterate_Owned(const AIterationStyle: TtiIterationStyle);
    procedure Visited_Iterate_ListAndOwned(const AIterationStyle: TtiIterationStyle);
    procedure Visited_AcceptVisitor(const AIterationStyle: TtiIterationStyle);
    procedure Visited_VisitBranch(const AIterationStyle: TtiIterationStyle);
    procedure Visited_ContinueVisiting(const AIterationStyle: TtiIterationStyle);
    procedure Visited_IterateAssignTouched(const AIterationStyle: TtiIterationStyle);

  protected
    procedure SetUp; override;
  published

    procedure TouchedByVisitor_Create;
    procedure TouchedByVisitorList_Add;
    procedure TouchedByVisitorList_AppendTopDown;
    procedure TouchedByVisitorList_AppendBottomUp;

    // Test the visitor
    procedure Visitor_AcceptVisitor;
    procedure Visitor_Execute;

    procedure Visitor_ContinueVisiting_BottomUpSinglePass;
    procedure Visitor_ContinueVisiting_TopDownSinglePass;
    procedure Visitor_ContinueVisiting_TopDownRecurse;

    procedure Visitor_VisitorControllerClass;

    procedure Visitor_Depth_TopDownRecurse;
    procedure Visitor_Depth_TopDownSinglePass;
    procedure Visitor_Depth_BottomUpSinglePass;

    // Test the visited
    procedure Visited_Caption;
    procedure Visited_Terminated;
    procedure Visited_ContinueVisitingFunction;

    procedure Visited_Iterate_Single_TopDownRecurse;
    procedure Visited_Iterate_Single_TopDownSinglePass;
    procedure Visited_Iterate_Single_BottomUpSinglePass;

    procedure Visited_Iterate_List_TopDownRecurse;
    procedure Visited_Iterate_List_TopDownSinglePass;
    procedure Visited_Iterate_List_BottomUpSinglePass;

    procedure Visited_Iterate_Owned_TopDownRecurse;
    procedure Visited_Iterate_Owned_TopDownSinglePass;
    procedure Visited_Iterate_Owned_BottomUpSinglePass;

    procedure Visited_Iterate_ListAndOwned_TopDownRecurse;
    procedure Visited_Iterate_ListAndOwned_TopDownSinglePass;
    procedure Visited_Iterate_ListAndOwned_BottomUpSinglePass;

    procedure Visited_Iterate_Override;

    procedure Visited_IterateAssignTouched_TopDownRecurse;
    procedure Visited_IterateAssignTouched_TopDownSinglePass;
    procedure Visited_IterateAssignTouched_BottomUpSinglePass;

    procedure Visited_Recurse_TopDownRecurse;
    procedure Visited_Recurse_TopDownSinglePass;
    procedure Visited_Recurse_BottomUpSinglePass;

    procedure Visited_AcceptVisitor_TopDownRecurse;
    procedure Visited_AcceptVisitor_TopDownSinglePass;
    procedure Visited_AcceptVisitor_BottomUpSinglePass;

    procedure Visited_VisitBranch_TopDownRecurse;
    procedure Visited_VisitBranch_TopDownSinglePass;
    procedure Visited_VisitBranch_BottomUpSinglePass;

    procedure Visited_ContinueVisiting_TopDownRecurse;
    procedure Visited_ContinueVisiting_TopDownSinglePass;
    procedure Visited_ContinueVisiting_BottomUpSinglePass;

    // Test some other special visitors
    procedure Visited_FindAllByClassType;

    procedure VisitorMappingGroup_Add;
    procedure VisitorMappingGroup_AssignVisitorInstances;
    procedure VisitorManager_RegisterVisitor;
    procedure VisitorManager_FindVisitorMappingGroup;
    procedure VisitorManager_VisitorController;
    procedure VisitorManager_VisitorControllerException;

    procedure VisClassCount_Execute;
    procedure VisFindAllByClass_Execute;
    procedure VisStream_Execute;
    procedure VisStreamToFile_Execute;
  end;

  TTestVisitor_Execute = class(TtiVisitor)
  private
    FExecuteCalled: boolean;
  public
    property ExecuteCalled: boolean read FExecuteCalled write FExecuteCalled;
    procedure Execute(const AVisited: TtiVisited); override;
  end;

  TTestVisitedAbs = class(TtiVisited)
  private
    FIndex: word;
  public
    property Index: word read FIndex write FIndex;
  end;

  TTestVisited = class(TTestVisitedAbs);

  TTestVisitedList = class(TTestVisitedAbs)
  private
    FList: TObjectList;
    function GetList: TList;
  published
    property List: TList read GetList;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;


  TTestVisitedOwned = class(TTestVisitedAbs)
  private
    FOwned1: TTestVisited;
    FOwned3: TTestVisited;
    FOwned2: TTestVisited;
  published
    property Owned1: TTestVisited read FOwned1;
    property Owned2: TTestVisited read FOwned2;
    property Owned3: TTestVisited read FOwned3;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TTestVisitedListAndOwned = class(TTestVisitedList)
  public
    constructor Create; override;
  end;

  TSensingVisitorAbs = class(TtiVisitor)
  private
    FData: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Data: TStringList read FData;
  end;

  TSensingVisitor = class(TSensingVisitorAbs)
  public
    procedure Execute(const AVisited: TtiVisited); override;
  end;

  TTestVisitorIterate = class(TSensingVisitorAbs)
  public
    procedure Execute(const AVisited: TtiVisited); override;
  end;

  TTestVisitorController = class(TtiVisitor)
  public
    class function VisitorControllerClass: TtiVisitorControllerClass; override;
  end;

  TTestVisitorGetAllToVisit = class(TtiVisitor)
  protected
    function AcceptVisitor: boolean; override;
  end;

  TTestVisStream = class(TVisStream)
  public
    procedure Execute(const AVisited: TtiVisited); override;
    procedure Write(const AValue: string); override;
    procedure WriteLn(const AValue: string = ''); override;
  end;

  TTestVisitorManager = class(TtiVisitorManager)
  public
    property VisitorMappings;
    function FindVisitorMappingGroup(const AGroupName: string): TtiVisitorMappingGroup; override;
  end;

procedure RegisterTests;


implementation

uses
  tiUtils,
  tiOPFManager,
  tiTestDependencies,
  tiStreams,
  tiObject,
  tiExcept,
  TypInfo,
  SysUtils;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIVisitor);
end;


{ TTestTiPtnVis }

procedure TTestTIVisitor.VisClassCount_Execute;
var
  lVisited: TTestVisitedListAndOwned;
  lVis:     TVisClassCount;
begin
  lVisited := TTestVisitedListAndOwned.Create;
  try
    lVis := TVisClassCount.Create;
    try
      lVisited.Iterate(lVis);
      CheckEquals(0, lVis.ClassCount[TTestVisitedList], 'Failed TTestVisitedList');
      CheckEquals(1, lVis.ClassCount[TTestVisitedListAndOwned],
        'Failed TTestVisitedListAndOwned');
      CheckEquals(3, lVis.ClassCount[TTestVisitedOwned], 'Failed TTestVisitedOwned');
      // Probably not what you would expect (13 is more logical) because
      // TTestVisitedList and TTestVisitedOwned are both TestVisited, but
      // TVisClassCount does not know about inheritance.
      // This should be fixed when TVisClassCount is next being used.
      CheckEquals(9, lVis.ClassCount[TTestVisited], 'Failed TTestVisited');
      CheckEquals(0, lVis.ClassCount[TtiVisited], 'Failed TtiVisited');
    finally
      lVis.Free;
    end;
  finally
    lVisited.Free;
  end;
end;


procedure TTestTIVisitor.VisFindAllByClass_Execute;
var
  lVisited: TTestVisitedListAndOwned;
  lVis:     TVisFindAllByClass;
  lList:    TList;
begin
  lVisited := TTestVisitedListAndOwned.Create;
  try
    lList := TList.Create;
    try
      lVis := TVisFindAllByClass.Create;
      try
        lVis.List := lList;

        lVis.ClassTypeToFind := TTestVisitedList;
        lVisited.Iterate(lVis);
        CheckEquals(1, lList.Count, 'Failed TTestVisitedList');

        lList.Clear;
        lVis.ClassTypeToFind := TTestVisitedOwned;
        lVisited.Iterate(lVis);
        CheckEquals(3, lList.Count, 'Failed TTestVisitedOwned');

        lList.Clear;
        lVis.ClassTypeToFind := TtiVisited;
        lVisited.Iterate(lVis);
        CheckEquals(13, lList.Count, 'Failed TtiVisited');

      finally
        lVis.Free;
      end;
    finally
      lList.Free;
    end;
  finally
    lVisited.Free;
  end;
end;


 //procedure TTestTIVisitor.VisGetAllToVisit_Execute;
 //var
 //  lVisGetAllToVisit : TtiVisGetAllToVisit;
 //  lVisitor : TTestVisitorGetAllToVisit;
 //  lVisitedList : TTestVisitedList;
 //begin
 //  lVisitedList := CreateListAndOwned;
 //  try
 //    lVisitor := TTestVisitorGetAllToVisit.Create;
 //    try
 //      lVisGetAllToVisit := TtiVisGetAllToVisit.Create;
 //      try
 //        lVisGetAllToVisit.Visitor := lVisitor;
 //        lVisitedList.Iterate(lVisGetAllToVisit);
 //        CheckEquals(3, lVisGetAllToVisit.List.Count);
 //      finally
 //        lVisGetAllToVisit.Free;
 //      end;
 //    finally
 //      lVisitor.Free;
 //    end;
 //  finally
 //    lVisitedList.Free;
 //  end;
 //end;

type

  TTestTIVisitedAcceptVisitor = class(TtiVisited)
  private
    FVisit: boolean;
    FID:    string;
    FChain: TtiVisited;
  public
    property Visit: boolean read FVisit write FVisit;
    property ID: string read FID write FID;
  published
    property Chain: TtiVisited read FChain write FChain;
  end;

  TTestVisitorAcceptVisitor = class(TSensingVisitorAbs)
  protected
    function AcceptVisitor: boolean; override;
  public
    procedure Execute(const AVisited: TtiVisited); override;
  end;

function TTestVisitorAcceptVisitor.AcceptVisitor: boolean;
begin
  Result := (Visited is TTestTIVisitedAcceptVisitor) and
    (Visited as TTestTIVisitedAcceptVisitor).Visit;
end;

procedure TTestTIVisitor.Visited_AcceptVisitor(const AIterationStyle: TtiIterationStyle);
var
  LItem1:   TTestTIVisitedAcceptVisitor;
  LItem2:   TTestTIVisitedAcceptVisitor;
  LItem3:   TTestTIVisitedAcceptVisitor;
  LItem4:   TTestTIVisitedAcceptVisitor;
  LVisitor: TTestVisitorAcceptVisitor;
begin
  LItem1   := nil;
  LItem2   := nil;
  LItem3   := nil;
  LItem4   := nil;
  LVisitor := nil;
  try
    LItem1 := TTestTIVisitedAcceptVisitor.Create;

    LItem1.ID    := '1';
    LItem1.Visit := False;

    LItem2       := TTestTIVisitedAcceptVisitor.Create;
    LItem2.ID    := '2';
    LItem2.Visit := True;
    LItem1.Chain := LItem2;

    LItem3       := TTestTIVisitedAcceptVisitor.Create;
    LItem3.ID    := '3';
    LItem3.Visit := True;
    LItem2.Chain := LItem3;

    LItem4       := TTestTIVisitedAcceptVisitor.Create;
    LItem4.ID    := '3';
    LItem4.Visit := False;
    LItem3.Chain := LItem4;

    LVisitor := TTestVisitorAcceptVisitor.Create;
    LVisitor.IterationStyle := AIterationStyle;
    LItem1.Iterate(LVisitor);
    CheckEquals(2, LVisitor.Data.Count);
    if AIterationStyle = isBottomUpSinglePass then
    begin
      CheckEquals('3', LVisitor.Data.Strings[0]);
      CheckEquals('2', LVisitor.Data.Strings[1]);
    end
    else
    begin
      CheckEquals('2', LVisitor.Data.Strings[0]);
      CheckEquals('3', LVisitor.Data.Strings[1]);
    end;

  finally
    LItem1.Free;
    LItem2.Free;
    LItem3.Free;
    LItem4.Free;
    LVisitor.Free;
  end;
end;

procedure TTestTIVisitor.Visited_AcceptVisitor_BottomUpSinglePass;
begin
  Visited_AcceptVisitor(isBottomUpSinglePass);
end;

procedure TTestTIVisitor.Visited_AcceptVisitor_TopDownRecurse;
begin
  Visited_AcceptVisitor(isTopDownRecurse);
end;

procedure TTestTIVisitor.Visited_AcceptVisitor_TopDownSinglePass;
begin
  Visited_AcceptVisitor(isTopDownSinglePass);
end;

procedure TTestTIVisitor.Visited_Caption;
var
  lVisited: TtiVisited;
begin
  lVisited := TtiVisited.Create;
  try
    CheckEquals(lVisited.Caption, lVisited.ClassName);
  finally
    lVisited.Free;
  end;
end;

procedure TTestTIVisitor.Visited_FindAllByClassType;
var
  lVisitedList: TTestVisitedListAndOwned;
  lList:        TList;
begin
  lVisitedList := TTestVisitedListAndOwned.Create;
  try
    lList := TList.Create;
    try
      lVisitedList.FindAllByClassType(TTestVisitedList, lList);
      CheckEquals(1, lList.Count, 'Failed on FindAllByClassType(TTestVisitedList)');

      lVisitedList.FindAllByClassType(TTestVisitedOwned, lList);
      CheckEquals(3, lList.Count, 'Failed on FindAllByClassType(TTestVisitedOwned)');

      lVisitedList.FindAllByClassType(TtiVisited, lList);
      CheckEquals(13, lList.Count, 'Failed on FindAllByClassType(TtiVisitor)');

    finally
      lList.Free;
    end;
  finally
    lVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visited_Iterate_Single(const AIterationStyle: TtiIterationStyle);
var
  lVisitor: TTestVisitorIterate;
  lVisited: TTestVisitedAbs;
begin
  lVisited := TTestVisitedAbs.Create;
  try
    lVisited.Index := 1;
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle := AIterationStyle;
      lVisited.Iterate(lVisitor);
      CheckEquals(1, lVisitor.Data.Count);
      CheckEquals('1', lVisitor.Data.Strings[0]);
    finally
      lVisitor.Free;
    end;
  finally
    lVisited.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Iterate_Single_BottomUpSinglePass;
begin
  Visited_Iterate_Single_TopDownRecurse;
end;

type

  TTestVisitor_AcceptVisitor = class(TtiVisitor)
  private
    FResult: boolean;
  public
    function AcceptVisitor: boolean; override;
    property Result: boolean read FResult write FResult;
  end;

function TTestVisitor_AcceptVisitor.AcceptVisitor: boolean;
begin
  Result := FResult;
end;

procedure TTestTIVisitor.Visitor_AcceptVisitor;
var
  LVisitor: TTestVisitor_AcceptVisitor;
  LVisited: TtiVisited;
begin
  LVisitor := nil;
  LVisited := nil;
  try
    LVisitor := TTestVisitor_AcceptVisitor.Create;
    LVisited := TtiVisited.Create;

    LVisitor.Result := True;
    Check(LVisitor.AcceptVisitor);
    Check(LVisitor.AcceptVisitor(LVisited));
    CheckSame(LVisited, LVisitor.Visited);

    LVisitor.SetVisited(nil);
    CheckNull(LVisitor.Visited);

    LVisitor.Result := False;
    Check(not LVisitor.AcceptVisitor);
    Check(not LVisitor.AcceptVisitor(LVisited));
    CheckSame(LVisited, LVisitor.Visited)

  finally
    LVisitor.Free;
    LVisited.Free;
  end;
end;

type

  TTestVisitorContinueVisiting = class(TTestVisitorIterate)
  private
    FCountVisited: integer;
  public
    constructor Create; override;
    procedure Execute(const AVisited: TtiVisited); override;
  end;

constructor TTestVisitorContinueVisiting.Create;
begin
  inherited;
  FCountVisited := 0;
end;

procedure TTestVisitorContinueVisiting.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  Inc(FCountVisited);
  ContinueVisiting := FCountVisited < 5;
end;

procedure TTestTIVisitor.Visitor_ContinueVisiting(const AIterationStyle: TtiIterationStyle);
var
  LVisitor:     TTestVisitorContinueVisiting;
  LVisitedList: TTestVisitedListAndOwned;
begin
  LVisitedList := TTestVisitedListAndOwned.Create;
  try
    LVisitor := TTestVisitorContinueVisiting.Create;
    LVisitor.IterationStyle := AIterationStyle;
    try
      LVisitedList.Iterate(LVisitor);
      CheckEquals(5, LVisitor.Data.Count);
    finally
      LVisitor.Free;
    end;
  finally
    LVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visitor_ContinueVisiting_BottomUpSinglePass;
begin
  Visitor_ContinueVisiting(isBottomUpSinglePass);
end;

procedure TTestTIVisitor.Visitor_ContinueVisiting_TopDownRecurse;
begin
  Visitor_ContinueVisiting(isTopDownRecurse);
end;

procedure TTestTIVisitor.Visitor_ContinueVisiting_TopDownSinglePass;
begin
  Visitor_ContinueVisiting(isTopDownSinglePass);
end;

type

  TTestVisitorDepth = class(TtiVisitor)
  private
    LResults: TStringList;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Execute(const AVisited: TtiVisited); override;
    property Results: TStringList read LResults;
  end;

constructor TTestVisitorDepth.Create;
begin
  inherited;
  LResults := TStringList.Create;
end;

destructor TTestVisitorDepth.Destroy;
begin
  LResults.Free;
  inherited;
end;

procedure TTestVisitorDepth.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  Assert(AVisited is TTestVisitedAbs, 'AVisited not a TTestVisitedAbs');
  LResults.Add(IntToStr(Depth));
end;

procedure TTestTIVisitor.Visitor_Depth_BottomUpSinglePass;
var
  LVisitor:     TTestVisitorDepth;
  LVisitedList: TTestVisitedListAndOwned;
begin
  LVisitedList := TTestVisitedListAndOwned.Create;
  try
    LVisitor := TTestVisitorDepth.Create;
    LVisitor.IterationStyle := isBottomUpSinglePass;
    try
      LVisitedList.Iterate(LVisitor);
      CheckEquals('1', LVisitor.Results.Strings[12]);
      CheckEquals('2', LVisitor.Results.Strings[11]);
      CheckEquals('3', LVisitor.Results.Strings[10]);
      CheckEquals('3', LVisitor.Results.Strings[9]);
      CheckEquals('3', LVisitor.Results.Strings[8]);
      CheckEquals('2', LVisitor.Results.Strings[7]);
      CheckEquals('3', LVisitor.Results.Strings[6]);
      CheckEquals('3', LVisitor.Results.Strings[5]);
      CheckEquals('3', LVisitor.Results.Strings[4]);
      CheckEquals('2', LVisitor.Results.Strings[3]);
      CheckEquals('3', LVisitor.Results.Strings[2]);
      CheckEquals('3', LVisitor.Results.Strings[1]);
      CheckEquals('3', LVisitor.Results.Strings[0]);
    finally
      LVisitor.Free;
    end;
  finally
    LVisitedList.Free;
  end;
end;

procedure TTestTIVisitor.Visitor_Depth_TopDownRecurse;
var
  LVisitor:     TTestVisitorDepth;
  LVisitedList: TTestVisitedListAndOwned;
begin
  LVisitedList := TTestVisitedListAndOwned.Create;
  try
    LVisitor := TTestVisitorDepth.Create;
    LVisitor.IterationStyle := isTopDownRecurse;
    try
      LVisitedList.Iterate(LVisitor);
      CheckEquals('1', LVisitor.Results.Strings[0]);
      CheckEquals('2', LVisitor.Results.Strings[1]);
      CheckEquals('3', LVisitor.Results.Strings[2]);
      CheckEquals('3', LVisitor.Results.Strings[3]);
      CheckEquals('3', LVisitor.Results.Strings[4]);
      CheckEquals('2', LVisitor.Results.Strings[5]);
      CheckEquals('3', LVisitor.Results.Strings[6]);
      CheckEquals('3', LVisitor.Results.Strings[7]);
      CheckEquals('3', LVisitor.Results.Strings[8]);
      CheckEquals('2', LVisitor.Results.Strings[9]);
      CheckEquals('3', LVisitor.Results.Strings[10]);
      CheckEquals('3', LVisitor.Results.Strings[11]);
      CheckEquals('3', LVisitor.Results.Strings[12]);
    finally
      LVisitor.Free;
    end;
  finally
    LVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visitor_Depth_TopDownSinglePass;
var
  LVisitor:     TTestVisitorDepth;
  LVisitedList: TTestVisitedListAndOwned;
begin
  LVisitedList := TTestVisitedListAndOwned.Create;
  try
    LVisitor := TTestVisitorDepth.Create;
    LVisitor.IterationStyle := isTopDownSinglePass;
    try
      LVisitedList.Iterate(LVisitor);
      CheckEquals('1', LVisitor.Results.Strings[0]);
      CheckEquals('2', LVisitor.Results.Strings[1]);
      CheckEquals('3', LVisitor.Results.Strings[2]);
      CheckEquals('3', LVisitor.Results.Strings[3]);
      CheckEquals('3', LVisitor.Results.Strings[4]);
      CheckEquals('2', LVisitor.Results.Strings[5]);
      CheckEquals('3', LVisitor.Results.Strings[6]);
      CheckEquals('3', LVisitor.Results.Strings[7]);
      CheckEquals('3', LVisitor.Results.Strings[8]);
      CheckEquals('2', LVisitor.Results.Strings[9]);
      CheckEquals('3', LVisitor.Results.Strings[10]);
      CheckEquals('3', LVisitor.Results.Strings[11]);
      CheckEquals('3', LVisitor.Results.Strings[12]);
    finally
      LVisitor.Free;
    end;
  finally
    LVisitedList.Free;
  end;
end;

procedure TTestTIVisitor.Visitor_Execute;
var
  lVisitor: TTestVisitor_Execute;
  lVisited: TtiVisited;
begin
  lVisitor := TTestVisitor_Execute.Create;
  try
    lVisited := TtiVisited.Create;
    try
      lVisitor.ExecuteCalled := False;
      lVisitor.Execute(lVisited);
      Check(lVisitor.ExecuteCalled);
    finally
      lVisited.Free;
    end;
  finally
    lVisitor.Free;
  end;
end;


procedure TTestTIVisitor.Visitor_VisitorControllerClass;
var
  lVis: TtiVisitor;
begin
  lVis := TTestVisitorController.Create;
  try
    CheckEquals(lVis.VisitorControllerClass, TtiVisitorController);
  finally
    lVis.Free;
  end;
end;

{
  TVisStream = class(TtiVisitor)
  private
    FStream : TStream;
  protected
    procedure Write(const AValue : string);
    procedure WriteLn; overload;
    procedure WriteLn(const AValue : string); overload;
    procedure SetStream(const AValue: TStream); virtual;
  public
    property  Stream : TStream read FStream write SetStream;
  end;
}


procedure TTestTIVisitor.VisStream_Execute;
var
  lVis:         TTestVisStream;
  lStream:      TtiPreSizedStream;
  lVisitedList: TTestVisitedListAndOwned;
const
  cATestLine = 'A test line';
begin
  lVis := TTestVisStream.Create;
  try
    lStream := TtiPreSizedStream.Create(1000, 100);
    try
      lVis.Stream := lStream;
      lVis.Write(cATestLine);
      CheckEquals(cATestLine, lStream.AsString, 'Failed on Write');

      lStream.Clear;
      lVis.WriteLn;
      CheckEquals(#13 + #10, lStream.AsString, 'Failed on WriteLn');

      lStream.Clear;
      lVis.WriteLn(cATestLine);
      CheckEquals(cATestLine + #13 + #10, lStream.AsString, 'Failed on WriteLn(cATestLine)');

      lStream.Clear;
      lVisitedList := TTestVisitedListAndOwned.Create;
      try
        lVisitedList.Iterate(lVis);
        CheckEquals('1,2,3,4,5,6,7,8,9,10,11,12,13', lStream.AsString)
      finally
        lVisitedList.Free;
      end;
    finally
      lStream.Free;
    end;
  finally
    lVis.Free;
  end;
end;


procedure TTestTIVisitor.VisStreamToFile_Execute;

  procedure _CheckFileEquals(const AFileName: TFileName; const AValue: string);
  var
    lFileStream:   TFileStream;
    lStringStream: TStringStream;
  begin
    lFileStream :=
      TFileStream.Create(AFileName,
      fmOpenRead or fmShareDenyNone);
    try
      lStringStream := TStringStream.Create('');
      try
        lStringStream.CopyFrom(lFileStream, lFileStream.Size);
        CheckEquals(AValue, lStringStream.DataString);
      finally
        lStringStream.Free;
      end;
    finally
      lFileStream.Free;
    end;
  end;

var
  LVisitedList: TTestVisitedListAndOwned;
  LFileName:    TFileName;
begin
  LFileName := TempFileName;
  try
    LVisitedList := TTestVisitedListAndOwned.Create;
    try
      tiVisitor.VisStreamToFile(LVisitedList,
        LFileName,
        TTestVisStream);
      _CheckFileEquals(LFileName, '1,2,3,4,5,6,7,8,9,10,11,12,13');

      tiVisitor.VisStreamToFile(LVisitedList,
        LFileName,
        TTestVisStream);
      _CheckFileEquals(LFileName, '1,2,3,4,5,6,7,8,9,10,11,12,13');

      tiVisitor.VisStreamToFile(LVisitedList,
        LFileName,
        TTestVisStream);

    finally
      LVisitedList.Free;
    end;
  finally
    tiDeleteFile(LFileName);
  end;
end;


procedure TTestTIVisitor.Visited_Iterate_List_TopDownRecurse;
begin
  Visited_Iterate_List(isTopDownRecurse);
end;


procedure TTestTIVisitor.Visited_Iterate_List_TopDownSinglePass;
begin
  Visited_Iterate_List(isTopDownSinglePass);
end;

procedure TTestTIVisitor.Visited_Iterate_Owned_TopDownRecurse;
begin
  Visited_Iterate_Owned(isTopDownRecurse);
end;


procedure TTestTIVisitor.Visited_Iterate_Owned_TopDownSinglePass;
begin
  Visited_Iterate_Owned(isTopDownSinglePass);
end;

procedure TTestTIVisitor.Visited_Iterate_Single_TopDownRecurse;
begin
  Visited_Iterate_Single(isTopDownRecurse);
end;


procedure TTestTIVisitor.Visited_Iterate_Single_TopDownSinglePass;
begin
  Visited_Iterate_Single(isTopDownSinglePass);
end;

type

  TTestVisitorTopDownRecurse = class(TtiVisitor)
  private
    FRecurseCount: integer;
  protected
    function AcceptVisitor: boolean; override;
  public
    constructor Create; override;
    procedure Execute(const AVisited: TtiVisited); override;
  end;

function TTestVisitorTopDownRecurse.AcceptVisitor: boolean;
begin
  Result := (Visited is TtiObjectList) and
    (FRecurseCount <= 2);
end;

constructor TTestVisitorTopDownRecurse.Create;
begin
  inherited;
  FRecurseCount := 0;
end;

procedure TTestVisitorTopDownRecurse.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  if not AcceptVisitor then
    Exit; //==>
  Inc(FRecurseCount);
  (Visited as TtiObjectList).Add(TtiObjectList.Create);
end;

procedure TTestTIVisitor.Visited_Recurse_TopDownRecurse;
var
  LData:    TtiObjectList;
  LVisitor: TTestVisitorTopDownRecurse;
begin
  LData := TtiObjectList.Create;
  try
    LVisitor := TTestVisitorTopDownRecurse.Create;
    LVisitor.IterationStyle := isTopDownRecurse;
    try
      LData.Iterate(LVisitor);
      CheckEquals(1, LData.Count);
      CheckEquals(1, (LData.Items[0] as TtiObjectList).Count);
      CheckEquals(1, ((LData.Items[0] as TtiObjectList).Items[0] as TtiObjectList).Count);
      CheckEquals(0, (((LData.Items[0] as TtiObjectList).Items[0] as TtiObjectList).Items[0] as
        TtiObjectList).Count);
    finally
      LVisitor.Free;
    end;
  finally
    LData.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Recurse_TopDownSinglePass;
var
  LData:    TtiObjectList;
  LVisitor: TTestVisitorTopDownRecurse;
begin
  LData := TtiObjectList.Create;
  try
    LVisitor := TTestVisitorTopDownRecurse.Create;
    LVisitor.IterationStyle := isTopDownSinglePass;
    try
      LData.Iterate(LVisitor);
      CheckEquals(1, LData.Count);
      CheckEquals(0, (LData.Items[0] as TtiObjectList).Count);
    finally
      LVisitor.Free;
    end;
  finally
    LData.Free;
  end;
end;

type

  TGetTouchCountEvent = function: byte of object;
  TIncTouchCountEvent = procedure of object;

  TtiVisitedTerminatedAbs = class(TtiVisited)
  private
    FTouchCount:         byte;
    FMaxTouchCount:      byte;
    FOnGetMaxTouchCount: TGetTouchCountEvent;
    FOnGetTouchCount:    TGetTouchCountEvent;
    FOnIncTouchCount:    TIncTouchCountEvent;
    function DoGetMaxTouchCount: byte;
    function DoGetTouchCount: byte;
    procedure DoIncTouchCount;
  protected
    function ContinueVisiting(const AVisitor: TtiVisitor): boolean; override;
    function GetTouchCount: byte; virtual;
  public
    procedure SetMaxTouchCount(const AMaxTouchCount: byte);
    property TouchCount: byte read GetTouchCount;
    property OnGetMaxTouchCount: TGetTouchCountEvent
      read FOnGetMaxTouchCount write FOnGetMaxTouchCount;
    property OnGetTouchCount: TGetTouchCountEvent read FOnGetTouchCount write FOnGetTouchCount;
    property OnIncTouchCount: TIncTouchCountEvent read FOnIncTouchCount write FOnIncTouchCount;
  end;

  TtiVisitedTerminatedData = class(TtiVisitedTerminatedAbs)
  end;

  TtiVisitedTerminatedItem = class(TtiVisitedTerminatedAbs)
  end;

  TtiVisitedTerminated = class(TtiVisitedTerminatedAbs)
  private
    FList: TList;
    FData: TtiVisitedTerminatedData;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Data: TtiVisitedTerminatedData read FData;
    property List: TList read FList;
  end;

function TtiVisitedTerminatedAbs.ContinueVisiting(const AVisitor: TtiVisitor): boolean;
begin
  Assert(Assigned(FOnIncTouchCount), 'FOnIncTouchCount not assigned');
  Assert(Assigned(FOnGetMaxTouchCount), 'FOnGetMaxTouchCount not assigned');
  FOnIncTouchCount;
  Result := TouchCount <= FOnGetMaxTouchCount;
end;

function TtiVisitedTerminatedAbs.DoGetMaxTouchCount: byte;
begin
  Result := FMaxTouchCount;
end;

function TtiVisitedTerminatedAbs.DoGetTouchCount: byte;
begin
  Result := FTouchCount;
end;

procedure TtiVisitedTerminatedAbs.DoIncTouchCount;
begin
  Inc(FTouchCount);
end;

function TtiVisitedTerminatedAbs.GetTouchCount: byte;
begin
  Assert(Assigned(FOnGetTouchCount), 'FOnGetTouchCount not assigned');
  Result := FOnGetTouchCount;
end;

procedure TtiVisitedTerminatedAbs.SetMaxTouchCount(const AMaxTouchCount: byte);
begin
  FTouchCount    := 0;
  FMaxTouchCount := AMaxTouchCount;
end;

constructor TtiVisitedTerminated.Create;
begin
  inherited;
  OnGetMaxTouchCount := DoGetMaxTouchCount;
  OnGetTouchCount := DoGetTouchCount;
  OnIncTouchCount := DoIncTouchCount;
  FList := TList.Create;
  FList.Add(TtiVisitedTerminatedItem.Create);
  TtiVisitedTerminatedItem(FList.Items[0]).OnGetMaxTouchCount := DoGetMaxTouchCount;
  TtiVisitedTerminatedItem(FList.Items[0]).OnGetTouchCount := DoGetTouchCount;
  TtiVisitedTerminatedItem(FList.Items[0]).OnIncTouchCount := DoIncTouchCount;
  FData := TtiVisitedTerminatedData.Create;
  FData.OnGetMaxTouchCount := DoGetMaxTouchCount;
  FData.OnGetTouchCount := DoGetTouchCount;
  FData.OnIncTouchCount := DoIncTouchCount;
end;

destructor TtiVisitedTerminated.Destroy;
begin
  FData.Free;
  TObject(FList.Items[0]).Free;
  FList.Free;
  inherited;
end;

procedure TTestTIVisitor.Visited_ContinueVisiting(const AIterationStyle: TtiIterationStyle);
var
  LVisited: TtiVisitedTerminated;
  LVisitor: TSensingVisitor;
begin
  // ContinueVisiting will be checked once for each object that is in the
  // iteration graph. ContinueVisiting could be checked inside loop, and
  // used to break from the loops, which will make for a slightly more
  // responsive system, however ContinueVisiting will return False on very
  // few occasions, so it's better to call less frequently, and suffer the
  // reduced response on those rare occasions that True is returned.
  LVisited := nil;
  LVisitor := nil;
  try
    LVisited := TtiVisitedTerminated.Create;
    LVisitor := TSensingVisitor.Create;

    LVisited.SetMaxTouchCount(3);
    LVisitor.IterationStyle := AIterationStyle;
    LVisited.Iterate(LVisitor);
    CheckEquals(3, LVisited.TouchCount);
    CheckEquals(3, LVisitor.Data.Count);
    if AIterationStyle in [isTopDownRecurse, isTopDownSinglePass] then
    begin
      CheckEquals(TtiVisitedTerminated.ClassName, LVisitor.Data.Strings[0]);
      CheckEquals(TtiVisitedTerminatedData.ClassName, LVisitor.Data.Strings[1]);
      CheckEquals(TtiVisitedTerminatedItem.ClassName, LVisitor.Data.Strings[2]);
    end
    else
    begin
      CheckEquals(TtiVisitedTerminatedItem.ClassName, LVisitor.Data.Strings[0]);
      CheckEquals(TtiVisitedTerminatedData.ClassName, LVisitor.Data.Strings[1]);
      CheckEquals(TtiVisitedTerminated.ClassName, LVisitor.Data.Strings[2]);
    end;

    LVisited.SetMaxTouchCount(2);
    LVisitor.Data.Clear;
    LVisitor.IterationStyle := AIterationStyle;
    LVisited.Iterate(LVisitor);
    CheckEquals(3, LVisited.TouchCount);
    CheckEquals(2, LVisitor.Data.Count);
    if AIterationStyle in [isTopDownRecurse, isTopDownSinglePass] then
    begin
      CheckEquals(TtiVisitedTerminated.ClassName, LVisitor.Data.Strings[0]);
      CheckEquals(TtiVisitedTerminatedData.ClassName, LVisitor.Data.Strings[1]);
    end
    else
    begin
      CheckEquals(TtiVisitedTerminatedItem.ClassName, LVisitor.Data.Strings[0]);
      CheckEquals(TtiVisitedTerminatedData.ClassName, LVisitor.Data.Strings[1]);
    end;

    LVisited.SetMaxTouchCount(1);
    LVisitor.Data.Clear;
    LVisitor.IterationStyle := AIterationStyle;
    LVisited.Iterate(LVisitor);

    if AIterationStyle = isTopDownRecurse then
      CheckEquals(3, LVisited.TouchCount)
    else
      CheckEquals(2, LVisited.TouchCount);
    CheckEquals(1, LVisitor.Data.Count);

    if AIterationStyle in [isTopDownRecurse, isTopDownSinglePass] then
      CheckEquals(TtiVisitedTerminated.ClassName, LVisitor.Data.Strings[0])
    else
      CheckEquals(TtiVisitedTerminatedItem.ClassName, LVisitor.Data.Strings[0]);

    LVisited.SetMaxTouchCount(0);
    LVisitor.Data.Clear;
    LVisitor.IterationStyle := AIterationStyle;
    LVisited.Iterate(LVisitor);
    CheckEquals(1, LVisited.TouchCount);
    CheckEquals(0, LVisitor.Data.Count);

  finally
    LVisited.Free;
    LVisitor.Free;
  end;
end;

type
  TtiTestVisitedContinueVisitingFunction = class(TtiVisited)
  private
    FTerminated: boolean;
  protected
    function GetTerminated: boolean; override;
    procedure SetTerminated(const AValue: boolean);
  end;

function TtiTestVisitedContinueVisitingFunction.GetTerminated: boolean;
begin
  Result := FTerminated;
end;

procedure TtiTestVisitedContinueVisitingFunction.SetTerminated(const AValue: boolean);
begin
  FTerminated := AValue;
end;

procedure TTestTIVisitor.Visited_ContinueVisitingFunction;
var
  LVisited: TtiTestVisitedContinueVisitingFunction;
  LVisitor: TtiVisitor;
begin
  LVisited := nil;
  LVisitor := nil;
  try
    LVisited := TtiTestVisitedContinueVisitingFunction.Create;
    LVisitor := TtiVisitor.Create;

    LVisited.SetTerminated(False);
    LVisitor.ContinueVisiting := True;
    LVisitor.IterationStyle   := isTopDownRecurse;
    CheckEquals(True, LVisited.ContinueVisiting(LVisitor));
    CheckEquals(True, LVisited.CheckContinueVisitingIfTopDownRecurse(LVisitor));

    LVisited.SetTerminated(True);
    LVisitor.ContinueVisiting := False;
    LVisitor.IterationStyle   := isTopDownRecurse;
    CheckEquals(False, LVisited.ContinueVisiting(LVisitor));
    CheckEquals(False, LVisited.CheckContinueVisitingIfTopDownRecurse(LVisitor));

    LVisited.SetTerminated(False);
    LVisitor.ContinueVisiting := False;
    LVisitor.IterationStyle   := isTopDownRecurse;
    CheckEquals(False, LVisited.ContinueVisiting(LVisitor));
    CheckEquals(False, LVisited.CheckContinueVisitingIfTopDownRecurse(LVisitor));

    LVisited.SetTerminated(True);
    LVisitor.ContinueVisiting := True;
    LVisitor.IterationStyle   := isTopDownRecurse;
    CheckEquals(False, LVisited.ContinueVisiting(LVisitor));
    CheckEquals(False, LVisited.CheckContinueVisitingIfTopDownRecurse(LVisitor));

    LVisited.SetTerminated(False);
    LVisitor.ContinueVisiting := True;
    LVisitor.IterationStyle   := isTopDownSinglePass;
    CheckEquals(True, LVisited.ContinueVisiting(LVisitor));
    CheckEquals(True, LVisited.CheckContinueVisitingIfTopDownRecurse(LVisitor));

    LVisited.SetTerminated(True);
    LVisitor.ContinueVisiting := False;
    LVisitor.IterationStyle   := isTopDownSinglePass;
    CheckEquals(False, LVisited.ContinueVisiting(LVisitor));
    CheckEquals(True, LVisited.CheckContinueVisitingIfTopDownRecurse(LVisitor));

    LVisited.SetTerminated(False);
    LVisitor.ContinueVisiting := False;
    LVisitor.IterationStyle   := isTopDownSinglePass;
    CheckEquals(False, LVisited.ContinueVisiting(LVisitor));
    CheckEquals(True, LVisited.CheckContinueVisitingIfTopDownRecurse(LVisitor));

    LVisited.SetTerminated(True);
    LVisitor.ContinueVisiting := True;
    LVisitor.IterationStyle   := isTopDownSinglePass;
    CheckEquals(False, LVisited.ContinueVisiting(LVisitor));
    CheckEquals(True, LVisited.CheckContinueVisitingIfTopDownRecurse(LVisitor));

  finally
    LVisited.Free;
    LVisitor.Free;
  end;
end;

type
  TTestTIVisitorTerminated = class(TtiVisited)
  private
    FTIOPFManager: TtiOPFManager;
  public
    function TIOPFManager: TObject; override;
    procedure SetTIOPFManager(const ATIOPFManager: TtiOPFManager);
  end;

function TTestTIVisitorTerminated.TIOPFManager: TObject;
begin
  Result := FTIOPFManager;
end;

procedure TTestTIVisitorTerminated.SetTIOPFManager(const ATIOPFManager: TtiOPFManager);
begin
  FTIOPFManager := ATIOPFManager;
end;

procedure TTestTIVisitor.Visited_Terminated;
var
  LVisited:      TTestTIVisitorTerminated;
  LTIOPFManager: TtiOPFManager;
begin
  LTIOPFManager := nil;
  LVisited      := nil;
  try
    LTIOPFManager := TtiOPFManager.Create;
    LVisited := TTestTIVisitorTerminated.Create;
    LVisited.SetTIOPFManager(LTIOPFManager);
    CheckSame(LTIOPFManager, LVisited.TIOPFManager);
    CheckEquals(False, LTIOPFManager.Terminated);
    CheckEquals(False, LVisited.Terminated);
    LTIOPFManager.Terminate;
    CheckEquals(True, LTIOPFManager.Terminated);
    CheckEquals(True, LVisited.Terminated);
  finally
    LTIOPFManager.Free;
    LVisited.Free;
  end;
end;

procedure TTestTIVisitor.Visited_ContinueVisiting_BottomUpSinglePass;
begin
  Visited_ContinueVisiting(isBottomUpSinglePass);
end;

procedure TTestTIVisitor.Visited_ContinueVisiting_TopDownRecurse;
begin
  Visited_ContinueVisiting(isTopDownRecurse);
end;

procedure TTestTIVisitor.Visited_ContinueVisiting_TopDownSinglePass;
begin
  Visited_ContinueVisiting(isTopDownSinglePass);
end;

type

  TTestVisitedVisitBranchChild2 = class(TtiVisited)
  end;

  TTestVisitedVisitBranchChild1 = class(TtiVisited)
  private
    FData: TTestVisitedVisitBranchChild2;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Data: TTestVisitedVisitBranchChild2 read FData;
  end;

  TTestVisitedVisitBranch = class(TtiVisited)
  private
    FData: TTestVisitedVisitBranchChild1;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Data: TTestVisitedVisitBranchChild1 read FData;
  end;

  TTestVisitorVisitBranch = class(TSensingVisitor)
  private
    FApplyTest:        boolean;
    FVisitBranchCalls: TStringList;
  protected
    function VisitBranch(const ADerivedParent, AVisited: TtiVisited): boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ApplyTest: boolean read FApplyTest write FApplyTest;
    property VisitBranchCalls: TStringList read FVisitBranchCalls;
  end;

{ TTestVisitedOverrideVisitBranchChild1 }

constructor TTestVisitedVisitBranchChild1.Create;
begin
  inherited;
  FData := TTestVisitedVisitBranchChild2.Create;
end;

destructor TTestVisitedVisitBranchChild1.Destroy;
begin
  FData.Free;
  inherited;
end;

constructor TTestVisitedVisitBranch.Create;
begin
  inherited;
  FData := TTestVisitedVisitBranchChild1.Create;
end;

destructor TTestVisitedVisitBranch.Destroy;
begin
  FData.Free;
  inherited;
end;

constructor TTestVisitorVisitBranch.Create;
begin
  inherited;
  FVisitBranchCalls := TStringList.Create;
end;

destructor TTestVisitorVisitBranch.Destroy;
begin
  FVisitBranchCalls.Free;
  inherited;
end;

function TTestVisitorVisitBranch.VisitBranch(const ADerivedParent, AVisited:
  TtiVisited): boolean;
var
  LDerivedParent: string;
begin
  if ADerivedParent = nil then
    LDerivedParent := 'nil'
  else
    LDerivedParent := IntToStr(Integer(ADerivedParent));
  VisitBranchCalls.Add(LDerivedParent + ',' + IntToStr(Integer(AVisited)));

  Result := (not ApplyTest) or
    (ApplyTest and not (AVisited is TTestVisitedVisitBranchChild1));
end;

procedure TTestTIVisitor.Visited_VisitBranch(const AIterationStyle: TtiIterationStyle);
var
  LVisited: TTestVisitedVisitBranch;
  LVisitor: TTestVisitorVisitBranch;
begin
  LVisited := nil;
  LVisitor := nil;
  try
    LVisited := TTestVisitedVisitBranch.Create;
    LVisitor := TTestVisitorVisitBranch.Create;
    LVisitor.ApplyTest := False;
    LVisited.Iterate(LVisitor);
    CheckEquals(3, LVisitor.Data.Count);
    CheckEquals('nil,' + IntToStr(Integer(LVisited)), LVisitor.VisitBranchCalls.Strings[0]);
    CheckEquals(IntToStr(Integer(LVisited)) + ',' + IntToStr(Integer(LVisited.Data)),
      LVisitor.VisitBranchCalls.Strings[1]);
    CheckEquals(IntToStr(Integer(LVisited.Data)) + ',' +
      IntToStr(Integer(LVisited.Data.Data)), LVisitor.VisitBranchCalls.Strings[2]);

  finally
    LVisited.Free;
    LVisitor.Free;
  end;

  LVisited := nil;
  LVisitor := nil;
  try
    LVisited := TTestVisitedVisitBranch.Create;
    LVisitor := TTestVisitorVisitBranch.Create;
    LVisitor.ApplyTest := True;
    LVisited.Iterate(LVisitor);
    CheckEquals(1, LVisitor.Data.Count);
    CheckEquals('nil,' + IntToStr(Integer(LVisited)), LVisitor.VisitBranchCalls.Strings[0]);
    CheckEquals(IntToStr(Integer(LVisited)) + ',' + IntToStr(Integer(LVisited.Data)),
      LVisitor.VisitBranchCalls.Strings[1]);
  finally
    LVisited.Free;
    LVisitor.Free;
  end;

end;

procedure TTestTIVisitor.Visited_VisitBranch_BottomUpSinglePass;
begin
  Visited_VisitBranch(isBottomUpSinglePass);
end;

procedure TTestTIVisitor.Visited_VisitBranch_TopDownRecurse;
begin
  Visited_VisitBranch(isTopDownRecurse);
end;

procedure TTestTIVisitor.Visited_VisitBranch_TopDownSinglePass;
begin
  Visited_VisitBranch(isTopDownSinglePass);
end;

procedure TTestTIVisitor.Visited_Recurse_BottomUpSinglePass;
var
  LData:    TtiObjectList;
  LVisitor: TTestVisitorTopDownRecurse;
begin
  LData := TtiObjectList.Create;
  try
    LVisitor := TTestVisitorTopDownRecurse.Create;
    LVisitor.IterationStyle := isBottomUpSinglePass;
    try
      LData.Iterate(LVisitor);
      CheckEquals(1, LData.Count);
      CheckEquals(0, (LData.Items[0] as TtiObjectList).Count);
    finally
      LVisitor.Free;
    end;
  finally
    LData.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Iterate_List_BottomUpSinglePass;
var
  lVisitor: TTestVisitorIterate;
  lVisited: TTestVisitedList;
begin
  lVisited := CreateList;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle := isBottomUpSinglePass;
      lVisited.Iterate(lVisitor);
      CheckEquals(5, lVisitor.Data.Count);
      CheckEquals('5', lVisitor.Data.Strings[0]);
      CheckEquals('4', lVisitor.Data.Strings[1]);
      CheckEquals('3', lVisitor.Data.Strings[2]);
      CheckEquals('2', lVisitor.Data.Strings[3]);
      CheckEquals('1', lVisitor.Data.Strings[4]);
    finally
      lVisitor.Free;
    end;
  finally
    lVisited.Free;
  end;
end;

type

  TTestVisitedOverrideIterate = class(TtiVisited)
  private
    FData: TtiVisited;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Data: TtiVisited read FData;
  end;

  TTestVisitedOverrideIterateChild = class(TtiVisited)
  private
    FData: TtiVisited;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure IterateRecurse(const AVisitor: TtiVisitor; const ADerivedParent: TtiVisited;
      const ATouchedByVisitorList: TtiTouchedByVisitorList;
      const ATouchMethod: TtiVisitedTouchMethod; const AIterationDepth: TIterationDepth);
      override;
  published
    property Data: TtiVisited read FData;
  end;

constructor TTestVisitedOverrideIterate.Create;
begin
  inherited;
  FData := TTestVisitedOverrideIterateChild.Create;
end;

destructor TTestVisitedOverrideIterate.Destroy;
begin
  FData.Free;
  inherited;
end;

constructor TTestVisitedOverrideIterateChild.Create;
begin
  inherited;
  FData := TtiVisited.Create;
end;

destructor TTestVisitedOverrideIterateChild.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TTestVisitedOverrideIterateChild.IterateRecurse(const AVisitor: TtiVisitor;
  const ADerivedParent: TtiVisited; const ATouchedByVisitorList: TtiTouchedByVisitorList;
  const ATouchMethod: TtiVisitedTouchMethod; const AIterationDepth: TIterationDepth);
begin
  AVisitor.Execute(Self);
end;

procedure TTestTIVisitor.Visited_Iterate_Override;
var
  LVisitor: TSensingVisitorAbs;
  LVisited: TTestVisitedOverrideIterate;
begin
  LVisitor := nil;
  LVisited := nil;
  try
    LVisitor := TSensingVisitor.Create;
    LVisited := TTestVisitedOverrideIterate.Create;
    LVisited.Iterate(LVisitor);
    CheckEquals(2, LVisitor.Data.Count);
    CheckEquals(TTestVisitedOverrideIterate.ClassName, LVisitor.Data.Strings[0]);
    CheckEquals(TTestVisitedOverrideIterateChild.ClassName, LVisitor.Data.Strings[1]);
  finally
    LVisitor.Free;
    LVisited.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Iterate_Owned(const AIterationStyle: TtiIterationStyle);
var
  lVisitor: TTestVisitorIterate;
  lVisited: TTestVisitedOwned;
begin
  lVisited := CreateOwned;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle := AIterationStyle;
      lVisited.Iterate(lVisitor);
      CheckEquals(4, lVisitor.Data.Count);
      CheckEquals('1', lVisitor.Data.Strings[0]);
      CheckEquals('2', lVisitor.Data.Strings[1]);
      CheckEquals('3', lVisitor.Data.Strings[2]);
      CheckEquals('4', lVisitor.Data.Strings[3]);
    finally
      lVisitor.Free;
    end;
  finally
    lVisited.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Iterate_Owned_BottomUpSinglePass;
var
  lVisitor: TTestVisitorIterate;
  lVisited: TTestVisitedOwned;
begin
  lVisited := CreateOwned;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle := isBottomUpSinglePass;
      lVisited.Iterate(lVisitor);
      CheckEquals(4, lVisitor.Data.Count);
      CheckEquals('4', lVisitor.Data.Strings[0]);
      CheckEquals('3', lVisitor.Data.Strings[1]);
      CheckEquals('2', lVisitor.Data.Strings[2]);
      CheckEquals('1', lVisitor.Data.Strings[3]);
    finally
      lVisitor.Free;
    end;
  finally
    lVisited.Free;
  end;
end;


procedure TTestTIVisitor.Visited_Iterate_ListAndOwned_TopDownRecurse;
begin
  Visited_Iterate_ListAndOwned(isTopDownRecurse);
end;


procedure TTestTIVisitor.Visited_Iterate_ListAndOwned_TopDownSinglePass;
begin
  Visited_Iterate_ListAndOwned(isTopDownSinglePass);
end;

type
  TTestVisitedIterateAssignTouched = class(TTestVisitedListAndOwned)
  public
    procedure IterateAssignTouched(const AVisitor: TtiVisitor;
      const ATouchedByVisitorList: TtiTouchedByVisitorList); override;
  end;

procedure TTestVisitedIterateAssignTouched.IterateAssignTouched(const AVisitor: TtiVisitor;
  const ATouchedByVisitorList: TtiTouchedByVisitorList);
begin
  inherited IterateAssignTouched(AVisitor, ATouchedByVisitorList);
end;

procedure TTestTIVisitor.Visited_IterateAssignTouched(const AIterationStyle: TtiIterationStyle);
var
  LTouchedByVisitorList: TtiTouchedByVisitorList;
  LVisitor: TTestVisitorIterate;
  LVisitedList: TTestVisitedIterateAssignTouched;
  i: integer;
  LSequenceFactor: integer;
begin
  // The expected order of objects will be reversed for BottomUpSinglePass
  if AIterationStyle = isBottomUpSinglePass then
    LSequenceFactor := 12
  else
    LSequenceFactor := 0;

  LTouchedByVisitorList := nil;
  LVisitor     := nil;
  LVisitedList := nil;
  try
    LVisitedList := TTestVisitedIterateAssignTouched.Create;
    LVisitor := TTestVisitorIterate.Create;
    LTouchedByVisitorList   := TtiTouchedByVisitorList.Create(True);
    LVisitor.IterationStyle := AIterationStyle;

    LVisitedList.IterateAssignTouched(LVisitor, LTouchedByVisitorList);

    CheckEquals(13, LTouchedByVisitorList.Count);
    for i := 0 to 12 do
      CheckSame(LVisitor, LTouchedByVisitorList.Items[I].Visitor);

    CheckSame(LVisitedList, LTouchedByVisitorList.Items[Abs(0 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[0]),
      LTouchedByVisitorList.Items[Abs(1 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[0]).Owned1,
      LTouchedByVisitorList.Items[Abs(2 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[0]).Owned2,
      LTouchedByVisitorList.Items[Abs(3 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[0]).Owned3,
      LTouchedByVisitorList.Items[Abs(4 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[1]),
      LTouchedByVisitorList.Items[Abs(5 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[1]).Owned1,
      LTouchedByVisitorList.Items[Abs(6 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[1]).Owned2,
      LTouchedByVisitorList.Items[Abs(7 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[1]).Owned3,
      LTouchedByVisitorList.Items[Abs(8 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[2]),
      LTouchedByVisitorList.Items[Abs(9 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[2]).Owned1,
      LTouchedByVisitorList.Items[Abs(10 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[2]).Owned2,
      LTouchedByVisitorList.Items[Abs(11 - LSequenceFactor)].Visited);
    CheckSame(TTestVisitedOwned(LVisitedList.List.Items[2]).Owned3,
      LTouchedByVisitorList.Items[Abs(12 - LSequenceFactor)].Visited);

  finally
    LVisitedList.Free;
    LVisitor.Free;
    LTouchedByVisitorList.Free;
  end;
end;

procedure TTestTIVisitor.Visited_IterateAssignTouched_BottomUpSinglePass;
begin
  Visited_IterateAssignTouched(isBottomUpSinglePass);
end;

procedure TTestTIVisitor.Visited_IterateAssignTouched_TopDownRecurse;
begin
  Visited_IterateAssignTouched(isTopDownRecurse);
end;

procedure TTestTIVisitor.Visited_IterateAssignTouched_TopDownSinglePass;
begin
  Visited_IterateAssignTouched(isTopDownSinglePass);
end;

procedure TTestTIVisitor.Visited_Iterate_List(const AIterationStyle: TtiIterationStyle);
var
  lVisitor: TTestVisitorIterate;
  lVisited: TTestVisitedList;
begin
  lVisited := CreateList;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle := AIterationStyle;
      lVisited.Iterate(lVisitor);
      CheckEquals(5, lVisitor.Data.Count);
      CheckEquals('1', lVisitor.Data.Strings[0]);
      CheckEquals('2', lVisitor.Data.Strings[1]);
      CheckEquals('3', lVisitor.Data.Strings[2]);
      CheckEquals('4', lVisitor.Data.Strings[3]);
      CheckEquals('5', lVisitor.Data.Strings[4]);

    finally
      lVisitor.Free;
    end;
  finally
    lVisited.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Iterate_ListAndOwned(const AIterationStyle: TtiIterationStyle);
var
  lVisitor: TTestVisitorIterate;
  lVisitedList: TTestVisitedListAndOwned;
  i: integer;
begin
  lVisitedList := TTestVisitedListAndOwned.Create;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle := AIterationStyle;
      lVisitedList.Iterate(lVisitor);
      CheckEquals(13, lVisitor.Data.Count);
      for i := 1 to 13 do
        CheckEquals(IntToStr(i), lVisitor.Data.Strings[i - 1]);
    finally
      lVisitor.Free;
    end;
  finally
    lVisitedList.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Iterate_ListAndOwned_BottomUpSinglePass;
var
  lVisitor: TTestVisitorIterate;
  lVisitedList: TTestVisitedListAndOwned;
  i: integer;
begin
  lVisitedList := TTestVisitedListAndOwned.Create;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle := isBottomUpSinglePass;
      lVisitedList.Iterate(lVisitor);
      CheckEquals(13, lVisitor.Data.Count);
      for i := 1 to 13 do
        CheckEquals(IntToStr(i), lVisitor.Data.Strings[13 - i]);
    finally
      lVisitor.Free;
    end;
  finally
    lVisitedList.Free;
  end;
end;

function TTestTIVisitor.CreateList: TTestVisitedList;
var
  lData: TTestVisited;
begin
  Result       := TTestVisitedList.Create;
  Result.Index := 1;

  lData       := TTestVisited.Create;
  lData.Index := 2;
  Result.List.Add(lData);

  lData       := TTestVisited.Create;
  lData.Index := 3;
  Result.List.Add(lData);

  lData       := TTestVisited.Create;
  lData.Index := 4;
  Result.List.Add(lData);

  lData       := TTestVisited.Create;
  lData.Index := 5;
  Result.List.Add(lData);
end;


function TTestTIVisitor.CreateOwned: TTestVisitedOwned;
begin
  Result       := TTestVisitedOwned.Create;
  Result.Index := 1;
  Result.Owned1.Index := 2;
  Result.Owned2.Index := 3;
  Result.Owned3.Index := 4;
end;

{
procedure TTestTIVisitor.Visited_ClassCount;
var
  lVisited : TTestVisitedList;
begin
  lVisited := CreateListAndOwned;
  try
    CheckEquals(1,  lVisited.CountByClass(TTestVisitedList),  'Failed TTestVisitedList');
    CheckEquals(3,  lVisited.CountByClass(TTestVisitedOwned), 'Failed TTestVisitedOwned');
    CheckEquals(9,  lVisited.CountByClass(TTestVisited),       'Failed TTestVisited');
    CheckEquals(13, lVisited.CountByClass(TtiVisited),        'Failed TtiVisited');
  finally
    lVisited.Free;
  end;
end;
}

procedure TTestTIVisitor.SetUp;
begin
  inherited;
  gTIOPFManager.Terminated := False;
end;

procedure TTestTIVisitor.TouchedByVisitorList_Add;
var
  LList:  TtiTouchedByVisitorList;
  LItem1: TtiTouchedByVisitor;
  LItem2: TtiTouchedByVisitor;
  LVisitor: TtiVisitor;
  LVisited: TtiVisited;
begin
  LVisitor:= nil;
  LVisited:= nil;
  LList:= nil;
  try
    LVisitor:= TtiVisitor.Create;
    LVisited:= TtiVisited.Create;
    LList := TtiTouchedByVisitorList.Create(True);
    LItem1 := TtiTouchedByVisitor.Create(LVisitor, LVisited, 0);
    LList.Add(LItem1);
    CheckEquals(1, LList.Count);
    CheckSame(LItem1, LList.Items[0]);
    LItem2 := TtiTouchedByVisitor.Create(LVisitor, LVisited, 0);
    LList.Add(LItem2);
    CheckEquals(2, LList.Count);
    CheckSame(LItem2, LList.Items[1]);
  finally
    LVisitor.Free;
    LVisited.Free;
    LList.Free;
  end;
end;

procedure TTestTIVisitor.TouchedByVisitorList_AppendBottomUp;
var
  LList1: TtiTouchedByVisitorList;
  LList2: TtiTouchedByVisitorList;
  LItem1: TtiTouchedByVisitor;
  LItem2: TtiTouchedByVisitor;
  LVisitor: TtiVisitor;
  LVisited: TtiVisited;
begin
  LVisitor:= nil;
  LVisited:= nil;
  LList1 := nil;
  LList2 := nil;
  try
    LVisitor:= TtiVisitor.Create;
    LVisited:= TtiVisited.Create;
    LList1 := TtiTouchedByVisitorList.Create(False);
    LList2 := TtiTouchedByVisitorList.Create(True);
    LItem1 := TtiTouchedByVisitor.Create(LVisitor, LVisited, 0);
    LList1.Add(LItem1);
    LItem2 := TtiTouchedByVisitor.Create(LVisitor, LVisited, 0);
    LList1.Add(LItem2);
    CheckSame(LItem1, LList1.Items[0]);
    CheckSame(LItem2, LList1.Items[1]);

    LList2.AppendBottomUp(LList1);
    CheckSame(LItem2, LList2.Items[0]);
    CheckSame(LItem1, LList2.Items[1]);

  finally
    LVisitor.Free;
    LVisited.Free;
    LList1.Free;
    LList2.Free;
  end;
end;

procedure TTestTIVisitor.TouchedByVisitorList_AppendTopDown;
var
  LList1: TtiTouchedByVisitorList;
  LList2: TtiTouchedByVisitorList;
  LItem1: TtiTouchedByVisitor;
  LItem2: TtiTouchedByVisitor;
  LVisitor: TtiVisitor;
  LVisited: TtiVisited;
begin
  LVisitor:= nil;
  LVisited:= nil;
  LList1 := nil;
  LList2 := nil;
  try
    LVisitor:= TtiVisitor.Create;
    LVisited:= TtiVisited.Create;
    LList1 := TtiTouchedByVisitorList.Create(False);
    LList2 := TtiTouchedByVisitorList.Create(True);
    LItem1 := TtiTouchedByVisitor.Create(LVisitor, LVisited, 0);
    LList1.Add(LItem1);
    LItem2 := TtiTouchedByVisitor.Create(LVisitor, LVisited, 0);
    LList1.Add(LItem2);
    CheckSame(LItem1, LList1.Items[0]);
    CheckSame(LItem2, LList1.Items[1]);

    LList2.AppendTopDown(LList1);
    CheckSame(LItem1, LList2.Items[0]);
    CheckSame(LItem2, LList2.Items[1]);

  finally
    LVisitor.Free;
    LVisited.Free;
    LList1.Free;
    LList2.Free;
  end;
end;

procedure TTestTIVisitor.TouchedByVisitor_Create;
var
  L:        TtiTouchedByVisitor;
  LVisitor: TtiVisitor;
  LVisited: TtiVisited;
begin
  LVisitor := nil;
  LVisited := nil;
  L        := nil;
  try
    LVisitor := TtiVisitor.Create;
    LVisited := TtiVisited.Create;
    L := TtiTouchedByVisitor.Create(LVisitor, LVisited, 1);
    CheckSame(LVisitor, L.Visitor);
    CheckSame(LVisited, L.Visited);
    CheckEquals(1, L.IterationDepth);
  finally
    LVisitor.Free;
    LVisited.Free;
    L.Free;
  end;
end;

{ TTestVisitor_Execute }

procedure TTestVisitor_Execute.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  ExecuteCalled := True;
end;


{ TTestVisitorIterate }

procedure TTestVisitorIterate.Execute(const AVisited: TtiVisited);
var
  LIndex: string;
begin
  inherited Execute(AVisited);
  Assert(AVisited is TTestVisitedAbs, 'AVisited not a TTestVisitedAbs');
  LIndex := IntToStr((AVisited as TTestVisitedAbs).Index);
  Data.Add(LIndex);
end;


{ TTestVisitedList }

constructor TTestVisitedList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;


destructor TTestVisitedList.Destroy;
begin
  FList.Free;
  inherited;
end;


function TTestVisitedList.GetList: TList;
begin
  Result := FList;
end;

{ TTestVisitedOwned }

constructor TTestVisitedOwned.Create;
begin
  inherited;
  FOwned1 := TTestVisited.Create;
  FOwned3 := TTestVisited.Create;
  FOwned2 := TTestVisited.Create;
end;


destructor TTestVisitedOwned.Destroy;
begin
  FOwned1.Free;
  FOwned3.Free;
  FOwned2.Free;
  inherited;
end;

{ TTestVisitorController }

class function TTestVisitorController.VisitorControllerClass: TtiVisitorControllerClass;
begin
  Result := TtiVisitorController;
end;

{ TTestVisitorGetAllToVisit }

function TTestVisitorGetAllToVisit.AcceptVisitor: boolean;
begin
  Result := Visited is TTestVisitedOwned;
end;


{ TTestVisStream }

procedure TTestVisStream.Execute(const AVisited: TtiVisited);
begin
  inherited;
  Assert(AVisited is TTestVisitedAbs, 'AVisited not a TTestVisitedAbs');
  if Stream.Size > 0 then
    Write(',');
  Write(IntToStr(TTestVisitedAbs(Visited).Index));
end;


procedure TTestVisStream.Write(const AValue: string);
begin
  // A protected method, so this surfaces it as public for testing
  inherited;
end;


procedure TTestVisStream.WriteLn(const AValue: string);
begin
  // A protected method, so this surfaces it as public for testing
  inherited;
end;

{ TTestVisitorAcceptVisitor }

procedure TTestVisitorAcceptVisitor.Execute(const AVisited: TtiVisited);
begin
  Data.Add((AVisited as TTestTIVisitedAcceptVisitor).ID);
end;

{ TSensingVisitor }

constructor TSensingVisitorAbs.Create;
begin
  inherited;
  FData := TStringList.Create;
end;

destructor TSensingVisitorAbs.Destroy;
begin
  FData.Free;
  inherited;
end;


procedure TSensingVisitor.Execute(const AVisited: TtiVisited);
begin
  inherited Execute(AVisited);
  Data.Add(AVisited.Caption);
end;

type
  TTestVisitorManagerRegisterVisitor = class(TtiVisitor)
  end;

procedure TTestTIVisitor.VisitorManager_RegisterVisitor;
var
  LVM: TTestVisitorManager;
begin
  LVM := TTestVisitorManager.Create(nil);
  try
    CheckEquals(0, LVM.VisitorMappings.Count);
    LVM.RegisterVisitor('test', TTestVisitorManagerRegisterVisitor);
    CheckEquals(1, LVM.VisitorMappings.Count);
    LVM.UnRegisterVisitors('test');
    CheckEquals(0, LVM.VisitorMappings.Count);
  finally
    LVM.Free;
  end;
end;

var
  USensingList: TStringList;

type

  TSensingVisitorController = class(TtiVisitorController)
  public
    procedure BeforeExecuteVisitorGroup; override;
    procedure BeforeExecuteVisitor(const AVisitor: TtiVisitor); override;
    procedure AfterExecuteVisitor(const AVisitor: TtiVisitor); override;
    procedure AfterExecuteVisitorGroup(const ATouchedByVisitorList: TtiTouchedByVisitorList);
      override;
    procedure AfterExecuteVisitorGroupError; override;
  end;

  TTestVisitorManagerVCVisitor = class(TtiVisitor)
  public
    class function VisitorControllerClass: TtiVisitorControllerClass; override;
  end;

  TTestVisitorManagerVCVisitorException = class(TTestVisitorManagerVCVisitor)
  public
    procedure Execute(const AVisited: TtiVisited); override;
  end;

procedure TSensingVisitorController.AfterExecuteVisitor(const AVisitor: TtiVisitor);
begin
  USensingList.Add('AfterExecuteVisitor');
end;

procedure TSensingVisitorController.AfterExecuteVisitorGroup(
  const ATouchedByVisitorList: TtiTouchedByVisitorList);
begin
  USensingList.Add('AfterExecuteVisitorGroup');
end;

procedure TSensingVisitorController.AfterExecuteVisitorGroupError;
begin
  USensingList.Add('AfterExecuteVisitorGroupError');
end;

procedure TSensingVisitorController.BeforeExecuteVisitor(const AVisitor: TtiVisitor);
begin
  USensingList.Add('BeforeExecuteVisitor');
end;

procedure TSensingVisitorController.BeforeExecuteVisitorGroup;
begin
  USensingList.Add('BeforeExecuteVisitorGroup');
end;

class function TTestVisitorManagerVCVisitor.VisitorControllerClass: TtiVisitorControllerClass;
begin
  Result := TSensingVisitorController;
end;

procedure TTestVisitorManagerVCVisitorException.Execute(const AVisited: TtiVisited);
begin
  raise Exception.Create('Test VisitorController exception');
end;

procedure TTestTIVisitor.VisitorManager_VisitorController;
var
  LM:  TtiOPFManager;
  LVM: TtiVisitorManager;
  LO:  TtiObject;
begin
  LM           := nil;
  USensingList := nil;
  LVM          := nil;
  LO           := nil;
  try
    USensingList := TStringList.Create;
    LM := TtiOPFManager.Create;
    LVM := TtiVisitorManager.Create(LM);
    LO  := TtiObject.Create;

    LVM.RegisterVisitor('test', TTestVisitorManagerVCVisitor);
    LVM.Execute('test', LO);

    CheckEquals(4, USensingList.Count);
    CheckEquals('BeforeExecuteVisitorGroup', USensingList.Strings[0]);
    CheckEquals('BeforeExecuteVisitor', USensingList.Strings[1]);
    CheckEquals('AfterExecuteVisitor', USensingList.Strings[2]);
    CheckEquals('AfterExecuteVisitorGroup', USensingList.Strings[3]);

  finally
    LO.Free;
    LVM.Free;
    LM.Free;
    USensingList.Free;
  end;
end;

procedure TTestTIVisitor.VisitorManager_VisitorControllerException;
var
  LVM: TtiVisitorManager;
  LO:  TtiObject;
begin
  USensingList := nil;
  LVM          := nil;
  LO           := nil;
  try
    USensingList := TStringList.Create;
    LVM := TtiVisitorManager.Create(nil);
    LO := TtiObject.Create;

    LVM.RegisterVisitor('test', TTestVisitorManagerVCVisitorException);
    try
      LVM.Execute('test', LO);
      Fail('Exception not raised');
    except
      on e: Exception do
      begin
        CheckEquals(4, USensingList.Count);
        CheckEquals('BeforeExecuteVisitorGroup', USensingList.Strings[0]);
        CheckEquals('BeforeExecuteVisitor', USensingList.Strings[1]);
        CheckEquals('AfterExecuteVisitor', USensingList.Strings[2]);
        CheckEquals('AfterExecuteVisitorGroupError', USensingList.Strings[3]);
      end;
    end;
  finally
    LO.Free;
    LVM.Free;
    USensingList.Free;
  end;
end;

type

  TTestVisitorMappingGroupVisitorControllerClass1 = class(TtiVisitorController)
  end;

  TTestVisitorMappingGroupVisitorControllerClass2 = class(TtiVisitorController)
  end;

  TTestVisitorMappingGroupVisitor1 = class(TtiVisitor)
  public
    class function VisitorControllerClass: TtiVisitorControllerClass; override;
  end;

  TTestVisitorMappingGroupVisitor2 = class(TtiVisitor)
  public
    class function VisitorControllerClass: TtiVisitorControllerClass; override;
  end;

class function TTestVisitorMappingGroupVisitor1.VisitorControllerClass: TtiVisitorControllerClass;
begin
  Result := TTestVisitorMappingGroupVisitorControllerClass1;
end;

class function TTestVisitorMappingGroupVisitor2.VisitorControllerClass: TtiVisitorControllerClass;
begin
  Result := TTestVisitorMappingGroupVisitorControllerClass2;
end;

procedure TTestTIVisitor.VisitorMappingGroup_Add;
var
  LVMG: TtiVisitorMappingGroup;
begin
  LVMG := TtiVisitorMappingGroup.Create('test', TTestVisitorMappingGroupVisitorControllerClass1);
  try
    CheckEquals('TEST', LVMG.GroupName);
    Check(LVMG.VisitorControllerClass = TTestVisitorMappingGroupVisitorControllerClass1);
    LVMG.Add(TTestVisitorMappingGroupVisitor1);
    // Check registration of duplicate visitor
    try
      LVMG.Add(TTestVisitorMappingGroupVisitor1);
      Fail('Exception not raised');
    except
      on e: Exception do
      begin
        CheckIs(e, EtiOPFProgrammerException);
        CheckFormattedMessage(CErrorAttemptToRegisterDuplicateVisitor,
          [TTestVisitorMappingGroupVisitor1.ClassName], e.message);
      end;
    end;

    // Check registration of visitor with different VisitorController
    try
      LVMG.Add(TTestVisitorMappingGroupVisitor2);
      Fail('Exception not raised');
    except
      on e: Exception do
      begin
        CheckIs(e, EtiOPFProgrammerException);
        CheckFormattedMessage(CErrorIncompatibleVisitorController,
          [TTestVisitorMappingGroupVisitorControllerClass1.ClassName,
          TTestVisitorMappingGroupVisitorControllerClass2.ClassName], e.message);
      end;
    end;

  finally
    LVMG.Free;
  end;
end;

type
  TTestVisitorMappingGroupAssignVisitorInstances1 = class(TtiVisitor)
  end;

  TTestVisitorMappingGroupAssignVisitorInstances2 = class(TtiVisitor)
  end;

procedure TTestTIVisitor.VisitorMappingGroup_AssignVisitorInstances;
var
  LVMG:  TtiVisitorMappingGroup;
  LList: TObjectList;
begin
  LVMG := TtiVisitorMappingGroup.Create('test', TtiVisitorController);
  try
    LVMG.Add(TTestVisitorMappingGroupAssignVisitorInstances1);
    LVMG.Add(TTestVisitorMappingGroupAssignVisitorInstances2);
    LList := TObjectList.Create;
    try
      LVMG.AssignVisitorInstances(LList);
      CheckEquals(2, LList.Count);
      CheckIs(LList.Items[0], TTestVisitorMappingGroupAssignVisitorInstances1);
      CheckIs(LList.Items[1], TTestVisitorMappingGroupAssignVisitorInstances2);
      CheckNotNull(LList.Items[0]);
      CheckNotNull(LList.Items[1]);
    finally
      LList.Free;
    end;
  finally
    LVMG.Free;
  end;
end;

procedure TTestTIVisitor.VisitorManager_FindVisitorMappingGroup;
var
  LVM: TTestVisitorManager;
begin
  LVM := TTestVisitorManager.Create(nil);
  try
    LVM.RegisterVisitor('test', TtiVisitor);
    LVM.RegisterVisitor('test1', TtiVisitor);
    LVM.RegisterVisitor('test2', TtiVisitor);
    CheckNotNull(LVM.FindVisitorMappingGroup('test'));
    CheckEquals('TEST', LVM.FindVisitorMappingGroup('test').GroupName);
    CheckNotNull(LVM.FindVisitorMappingGroup('test1'));
    CheckEquals('TEST1', LVM.FindVisitorMappingGroup('test1').GroupName);
    CheckNotNull(LVM.FindVisitorMappingGroup('test2'));
    CheckEquals('TEST2', LVM.FindVisitorMappingGroup('test2').GroupName);
  finally
    LVM.Free;
  end;
end;

function TTestVisitorManager.FindVisitorMappingGroup(
  const AGroupName: string): TtiVisitorMappingGroup;
begin
  Result := inherited FindVisitorMappingGroup(AGroupName);
end;

 { TTestVisitorManagerVCVisitorException }

 { TTestVisitedListAndOwned }

constructor TTestVisitedListAndOwned.Create;
var
  LVisitedOwned: TTestVisitedOwned;
begin
  {
    Creates an object graph like this:
      TTestVisitedList
        TTestVisitedOwned
          TTestVisited
          TTestVisited
          TTestVisited
        TTestVisitedOwned
          TTestVisited
          TTestVisited
          TTestVisited
        TTestVisitedOwned
          TTestVisited
          TTestVisited
          TTestVisited
  }
  inherited Create;
  Index         := 1;
  LVisitedOwned := TTestVisitedOwned.Create;
  LVisitedOwned.Index := 2;
  LVisitedOwned.Owned1.Index := 3;
  LVisitedOwned.Owned2.Index := 4;
  LVisitedOwned.Owned3.Index := 5;
  List.Add(LVisitedOwned);

  LVisitedOwned       := TTestVisitedOwned.Create;
  LVisitedOwned.Index := 6;
  LVisitedOwned.Owned1.Index := 7;
  LVisitedOwned.Owned2.Index := 8;
  LVisitedOwned.Owned3.Index := 9;
  List.Add(LVisitedOwned);

  LVisitedOwned       := TTestVisitedOwned.Create;
  LVisitedOwned.Index := 10;
  LVisitedOwned.Owned1.Index := 11;
  LVisitedOwned.Owned2.Index := 12;
  LVisitedOwned.Owned3.Index := 13;
  List.Add(LVisitedOwned);

end;

{ TTestVisitedIterateAssignTouched }

end.
