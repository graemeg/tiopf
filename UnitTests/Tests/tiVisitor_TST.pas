unit tiVisitor_TST;

{$I tiDefines.inc}

interface
uses
  Classes  // needed for TStringList
  {$IFNDEF FPC}
  ,TestFrameWork
  {$ENDIF}
  ,tiTestFramework
  ,tiVisitor
  ,Contnrs
 ;

type
  TTestVisitedList = class;
  TTestVisitedOwned = class;

  TTestTIVisitor = class(TtiTestCase)
  private
    function CreateListAndOwned : TTestVisitedList;
    function CreateList : TTestVisitedList;
    function CreateOwned : TTestVisitedOwned;

    procedure Visitor_ContinueVisiting(const AIterationStyle: TtiIterationStyle);

    procedure Visited_Iterate_Single(AIterationStyle: TtiIterationStyle);
    procedure Visited_Iterate_List(AIterationStyle: TtiIterationStyle);
    procedure Visited_Iterate_Owned(AIterationStyle: TtiIterationStyle);
    procedure Visited_Iterate_ListAndOwned(AIterationStyle: TtiIterationStyle);
    procedure Visited_AcceptVisitor(AIterationStyle: TtiIterationStyle);
    procedure Visited_VisitBranch(AIterationStyle: TtiIterationStyle);

  protected
    procedure SetUp; override;
  published

    // Test the visitor
    procedure Visitor_AcceptVisitor;
    procedure Visitor_Execute;

    procedure Visitor_ContinueVisiting_BottomUpSinglePass;
    procedure Visitor_ContinueVisiting_TopDownSinglePass;
    procedure Visitor_ContinueVisiting_TopDownRecurse;

    procedure Visitor_VisitorController;

    procedure Visitor_Depth_TopDownRecurse;
    procedure Visitor_Depth_TopDownSinglePass;
    procedure Visitor_Depth_BottomUpSinglePass;

    // Test the visited
    procedure Visited_Caption;

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

    procedure Visited_Recurse_TopDownRecurse;
    procedure Visited_Recurse_TopDownSinglePass;
    procedure Visited_Recurse_BottomUpSinglePass;

    procedure Visited_AcceptVisitor_TopDownRecurse;
    procedure Visited_AcceptVisitor_TopDownSinglePass;
    procedure Visited_AcceptVisitor_BottomUpSinglePass;

    procedure Visited_VisitBranch_TopDownRecurse;
    procedure Visited_VisitBranch_TopDownSinglePass;
    procedure Visited_VisitBranch_BottomUpSinglePass;

    procedure Visited_FindAllByClassType;

    // Test some other special visitors
    procedure VisClassCount_Execute;
    procedure VisFindAllByClass_Execute;

    // Test the special stream writing visitor
    procedure VisStream;

    // Test the TFileStream visitor wrapper
    procedure VisStreamToFile;
  end;

  TTestVisitor_Execute = class(TtiVisitor)
  private
    FExecuteCalled: boolean;
  public
    property  ExecuteCalled : boolean read FExecuteCalled write FExecuteCalled;
    procedure Execute(const AVisited : TtiVisited); override;
  end;


  TTestVisitedAbs = class(TtiVisited)
  private
    FIndex: Word;
  public
    property Index : Word read FIndex write FIndex;
  end;


  TTestVisited = class(TTestVisitedAbs);


  TTestVisitedList = class(TTestVisitedAbs)
  private
    FList: TObjectList;
    function GetList: TList;
  published
    property    List : TList read GetList;
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;


  TTestVisitedOwned = class(TTestVisitedAbs)
  private
    FOwned1: TTestVisited;
    FOwned3: TTestVisited;
    FOwned2: TTestVisited;
  published
    property Owned1 : TTestVisited read FOwned1;
    property Owned2 : TTestVisited read FOwned2;
    property Owned3 : TTestVisited read FOwned3;
  public
    constructor Create; override;
    destructor  Destroy; override;
  end;

  TSensingVisitorAbs = class abstract(TtiVisitor)
  private
    FData: TStringList;
  public
    constructor Create; override;
    destructor  Destroy; override;
    property    Data : TStringList read FData;
  end;

  TSensingVisitor = class (TSensingVisitorAbs)
  public
    procedure   Execute(const AVisited : TtiVisited); override;
  end;

  TTestVisitorIterate = class(TSensingVisitorAbs)
  public
    procedure   Execute(const AVisited : TtiVisited); override;
  end;

  TTestVisitorController = class(TtiVisitor)
  public
    function VisitorControllerClass : TtiVisitorControllerClass; override;
  end;


  TTestVisitorGetAllToVisit = class(TtiVisitor)
  protected
    function AcceptVisitor : boolean; override;
  end;


  TTestVisStream = class(TVisStream)
  public
    procedure Execute(const AVisited : TtiVisited); override;
    procedure Write(const AValue : string); override;
    procedure WriteLn(const AValue : string = ''); override;
  end;

procedure RegisterTests;


implementation
uses
  SysUtils
  ,tiUtils
  ,tiOPFManager
  ,TypInfo
  ,tiOPFTestManager
  ,tiDUnitDependencies
  ,tiStreams
  ,tstPerFramework_BOM
  ,tiRTTI
  ,tiObject
 ;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIVisitor);
end;


{ TTestTiPtnVis }

procedure TTestTIVisitor.VisClassCount_Execute;
var
  lVisited : TTestVisitedList;
  lVis : TVisClassCount;
begin
  lVisited := CreateListAndOwned;
  try
      lVis := TVisClassCount.Create;
      try
        lVisited.Iterate(lVis);
        CheckEquals(1,  lVis.ClassCount[TTestVisitedList],  'Failed TTestVisitedList');
        CheckEquals(3,  lVis.ClassCount[TTestVisitedOwned], 'Failed TTestVisitedOwned');
        // Probably not what you would expect (13 is more logical) because
        // TTestVisitedList and TTestVisitedOwned are both TestVisited, but
        // TVisClassCount does not know about inheritance.
        // This should be fixed when TVisClassCount is next being used.
        CheckEquals(9,  lVis.ClassCount[TTestVisited],      'Failed TTestVisited');
        CheckEquals(0, lVis.ClassCount[TtiVisited],        'Failed TtiVisited');
      finally
        lVis.Free;
      end;
  finally
    lVisited.Free;
  end;
end;


procedure TTestTIVisitor.VisFindAllByClass_Execute;
var
  lVisited : TTestVisitedList;
  lVis : TVisFindAllByClass;
  lList : TList;
begin
  lVisited := CreateListAndOwned;
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
    FID: string;
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
    result:= (Visited is TTestTIVisitedAcceptVisitor) and
      (Visited as TTestTIVisitedAcceptVisitor).Visit;
  end;

procedure TTestTIVisitor.Visited_AcceptVisitor(
  AIterationStyle: TtiIterationStyle);
var
  LItem1: TTestTIVisitedAcceptVisitor;
  LItem2: TTestTIVisitedAcceptVisitor;
  LItem3: TTestTIVisitedAcceptVisitor;
  LItem4: TTestTIVisitedAcceptVisitor;
  LVisitor: TTestVisitorAcceptVisitor;
begin
  LItem1:= nil;
  LItem2:= nil;
  LItem3:= nil;
  LItem4:= nil;
  LVisitor:= nil;
  try
    LItem1:= TTestTIVisitedAcceptVisitor.Create;

    LItem1.ID:= '1';
    LItem1.Visit:= False;

    LItem2:= TTestTIVisitedAcceptVisitor.Create;
    LItem2.ID:= '2';
    LItem2.Visit:= true;
    LItem1.Chain:= LItem2;

    LItem3:= TTestTIVisitedAcceptVisitor.Create;
    LItem3.ID:= '3';
    LItem3.Visit:= true;
    LItem2.Chain:= LItem3;

    LItem4:= TTestTIVisitedAcceptVisitor.Create;
    LItem4.ID:= '3';
    LItem4.Visit:= false;
    LItem3.Chain:= LItem4;

    LVisitor:= TTestVisitorAcceptVisitor.Create;
    LVisitor.IterationStyle:= AIterationStyle;
    LItem1.Iterate(LVisitor);
    CheckEquals(2, LVisitor.Data.Count);
    if AIterationStyle = isBottomUpSinglePass then
    begin
      CheckEquals('3', LVisitor.Data.Strings[0]);
      CheckEquals('2', LVisitor.Data.Strings[1]);
    end else
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
  lVisited : TtiVisited;
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
  lVisitedList : TTestVisitedList;
  lList : TList;
begin
  lVisitedList := CreateListAndOwned;
  try
    lList := TList.Create;
    try
      lVisitedList.FindAllByClassType(TTestVisitedList,  lList);
      CheckEquals(1, lList.Count, 'Failed on FindAllByClassType(TTestVisitedList)');

      lVisitedList.FindAllByClassType(TTestVisitedOwned, lList);
      CheckEquals(3, lList.Count, 'Failed on FindAllByClassType(TTestVisitedOwned)');

      lVisitedList.FindAllByClassType(TtiVisited,  lList);
      CheckEquals(13, lList.Count, 'Failed on FindAllByClassType(TtiVisitor)');

    finally
      lList.Free;
    end;
  finally
    lVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visited_Iterate_Single(
  AIterationStyle: TtiIterationStyle);
var
  lVisitor : TTestVisitorIterate;
  lVisited : TTestVisitedAbs;
begin
  lVisited := TTestVisitedAbs.Create;
  try
    lVisited.Index := 1;
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle:= AIterationStyle;
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
    function AcceptVisitor : boolean; override;
    property Result: boolean read FResult write FResult;
  end;

  function TTestVisitor_AcceptVisitor.AcceptVisitor: boolean;
  begin
    result := FResult;
  end;

procedure TTestTIVisitor.Visitor_AcceptVisitor;
var
  LVisitor : TTestVisitor_AcceptVisitor;
  LVisited: TtiVisited;
begin
  LVisitor := nil;
  LVisited:= nil;
  try
    LVisitor := TTestVisitor_AcceptVisitor.Create;
    LVisited:= TtiVisited.Create;

    LVisitor.Result:= true;
    Check(LVisitor.AcceptVisitor);
    Check(LVisitor.AcceptVisitor(LVisited));
    CheckSame(LVisited, LVisitor.Visited);

    LVisitor.SetVisited(nil);
    CheckNull(LVisitor.Visited);

    LVisitor.Result:= false;
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
    procedure   Execute(const AVisited : TtiVisited); override;
  end;

  constructor TTestVisitorContinueVisiting.Create;
  begin
    inherited;
    FCountVisited:= 0 ;
  end;

procedure TTestVisitorContinueVisiting.Execute(const AVisited: TtiVisited);
  begin
    inherited Execute(AVisited);
    Inc(FCountVisited);
    ContinueVisiting := FCountVisited < 5;
  end;

procedure TTestTIVisitor.Visitor_ContinueVisiting(const AIterationStyle: TtiIterationStyle);
var
  LVisitor : TTestVisitorContinueVisiting;
  LVisitedList : TTestVisitedList;
begin
  LVisitedList := CreateListAndOwned;
  try
    LVisitor := TTestVisitorContinueVisiting.Create;
    LVisitor.IterationStyle:= AIterationStyle;
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
    destructor  Destroy; override;
    procedure   Execute(const AVisited : TtiVisited); override;
    property    Results: TStringList read LResults;
  end;

  constructor TTestVisitorDepth.Create;
  begin
    inherited;
    LResults:= TStringList.Create;
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
  LVisitor : TTestVisitorDepth;
  LVisitedList : TTestVisitedList;
begin
  LVisitedList := CreateListAndOwned;
  try
    LVisitor := TTestVisitorDepth.Create;
    LVisitor.IterationStyle:= isBottomUpSinglePass;
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
  LVisitor : TTestVisitorDepth;
  LVisitedList : TTestVisitedList;
begin
  LVisitedList := CreateListAndOwned;
  try
    LVisitor := TTestVisitorDepth.Create;
    LVisitor.IterationStyle:= isTopDownRecurse;
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
  LVisitor : TTestVisitorDepth;
  LVisitedList : TTestVisitedList;
begin
  LVisitedList := CreateListAndOwned;
  try
    LVisitor := TTestVisitorDepth.Create;
    LVisitor.IterationStyle:= isTopDownSinglePass;
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
  lVisitor : TTestVisitor_Execute;
  lVisited : TtiVisited;
begin
  lVisitor := TTestVisitor_Execute.Create;
  try
    lVisited := TtiVisited.Create;
    try
      lVisitor.ExecuteCalled := false;
      lVisitor.Execute(lVisited);
      Check(lVisitor.ExecuteCalled);
    finally
      lVisited.Free;
    end;
  finally
    lVisitor.Free;
  end;
end;


procedure TTestTIVisitor.Visitor_VisitorController;
var
  lVis : TtiVisitor;
begin
  lVis := TTestVisitorController.Create;
  try
    CheckEquals(lVis.VisitorControllerClass, TtiVisitorCtrlr);
    CheckNull(lVis.VisitorController);
    lVis.VisitorController := lVis.VisitorControllerClass.Create;
    try
      CheckNotNull(lVis.VisitorController);
      CheckIs(lVis.VisitorController, TtiVisitorCtrlr);
    finally
      lVis.VisitorController.Free;
    end;
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


procedure TTestTIVisitor.VisStream;
var
  lVis : TTestVisStream;
  lStream : TtiPreSizedStream;
  lVisitedList : TTestVisitedList;
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
      lVisitedList := CreateListAndOwned;
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


procedure TTestTIVisitor.VisStreamToFile;
  procedure _CheckFileEquals(const AFileName : TFileName;
                              const AValue : string);
  var
    lFileStream : TFileStream;
    lStringStream : TStringStream;
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
  LVisitedList : TTestVisitedList;
  LFileName : TFileName;
begin
  LFileName := TempFileName;
  try
    LVisitedList := CreateListAndOwned;
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
    FRecurseCount: Integer;
  protected
    function    AcceptVisitor : boolean; override;
  public
    constructor Create; override;
    procedure   Execute(const AVisited : TtiVisited); override;
  end;

  function TTestVisitorTopDownRecurse.AcceptVisitor: boolean;
  begin
    result:= (Visited is TtiObjectList) and
             (FRecurseCount <= 2);
  end;

  constructor TTestVisitorTopDownRecurse.Create;
  begin
    inherited;
    FRecurseCount:= 0;
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
  LData: TtiObjectList;
  LVisitor: TTestVisitorTopDownRecurse;
begin
  LData:= TtiObjectList.Create;
  try
    LVisitor:= TTestVisitorTopDownRecurse.Create;
    LVisitor.IterationStyle:= isTopDownRecurse;
    try
      LData.Iterate(LVisitor);
      CheckEquals(1, LData.Count);
      CheckEquals(1, (LData.Items[0] as TtiObjectList).Count);
      CheckEquals(1, ((LData.Items[0] as TtiObjectList).Items[0] as TtiObjectList).Count);
      CheckEquals(0, (((LData.Items[0] as TtiObjectList).Items[0] as TtiObjectList).Items[0] as TtiObjectList).Count);
    finally
      LVisitor.Free;
    end;
  finally
    LData.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Recurse_TopDownSinglePass;
var
  LData: TtiObjectList;
  LVisitor: TTestVisitorTopDownRecurse;
begin
  LData:= TtiObjectList.Create;
  try
    LVisitor:= TTestVisitorTopDownRecurse.Create;
    LVisitor.IterationStyle:= isTopDownSinglePass;
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
    FApplyTest: boolean;
  protected
    function VisitBranch(const ADerivedParent, AVisited: TtiVisited) : boolean; override;
  public
    property ApplyTest: boolean read FApplyTest write FApplyTest;
  end;

{ TTestVisitedOverrideVisitBranchChild1 }

  constructor TTestVisitedVisitBranchChild1.Create;
  begin
    inherited;
    FData:= TTestVisitedVisitBranchChild2.Create;
  end;

  destructor TTestVisitedVisitBranchChild1.Destroy;
  begin
    FData.Free;
    inherited;
  end;

  constructor TTestVisitedVisitBranch.Create;
  begin
    inherited;
    FData:= TTestVisitedVisitBranchChild1.Create;
  end;

  destructor TTestVisitedVisitBranch.Destroy;
  begin
    FData.Free;
    inherited;
  end;

  function TTestVisitorVisitBranch.VisitBranch(const ADerivedParent, AVisited: TtiVisited) : boolean;
  begin
    result:= (not ApplyTest) or
             (ApplyTest and not (AVisited is TTestVisitedVisitBranchChild1));
  end;

procedure TTestTIVisitor.Visited_VisitBranch(
  AIterationStyle: TtiIterationStyle);
var
  LVisited: TTestVisitedVisitBranch;
  LVisitor: TTestVisitorVisitBranch;
begin
  LVisited:= nil;
  LVisitor:= nil;
  try
    LVisited:= TTestVisitedVisitBranch.Create;
    LVisitor:= TTestVisitorVisitBranch.Create;
    LVisitor.ApplyTest:= False;
    LVisited.Iterate(LVisitor);
    CheckEquals(3, LVisitor.Data.Count);

  finally
    LVisited.Free;
    LVisitor.Free;
  end;

  LVisited:= nil;
  LVisitor:= nil;
  try
    LVisited:= TTestVisitedVisitBranch.Create;
    LVisitor:= TTestVisitorVisitBranch.Create;
    LVisitor.ApplyTest:= True;
    LVisited.Iterate(LVisitor);
    CheckEquals(1, LVisitor.Data.Count);

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
  LData: TtiObjectList;
  LVisitor: TTestVisitorTopDownRecurse;
begin
  LData:= TtiObjectList.Create;
  try
    LVisitor:= TTestVisitorTopDownRecurse.Create;
    LVisitor.IterationStyle:= isBottomUpSinglePass;
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
  lVisitor : TTestVisitorIterate;
  lVisited : TTestVisitedList;
begin
  lVisited := CreateList;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle:= isBottomUpSinglePass;
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
    procedure Iterate(const AVisitor: TtiVisitor); override;
  published
    property Data: TtiVisited read FData;
  end;

  constructor TTestVisitedOverrideIterate.Create;
  begin
    inherited;
    FData:= TTestVisitedOverrideIterateChild.Create;
  end;

  destructor TTestVisitedOverrideIterate.destroy;
  begin
    FData.Free;
    inherited;
  end;

  constructor TTestVisitedOverrideIterateChild.Create;
  begin
    inherited;
    FData:= TtiVisited.Create;
  end;

  destructor TTestVisitedOverrideIterateChild.Destroy;
  begin
    FData.Free;
    inherited;
  end;

  procedure TTestVisitedOverrideIterateChild.Iterate(const AVisitor: TtiVisitor);
  begin
    AVisitor.Execute(Self);
  end;

procedure TTestTIVisitor.Visited_Iterate_Override;
var
  LVisitor: TSensingVisitorAbs;
  LVisited: TTestVisitedOverrideIterate;
begin
  LVisitor:= nil;
  LVisited:= nil;
  try
     LVisitor:= TSensingVisitor.Create;
     LVisited:= TTestVisitedOverrideIterate.Create;

     LVisited.Iterate(LVisitor);
     CheckEquals(2, LVisitor.Data.Count);
     CheckEquals(TTestVisitedOverrideIterate.ClassName, LVisitor.Data.Strings[0]);
     CheckEquals(TTestVisitedOverrideIterateChild.ClassName, LVisitor.Data.Strings[1]);

  finally
    LVisitor.Free;
    LVisited.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Iterate_Owned(
  AIterationStyle: TtiIterationStyle);
var
  lVisitor : TTestVisitorIterate;
  lVisited : TTestVisitedOwned;
begin
  lVisited := CreateOwned;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle:= AIterationStyle;
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
  lVisitor : TTestVisitorIterate;
  lVisited : TTestVisitedOwned;
begin
  lVisited := CreateOwned;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle:= isBottomUpSinglePass;
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

procedure TTestTIVisitor.Visited_Iterate_List(
  AIterationStyle: TtiIterationStyle);
var
  lVisitor : TTestVisitorIterate;
  lVisited : TTestVisitedList;
begin
  lVisited := CreateList;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle:= AIterationStyle;
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

procedure TTestTIVisitor.Visited_Iterate_ListAndOwned(
  AIterationStyle: TtiIterationStyle);
var
  lVisitor : TTestVisitorIterate;
  lVisitedList : TTestVisitedList;
  i : integer;
begin
  lVisitedList := CreateListAndOwned;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle:= AIterationStyle;
      lVisitedList.Iterate(lVisitor);
      CheckEquals(13, lVisitor.Data.Count);
      for i := 1 to 13 do
        CheckEquals(IntToStr(i), lVisitor.Data.Strings[i-1]);
    finally
      lVisitor.Free;
    end;
  finally
    lVisitedList.Free;
  end;
end;

procedure TTestTIVisitor.Visited_Iterate_ListAndOwned_BottomUpSinglePass;
var
  lVisitor : TTestVisitorIterate;
  lVisitedList : TTestVisitedList;
  i : integer;
begin
  lVisitedList := CreateListAndOwned;
  try
    lVisitor := TTestVisitorIterate.Create;
    try
      LVisitor.IterationStyle:= isBottomUpSinglePass;
      lVisitedList.Iterate(lVisitor);
      CheckEquals(13, lVisitor.Data.Count);
      for i := 1 to 13 do
        CheckEquals(IntToStr(i), lVisitor.Data.Strings[13-i]);
    finally
      lVisitor.Free;
    end;
  finally
    lVisitedList.Free;
  end;
end;


function TTestTIVisitor.CreateListAndOwned: TTestVisitedList;
var
  lVisitedOwned : TTestVisitedOwned;
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
  result := TTestVisitedList.Create;
  result.Index := 1;
  lVisitedOwned := TTestVisitedOwned.Create;
  lVisitedOwned.Index := 2;
  lVisitedOwned.Owned1.Index := 3;
  lVisitedOwned.Owned2.Index := 4;
  lVisitedOwned.Owned3.Index := 5;
  result.List.Add(lVisitedOwned);

  lVisitedOwned := TTestVisitedOwned.Create;
  lVisitedOwned.Index := 6;
  lVisitedOwned.Owned1.Index := 7;
  lVisitedOwned.Owned2.Index := 8;
  lVisitedOwned.Owned3.Index := 9;
  result.List.Add(lVisitedOwned);

  lVisitedOwned := TTestVisitedOwned.Create;
  lVisitedOwned.Index := 10;
  lVisitedOwned.Owned1.Index := 11;
  lVisitedOwned.Owned2.Index := 12;
  lVisitedOwned.Owned3.Index := 13;
  result.List.Add(lVisitedOwned);
end;


function TTestTIVisitor.CreateList: TTestVisitedList;
var
  lData : TTestVisited;
begin
  result := TTestVisitedList.Create;
  result.Index := 1;

  lData   := TTestVisited.Create;
  lData.Index := 2;
  result.List.Add(lData);

  lData   := TTestVisited.Create;
  lData.Index := 3;
  result.List.Add(lData);

  lData   := TTestVisited.Create;
  lData.Index := 4;
  result.List.Add(lData);

  lData   := TTestVisited.Create;
  lData.Index := 5;
  result.List.Add(lData);
end;


function TTestTIVisitor.CreateOwned: TTestVisitedOwned;
begin
  result := TTestVisitedOwned.Create;
  result.Index := 1;
  result.Owned1.Index := 2;
  result.Owned2.Index := 3;
  result.Owned3.Index := 4;
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
  gTIOPFManager.Terminated := false;
end;

{ TTestVisitor_Execute }

procedure TTestVisitor_Execute.Execute(const AVisited : TtiVisited);
begin
  inherited Execute(AVisited);
  ExecuteCalled := true;
end;


{ TTestVisitorIterate }

procedure TTestVisitorIterate.Execute(const AVisited: TtiVisited);
var
  LIndex: string;
begin
  inherited Execute(AVisited);
  Assert(AVisited is TTestVisitedAbs, 'AVisited not a TTestVisitedAbs');
  LIndex:= IntToStr((AVisited as TTestVisitedAbs).Index);
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
  result := FList;
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

function TTestVisitorController.VisitorControllerClass: TtiVisitorControllerClass;
begin
  result := TtiVisitorCtrlr;
end;

{ TTestVisitorGetAllToVisit }

function TTestVisitorGetAllToVisit.AcceptVisitor: boolean;
begin
  result := Visited is TTestVisitedOwned;
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
  FData:= TStringList.Create;
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

{ TTestVisitorVisitBranch }

end.

