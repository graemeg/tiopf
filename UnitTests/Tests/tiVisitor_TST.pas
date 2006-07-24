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
  TTestVisitedList = class ;
  TTestVisitedOwned = class ;


  TTestTIVisitor = class(TtiTestCase)
  private
    function CreateListAndOwned : TTestVisitedList ;
    function CreateList : TTestVisitedList ;
    function CreateOwned : TTestVisitedOwned ;
  protected
    procedure SetUp ; override ;
  published
    // RTTI Support
    procedure tiGetPropertyNames;
    procedure tiGetSimplePropType;
    procedure tiVarSimplePropType;
    procedure tiIsNumericProp;
    procedure tiIsReadWriteProp;

    // Test the visitor
    procedure Visitor_AcceptVisitor;
    procedure Visitor_DoAcceptVisitor;
    procedure Visitor_Execute;
    procedure Visitor_ContinueVisiting;
    procedure Visitor_VisitorController;
    procedure Visitor_Depth;

    // Test the visited
    procedure Visited_Caption;

    procedure Visited_IterateSingle;
    procedure Visited_IterateList;
    procedure Visited_IterateOwned;
    procedure Visited_IterateListAndOwned;
    procedure Visited_IterateBottomUpSingle;
    procedure Visited_IterateBottomUpList;
    procedure Visited_IterateBottomUpOwned;
    procedure Visited_IterateBottupUpListAndOwned;

    procedure Visited_SelfIterate;
    procedure Visited_FindAllByClassType;
    procedure Visited_ClassCount ;

    // Test some other special visitors
    procedure VisGetAllToVisit_Execute;
    procedure VisClassCount_Execute;
    procedure VisFindAllByClass_Execute;

    // Test the special stream writing visitor
    procedure VisStream;

    // Test the TFileStream visitor wrapper
    procedure VisStreamToFile;
  end ;


  TTestVisitor_AcceptVisitor = class( TtiVisitor )
  public
    function AcceptVisitor : boolean ; override ;
  end ;


  TTestVisitor_DoAcceptVisitor = class( TtiVisitor )
  public
    function AcceptVisitor : boolean ; override ;
  end ;


  TTestVisitor_Execute = class( TtiVisitor )
  private
    FExecuteCalled: boolean;
  public
    property  ExecuteCalled : boolean read FExecuteCalled write FExecuteCalled ;
    procedure Execute( const pVisited : TtiVisited ) ; override ;
  end ;


  TTestVisitedAbs = class( TtiVisited )
  private
    FIndex: Word;
  public
    property Index : Word read FIndex write FIndex ;
  end;


  TTestVisited = class( TTestVisitedAbs ) ;


  TTestVisitedList = class( TTestVisitedAbs )
  private
    FList: TObjectList;
    function GetList: TList;
  published
    property    List : TList read GetList ;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
  end ;


  TTestVisitedOwned = class( TTestVisitedAbs )
  private
    FOwned1: TTestVisited;
    FOwned3: TTestVisited;
    FOwned2: TTestVisited;
  published
    property Owned1 : TTestVisited read FOwned1;
    property Owned2 : TTestVisited read FOwned2;
    property Owned3 : TTestVisited read FOwned3;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
  end ;


  TTestVisitorIterate = class( TtiVisitor )
  private
    FIndexes: TStringList;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
    property    Indexes : TStringList read FIndexes ;
  end ;


  TTestVisitorContinueVisiting = class( TTestVisitorIterate )
  public
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
  end ;


  TTestVisitorDepth = class( TTestVisitorIterate )
  public
    procedure   Execute( const pVisited : TtiVisited ) ; override ;
  end ;


  TTestVisitorController = class( TtiVisitor )
  public
    function VisitorControllerClass : TtiVisitorControllerClass ; override ;
  end ;


  TTestVisitorGetAllToVisit = class( TtiVisitor )
  protected
    function AcceptVisitor : boolean ; override ;
  end;


  TTestVisStream = class( TVisStream )
  public
    procedure Execute( const pVisited : TtiVisited ) ; override ;
    procedure Write( const psValue : string ) ; override ;
    procedure WriteLn( const psValue : string = '') ; override ;
  end ;


const
  cTestDoAcceptVisitorException = 'Test DoAcceptVisitor exception.' ;


procedure RegisterTests ;


implementation
uses
  SysUtils
//  ,tiDialogs
  ,tiUtils
  ,tiOPFManager
  ,TypInfo
  ,tiOPFTestManager
  ,tiDUnitDependencies
  ,tiStreams
  ,tstPerFramework_BOM
  ;


procedure RegisterTests ;
begin
  RegisterNonPersistentTest(TTestTIVisitor);
end ;


{ TTestTiPtnVis }

procedure TTestTIVisitor.VisClassCount_Execute;
var
  lVisited  : TTestVisitedList ;
  lVis : TVisClassCount;
begin
  lVisited := CreateListAndOwned ;
  try
      lVis := TVisClassCount.Create ;
      try
        lVisited.Iterate( lVis ) ;
        CheckEquals( 1,  lVis.ClassCount[TTestVisitedList],  'Failed TTestVisitedList' );
        CheckEquals( 3,  lVis.ClassCount[TTestVisitedOwned], 'Failed TTestVisitedOwned' );
        // Probably not what you would expect (13 is more logical) because
        // TTestVisitedList and TTestVisitedOwned are both TestVisited, but
        // TVisClassCount does not know about inheritance.
        // This should be fixed when TVisClassCount is next being used.
        CheckEquals( 9,  lVis.ClassCount[TTestVisited],      'Failed TTestVisited' );
        CheckEquals( 0, lVis.ClassCount[TtiVisited],        'Failed TtiVisited' );
      finally
        lVis.Free ;
      end ;
  finally
    lVisited.Free;
  end;
end;


procedure TTestTIVisitor.VisFindAllByClass_Execute;
var
  lVisited  : TTestVisitedList ;
  lVis : TVisFindAllByClass;
  lList : TList ;
begin
  lVisited := CreateListAndOwned ;
  try
    lList := TList.Create ;
    try
      lVis := TVisFindAllByClass.Create ;
      try
        lVis.List := lList ;

        lVis.ClassTypeToFind := TTestVisitedList ;
        lVisited.Iterate( lVis ) ;
        CheckEquals( 1, lList.Count, 'Failed TTestVisitedList' );

        lList.Clear ;
        lVis.ClassTypeToFind := TTestVisitedOwned ;
        lVisited.Iterate( lVis ) ;
        CheckEquals( 3, lList.Count, 'Failed TTestVisitedOwned' );

        lList.Clear ;
        lVis.ClassTypeToFind := TtiVisited ;
        lVisited.Iterate( lVis ) ;
        CheckEquals( 13, lList.Count, 'Failed TtiVisited' );

      finally
        lVis.Free ;
      end ;
    finally
      lList.Free ;
    end ;
  finally
    lVisited.Free;
  end;
end;


procedure TTestTIVisitor.VisGetAllToVisit_Execute;
var
  lVisGetAllToVisit : TtiVisGetAllToVisit ;
  lVisitor : TTestVisitorGetAllToVisit ;
  lVisitedList  : TTestVisitedList ;
begin
  lVisitedList := CreateListAndOwned ;
  try
    lVisitor := TTestVisitorGetAllToVisit.Create ;
    try
      lVisGetAllToVisit := TtiVisGetAllToVisit.Create ;
      try
        lVisGetAllToVisit.Visitor := lVisitor ;
        lVisitedList.Iterate( lVisGetAllToVisit ) ;
        CheckEquals( 3, lVisGetAllToVisit.List.Count ) ;
      finally
        lVisGetAllToVisit.Free ;
      end ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visited_Caption;
var
  lVisited : TtiVisited ;
begin
  lVisited := TtiVisited.Create ;
  try
    CheckEquals( lVisited.Caption, lVisited.ClassName ) ;
  finally
    lVisited.Free ;
  end ;
end;


procedure TTestTIVisitor.Visited_FindAllByClassType;
var
  lVisitedList  : TTestVisitedList ;
  lList : TList ;
begin
  lVisitedList := CreateListAndOwned ;
  try
    lList := TList.Create ;
    try
      lVisitedList.FindAllByClassType( TTestVisitedList,  lList ) ;
      CheckEquals( 1, lList.Count, 'Failed on FindAllByClassType( TTestVisitedList )' );

      lVisitedList.FindAllByClassType( TTestVisitedOwned, lList ) ;
      CheckEquals( 3, lList.Count, 'Failed on FindAllByClassType( TTestVisitedOwned )' );

      lVisitedList.FindAllByClassType( TtiVisited,  lList ) ;
      CheckEquals( 13, lList.Count, 'Failed on FindAllByClassType( TtiVisitor )' );

    finally
      lList.Free ;
    end ;
  finally
    lVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visited_IterateBottomUpSingle;
begin
  Visited_IterateSingle;
end;


procedure TTestTIVisitor.Visited_SelfIterate;
var
  lVisitor : TTestVisitorIterate ;
  lVisitedOwned : TTestVisitedOwned ;
  lVisitedList  : TTestVisitedList ;
begin
  lVisitedList  := TTestVisitedList.Create ;
  lVisitedList.Index := 1 ;
  try
    lVisitedOwned := TTestVisitedOwned.Create ;
    lVisitedOwned.Index := 2 ;
    lVisitedOwned.Owned1.Index := 3 ;
    lVisitedOwned.Owned2.Index := 4 ;
    lVisitedOwned.Owned3.Index := 5 ;
    lVisitedList.List.Add( lVisitedOwned ) ;

    lVisitedOwned := TTestVisitedOwned.Create ;
    lVisitedOwned.Index := 6 ;
    lVisitedOwned.Owned1.Index := 7 ;
    lVisitedOwned.Owned2.Index := 8 ;
    lVisitedOwned.Owned3.Index := 9 ;
    lVisitedList.List.Add( lVisitedOwned ) ;

    lVisitedOwned := TTestVisitedOwned.Create ;
    lVisitedOwned.Index := 10 ;
    lVisitedOwned.Owned1.Index := 11 ;
    lVisitedOwned.Owned2.Index := 12 ;
    lVisitedOwned.Owned3.Index := 13 ;
    lVisitedList.List.Add( lVisitedOwned ) ;

    lVisitedList.SelfIterate := false ;
    lVisitor := TTestVisitorIterate.Create ;
    try
      lVisitedList.Iterate( lVisitor ) ;
      CheckEquals( 1, lVisitor.Indexes.Count ) ;
      CheckEquals( '1', lVisitor.Indexes.Strings[0] ) ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visitor_AcceptVisitor;
var
  lVis : TTestVisitor_AcceptVisitor ;
begin
  lVis := TTestVisitor_AcceptVisitor.Create ;
  try
    Check( lVis.AcceptVisitor ) ;
  finally
    lVis.Free ;
  end ;
end;


procedure TTestTIVisitor.Visitor_ContinueVisiting;
var
  lVisitor : TTestVisitorContinueVisiting ;
  lVisitedList  : TTestVisitedList ;
begin
  lVisitedList := CreateListAndOwned ;
  try
    lVisitor := TTestVisitorContinueVisiting.Create ;
    try
      lVisitedList.Iterate( lVisitor ) ;
      CheckEquals( 5, lVisitor.Indexes.Count ) ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visitor_Depth;
var
  lVisitor : TTestVisitorDepth ;
  lVisitedList  : TTestVisitedList ;
begin
  lVisitedList := CreateListAndOwned ;
  try
    lVisitor := TTestVisitorDepth.Create ;
    try
      lVisitedList.Iterate( lVisitor ) ;
      CheckEquals( '1', lVisitor.Indexes.Values['1'] );
      CheckEquals( '2', lVisitor.Indexes.Values['2'] );
      CheckEquals( '3', lVisitor.Indexes.Values['3'] );
      CheckEquals( '3', lVisitor.Indexes.Values['4'] );
      CheckEquals( '3', lVisitor.Indexes.Values['5'] );
      CheckEquals( '2', lVisitor.Indexes.Values['6'] );
      CheckEquals( '3', lVisitor.Indexes.Values['7'] );
      CheckEquals( '3', lVisitor.Indexes.Values['8'] );
      CheckEquals( '3', lVisitor.Indexes.Values['9'] );
      CheckEquals( '2', lVisitor.Indexes.Values['10'] );
      CheckEquals( '3', lVisitor.Indexes.Values['11'] );
      CheckEquals( '3', lVisitor.Indexes.Values['12'] );
      CheckEquals( '3', lVisitor.Indexes.Values['13'] );
    finally
      lVisitor.Free ;
    end;
  finally
    lVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visitor_DoAcceptVisitor;
var
  lVis : TTestVisitor_DoAcceptVisitor ;
begin
  lVis := TTestVisitor_DoAcceptVisitor.Create ;
  try
    try
      lVis.AcceptVisitor ;
      Check( False, 'Exception not raised' ) ;
    except
      on e:exception do
        Check( Pos( cTestDoAcceptVisitorException,
                    e.Message ) <> 0, 'Exception does not contain correct error message.' ) ;
    end;
  finally
    lVis.Free ;
  end ;
end;


procedure TTestTIVisitor.Visitor_Execute;
var
  lVisitor : TTestVisitor_Execute ;
  lVisited : TtiVisited ;
begin
  lVisitor := TTestVisitor_Execute.Create ;
  try
    lVisited := TtiVisited.Create ;
    try
      lVisitor.ExecuteCalled := false ;
      lVisitor.Execute( lVisited ) ;
      Check( lVisitor.ExecuteCalled ) ;
    finally
      lVisited.Free ;
    end ;
  finally
    lVisitor.Free ;
  end;
end;


procedure TTestTIVisitor.Visitor_VisitorController;
var
  lVis : TtiVisitor ;
begin
  lVis := TTestVisitorController.Create ;
  try
    CheckEquals( lVis.VisitorControllerClass, TtiVisitorCtrlr ) ;
    CheckNull( lVis.VisitorController ) ;
    lVis.VisitorController := lVis.VisitorControllerClass.Create ;
    try
      CheckNotNull( lVis.VisitorController ) ;
      CheckIs( lVis.VisitorController, TtiVisitorCtrlr ) ;
    finally
      lVis.VisitorController.Free;
    end;
  finally
    lVis.Free ;
  end ;
end;

{
  TVisStream = class( TtiVisitor )
  private
    FStream : TStream ;
  protected
    procedure Write( const psValue : string ) ;
    procedure WriteLn ; overload ;
    procedure WriteLn( const psValue : string ) ; overload ;
    procedure SetStream(const Value: TStream) ; virtual ;
  public
    property  Stream : TStream read FStream write SetStream ;
  end ;
}


procedure TTestTIVisitor.VisStream;
var
  lVis : TTestVisStream ;
  lStream : TtiPreSizedStream ;
  lVisitedList  : TTestVisitedList ;
const
  cATestLine = 'A test line' ;
begin
  lVis := TTestVisStream.Create ;
  try
    lStream := TtiPreSizedStream.Create( 1000, 100 ) ;
    try
      lVis.Stream := lStream ;
      lVis.Write( cATestLine ) ;
      CheckEquals( cATestLine, lStream.AsString, 'Failed on Write' ) ;

      lStream.Clear ;
      lVis.WriteLn ;
      CheckEquals( #13 + #10, lStream.AsString, 'Failed on WriteLn' ) ;

      lStream.Clear;
      lVis.WriteLn( cATestLine ) ;
      CheckEquals( cATestLine + #13 + #10, lStream.AsString, 'Failed on WriteLn( cATestLine )' ) ;

      lStream.Clear;
      lVisitedList := CreateListAndOwned ;
      try
        lVisitedList.Iterate( lVis ) ;
        CheckEquals( '1,2,3,4,5,6,7,8,9,10,11,12,13', lStream.AsString )
      finally
        lVisitedList.Free;
      end;
    finally
      lStream.Free ;
    end ;
  finally
    lVis.Free ;
  end;
end;


procedure TTestTIVisitor.VisStreamToFile;
  procedure _CheckFileEquals( const pFileName : TFileName ;
                              const pValue : string ) ;
  var
    lFileStream : TFileStream ;
    lStringStream : TStringStream ;
  begin
    lFileStream :=
      TFileStream.Create( pFileName,
                          fmOpenRead or fmShareDenyNone ) ;
    try
      lStringStream := TStringStream.Create( '' ) ;
      try
        lStringStream.CopyFrom( lFileStream, lFileStream.Size ) ;
        CheckEquals( pValue, lStringStream.DataString ) ;
      finally
        lStringStream.Free ;
      end ;
    finally
      lFileStream.Free ;
    end ;
  end ;

var
  LVisitedList  : TTestVisitedList ;
  LFileName : TFileName ;
begin
  LFileName := TempFileName ;
  try
    LVisitedList := CreateListAndOwned ;
    try
      tiVisitor.VisStreamToFile( LVisitedList,
                                LFileName,
                                TTestVisStream );
      _CheckFileEquals( LFileName, '1,2,3,4,5,6,7,8,9,10,11,12,13' ) ;

      tiVisitor.VisStreamToFile( LVisitedList,
                                LFileName,
                                TTestVisStream);
      _CheckFileEquals( LFileName, '1,2,3,4,5,6,7,8,9,10,11,12,13' ) ;

      tiVisitor.VisStreamToFile( LVisitedList,
                                LFileName,
                                TTestVisStream);

    finally
      LVisitedList.Free ;
    end ;
  finally
    DeleteFile(LFileName);
  end;
end;


procedure TTestTIVisitor.Visited_IterateList;
var
  lVisitor : TTestVisitorIterate ;
  lVisited : TTestVisitedList;
begin
  lVisited := CreateList ;
  try
    lVisitor := TTestVisitorIterate.Create ;
    try
      lVisited.Iterate( lVisitor ) ;
      CheckEquals( lVisitor.Indexes.Count, 5 ) ;
      CheckEquals( '1', lVisitor.Indexes.Strings[0] ) ;
      CheckEquals( '2', lVisitor.Indexes.Strings[1] ) ;
      CheckEquals( '3', lVisitor.Indexes.Strings[2] ) ;
      CheckEquals( '4', lVisitor.Indexes.Strings[3] ) ;
      CheckEquals( '5', lVisitor.Indexes.Strings[4] ) ;

    finally
      lVisitor.Free ;
    end;
  finally
    lVisited.Free ;
  end;
end;


procedure TTestTIVisitor.Visited_IterateOwned;
var
  lVisitor : TTestVisitorIterate ;
  lVisited : TTestVisitedOwned ;
begin
  lVisited := CreateOwned ;
  try
    lVisitor := TTestVisitorIterate.Create ;
    try
      lVisited.Iterate( lVisitor ) ;
      CheckEquals( 4, lVisitor.Indexes.Count ) ;
      CheckEquals( '1', lVisitor.Indexes.Strings[0] ) ;
      CheckEquals( '2', lVisitor.Indexes.Strings[1] ) ;
      CheckEquals( '3', lVisitor.Indexes.Strings[2] ) ;
      CheckEquals( '4', lVisitor.Indexes.Strings[3] ) ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisited.Free ;
  end;
end;


procedure TTestTIVisitor.Visited_IterateSingle;
var
  lVisitor : TTestVisitorIterate ;
  lVisited : TTestVisitedAbs ;
begin
  lVisited := TTestVisitedAbs.Create ;
  try
    lVisited.Index := 1 ;
    lVisitor := TTestVisitorIterate.Create ;
    try
      lVisited.Iterate( lVisitor ) ;
      CheckEquals( 1, lVisitor.Indexes.Count ) ;
      CheckEquals( '1', lVisitor.Indexes.Strings[0] ) ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisited.Free ;
  end;
end;


procedure TTestTIVisitor.Visited_IterateBottomUpList;
var
  lVisitor : TTestVisitorIterate ;
  lVisited : TTestVisitedList;
begin
  lVisited := CreateList ;
  try
    lVisitor := TTestVisitorIterate.Create ;
    try
      lVisited.IterateBottomUp( lVisitor ) ;
      CheckEquals( lVisitor.Indexes.Count, 5 ) ;
      CheckEquals( '5', lVisitor.Indexes.Strings[0] ) ;
      CheckEquals( '4', lVisitor.Indexes.Strings[1] ) ;
      CheckEquals( '3', lVisitor.Indexes.Strings[2] ) ;
      CheckEquals( '2', lVisitor.Indexes.Strings[3] ) ;
      CheckEquals( '1', lVisitor.Indexes.Strings[4] ) ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisited.Free ;
  end;
end;


procedure TTestTIVisitor.Visited_IterateBottomUpOwned;
var
  lVisitor : TTestVisitorIterate ;
  lVisited : TTestVisitedOwned ;
begin
  lVisited := CreateOwned ;
  try
    lVisitor := TTestVisitorIterate.Create ;
    try
      lVisited.IterateBottomUp( lVisitor ) ;
      CheckEquals( 4, lVisitor.Indexes.Count ) ;
      CheckEquals( '4', lVisitor.Indexes.Strings[0] ) ;
      CheckEquals( '3', lVisitor.Indexes.Strings[1] ) ;
      CheckEquals( '2', lVisitor.Indexes.Strings[2] ) ;
      CheckEquals( '1', lVisitor.Indexes.Strings[3] ) ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisited.Free ;
  end;
end;


procedure TTestTIVisitor.Visited_IterateListAndOwned;
var
  lVisitor : TTestVisitorIterate ;
  lVisitedList  : TTestVisitedList ;
  i : integer ;
begin
  lVisitedList := CreateListAndOwned ;
  try
    lVisitor := TTestVisitorIterate.Create ;
    try
      lVisitedList.Iterate( lVisitor ) ;
      CheckEquals( 13, lVisitor.Indexes.Count ) ;
      for i := 1 to 13 do
        CheckEquals( IntToStr( i ), lVisitor.Indexes.Strings[i-1] ) ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisitedList.Free;
  end;
end;


procedure TTestTIVisitor.Visited_IterateBottupUpListAndOwned;
var
  lVisitor : TTestVisitorIterate ;
  lVisitedList  : TTestVisitedList ;
  i : integer ;
begin
  lVisitedList  := CreateListAndOwned ;
  try
    lVisitor := TTestVisitorIterate.Create ;
    try
      lVisitedList.IterateBottomUp( lVisitor ) ;
      CheckEquals( 13, lVisitor.Indexes.Count ) ;
      for i := 1 to 13 do
        CheckEquals( IntToStr( i ), lVisitor.Indexes.Strings[13-i] ) ;
    finally
      lVisitor.Free ;
    end;
  finally
    lVisitedList.Free;
  end;
end;


function TTestTIVisitor.CreateListAndOwned: TTestVisitedList;
var
  lVisitedOwned : TTestVisitedOwned ;
begin
  result  := TTestVisitedList.Create ;
  result.Index := 1 ;
  lVisitedOwned := TTestVisitedOwned.Create ;
  lVisitedOwned.Index := 2 ;
  lVisitedOwned.Owned1.Index := 3 ;
  lVisitedOwned.Owned2.Index := 4 ;
  lVisitedOwned.Owned3.Index := 5 ;
  result.List.Add( lVisitedOwned ) ;

  lVisitedOwned := TTestVisitedOwned.Create ;
  lVisitedOwned.Index := 6 ;
  lVisitedOwned.Owned1.Index := 7 ;
  lVisitedOwned.Owned2.Index := 8 ;
  lVisitedOwned.Owned3.Index := 9 ;
  result.List.Add( lVisitedOwned ) ;

  lVisitedOwned := TTestVisitedOwned.Create ;
  lVisitedOwned.Index := 10 ;
  lVisitedOwned.Owned1.Index := 11 ;
  lVisitedOwned.Owned2.Index := 12 ;
  lVisitedOwned.Owned3.Index := 13 ;
  result.List.Add( lVisitedOwned ) ;
end;


function TTestTIVisitor.CreateList: TTestVisitedList;
var
  lData : TTestVisited ;
begin
  result := TTestVisitedList.Create ;
  result.Index := 1 ;

  lData    := TTestVisited.Create ;
  lData.Index := 2 ;
  result.List.Add( lData ) ;

  lData    := TTestVisited.Create ;
  lData.Index := 3 ;
  result.List.Add( lData ) ;

  lData    := TTestVisited.Create ;
  lData.Index := 4 ;
  result.List.Add( lData ) ;

  lData    := TTestVisited.Create ;
  lData.Index := 5 ;
  result.List.Add( lData ) ;
end;


function TTestTIVisitor.CreateOwned: TTestVisitedOwned;
begin
  result := TTestVisitedOwned.Create ;
  result.Index := 1 ;
  result.Owned1.Index := 2 ;
  result.Owned2.Index := 3 ;
  result.Owned3.Index := 4 ;
end;


procedure TTestTIVisitor.Visited_ClassCount;
var
  lVisited  : TTestVisitedList ;
begin
  lVisited := CreateListAndOwned ;
  try
    CheckEquals( 1,  lVisited.CountByClass(TTestVisitedList),  'Failed TTestVisitedList' );
    CheckEquals( 3,  lVisited.CountByClass(TTestVisitedOwned), 'Failed TTestVisitedOwned' );
    CheckEquals( 9,  lVisited.CountByClass(TTestVisited),       'Failed TTestVisited' );
    CheckEquals( 13, lVisited.CountByClass(TtiVisited),        'Failed TtiVisited' );
  finally
    lVisited.Free;
  end;
end;


procedure TTestTIVisitor.SetUp;
begin
  inherited;
  gTIOPFManager.Terminated := false ;
end;


{ TTestVisitor_DoAcceptVisitor }

function TTestVisitor_DoAcceptVisitor.AcceptVisitor: boolean;
begin
  Result := True;   // To get rid of compiler warning
  raise exception.Create( cTestDoAcceptVisitorException ) ;
end;


{ TTestVisitor_AcceptVisitor }

function TTestVisitor_AcceptVisitor.AcceptVisitor: boolean;
begin
  result := ( inherited AcceptVisitor ) ;
end;


{ TTestVisitor_Execute }

procedure TTestVisitor_Execute.Execute( const pVisited : TtiVisited ) ;
begin
  inherited Execute( pVisited ) ;
  ExecuteCalled := true ;
end;


{ TTestVisitorIterate }

constructor TTestVisitorIterate.Create;
begin
  inherited;
  FIndexes := TStringList.Create;
end;


destructor TTestVisitorIterate.Destroy;
begin
  FIndexes.Free;
  inherited;
end;


procedure TTestVisitorIterate.Execute(const pVisited: TtiVisited);
begin
  inherited Execute( pVisited );
  Assert( pVisited is TTestVisitedAbs, 'pVisited not a TTestVisitedAbs' ) ;
  FIndexes.Add( IntToStr(( pVisited as TTestVisitedAbs ).Index ));
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
  result := FList ;
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


{ TTestVisitorDepth }

procedure TTestVisitorDepth.Execute(const pVisited: TtiVisited);
begin
  Visited := pVisited ;
  Assert( pVisited is TTestVisitedAbs, 'pVisited not a TTestVisitedAbs' ) ;
  FIndexes.Values[ IntToStr(( pVisited as TTestVisitedAbs ).Index )] :=
    IntToStr( Depth ) ;
end;


{ TTestVisitorController }

function TTestVisitorController.VisitorControllerClass: TtiVisitorControllerClass;
begin
  result := TtiVisitorCtrlr ;
end;


{ TTestVisitorContinueVisiting }

procedure TTestVisitorContinueVisiting.Execute(const pVisited: TtiVisited);
begin
  inherited Execute( pVisited ) ;
  ContinueVisiting := ( pVisited as TTestVisitedAbs ).Index < 5 ;
end;


{ TTestVisitorGetAllToVisit }

function TTestVisitorGetAllToVisit.AcceptVisitor: boolean;
begin
  result := Visited is TTestVisitedOwned ;
end;


{ TTestVisStream }

procedure TTestVisStream.Execute(const pVisited: TtiVisited);
begin
  inherited;
  Assert( pVisited is TTestVisitedAbs, 'pVisited not a TTestVisitedAbs' ) ;
  if Stream.Size > 0 then
    Write( ',' ) ;
  Write( IntToStr( TTestVisitedAbs( Visited ).Index )) ;
end;


procedure TTestVisStream.Write(const psValue: string);
begin
  // A protected method, so this surfaces it as public for testing
  inherited;
end;


procedure TTestVisStream.WriteLn(const psValue: string);
begin
  // A protected method, so this surfaces it as public for testing
  inherited;
end;


procedure TTestTIVisitor.tiGetPropertyNames ;
var
  lsl : TStringList ;
  lObj : TtiVisited ;
begin
  lsl := TStringList.Create ;
  try
    lObj := TTestGetPropNames.Create ;
    try
      tiVisitor.tiGetPropertyNames( lObj, lsl, [ tkLString {$IFDEF FPC},tkAString{$ENDIF} ] ) ;
      CheckEquals( 3, lsl.Count, 'Failed on StringProp' ) ;
      CheckEquals( 'Caption',            lsl.Strings[0], 'Caption' ) ;
      CheckEquals( 'StringProp',         lsl.Strings[1], 'StringProp' ) ;
      CheckEquals( 'ReadOnlyStringProp', lsl.Strings[2], 'ReadOnlyStringProp' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, [ tkString] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on ShortStringProp' ) ;
      CheckEquals( 'ShortStringProp', lsl.Strings[0], 'Failed on ShortStringProp' ) ;
      CheckEquals( 'ReadOnlyShortStringProp', lsl.Strings[1], 'Failed on ReadOnlyShortStringProp' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, [ tkWString ] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on WideStringProp' ) ;
      CheckEquals( 'WideStringProp', lsl.Strings[0], 'Failed on WideStringProp' ) ;
      CheckEquals( 'ReadOnlyWideStringProp', lsl.Strings[1], 'Failed on ReadOnlyWideStringProp' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, [tkChar] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on CharProp' ) ;
      CheckEquals( 'CharProp', lsl.Strings[0], 'Failed on CharProp' ) ;
      CheckEquals( 'ReadOnlyCharProp', lsl.Strings[1], 'Failed on ReadOnlyCharProp' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, [tkWChar] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on WideCharProp' ) ;
      CheckEquals( 'WideCharProp', lsl.Strings[0], 'Failed on WideCharProp' ) ;
      CheckEquals( 'ReadOnlyWideCharProp', lsl.Strings[1], 'Failed on ReadOnlyWideCharProp' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, ctkString ) ;
      CheckEquals( 11, lsl.Count, 'Failed testing ctkString' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, [tkInteger] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on IntProp' ) ;
      CheckEquals( 'IntProp', lsl.Strings[0], 'Failed on IntProp' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, [tkInt64] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on Int64Prop' ) ;
      CheckEquals( 'Int64Prop', lsl.Strings[0], 'Failed on Int64Prop' ) ;

      { Delphi doesn't have this type defined! }
      {$IFDEF FPC}
      tiVisitor.tiGetPropertyNames( lObj, lsl, [tkBool] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on tkBool' ) ;
      CheckEquals( 'BoolProp',          lsl.Strings[0], 'Failed on BoolProp' ) ;
      CheckEquals( 'ReadOnlyBoolProp',  lsl.Strings[1], 'Failed on ReadOnlyBoolProp' ) ;
      {$ENDIF}

      tiVisitor.tiGetPropertyNames( lObj, lsl, ctkInt ) ;
      {$IFDEF FPC}
      CheckEquals( 6, lsl.Count, 'Failed testing ctkInt' ) ;
      {$ELSE}
      CheckEquals( 4, lsl.Count, 'Failed testing ctkInt' ) ;
      {$ENDIF}

      tiVisitor.tiGetPropertyNames( lObj, lsl, [tkFloat] ) ;
      CheckEquals( 4, lsl.Count, 'Failed on tkFloatProp' ) ;
      CheckEquals( 'DateTimeProp', lsl.Strings[0], 'Failed on tkFloatProp' ) ;
      CheckEquals( 'FloatProp',    lsl.Strings[1], 'Failed on tkFloatProp' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, ctkFloat ) ;
      CheckEquals( 4, lsl.Count, 'Failed testing ctkFloat' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, ctkNumeric ) ;
      CheckEquals( 8, lsl.Count, 'Failed testing ctkNumeric' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, ctkSimple ) ;
      {$IFDEF FPC}
      CheckEquals( 21, lsl.Count, 'Failed testing ctkSimple' ) ;
      {$ELSE}
      CheckEquals( 19, lsl.Count, 'Failed testing ctkSimple' ) ;
      {$ENDIF}

      tiVisitor.tiGetPropertyNames( lObj, lsl, [tkClass] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on ObjectProp' ) ;
      CheckEquals( 'ObjectProp', lsl.Strings[0], 'Failed on ObjectProp' ) ;

      tiVisitor.tiGetPropertyNames( lObj, lsl, [tkMethod] ) ;
      CheckEquals( 2, lsl.Count, 'Failed on MethodProp' ) ;
      CheckEquals( 'MethodProp', lsl.Strings[0], 'Failed on MethodProp' ) ;
    finally
      lObj.Free;
    end;
  finally
    lsl.Free;
  end;
end;


procedure TTestTIVisitor.tiGetSimplePropType ;
var
  lObj : TTestGetPropNames ;
begin
  lObj := TTestGetPropNames.Create ;
  try

    Check( tiVisitor.tiGetSimplePropType( lObj, 'StringProp' )      = tiTKString, 'Failed on StringProp' ) ;
    Check( tiVisitor.tiGetSimplePropType( lObj, 'ShortStringProp' ) = tiTKString, 'Failed on ShortStringProp' ) ;
    Check( tiVisitor.tiGetSimplePropType( lObj, 'WideStringProp' )  = tiTKString, 'Failed on WideStringProp' ) ;
    Check( tiVisitor.tiGetSimplePropType( lObj, 'CharProp' )        = tiTKString, 'Failed on CharProp' ) ;
    Check( tiVisitor.tiGetSimplePropType( lObj, 'WideCharProp' )    = tiTKString, 'Failed on WideCharProp' ) ;

    Check( tiVisitor.tiGetSimplePropType( lObj, 'IntProp' )         = tiTKInteger, 'Failed on IntProp' ) ;
    Check( tiVisitor.tiGetSimplePropType( lObj, 'Int64Prop' )       = tiTKInteger, 'Failed on Int64Prop' ) ;
    Check( tiVisitor.tiGetSimplePropType( lObj, 'BoolProp' )        = tiTKBoolean, 'Failed on BoolProp' ) ;

    Check( tiVisitor.tiGetSimplePropType( lObj, 'FloatProp' )       = tiTKFloat, 'Failed on FloatProp' ) ;
    Check( tiVisitor.tiGetSimplePropType( lObj, 'DateTimeProp' )    = tiTKDateTime, 'Failed on DateTimeProp' ) ;

    try
      tiVisitor.tiGetSimplePropType( lObj, 'ObjectProp' ) ;
      Check( false, 'Failed on ObjectProp' ) ;
    except
      on e:exception do
        CheckIs( e, Exception, 'Failed on ObjectProp' ) ;
    end ;

    try
      tiVisitor.tiGetSimplePropType( lObj, 'MethodProp' ) ;
      Check( false, 'Failed on MethodProp' ) ;
    except
      on e:exception do
        CheckIs( e, Exception, 'Failed on MethodProp' ) ;
    end ;
  finally
    lObj.Free;
  end;
end;


procedure TTestTIVisitor.tiVarSimplePropType;
begin
  Check( tiVisitor.tiVarSimplePropType( 'string' ) = tiTKString,   'Failed on tiTKString'   ) ;
  Check( tiVisitor.tiVarSimplePropType( 123      ) = tiTKInteger,  'Failed on tiTKInteger'  ) ;
  Check( tiVisitor.tiVarSimplePropType( true     ) = tiTKBoolean,  'Failed on tiTKBoolean'  ) ;
  Check( tiVisitor.tiVarSimplePropType( 123.456  ) = tiTKFloat,    'Failed on tiTKFloat'    ) ;
  Check( tiVisitor.tiVarSimplePropType( Now      ) = tiTKDateTime, 'Failed on tiTKDateTime' ) ;
end;


procedure TTestTIVisitor.tiIsNumericProp ;
var
  lObj : TTestGetPropNames ;
begin
  lObj := TTestGetPropNames.Create ;
  try
    Check( not tiVisitor.tiIsNumericProp( lObj, 'StringProp'      ), 'Failed on StringProp' ) ;
    Check( not tiVisitor.tiIsNumericProp( lObj, 'ShortStringProp' ), 'Failed on ShortStringProp' ) ;
    Check( not tiVisitor.tiIsNumericProp( lObj, 'WideStringProp'  ), 'Failed on WideStringProp' ) ;
    Check( not tiVisitor.tiIsNumericProp( lObj, 'CharProp'        ), 'Failed on CharProp' ) ;
    Check( not tiVisitor.tiIsNumericProp( lObj, 'WideCharProp'    ), 'Failed on WideCharProp' ) ;
    Check(     tiVisitor.tiIsNumericProp( lObj, 'IntProp'         ), 'Failed on IntProp' ) ;
    Check(     tiVisitor.tiIsNumericProp( lObj, 'Int64Prop'       ), 'Failed on Int64Prop' ) ;
    Check(     tiVisitor.tiIsNumericProp( lObj, 'DateTimeProp'    ), 'Failed on DateTimeProp' ) ;
    Check(     tiVisitor.tiIsNumericProp( lObj, 'FloatProp'       ), 'Failed on FloatProp' ) ;
    Check( not tiVisitor.tiIsNumericProp( lObj, 'ObjectProp'      ), 'Failed on ObjectProp' ) ;
    Check( not tiVisitor.tiIsNumericProp( lObj, 'MethodProp'      ), 'Failed on MethodProp' ) ;
  finally
    lObj.Free ;
  end ;
end ;


procedure TTestTIVisitor.tiIsReadWriteProp ;
var
  lObj : TTestGetPropNames ;
begin
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyStringProp' ),      'Failed on ReadOnlyStringProp'       ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyShortStringProp' ), 'Failed on ReadOnlyShortStringProp'  ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyWideStringProp' ),  'Failed on ReadOnlyWideStringProp '  ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyCharProp' ),        'Failed on ReadOnlyCharProp'         ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyWideCharProp' ),    'Failed on ReadOnlyWideCharProp'     ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyIntProp' ),         'Failed on ReadOnlyIntProp'          ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyInt64Prop' ),       'Failed on ReadOnlyInt64Prop'        ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyDateTimeProp' ),    'Failed on ReadOnlyDateTimeProp'     ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyFloatProp' ),       'Failed on ReadOnlyFloatProp'        ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyObjectProp' ),      'Failed on ReadOnlyObjectProp'       ) ;
  Check( not tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ReadOnlyMethodProp' ),      'Failed on ReadOnlyMethodProp'       ) ;

  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'StringProp' ),  'Failed on OnlyStringProp'    ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ShortStringProp' ), 'Failed on ShortStringProp'   ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'WideStringProp' ),  'Failed on WideStringProp '   ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'CharProp' ),        'Failed on CharProp'          ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'WideCharProp' ),    'Failed on WideCharProp'      ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'IntProp' ),         'Failed on IntProp'           ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'Int64Prop' ),       'Failed on Int64Prop'         ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'DateTimeProp' ),    'Failed on DateTimeProp'      ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'FloatProp' ),       'Failed on FloatProp'         ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'ObjectProp' ),      'Failed on ObjectProp'        ) ;
  Check( tiVisitor.tiIsReadWriteProp( TTestGetPropNames, 'MethodProp' ),      'Failed on MethodProp'        ) ;

  lObj := TTestGetPropNames.Create ;
  try
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyStringProp' ),      'Failed on ReadOnlyStringProp'       ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyShortStringProp' ), 'Failed on ReadOnlyShortStringProp'  ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyWideStringProp' ),  'Failed on ReadOnlyWideStringProp '  ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyCharProp' ),        'Failed on ReadOnlyCharProp'         ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyWideCharProp' ),    'Failed on ReadOnlyWideCharProp'     ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyIntProp' ),         'Failed on ReadOnlyIntProp'          ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyInt64Prop' ),       'Failed on ReadOnlyInt64Prop'        ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyBoolProp' ),        'Failed on ReadOnlyBoolProp'         ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyDateTimeProp' ),    'Failed on ReadOnlyDateTimeProp'     ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyFloatProp' ),       'Failed on ReadOnlyFloatProp'        ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyObjectProp' ),      'Failed on ReadOnlyObjectProp'       ) ;
    Check( not tiVisitor.tiIsReadWriteProp( lObj, 'ReadOnlyMethodProp' ),      'Failed on ReadOnlyMethodProp'       ) ;

    Check( tiVisitor.tiIsReadWriteProp( lObj, 'StringProp' ),  'Failed on OnlyStringProp'    ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'ShortStringProp' ), 'Failed on ShortStringProp'   ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'WideStringProp' ),  'Failed on WideStringProp '   ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'CharProp' ),        'Failed on CharProp'          ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'WideCharProp' ),    'Failed on WideCharProp'      ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'IntProp' ),         'Failed on IntProp'           ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'Int64Prop' ),       'Failed on Int64Prop'         ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'BoolProp' ),        'Failed on BoolProp'          ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'DateTimeProp' ),    'Failed on DateTimeProp'      ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'FloatProp' ),       'Failed on FloatProp'         ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'ObjectProp' ),      'Failed on ObjectProp'        ) ;
    Check( tiVisitor.tiIsReadWriteProp( lObj, 'MethodProp' ),      'Failed on MethodProp'        ) ;
  finally
    lObj.Free;
  end;
end;


end.

