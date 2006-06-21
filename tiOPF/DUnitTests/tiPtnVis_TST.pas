{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

  Revision history:
    June, 2002, Peter Hinrichsen, Created
  Purpose:

  ToDo:

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiPtnVis_TST;

{$I tiDefines.inc}

interface
uses
  Classes  // needed for TStringList
  ,TestFrameWork
  ,tiPtnVis
  ,Contnrs
  ;

type
  TTestVisitedList = class ;
  TTestVisitedOwned = class ;

  TTestTiPtnVis = class( TTestCase )
  private
    function CreateListAndOwned : TTestVisitedList ;
    function CreateList : TTestVisitedList ;
    function CreateOwned : TTestVisitedOwned ;
  protected
    procedure Setup ; override ;
  published

    // Test the visitor
    procedure VisitorAbs_AcceptVisitor;
    procedure VisitorAbs_DoAcceptVisitor;
    procedure VisitorAbs_Execute;
    procedure VisitorAbs_ContinueVisiting;
    procedure VisitorAbs_VisitorController;
    procedure VisitorAbs_Depth;

    // Test the visited
    procedure VisitedAbs_Caption;

    procedure VisitedAbs_IterateSingle;
    procedure VisitedAbs_IterateList;
    procedure VisitedAbs_IterateOwned;
    procedure VisitedAbs_IterateListAndOwned;
    procedure VisitedAbs_IterateBottomUpSingle;
    procedure VisitedAbs_IterateBottomUpList;
    procedure VisitedAbs_IterateBottomUpOwned;
    procedure VisitedAbs_IterateBottupUpListAndOwned;

    procedure VisitedAbs_SelfIterate;
    procedure VisitedAbs_FindAllByClassType;
    procedure VisitedAbs_ClassCount ;
    procedure VisitedAbs_PropCount ;

    // Test some other special visitors
    procedure VisGetAllToVisit_Execute;
    procedure VisClassCount_Execute;
    procedure VisFindAllByClass_Execute;

    // Test the special stream writing visitor
    procedure VisStream;

    // Test the TFileStream visitor wrapper
    procedure VisStreamToFile;

  end ;

  TTestVisitorAbs_AcceptVisitor = class( TVisitorAbs )
  public
    function AcceptVisitor : boolean ; override ;
  end ;

  TTestVisitorAbs_DoAcceptVisitor = class( TVisitorAbs )
  public
    function AcceptVisitor : boolean ; override ;
  end ;

  TTestVisitorAbs_Execute = class( TVisitorAbs )
  private
    FExecuteCalled: boolean;
  public
    property  ExecuteCalled : boolean read FExecuteCalled write FExecuteCalled ;
    procedure Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

  TTestVisitedAbs = class( TVisitedAbs )
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
    constructor create ; override ;
    destructor  destroy ; override ;
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
    constructor create ; override ;
    destructor  destroy ; override ;
  end ;

  TTestVisitorIterate = class( TVisitorAbs )
  private
    FIndexes: TStringList;
  public
    constructor Create ; override ;
    destructor  Destroy ; override ;
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
    property    Indexes : TStringList read FIndexes ;
  end ;

  TTestVisitorContinueVisiting = class( TTestVisitorIterate )
  public
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

  TTestVisitorDepth = class( TTestVisitorIterate )
  public
    procedure   Execute( const pVisited : TVisitedAbs ) ; override ;
  end ;

  TTestVisitorController = class( TVisitorAbs )
  public
    function VisitorControllerClass : TVisitorControllerClass ; override ;
  end ;

  TTestVisitorGetAllToVisit = class( TVisitorAbs )
  protected
    function AcceptVisitor : boolean ; override ;
  end;

  TTestVisStream = class( TVisStream )
  public
    procedure Execute( const pVisited : TVisitedAbs ) ; override ;
    procedure Write( const psValue : string ) ; override ;
    procedure WriteLn( const psValue : string = '') ; override ;
  end ;


const
  cTestDoAcceptVisitorException = 'Test DoAcceptVisitor exception.' ;

procedure RegisterTests ;

implementation
uses
  SysUtils
  ,tiDialogs
  ,tiUtils
  ,tiPersist
  ,TypInfo
  ,tiDBConnectionSetupAbs_TST
  ,tiDUnitDependencies
  ;

procedure RegisterTests ;
begin
  if gPerFrameworkSetupFactory.TestNonPersistentClasses then
    RegisterTest( TTestTiPtnVis.Suite );
end ;

{ TTestTiPtnVis }

procedure TTestTiPtnVis.VisClassCount_Execute;
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
        CheckEquals( 0, lVis.ClassCount[TVisitedAbs],        'Failed TVisitedAbs' );
      finally
        lVis.Free ;
      end ;
  finally
    lVisited.Free;
  end;
end;

procedure TTestTiPtnVis.VisFindAllByClass_Execute;
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
        lVis.ClassTypeToFind := TVisitedAbs ;
        lVisited.Iterate( lVis ) ;
        CheckEquals( 13, lList.Count, 'Failed TVisitedAbs' );

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

procedure TTestTiPtnVis.VisGetAllToVisit_Execute;
var
  lVisGetAllToVisit : TVisGetAllToVisit ;
  lVisitor : TTestVisitorGetAllToVisit ;
  lVisitedList  : TTestVisitedList ;
begin
  lVisitedList := CreateListAndOwned ;
  try
    lVisitor := TTestVisitorGetAllToVisit.Create ;
    try
      lVisGetAllToVisit := TVisGetAllToVisit.Create ;
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

procedure TTestTiPtnVis.VisitedAbs_Caption;
var
  lVisited : TVisitedAbs ;
begin
  lVisited := TVisitedAbs.Create ;
  try
    CheckEquals( lVisited.Caption, lVisited.ClassName ) ;
  finally
    lVisited.Free ;
  end ;
end;

procedure TTestTiPtnVis.VisitedAbs_FindAllByClassType;
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

      lVisitedList.FindAllByClassType( TVisitedAbs,  lList ) ;
      CheckEquals( 13, lList.Count, 'Failed on FindAllByClassType( TVisitorAbs )' );

    finally
      lList.Free ;
    end ;
  finally
    lVisitedList.Free;
  end;
end;

procedure TTestTiPtnVis.VisitedAbs_IterateBottomUpSingle;
begin
  VisitedAbs_IterateSingle;
end;

procedure TTestTiPtnVis.VisitedAbs_SelfIterate;
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

procedure TTestTiPtnVis.VisitorAbs_AcceptVisitor;
var
  lVis : TTestVisitorAbs_AcceptVisitor ;
begin
  lVis := TTestVisitorAbs_AcceptVisitor.Create ;
  try
    Check( lVis.AcceptVisitor ) ;
  finally
    lVis.Free ;
  end ;
end;

procedure TTestTiPtnVis.VisitorAbs_ContinueVisiting;
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

procedure TTestTiPtnVis.VisitorAbs_Depth;
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

procedure TTestTiPtnVis.VisitorAbs_DoAcceptVisitor;
var
  lVis : TTestVisitorAbs_DoAcceptVisitor ;
begin
  lVis := TTestVisitorAbs_DoAcceptVisitor.Create ;
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

procedure TTestTiPtnVis.VisitorAbs_Execute;
var
  lVisitor : TTestVisitorAbs_Execute ;
  lVisited : TVisitedAbs ;
begin
  lVisitor := TTestVisitorAbs_Execute.Create ;
  try
    lVisited := TVisitedAbs.Create ;
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

procedure TTestTiPtnVis.VisitorAbs_VisitorController;
var
  lVis : TVisitorAbs ;
begin
  lVis := TTestVisitorController.Create ;
  try
    CheckEquals( lVis.VisitorControllerClass, TVisitorCtrlr ) ;
    CheckNull( lVis.VisitorController ) ;
    lVis.VisitorController := lVis.VisitorControllerClass.Create ;
    CheckNotNull( lVis.VisitorController ) ;
    CheckIs( lVis.VisitorController, TVisitorCtrlr ) ;
  finally
    lVis.Free ;
  end ;
end;

{
  TVisStream = class( TVisitorAbs )
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

procedure TTestTiPtnVis.VisStream;
var
  lVis : TTestVisStream ;
  lStream : TStringStream ;
  lVisitedList  : TTestVisitedList ;
const
  cATestLine = 'A test line' ;
begin
  lVis := TTestVisStream.Create ;
  try
    lStream := TStringStream.Create( '' ) ;
    try
      lVis.Stream := lStream ;
      lVis.Write( cATestLine ) ;
      CheckEquals( cATestLine, lStream.DataString, 'Failed on Write' ) ;

      lStream.Size := 0 ;
      lVis.WriteLn ;
      CheckEquals( #13 + #10, lStream.DataString, 'Failed on WriteLn' ) ;

      lStream.Size := 0 ;
      lVis.WriteLn( cATestLine ) ;
      CheckEquals( cATestLine + #13 + #10, lStream.DataString, 'Failed on WriteLn( cATestLine )' ) ;

      lStream.Size := 0 ;
      lVisitedList := CreateListAndOwned ;
      try
        lVisitedList.Iterate( lVis ) ;
        CheckEquals( '1,2,3,4,5,6,7,8,9,10,11,12,13', lStream.DataString )
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

procedure TTestTiPtnVis.VisStreamToFile;
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
  lVisitedList  : TTestVisitedList ;
  lFileName : TFileName ;
  lFileStream : TFileStream ;
begin
  lFileName := tiGetTempFile( 'txt' ) ;
  lVisitedList := CreateListAndOwned ;
  try
    tiPtnVis.VisStreamToFile( lVisitedList,
                              lFileName,
                              TTestVisStream );
    _CheckFileEquals( lFileName, '1,2,3,4,5,6,7,8,9,10,11,12,13' ) ;

    lFileStream :=
      TFileStream.Create( lFileName,
                          fmCreate or fmShareDenyNone ) ;
    try
      tiPtnVis.VisStreamToFile( lVisitedList,
                                lFileName,
                                TTestVisStream,
                                lFileStream );
    finally
      lFileStream.Free ;
    end ;
    _CheckFileEquals( lFileName, '1,2,3,4,5,6,7,8,9,10,11,12,13' ) ;

    lFileStream :=
      TFileStream.Create( lFileName,
                          fmOpenReadWrite or fmShareDenyNone ) ;
    try
      lFileStream.Seek( 0, soFromEnd ) ;
      tiPtnVis.VisStreamToFile( lVisitedList,
                                lFileName,
                                TTestVisStream,
                                lFileStream );
    finally
      lFileStream.Free ;
    end ;
    _CheckFileEquals( lFileName,
                      '1,2,3,4,5,6,7,8,9,10,11,12,13' +
                      ',1,2,3,4,5,6,7,8,9,10,11,12,13' ) ;

  finally
    lVisitedList.Free ;
  end ;


{
var
  lVis : TTestVisStream ;
  lStream : TStringStream ;
  lVisitedList  : TTestVisitedList ;
const
  cATestLine = 'A test line' ;
begin
  lVis := TTestVisStream.Create ;
  try
    lStream := TStringStream.Create( '' ) ;
    try
      lVis.Stream := lStream ;
      lVis.Write( cATestLine ) ;
      CheckEquals( cATestLine, lStream.DataString, 'Failed on Write' ) ;

      lStream.Size := 0 ;
      lVis.WriteLn ;
      CheckEquals( #13 + #10, lStream.DataString, 'Failed on WriteLn' ) ;

      lStream.Size := 0 ;
      lVis.WriteLn( cATestLine ) ;
      CheckEquals( cATestLine + #13 + #10, lStream.DataString, 'Failed on WriteLn( cATestLine )' ) ;

      lStream.Size := 0 ;
      lVisitedList := CreateListAndOwned ;
      try
        lVisitedList.Iterate( lVis ) ;
        CheckEquals( '1,2,3,4,5,6,7,8,9,10,11,12,13', lStream.DataString )
      finally
        lVisitedList.Free;
      end;
    finally
      lStream.Free ;
    end ;
  finally
    lVis.Free ;
  end;
}

end;

procedure TTestTiPtnVis.VisitedAbs_IterateList;
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

procedure TTestTiPtnVis.VisitedAbs_IterateOwned;
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

procedure TTestTiPtnVis.VisitedAbs_IterateSingle;
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

procedure TTestTiPtnVis.VisitedAbs_IterateBottomUpList;
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

procedure TTestTiPtnVis.VisitedAbs_IterateBottomUpOwned;
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

procedure TTestTiPtnVis.VisitedAbs_IterateListAndOwned;
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

procedure TTestTiPtnVis.VisitedAbs_IterateBottupUpListAndOwned;
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

function TTestTiPtnVis.CreateListAndOwned: TTestVisitedList;
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

function TTestTiPtnVis.CreateList: TTestVisitedList;
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

function TTestTiPtnVis.CreateOwned: TTestVisitedOwned;
begin
  result := TTestVisitedOwned.Create ;
  result.Index := 1 ;
  result.Owned1.Index := 2 ;
  result.Owned2.Index := 3 ;
  result.Owned3.Index := 4 ;
end;

procedure TTestTiPtnVis.VisitedAbs_ClassCount;
var
  lVisited  : TTestVisitedList ;
begin
  lVisited := CreateListAndOwned ;
  try
    CheckEquals( 1,  lVisited.CountByClass(TTestVisitedList),  'Failed TTestVisitedList' );
    CheckEquals( 3,  lVisited.CountByClass(TTestVisitedOwned), 'Failed TTestVisitedOwned' );
    CheckEquals( 9,  lVisited.CountByClass(TTestVisited),       'Failed TTestVisited' );
    CheckEquals( 13, lVisited.CountByClass(TVisitedAbs),        'Failed TVisitedAbs' );
  finally
    lVisited.Free;
  end;
end;

procedure TTestTiPtnVis.Setup;
begin
  gTIPerMgr.Terminated := false ;
end;

procedure TTestTiPtnVis.VisitedAbs_PropCount;
var
  lData : TVisitedAbs ;
begin

  // The extra simple property is Caption
  lData :=  TTestVisitedAbs.Create ;
  try
    CheckEquals( 0, lData.PropCount( [tkClass] ), 'Failed on TTestVisitedList/tkClass' ) ;
    CheckEquals( 1, lData.PropCount( ctkSimple ), 'Failed on TTestVisitedList/ctkSimple' ) ;
  finally
    lData.Free ;
  end ;

  lData :=  TTestVisitedList.Create ;
  try
    CheckEquals( 1, lData.PropCount( [tkClass] ), 'Failed on TTestVisitedList/tkClass' ) ;
    CheckEquals( 1, lData.PropCount( ctkSimple ), 'Failed on TTestVisitedList/ctkSimple' ) ;
  finally
    lData.Free ;
  end ;

  lData :=  TTestVisitedOwned.Create ;
  try
    CheckEquals( 3, lData.PropCount( [tkClass] ), 'Failed on TTestVisitedOwned/tkClass' ) ;
    CheckEquals( 1, lData.PropCount( ctkSimple ), 'Failed on TTestVisitedOwned/ctkSimple' ) ;
  finally
    lData.Free ;
  end ;

end;

{ TTestVisitorAbs_DoAcceptVisitor }
function TTestVisitorAbs_DoAcceptVisitor.AcceptVisitor: boolean;
begin
  raise exception.create( cTestDoAcceptVisitorException ) ;
end;

{ TTestVisitorAbs_AcceptVisitor }

function TTestVisitorAbs_AcceptVisitor.AcceptVisitor: boolean;
begin
  result := ( inherited AcceptVisitor ) ;
end;

{ TTestVisitorAbs_Execute }

procedure TTestVisitorAbs_Execute.Execute( const pVisited : TVisitedAbs ) ;
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

procedure TTestVisitorIterate.Execute(const pVisited: TVisitedAbs);
begin
  inherited Execute( pVisited );
  Assert( pVisited is TTestVisitedAbs, 'pVisited not a TTestVisitedAbs' ) ;
  FIndexes.Add( IntToStr(( pVisited as TTestVisitedAbs ).Index ));
end;

{ TTestVisitedList }

constructor TTestVisitedList.create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TTestVisitedList.destroy;
begin
  FList.Free;
  inherited;
end;

function TTestVisitedList.GetList: TList;
begin
  result := FList ;
end;

{ TTestVisitedOwned }

constructor TTestVisitedOwned.create;
begin
  inherited;
  FOwned1 := TTestVisited.Create;
  FOwned3 := TTestVisited.Create;
  FOwned2 := TTestVisited.Create;
end;

destructor TTestVisitedOwned.destroy;
begin
  FOwned1.Free;
  FOwned3.Free;
  FOwned2.Free;
  inherited;
end;

{ TTestVisitorDepth }

procedure TTestVisitorDepth.Execute(const pVisited: TVisitedAbs);
begin
  Visited := pVisited ;
  Assert( pVisited is TTestVisitedAbs, 'pVisited not a TTestVisitedAbs' ) ;
  FIndexes.Values[ IntToStr(( pVisited as TTestVisitedAbs ).Index )] :=
    IntToStr( Depth ) ;
end;

{ TTestVisitorController }


function TTestVisitorController.VisitorControllerClass: TVisitorControllerClass;
begin
  result := TVisitorCtrlr ;
end;

{ TTestVisitorContinueVisiting }

procedure TTestVisitorContinueVisiting.Execute(const pVisited: TVisitedAbs);
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

procedure TTestVisStream.Execute(const pVisited: TVisitedAbs);
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

end.
