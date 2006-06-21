
{$I tiDefines.inc}

unit TestPerObjThreadList;

interface
uses
  tiPtnVisPerObj
  ,Classes
  ;

type

  TTestPerObjThreadList = class ;
  TTestObj = class ;

  //----------------------------------------------------------------------------
  TTestPerObjThreadList = class( TPerObjThreadList )
  private
  protected
    function    GetItems(i: integer): TTestObj ; reintroduce ;
    procedure   SetItems(i: integer; const Value: TTestObj); reintroduce ;
  public
    property    Items[i:integer] : TTestObj read GetItems write SetItems ;
    procedure   Add( pObject : TTestObj   ; pDefDispOrdr : boolean = true ) ; reintroduce ;
  end ;

  //----------------------------------------------------------------------------
  TTestObj = class( TPerObjAbs )
  private
    FIntProp: integer;
    FThreadID: integer;
  protected
    function    GetOwner: TTestPerObjThreadList; reintroduce ;
    procedure   SetOwner(const Value: TTestPerObjThreadList); reintroduce ;
  public
    property    Owner : TTestPerObjThreadList read GetOwner write SetOwner ;
  published
    property IntProp : integer read FIntProp write FIntProp ;
    property ThreadID : integer read FThreadID write FThreadID ;
  end ;

  //----------------------------------------------------------------------------
  TTestThread = class( TThread )
  private
    FTestPerObjThreadList: TTestPerObjThreadList;
  public
    constructor CreateExt ;
    procedure Execute ; override ;
    property  TestPerObjThreadList : TTestPerObjThreadList read FTestPerObjThreadList write FTestPerObjThreadList ;
  end ;

procedure ExecuteMultiThreadTest( pThrdCount : integer ) ;

implementation
uses
  tiPtnVisPerObj_Cli
  ,Windows
  ,tiLog
  ;

function _CreateThread( pData : TTestPerObjThreadList ) : TTestThread ;
begin
  result := TTestThread.CreateExt ;
  result.TestPerObjThreadList := pData ;
  Sleep( 250 ) ;
  result.Resume ;
end ;

procedure ExecuteMultiThreadTest( pThrdCount : integer ) ;
var
  lData : TTestPerObjThreadList ;
  lThrd : TTestThread ;
  i : integer ;
begin
  lThrd := nil ;
  lData := TTestPerObjThreadList.Create ;
  try
    for i := 1 to pThrdCount do
      lThrd := _CreateThread( lData ) ;
    lThrd.WaitFor ;
    tiShowPerObjAbs( lData ) ;
  finally
    lData.Free;
  end ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TTestPerObjThreadList
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
procedure TTestPerObjThreadList.Add(pObject: TTestObj; pDefDispOrdr: boolean);
begin
  inherited Add( pObject, pDefDispOrdr ) ;
end;

function TTestPerObjThreadList.GetItems(i: integer): TTestObj;
begin
  result := TTestObj( inherited GetItems( i )) ;
end;

procedure TTestPerObjThreadList.SetItems(i: integer; const Value: TTestObj);
begin
  inherited SetItems( i, Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TTestObj
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
function TTestObj.GetOwner: TTestPerObjThreadList;
begin
  result := TTestPerObjThreadList( inherited GetOwner ) ;
end;

procedure TTestObj.SetOwner(const Value: TTestPerObjThreadList);
begin
  inherited SetOwner( Value ) ;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TTestThread
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
constructor TTestThread.CreateExt;
begin
  Create( true ) ;
  FreeOnTerminate := true ;
end;

procedure TTestThread.Execute;
var
  i : integer ;
  lData : TTestObj ;
begin
  // Add some more data
  // The object is not locked when this data is being added because the
  // Add( ) method contains locking code.
  for i := 0 to 99 do
  begin
    lData := TTestObj.Create ;
    lData.IntProp := i ;
    lData.ThreadID := GetCurrentThreadID ;
    lData.ObjectState := posClean ;
    FTestPerObjThreadList.Add( lData ) ;
  end ;

  // Read the data back
  // The object is locked before it is read.
  FTestPerObjThreadList.Lock ;
  try
    for i := 0 to FTestPerObjThreadList.Count do
      LogArray([ FTestPerObjThreadList.Items[i].ThreadID,
                 FTestPerObjThreadList.Items[i].IntProp ]) ;
  finally
    FTestPerObjThreadList.UnLock ;
  end ;

end;

end.
