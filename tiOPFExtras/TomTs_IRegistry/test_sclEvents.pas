unit test_sclEvents;

interface

uses
  Sysutils,
  TestFramework,
  haClasses, haSDK;

type
  TTest_TEventListenerList = class(TTestCase)
  private
    procedure AddDummyEventListeners(EventListenerList: IEventListenerList;
      Count: Integer);
    procedure VisitDummyEventListener(Listener: IEventListener);  
  published
    procedure Test_Add;
    procedure Test_CreateAndDestroy;
    procedure Test_Clear;
    procedure Test_Delete;
    procedure Test_EnumListeners;
  end;

implementation

type
  IEnumTestEventListener = interface(IEventListener)
    ['{DCCF722B-9634-467B-AE8E-B4C70683AE74}']
    function GetVisited: Boolean;
    procedure SetVisited(const Value: Boolean);
    property Visited: Boolean read GetVisited write SetVisited;
  end;

  TDummyEventListener = class(TInterfacedObject, IEventListener, IEnumTestEventListener)
  private
    FVisited: Boolean;
    function GetVisited: Boolean;
    procedure SetVisited(const Value: Boolean);
  published
    property Visited: Boolean read GetVisited write SetVisited;
  end;

const
  IID_IEnumTestEventListener: TGUID = '{DCCF722B-9634-467B-AE8E-B4C70683AE74}';

{ TTest_TEventListenerList }

procedure TTest_TEventListenerList.AddDummyEventListeners(
  EventListenerList: IEventListenerList; Count: Integer);
var
  Counter: Integer;
  DummyEventListener: IEventListener;
begin
  for Counter := 1 to Count do
  begin
    DummyEventListener := TDummyEventListener.Create;
    EventListenerList.Add(DummyEventListener);
  end;
end;

procedure TTest_TEventListenerList.Test_Add;
var
  EventListenerList: IEventListenerList;

  procedure TestAddNewEventListener(ExpectedCount, ExpectedIndex: Integer);
  var
    DummyEventListener: IEventListener;
    DummyEventListenerIndex: Integer;
  begin
    DummyEventListener := TDummyEventListener.Create;
    DummyEventListenerIndex := EventListenerList.Add(DummyEventListener);
    Check(DummyEventListenerIndex = ExpectedIndex,
      '.Add returned an unexpected value');
    Check(EventListenerList.Count = ExpectedCount,
      '.Count returned an unexpected value');
    Check(EventListenerList.Items[ExpectedIndex] = DummyEventListener,
      'EventListenerList.Items[x] is not expected value');
  end;

var
  Counter: Integer;
begin
  EventListenerList := TEventListenerList.Create;

  for Counter := 0 to 100 do
    TestAddNewEventListener(Counter+1, Counter);
end;

procedure TTest_TEventListenerList.Test_Clear;
var
  Counter: Integer;
  DummyEventListener: IEventListener;
  DummyEventListenerIndex: Integer;
  EventListenerList: IEventListenerList;
begin
  EventListenerList := TEventListenerList.Create;
  for Counter := 1 to 100 do
  begin
    DummyEventListener := TDummyEventListener.Create;
    EventListenerList.Add(DummyEventListener);
  end;
  Check(EventListenerList.Count = 100, 'EventListenerList failed to add all listeners');
  EventListenerList.Clear;
  Check(EventListenerList.Count = 0, 'EventListenerList failed to clear all listeners');

end;

procedure TTest_TEventListenerList.Test_CreateAndDestroy;
var
  EventListenerListAsClass: TEventListenerList;
  EventListenerListAsInterface: IEventListenerList;
begin
  try
    EventListenerListAsClass := TEventListenerList.Create;
  except
    Check(FALSE, 'EventListenerListAsClass failed on .Create');
  end;
  try
    EventListenerListAsClass.Free;
  except
    Check(FALSE, 'EventListenerListAsClass failed on .Free');
  end;

  try
    EventListenerListAsInterface := TEventListenerList.Create;
  except
    Check(FALSE, 'EventListenerListAsInterface failed on .Create');
  end;
  try
    EventListenerListAsInterface := nil;
  except
    Check(FALSE, 'EventListenerListAsInterface failed on setting to nil');
  end;
end;

procedure TTest_TEventListenerList.Test_Delete;
var
  EventListenerList: IEventListenerList;



var
  Counter: Integer;
begin
  EventListenerList := TEventListenerList.Create;

  // Add and delete single plugin
  AddDummyEventListeners(EventListenerList, 1);
  Check(EventListenerList.Count = 1, '.Count is not expected value');
  EventListenerList.Delete(0);
  Check(EventListenerList.Count = 0, '.Count is not expected value');

  // Add multiple plugins, delete by moving forwards through item list
  AddDummyEventListeners(EventListenerList, 5);
  Check(EventListenerList.Count = 5, '.Count is not expected value');
  for Counter := 0 to 4 do
  begin
    EventListenerList.Delete(0);
    Check(EventListenerList.Count = 4 - Counter, '.Count is not expected value');
  end;
  Check(EventListenerList.Count = 0, '.Count is not expected value');

  // Add multiple plugins, delete by moving backwards through item list
  AddDummyEventListeners(EventListenerList, 5);
  Check(EventListenerList.Count = 5, '.Count is not expected value');
  for Counter := 4 downto 0 do
  begin
    EventListenerList.Delete(Counter);
    Check(EventListenerList.Count = Counter, '.Count is not expected value');
  end;
  Check(EventListenerList.Count = 0, '.Count is not expected value');
end;

procedure TTest_TEventListenerList.Test_EnumListeners;
var
  Counter: Integer;
  EnumTestEventListener: IEnumTestEventListener;
  EventListenerList: IEventListenerList;
begin
  EventListenerList := TEventListenerList.Create;
  AddDummyEventListeners(EventListenerList, 5);
  for Counter := 0 to EventListenerList.Count -1 do
  begin
    if Supports(EventListenerList.Items[Counter], IID_IEnumTestEventListener,
      EnumTestEventListener) then
    begin
      EnumTestEventListener.Visited := False;
    end;
  end;

  EventListenerList.EnumListeners(VisitDummyEventListener);

  for Counter := 0 to EventListenerList.Count -1 do
  begin
    if Supports(EventListenerList.Items[Counter], IID_IEnumTestEventListener,
      EnumTestEventListener) then
    begin
      Check(EnumTestEventListener.Visited = True,
        'Not all listeners were visited in the enumeration of listeners');
    end;
  end;

end;

procedure TTest_TEventListenerList.VisitDummyEventListener(
  Listener: IEventListener);
var
  EnumTestEventListener: IEnumTestEventListener;
begin
  if Supports(Listener, IID_IEnumTestEventListener, EnumTestEventListener) then
    EnumTestEventListener.Visited := True;
end;

{ TDummyEventListener }

function TDummyEventListener.GetVisited: Boolean;
begin
  Result := FVisited;
end;

procedure TDummyEventListener.SetVisited(const Value: Boolean);
begin
  FVisited := Value;
end;

initialization
  RegisterTest('SCL.Events', TTest_TEventListenerList);

end.

