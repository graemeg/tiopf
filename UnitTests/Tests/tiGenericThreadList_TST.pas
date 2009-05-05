unit tiGenericThreadList_TST;

{$I tiDefines.inc}

interface
uses
  Classes  // needed for TStringList
  ,tiOPFTestCase
  ,tiTestFramework
  ,tiObject
  ,tiBOMsForTesting
  ,tiGenericThreadList
  ;
type
  TTestGenericObjectThreadList = class(TtiTestCase)
  private
    FList: TtiGenericObjectThreadList<TtiOPFTestIntegerProp>;
  protected
    procedure   SetUp; override;
    procedure   TearDown; override;
    procedure   AddObjects;
  public
  published
    procedure TestAdd;
    procedure TestEnumerator;
    procedure TestFilteredEnumerator_AllObjects;
    procedure TestFilteredEnumerator_NoObjects;
    procedure TestFilteredEnumerator_OddObjects;
    procedure TestFilteredEnumerator_EvenObjects;
    procedure TestItems;
    procedure TestLast;
    procedure TestFirst;
    procedure TestFirstExcludeDeleted;
    procedure TestLastExcludeDeleted;
    procedure TestInsert;
    procedure TestInsertBefore;
  end;

procedure RegisterTests;

implementation

uses
  SysUtils
  ,tiOPFManager
  ,tiTestDependencies
 ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestGenericObjectThreadList);
end;

{ TtiTestGenericObjectList }

procedure TTestGenericObjectThreadList.AddObjects;
var
  item: TtiOPFTestIntegerProp;
  i: integer;
begin
  for i := 1 to 5 do
  begin
    item:= TtiOPFTestIntegerProp.CreateNew;
    item.IntField:= i;
    FList.Add(item);
  end;
end;

procedure TTestGenericObjectThreadList.SetUp;
begin
  inherited;
  FList:= TtiGenericObjectThreadList<TtiOPFTestIntegerProp>.Create;
  FList.OwnsObjects:= true;
end;

procedure TTestGenericObjectThreadList.TearDown;
begin
  inherited;
  FreeAndNil(FList);
end;

procedure TTestGenericObjectThreadList.TestAdd;
var item: TtiOPFTestIntegerProp;
begin
  item:= TtiOPFTestIntegerProp.CreateNew;
  item.IntField:= 1;
  CheckEquals(0, FList.Count);
  FList.Add(item);
  CheckEquals(1, FList.Count);
end;

procedure TTestGenericObjectThreadList.TestEnumerator;
var item: TtiOPFTestIntegerProp;
  intSum: integer;
begin
  AddObjects;

  intSum:= 0;
  for item in Flist  do
    intSum:= intSum + item.IntField;

  CheckEquals(15, intSum);
end;

procedure TTestGenericObjectThreadList.TestFilteredEnumerator_AllObjects;
var item: TtiOPFTestIntegerProp;
  intSum, intCount: integer;
begin
  AddObjects;

  intSum:= 0;
  intCount:= 0;
  for item in Flist.FilteredEnumerator(function (TestObject: TtiOPFTestIntegerProp): Boolean
                                       begin
                                         result:= true;
                                       end) do
  begin
    inc(intCount);
    intSum:= intSum + item.IntField;
  end;
  CheckEquals(15, intSum);
  CheckEquals(5, intCount);
end;

procedure TTestGenericObjectThreadList.TestFilteredEnumerator_EvenObjects;
var item: TtiOPFTestIntegerProp;
  intSum, intCount: integer;
begin
  AddObjects;

  intSum:= 0;
  intCount:= 0;
  for item in Flist.FilteredEnumerator(function (TestObject: TtiOPFTestIntegerProp): Boolean
                                       begin
                                         result:= TestObject.IntField mod 2 = 0;
                                       end) do
  begin
    inc(intCount);
    intSum:= intSum + item.IntField;
  end;
  CheckEquals(6, intSum);
  CheckEquals(2, intCount);
end;

procedure TTestGenericObjectThreadList.TestFilteredEnumerator_NoObjects;
var item: TtiOPFTestIntegerProp;
  intSum, intCount: integer;
begin
  AddObjects;

  intSum:= 0;
  intCount:= 0;
  for item in Flist.FilteredEnumerator(function (TestObject: TtiOPFTestIntegerProp): Boolean
                                       begin
                                         result:= false;
                                       end) do
  begin
    inc(intCount);
    intSum:= intSum + item.IntField;
  end;
  CheckEquals(0, intSum);
  CheckEquals(0, intCount);
end;

procedure TTestGenericObjectThreadList.TestFilteredEnumerator_OddObjects;
var item: TtiOPFTestIntegerProp;
  intSum, intCount: integer;
begin
  AddObjects;

  intSum:= 0;
  intCount:= 0;
  for item in Flist.FilteredEnumerator(function (TestObject: TtiOPFTestIntegerProp): Boolean
                                       begin
                                         result:= TestObject.IntField mod 2 = 1;
                                       end) do
  begin
    inc(intCount);
    intSum:= intSum + item.IntField;
  end;
  CheckEquals(9, intSum);
  CheckEquals(3, intCount);
end;

procedure TTestGenericObjectThreadList.TestFirst;
begin
  AddObjects;
  CheckEquals(1, FList.First.IntField);
end;

procedure TTestGenericObjectThreadList.TestFirstExcludeDeleted;
begin
  AddObjects;
  CheckEquals(1, FList.First.IntField);
  FList.First.Deleted:= true;
  CheckEquals(1, FList.First.IntField);
  CheckEquals(2, FList.FirstExcludeDeleted.IntField);
end;

procedure TTestGenericObjectThreadList.TestInsert;
var item: TtiOPFTestIntegerProp;
begin
  AddObjects;
  CheckEquals(1, FList[0].IntField);
  CheckEquals(2, FList[1].IntField);

  item:= TtiOPFTestIntegerProp.CreateNew;
  item.IntField:= 99;
  FList.Insert(1, item);
  CheckEquals(1, FList[0].IntField);
  CheckEquals(99, FList[1].IntField);
  CheckEquals(2, FList[2].IntField);
end;

procedure TTestGenericObjectThreadList.TestInsertBefore;
var item: TtiOPFTestIntegerProp;
begin
  AddObjects;
  CheckEquals(1, FList[0].IntField);
  CheckEquals(2, FList[1].IntField);

  item:= TtiOPFTestIntegerProp.CreateNew;
  item.IntField:= 99;
  FList.Insert(FList[1], item);
  CheckEquals(1, FList[0].IntField);
  CheckEquals(99, FList[1].IntField);
  CheckEquals(2, FList[2].IntField);

end;

procedure TTestGenericObjectThreadList.TestItems;
var
  item: TtiOPFTestIntegerProp;
  i: integer;
begin
  AddObjects;

  // check reads
  for i := 0 to 4 do
  begin
    CheckEquals(i + 1, FList[i].IntField);
  end;

  FList.OwnsObjects:= false;
  // check writes
  item:= FList.First;
  FList[0]:= FList.Last;
  FList[4]:= item;

  CheckEquals(5, FList.First.IntField);
  CheckEquals(1, FList.Last.IntField);

  // test frees properly
  FList.OwnsObjects:= true;
  FreeAndNil(FList);
end;

procedure TTestGenericObjectThreadList.TestLast;
begin
  AddObjects;
  CheckEquals(5, FList.Last.IntField);
end;

procedure TTestGenericObjectThreadList.TestLastExcludeDeleted;
begin
  AddObjects;
  CheckEquals(5, FList.Last.IntField);
  FList.Last.Deleted:= true;
  CheckEquals(5, FList.Last.IntField);
  CheckEquals(4, FList.LastExcludeDeleted.IntField);
end;

end.
