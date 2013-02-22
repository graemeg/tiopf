unit tiObject_TST;

{$I tiDefines.inc}

interface

uses
  Classes,  // needed for TStringList
  {$IFDEF FPC}
  testregistry,
  {$ENDIF}
  tiTestFramework,
  tiObject;

type
  TtiObjectListForTesting  = class;
  TtiObjectForTesting   = class;

  TtiObjectTestCase = class(TtiTestCase)
  private
    procedure CheckTSTPerObjAbs(AData: TtiObjectForTesting; AValue: integer);
    procedure CheckTSTPerObjList(AData: TtiObjectListForTesting; AValue: integer);
    procedure TstFindMethod(AObject : TtiObject; var AFound : boolean);
    procedure TstFindMethodWithParam(AObject : TtiObject; var AFound : boolean; AUserContext: Pointer);
    procedure TstFindAll(AObject : TtiObject; var AFound : boolean);
  published
    procedure Owner;
    procedure Parent_InheritsFromVsIs;
    procedure Parent_TtiObject;
    procedure Parent_TtiObjectList;

    procedure OIDGenerator;
    procedure GetOID;

    procedure PropType;
    procedure IsReadWriteProp;
    procedure PropCount;
    procedure SetPropValue;
    procedure SetBooleanPropValue;
    procedure GetPropValue;
    procedure SetPropValueNested;
    procedure GetPropValueNested;

    procedure Deleted_TtiObject;
    procedure Deleted_TtiObjectList_AutoSetItemOwnerTrue;
    procedure Deleted_TtiObjectList_AutoSetItemOwnerFalse;
    procedure Deleted_Owned;
    procedure Deleted_Referenced;
    procedure Dirty;
    procedure Dirty_And_OID;
    procedure Index;
    procedure tiObject_Equals;
    procedure tiObject_EqualsDifMessage;
    procedure ObjectStateAsString;
    procedure FindByOID;
    procedure FindWithMethod;
    procedure FindWithMethodAndParam;
    procedure FindAllWithMethod;
    procedure IsUniqueOID;
    procedure AssignFlat;
    procedure AssignList;
    procedure AssignCompound;
    procedure AssignCaptions;
    procedure AssignCaptionsAndObjects;
    procedure CloneFlat;
    procedure CloneList;
    procedure CloneCompound;
    procedure TopOfHierarchy;
    procedure IsValid;
    procedure AsDebugString;

    procedure FieldString;
    procedure FieldString_Equals;
    procedure FieldString_Assign;

    procedure FieldInt64;
    procedure FieldInt64_Equals;
    procedure FieldInt64_Assign;

    procedure FieldFloat;
    procedure FieldFloat_Equals;
    procedure FieldFloat_Assign;

    procedure FieldCurrency;
    procedure FieldCurrency_Equals;
    procedure FieldCurrency_Assign;
    procedure FieldCurrency_Inc1;
    procedure FieldCurrency_Inc2;

    procedure FieldDate;
    procedure FieldDate_Fail;
    procedure FieldDate_Equals;
    procedure FieldDate_Assign;
    procedure FieldDate_YearsMonthsDays;

    procedure FieldDateTime;
    procedure FieldDateTime_Equals;
    procedure FieldDateTime_Assign;
    procedure FieldDateTime_YearsMonthsDays;
    procedure FieldDateTime_HoursMinutesSeconds;

    procedure FieldBoolean;
    procedure FieldBoolean_Equals;
    procedure FieldBoolean_Assign;
    // procedure FieldStream;
    // procedure FieldStream_Equals;
    procedure FieldName;
    procedure AttachDetachObserver;
    procedure NotifyObservers;
    procedure NotifyObservers_vs_IsValid_call_count;
  end;


  TtiObjectListTestCase = class(TtiTestCase)
  private
    FInBothAndEquals: TtiObjectList;
    FInBothAndNotEquals: TtiObjectList;
    FIn1Only: TtiObjectList;
    FIn2Only: TtiObjectList;
    FDifferenceMessage: string;

    procedure InBothAndEqualsEvent(AItem1, AItem2: TtiObject);
    procedure InBothAndNotEqualsEvent(const AItem1, AItem2: TtiObject; var ADifferenceMessage: string);
    procedure In1OnlyEvent(AItem1, AItem2: TtiObject);
    procedure In2OnlyEvent(AItem1, AItem2: TtiObject);
    function ItemMatchFunc(const AItem1, AItem2: TtiObject): Boolean;
    function ItemCompareFunc(const AItem1, AItem2: TtiObject): Boolean;

    function  CreateList: TtiObjectListForTesting;
    procedure DoForEachMethod(const AData: TtiObject);
    function DoIsUnique(const AItem1, AItem2: TtiObject): integer;
  public
    procedure   SetUp; override;
    procedure   TearDown; override;
  published
    procedure   Add;
    procedure   AddItemOwner;
    procedure   Items;
    procedure   Count;  {$IFDEF DUNIT2} reintroduce; overload; {$ENDIF}
    procedure   CountNotDeleted;
    procedure   OwnsObjects;
    procedure   ItemOwner;
    procedure   AutoSetItemOwner;
    procedure   ForEachMethod;
    procedure   ForEachMethodRegular;
    procedure   Clear;
    procedure   Empty;
    procedure   IndexOf;
    procedure   Last;
    procedure   First;
    procedure   FirstExcludeDeleted;
    procedure   LastExcludeDeleted;
    procedure   Delete;
    procedure   Remove;
    procedure   Extract;
    procedure   InsertByIndex;
    procedure   InsertByObject;
    procedure   InsertItemOwner;
    procedure   MarkListItemsForDeletion;
    procedure   MarkListItemsDirty;
    procedure   PropToStrings;
    procedure   FindByProps_DirectValue;
    procedure   FindByProps_TypedValue;
    procedure   FindByProps_PropertyPath;
    procedure   SortByOID;
    procedure   SortByProps;
    procedure   Find;
    procedure   FindInHierarchy;
    procedure   FindSortedUntyped;
    procedure   CompareWithEvent;
    procedure   FreeDeleted;
    procedure   tiListToCSVDefault;
    procedure   tiListToCSVFields;
    procedure   tiListToStreamDefault;
    procedure   tiListToStreamDelims;
    procedure   tiListToStreamFields;
    procedure   GetEnumerator;
    procedure   ForIn;
    procedure   Add_NotifyObservers_Subject;
    procedure   Remove_NotifyObservers_Subject;
    procedure   BeginEnd_NotifyObservers_Subject;
    procedure   IsUniqueCustom;
  end;


  TtiObjectListForTesting = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtiObjectForTesting; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtiObjectForTesting); reintroduce;
  public
    property    Items[i:integer]: TtiObjectForTesting read GetItems write SetItems;
    function    Add(AObject: TtiObjectForTesting): integer; reintroduce;
    function    Clone : TtiObjectListForTesting; reintroduce;
    constructor CreateNew(const ADatabaseName : string = ''; const APersistenceLayerName : string = ''); override;
    function    FindCompareIntProp(const AObject: TtiObject; const AValue): integer;
  published
  end;


  TtstOrdProp = (tstOrdProp_1, tstOrdProp_2, tstOrdProp_3);


  TtiObjectForTesting = class(TtiObject)
  private
    FBoolProp: boolean;
    FIntProp: integer;
    FFloatProp: extended;
    FStrProp: string;
    FDateProp: TDateTime;
    FOrdProp: TtstOrdProp;
  protected
    function    GetOwner: TtiObjectListForTesting; reintroduce;
    procedure   SetOwner(const AValue: TtiObjectListForTesting); reintroduce;
  public
    constructor Create; override;
    constructor CreateNew(const AOwner: TtiObject; const ADatabaseName: string = ''; const APersistenceLayerName : string = ''); override;
    property    Owner: TtiObjectListForTesting read GetOwner write SetOwner;
    procedure   Populate;
    function    Clone: TtiObjectForTesting; reintroduce;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
    function    Equals(const AData: TtiObject): boolean; override;
  published
    property    StrProp: string read FStrProp write FStrProp;
    property    IntProp: integer read FIntProp write FIntProp;
    property    FloatProp: extended read FFloatProp write FFloatProp;
    property    DateProp: TDateTime read FDateProp write FDateProp;
    property    BoolProp: boolean read FBoolProp write FBoolProp;
    property    OrdProp: TtstOrdProp read FOrdProp write FOrdProp;
    property    StrPropReadOnly: string read FStrProp;
  end;


  TtiObjectWithOwnedForTesting = class(TtiObjectForTesting)
  private
    FObjProp: TtiObjectForTesting;
  public
    constructor Create; override;
    destructor  Destroy; override;
    function    Equals(const AData : TtiObject): boolean; override;
  published
    property ObjProp : TtiObjectForTesting read FObjProp;
  end;


  TtstPerObjOwnedObjCanAssign = class(TtiObjectWithOwnedForTesting)
  protected
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    function    Clone : TtstPerObjOwnedObjCanAssign; reintroduce;
  end;


  TtstPerObjMappedObj = class(TtiObjectForTesting)
  private
    FObjProp: TtiObjectForTesting;
  published
    property ObjProp : TtiObjectForTesting read FObjProp write FObjProp;
  end;


  // Used in AttachDetachObserver test.
  TtstSubject = class(TtiObject)
  private
    FIsValidCallCount: integer;
    FName: string;
    procedure SetName(const AValue: string);
  public
    constructor Create; override;
    function    IsValid(const AErrors: TtiObjectErrors): boolean; override;
    property    Name: string read FName write SetName;
    property    IsValidCallCount: integer read FIsValidCallCount write FIsValidCallCount;
  end;


  // Used in AttachDetachObserver test.
  TtstObserver = class(TtiObject)
  private
    FName: string;
  public
    property  Name: string read FName write FName;
    procedure Update(ASubject: TtiObject); override;
  end;


  // Used in NotifyObserver test.
  TtstObserver2 = class(TtiObject)
  private
    FLastUpdateSubject: TtiObject;
    FLastNotifyOperation: TNotifyOperation;
    FUpdateCount: integer;
  public
    constructor Create; override;
    procedure Update(ASubject: TtiObject; AOperation: TNotifyOperation; AData: TtiObject=nil); override;
    property LastNotifyOperation: TNotifyOperation read FLastNotifyOperation write FLastNotifyOperation;
    property LastUpdateSubject: TtiObject read FLastUpdateSubject write FLastUpdateSubject;
    property UpdateCount: integer read FUpdateCount write FUpdateCount;
  end;



procedure RegisterTests;


const
  cStrPropErrorMsg    = 'Please enter a StrProp value';
  cStrPropErrorCode   = 1;
  cIntPropErrorMsg    = 'Please enter an IntProp value';
  cIntpropErrorCode   = 2;
  cFloatPropErrorMsg  = 'Please enter a FloatProp value';
  cFloatPropErrorCode = 3;


implementation
uses
  tiBOMsForTesting,
  tiTestDependencies,
  tiOPFManager,
  tiUtils,
  tiConstants,
  tiOID,
  tiOIDGUID,
  tiRTTI,
  tiExcept,
  tiSmartPointer,
  tiBaseMediator,
  SysUtils,
  TypInfo,
  DateUtils;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TtiObjectTestCase);
  tiRegisterNonPersistentTest(TtiObjectListTestCase);
end;


function CreateTestData : TtiObjectListForTesting;
var
  lOID : Integer;
  i : integer;
  lData : TtiObjectWithOwnedForTesting;
begin
  lOID := 1;
  result := TtiObjectListForTesting.Create;
  result.ObjectState := posClean;
  result.OID.AsString := IntToStr(lOID);
  Inc(lOID);

  for i := 9 downto 0 do
  begin
    lData := TtiObjectWithOwnedForTesting.Create;
    lData.ObjectState := posClean;
    lData.OID.AsString := IntToStr(lOID);
    Inc(lOID);
    lData.ObjProp.ObjectState := posClean;
    lData.ObjProp.OID.AsString := IntToStr(lOID);
    Inc(lOID);
    result.Add(lData);
  end;
end;

procedure SetTSTPerObjList(AData: TtiObjectListForTesting; AValue: integer);
begin
  AData.OID.AsString := IntToStr(AValue);
end;

procedure SetTSTPerObjAbs(AData: TtiObjectForTesting; AValue: integer);
begin
  AData.StrProp  := IntToStr(AValue);
  AData.IntProp  := AValue;
  AData.FloatProp := AValue + AValue / 10 + AValue / 100 + AValue + AValue / 1000;
  AData.DateProp := AValue;
  AData.BoolProp := (AValue mod 2) = 0;
  AData.OID.AsString       := IntToStr(AValue);
end;

function CreateTestDataList: TtiObjectListForTesting;
var
  lItem : TtiObjectWithOwnedForTesting;
  i     : integer;
begin
  result := TtiObjectListForTesting.Create;
  SetTSTPerObjList(Result, -1);
  for i := 0 to 4 do
  begin
    lItem := TtiObjectWithOwnedForTesting.Create;
    Result.Add(lItem);
    SetTSTPerObjAbs(lItem, i*2);
    SetTSTPerObjAbs(lItem.ObjProp, i*2+1);
  end;
end;

type

  TtstAsDebugStringObject = class(TtiObject)
  private
    FProp2: string;
  published
    property Prop2: string read FProp2 write FProp2;
  end;

  TtstAsDebugStringObjectList = class(TtiObjectList)
  private
    FProp1: string;
    FObjectProp: TtstAsDebugStringObject;
  protected
    function GetCaption: string; override;
  published
    property Prop1: string read FProp1 write FProp1;
    property ObjectProp: TtstAsDebugStringObject read FObjectProp write FObjectProp;
  end;


  TtiObjectForTestingOID = class(TtiObject)
  public
    function OIDGenerator: TtiOIDGenerator; override;
    function GetOID: TtiOID; override;
  end;


function TtstAsDebugStringObjectList.GetCaption: string;
begin
  result:= 'Class name for ' + ClassName;
end;

{ TtiObjectForTestingOID }

function TtiObjectForTestingOID.GetOID: TtiOID;
begin
  result:= inherited GetOID;
end;

function TtiObjectForTestingOID.OIDGenerator: TtiOIDGenerator;
begin
  result:= inherited OIDGenerator;
end;

procedure TtiObjectTestCase.AsDebugString;
var
  LL: TtstAsDebugStringObjectList;
  LO1: TtstAsDebugStringObject;
  LO2: TtstAsDebugStringObject;
  LO3: TtstAsDebugStringObject;

const
  // ToDo: Remove the trailing space after OID=X, 
  CAll =
    'TtstAsDebugStringObjectList, Class name for TtstAsDebugStringObjectList, posEmpty, OID=1, **Dirty**' + #13#10 +
    '  Prop1 = test prop 1' + #13#10 +
    '  TtstAsDebugStringObject, posEmpty, OID=2, ' + #13#10 +
    '    Prop2 = test prop 2' + #13#10 +
    '  TtstAsDebugStringObject, posDelete, OID=3, **Dirty**' + #13#10 +
    '    Prop2 = test prop 3' + #13#10 +
    '  TtstAsDebugStringObject, posEmpty, OID=4, ' + #13#10 +
    '    Prop2 = test prop 4';

  CDeleted =
    'TtstAsDebugStringObjectList, Class name for TtstAsDebugStringObjectList, posEmpty, OID=1, **Dirty**' + #13#10 +
    '  Prop1 = test prop 1' + #13#10 +
    '  TtstAsDebugStringObject, posEmpty, OID=2, ' + #13#10 +
    '    Prop2 = test prop 2' + #13#10 +
    '  TtstAsDebugStringObject, posEmpty, OID=4, ' + #13#10 +
    '    Prop2 = test prop 4';

  CChildren =
    'TtstAsDebugStringObjectList, Class name for TtstAsDebugStringObjectList, posEmpty, OID=1, **Dirty**' + #13#10 +
    '  Prop1 = test prop 1';

  CData =
  '  Prop1 = test prop 1' + #13#10 +
  '      Prop2 = test prop 2' + #13#10 +
  '      Prop2 = test prop 3';

  CClassName =
    'TtstAsDebugStringObjectList';

  CObjectState =
    'posEmpty, **Dirty**';

  COID =
    'OID=1';

  CCaption =
    'Class name for TtstAsDebugStringObjectList';

begin

  LL:= nil;
//  LO1:= nil;
//  LO2:= nil;
  LO3:= nil;
  try
    LL:= TtstAsDebugStringObjectList.Create;
    LL.OID.AsString:= '1';
    LL.Prop1:= 'test prop 1';

    LO1:= TtstAsDebugStringObject.Create;
    LO1.Prop2:= 'test prop 2';
    LO1.OID.AsString:= '2';
    LL.Add(LO1);

    LO2:= TtstAsDebugStringObject.Create;
    LO2.Prop2:= 'test prop 3';
    LO2.OID.AsString:= '3';
    LO2.Deleted:= true;
    LL.Add(LO2);

    LO3:= TtstAsDebugStringObject.Create;
    LO3.Prop2:= 'test prop 4';
    LO3.OID.AsString:= '4';
    LL.ObjectProp:= LO3;


    CheckEquals(CAll, LL.AsDebugString);
    CheckEquals(CDeleted, LL.AsDebugString(CTIAsDebugStringDataAll-[adsDeleted]));
    CheckEquals(CChildren, LL.AsDebugString(CTIAsDebugStringDataAll-[adsChildren]));
    CheckEquals(CClassName, LL.AsDebugString([adsClassName]));
    CheckEquals(CObjectState, LL.AsDebugString([adsObjectState]));
    CheckEquals(COID, LL.AsDebugString([adsOID]));
    CheckEquals(CCaption, LL.AsDebugString([adsCaption]));

  finally
    LL.Free;
    LO3.Free;
  end;
end;

type

  TtiObjectForTestingAssignCaptions = class(TtiObject)
  protected
    function GetCaption: string; override;
  end;

  { TtiObjectForTestingAssignCaptions }

  function TtiObjectForTestingAssignCaptions.GetCaption: string;
  begin
    result:= IntToStr(Index);
  end;

procedure TtiObjectTestCase.AssignCaptions;
var
  LObjectList: TtiObjectList;
  LStringList: TStringList;
begin
  LObjectList:= nil;
  LStringList:= nil;
  try
    LObjectList := TtiObjectList.Create;
    LStringList:= TStringList.Create;
    LObjectList.Add(TtiObjectForTestingAssignCaptions.Create);
    LObjectList.Add(TtiObjectForTestingAssignCaptions.Create);
    LObjectList.Add(TtiObjectForTestingAssignCaptions.Create);
    LObjectList.AssignCaptions(LStringList);
    CheckEquals(3, LStringList.Count);
    CheckEquals('0', LStringList.Strings[0]);
    CheckNull(LStringList.Objects[0]);
    CheckEquals('1', LStringList.Strings[1]);
    CheckNull(LStringList.Objects[1]);
    CheckEquals('2', LStringList.Strings[2]);
    CheckNull(LStringList.Objects[2]);
  finally
    LObjectList.Free;
    LStringList.Free;
  end;
end;

procedure TtiObjectTestCase.AssignCaptionsAndObjects;
var
  LObjectList: TtiObjectList;
  LStringList: TStringList;
begin
  LObjectList:= nil;
  LStringList:= nil;
  try
    LObjectList := TtiObjectList.Create;
    LStringList:= TStringList.Create;
    LObjectList.Add(TtiObjectForTestingAssignCaptions.Create);
    LObjectList.Add(TtiObjectForTestingAssignCaptions.Create);
    LObjectList.Add(TtiObjectForTestingAssignCaptions.Create);
    LObjectList.AssignCaptionsAndObjects(LStringList);
    CheckEquals(3, LStringList.Count);
    CheckEquals('0', LStringList.Strings[0]);
    CheckSame(LObjectList.Items[0], LStringList.Objects[0]);
    CheckEquals('1', LStringList.Strings[1]);
    CheckSame(LObjectList.Items[1], LStringList.Objects[1]);
    CheckEquals('2', LStringList.Strings[2]);
    CheckSame(LObjectList.Items[2], LStringList.Objects[2]);
  finally
    LObjectList.Free;
    LStringList.Free;
  end;

end;

procedure TtiObjectTestCase.AssignCompound;
var
  lTestExceptionFrom : TtiObjectWithOwnedForTesting;
  lTestExceptionTo  : TtiObjectWithOwnedForTesting;
  lFrom : TtstPerObjOwnedObjCanAssign;
  lTo  : TtstPerObjOwnedObjCanAssign;
begin
  lTestExceptionFrom := TtiObjectWithOwnedForTesting.Create;
  try
    lTestExceptionTo := TtiObjectWithOwnedForTesting.Create;
    try
      try
        lTestExceptionTo.Assign(lTestExceptionFrom);
        Fail('An exception was not raised when it should have been.');
      except
        on e:exception do
          Check(true, 'Should have special type of exception?');
      end;
    finally
      lTestExceptionTo.Free;
    end;
  finally
    lTestExceptionFrom.Free;
  end;

  lFrom := TtstPerObjOwnedObjCanAssign.Create;
  try
    SetTSTPerObjAbs(lFrom, 1);
    SetTSTPerObjAbs(lFrom.ObjProp, 2);
    lTo  := TtstPerObjOwnedObjCanAssign.Create;
    try
      lTo.Assign(lFrom);
      CheckTSTPerObjAbs(lFrom, 1);
      CheckTSTPerObjAbs(lFrom.ObjProp, 2);
    finally
      lTo.Free;
    end;
  finally
    lFrom.Free;
  end;
end;


procedure TtiObjectTestCase.CheckTSTPerObjAbs(AData : TtiObjectForTesting; AValue : integer);
begin
  CheckEquals(IntToStr(AValue), AData.StrProp, 'Failed on StrField');
  CheckEquals(AValue, AData.IntProp,'Failed on IntField');
  CheckEquals(AValue + AValue / 10 + AValue / 100 + AValue + AValue / 1000, AData.FloatProp, cDUnitTestFloatPrecision, 'Failed on FloatField');
  CheckEquals(AValue, AData.DateProp, 0.00001, 'Failed on DateField');
  CheckEquals((AValue mod 2) = 0, AData.BoolProp, 'Failed on Bool');
  CheckEquals(IntToStr(AValue), AData.OID.AsString,'Failed on OID');
end;


procedure TtiObjectTestCase.CheckTSTPerObjList(AData : TtiObjectListForTesting; AValue : integer);
begin
  CheckEquals(IntToStr(AValue), AData.OID.AsString,'Failed on OID');
end;


procedure TtiObjectTestCase.AssignFlat;
var
  lFrom : TtiObjectForTesting;
  lTo  : TtiObjectForTesting;
begin
  lFrom := TtiObjectForTesting.Create;
  try
    SetTSTPerObjAbs(lFrom, 1);
    lTo  := TtiObjectForTesting.Create;
    try
      SetTSTPerObjAbs(lTo, 2);
      lTo.Assign(lFrom);
      CheckTSTPerObjAbs(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      //CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
      Check(nil = lTo.Owner, 'Failed on Owner');
    finally
      lTo.Free;
    end;
  finally
    lFrom.Free;
  end;
end;


procedure TtiObjectTestCase.AssignList;
var
  lFrom : TtiObjectListForTesting;
  lItem : TtiObjectForTesting;
  lTo  : TtiObjectListForTesting;
begin
  lFrom := TtiObjectListForTesting.Create;
  try
    lFrom.OwnsObjects := true;
    SetTSTPerObjList(lFrom, 1);
    lItem := TtiObjectForTesting.Create;
    SetTSTPerObjAbs(lItem, 2);
    lFrom.Add(lItem);
    lTo  := TtiObjectListForTesting.Create;
    try
      SetTSTPerObjList(lTo, 3);
      lTo.Assign(lFrom);
      CheckTSTPerObjList(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      //CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
      Check(nil = lTo.Owner, 'Failed on Owner');
      CheckEquals(1, lTo.Count, 'Failed on Count');
      CheckTSTPerObjAbs(lTo.Items[0], 2);
      Check(lTo = lTo.Items[0].Owner, 'Failed on Owner');
    finally
      lTo.Free;
    end;
  finally
    lFrom.Free;
  end;

  lFrom := TtiObjectListForTesting.Create;
  try
    lFrom.OwnsObjects := false;
    lFrom.AutoSetItemOwner := false;
    SetTSTPerObjList(lFrom, 1);
    lItem := TtiObjectForTesting.Create;
    try
      SetTSTPerObjAbs(lItem, 2);
      lFrom.Add(lItem);
      lTo  := TtiObjectListForTesting.Create;
      lTo.OwnsObjects := false;
      lTo.AutoSetItemOwner := false;
      try
        SetTSTPerObjList(lTo, 3);
        lTo.Assign(lFrom);
        CheckTSTPerObjList(lTo, 1);
        Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
        //CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
        Check(nil = lTo.Owner, 'Failed on Owner');
        CheckEquals(1, lTo.Count, 'Failed on Count');
        CheckTSTPerObjAbs(lTo.Items[0], 2);
        Check(nil = lTo.Items[0].Owner, 'Failed on Owner');
      finally
        lTo.Free;
      end;
    finally
      lItem.Free;
    end;
  finally
    lFrom.Free;
  end;
end;

procedure TtiObjectTestCase.CloneFlat;
var
  lFrom : TtiObjectForTesting;
  lTo  : TtiObjectForTesting;
begin
  lFrom := TtiObjectForTesting.Create;
  try
    SetTSTPerObjAbs(lFrom, 1);
    lTo  := lFrom.Clone;
    try
      SetTSTPerObjAbs(lTo, 2);
      lTo.Assign(lFrom);
      CheckTSTPerObjAbs(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      //CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
      Check(nil = lTo.Owner, 'Failed on Owner');
    finally
      lTo.Free;
    end;
  finally
    lFrom.Free;
  end;
end;


procedure TtiObjectTestCase.Deleted_TtiObjectList_AutoSetItemOwnerFalse;
var
  LList : TtiObjectList;
begin
  LList := TtiObjectList.Create;
  try
    LList.AutoSetItemOwner:= False;
    LList.Add(TtiObject.Create);
    LList.Add(TtiObject.Create);
    CheckEquals(2, LList.Count);
    CheckObjectState(posEmpty, LList);
    CheckObjectState(posEmpty, LList.Items[0]);
    CheckObjectState(posEmpty, LList.Items[1]);
    LList.Deleted:= true;
    CheckObjectState(posDelete, LList);
    CheckObjectState(posEmpty, LList.Items[0]);
    CheckObjectState(posEmpty, LList.Items[1]);
  finally
    LList.Free;
  end;
end;

procedure TtiObjectTestCase.Deleted_TtiObjectList_AutoSetItemOwnerTrue;
var
  LList : TtiObjectList;
begin
  LList := TtiObjectList.Create;
  try
    LList.AutoSetItemOwner:= True;
    LList.Add(TtiObject.Create);
    LList.Add(TtiObject.Create);
    CheckEquals(2, LList.Count);
    CheckObjectState(posEmpty, LList);
    CheckObjectState(posEmpty, LList.Items[0]);
    CheckObjectState(posEmpty, LList.Items[1]);
    LList.Deleted:= true;
    CheckObjectState(posDelete, LList);
    CheckObjectState(posDelete, LList.Items[0]);
    CheckObjectState(posDelete, LList.Items[1]);
  finally
    LList.Free;
  end;
end;

type
  TTestTIObjectDeleteOwned = class(TtiObject)
  private
    FData: TtiObject;
  public
    constructor Create(const AOwnsData: Boolean); reintroduce;
    destructor Destroy; override;
  published
    property Data: TtiObject read FData write FData;
  end;

  { TTestTIObjectDeleteOwned }

  constructor TTestTIObjectDeleteOwned.Create(const AOwnsData: Boolean);
  begin
    inherited Create;
    FData:= TtiObject.Create;
    if AOwnsData then
      FData.Owner:= Self;
  end;

  destructor TTestTIObjectDeleteOwned.Destroy;
  begin
    FData.Free;
    inherited;
  end;

procedure TtiObjectTestCase.Deleted_Owned;
var
  LData: TTestTIObjectDeleteOwned;
begin
  LData:= TTestTIObjectDeleteOwned.Create(True);
  try
    CheckObjectState(posEmpty, LData);
    CheckObjectState(posEmpty, LData.Data);
    LData.Deleted:= True;
    CheckObjectState(posDelete, LData);
    CheckObjectState(posDelete, LData.Data);
  finally
    LData.Free;
  end;
end;

procedure TtiObjectTestCase.Deleted_Referenced;
var
  LData: TTestTIObjectDeleteOwned;
begin
  LData:= TTestTIObjectDeleteOwned.Create(False);
  try
    CheckObjectState(posEmpty, LData);
    CheckObjectState(posEmpty, LData.Data);
    LData.Deleted:= True;
    CheckObjectState(posDelete, LData);
    CheckObjectState(posEmpty, LData.Data);
  finally
    LData.Free;
  end;
end;

procedure TtiObjectTestCase.Deleted_TtiObject;
var
  LItem : TtiObject;
begin
  LItem := TtiObject.Create;
  try
    Check(Not LItem.Deleted, 'Failed on 1');
    LItem.ObjectState := posPK;
    Check(Not LItem.Deleted, 'Failed on 2');
    LItem.ObjectState := posClean;
    Check(Not LItem.Deleted, 'Failed on 3');
    LItem.ObjectState := posCreate;
    Check(not LItem.Deleted, 'Failed on 4');
    LItem.ObjectState := posUpdate;
    Check(not LItem.Deleted, 'Failed on 5');
    LItem.ObjectState := posDelete;
    Check(LItem.Deleted, 'Failed on 6');
    LItem.ObjectState := posDeleted;
    Check(LItem.Deleted, 'Failed on 7');
    LItem.ObjectState := posClean;
    Check(Not LItem.Deleted, 'Failed on 8');
  finally
    LItem.Free;
  end;
end;

procedure TtiObjectTestCase.Dirty;
var
  lData : TtiObjectListForTesting;
  lGroup : TtiObjectForTesting;
  lItem : TtiObjectWithOwnedForTesting;
begin
  { Test reading and writing to the .Dirty property }
  lData := TtiObjectListForTesting.Create;
  try
    Check(Not lData.Dirty, 'Failed on 1');
    lData.ObjectState := posPK;
    Check(Not lData.Dirty, 'Failed on 2');
    lData.Dirty := False;
    Check(Not lData.Dirty, 'Failed on 2.1');
    Check(lData.ObjectState = posClean, 'Failed on 2.2');
    lData.ObjectState := posClean;
    Check(Not lData.Dirty, 'Failed on 3');
    lData.Dirty := False;
    Check(Not lData.Dirty, 'Failed on 3.1');
    Check(lData.ObjectState = posClean, 'Failed on 3.2');
    lData.ObjectState := posCreate;
    Check(lData.Dirty, 'Failed on 4');
    lData.Dirty := False;
    Check(Not lData.Dirty, 'Failed on 4.1');
    Check(lData.ObjectState = posClean, 'Failed on 4.2');
    lData.ObjectState := posUpdate;
    Check(lData.Dirty, 'Failed on 5');
    lData.Dirty := False;
    Check(Not lData.Dirty, 'Failed on 5.1');
    Check(lData.ObjectState = posClean, 'Failed on 5.2');
    lData.ObjectState := posDelete;
    Check(lData.Dirty, 'Failed on 6');
    lData.Dirty := False;
    Check(Not lData.Dirty, 'Failed on 6.1');
    Check(lData.ObjectState = posClean, 'Failed on 6.2');
    { 2006-05-11 graemeg: As soon as we know what to do in this case, we can
      write the test for setting Dirty = False. }
    lData.ObjectState := posDeleted;
    Check(not lData.Dirty, 'Failed on 7');
    lData.ObjectState := posClean;
    Check(Not lData.Dirty, 'Failed on 8');

    lGroup := TtiObjectForTesting.Create;
    lData.Add(lGroup);
    Check(Not lData.Dirty, 'Failed on 9');
    lGroup.ObjectState := posUpdate;
    Check(lData.Dirty, 'Failed on 10');
    lGroup.ObjectState := posClean;

    lItem := TtiObjectWithOwnedForTesting.Create;
    lData.Add(lItem);
    Check(Not lData.Dirty, 'Failed on 11');
    lItem.ObjectState := posUpdate;
    Check(lData.Dirty, 'Failed on 12');
    lItem.ObjectState := posClean;

    lItem.ObjProp.ObjectState := posUpdate;
    Check(lData.Dirty, 'Failed on 13');
  finally
    lData.Free;
  end;
end;

{ testing automatic generation or OID value }
procedure TtiObjectTestCase.Dirty_And_OID;
var
  LObject: TtiObjectForTestingOID;
  LOIDGeneratorClass: TtiOIDGeneratorClass;
begin
  LOIDGeneratorClass:= TtiOIDGeneratorClass(GTIOPFManager.DefaultOIDGenerator.ClassType);
  try
    GTIOPFManager.DefaultOIDGenerator:= TtiOIDGeneratorGUID.Create;
    try
      LObject:= TtiObjectForTestingOID.Create;
      try
        Check(LObject.GetOID.ClassType = GTIOPFManager.DefaultOIDGenerator.OIDClass, 'Failed on 1');
        LObject.ObjectState := posEmpty;
        Check(LObject.OID.IsNull, 'Failed on 2');
        LObject.Dirty := True;
        Check(not LObject.OID.IsNull, 'Failed on 3');
      finally
        LObject.Free;
      end;
    finally
      GTIOPFManager.DefaultOIDGenerator:= nil;
    end;
  finally
    GTIOPFManager.DefaultOIDGenerator:= LOIDGeneratorClass.Create;
  end;
end;


procedure TtiObjectTestCase.tiObject_Equals;
var
  lObj1     : TtiObjectForTesting;
  lObj2     : TtiObjectForTesting;
  lObj3     : TtiObjectForTesting;
  lObj4     : TtiObjectForTesting;
  lList1    : TtiObjectListForTesting;
  lList2    : TtiObjectListForTesting;
  lOwnedObj1 : TtiObjectWithOwnedForTesting;
  lOwnedObj2 : TtiObjectWithOwnedForTesting;
begin
  lObj1 := TtiObjectForTesting.Create;
  try
    lObj2 := TtiObjectForTesting.Create;
    try
      Check(lObj1.Equals(lObj2), 'Failed on 1');
      Check(lObj2.Equals(lObj1), 'Failed on 2');

      lObj2.BoolProp := true;
      Check(not lObj1.Equals(lObj2), 'Failed on 3');
      Check(not lObj2.Equals(lObj1), 'Failed on 4');
      lObj2.BoolProp := false;
      Check(lObj1.Equals(lObj2), 'Failed on 5');

      lObj2.IntProp  := 10;
      Check(not lObj1.Equals(lObj2), 'Failed on 6');
      Check(not lObj2.Equals(lObj1), 'Failed on 7');
      lObj2.IntProp  := 1;
      Check(lObj1.Equals(lObj2), 'Failed on 8');

      lObj2.FloatProp := 1.1111112;
      Check(not lObj1.Equals(lObj2), 'Failed on 9');
      Check(not lObj2.Equals(lObj1), 'Failed on 10');
      lObj2.FloatProp := 1.1111111;
      Check(lObj1.Equals(lObj2), 'Failed on 11');

      lObj2.StrProp  := 'testing, testing';
      Check(not lObj1.Equals(lObj2), 'Failed on 12');
      Check(not lObj2.Equals(lObj1), 'Failed on 13');
      lObj2.StrProp  := 'testing';
      Check(lObj1.Equals(lObj2), 'Failed on 14');

      lObj2.DateProp := EncodeDate(2002, 01, 01) + 1;     // StrToDate('01/01/2002') + 1;
      Check(not lObj1.Equals(lObj2), 'Failed on 15');
      Check(not lObj2.Equals(lObj1), 'Failed on 16');
      lObj2.DateProp := EncodeDate(2002, 01, 01);   // StrToDate('01/01/2002');
      Check(lObj1.Equals(lObj2), 'Failed on 17');

      lObj2.OrdProp  := tstOrdProp_2;
      Check(not lObj1.Equals(lObj2), 'Failed on 18');
      Check(not lObj2.Equals(lObj1), 'Failed on 18');
      lObj2.OrdProp  := tstOrdProp_1;
      Check(lObj1.Equals(lObj2), 'Failed on 19');

    finally
      lObj2.Free;
    end;
  finally
    lObj1.Free;
  end;

  lList1 := TtiObjectListForTesting.Create;
  try
    lList2 := TtiObjectListForTesting.Create;
    try
      Check(lList1.Equals(lList2), 'Failed on 20');
      Check(lList2.Equals(lList1), 'Failed on 21');

      lObj1 := TtiObjectForTesting.Create;
      lList1.Add(lObj1);
      Check(not lList1.Equals(lList2), 'Failed on 22');
      Check(not lList2.Equals(lList1), 'Failed on 23');

      lObj2 := TtiObjectForTesting.Create;
      lList2.Add(lObj2);
      Check(lList1.Equals(lList2), 'Failed on 24');
      Check(lList2.Equals(lList1), 'Failed on 25');

      lObj3 := TtiObjectForTesting.Create;
      lList1.Add(lObj3);
      Check(not lList1.Equals(lList2), 'Failed on 26');
      Check(not lList2.Equals(lList1), 'Failed on 27');

      lObj4 := TtiObjectForTesting.Create;
      lList2.Add(lObj4);
      Check(lList1.Equals(lList2), 'Failed on 28');
      Check(lList2.Equals(lList1), 'Failed on 29');

      lObj1.StrProp := 'hos';
      Check(not lList1.Equals(lList2), 'Failed on 30');
      Check(not lList2.Equals(lList1), 'Failed on 31');

      lObj1.Populate;
      Check(lList1.Equals(lList2), 'Failed on 32');
      Check(lList2.Equals(lList1), 'Failed on 33');

      lObj1.OID.AsString := '1';
      lObj2.OID.AsString := '2';
      Check(lList1.Equals(lList2), 'Failed on 34');
      Check(lList2.Equals(lList1), 'Failed on 35');
      lObj1.Populate;
      lObj2.Populate;

      lObj1.ObjectState := posEmpty;
      lObj2.ObjectState := posClean;
      Check(lList1.Equals(lList2), 'Failed on 36');
      Check(lList2.Equals(lList1), 'Failed on 37');

    finally
      lList2.Free;
    end;
  finally
    lList1.Free;
  end;

  lOwnedObj1 := TtiObjectWithOwnedForTesting.Create;
  try
    lOwnedObj2 := TtiObjectWithOwnedForTesting.Create;
    try
      Check(lOwnedObj1.Equals(lOwnedObj2), 'Failed on 38');
      Check(lOwnedObj2.Equals(lOwnedObj1), 'Failed on 39');

      lOwnedObj1.ObjProp.StrProp := 'HOS';
      Check(not lOwnedObj1.Equals(lOwnedObj2), 'Failed on 40');
      Check(not lOwnedObj2.Equals(lOwnedObj1), 'Failed on 41');
      lOwnedObj1.ObjProp.Populate;
      Check(lOwnedObj1.Equals(lOwnedObj2), 'Failed on 42');
      Check(lOwnedObj2.Equals(lOwnedObj1), 'Failed on 43');

    finally
      lOwnedObj2.Free;
    end;
  finally
    lOwnedObj1.Free;
  end;
end;

type
 TtiObjectForTestingDifMessage = class(TtiObjectForTesting)
 protected
   function GetCaption: string; override;
 end;

  function TtiObjectForTestingDifMessage.GetCaption: string;
  begin
    result:= ClassName + '.OID="' + OID.AsString + '"';
  end;

procedure TtiObjectTestCase.tiObject_EqualsDifMessage;
var
  LObj1     : TtiObjectForTestingDifMessage;
  LObj2     : TtiObjectForTestingDifMessage;
  LActual: string;
const
  CExpected =
    '"TtiObjectForTestingDifMessage: TtiObjectForTestingDifMessage.OID="1" <> TtiObjectForTestingDifMessage.OID="2""';
begin
  LObj1:= nil;
  LObj2:= nil;
  try
    LObj1:= TtiObjectForTestingDifMessage.Create;
    LObj1.OID.AsString:='1';
    LObj2:= TtiObjectForTestingDifMessage.Create;
    LObj2.OID.AsString:='2';
    LObj2.BoolProp := true;
    LObj1.Equals(LObj2, LActual);;
    CheckEquals(CExpected, LActual);
  finally
    LObj1.Free;
    LObj2.Free;
  end;
end;

procedure TtiObjectTestCase.FindAllWithMethod;
var
  lData : TtiObjectListForTesting;
  lList : TList;
  i : integer;
begin
  lData := CreateTestDataList;
  try
    lList := TList.Create;
    try
      lData.FindAll(TstFindAll, lList);
      CheckEquals(6, lList.Count, 'Failed on count');
      for i := 0 to 5 do
        CheckEquals(IntToStr(i), TtiObjectForTesting(lList.Items[i]).OID.AsString, 'Failed on OID');
    finally
      lList.Free;
    end;
  finally
    lData.Free;
  end;
end;


procedure TtiObjectTestCase.FindByOID;
var
  lData : TtiObjectListForTesting;
  lToFind : TtiObject;
  lFound : TtiObject;
  i : integer;
begin
  lData := CreateTestData;
  try
    lToFind := lData;
    lFound := lData.Find(lToFind.OID);
    CheckNull(lFound, 'Failed on 1');
    for i := 0 to lData.Count - 1 do
    begin
      lToFind := lData.Items[i];
      lFound := lData.Find(lToFind.OID);
      CheckSame(lToFind, lFound, 'Failed on 2');
      lToFind := (lData.Items[i] as TtiObjectWithOwnedForTesting).ObjProp;
      lFound := lData.Find(lToFind.OID);
      CheckNull(lFound, 'Failed on 3');
    end;
  finally
    lData.Free;
  end;
end;


procedure TtiObjectTestCase.FindWithMethod;
var
  lData : TtiObjectListForTesting;
  lFound : TtiObjectForTesting;
begin
  lData := CreateTestDataList;
  try
    lFound := lData.Find(TstFindMethod) as TtiObjectForTesting;
    Check(Assigned(lFound), 'Find failed');
    CheckEquals('1', lFound.OID.AsString, 'Find failed');
  finally
    lData.Free;
  end;
end;


procedure TtiObjectTestCase.TstFindMethod(AObject: TtiObject; var AFound: boolean);
begin
  AFound := false;
  if not (AObject is TtiObjectForTesting) then
    Exit; //==>
  AFound := TtiObjectForTesting(AObject).IntProp = 1;
end;


procedure TtiObjectTestCase.FindWithMethodAndParam;
var
  lData : TtiObjectListForTesting;
  lFound : TtiObjectForTesting;
begin
  lData := CreateTestDataList;
  try
    lFound := lData.Find(TstFindMethodWithParam, Pointer(1)) as TtiObjectForTesting;
    Check(Assigned(lFound), 'Find failed');
    CheckEquals('1', lFound.OID.AsString, 'Find failed');

    lFound := lData.Find(TstFindMethodWithParam, Pointer(100)) as TtiObjectForTesting;
    Check(not Assigned(lFound), 'Found when it shoud have failed');

  finally
    lData.Free;
  end;
end;


procedure TtiObjectTestCase.Index;
var
  lData : TtiObjectListForTesting;
  lItem0 : TtiObjectForTesting;
  lItem1 : TtiObjectForTesting;
  lItem2 : TtiObjectForTesting;
  lItem3: TtiObjectForTesting;
begin
  lData := TtiObjectListForTesting.Create;
  try
    lData.ObjectState := posClean;
    lItem0 := TtiObjectForTesting.Create;
    try
      lItem0.ObjectState := posClean;
      lItem0.Index;
      Check(false, 'Exception not raised');
    except
      on e:exception do
        CheckIs(e, Exception, 'Exception of the correct class not raised');
    end;
    lData.Add(lItem0);
    CheckEquals(0, lItem0.Index);
    lItem1 := TtiObjectForTesting.Create;
    lItem1.ObjectState := posClean;
    lData.Add(lItem1);
    CheckEquals(1, lItem1.Index);
    lItem2 := TtiObjectForTesting.Create;
    lItem2.ObjectState := posClean;
    lData.Insert(lItem1, lItem2);

    CheckEquals(0, lItem0.Index);
    CheckEquals(1, lItem2.Index);
    CheckEquals(2, lItem1.Index);

    lItem3 := TtiObjectForTesting.Create;
    lItem3.ObjectState := posClean;
    lData.Insert(2, lItem3);
    CheckEquals(0, lItem0.Index);
    CheckEquals(1, lItem2.Index);
    CheckEquals(2, lItem3.Index);
    CheckEquals(3, lItem1.Index);
  finally
    lData.Free;
  end;
end;


procedure TtiObjectTestCase.ObjectStateAsString;
var
  lItem : TtiOPFTestItem;
begin
  lItem := TtiOPFTestItem.Create;
  try
    CheckEquals('posEmpty', lItem.ObjectStateAsString);
    lItem.ObjectState := posPK;
    CheckEquals('posPK', lItem.ObjectStateAsString);
    lItem.ObjectState := posCreate;
    CheckEquals('posCreate', lItem.ObjectStateAsString);
    lItem.ObjectState := posUpdate;
    CheckEquals('posUpdate', lItem.ObjectStateAsString);
    lItem.ObjectState := posDelete;
    CheckEquals('posDelete', lItem.ObjectStateAsString);
    lItem.ObjectState := posDeleted;
    CheckEquals('posDeleted', lItem.ObjectStateAsString);
    lItem.ObjectState := posClean;
    CheckEquals('posClean', lItem.ObjectStateAsString);
  finally
    lItem.Free;
  end;
end;

procedure TtiObjectTestCase.OIDGenerator;
var
  LObject: TtiObjectForTestingOID;
  LOIDGeneratorClass: TtiOIDGeneratorClass;
begin
  LOIDGeneratorClass:= TtiOIDGeneratorClass(GTIOPFManager.DefaultOIDGenerator.ClassType);
  try
    GTIOPFManager.DefaultOIDGenerator:= TtiOIDGeneratorGUID.Create;
    LObject:= TtiObjectForTestingOID.Create;
    try
      CheckIs(LObject.OIDGenerator, TtiOIDGeneratorGUID);
      CheckSame(GTIOPFManager.DefaultOIDGenerator, LObject.OIDGenerator);
    finally
      LObject.Free;
    end;
  finally
    GTIOPFManager.DefaultOIDGenerator:= LOIDGeneratorClass.Create;
  end;
end;

procedure TtiObjectTestCase.Owner;
var
  lGroup : TtiObjectListNestedForTesting;
  lItem : TtiOPFTestItem;
begin
  // This is really a bit trivial, but here goes anyway...
  lGroup := TtiObjectListNestedForTesting.Create;
  try
    lItem := TtiOPFTestItem.Create;
    try
      CheckNull(lItem.Owner);
      lItem.Owner := lGroup;
      CheckNotNull(lItem.Owner);
      Check(lItem.Owner = lGroup);
      lItem.Owner := nil;
      CheckNull(lItem.Owner);
    finally
      lItem.Free;
    end;
  finally
    lGroup.Free;
  end;
end;


procedure TtiObjectTestCase.TopOfHierarchy;
var
  lData : TtiObjectListForTesting;
  lGroup : TtiObjectForTesting;
  lItem : TtiObjectWithOwnedForTesting;
begin
  lData := TtiObjectListForTesting.Create;
  try
    lGroup := TtiObjectForTesting.Create;
    lData.Add(lGroup);
    lItem := TtiObjectWithOwnedForTesting.Create;
    lData.Add(lItem);
    Check(lData.TopOfHierarchy = lData, 'Failed on 1');
    Check(lGroup.TopOfHierarchy = lData, 'Failed on 2');
    Check(lItem.TopOfHierarchy = lData, 'Failed on 3');
  finally
    lData.Free;
  end;
end;

procedure TtiObjectTestCase.IsUniqueOID;
var
  lTop : TtiObjectListForTesting;
  lItem : TtiObjectForTesting;
begin
  lTop := TtiObjectListForTesting.Create;
  try
    lTop.OID.AsString := '1';
    Check(lTop.IsUnique(lTop), 'Failed on test 1');
    lItem := TtiObjectForTesting.Create;
    lItem.OID.AsString := '1';
    Check(not lTop.IsUnique(lItem), 'Failed on test 2');
    lTop.Add(lItem);
    Check(not lTop.IsUnique(lItem), 'Failed on test 3');
    lItem := TtiObjectForTesting.Create;
    lItem.OID.AsString := '2';
    Check(lTop.IsUnique(lItem), 'Failed on test 4');
    lTop.Add(lItem);
    Check(lTop.IsUnique(lItem), 'Failed on test 5');
  finally
    lTop.Free;
  end;
end;


procedure TtiObjectTestCase.CloneCompound;
var
  lFrom : TtstPerObjOwnedObjCanAssign;
  lTo  : TtstPerObjOwnedObjCanAssign;
begin
  lFrom := TtstPerObjOwnedObjCanAssign.Create;
  try
    SetTSTPerObjAbs(lFrom, 1);
    SetTSTPerObjAbs(lFrom.ObjProp, 2);
    lTo  := lFrom.Clone;
    try
      CheckTSTPerObjAbs(lFrom, 1);
      CheckTSTPerObjAbs(lFrom.ObjProp, 2);
    finally
      lTo.Free;
    end;
  finally
    lFrom.Free;
  end;
end;


procedure TtiObjectTestCase.CloneList;
var
  lFrom : TtiObjectListForTesting;
  lItem : TtiObjectForTesting;
  lTo  : TtiObjectListForTesting;
  i: integer;
begin
  lFrom := TtiObjectListForTesting.Create;
  try
    lFrom.OwnsObjects := true;
    SetTSTPerObjList(lFrom, 1);
    lItem := TtiObjectForTesting.Create;
    SetTSTPerObjAbs(lItem, 2);
    lFrom.Add(lItem);
    lTo  := lFrom.Clone;
    try
      CheckTSTPerObjList(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      //CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
      Check(nil = lTo.Owner, 'Failed on Owner');
      CheckEquals(1, lTo.Count, 'Failed on Count');
      CheckTSTPerObjAbs(lTo.Items[0], 2);
      CheckSame(lTo, lTo.Items[0].Owner, 'Failed on Owner');
    finally
      lTo.Free;
    end;
  finally
    lFrom.Free;
  end;

  lFrom := TtiObjectListForTesting.Create;
  try
    lFrom.OwnsObjects := false;
    lFrom.AutoSetItemOwner := false;
    SetTSTPerObjList(lFrom, 1);
    lItem := TtiObjectForTesting.Create;
    SetTSTPerObjAbs(lItem, 2);
    lFrom.Add(lItem);
    lTo  := lFrom.Clone;
    try
      CheckTSTPerObjList(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      //CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
      Check(nil = lTo.Owner, 'Failed on Owner');
      CheckEquals(1, lTo.Count, 'Failed on Count');
      CheckTSTPerObjAbs(lTo.Items[0], 2);
      // This test will fail because lTo.AutoSetItemOwner must be set
      // and there is no opportunity to do this because Clone is being
      // called.
      // CheckSame(lFrom, lTo.Items[0].Owner, 'Failed on Owner');
    finally
      lTo.Free;
    end;
    for i:= lFrom.Count-1 downto 0 do
      lFrom.Items[i].Free;
  finally
    lFrom.Free;
  end;
end;


procedure TtiObjectTestCase.TstFindMethodWithParam(AObject: TtiObject;
  var AFound: boolean; AUserContext: Pointer);
begin
  AFound := false;
  if not (AObject is TtiObjectForTesting) then
    Exit; //==>
  AFound := TtiObjectForTesting(AObject).IntProp = Integer(AUserContext);
end;


procedure TtiObjectTestCase.TstFindAll(AObject : TtiObject; var AFound : boolean);
begin
  AFound := false;
  if not (AObject is TtiObjectForTesting) then
    Exit; //==>
  AFound := TtiObjectForTesting(AObject).IntProp <= 5;
end;


procedure TtiObjectTestCase.IsValid;
var
  lData : TtiObjectForTesting;
  lErrors : TtiObjectErrors;
begin
  lData := TtiObjectForTesting.Create;
  try
    lData.StrProp := '';
    lData.IntProp := 0;
    lData.FloatProp := 0;
    lErrors := TtiObjectErrors.Create;
    try
      Check(not lData.IsValid, 'Valid passed when it should have failed');
      lData.IsValid(lErrors);
      CheckEquals(3, lErrors.Count, 'Wrong number of errors returned');
      CheckEquals(cStrPropErrorMsg,    lErrors.Items[0].ErrorMessage, 'Failed on test 1');
      CheckEquals(cStrPropErrorCode,   lErrors.Items[0].ErrorCode, 'Failed on test 2');
      CheckEquals(cIntPropErrorMsg,    lErrors.Items[1].ErrorMessage, 'Failed on test 3');
      CheckEquals(cIntpropErrorCode,   lErrors.Items[1].ErrorCode, 'Failed on test 4');
      CheckEquals(cFloatPropErrorMsg,  lErrors.Items[2].ErrorMessage, 'Failed on test 5');
      CheckEquals(cFloatPropErrorCode, lErrors.Items[2].ErrorCode, 'Failed on test 6');

      CheckNotNull( lErrors.FindByMessage(cStrPropErrorMsg));
      CheckEquals(cStrPropErrorMsg, lErrors.FindByMessage(cStrPropErrorMsg).ErrorMessage);
      CheckNotNull( lErrors.FindByMessage(cIntPropErrorMsg));
      CheckEquals(cIntPropErrorMsg, lErrors.FindByMessage(cIntPropErrorMsg).ErrorMessage);
      CheckNotNull( lErrors.FindByMessage(cFloatPropErrorMsg));
      CheckEquals(cFloatPropErrorMsg, lErrors.FindByMessage(cFloatPropErrorMsg).ErrorMessage);

      lData.StrProp := 'test';
      lData.IntProp := 1;
      lData.FloatProp := 1.1;
      Check(lData.IsValid, 'Valid failed when it should have passed');
      lData.IsValid(lErrors);
      CheckEquals(0, lErrors.Count, 'Wrong number of errors returned');
    finally
      lErrors.Free;
    end;

  finally
    lData.Free;
  end;
end;

procedure TtiObjectTestCase.SetBooleanPropValue;
var
  lData : TtiObjectForTesting;
begin
  lData := TtiObjectForTesting.Create;
  try
    lData.PropValue['BoolProp']:= true;
    CheckEquals(true, lData.BoolProp, 'failed on 1');
    lData.PropValue['BoolProp']:= false;
    CheckEquals(false, lData.BoolProp, 'failed on 2');

    lData.PropValue['BoolProp']:= 'true';
    CheckEquals(true, lData.BoolProp, 'failed on 3');
    lData.PropValue['BoolProp']:= 'false';
    CheckEquals(false, lData.BoolProp, 'failed on 4');

    lData.PropValue['BoolProp']:= 'TRUE';
    CheckEquals(true, lData.BoolProp, 'failed on 5');
    lData.PropValue['BoolProp']:= 'FALSE';
    CheckEquals(false, lData.BoolProp, 'failed on 6');

    lData.PropValue['BoolProp']:= 'True';
    CheckEquals(true, lData.BoolProp, 'failed on 7');
    lData.PropValue['BoolProp']:= 'False';
    CheckEquals(false, lData.BoolProp, 'failed on 8');

    lData.PropValue['BoolProp']:= 'T';
    CheckEquals(true, lData.BoolProp, 'failed on 9');
    lData.PropValue['BoolProp']:= 'F';
    CheckEquals(false, lData.BoolProp, 'failed on 10');

    lData.PropValue['BoolProp']:= 1;
    CheckEquals(true, lData.BoolProp, 'failed on 11');
    lData.PropValue['BoolProp']:= 0;
    CheckEquals(false, lData.BoolProp, 'failed on 12');

    lData.PropValue['BoolProp']:= '1';
    CheckEquals(true, lData.BoolProp, 'failed on 13');
    lData.PropValue['BoolProp']:= '0';
    CheckEquals(false, lData.BoolProp, 'failed on 14');
  finally
    lData.Free;
  end;
end;

const
  CTestPropValOID = '{8DE032BE-1BE2-4F69-A894-334B023E5DF9}';
  CTestPropValString   = 'test';
  CTestPropValInteger  = 1234;
  CTestPropValFloat    = 1234.5678;
  CTestPropValBoolean  = True;
  CTestPropValOrd      = tstOrdProp_2;
  CTestPropValDateTime = 37622.4274305556; // EncodeDateTime(2003, 01, 01, 10, 15, 30, 00)

procedure TtiObjectTestCase.SetPropValue;
var
  lData : TtiObjectForTesting;
begin
  lData := TtiObjectForTesting.Create;
  try
    lData.PropValue['OID']:= CTestPropValOID;
    CheckEquals(CTestPropValOID, lData.OID.AsString, 'OID');

    lData.PropValue['StrProp']:= CTestPropValString;
    CheckEquals(CTestPropValString, lData.StrProp, 'StrProp');

    lData.PropValue['IntProp']:= CTestPropValInteger;
    CheckEquals(CTestPropValInteger, lData.IntProp, 'IntProp');

    lData.PropValue['FloatProp']:= CTestPropValFloat;
    CheckEquals(CTestPropValFloat, lData.FloatProp, cDUnitTestFloatPrecision, 'FloatProp');

    lData.PropValue['DateProp']:= CTestPropValDateTime;
    CheckNearEnough(CTestPropValDateTime, lData.DateProp, 'DateProp');

    lData.PropValue['BoolProp']:= CTestPropValBoolean;
    CheckEquals(CTestPropValBoolean, lData.BoolProp, 'BoolProp true');

    lData.PropValue['OrdProp']:= CTestPropValOrd;
    Check(CTestPropValOrd = lData.OrdProp, 'OrdProp');

  finally
    lData.Free;
  end;
end;

procedure TtiObjectTestCase.SetPropValueNested;
var
  LData : TtiObjectWithOwnedForTesting;
begin
  LData := TtiObjectWithOwnedForTesting.Create;
  try
    LData.PropValue['OID']:= CTestPropValOID;
    LData.PropValue['StrProp']   := CTestPropValString;
    LData.PropValue['IntProp']   := CTestPropValInteger;
    LData.PropValue['FloatProp'] := CTestPropValFloat;
    LData.PropValue['DateProp']  := CTestPropValDateTime;
    LData.PropValue['BoolProp']  := CTestPropValBoolean;
    LData.PropValue['OrdProp']   := CTestPropValOrd;

    LData.PropValue['ObjProp.OID']:= CTestPropValOID;
    LData.PropValue['ObjProp.StrProp']   := CTestPropValString;
    LData.PropValue['ObjProp.IntProp']   := CTestPropValInteger;
    LData.PropValue['ObjProp.FloatProp'] := CTestPropValFloat;
    LData.PropValue['ObjProp.DateProp']  := CTestPropValDateTime;
    LData.PropValue['ObjProp.BoolProp']  := CTestPropValBoolean;
    LData.PropValue['ObjProp.OrdProp']   := CTestPropValOrd;

    CheckEquals(CTestPropValOID,      LData.OID.AsString, 'OID');
    CheckEquals(CTestPropValString,   LData.StrProp, 'StrProp');
    CheckEquals(CTestPropValInteger,  LData.IntProp, 'IntProp');
    CheckEquals(CTestPropValFloat,    LData.FloatProp, cDUnitTestFloatPrecision, 'FloatProp');
    CheckEquals(CTestPropValDateTime, LData.DateProp, 0.00001, 'DateProp');
    CheckEquals(CTestPropValBoolean,  LData.BoolProp, 'BoolProp true');
    Check(CTestPropValOrd = LData.OrdProp, 'OrdProp');

// ToDo: OID will have to be published for this to work, but that will have side effects.
//       Fix, but with care
//    CheckEquals(CTestPropValOID,      LData.ObjProp.OID.AsString, 'OID');
    CheckEquals('',                   LData.ObjProp.OID.AsString, 'OID');
    CheckEquals(CTestPropValString,   LData.ObjProp.StrProp, 'StrProp');
    CheckEquals(CTestPropValInteger,  LData.ObjProp.IntProp, 'IntProp');
    CheckEquals(CTestPropValFloat,    LData.ObjProp.FloatProp, cDUnitTestFloatPrecision, 'FloatProp');
    CheckEquals(CTestPropValDateTime, LData.ObjProp.DateProp, 0.00001, 'DateProp');
    CheckEquals(CTestPropValBoolean,  LData.ObjProp.BoolProp, 'BoolProp true');
    Check(CTestPropValOrd = LData.ObjProp.OrdProp, 'OrdProp');

  finally
    LData.Free;
  end;
end;

procedure TtiObjectTestCase.GetOID;
var
  LObject: TtiObjectForTestingOID;
  LOIDGeneratorClass: TtiOIDGeneratorClass;
begin
  LOIDGeneratorClass:= TtiOIDGeneratorClass(GTIOPFManager.DefaultOIDGenerator.ClassType);
  try
    GTIOPFManager.DefaultOIDGenerator:= TtiOIDGeneratorGUID.Create;
    try
      LObject:= TtiObjectForTestingOID.Create;
      try
        Check(LObject.GetOID.ClassType = GTIOPFManager.DefaultOIDGenerator.OIDClass);
      finally
        LObject.Free;
      end;
    finally
      GTIOPFManager.DefaultOIDGenerator:= nil;
    end;

    InhibitStackTrace;
    LObject:= TtiObjectForTestingOID.Create;
    try
      try
        LObject.GetOID;
        Fail('Exception not raised');
      except
        on e:exception do
        begin
          CheckIs(e, EtiOPFProgrammerException);
          CheckEquals(CErrorDefaultOIDGeneratorNotAssigned, e.message);
        end;
      end;
    finally
      LObject.Free;
    end;
  finally
    GTIOPFManager.DefaultOIDGenerator:= LOIDGeneratorClass.Create;
  end;
end;

procedure TtiObjectTestCase.GetPropValue;
var
  LData : TtiObjectForTesting;
  LOrd : String;
begin
  LData := TtiObjectForTesting.Create;
  try
    LData.OID.AsString:= CTestPropValOID;
    LData.StrProp := CTestPropValString;
    LData.IntProp := CTestPropValInteger;
    LData.FloatProp := CTestPropValFloat;
    LData.DateProp := CTestPropValDateTime;
    LData.BoolProp := CTestPropValBoolean;
    LData.OrdProp := CTestPropValOrd;

    CheckEquals(CTestPropValOID, LData.PropValue['OID'], 'OID');
    CheckEquals(CTestPropValString, LData.PropValue['StrProp'], 'StrProp');
    CheckEquals(CTestPropValInteger, LData.PropValue['IntProp'], 'IntProp');
    CheckEquals(CTestPropValFloat, LData.PropValue['FloatProp'], cDUnitTestFloatPrecision, 'FloatProp');
    CheckNearEnough(CTestPropValDateTime, LData.PropValue['DateProp'], 'DateProp');
    CheckEquals(CTestPropValBoolean, LData.PropValue['BoolProp'], 'BoolProp');

    // This is a bit of a mess, but it was the
    // best I could come up with to get PropValue working
    // with ordinals.
    LOrd := LData.PropValue['OrdProp'];
    CheckEquals(LOrd,
      GetEnumName(TypeInfo(TtstOrdProp), ord(CTestPropValOrd)),
      'OrdProp');

  finally
    LData.Free;
  end;
end;


procedure TtiObjectTestCase.GetPropValueNested;
var
  LData : TtiObjectWithOwnedForTesting;
  LOrd: String;
begin
  LData := TtiObjectWithOwnedForTesting.Create;
  try
    LData.OID.AsString:= CTestPropValOID;
    LData.StrProp := CTestPropValString;
    LData.IntProp := CTestPropValInteger;
    LData.FloatProp := CTestPropValFloat;
    LData.DateProp := CTestPropValDateTime;
    LData.BoolProp := CTestPropValBoolean;

    LData.ObjProp.OID.AsString:= CTestPropValOID;
    LData.ObjProp.StrProp := CTestPropValString;
    LData.ObjProp.IntProp := CTestPropValInteger;
    LData.ObjProp.FloatProp := CTestPropValFloat;
    LData.ObjProp.DateProp := CTestPropValDateTime;
    LData.ObjProp.BoolProp := CTestPropValBoolean;

    CheckEquals(CTestPropValOID, LData.PropValue['OID'], 'OID');
    CheckEquals(CTestPropValString, LData.PropValue['StrProp'], 'StrProp');
    CheckEquals(CTestPropValInteger, LData.PropValue['IntProp'], 'IntProp');
    CheckEquals(CTestPropValFloat, LData.PropValue['FloatProp'], cDUnitTestFloatPrecision, 'FloatProp');
    CheckNearEnough(CTestPropValDateTime, LData.PropValue['DateProp'], 'DateProp');
    CheckEquals(CTestPropValBoolean, LData.PropValue['BoolProp'], 'BoolProp');

    // This is a bit of a mess, but it was the
    // best I could come up with to get PropValue working
    // with ordinals.
    LData.OrdProp := CTestPropValOrd;
    LOrd := LData.PropValue['OrdProp'];
    CheckEquals(LOrd,
      GetEnumName(TypeInfo(TtstOrdProp), ord(CTestPropValOrd)),
      'OrdProp');

    CheckEquals(CTestPropValOID, LData.ObjProp.PropValue['OID'], 'OID');
    CheckEquals(CTestPropValString, LData.ObjProp.PropValue['StrProp'], 'StrProp');
    CheckEquals(CTestPropValInteger, LData.ObjProp.PropValue['IntProp'], 'IntProp');
    CheckEquals(CTestPropValFloat, LData.ObjProp.PropValue['FloatProp'], cDUnitTestFloatPrecision, 'FloatProp');
    CheckNearEnough(CTestPropValDateTime, LData.ObjProp.PropValue['DateProp'], 'DateProp');
    CheckEquals(CTestPropValBoolean, LData.ObjProp.PropValue['BoolProp'], 'BoolProp');

    // This is a bit of a mess, but it was the
    // best I could come up with to get PropValue working
    // with ordinals.
    LData.ObjProp.OrdProp := CTestPropValOrd;
    LOrd := LData.ObjProp.PropValue['OrdProp'];
    CheckEquals(LOrd,
      GetEnumName(TypeInfo(TtstOrdProp), ord(CTestPropValOrd)),
      'OrdProp');

  finally
    LData.Free;
  end;
end;

procedure TtiObjectTestCase.IsReadWriteProp;
var
  lData : TtiObjectForTesting;
begin
  lData := TtiObjectForTesting.Create;
  try
    Check(lData.IsReadWriteProp('StrProp'),   'StrProp');
    Check(lData.IsReadWriteProp('IntProp'),   'IntProp');
    Check(lData.IsReadWriteProp('FloatProp'), 'FloatProp');
    Check(lData.IsReadWriteProp('DateProp'),  'DateProp');
    Check(lData.IsReadWriteProp('BoolProp'),  'BoolProp');
    Check(lData.IsReadWriteProp('OrdProp'),   'OrdProp');
    Check(lData.IsReadWriteProp('OID'),       'OID');
    Check(not lData.IsReadWriteProp('StrPropReadOnly'), 'StrPropReadOnly');
  finally
    lData.Free;
  end;
end;


type
  TtiFieldTest = class(TtiObject)
  private
    FFloatField: TtiFieldFloat;
    FStrField: TtiFieldString;
  public
    constructor Create; override;
    destructor  Destroy; override;
  published
    property    StrField: TtiFieldString read FStrField;
    property    FloatField: TtiFieldFloat read FFloatField;
  end;


procedure TtiObjectTestCase.FieldName;
var
  lObj: TtiFieldTest;
begin
  lObj:= TtiFieldTest.Create;
  try
    CheckEquals('StrField', lObj.StrField.FieldName, '#1');
    CheckEquals('FloatField', lObj.FloatField.FieldName, '#2');
  finally
    lObj.Free;
  end;
end;


procedure TtiObjectTestCase.FieldBoolean;
var
  lPerObj: TtiObject;
  lField:  TtiFieldBoolean;
begin
  lPerObj := TtiObject.Create;
  try
    lField:= TtiFieldBoolean.Create(lPerObj);
    try
      Check(lField.IsNull, 'IsNull #1');

      lField.AsString := cTrueDB;
      CheckEquals(cTrueDB, lField.AsString, 'AsString #1');
      CheckEquals(True, lField.AsBoolean, 'AsBoolean #1');
      Check(not lField.IsNull, 'IsNull #2');

      lField.AsString := '';
      CheckEquals(cFalseDB, lField.AsString, 'AsString #2');
      CheckEquals(false, lField.AsBoolean, 'AsBoolean #2');
      Check(lField.IsNull, 'IsNull #2');

      lField.AsBoolean := False;
      CheckEquals(cFalseDB, lField.AsString, 'AsString #3');
      CheckEquals(false, lField.AsBoolean,  'AsBoolean #3');
      Check(not lField.IsNull, 'IsNull #3');

      lField.IsNull := True;
      CheckEquals(cFalseDB, lField.AsString, 'AsString #4');
      CheckEquals(false, lField.AsBoolean, 'AsBoolean #4');
      Check(lField.IsNull, 'IsNull #4');
    finally
      lField.Free;
    end;
  finally
    lPerObj.Free;
  end;
end;


procedure TtiObjectTestCase.FieldDate;
var
  LObj: TtiObject;
  LField:  TtiFieldDate;
  LValue   : TDateTime;
  LValueStr : string;
begin

  LObj := TtiObject.Create;
  try
    LField:= TtiFieldDate.Create(LObj);
    try
      Check(LField.IsNull, 'IsNull #1');

      LValue   := EncodeDate(2004, 06, 03);
      LValueStr := tiDateAsXMLString(LValue);

      LField.AsString := LValueStr;
      CheckEquals(LValueStr, LField.AsString, 'AsString #1');
      CheckEquals(LValue, LField.AsDateTime, 0.0001, 'AsDateTime #1');
      Check(not LField.IsNull, 'IsNull #1');

      LField.AsString := '';
      CheckEquals('30/12/1899', LField.AsString, 'AsString #2');
      CheckEquals(0, LField.AsDateTime, 0.0001, 'AsDateTime #2');
      Check(LField.IsNull, 'IsNull #2');

      LField.AsDateTime := LValue;
      CheckEquals(LValueStr, LField.AsString, 'AsString #3');
      CheckEquals(LValue, LField.AsDateTime, 0.0001, 'AsDateTime #3');
      Check(not LField.IsNull, 'IsNull #3');

      LField.IsNull := true;
      CheckEquals('30/12/1899', LField.AsString, 'AsString #4');
      CheckEquals(0, LField.AsDateTime, 0.0001, 'AsDateTime #4');
      Check(LField.IsNull, 'IsNull #4');
    finally
      LField.Free;
    end;
  finally
    LObj.Free;
  end;
end;


procedure TtiObjectTestCase.FieldDateTime;
var
  lPerObj: TtiObject;
  lField:  TtiFieldDateTime;
  lValue   : TDateTime;
  lValueStr : string;
begin
  lValue   := EncodeDateTime(2004, 06, 03, 13, 45, 20, 00);
  lValueStr := tiDateTimeAsXMLString(lValue);

  lPerObj := TtiObject.Create;
  try
    lField:= TtiFieldDateTime.Create(lPerObj);
    try
      Check(lField.IsNull, 'IsNull #1');

      lField.AsString := lValueStr;
      CheckEquals(lValueStr, lField.AsString, 'AsString #2');
      CheckEquals(lValue, lField.AsDateTime, 0.0001, 'AsDateTime #2');
      Check(not lField.IsNull, 'IsNull #2');

      lField.AsString := '';
      CheckEquals('30/12/1899 00:00:00:000', lField.AsString, 'AsString #3');
      CheckEquals(0, lField.AsDateTime, 0.0001, 'AsDateTime #3');
      Check(not lField.IsNull, 'IsNull #3');

      lField.AsDateTime := lValue;
      CheckEquals(lValueStr, lField.AsString, 'AsString #4');
      CheckEquals(lValue, lField.AsDateTime, 0.0001, 'AsDateTime #4');
      Check(not lField.IsNull, 'IsNull #4');

      lField.IsNull := true;
      CheckEquals('30/12/1899 00:00:00:000', lField.AsString, 'AsString #5');
      CheckEquals(0, lField.AsDateTime, 0.0001, 'AsDateTime #5');
      Check(lField.IsNull, 'IsNull #5');
    finally
      lField.Free;
    end;
  finally
    lPerObj.Free;
  end;
end;

procedure TtiObjectTestCase.FieldDateTime_Assign;
var
  lF1: TtiFieldDateTime;
  lF2: TtiFieldDateTime;
begin
  lF1:= TtiFieldDateTime.Create(nil);
  try
    lF2:= TtiFieldDateTime.Create(nil);
    try
      lF1.AsDateTime := EncodeDate(2006, 01, 01);
      lF2.AsDateTime := EncodeDate(2006, 01, 02);
      CheckNotEquals(LF1.AsDateTime, LF2.AsDateTime);
      lF2.Assign(LF1);
      CheckEquals(LF1.AsDateTime, LF2.AsDateTime);
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldDateTime_Equals;
var
  lF1: TtiFieldDateTime;
  lF2: TtiFieldDateTime;
begin
  lF1:= TtiFieldDateTime.Create(nil);
  try
    lF2:= TtiFieldDateTime.Create(nil);
    try
      lF1.AsDateTime := EncodeDate(2005, 01, 01);
      lF2.AsDateTime := EncodeDate(2005, 01, 01);
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsDateTime := EncodeDate(2005, 01, 02);
      Check(not lF1.Equals(lF2));
      Check(not lF2.Equals(lF1));
      lF1.AsDateTime := EncodeDate(2005, 01, 01) + EncodeTime(10, 15, 30, 00);
      lF2.AsDateTime := EncodeDate(2005, 01, 01) + EncodeTime(10, 15, 30, 00);
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsDateTime := EncodeDate(2005, 01, 01) + EncodeTime(10, 15, 30, 10);
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsDateTime := EncodeDate(2005, 01, 01) + EncodeTime(10, 15, 31, 00);
      Check(not lF1.Equals(lF2));
      Check(not lF2.Equals(lF1));
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldDateTime_HoursMinutesSeconds;
var
  lPerObj: TtiObject;
  lField:  TtiFieldDateTime;
  lValue   : TDateTime;
  lValueStr : string;
begin
  lValue   := EncodeDate(2004, 06, 03) + EncodeTime(13, 45, 20, 00);
  lValueStr := tiDateTimeAsXMLString(lValue);

  lPerObj := TtiObject.Create;
  try
    lField:= TtiFieldDateTime.Create(lPerObj);
    try
      lField.AsDateTime := lValue;
      CheckEquals(13, lField.Hours, 'Failed on 1');
      CheckEquals(45, lField.Minutes, 'Failed on 2');
      CheckEquals(20, lField.Seconds, 'Failed on 3');
      Check(not lField.IsNull, 'Failed on 4');

      lField.IsNull := True;
      CheckEquals(0, lField.Hours, 'Failed on 5');
      CheckEquals(0, lField.Minutes, 'Failed on 6');
      CheckEquals(0, lField.Seconds, 'Failed on 7');
      Check(lField.IsNull, 'Failed on 8');
    finally
      lField.Free;
    end;
  finally
    lPerObj.Free;
  end;
end;

procedure TtiObjectTestCase.FieldDateTime_YearsMonthsDays;
var
  lPerObj: TtiObject;
  lField:  TtiFieldDateTime;
  lValue   : TDateTime;
  lValueStr : string;
begin
  lValue   := EncodeDate(2004, 06, 03) + EncodeTime(13, 45, 20, 00);
  lValueStr := tiDateTimeAsXMLString(lValue);

  lPerObj := TtiObject.Create;
  try
    lField:= TtiFieldDateTime.Create(lPerObj);
    try
      lField.AsDateTime := lValue;
      CheckEquals(2004, lField.Years, 'Failed on 1');
      CheckEquals(6, lField.Months, 'Failed on 2');
      CheckEquals(3, lField.Days, 'Failed on 3');
      Check(not lField.IsNull, 'Failed on 4');

      lField.IsNull := True;
      CheckEquals(0, lField.Years, 'Failed on 5');
      CheckEquals(0, lField.Months, 'Failed on 6');
      CheckEquals(0, lField.Days, 'Failed on 7');
      Check(lField.IsNull, 'Failed on 8');
    finally
      lField.Free;
    end;
  finally
    lPerObj.Free;
  end;
end;

procedure TtiObjectTestCase.FieldFloat;
var
  lPerObj: TtiObject;
  lField:  TtiFieldFloat;
const
  cValue    = 1234.456;
  cValueStr = '1234.456';
begin
  lPerObj := TtiObject.Create;
  try
    lField:= TtiFieldFloat.Create(lPerObj);
    try
      Check(lField.IsNull, 'IsNull #1');

      lField.AsString := cValueStr;
      CheckEquals(cValueStr, lField.AsString, 'AsString #1');
      CheckEquals(cValue, lField.AsFloat, 0.0001, 'AsFloat #1');
      Check(not lField.IsNull, 'IsNull #2');

      lField.AsString := '';
      CheckEquals('0', lField.AsString, 'AsString #2');
      CheckEquals(0, lField.AsFloat, 0.0001, 'AsFloat #2');
      Check(not lField.IsNull, 'IsNull #3');

      lField.AsFloat := cValue;
      CheckEquals(cValueStr, lField.AsString, 'AsString #3');
      CheckEquals(cValue, lField.AsFloat, 0.0001, 'AsFloat #2');
      Check(not lField.IsNull, 'IsNull #4');

      lField.IsNull := true;
      CheckEquals('0', lField.AsString, 'AsString #2');
      CheckEquals(0, lField.AsFloat, 0.0001, 'AsFloat #2');
      Check(lField.IsNull, 'IsNull #3');

      lField.Precision := 1;
      lField.AsFloat := 123.456;
      CheckEquals('123.5', lField.AsString, '#4');

      lField.Precision := 2;
      lField.AsFloat := 123.456;
      CheckEquals('123.46', lField.AsString, '#5');

      lField.Precision := 3;
      lField.AsFloat := 123.456;
      CheckEquals('123.456', lField.AsString, '#6');

      lField.Precision := 4;
      lField.AsFloat := 123.456;
      CheckEquals('123.4560', lField.AsString, '#7');

      lField.Precision := 5;
      lField.AsFloat := 123.456;
      CheckEquals('123.45600', lField.AsString, '#8');

      lField.Precision := 6;
      lField.AsFloat := 123.456;
      CheckEquals('123.456000', lField.AsString, '#9');

      lField.Precision := 6;
      lField.AsFloat := 123.45600001;
      CheckEquals('123.456000', lField.AsString, '#10');

      lField.Precision := 2;
      lField.AsFloat := 123.455;
      CheckEquals('123.46', lField.AsString, '#11');

      lField.Precision := 2;
      lField.AsFloat := 123.454;
      CheckEquals('123.45', lField.AsString, '#11');
    finally
      lField.Free;
    end;
  finally
    lPerObj.Free;
  end;
end;


procedure TtiObjectTestCase.FieldInt64;
var
  lPerObj: TtiObject;
  lField:  TtiFieldInteger;
const
  cValue    = 1234;
  cValueStr = '1234';
begin
  lPerObj := TtiObject.Create;
  try
    lField:= TtiFieldInteger.Create(lPerObj);
    try
      Check(lField.IsNull, 'IsNull #1');

      lField.AsString := cValueStr;
      CheckEquals(cValueStr, lField.AsString, 'AsString #1');
      CheckEquals(cValue, lField.AsInteger, 'AsInteger #1');
      Check(not lField.IsNull, 'IsNull #2');

      lField.AsString := '';
      CheckEquals('0', lField.AsString, 'AsString #2');
      CheckEquals(0, lField.AsInteger, 'AsInteger #2');
      Check(not lField.IsNull, 'IsNull #3');

      lField.AsInteger := cValue;
      CheckEquals(cValueStr, lField.AsString, 'AsString #3');
      CheckEquals(cValue, lField.AsInteger, 'AsInteger #2');
      Check(not lField.IsNull, 'IsNull #4');

      lField.IsNull := true;
      CheckEquals('', lField.AsString, 'AsString #2');
      CheckEquals(0, lField.AsInteger, 'AsInteger #2');
      Check(lField.IsNull, 'IsNull #3');
    finally
      lField.Free;
    end;
  finally
    lPerObj.Free;
  end;
end;


procedure TtiObjectTestCase.FieldString;
var
  lPerObj: TtiObject;
  lField:  TtiFieldString;
const
  cValue = 'A test string value';
begin
  lPerObj := TtiObject.Create;
  try
    lField:= TtiFieldString.Create(lPerObj);
    try
      Check(lField.IsNull, 'IsNull #1');
      lField.AsString := cValue;
      CheckEquals(cValue, lField.AsString, 'AsString #1');
      Check(not lField.IsNull, 'IsNull #2');
      lField.AsString := '';
      CheckEquals('', lField.AsString, 'AsString #2');
      Check(not lField.IsNull, 'IsNull #3');
      lField.IsNull := true;
      CheckEquals('', lField.AsString, 'AsString #2');
      Check(lField.IsNull, 'IsNull #3');
    finally
      lField.Free;
    end;
  finally
    lPerObj.Free;
  end;
end;


procedure TtiObjectTestCase.PropType;
var
  lObj : TTestGetPropNames;
begin
  lObj := TTestGetPropNames.Create;
  try
    Check(lObj.PropType('StringProp')      = tiTKString, 'Failed on StringProp');
    Check(lObj.PropType('ShortStringProp') = tiTKString, 'Failed on ShortStringProp');
    Check(lObj.PropType('WideStringProp')  = tiTKString, 'Failed on WideStringProp');
    Check(lObj.PropType('CharProp')        = tiTKString, 'Failed on CharProp');
    Check(lObj.PropType('WideCharProp')    = tiTKString, 'Failed on WideCharProp');

    Check(lObj.PropType('IntProp')         = tiTKInteger, 'Failed on IntProp');
    Check(lObj.PropType('Int64Prop')       = tiTKInteger, 'Failed on Int64Prop');
    Check(lObj.PropType('BoolProp')        = tiTKBoolean, 'Failed on BoolProp');

    Check(lObj.PropType('FloatProp')       = tiTKFloat, 'Failed on FloatProp');
    Check(lObj.PropType('DateTimeProp')    = tiTKDateTime, 'Failed on DateTimeProp');

    try
      lObj.PropType('ObjectProp');
      Check(false, 'Failed on ObjectProp');
    except
      on e:exception do
        CheckIs(e, Exception, 'Failed on ObjectProp');
    end;

    {$IFNDEF FPC}
    FExecControl.InhibitStackTrace := True;
    {$ENDIF}
    try
      lObj.PropType('MethodProp');
      Check(false, 'Failed on MethodProp');
    except
      on e:exception do
        CheckIs(e, Exception, 'Failed on MethodProp');
    end;
  finally
    lObj.Free;
  end;
end;


procedure TtiObjectTestCase.FieldBoolean_Assign;
var
  lF1: TtiFieldBoolean;
  lF2: TtiFieldBoolean;
begin
  lF1:= TtiFieldBoolean.Create(nil);
  try
    lF2:= TtiFieldBoolean.Create(nil);
    try
      lF1.AsBoolean := False;
      lF2.AsBoolean := True;
      CheckNotEquals(LF1.AsBoolean, LF2.AsBoolean);
      lF2.Assign(LF1);
      CheckEquals(LF1.AsBoolean, LF2.AsBoolean);
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldBoolean_Equals;
var
  lF1: TtiFieldBoolean;
  lF2: TtiFieldBoolean;
begin
  lF1:= TtiFieldBoolean.Create(nil);
  try
    lF2:= TtiFieldBoolean.Create(nil);
    try
      lF1.AsBoolean := True;
      lF2.AsBoolean := True;
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsBoolean := False;
      Check(not lF1.Equals(lF2));
      Check(not lF2.Equals(lF1));
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;


procedure TtiObjectTestCase.FieldCurrency;
var
  LObj: TtiObject;
  LField:  TtiFieldCurrency;
const
  CValue    = 1234.56;
  CValueStr = '1234.56';
  CValueInt = 123456;
  CValueCurrencyStr = '$ 1,234.56';
begin
  LObj := TtiObject.Create;
  try
    LField:= TtiFieldCurrency.Create(LObj);
    try
      Check(LField.IsNull, 'IsNull #1');

      LField.AsString := CValueStr;
      CheckEquals(CValueStr, LField.AsString, 'AsString #1');
      CheckEquals(CValue, LField.AsFloat, 0.0001, 'AsFloat #1');
      CheckEquals(CValueInt, LField.AsInteger, 'AsInteger #1');
      CheckEquals(CValueCurrencyStr, LField.AsCurrencyString, 'AsCurrencyStr #1');
      Check(not LField.IsNull, 'IsNull #1');

      LField.AsString := '';
      CheckEquals('', LField.AsString, 'AsString #2');
      CheckEquals(0, LField.AsFloat, 0.0001, 'AsFloat #2');
      CheckEquals(0, LField.AsInteger, 'AsInteger #2');
      CheckEquals('', LField.AsCurrencyString, 'AsCurrencyStr #2');
      Check(LField.IsNull, 'IsNull #2');

      LField.AsFloat := CValue;
      CheckEquals(CValueStr, LField.AsString, 'AsString #3');
      CheckEquals(CValue, LField.AsFloat, 0.0001, 'AsFloat #3');
      CheckEquals(CValueInt, LField.AsInteger, 'AsInteger #3');
      CheckEquals(CValueCurrencyStr, LField.AsCurrencyString, 'AsCurrencyStr #3');
      Check(not LField.IsNull, 'IsNull #3');

      LField.IsNull:= True;
      CheckEquals('', LField.AsString, 'AsString #4');
      CheckEquals(0, LField.AsFloat, 0.0001, 'AsFloat #4');
      CheckEquals(0, LField.AsInteger, 'AsInteger #4');
      CheckEquals('', LField.AsCurrencyString, 'AsCurrencyStr #4');
      Check(LField.IsNull, 'IsNull #4');

      LField.AsInteger := CValueInt;
      CheckEquals(CValueStr, LField.AsString, 'AsString #5');
      CheckEquals(CValue, LField.AsFloat, 0.0001, 'AsFloat #5');
      CheckEquals(CValueInt, LField.AsInteger, 'AsInteger #5');
      CheckEquals(CValueCurrencyStr, LField.AsCurrencyString, 'AsCurrencyStr #5');
      Check(not LField.IsNull, 'IsNull #5');

      LField.IsNull:= True;
      CheckEquals('', LField.AsString, 'AsString #6');
      CheckEquals(0, LField.AsFloat, 0.0001, 'AsFloat #6');
      CheckEquals(0, LField.AsInteger, 'AsInteger #6');
      CheckEquals('', LField.AsCurrencyString, 'AsCurrencyStr #6');
      Check(LField.IsNull, 'IsNull #6');

      LField.AsCurrencyString := CValueCurrencyStr;
      CheckEquals(CValueStr, LField.AsString, 'AsString #7');
      CheckEquals(CValue, LField.AsFloat, 0.0001, 'AsFloat #7');
      CheckEquals(CValueInt, LField.AsInteger, 'AsInteger #7');
      CheckEquals(CValueCurrencyStr, LField.AsCurrencyString, 'AsCurrencyStr #7');
      Check(not LField.IsNull, 'IsNull #7');

      LField.IsNull:= True;
      CheckEquals('', LField.AsString, 'AsString #8');
      CheckEquals(0, LField.AsFloat, 0.0001, 'AsFloat #8');
      CheckEquals(0, LField.AsInteger, 'AsInteger #8');
      CheckEquals('', LField.AsCurrencyString, 'AsCurrencyStr #8');
      Check(LField.IsNull, 'IsNull #2');

    finally
      LField.Free;
    end;
  finally
    LObj.Free;
  end;
end;

procedure TtiObjectTestCase.FieldCurrency_Assign;
var
  lF1: TtiFieldCurrency;
  lF2: TtiFieldCurrency;
begin
  lF1:= TtiFieldCurrency.Create(nil);
  try
    lF2:= TtiFieldCurrency.Create(nil);
    try
      lF1.AsInteger := 110;
      lF2.AsInteger := 220;
      CheckNotEquals(LF1.AsInteger, LF2.AsInteger);
      lF2.Assign(LF1);
      CheckEquals(LF1.AsInteger, LF2.AsInteger);
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldCurrency_Equals;
var
  lF1: TtiFieldCurrency;
  lF2: TtiFieldCurrency;
begin
  lF1:= TtiFieldCurrency.Create(nil);
  try
    lF2:= TtiFieldCurrency.Create(nil);
    try
      lF1.AsFloat := 123.45;
      lF2.AsFloat := 123.45;
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsFloat := 456;
      Check(not lF1.Equals(lF2));
      Check(not lF2.Equals(lF1));
      lF1.AsFloat := 123.451;
      lF2.AsFloat := 123.452;
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldCurrency_Inc1;
var
  LObj: TtiObject;
  LField1:  TtiFieldCurrency;
  LField2:  TtiFieldCurrency;
begin
  LObj := TtiObject.Create;
  try
    LField1:= nil;
    LField2:= nil;
    try
      LField1:= TtiFieldCurrency.Create(LObj);
      LField2:= TtiFieldCurrency.Create(LObj);
      LField1.AsInteger:= 100;
      LField2.AsInteger:= 1;
      LField1.Inc(LField2);
      CheckEquals(101, LField1.AsInteger);
      LField2.AsInteger:= -1;
      LField1.Inc(LField2);
      CheckEquals(100, LField1.AsInteger);
    finally
      LField1.Free;
      LField2.Free;
    end;
  finally
    LObj.Free;
  end;
end;

procedure TtiObjectTestCase.FieldCurrency_Inc2;
var
  LObj: TtiObject;
  LField1:  TtiFieldCurrency;
begin
  LObj := TtiObject.Create;
  try
    LField1:= TtiFieldCurrency.Create(LObj);
    try
      LField1.AsInteger:= 100;
      LField1.Inc(1);
      CheckEquals(101, LField1.AsInteger);
      LField1.Inc(-1);
      CheckEquals(100, LField1.AsInteger);
    finally
      LField1.Free;
    end;
  finally
    LObj.Free;
  end;
end;

procedure TtiObjectTestCase.FieldDate_Assign;
var
  lF1: TtiFieldDate;
  lF2: TtiFieldDate;
begin
  lF1:= TtiFieldDate.Create(nil);
  try
    lF2:= TtiFieldDate.Create(nil);
    try
      lF1.AsDateTime := EncodeDate(2006, 01, 01);
      lF2.AsDateTime := EncodeDate(2006, 01, 02);
      CheckNotEquals(LF1.AsDateTime, LF2.AsDateTime);
      lF2.Assign(LF1);
      CheckEquals(LF1.AsDateTime, LF2.AsDateTime);
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldDate_YearsMonthsDays;
var
  lPerObj: TtiObject;
  lField:  TtiFieldDate;
  lValue   : TDateTime;
  lValueStr : string;
begin
  lValue   := EncodeDate(2004, 06, 03);
  lValueStr := tiDateTimeAsXMLString(lValue);

  lPerObj := TtiObject.Create;
  try
    lField:= TtiFieldDate.Create(lPerObj);
    try
      lField.AsDateTime := lValue;
      CheckEquals(2004, lField.Years, 'Failed on 1');
      CheckEquals(6, lField.Months, 'Failed on 2');
      CheckEquals(3, lField.Days, 'Failed on 3');
      Check(not lField.IsNull, 'Failed on 4');

      lField.IsNull := True;
      CheckEquals(0, lField.Years, 'Failed on 5');
      CheckEquals(0, lField.Months, 'Failed on 6');
      CheckEquals(0, lField.Days, 'Failed on 7');
      Check(lField.IsNull, 'Failed on 8');
    finally
      lField.Free;
    end;
  finally
    lPerObj.Free;
  end;
end;

procedure TtiObjectTestCase.FieldDate_Equals;
var
  lF1: TtiFieldDate;
  lF2: TtiFieldDate;
begin
  lF1:= TtiFieldDate.Create(nil);
  try
    lF2:= TtiFieldDate.Create(nil);
    try
      lF1.AsDateTime := EncodeDate(2005, 01, 01);
      lF2.AsDateTime := EncodeDate(2005, 01, 01);
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsDateTime := EncodeDate(2005, 01, 02);
      Check(not lF1.Equals(lF2));
      Check(not lF2.Equals(lF1));
      lF1.AsDateTime := EncodeDate(2005, 01, 01);
      lF2.AsDateTime := EncodeDate(2005, 01, 01);
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsDateTime := EncodeDate(2005, 01, 02);
      Check(not lF1.Equals(lF2));
      Check(not lF2.Equals(lF1));
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;


procedure TtiObjectTestCase.FieldDate_Fail;
var
  LObj: TtiObject;
  LField:  TtiFieldDate;
  LValue   : TDateTime;
  LValueStr : string;
begin

  LObj := TtiObject.Create;
  try
    LField:= TtiFieldDate.Create(LObj);
    try
      Check(LField.IsNull, 'IsNull #1');

      LValue   := EncodeDate(2004, 06, 03) + EncodeTime( 12, 45, 15, 00);
      LValueStr := tiDateTimeAsXMLString(LValue);

//      StartExpectingException(EtiOPFDataException);
//      LField.AsString := LValueStr;
//      StopExpectingException;
//
//      StartExpectingException(EtiOPFDataException);
//      LField.AsDateTime := LValue;
//      StopExpectingException;

    finally
      LField.Free;
    end;
  finally
    LObj.Free;
  end;
end;

procedure TtiObjectTestCase.FieldFloat_Assign;
var
  lF1: TtiFieldFloat;
  lF2: TtiFieldFloat;
begin
  lF1:= TtiFieldFloat.Create(nil);
  try
    lF2:= TtiFieldFloat.Create(nil);
    try
      lF1.AsFloat := 1.1;
      lF2.AsFloat := 2.2;
      CheckNotEquals(LF1.AsFloat, LF2.AsFloat);
      lF2.Assign(LF1);
      CheckEquals(LF1.AsFloat, LF2.AsFloat);
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldFloat_Equals;
var
  lF1: TtiFieldFloat;
  lF2: TtiFieldFloat;
begin
  lF1:= TtiFieldFloat.Create(nil);
  try
    lF2:= TtiFieldFloat.Create(nil);
    try
      lF1.Precision := 3;
      lF2.Precision := 3;

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 123.456;
      Check(lF1.Equals(lF2), 'Failed on 1');
      Check(lF2.Equals(lF1), 'Failed on 2');

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 456.789;
      Check(not lF1.Equals(lF2), 'Failed on 3');
      Check(not lF2.Equals(lF1), 'Failed on 4');

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 123.457;
      Check(not lF1.Equals(lF2), 'Failed on 5');
      Check(not lF2.Equals(lF1), 'Failed on 6');

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 123.4561;
      Check(lF1.Equals(lF2), 'Failed on 7');
      Check(lF2.Equals(lF1), 'Failed on 8');

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 123.4559;
      Check(lF1.Equals(lF2), 'Failed on 9');
      Check(lF2.Equals(lF1), 'Failed on 10');

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 123.4555;
      Check(not lF1.Equals(lF2), 'Failed on 11');
      Check(not lF2.Equals(lF1), 'Failed on 12');

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 123.4554;
      Check(not lF1.Equals(lF2), 'Failed on 13');
      Check(not lF2.Equals(lF1), 'Failed on 14');

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 123.4564;
      Check(not lF1.Equals(lF2), 'Failed on 15');
      Check(not lF2.Equals(lF1), 'Failed on 16');

      lF1.AsFloat := 123.4563;
      lF2.AsFloat := 123.4564;
      Check(lF1.Equals(lF2), 'Failed on 17');
      Check(lF2.Equals(lF1), 'Failed on 18');

      lF1.AsFloat := 123.456;
      lF2.AsFloat := 123.4564;
      lF1.Precision:= 2;
      lF2.Precision:= 2;
      Check(lF1.Equals(lF2), 'Failed on 19');
      Check(lF2.Equals(lF1), 'Failed on 20');
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;


procedure TtiObjectTestCase.FieldInt64_Assign;
var
  lF1: TtiFieldInteger;
  lF2: TtiFieldInteger;
begin
  lF1:= TtiFieldInteger.Create(nil);
  try
    lF2:= TtiFieldInteger.Create(nil);
    try
      lF1.AsInteger := 1;
      lF2.AsInteger := 2;
      CheckNotEquals(LF1.AsInteger, LF2.AsInteger);
      lF2.Assign(LF1);
      CheckEquals(LF1.AsInteger, LF2.AsInteger);
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldInt64_Equals;
var
  lF1: TtiFieldInteger;
  lF2: TtiFieldInteger;
begin
  lF1:= TtiFieldInteger.Create(nil);
  try
    lF2:= TtiFieldInteger.Create(nil);
    try
      lF1.AsInteger := 123;
      lF2.AsInteger := 123;
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsInteger := 456;
      Check(not lF1.Equals(lF2));
      Check(not lF2.Equals(lF1));
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;


procedure TtiObjectTestCase.FieldString_Assign;
var
  lF1: TtiFieldString;
  lF2: TtiFieldString;
begin
  lF1:= TtiFieldString.Create(nil);
  try
    lF2:= TtiFieldString.Create(nil);
    try
      lF1.AsString := 'string 1';
      lF2.AsString := 'string 2';
      CheckNotEquals(LF1.AsString, LF2.AsString);
      lF2.Assign(LF1);
      CheckEquals(LF1.AsString, LF2.AsString);
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

procedure TtiObjectTestCase.FieldString_Equals;
var
  lF1: TtiFieldString;
  lF2: TtiFieldString;
begin
  lF1:= TtiFieldString.Create(nil);
  try
    lF2:= TtiFieldString.Create(nil);
    try
      lF1.AsString := 'string 1';
      lF2.AsString := 'string 1';
      Check(lF1.Equals(lF2));
      Check(lF2.Equals(lF1));
      lF2.AsString := 'string 2';
      Check(not lF1.Equals(lF2));
      Check(not lF2.Equals(lF1));
    finally
      lF2.Free;
    end;
  finally
    lF1.Free;
  end;
end;

type
  TtiObjectForTestingParent = class(TtiObject)
  end;

  TtiObjectListForTestingParent = class(TtiObjectList)
  end;

procedure TtiObjectTestCase.Parent_InheritsFromVsIs;
var
  LList: TtiObjectListForTestingParent;
begin
  LList:= TtiObjectListForTestingParent.Create;
  try
    Check(LList is TtiObject);
    Check(LList is TtiObjectList);
    Check(LList is TtiObjectListForTestingParent);

    Check(LList.InheritsFrom(TtiObject));
    Check(LList.InheritsFrom(TtiObjectList));
    Check(LList.InheritsFrom(TtiObjectListForTestingParent));
  finally
    LList.Free;
  end;
end;

procedure TtiObjectTestCase.Parent_TtiObject;
var
  LObject1: TtiObjectForTestingParent;
  LObject2: TtiObjectForTestingParent;
begin
  LObject1:= TtiObjectForTestingParent.Create;
  try
    LObject2:= TtiObjectForTestingParent.Create;
    try
      CheckNull(LObject2.Owner, 'Test #1');
      CheckNull(LObject2.Parent, 'Test #2');
      LObject2.Owner:= LObject1;
      CheckSame(LObject1, LObject2.Owner, 'Test #3');
      CheckSame(LObject1, LObject2.Parent, 'Test #4');
    finally
      LObject2.Free;
    end;
  finally
    LObject1.Free;
  end;
end;

procedure TtiObjectTestCase.Parent_TtiObjectList;
var
  LObject1: TtiObjectForTestingParent;
  LObject2: TtiObjectForTestingParent;
  LList:    TtiObjectListForTestingParent;
begin
  LObject1:= TtiObjectForTestingParent.Create;
  try
    LList:= TtiObjectListForTestingParent.Create;
    try
      LList.Owner:= LObject1;
      LObject2:= TtiObjectForTestingParent.Create;
      CheckNull(LObject2.Owner, 'Test #1');
      CheckNull(LObject2.Parent, 'Test #2');
      LList.Add(LObject2);
      CheckSame(LList, LObject2.Owner, 'Test #3');
      CheckSame(LObject1, LObject2.Parent, 'Test #4');
    finally
      LList.Free;
    end;
  finally
    LObject1.Free;
  end;
end;

procedure TtiObjectTestCase.PropCount;
var
  lData : TtiObject;
begin
  // The extra simple property is Caption
  lData :=  TTestGetPropNames.Create;
  try
    CheckEquals(2, lData.PropCount([tkClass]), 'Failed on TTestVisitedList/tkClass');
    {$IFDEF FPC}
    CheckEquals(21, lData.PropCount(ctkSimple), 'Failed on TTestVisitedList/ctkSimple');
    {$ELSE}
    CheckEquals(19, lData.PropCount(ctkSimple), 'Failed on TTestVisitedList/ctkSimple');
    {$ENDIF}
  finally
    lData.Free;
  end;
end;

{ TtiObjectListTestCase }

procedure TtiObjectListTestCase.Add;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
  i : integer;
  idx: integer;
begin
  lList := TtiObjectListForTesting.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtiObjectForTesting.Create;
      idx := lList.Add(lData);
      CheckEquals(i, lList.List.IndexOf(lData), 'Failed on ' + IntToStr(i));
      CheckEquals(idx, i, 'Failed on ' + IntToStr(i));
      CheckEquals(idx, lList.List.IndexOf(lData), 'Failed on ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;


procedure TtiObjectListTestCase.AddItemOwner;
var
  LList: TtiObjectList;
  LItemOwner: TtiObject;
  LItem: TtiObject;
begin
  LList:= nil;
  LItemOwner:= nil;

  // Default behaviour
  try
    LList:= TtiObjectList.Create;
    LItemOwner:= TtiObject.Create;
    LItem:= TtiObject.Create;
    CheckNull(LItem.Owner);
    Check(LList.AutoSetItemOwner);
    CheckSame(LList, LList.ItemOwner);
    LList.Add(LItem);
    CheckSame(LList, LItem.Owner);
  finally
    LList.Free;
    LItemOwner.Free;
  end;

  // TtiObjectList.ItemOwner set
  try
    LList:= TtiObjectList.Create;
    LItemOwner:= TtiObject.Create;
    LItem:= TtiObject.Create;
    LList.ItemOwner:= LItemOwner;
    CheckNull(LItem.Owner);
    Check(LList.AutoSetItemOwner);
    CheckSame(LItemOwner, LList.ItemOwner);
    LList.Add(LItem);
    CheckSame(LItemOwner, LItem.Owner);
  finally
    LList.Free;
    LItemOwner.Free;
  end;

  // TtiObjectList.AutoSetItemOwner:= False
  try
    LList:= TtiObjectList.Create;
    LItem:= TtiObject.Create;
    LList.AutoSetItemOwner:= false;
    CheckNull(LItem.Owner);
    Check(not LList.AutoSetItemOwner);
    CheckSame(LList, LList.ItemOwner);
    LList.Add(LItem);
    CheckNull(LItem.Owner);
  finally
    LList.Free;
  end;
end;

procedure TtiObjectListTestCase.AutoSetItemOwner;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    CheckSame(lList, lData1.Owner, 'Failed on test 1');
    lList.AutoSetItemOwner := false;
    lData2 := TtiObjectForTesting.Create;
    lList.Add(lData2);
    CheckNull(lData2.Owner, 'Failed on test 2');
  finally
    lList.Free;
  end;
end;


procedure TtiObjectListTestCase.PropToStrings;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
  lData3     : TtiObjectForTesting;
  lsl        : TStringList;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData2 := TtiObjectForTesting.Create;
    lData3 := TtiObjectForTesting.Create;
    lData1.StrProp := '1';
    lData2.StrProp := '2';
    lData3.StrProp := '3';
    lList.Add(lData1);
    lList.Add(lData2);
    lList.Add(lData3);
    lsl := TStringList.Create;
    try
      lList.PropToStrings(lsl, 'StrProp');
      CheckEquals(3,    lsl.Count, 'Failed on lsl.Count');
      CheckEquals('1',  lsl.Strings[0], 'Failed on 1');
      CheckSame(lData1, lsl.Objects[0], 'Failed on 1.Objects');
      CheckEquals('2',  lsl.Strings[1], 'Failed on 2');
      CheckSame(lData2, lsl.Objects[1], 'Failed on 1.Objects');
      CheckEquals('3',  lsl.Strings[2], 'Failed on 3');
      CheckSame(lData3, lsl.Objects[2], 'Failed on 1.Objects');
    finally
      lsl.Free;
    end;
  finally
    lList.Free;
  end;
end;


procedure TtiObjectListTestCase.Clear;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData := TtiObjectForTesting.Create;
    lList.Add(lData);
    CheckEquals(1, lList.Count, 'Count');
    lList.Clear;
    CheckEquals(0, lList.Count, 'Count');
    try
      lData.Caption;
      Fail('lData exists but is should have been freed');
    except
      on e:exception do
        begin
          { do nothing, it is ok }
        end;
    end;

    lList.OwnsObjects := false;
    lData := TtiObjectForTesting.Create;
    lList.Add(lData);
    CheckEquals(1, lList.Count, 'Count');
    lList.Clear;
    CheckEquals(0, lList.Count, 'Count');
    try
      lData.Caption;
    except
      on e:exception do
        begin
          Fail('lData was freed when it should not have been');
        end;
    end;
    lData.Free;
  finally
    lList.Free;
  end;
end;


procedure TtiObjectListTestCase.Count;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
  i : integer;
begin
  lList := TtiObjectListForTesting.Create;
  try
    CheckEquals(0, lList.Count, 'Failed on 0');
    for i := 1 to 10 do
    begin
      lData := TtiObjectForTesting.Create;
      lList.Add(lData);
      CheckEquals(i, lList.Count, 'Failed on ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;


procedure TtiObjectListTestCase.CountNotDeleted;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
  i : integer;
begin
  lList := TtiObjectListForTesting.Create;
  try
    CheckEquals(0, lList.Count, 'Failed on 0');
    for i := 1 to 10 do
    begin
      lData := TtiObjectForTesting.Create;
      lList.Add(lData);
      CheckEquals(i, lList.CountNotDeleted, 'Failed on ' + IntToStr(i));
    end;
    for i := 0 to 9 do
    begin
      lList.Items[i].Deleted := true;
      CheckEquals(9-i, lList.CountNotDeleted, 'Failed on ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;


function TtiObjectListTestCase.CreateList: TtiObjectListForTesting;
var
  xItem : TtiObjectForTesting;
begin
  result:=TtiObjectListForTesting.Create;
  // 1
  xItem:=TtiObjectForTesting.Create;
  xItem.OID.AsString:='1';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=1;
    FFloatProp:=1.23;
    FStrProp:='123';
    FDateProp:=EncodeDate(2000,01,02)+EncodeTime(3,4,5,6);
  end;
  result.Add(xItem);
  // 2
  xItem:=TtiObjectForTesting.Create;
  xItem.OID.AsString:='2';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=-4942345;
    FFloatProp:=4.56;
    FStrProp:='456';
    FDateProp:=EncodeDate(2000,02,03)+EncodeTime(4,5,6,7);
  end;
  result.Add(xItem);
  // 3
  xItem:=TtiObjectForTesting.Create;
  xItem.OID.AsString:='3';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=394554;
    FFloatProp:=123.456;
    FStrProp:='TEST VALUE';
    FDateProp:=EncodeDate(1999,02,06)+EncodeTime(20,0,0,0);
  end;
  result.Add(xItem);
  // 4
  xItem:=TtiObjectForTesting.Create;
  xItem.OID.AsString:='4';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=19867;
    FFloatProp:=235465.34321;
    FStrProp:='WHY USE WINDOWS WHEN THERE IS A DOOR?'; //:))
    FDateProp:=0;
  end;
  result.Add(xItem);
  // 5
  xItem:=TtiObjectForTesting.Create;
  xItem.OID.AsString:='5';
  with xITem do
  begin
    FBoolProp:=true;
    FIntProp:=936;
    FFloatProp:=345/678;
    FStrProp:='HOW IT WORKS?';
    FDateProp:=EncodeDate(2004,05,12);
  end;
  result.Add(xItem);
end;


procedure TtiObjectListTestCase.Delete;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
  i : integer;
begin
  lList := TtiObjectListForTesting.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtiObjectForTesting.Create;
      lList.Add(lData);
      CheckEquals(i, lList.IndexOf(lData), 'Failed on add ' + IntToStr(i));
    end;
    for i := 9 downto 0 do
    begin
      lData := lList.Items[i];
      lList.Delete(i);
      CheckEquals(-1, lList.IndexOf(lData), 'Failed on delete ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.Empty;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
begin
  Check(True); // To Force OnCheckCalled to be called
  lList := TtiObjectListForTesting.Create;
  try
    lData := TtiObjectForTesting.Create;
    lList.Add(lData);
    lList.Empty;
    try
      lData.Caption;
    except
      on e:exception do
        Fail('lData was freed when it should not have been');
    end;
    lData.Free;
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.Extract;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
  i : integer;
begin
  lList := TtiObjectListForTesting.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtiObjectForTesting.Create;
      lList.Add(lData);
      CheckEquals(i, lList.IndexOf(lData), 'Failed on add ' + IntToStr(i));
    end;
    for i := 9 downto 0 do
    begin
      lData := lList.Items[i];
      lList.Extract(lData);
      if i > 0 then
        CheckEquals(i, lList.Count, 'Failed on Extract (Count) ' + IntToStr(i))
      else
        CheckEquals(0, lList.Count, 'Failed on Extract (Count) ' + IntToStr(i));
      CheckEquals(-1, lList.IndexOf(lData), 'Failed on Extract (IndexOf) ' + IntToStr(i));
      lData.Free;
    end;
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.FindByProps_DirectValue;
var
  LList: TtiObjectListForTesting;
  LItem: TtiObjectForTesting;
begin
  LList := CreateList;
  try
    // by boolean
    CheckEquals(5,LList.Count,'Wrong list! - Wrong count!');
    LItem:=TtiObjectForTesting(LList.FindByProps(['BoolProp'],[true]));
    CheckNotNull(LItem,'Find By Boolean value - NO result when expected');
    CheckEquals('1',LItem.OID.AsString,'Find By Boolean value - wrong object!');
    LItem:=TtiObjectForTesting(LList.FindByProps(['BoolProp'],[false]));
    CheckNull(LItem,'Find By Boolean value - result when not expected');
    // by int
    LItem:=TtiObjectForTesting(LList.FindByProps(['IntProp'],[-4942345]));
    CheckNotNull(LItem,'Find By Integer value - NO result when expected -4942345');
    CheckEquals('2',LItem.OID.AsString,'Find By Integer value - wrong object! -4942345');
    LItem:=TtiObjectForTesting(LList.FindByProps(['IntProp'],[7878787]));
    CheckNull(LItem,'Find By Integer value - result when not expected 7878787');
    // by string
    LItem:=TtiObjectForTesting(LList.FindByProps(['StrProp'],['TEST VALUE']));
    CheckNotNull(LItem,'Find By String value (case sensitive) - NO result when expected <TEST VALUE>');
    CheckEquals('3',LItem.OID.AsString,'Find By String value (case sensitive) - wrong object! <TEST VALUE>');
    LItem:=TtiObjectForTesting(LList.FindByProps(['StrProp'],['Test Value'],false));
    CheckNotNull(LItem,'Find By String value (not case sensitive) - NO result when expected <Test AValue>');
    CheckEquals('3',LItem.OID.AsString,'Find By String value (not case sensitive) - wrong object! <Test AValue>');
    LItem:=TtiObjectForTesting(LList.FindByProps(['StrProp'],['Test Value'],true));
    CheckNull(LItem,'Find By String value (case sensitive) - result when not expected <Test AValue>');
    LItem:=TtiObjectForTesting(LList.FindByProps(['StrProp'],['_Test Value_'],false));
    CheckNull(LItem,'Find By String value (not case sensitive) - result when not expected <_Test Value_>');
    // float
    LItem:=TtiObjectForTesting(LList.FindByProps(['FloatProp'],[235465.34321]));
    CheckNotNull(LItem,'Find By Float value - NO result when expected 235465.34321');
    CheckEquals('4',LItem.OID.AsString,'Find By Float value - wrong object! 235465.34321');
    LItem:=TtiObjectForTesting(LList.FindByProps(['FloatProp'],[235465.3432]));
    CheckNull(LItem,'Find By Float value - result when not expected 235465.3432');
    // by Date
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[EncodeDate(2004,05,12)]));
    CheckNotNull(LItem,'Find By Date value 2004-05-12 - NO result when expected');
    CheckEquals('5',LItem.OID.AsString,'Find By Date value - wrong object!');
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[0]));
    CheckNotNull(LItem,'Find By Date value 0 - NO result when expected');
    CheckEquals('4',LItem.OID.AsString,'Find By Date value - wrong object!');
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[EncodeDate(2004,05,13)]));
    CheckNull(LItem,'Find By Date value 2004-05-13 - result when not expected');
    // by DateTime
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[EncodeDate(2000,02,03)+EncodeTime(4,5,6,7)]));
    CheckNotNull(LItem,'Find By DateTime value 2000-02-03, 4:05:06.007 - NO result when expected');
    CheckEquals('2',LItem.OID.AsString,'Find By Integer value 2000-02-03, 4:05:06.007 - wrong object!');
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[EncodeDate(1999,02,06)+EncodeTime(20,0,0,1)]));
    CheckNull(LItem,'Find By DateTime value 1999-02-06, 20:00:00.001 - result when not expected');
  finally
    LList.Free;
  end;
end;

procedure TtiObjectListTestCase.FindByProps_TypedValue;
var
  LList : TtiObjectListForTesting;
  LItem : TtiObjectForTesting;
  LInt : integer;
  LDate : TDateTime;
  LStr : string;
  LFloat : Extended;
  LBool : boolean;
begin
  LList:=CreateList;
  try
    // by boolean
    CheckEquals(5,LList.Count,'Wrong list! - Wrong count!');
    LBool:=true;
    LItem:=TtiObjectForTesting(LList.FindByProps(['BoolProp'],[LBool]));
    CheckNotNull(LItem,'Find By Boolean value - NO result when expected');
    CheckEquals('1',LItem.OID.AsString,'Find By Boolean value - wrong object!');
    LBool:=false;
    LItem:=TtiObjectForTesting(LList.FindByProps(['BoolProp'],[LBool]));
    CheckNull(LItem,'Find By Boolean value - result when not expected');
    // by int
    LInt:=-4942345;
    LItem:=TtiObjectForTesting(LList.FindByProps(['IntProp'],[LInt]));
    CheckNotNull(LItem,'Find By Integer value - NO result when expected -4942345');
    CheckEquals('2',LItem.OID.AsString,'Find By Integer value - wrong object! -4942345');
    LInt:=7878787;
    LItem:=TtiObjectForTesting(LList.FindByProps(['IntProp'],[LInt]));
    CheckNull(LItem,'Find By Integer value - result when not expected 7878787');
    // by string
    LStr:='TEST VALUE';
    LItem:=TtiObjectForTesting(LList.FindByProps(['StrProp'],[LStr]));
    CheckNotNull(LItem,'Find By String value (case sensitive) - NO result when expected <TEST VALUE>');
    CheckEquals('3',LItem.OID.AsString,'Find By String value (case sensitive) - wrong object! <TEST VALUE>');

    LStr:='Test Value';
    LItem:=TtiObjectForTesting(LList.FindByProps(['StrProp'],[LStr],false));
    CheckNotNull(LItem,'Find By String value (not case sensitive) - NO result when expected <Test AValue>');
    CheckEquals('3',LItem.OID.AsString,'Find By String value (not case sensitive) - wrong object! <Test AValue>');
    LItem:=TtiObjectForTesting(LList.FindByProps(['StrProp'],[LStr],true));
    CheckNull(LItem,'Find By String value (case sensitive) - result when not expected <Test AValue>');

    LStr:='_Test Value_';
    LItem:=TtiObjectForTesting(LList.FindByProps(['StrProp'],[LStr],false));
    CheckNull(LItem,'Find By String value (not case sensitive) - result when not expected <_Test Value_>');

    // float
    LFloat:=235465.34321;
    LItem:=TtiObjectForTesting(LList.FindByProps(['FloatProp'],[LFloat]));
    CheckNotNull(LItem,'Find By Float value - NO result when expected 235465.34321');
    CheckEquals('4',LItem.OID.AsString,'Find By Float value - wrong object! 235465.34321');
    LFloat:=235465.3432;
    LItem:=TtiObjectForTesting(LList.FindByProps(['FloatProp'],[LFloat]));
    CheckNull(LItem,'Find By Float value - result when not expected 235465.3432');

    // by Date
    LDate:=EncodeDate(2004,05,12);
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[LDate]));
    CheckNotNull(LItem,'Find By Date value 2004-05-12 - NO result when expected');
    CheckEquals('5',LItem.OID.AsString,'Find By Date value - wrong object!');

    LDate:=0;
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[LDate]));
    CheckNotNull(LItem,'Find By Date value 0 - NO result when expected');
    CheckEquals('4',LItem.OID.AsString,'Find By Date value - wrong object!');

    LDate:=EncodeDate(2004,05,13);
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[LDate]));
    CheckNull(LItem,'Find By Date value 2004-05-13 - result when not expected');

    // by DateTime
    LDate:=EncodeDate(2000,02,03)+EncodeTime(4,5,6,7);
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[LDate]));
    CheckNotNull(LItem,'Find By DateTime value 2000-02-03, 4:05:06.007 - NO result when expected');
    CheckEquals('2',LItem.OID.AsString,'Find By Integer value 2000-02-03, 4:05:06.007 - wrong object!');

    LDate:=EncodeDate(1999,02,06)+EncodeTime(20,0,0,1);
    LItem:=TtiObjectForTesting(LList.FindByProps(['DateProp'],[LDate]));
    CheckNull(LItem,'Find By DateTime value 1999-02-06, 20:00:00.001 - result when not expected');
  finally
    LList.Free;
  end;
end;

procedure TtiObjectListTestCase.FindByProps_PropertyPath;
var
  LList: TtiObjectListForTesting;
  lData: TtiObjectWithOwnedForTesting;
  lFound: TtiObjectWithOwnedForTesting;
  d: TDateTime;
begin
  LList := TtiObjectListForTesting.Create;
  try
    CheckEquals(0, LList.Count, 'failed on 1');

    // Setup test values
    lData := TtiObjectWithOwnedForTesting.Create;
    lData.OID.AsString := 'OID1';
    with lData.ObjProp do
    begin
      OID.AsString := 'OID1.1';
      BoolProp   := true;
      IntProp    := 1;
      FloatProp  := 1.23;
      StrProp    := '123';
      DateProp   := EncodeDate(2000,01,02)+EncodeTime(3,4,5,6);
    end;
    LList.Add(lData);

    // Setup test values
    lData := TtiObjectWithOwnedForTesting.Create;
    lData.OID.AsString := 'OID2';
    with lData.ObjProp do
    begin
      OID.AsString := 'OID2.1';
      BoolProp   := false;
      IntProp    := 5;
      FloatProp  := 1234.56;
      StrProp    := 'hello';
      DateProp   := EncodeDate(2009,04,02)+EncodeTime(3,4,5,6);
    end;
    LList.Add(lData);

    CheckEquals(2, LList.Count, 'failed on 2');

    // String property - return item 2
    lFound := TtiObjectWithOwnedForTesting(LList.FindByProps(['ObjProp.StrProp'],['hello']));
    CheckNotNull(lFound, 'failed on 3');
    CheckEquals('OID2', lFound.OID.AsString, 'failed in 3.1');
    CheckEquals('hello', lFound.ObjProp.StrProp, 'failed on 3.2');

    // Boolean property - return item 1
    lFound := TtiObjectWithOwnedForTesting(LList.FindByProps(['ObjProp.BoolProp'],[True]));
    CheckNotNull(lFound, 'failed on 4');
    CheckEquals('OID1', lFound.OID.AsString, 'failed in 4.1');
    CheckEquals(True, lFound.ObjProp.BoolProp, 'failed on 4.2');

    // Float property - return item 2
    lFound := TtiObjectWithOwnedForTesting(LList.FindByProps(['ObjProp.FloatProp'],[1234.56]));
    CheckNotNull(lFound, 'failed on 5');
    CheckEquals('OID2', lFound.OID.AsString, 'failed in 5.1');
    CheckEquals(1234.56, lFound.ObjProp.FloatProp, 'failed on 5.2');

    // Integer property - return item 1
    lFound := TtiObjectWithOwnedForTesting(LList.FindByProps(['ObjProp.IntProp'],[1]));
    CheckNotNull(lFound, 'failed on 6');
    CheckEquals('OID1', lFound.OID.AsString, 'failed in 6.1');
    CheckEquals(1, lFound.ObjProp.IntProp, 'failed on 6.2');

    // DateTime property - return item 2
    d := EncodeDate(2009,04,02)+EncodeTime(3,4,5,6);
    lFound := TtiObjectWithOwnedForTesting(LList.FindByProps(['ObjProp.DateProp'],[d]));
    CheckNotNull(lFound, 'failed on 7');
    CheckEquals('OID2', lFound.OID.AsString, 'failed in 7.1');
    CheckEquals(d, lFound.ObjProp.DateProp, 'failed on 7.2');

  finally
    LList.Free;
  end;
end;

procedure TtiObjectListTestCase.First;
var
  lList : TtiObjectListForTesting;
  lData0 : TtiObjectForTesting;
  lData1 : TtiObjectForTesting;
  lData2 : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData0 := TtiObjectForTesting.Create;
    lList.Add(lData0);
    lData1 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    lData2 := TtiObjectForTesting.Create;
    lList.Add(lData2);
    CheckSame(lData0, lList.First, 'Failed on First');
    lData0.Deleted := true;
    // This test will fail. Should it?
    // CheckSame(lData1, lList.First, 'Failed on First when the first was deleted');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.DoForEachMethod(const AData : TtiObject);
begin
  Assert(AData is TtiObjectForTesting, 'AData not a TtstPerObjAbs');
  TtiObjectForTesting(AData).StrProp := 'tested';
end;

procedure TtiObjectListTestCase.ForEachMethod;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
  lData3     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData2 := TtiObjectForTesting.Create;
    lData3 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    lList.Add(lData2);
    lList.Add(lData3);
    lList.ForEach(DoForEachMethod);
    CheckEquals('tested', lList.Items[0].StrProp, 'Failed on 1');
    CheckEquals('tested', lList.Items[1].StrProp, 'Failed on 2');
    CheckEquals('tested', lList.Items[2].StrProp, 'Failed on 3');
  finally
    lList.Free;
  end;
end;

procedure DoForEachMethodRegular(const AData : TtiObject);
begin
  Assert(AData is TtiObjectForTesting, 'AData not a TtstPerObjAbs');
  TtiObjectForTesting(AData).StrProp := 'tested';
end;

procedure TtiObjectListTestCase.ForEachMethodRegular;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
  lData3     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData2 := TtiObjectForTesting.Create;
    lData3 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    lList.Add(lData2);
    lList.Add(lData3);
    lList.ForEach(DoForEachMethodRegular);
    CheckEquals('tested', lList.Items[0].StrProp, 'Failed on 1');
    CheckEquals('tested', lList.Items[1].StrProp, 'Failed on 2');
    CheckEquals('tested', lList.Items[2].StrProp, 'Failed on 3');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.ForIn;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
  lData3     : TtiObjectForTesting;
  lItem      : TtiObject;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData2 := TtiObjectForTesting.Create;
    lData3 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    lList.Add(lData2);
    lList.Add(lData3);

    for lItem in lList do
    begin
      Assert(lItem is TtiObjectForTesting, 'AData not a TtstPerObjAbs');
      TtiObjectForTesting(lItem).StrProp := 'tested';
    end;

    CheckEquals('tested', lList.Items[0].StrProp, 'Failed on 1');
    CheckEquals('tested', lList.Items[1].StrProp, 'Failed on 2');
    CheckEquals('tested', lList.Items[2].StrProp, 'Failed on 3');
  finally
    lList.Free;
  end;

end;

procedure TtiObjectListTestCase.FreeDeleted;
var
  LList: TtiObjectList;
  LItem1: TtiObject;
  LItem2: TtiObject;
  LItem3: TtiObject;
begin
  LList:= TtiObjectList.Create;
  try
    LItem1:= TtiObject.Create;
    LItem1.ObjectState:= posClean;
    LList.Add(LItem1);

    LItem2:= TtiObject.Create;
    LItem2.ObjectState:= posDeleted;
    LList.Add(LItem2);

    LItem3:= TtiObject.Create;
    LItem3.ObjectState:= posDelete;
    LList.Add(LItem3);

    CheckEquals(3, LList.Count);
    LList.FreeDeleted;
    CheckEquals(2, LList.Count);

  finally
    LList.Free;
  end;
end;

procedure TtiObjectListTestCase.GetEnumerator;
var
  LList: TtiObjectList;
  LItem1: TtiObject;
  LItem2: TtiObject;
  LItem3: TtiObject;
  LEnumerator: TtiEnumerator;
  LItemCount: integer;
begin
  LList:= TtiObjectList.Create;
  try
    LItem1:= TtiObject.Create;
    LItem1.OID.AsString:= '1';
    LList.Add(LItem1);

    LItem2:= TtiObject.Create;
    LItem2.OID.AsString:= '2';
    LList.Add(LItem2);

    LItem3:= TtiObject.Create;
    LItem3.OID.AsString:= '3';
    LList.Add(LItem3);

    LEnumerator:= LList.GetEnumerator;
    try
      CheckTrue(Assigned(LEnumerator));

      LItemCount:= 0;
      while LEnumerator.MoveNext do
      begin
        inc(LItemCount);
        CheckEquals(IntToStr(LItemCount), LEnumerator.Current.OID.AsString);
      end;

      CheckEquals(3, LItemCount);
    finally
      LEnumerator.Free;
    end;
  finally
    LList.Free;
  end;
end;

procedure TtiObjectListTestCase.Add_NotifyObservers_Subject;
var
  lList: TtiObjectList;
  lItem: TtiObject;
  lObserver: TtstObserver2;
begin
  lList := TtiObjectList.Create;
  lItem := TtiObject.Create;
  lObserver := TtstObserver2.Create;

  lList.AttachObserver(lObserver);
  { simply testing the default value - first item in TNotifyOperation enum }
  CheckNotifyOperation(noCustom, lObserver.LastNotifyOperation, 'Failed on 1');
  CheckEquals(0, lObserver.UpdateCount, 'Failed on 2');
  CheckTrue(lObserver.LastUpdateSubject = nil, 'Failed on 3');

  lList.Add(lItem);
  CheckEquals(1, lObserver.UpdateCount, 'Failed on 4');
  CheckNotifyOperation(noAddItem, lObserver.LastNotifyOperation, 'Failed on 5');
  { The Subject in Update() should be the list, not the item added }
  CheckTrue(lObserver.LastUpdateSubject = lList, 'Failed on 6');

  lList.Remove(lItem);
  lList.Free;
  lObserver.Free;
end;

procedure TtiObjectListTestCase.Remove_NotifyObservers_Subject;
var
  lList: TtiObjectList;
  lItem: TtiObject;
  lObserver: TtstObserver2;
begin
  lList := TtiObjectList.Create;
  lItem := TtiObject.Create;
  lObserver := TtstObserver2.Create;

  lList.AttachObserver(lObserver);
  { simply testing the default value - first item in TNotifyOperation enum }
  CheckNotifyOperation(noCustom, lObserver.LastNotifyOperation, 'Failed on 1');
  CheckEquals(0, lObserver.UpdateCount, 'Failed on 2');
  CheckTrue(lObserver.LastUpdateSubject = nil, 'Failed on 3');

  lList.Add(lItem);
  { Reset last values }
  lObserver.UpdateCount := 0;
  lObserver.LastUpdateSubject := nil;

  lList.Remove(lItem);
  CheckEquals(1, lObserver.UpdateCount, 'Failed on 4');
  CheckNotifyOperation(noDeleteItem, lObserver.LastNotifyOperation, 'Failed on 5');
  { The Subject in Update() should be the list, not the item added }
  CheckTrue(lObserver.LastUpdateSubject = lList, 'Failed on 6');

  lList.Free;
  lObserver.Free;
end;

procedure TtiObjectListTestCase.BeginEnd_NotifyObservers_Subject;
var
  lList: TtiObjectList;
  lItem: TtiObject;
  lObserver: TtstObserver2;
begin
  lList := TtiObjectList.Create;
  lItem := TtiObject.Create;
  lObserver := TtstObserver2.Create;

  lList.AttachObserver(lObserver);
  { simply testing the default value - first item in TNotifyOperation enum }
  CheckNotifyOperation(noCustom, lObserver.LastNotifyOperation, 'Failed on 1');
  CheckEquals(0, lObserver.UpdateCount, 'Failed on 2');
  CheckTrue(lObserver.LastUpdateSubject = nil, 'Failed on 3');

  lList.BeginUpdate;
  lList.Add(lItem);
  lList.EndUpdate;
  CheckEquals(2, lObserver.UpdateCount, 'Failed on 4');
  CheckNotifyOperation(noChanged, lObserver.LastNotifyOperation, 'Failed on 5');
  { The Subject in Update() should be the list, not the item added }
  CheckTrue(lObserver.LastUpdateSubject = lList, 'Failed on 6');

  lList.Remove(lItem);
  lList.Free;
  lObserver.Free;
end;

procedure TtiObjectListTestCase.tiListToStreamDefault;
var
  lStream : TStringStream;
  lList  : TTestListOfPersistents;
begin
  lStream := TStringStream.Create('');
  try
    lList := TTestListOfPersistents.Create;
    try
      tiObject.tiListToStream(lStream, lList);
      CheckEquals(lList.AsString, lStream.DataString, 'Failing on contents');
      CheckEquals(Length(lList.AsString), lStream.Size, 'Failing on size');
    finally
      lList.Free;
    end;
  finally
    lStream.Free;
  end;
end;

procedure TtiObjectListTestCase.tiListToStreamDelims;
var
  lStream : TStringStream;
  lList  : TTestListOfPersistents;
  lFields : TStringList;
begin
  lStream := TStringStream.Create('');
  try
    lList  := TTestListOfPersistents.Create;
    try
      lFields := TStringList.Create;
      try
        lFields.Add('Caption');
        lFields.Add('StringProp');
        lFields.Add('IntProp');
        lFields.Add('DateTimeProp');
        lFields.Add('FloatProp');
        tiObject.tiListToStream(lStream, lList, #9, '|', lFields);
        CheckEquals(lList.AsString(#9, '|', lFields), lStream.DataString, 'Contents');
        CheckEquals(Length(lList.AsString(#9, '|', lFields)), lStream.Size, 'Failed on size');
      finally
        lFields.Free;
      end;
    finally
      lList.Free;
    end;
  finally
    lStream.Free;
  end;
end;

procedure TtiObjectListTestCase.tiListToStreamFields;
var
  lStream : TStringStream;
  lList  : TTestListOfPersistents;
  lFields : TStringList;
begin
  lStream := TStringStream.Create('');
  try
    lList  := TTestListOfPersistents.Create;
    try
      lFields := TStringList.Create;
      try
        lFields.Add('StringProp');
        lFields.Add('IntProp');
        lFields.Add('FloatProp');
        tiObject.tiListToStream(lStream, lList, ',', #13#10, lFields);
        CheckEquals(lList.AsString(',', #13#10, lFields), lStream.DataString, 'Failed on contents');
        CheckEquals(Length(lList.AsString(',', #13#10, lFields)), lStream.Size, 'Failed on size');
      finally
        lFields.Free;
      end;
    finally
      lList.Free;
    end;
  finally
    lStream.Free;
  end;
end;

procedure TtiObjectListTestCase.tiListToCSVDefault;
var
  lList: TTestListOfPersistents;
  lString1: string;
  lString2: string;
  lFileName: string;
  lFields: TStringList;
begin
  ForceDirectories(TempDirectory);
  lFileName := TempFileName('DUnitTest.txt');
  lList := TTestListOfPersistents.Create;
  try
    tiObject.tiListToCSV(lList, lFileName);
    lString1 := tiUtils.tiFileToString(lFileName);
    tiDeleteFile(lFileName);
    lFields := TStringList.Create;
    try
      lFields.Add('Caption');
      lFields.Add('StringProp');
      lFields.Add('IntProp');
      lFields.Add('DateTimeProp');
      lFields.Add('FloatProp');
      lString2 := lList.AsString(',', #13#10, lFields);
    finally
      lFields.Free;
    end;
  finally
    lList.Free;
  end;
  CheckEquals(lString1, lString2);
end;

procedure TtiObjectListTestCase.tiListToCSVFields;
var
  lList      : TTestListOfPersistents;
  lString1: string;
  lString2 : string;
  lFileName : string;
  lFields: TStringList;
begin
  ForceDirectories(TempDirectory);
  lFileName := TempFileName('DUnitTest.txt');
  lList  := TTestListOfPersistents.Create;
  try
    lFields:= TStringList.Create;
    try
      lFields.Add('StringProp');
      lFields.Add('IntProp');
      lFields.Add('FloatProp');
      tiObject.tiListToCSV(lList, lFileName, lFields);
      lString1 := tiUtils.tiFileToString(lFileName);
      tiDeleteFile(lFileName);
      lString2 := lList.AsString(',', #13#10, lFields);
    finally
      lFields.Free;
    end;
  finally
    lList.Free;
  end;
  CheckEquals(lString1, lString2, 'String');
end;

procedure TtiObjectListTestCase.IndexOf;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    lData2 := TtiObjectForTesting.Create;
    lList.Add(lData2);
    CheckEquals(0, lList.IndexOf(lData1), 'Failed on test 1');
    CheckEquals(1, lList.IndexOf(lData2), 'Failed on test 2');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.InsertByObject;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
  lData3     : TtiObjectForTesting;
  lData4     : TtiObjectForTesting;
  lData5     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData1.ObjectState := posClean;
    lList.Add(lData1);
    lData2 := TtiObjectForTesting.Create;
    lData2.ObjectState := posClean;
    lList.Add(lData2);
    lData3 := TtiObjectForTesting.Create;
    lData3.ObjectState := posClean;
    lData4 := TtiObjectForTesting.Create;
    lData4.ObjectState := posClean;
    lData5 := TtiObjectForTesting.Create;
    lData5.ObjectState := posClean;
    // Test inserting before the last element
    lList.Insert(lData2, lData3);
    CheckSame(lData1, lList.Items[0], 'Failed on test 1');
    CheckSame(lData2, lList.Items[2], 'Failed on test 2');
    CheckSame(lData3, lList.Items[1], 'Failed on test 3');

    // Test inserting to position 0
    lList.Insert(lData1, lData4);
    CheckSame(lData4, lList.Items[0], 'Failed on test 4');
    CheckSame(lData1, lList.Items[1], 'Failed on test 5');
    CheckSame(lData2, lList.Items[3], 'Failed on test 6');
    CheckSame(lData3, lList.Items[2], 'Failed on test 7');

    // Test inserting in the middle
    lList.Insert(lData1, lData5);
    CheckSame(lData4, lList.Items[0], 'Failed on test 8');
    CheckSame(lData5, lList.Items[1], 'Failed on test 9');
    CheckSame(lData1, lList.Items[2], 'Failed on test 10');
    CheckSame(lData2, lList.Items[4], 'Failed on test 11');
    CheckSame(lData3, lList.Items[3], 'Failed on test 12');

  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.InsertItemOwner;
var
  LList: TtiObjectList;
  LItemOwner: TtiObject;
  LItem: TtiObject;
begin
  LList:= nil;
  LItemOwner:= nil;

  // Default behaviour
  try
    LList:= TtiObjectList.Create;
    LItemOwner:= TtiObject.Create;
    LItem:= TtiObject.Create;
    CheckNull(LItem.Owner);
    Check(LList.AutoSetItemOwner);
    CheckSame(LList, LList.ItemOwner);
    LList.Insert(0, LItem);
    CheckSame(LList, LItem.Owner);
  finally
    LList.Free;
    LItemOwner.Free;
  end;

  // TtiObjectList.ItemOwner set
  try
    LList:= TtiObjectList.Create;
    LItemOwner:= TtiObject.Create;
    LItem:= TtiObject.Create;
    LList.ItemOwner:= LItemOwner;
    CheckNull(LItem.Owner);
    Check(LList.AutoSetItemOwner);
    CheckSame(LItemOwner, LList.ItemOwner);
    LList.Insert(0, LItem);
    CheckSame(LItemOwner, LItem.Owner);
  finally
    LList.Free;
    LItemOwner.Free;
  end;

  // TtiObjectList.AutoSetItemOwner:= False
  try
    LList:= TtiObjectList.Create;
    LItem:= TtiObject.Create;
    LList.AutoSetItemOwner:= false;
    CheckNull(LItem.Owner);
    Check(not LList.AutoSetItemOwner);
    CheckSame(LList, LList.ItemOwner);
    LList.Insert(0, LItem);
    CheckNull(LItem.Owner);
  finally
    LList.Free;
  end;
end;

function TtiObjectListTestCase.DoIsUnique(const AItem1, AItem2: TtiObject): integer;
var
  LItem1: TtiObjectForTesting;
  LItem2: TtiObjectForTesting;
begin
  LItem1:= AItem1 as TtiObjectForTesting;
  LItem2:= AItem2 as TtiObjectForTesting;
  result:= CompareStr(LItem1.StrProp, LItem2.StrProp);
end;

procedure TtiObjectListTestCase.IsUniqueCustom;
var
  LList: TtiObjectListForTesting;
  LItem1 : TtiObjectForTesting;
  LItem2 : TtiObjectForTesting;
  LItem3 : TtiObjectForTesting;
begin
  LList := TtiObjectListForTesting.Create;
  try
    LItem1 := TtiObjectForTesting.Create;
    LItem1.StrProp:= 'Test 1';
    LList.Add(LItem1);
    LItem2 := TtiObjectForTesting.Create;
    LItem2.StrProp:= 'Test 2';
    LList.Add(LItem2);

    Check(LList.IsUnique(LItem1, DoIsUnique), 'Failed on test 1');
    Check(LList.IsUnique(LItem2, DoIsUnique), 'Failed on test 2');
    LItem2.StrProp:= 'Test 1';
    Check(not LList.IsUnique(LItem1, DoIsUnique), 'Failed on test 3');
    Check(not LList.IsUnique(LItem2, DoIsUnique), 'Failed on test 4');
    LItem1.Deleted:= True;
    Check(LList.IsUnique(LItem2, DoIsUnique), 'Failed on test 5');
    LItem3 := TtiObjectForTesting.Create;
    LItem3.StrProp:= 'Test 1';
    Check(not LList.IsUnique(LItem3, DoIsUnique), 'Failed on test 6');
    Check(LList.IsUnique(LItem2, DoIsUnique), 'Failed on test 7');
    LList.Add(LItem3);
    Check(not LList.IsUnique(LItem3, DoIsUnique), 'Failed on test 8');
    Check(not LList.IsUnique(LItem2, DoIsUnique), 'Failed on test 9');

  finally
    LList.Free;
  end;
end;

procedure TtiObjectListTestCase.InsertByIndex;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
  lData3     : TtiObjectForTesting;
  lData4     : TtiObjectForTesting;
  lData5     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData1.ObjectState := posClean;
    lList.Add(lData1);
    lData2 := TtiObjectForTesting.Create;
    lData2.ObjectState := posClean;
    lList.Add(lData2);
    lData3 := TtiObjectForTesting.Create;
    lData3.ObjectState := posClean;
    lData4 := TtiObjectForTesting.Create;
    lData4.ObjectState := posClean;
    lData5 := TtiObjectForTesting.Create;
    lData5.ObjectState := posClean;
    // Test inserting before the last element
    lList.Insert(1, lData3);
    CheckSame(lData1, lList.Items[0], 'Failed on test 1');
    CheckSame(lData2, lList.Items[2], 'Failed on test 2');
    CheckSame(lData3, lList.Items[1], 'Failed on test 3');

    // Test inserting to position 0
    lList.Insert(0, lData4);
    CheckSame(lData4, lList.Items[0], 'Failed on test 4');
    CheckSame(lData1, lList.Items[1], 'Failed on test 5');
    CheckSame(lData2, lList.Items[3], 'Failed on test 6');
    CheckSame(lData3, lList.Items[2], 'Failed on test 7');

    // Test inserting in the middle
    lList.Insert(1, lData5);
    CheckSame(lData4, lList.Items[0], 'Failed on test 8');
    CheckSame(lData5, lList.Items[1], 'Failed on test 9');
    CheckSame(lData1, lList.Items[2], 'Failed on test 10');
    CheckSame(lData2, lList.Items[4], 'Failed on test 11');
    CheckSame(lData3, lList.Items[3], 'Failed on test 12');

  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.ItemOwner;
var
  lList      : TtiObjectListForTesting;
  lItemOwner1 : TtiObjectForTesting;
  lData      : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    CheckSame(lList, lList.ItemOwner, 'Failed on test 1');
    lData := TtiObjectForTesting.Create;
    lList.Add(lData);
    CheckSame(lList, lData.Owner, 'Failed on test 2');
    lItemOwner1 := TtiObjectForTesting.Create;
    try
      lList.ItemOwner := lItemOwner1;
      CheckSame(lItemOwner1, lList.ItemOwner, 'Failed on test 3');
      CheckSame(lItemOwner1, lData.Owner, 'Failed on test 4');
    finally
      lItemOwner1.Free;
    end;
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.Items;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
  i : integer;
begin
  lList := TtiObjectListForTesting.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtiObjectForTesting.Create;
      lList.Add(lData);
      CheckSame(lData, lList.Items[i], 'Failed on ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.Last;
var
  lList : TtiObjectListForTesting;
  lData0 : TtiObjectForTesting;
  lData1 : TtiObjectForTesting;
  lData2 : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData0 := TtiObjectForTesting.Create;
    lList.Add(lData0);
    lData1 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    lData2 := TtiObjectForTesting.Create;
    lList.Add(lData2);
    CheckSame(lData2, lList.Last, 'Failed on Last');
    lData0.Deleted := true;
    Check(lData1 <> lList.First, 'Failed on First when the first was deleted');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.LastExcludeDeleted;
var
  lList : TtiObjectListForTesting;
  lData0 : TtiObjectForTesting;
  lData1 : TtiObjectForTesting;
  lData2 : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData0 := TtiObjectForTesting.Create;
    lList.Add(lData0);
    lData1 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    lData2 := TtiObjectForTesting.Create;
    lList.Add(lData2);
    CheckSame(lData2, lList.LastExcludeDeleted, 'Failed on LastExcludeDeleted');
    lData2.Deleted := true;
    CheckSame(lData1, lList.LastExcludeDeleted, 'Failed on LastExcludeDeleted');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.MarkListItemsDirty;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData2 := TtiObjectForTesting.Create;
    lData1.ObjectState := posClean;
    lData2.ObjectState := posClean;
    lList.Add(lData1);
    lList.Add(lData2);
    lList.MarkListItemsDirty;
    Check(lList.Items[0].Dirty, 'Failed on test 1');
    Check(lList.Items[1].Dirty, 'Failed on test 2');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.MarkListItemsForDeletion;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData2 := TtiObjectForTesting.Create;
    lList.Add(lData1);
    lList.Add(lData2);
    Check(not lList.Items[0].Deleted, 'Failed on test 1');
    Check(not lList.Items[1].Deleted, 'Failed on test 2');
    lList.MarkListItemsForDeletion;
    Check(lList.Items[0].ObjectState = posDelete, 'Failed on test 3');
    Check(lList.Items[1].ObjectState = posDelete, 'Failed on test 4');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.OwnsObjects;
var
  lList : TtiObjectListForTesting;
begin
  // A trivial test because OwnsObjects is delegated to the
  // owned instance of TObjectList
  lList := TtiObjectListForTesting.Create;
  try
    Check(lList.OwnsObjects, 'failed on OwnsObjects = true');
    lList.OwnsObjects := false;
    Check(not lList.OwnsObjects, 'failed on OwnsObjects = false');
    lList.OwnsObjects := true;        
    Check(lList.OwnsObjects, 'failed on OwnsObjects = true');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.Remove;
var
  lList : TtiObjectListForTesting;
  lData : TtiObjectForTesting;
  i : integer;
begin
  lList := TtiObjectListForTesting.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtiObjectForTesting.Create;
      lList.Add(lData);
      CheckEquals(i, lList.IndexOf(lData), 'Failed on add ' + IntToStr(i));
    end;
    for i := 9 downto 0 do
    begin
      lData := lList.Items[i];
      lList.Remove(lData);
      if i > 0 then
        CheckEquals(i, lList.Count, 'Failed on Remove (Count) ' + IntToStr(i))
      else
        CheckEquals(0, lList.Count, 'Failed on Remove (Count) ' + IntToStr(i));
      CheckEquals(-1, lList.IndexOf(lData), 'Failed on Remove (IndexOf) ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.SortByOID;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
  lData3     : TtiObjectForTesting;
  lData4     : TtiObjectForTesting;
  lData5     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData1.OID.AsString := '1';
    lData2 := TtiObjectForTesting.Create;
    lData2.OID.AsString := '2';
    lData3 := TtiObjectForTesting.Create;
    lData3.OID.AsString := '3';
    lData4 := TtiObjectForTesting.Create;
    lData4.OID.AsString := '4';
    lData5 := TtiObjectForTesting.Create;
    lData5.OID.AsString := '5';

    lList.Add(lData2);
    lList.Add(lData1);
    lList.Add(lData3);
    lList.Add(lData5);
    lList.Add(lData4);

    lList.SortByOID;
    CheckSame(lData1, lList.Items[0], 'Failed on test 1');
    CheckSame(lData2, lList.Items[1], 'Failed on test 2');
    CheckSame(lData3, lList.Items[2], 'Failed on test 3');
    CheckSame(lData4, lList.Items[3], 'Failed on test 4');
    CheckSame(lData5, lList.Items[4], 'Failed on test 5');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectListTestCase.SortByProps;
var
  lList      : TtiObjectListForTesting;
  lData1     : TtiObjectForTesting;
  lData2     : TtiObjectForTesting;
  lData3     : TtiObjectForTesting;
  lData4     : TtiObjectForTesting;
  lData5     : TtiObjectForTesting;
begin
  lList := TtiObjectListForTesting.Create;
  try
    lData1 := TtiObjectForTesting.Create;
    lData1.StrProp := '1';
    lData2 := TtiObjectForTesting.Create;
    lData2.StrProp := '2';
    lData3 := TtiObjectForTesting.Create;
    lData3.StrProp := '3';
    lData4 := TtiObjectForTesting.Create;
    lData4.StrProp := '4';
    lData5 := TtiObjectForTesting.Create;
    lData5.StrProp := '5';

    lList.Add(lData2);
    lList.Add(lData1);
    lList.Add(lData3);
    lList.Add(lData5);
    lList.Add(lData4);

    lList.SortByProps(['StrProp']);
    CheckSame(lData1, lList.Items[0], 'Failed on test 1');
    CheckSame(lData2, lList.Items[1], 'Failed on test 2');
    CheckSame(lData3, lList.Items[2], 'Failed on test 3');
    CheckSame(lData4, lList.Items[3], 'Failed on test 4');
    CheckSame(lData5, lList.Items[4], 'Failed on test 5');
  finally
    lList.Free;
  end;
end;

function _ObjectListItemMatchFunc(const AItem1, AItem2: TtiObject): Boolean;
begin
  result := TtiObjectForTesting(AItem1).StrProp = TtiObjectForTesting(AItem2).StrProp;
end;

procedure TtiObjectListTestCase.Find;
var
  LData: TtiObjectListForTesting;
  LTarget: TtiObjectForTesting;
  LOrigToFind: ItiSmartPointer<TtiObjectForTesting>;
  i: integer;
begin
  LData := CreateTestDataList;
  try
    for i := 0 to LData.Count - 1 do
    begin
      LTarget := LData.Items[i];
      CheckSame(LTarget, LData.Find(LTarget.OID), '#' + LTarget.OID.AsString);

      LTarget := (LData.Items[i] as TtiObjectWithOwnedForTesting).ObjProp;
      CheckNull(LData.Find(LTarget.OID), '#' + LTarget.OID.AsString);
    end;

    LData.SortByOID;
    for i := 0 to LData.Count - 1 do
    begin
      LTarget := LData.Items[i];
      CheckSame(LTarget, LData.Find(LTarget.OID), '#' + LTarget.OID.AsString);

      LTarget := (LData.Items[i] as TtiObjectWithOwnedForTesting).ObjProp;
      CheckNull(LData.Find(LTarget.OID), '#' + LTarget.OID.AsString);
    end;

    LOrigToFind := TtiSmartPointer<TtiObjectForTesting>.Create();
    for i := 0 to LData.Count - 1 do
    begin
      LTarget := LData.Items[i];
      CheckSame(LTarget, LData.Find(LTarget, _ObjectListItemMatchFunc), '#' + LTarget.OID.AsString);

      LOrigToFind.StrProp := LTarget.StrProp;
      LTarget.StrProp := LTarget.StrProp + 'a';
      CheckSame(LTarget, LData.Find(LTarget, _ObjectListItemMatchFunc), '#' + LTarget.OID.AsString);
      CheckNull(LData.Find(TtiObject(LOrigToFind), _ObjectListItemMatchFunc), '#' + LTarget.OID.AsString);
    end;
  finally
    LData.Free;
  end;
end;

procedure TtiObjectListTestCase.FindInHierarchy;
var
  lData : TtiObjectListForTesting;
  lTarget: TtiObjectForTesting;
  lOID: TtiOID;
  i : integer;
begin
  lData := CreateTestData;
  try
    for i := 0 to lData.Count - 1 do
    begin
      lTarget := lData.Items[i];
      lOID := lTarget.OID;
      CheckSame(lTarget, lData.FindInHierarchy(lOID), '#' + lOID.AsString);

      lTarget := (lData.Items[i] as TtiObjectWithOwnedForTesting).ObjProp;
      lOID := lTarget.OID;
      CheckSame(lTarget, lData.FindInHierarchy(lOID), '#' + lOID.AsString);
    end;
  finally
    lData.Free;
  end;
end;

procedure TtiObjectListTestCase.FindSortedUntyped;
var
  LList: TtiObjectListForTesting;
  LObject: TtiObject;
  LtstObject: TtiObjectForTesting;
  LIndex: integer;
  LValue: integer;
begin
  LList := TtiObjectListForTesting.Create;
  try
    LValue := 10;
    CheckFalse(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNull(LObject);
    CheckEquals(0, LIndex);
    LtstObject := TtiObjectForTesting.Create;
    LList.Insert(LIndex, LtstObject);
    LtstObject.IntProp := LValue;
    CheckTrue(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNotNull(LObject);
    LtstObject := LObject as TtiObjectForTesting;
    CheckEquals(LValue, LtstObject.IntProp);
    CheckEquals(0, LIndex);

    LValue := 20;
    CheckFalse(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNull(LObject);
    CheckEquals(1, LIndex);
    LtstObject := TtiObjectForTesting.Create;
    LList.Insert(LIndex, LtstObject);
    LtstObject.IntProp := LValue;
    CheckTrue(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNotNull(LObject);
    LtstObject := LObject as TtiObjectForTesting;
    CheckEquals(LValue, LtstObject.IntProp);
    CheckEquals(1, LIndex);

    LValue := 0;
    CheckFalse(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNull(LObject);
    CheckEquals(0, LIndex);
    LtstObject := TtiObjectForTesting.Create;
    LList.Insert(LIndex, LtstObject);
    LtstObject.IntProp := LValue;
    CheckTrue(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNotNull(LObject);
    LtstObject := LObject as TtiObjectForTesting;
    CheckEquals(LValue, LtstObject.IntProp);
    CheckEquals(0, LIndex);

    LValue := 5;
    CheckFalse(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNull(LObject);
    CheckEquals(1, LIndex);
    LtstObject := TtiObjectForTesting.Create;
    LList.Insert(LIndex, LtstObject);
    LtstObject.IntProp := LValue;
    CheckTrue(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNotNull(LObject);
    LtstObject := LObject as TtiObjectForTesting;
    CheckEquals(LValue, LtstObject.IntProp);
    CheckEquals(1, LIndex);

    LValue := 15;
    CheckFalse(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNull(LObject);
    CheckEquals(3, LIndex);
    LtstObject := TtiObjectForTesting.Create;
    LList.Insert(LIndex, LtstObject);
    LtstObject.IntProp := LValue;
    CheckTrue(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNotNull(LObject);
    LtstObject := LObject as TtiObjectForTesting;
    CheckEquals(LValue, LtstObject.IntProp);
    CheckEquals(3, LIndex);

    LValue := 30;
    CheckFalse(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNull(LObject);
    CheckEquals(5, LIndex);
    LtstObject := TtiObjectForTesting.Create;
    LList.Insert(LIndex, LtstObject);
    LtstObject.IntProp := LValue;
    CheckTrue(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNotNull(LObject);
    LtstObject := LObject as TtiObjectForTesting;
    CheckEquals(LValue, LtstObject.IntProp);
    CheckEquals(5, LIndex);

    LValue := -5;
    CheckFalse(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNull(LObject);
    CheckEquals(0, LIndex);
    LtstObject := TtiObjectForTesting.Create;
    LList.Insert(LIndex, LtstObject);
    LtstObject.IntProp := LValue;
    CheckTrue(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNotNull(LObject);
    LtstObject := LObject as TtiObjectForTesting;
    CheckEquals(LValue, LtstObject.IntProp);
    CheckEquals(0, LIndex);

    LValue := 25;
    CheckFalse(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNull(LObject);
    CheckEquals(6, LIndex);
    LtstObject := TtiObjectForTesting.Create;
    LList.Insert(LIndex, LtstObject);
    LtstObject.IntProp := LValue;
    CheckTrue(LList.FindSortedUntyped(LList.FindCompareIntProp, LValue, LObject, LIndex));
    CheckNotNull(LObject);
    LtstObject := LObject as TtiObjectForTesting;
    CheckEquals(LValue, LtstObject.IntProp);
    CheckEquals(6, LIndex);
  finally
    LList.Free;
  end;
end;

{ TtstPerObjList }

function TtiObjectListForTesting.Add(AObject: TtiObjectForTesting): integer;
begin
  result := inherited Add(AObject);
end;

function TtiObjectListForTesting.Clone: TtiObjectListForTesting;
begin
  result := TtiObjectListForTesting(inherited Clone);
end;

function TtiObjectListForTesting.GetItems(i: integer): TtiObjectForTesting;
begin
  result := TtiObjectForTesting(inherited GetItems(i));
end;

procedure TtiObjectListForTesting.SetItems(i: integer; const AValue: TtiObjectForTesting);
begin
  inherited SetItems(i, AValue);
end;

constructor TtiObjectListForTesting.CreateNew(const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
begin
  Create;
  OID.AsString := IntToStr(PtrInt(Self));
  ObjectState := posCreate;
end;

function TtiObjectListForTesting.FindCompareIntProp(const AObject: TtiObject;
  const AValue): integer;
var
  LObject: TtiObjectForTesting;
  LValue: integer;
begin
  LObject := AObject as TtiObjectForTesting;
  LValue := integer(AValue);
  if LObject.IntProp < LValue then
    result := -1
  else if LObject.IntProp > LValue then
    result := +1
  else
    result := 0;
end;

{ TtstPerObjAbs }

function TtiObjectForTesting.Clone: TtiObjectForTesting;
begin
  result := TtiObjectForTesting(inherited Clone);
end;

constructor TtiObjectForTesting.Create;
begin
  inherited;
  Populate;
end;

constructor TtiObjectForTesting.CreateNew(const AOwner : TtiObject; const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
begin
  Create;
  Owner := AOwner as TtiObjectListForTesting;
  OID.AsString := IntToStr(PtrInt(Self));
  ObjectState := posCreate;
end;

function TtiObjectForTesting.Equals(const AData: TtiObject): boolean;
var
  LData: TtiObjectForTesting;
begin
  Assert(AData.TestValid(TtiObjectForTesting), CTIErrorInvalidObject);
  LData:= AData as TtiObjectForTesting;
  result :=
    (Self.StrProp   = LData.StrProp)   and
    (Self.IntProp   = LData.IntProp)   and
    (Self.FloatProp = LData.FloatProp) and
    (Self.DateProp  = LData.DateProp)  and
    (Self.BoolProp  = LData.BoolProp)  and
    (Self.OrdProp   = LData.OrdProp);
end;

function TtiObjectForTesting.GetOwner: TtiObjectListForTesting;
begin
  result := TtiObjectListForTesting(inherited GetOwner);
end;

function TtiObjectForTesting.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  if StrProp = '' then
    AErrors.AddError('StrProp', cStrPropErrorMsg, cStrPropErrorCode);
  if IntProp = 0 then
    AErrors.AddError('IntProp', cIntPropErrorMsg, cIntPropErrorCode);
  if FloatProp = 0 then
    AErrors.AddError('FloatProp', cFloatPropErrorMsg, cFloatPropErrorCode);
  result := AErrors.Count = 0;
end;

procedure TtiObjectForTesting.Populate;
begin
  BoolProp := false;
  IntProp  := 1;
  FloatProp := 1.1111111;
  StrProp  := 'testing';
  DateProp := EncodeDate(2002,01,01);
  OrdProp  := tstOrdProp_1;
end;

procedure TtiObjectForTesting.SetOwner(const AValue: TtiObjectListForTesting);
begin
  inherited SetOwner(AValue);
end;

{ TtstPerObjOwnedObj }

constructor TtiObjectWithOwnedForTesting.Create;
begin
  inherited;
  FObjProp := TtiObjectForTesting.Create;
end;

destructor TtiObjectWithOwnedForTesting.Destroy;
begin
  FObjProp.Free;
  inherited;
end;

function TtiObjectWithOwnedForTesting.Equals(const AData: TtiObject): boolean;
var
  LData: TtiObjectWithOwnedForTesting;
begin
  Assert(AData.TestValid(TtiObjectWithOwnedForTesting), CTIErrorInvalidObject);
  LData:= AData as TtiObjectWithOwnedForTesting;
  result :=
    (inherited Equals(LData))   and
    (Self.ObjProp.Equals(LData.ObjProp));
end;

{ TtstPerObjOwnedObjCanAssign }

procedure TtstPerObjOwnedObjCanAssign.AssignClassProps(ASource: TtiObject);
begin
  ObjProp.Assign(TtstPerObjOwnedObjCanAssign(ASource).ObjProp);
end;

function TtstPerObjOwnedObjCanAssign.Clone: TtstPerObjOwnedObjCanAssign;
begin
  result := TtstPerObjOwnedObjCanAssign(inherited Clone);
end;

{ TtiFieldTest }

constructor TtiFieldTest.Create;
begin
  inherited;
  FFloatField:= TtiFieldFloat.Create(Self, nvAllowNull);
  FStrField:= TtiFieldString.Create(Self, nvAllowNull);
end;

destructor TtiFieldTest.Destroy;
begin
  FFloatField.Free;
  FStrField.Free;
  inherited;
end;

procedure TtiObjectListTestCase.FirstExcludeDeleted;
var
  LList : TtiObjectListForTesting;
  LData0 : TtiObjectForTesting;
  LData1 : TtiObjectForTesting;
  LData2 : TtiObjectForTesting;
begin
  LList := TtiObjectListForTesting.Create;
  try
    LData0 := TtiObjectForTesting.Create;
    LList.Add(LData0);
    LData1 := TtiObjectForTesting.Create;
    LList.Add(LData1);
    LData2 := TtiObjectForTesting.Create;
    LList.Add(LData2);
    CheckSame(LData0, lList.FirstExcludeDeleted, 'Failed on FirstExcludeDeleted');
    LData0.Deleted := true;
    CheckSame(LData1, lList.FirstExcludeDeleted, 'Failed on FirstExcludeDeleted');
  finally
    lList.Free;
  end;
end;

procedure TtiObjectTestCase.AttachDetachObserver;
var
  lSubject: TtstSubject;
  lObserver: TtstObserver;
begin
  lSubject := TtstSubject.Create;
  lObserver := TtstObserver.Create;

  try
    { Observer not yet attached }
    lSubject.Name := 'Subject';
    CheckEquals(lObserver.Name <> lSubject.Name, True, 'Failed on 1');

    { test attached observer }
    lSubject.AttachObserver(lObserver);
    lSubject.Name := 'SubjectObserved';
    CheckEquals(lObserver.Name = lSubject.Name , True, 'Failed on 2');

    { and again...}
    lSubject.Name := 'Subject';
    CheckEquals(lObserver.Name = lSubject.Name , True, 'Failed on 3');

    { test after detaching observer }
    lSubject.DetachObserver(lObserver);
    lSubject.Name := 'ObserverDetached';
    CheckEquals(lObserver.Name <> lSubject.Name, True, 'Failed on 4');
    CheckEquals(lObserver.Name = 'Subject', True, 'Failed on 5');
  finally
    lObserver.Free;
    lSubject.Free;
  end;
end;

procedure TtiObjectTestCase.NotifyObservers;
var
  lList: TtiObjectList;
  lItem: TtiObject;
  lObserver: TtstObserver2;
begin
  lList := TtiObjectList.Create;
  lItem := TtiObject.Create;
  lObserver := TtstObserver2.Create;

  lList.AttachObserver(lObserver);
  { simply testing the default value - first item in TNotifyOperation enum }
  CheckNotifyOperation(noCustom, lObserver.LastNotifyOperation, 'Failed on 1');
  CheckEquals(0, lObserver.UpdateCount, 'Failed on 2');

  { BeginUpdate..EndUpdate is added here to show that an extra notification is
    added for observers, using the generic noChanged state. }
  lList.BeginUpdate;
  lList.Add(lItem);
  CheckEquals(1, lObserver.UpdateCount, 'Failed on 3');
  CheckNotifyOperation(noAddItem, lObserver.LastNotifyOperation, 'Failed on 4');
  lList.EndUpdate;
  CheckEquals(2, lObserver.UpdateCount, 'Failed on 5');
  CheckNotifyOperation(noChanged, lObserver.LastNotifyOperation, 'Failed on 6');

  lObserver.UpdateCount := 0; // reset the counter
  lList.Remove(lItem);
  CheckEquals(1, lObserver.UpdateCount, 'Failed on 7');
  CheckNotifyOperation(noDeleteItem, lObserver.LastNotifyOperation, 'Failed on 8');

  lObserver.UpdateCount := 0; // reset the counter
  lList.Free;
  CheckEquals(1, lObserver.UpdateCount, 'Failed on 9');
  CheckNotifyOperation(noFree, lObserver.LastNotifyOperation, 'Failed on 10');

  lObserver.Free;
end;

procedure TtiObjectTestCase.NotifyObservers_vs_IsValid_call_count;
var
  lItem: TtstSubject;
  lObserver: TtstObserver;
  lMediator: TtiMediatorView;
begin
  lItem := TtstSubject.Create;
  lObserver := TtstObserver.Create;
  lItem.AttachObserver(lObserver);

  { No observer implements the ItiObserverHandlesErrorState interface, so IsValid
    should not be called during NotifyObservers. }
  CheckEquals(0, lItem.IsValidCallCount, 'Failed on 1');
  lItem.Name := 'AAAAA';
  CheckEquals(0, lItem.IsValidCallCount, 'Failed on 2');

  { reset counter }
  lItem.IsValidCallCount := 0;

  { The TtiMediatorView implementes the ItiObserverHandlesErrorState interface,
    so it is interested in IsValid results. }
  lMediator := TtiMediatorView.Create;
  lItem.AttachObserver(lMediator);

  CheckEquals(0, lItem.IsValidCallCount, 'Failed on 3');
  lItem.Name := 'BBBBB';
  CheckEquals(1, lItem.IsValidCallCount, 'Failed on 4');
end;

{ TtstSubject }

procedure TtstSubject.SetName(const AValue: string);
begin
  BeginUpdate;
  FName := AValue;
  EndUpdate;
end;

constructor TtstSubject.Create;
begin
  inherited Create;
  FIsValidCallCount := 0;
end;

function TtstSubject.IsValid(const AErrors: TtiObjectErrors): boolean;
begin
  inherited IsValid(AErrors);
  Inc(FIsValidCallCount);
  if FName = '' then
    AErrors.AddError('The Name field is missing data');
  Result := (AErrors.Count = 0);
end;

{ TtstObserver }

procedure TtstObserver.Update(ASubject: TtiObject);
begin
  inherited;
  FName := TtstSubject(ASubject).Name;
end;

{ TtstObserver2 }

constructor TtstObserver2.Create;
begin
  inherited Create;
  FUpdateCount := 0;
  FLastNotifyOperation := noCustom;
end;

procedure TtstObserver2.Update(ASubject: TtiObject; AOperation: TNotifyOperation; AData: TtiObject);
begin
  Inc(FUpdateCount);
  FLastUpdateSubject := ASubject;
  FLastNotifyOperation := AOperation;
  inherited Update(ASubject, AOperation, AData);
end;

procedure TtiObjectListTestCase.CompareWithEvent;
var
  LList1: TtiObjectList;
  LList2: TtiObjectList;
  LItemInBothSame1: TtiObjectForTesting;
  LItemInBothSame2: TtiObjectForTesting;
  LItemInBothNotSame1: TtiObjectForTesting;
  LItemInBothNotSame2: TtiObjectForTesting;
  LItemIn1Only: TtiObjectForTesting;
  LItemIn2Only: TtiObjectForTesting;
begin
  LList1:= TtiObjectList.Create;
  try
    LList2:= TtiObjectList.Create;
    try
      LItemInBothSame1  := TtiObjectForTesting.Create;
      LItemInBothSame2  := TtiObjectForTesting.Create;
      LItemInBothNotSame1:= TtiObjectForTesting.Create;
      LItemInBothNotSame2:= TtiObjectForTesting.Create;
      LItemIn1Only      := TtiObjectForTesting.Create;
      LItemIn2Only      := TtiObjectForTesting.Create;

      LItemInBothSame1.OID.AsString   := '1';
      LItemInBothSame1.StrProp        := 'A';
      LItemInBothSame2.OID.AsString   := '1';
      LItemInBothSame2.StrProp        := 'A';

      LItemInBothNotSame1.OID.AsString := '2';
      LItemInBothNotSame1.StrProp     := 'B';
      LItemInBothNotSame2.OID.AsString := '2';
      LItemInBothNotSame2.StrProp     := 'C';

      LItemIn1Only.OID.AsString       := '3';
      LItemIn1Only.StrProp            := 'D';

      LItemIn2Only.OID.AsString       := '4';
      LItemIn2Only.StrProp            := 'E';

      LList1.Add(LItemInBothSame1);
      LList2.Add(LItemInBothSame2);
      LList1.Add(LItemInBothNotSame1);
      LList2.Add(LItemInBothNotSame2);
      LList1.Add(LItemIn1Only);
      LList2.Add(LItemIn2Only);

      // OID matching and Item.Equals() comparison
      LList1.CompareWith(LList2, InBothAndEqualsEvent, InBothAndNotEqualsEvent,
          In1OnlyEvent, In2OnlyEvent);

      CheckEquals(1, FInBothAndEquals.Count,    'FInBothAndEquals.Count');
      CheckEquals(1, FInBothAndNotEquals.Count, 'FInBothAndNotEquals.Count');
      CheckEquals(1, FIn1Only.Count,            'FIn1Only.Count');
      CheckEquals(1, FIn2Only.Count,            'FIn2Only.Count');
      CheckEquals('"TtiObjectForTesting: TtiObjectForTesting <> TtiObjectForTesting"', FDifferenceMessage);

      CheckSame(LItemInBothSame1, FInBothAndEquals.Items[0], 'LItemInBothSame1');
      CheckSame(LItemInBothNotSame1, FInBothAndNotEquals.Items[0], 'LItemInBothNotSame1');
      CheckSame(LItemIn1Only, FIn1Only.Items[0], 'LItemIn1Only');
      CheckSame(LItemIn2Only, FIn2Only.Items[0], 'LItemIn2Only');

      // Custom item matching and item comparison
      // To test this and make sure that we're not doing the default OID
      // matching and full Equals() comparison we make all OID's different,
      // do matching of IntProp (not OID) and comparison of StrProp (only).
      LItemInBothSame1.OID.AsString   := '1';
      LItemInBothSame1.IntProp        :=  1;
      LItemInBothSame1.StrProp        := 'A';
      LItemInBothSame2.OID.AsString   := '2';
      LItemInBothSame2.IntProp        :=  1;
      LItemInBothSame2.StrProp        := 'A';

      LItemInBothNotSame1.OID.AsString:= '3';
      LItemInBothNotSame1.IntProp     :=  2;
      LItemInBothNotSame1.StrProp     := 'B';
      LItemInBothNotSame2.OID.AsString:= '4';
      LItemInBothNotSame2.IntProp     :=  2;
      LItemInBothNotSame2.StrProp     := 'C';

      LItemIn1Only.OID.AsString       := '5';
      LItemIn1Only.IntProp            :=  5;
      LItemIn1Only.StrProp            := 'D';

      LItemIn2Only.OID.AsString       := '6';
      LItemIn2Only.IntProp            :=  6;
      LItemIn2Only.StrProp            := 'E';

      FInBothAndEquals.Clear;
      FInBothAndNotEquals.Clear;
      FIn1Only.Clear;
      FIn2Only.Clear;
      LList1.CompareWith(LList2, InBothAndEqualsEvent, InBothAndNotEqualsEvent,
          In1OnlyEvent, In2OnlyEvent, ItemMatchFunc, ItemCompareFunc);

      CheckEquals(1, FInBothAndEquals.Count,    'FInBothAndEquals.Count');
      CheckEquals(1, FInBothAndNotEquals.Count, 'FInBothAndNotEquals.Count');
      CheckEquals(1, FIn1Only.Count,            'FIn1Only.Count');
      CheckEquals(1, FIn2Only.Count,            'FIn2Only.Count');
      CheckEquals('', FDifferenceMessage); // Difference not implemented in ItemCompareFunc

      CheckSame(LItemInBothSame1, FInBothAndEquals.Items[0], 'LItemInBothSame1');
      CheckSame(LItemInBothNotSame1, FInBothAndNotEquals.Items[0], 'LItemInBothNotSame1');
      CheckSame(LItemIn1Only, FIn1Only.Items[0], 'LItemIn1Only');
      CheckSame(LItemIn2Only, FIn2Only.Items[0], 'LItemIn2Only');
    finally
      LList2.Free;
    end;
  finally
    LList1.Free;
  end;
end;

procedure TtiObjectListTestCase.SetUp;
begin
  inherited SetUp;
  FInBothAndEquals   := TtiObjectList.Create;
  FInBothAndEquals.OwnsObjects:= False;
  FInBothAndEquals.AutoSetItemOwner:= False;

  FInBothAndNotEquals := TtiObjectList.Create;
  FInBothAndNotEquals.OwnsObjects:= False;
  FInBothAndNotEquals.AutoSetItemOwner:= False;

  FIn1Only           := TtiObjectList.Create;
  FIn1Only.OwnsObjects:= False;
  FIn1Only.AutoSetItemOwner:= False;

  FIn2Only           := TtiObjectList.Create;
  FIn2Only.OwnsObjects:= False;
  FIn2Only.AutoSetItemOwner:= False;

  FDifferenceMessage:= '';
end;

procedure TtiObjectListTestCase.TearDown;
begin
  inherited TearDown;
  FInBothAndEquals.Free;
  FInBothAndNotEquals.Free;
  FIn1Only.Free;
  FIn2Only.Free;
end;

procedure TtiObjectListTestCase.In1OnlyEvent(AItem1, AItem2: TtiObject);
begin
  Assert(AItem1.TestValid, CTIErrorInvalidObject);
  Assert(AItem2=nil, 'AItem2 assigned');
  Assert(FIn1Only.TestValid, CTIErrorInvalidObject);
  FIn1Only.Add(AItem1);
end;

procedure TtiObjectListTestCase.In2OnlyEvent(AItem1, AItem2: TtiObject);
begin
  Assert(AItem1=nil, 'AItem1 assigned');
  Assert(AItem2.TestValid, CTIErrorInvalidObject);
  FIn2Only.Add(AItem2);
end;

procedure TtiObjectListTestCase.InBothAndEqualsEvent(AItem1, AItem2: TtiObject);
begin
  Assert(AItem1.TestValid, CTIErrorInvalidObject);
  Assert(AItem2.TestValid, CTIErrorInvalidObject);
  Assert(FInBothAndEquals.TestValid, CTIErrorInvalidObject);
  FInBothAndEquals.Add(AItem1);
end;

procedure TtiObjectListTestCase.InBothAndNotEqualsEvent(
  const AItem1, AItem2: TtiObject; var ADifferenceMessage: string);
begin
  Assert(AItem1.TestValid, CTIErrorInvalidObject);
  Assert(AItem2.TestValid, CTIErrorInvalidObject);
  Assert(FInBothAndNotEquals.TestValid, CTIErrorInvalidObject);
  FInBothAndNotEquals.Add(AItem1);
  FDifferenceMessage:= ADifferenceMessage;
end;

function TtiObjectListTestCase.ItemMatchFunc(const AItem1, AItem2: TtiObject): Boolean;
begin
  Assert(AItem1.TestValid((TtiObjectForTesting)), CTIErrorInvalidObject);
  Assert(AItem2.TestValid(TtiObjectForTesting), CTIErrorInvalidObject);
  result := TtiObjectForTesting(AItem1).IntProp = TtiObjectForTesting(AItem2).IntProp;
end;

function TtiObjectListTestCase.ItemCompareFunc(const AItem1, AItem2: TtiObject): Boolean;
begin
  Assert(AItem1.TestValid((TtiObjectForTesting)), CTIErrorInvalidObject);
  Assert(AItem2.TestValid(TtiObjectForTesting), CTIErrorInvalidObject);
  result := TtiObjectForTesting(AItem1).StrProp = TtiObjectForTesting(AItem2).StrProp;
end;

{ TtiObjectForTestingDifMessage }

end.

