unit tiObject_TST;

{$I tiDefines.inc}

interface

uses
  Classes  // needed for TStringList
  {$IFDEF FPC}
  ,testregistry
  {$ELSE}
  ,TestFramework
  {$ENDIF}
  ,tiTestFramework
  ,tiObject
  ,tstPerFramework_BOM
 ;


type
  TtstPerObjList  = class;
  TtstPerObjAbs   = class;


  TTestTIObject = class(TtiTestCase)
  private
    procedure CheckTSTPerObjAbs(AData: TtstPerObjAbs; AValue: integer);
    procedure SetTSTPerObjAbs(AData: TtstPerObjAbs; AValue: integer);
    procedure CheckTSTPerObjList(AData: TtstPerObjList; AValue: integer);
    procedure SetTSTPerObjList(AData: TtstPerObjList; AValue: integer);
    procedure TstFindMethod(AObject : TtiObject; var AFound : boolean);
    procedure TstFindMethodWithParam(AObject : TtiObject; var AFound : boolean; AUserContext: Pointer);
    procedure TstFindAll(AObject : TtiObject; var AFound : boolean);
    function  CreateTestDataList: TtstPerObjList;
  published
    procedure Owner;
    procedure Parent_TtiObject;
    procedure Parent_TtiObjectList;
    procedure Deleted;
    procedure Dirty;
    procedure Index;
    procedure SetPropValue;
    procedure GetPropValue;
    procedure PropType;
    procedure IsReadWriteProp;
    procedure PropCount;

    procedure Equals;
    procedure ObjectStateAsString;
    procedure FindByOID;
    procedure FindWithMethod;
    procedure FindWithMethodAndParam;
    procedure FindAllWithMethod;
    procedure IsUnique;
    procedure AssignFlat;
    procedure AssignList;
    procedure AssignCompound;
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
  end;


  TTestTIObjectList = class(TtiTestCase)
  private
    FInBothAndEquals: TtiObjectList;
    FInBothAndNotEquals: TtiObjectList;
    FIn1Only: TtiObjectList;
    FIn2Only: TtiObjectList;

    procedure InBothAndEqualsEvent(  AItem1, AItem2: TtiObject);
    procedure InBothAndNotEqualsEvent(AItem1, AItem2: TtiObject);
    procedure In1OnlyEvent(          AItem1, AItem2: TtiObject);
    procedure In2OnlyEvent(          AItem1, AItem2: TtiObject);

    function  CreateList : TtstPerObjList;
    procedure DoForEachMethod(AData: TtiObject);
  public
    {$IFDEF FPC}
    constructor Create; override;
    {$ELSE}
    constructor Create(AMethodName: string); override;
    {$ENDIF}
    destructor  Destroy; override;
  published
    procedure   Add;
    procedure   Items;
    procedure   Count;
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
    procedure   MarkListItemsForDeletion;
    procedure   MarkListItemsDirty;
    procedure   PropToStrings;
    procedure   FindByProps_DirectValue;
    procedure   FindByProps_TypedValue;
    procedure   SortByOID;
    procedure   SortByProps;
    procedure   Find;
    procedure   FindInHierarchy;
    procedure   CompareWithEvent;
    procedure   FreeDeleted;
    procedure   tiListToCSVDefault;
    procedure   tiListToCSVFields;
    procedure   tiListToStreamDefault;
    procedure   tiListToStreamDelims;
    procedure   tiListToStreamFields;
  end;


  TtstPerObjList = class(TtiObjectList)
  private
  protected
    function    GetItems(i: integer): TtstPerObjAbs; reintroduce;
    procedure   SetItems(i: integer; const AValue: TtstPerObjAbs); reintroduce;
  public
    property    Items[i:integer]: TtstPerObjAbs read GetItems write SetItems;
    function    Add(AObject : TtstPerObjAbs ; ADefDispOrdr : boolean = true): integer; reintroduce;
    function    Clone : TtstPerObjList; reintroduce;
    constructor CreateNew(const ADatabaseName : string = ''; const APersistenceLayerName : string = ''); override;
  published
  end;


  TtstOrdProp = (tstOrdProp_1, tstOrdProp_2, tstOrdProp_3);


  TtstPerObjAbs = class(TtiObject)
  private
    FBoolProp: boolean;
    FIntProp: integer;
    FFloatProp: extended;
    FStrProp: string;
    FDateProp: TDateTime;
    FOrdProp: TtstOrdProp;
  protected
    function    GetOwner: TtstPerObjList; reintroduce;
    procedure   SetOwner(const AValue: TtstPerObjList); reintroduce;
  public
    constructor Create; override;
    constructor CreateNew(const AOwner : TtiObject; const ADatabaseName : string = ''; const APersistenceLayerName : string = ''); override;
    property    Owner      : TtstPerObjList             read GetOwner      write SetOwner;
    procedure   Populate;
    function    Clone : TtstPerObjAbs; reintroduce;
    function    IsValid(const AErrors : TtiObjectErrors): boolean; override;
    function    Equals(const AData : TtiObject): boolean; override;
  published
    property    StrProp  : string      read FStrProp   write FStrProp;
    property    IntProp  : integer     read FIntProp   write FIntProp;
    property    FloatProp : extended    read FFloatProp write FFloatProp;
    property    DateProp : TDateTime   read FDateProp  write FDateProp;
    property    BoolProp : boolean     read FBoolProp  write FBoolProp;
    property    OrdProp  : TtstOrdProp read FOrdProp   write FOrdProp;
    property    StrPropReadOnly : string read FStrProp;
  end;


  TtstPerObjOwnedObj = class(TtstPerObjAbs)
  private
    FObjProp: TtstPerObjAbs;
  public
    constructor Create; override;
    destructor  Destroy; override;
    function    Equals(const AData : TtiObject): boolean; override;
  published
    property ObjProp : TtstPerObjAbs read FObjProp;
  end;


  TtstPerObjOwnedObjCanAssign = class(TtstPerObjOwnedObj)
  protected
    procedure   AssignClassProps(ASource: TtiObject); override;
  public
    function    Clone : TtstPerObjOwnedObjCanAssign; reintroduce;
  end;


  TtstPerObjMappedObj = class(TtstPerObjAbs)
  private
    FObjProp: TtstPerObjAbs;
  published
    property ObjProp : TtstPerObjAbs read FObjProp write FObjProp;
  end;


  // Used in AttachDetachObserver test.
  TtstSubject = class(TtiObject)
  private
    FName: string;
    procedure SetName(const AValue: string);
  public
    property  Name: string read FName write SetName;
  end;


  // Used in AttachDetachObserver test.
  TtstObserver = class(TtiObject)
  private
    FName: string;
  public
    property  Name: string read FName write FName;
    procedure Update(ASubject: TtiObject); override;
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
  // tiOPF
   tiOPFManager
  ,tiOPFTestManager
  ,tiDUnitUtils
  ,tiDUnitDependencies
  ,tiOIDGUID
  ,tiOIDInteger
  ,tiXML
  ,tiUtils
  ,tiVisitor
  ,tiConstants
  ,tiOID
  ,tiRTTI

  // Delphi
  ,SysUtils
  ,TypInfo
  ,Clipbrd
 ;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIObject);
  RegisterNonPersistentTest(TTestTIObjectList);
end;


function CreateTestData : TtstPerObjList;
var
  lOID : Integer;
  i : integer;
  lData : TtstPerObjOwnedObj;
begin
  lOID := 1;
  result := TtstPerObjList.Create;
  result.ObjectState := posClean;
  result.OID.AsString := IntToStr(lOID);
  Inc(lOID);

  for i := 9 downto 0 do
  begin
    lData := TtstPerObjOwnedObj.Create;
    lData.ObjectState := posClean;
    lData.OID.AsString := IntToStr(lOID);
    Inc(lOID);
    lData.ObjProp.ObjectState := posClean;
    lData.ObjProp.OID.AsString := IntToStr(lOID);
    Inc(lOID);
    result.Add(lData);
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

function TtstAsDebugStringObjectList.GetCaption: string;
begin
  result:= 'Class name for ' + ClassName;
end;

procedure TTestTIObject.AsDebugString;
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

procedure TTestTIObject.AssignCompound;
var
  lTestExceptionFrom : TtstPerObjOwnedObj;
  lTestExceptionTo  : TtstPerObjOwnedObj;
  lFrom : TtstPerObjOwnedObjCanAssign;
  lTo  : TtstPerObjOwnedObjCanAssign;
begin
  lTestExceptionFrom := TtstPerObjOwnedObj.Create;
  try
    lTestExceptionTo := TtstPerObjOwnedObj.Create;
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


procedure TTestTIObject.SetTSTPerObjAbs(AData : TtstPerObjAbs; AValue : integer);
begin
  AData.StrProp  := IntToStr(AValue);
  AData.IntProp  := AValue;
  AData.FloatProp := AValue + AValue / 10 + AValue / 100 + AValue + AValue / 1000;
  AData.DateProp := AValue;
  AData.BoolProp := (AValue mod 2) = 0;
  AData.OID.AsString       := IntToStr(AValue);
end;


procedure TTestTIObject.CheckTSTPerObjAbs(AData : TtstPerObjAbs; AValue : integer);
begin
  CheckEquals(IntToStr(AValue), AData.StrProp, 'Failed on StrField');
  CheckEquals(AValue, AData.IntProp,'Failed on IntField');
  CheckEquals(AValue + AValue / 10 + AValue / 100 + AValue + AValue / 1000, AData.FloatProp, cDUnitTestFloatPrecision, 'Failed on FloatField');
  CheckEquals(AValue, AData.DateProp, 0.00001, 'Failed on DateField');
  CheckEquals((AValue mod 2) = 0, AData.BoolProp, 'Failed on Bool');
  CheckEquals(IntToStr(AValue), AData.OID.AsString,'Failed on OID');
end;


procedure TTestTIObject.SetTSTPerObjList(AData : TtstPerObjList; AValue : integer);
begin
  AData.OID.AsString       := IntToStr(AValue);
end;


procedure TTestTIObject.CheckTSTPerObjList(AData : TtstPerObjList; AValue : integer);
begin
  CheckEquals(IntToStr(AValue), AData.OID.AsString,'Failed on OID');
end;


procedure TTestTIObject.AssignFlat;
var
  lFrom : TtstPerObjAbs;
  lTo  : TtstPerObjAbs;
begin
  lFrom := TtstPerObjAbs.Create;
  try
    SetTSTPerObjAbs(lFrom, 1);
    lTo  := TtstPerObjAbs.Create;
    try
      SetTSTPerObjAbs(lTo, 2);
      lTo.Assign(lFrom);
      CheckTSTPerObjAbs(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
      Check(nil = lTo.Owner, 'Failed on Owner');
    finally
      lTo.Free;
    end;
  finally
    lFrom.Free;
  end;
end;


procedure TTestTIObject.AssignList;
var
  lFrom : TtstPerObjList;
  lItem : TtstPerObjAbs;
  lTo  : TtstPerObjList;
begin
  lFrom := TtstPerObjList.Create;
  try
    lFrom.OwnsObjects := true;
    SetTSTPerObjList(lFrom, 1);
    lItem := TtstPerObjAbs.Create;
    SetTSTPerObjAbs(lItem, 2);
    lFrom.Add(lItem);
    lTo  := TtstPerObjList.Create;
    try
      SetTSTPerObjList(lTo, 3);
      lTo.Assign(lFrom);
      CheckTSTPerObjList(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
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

  lFrom := TtstPerObjList.Create;
  try
    lFrom.OwnsObjects := false;
    lFrom.AutoSetItemOwner := false;
    SetTSTPerObjList(lFrom, 1);
    lItem := TtstPerObjAbs.Create;
    try
      SetTSTPerObjAbs(lItem, 2);
      lFrom.Add(lItem);
      lTo  := TtstPerObjList.Create;
      lTo.OwnsObjects := false;
      lTo.AutoSetItemOwner := false;
      try
        SetTSTPerObjList(lTo, 3);
        lTo.Assign(lFrom);
        CheckTSTPerObjList(lTo, 1);
        Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
        CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
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


procedure TTestTIObject.CloneFlat;
var
  lFrom : TtstPerObjAbs;
  lTo  : TtstPerObjAbs;
begin
  lFrom := TtstPerObjAbs.Create;
  try
    SetTSTPerObjAbs(lFrom, 1);
    lTo  := lFrom.Clone;
    try
      SetTSTPerObjAbs(lTo, 2);
      lTo.Assign(lFrom);
      CheckTSTPerObjAbs(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
      Check(nil = lTo.Owner, 'Failed on Owner');
    finally
      lTo.Free;
    end;
  finally
    lFrom.Free;
  end;
end;


procedure TTestTIObject.Deleted;
var
  lData : TtstPerObjList;
  lGroup : TtstPerObjAbs;
  lItem : TtstPerObjOwnedObj;
  lMap  : TtstPerObjMappedObj;
begin
  lData := TtstPerObjList.Create;
  try
    Check(Not lData.Deleted, 'Failed on 1');
    lData.ObjectState := posPK;
    Check(Not lData.Deleted, 'Failed on 2');
    lData.ObjectState := posClean;
    Check(Not lData.Deleted, 'Failed on 3');
    lData.ObjectState := posCreate;
    Check(not lData.Deleted, 'Failed on 4');
    lData.ObjectState := posUpdate;
    Check(not lData.Deleted, 'Failed on 5');
    lData.ObjectState := posDelete;
    Check(lData.Deleted, 'Failed on 6');
    lData.ObjectState := posDeleted;
    Check(lData.Deleted, 'Failed on 7');
    lData.ObjectState := posClean;
    Check(Not lData.Deleted, 'Failed on 8');

    lGroup := TtstPerObjAbs.Create;
    lData.Add(lGroup);
    lItem := TtstPerObjOwnedObj.Create;
    lData.Add(lItem);

    lData.Deleted := true;
    Check(lData.Deleted, 'Failed on 9');
    Check(lGroup.Deleted, 'Failed on 10');
    Check(lItem.Deleted, 'Failed on 11');

  finally
    lData.Free;
  end;

  lMap  := TtstPerObjMappedObj.Create;
  try
    lMap.ObjectState := posClean;
    lGroup := TtstPerObjAbs.Create;
    try
      lGroup.ObjectState := posClean;
      lMap.ObjProp := lGroup;
      lMap.Deleted := true;
      Check(lMap.ObjectState  = posDelete, 'Failed on 12');
      // Mapped objects should not have their object state touched
      // during a delete, only owned objects should.
      Check(lGroup.ObjectState = posClean, 'Failed on 13');
    finally
      lGroup.Free;
    end;
  finally
    lMap.Free;
  end;
end;


procedure TTestTIObject.Dirty;
var
  lData : TtstPerObjList;
  lGroup : TtstPerObjAbs;
  lItem : TtstPerObjOwnedObj;
begin
  { Test reading and writing to the .Dirty property }
  lData := TtstPerObjList.Create;
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

    lGroup := TtstPerObjAbs.Create;
    lData.Add(lGroup);
    Check(Not lData.Dirty, 'Failed on 9');
    lGroup.ObjectState := posUpdate;
    Check(lData.Dirty, 'Failed on 10');
    lGroup.ObjectState := posClean;

    lItem := TtstPerObjOwnedObj.Create;
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


procedure TTestTIObject.Equals;
var
  lObj1     : TtstPerObjAbs;
  lObj2     : TtstPerObjAbs;
  lObj3     : TtstPerObjAbs;
  lObj4     : TtstPerObjAbs;
  lList1    : TtstPerObjList;
  lList2    : TtstPerObjList;
  lOwnedObj1 : TtstPerObjOwnedObj;
  lOwnedObj2 : TtstPerObjOwnedObj;
begin
  lObj1 := TtstPerObjAbs.Create;
  try
    lObj2 := TtstPerObjAbs.Create;
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

  lList1 := TtstPerObjList.Create;
  try
    lList2 := TtstPerObjList.Create;
    try
      Check(lList1.Equals(lList2), 'Failed on 20');
      Check(lList2.Equals(lList1), 'Failed on 21');

      lObj1 := TtstPerObjAbs.Create;
      lList1.Add(lObj1);
      Check(not lList1.Equals(lList2), 'Failed on 22');
      Check(not lList2.Equals(lList1), 'Failed on 23');

      lObj2 := TtstPerObjAbs.Create;
      lList2.Add(lObj2);
      Check(lList1.Equals(lList2), 'Failed on 24');
      Check(lList2.Equals(lList1), 'Failed on 25');

      lObj3 := TtstPerObjAbs.Create;
      lList1.Add(lObj3);
      Check(not lList1.Equals(lList2), 'Failed on 26');
      Check(not lList2.Equals(lList1), 'Failed on 27');

      lObj4 := TtstPerObjAbs.Create;
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

  lOwnedObj1 := TtstPerObjOwnedObj.Create;
  try
    lOwnedObj2 := TtstPerObjOwnedObj.Create;
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


procedure TTestTIObject.FindAllWithMethod;
var
  lData : TtstPerObjList;
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
        CheckEquals(IntToStr(i), TtstPerObjAbs(lList.Items[i]).OID.AsString, 'Failed on OID');
    finally
      lList.Free;
    end;
  finally
    lData.Free;
  end;
end;


procedure TTestTIObject.FindByOID;
var
  lData : TtstPerObjList;
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
      lToFind := (lData.Items[i] as TtstPerObjOwnedObj).ObjProp;
      lFound := lData.Find(lToFind.OID);
      CheckNull(lFound, 'Failed on 3');
    end;
  finally
    lData.Free;
  end;
end;


function TTestTIObject.CreateTestDataList : TtstPerObjList;
var
  lItem : TtstPerObjOwnedObj;
  i     : integer;
begin
  result := TtstPerObjList.Create;
  SetTSTPerObjList(Result, -1);
  for i := 0 to 4 do
  begin
    lItem := TtstPerObjOwnedObj.Create;
    Result.Add(lItem);
    SetTSTPerObjAbs(lItem, i*2);
    SetTSTPerObjAbs(lItem.ObjProp, i*2+1);
  end;
end;


procedure TTestTIObject.FindWithMethod;
var
  lData : TtstPerObjList;
  lFound : TtstPerObjAbs;
begin
  lData := CreateTestDataList;
  try
    lFound := lData.Find(TstFindMethod) as TtstPerObjAbs;
    Check(Assigned(lFound), 'Find failed');
    CheckEquals('1', lFound.OID.AsString, 'Find failed');
  finally
    lData.Free;
  end;
end;


procedure TTestTIObject.TstFindMethod(AObject: TtiObject; var AFound: boolean);
begin
  AFound := false;
  if not (AObject is TtstPerObjAbs) then
    Exit; //==>
  AFound := TtstPerObjAbs(AObject).IntProp = 1;
end;


procedure TTestTIObject.FindWithMethodAndParam;
var
  lData : TtstPerObjList;
  lFound : TtstPerObjAbs;
begin
  lData := CreateTestDataList;
  try
    lFound := lData.Find(TstFindMethodWithParam, Pointer(1)) as TtstPerObjAbs;
    Check(Assigned(lFound), 'Find failed');
    CheckEquals('1', lFound.OID.AsString, 'Find failed');

    lFound := lData.Find(TstFindMethodWithParam, Pointer(100)) as TtstPerObjAbs;
    Check(not Assigned(lFound), 'Found when it shoud have failed');

  finally
    lData.Free;
  end;
end;


procedure TTestTIObject.Index;
var
  lData : TtstPerObjList;
  lItem0 : TtstPerObjAbs;
  lItem1 : TtstPerObjAbs;
  lItem2 : TtstPerObjAbs;
  lItem3: TtstPerObjAbs;
begin
  lData := TtstPerObjList.Create;
  try
    lData.ObjectState := posClean;
    lItem0 := TtstPerObjAbs.Create;
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
    lItem1 := TtstPerObjAbs.Create;
    lItem1.ObjectState := posClean;
    lData.Add(lItem1);
    CheckEquals(1, lItem1.Index);
    lItem2 := TtstPerObjAbs.Create;
    lItem2.ObjectState := posClean;
    lData.Insert(lItem1, lItem2);

    CheckEquals(0, lItem0.Index);
    CheckEquals(1, lItem2.Index);
    CheckEquals(2, lItem1.Index);

    lItem3 := TtstPerObjAbs.Create;
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


procedure TTestTIObject.ObjectStateAsString;
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


procedure TTestTIObject.Owner;
var
  lGroup : TtiOPFTestGroup;
  lItem : TtiOPFTestItem;
begin
  // This is really a bit trivial, but here goes anyway...
  lGroup := TtiOPFTestGroup.Create;
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


procedure TTestTIObject.TopOfHierarchy;
var
  lData : TtstPerObjList;
  lGroup : TtstPerObjAbs;
  lItem : TtstPerObjOwnedObj;
begin
  lData := TtstPerObjList.Create;
  try
    lGroup := TtstPerObjAbs.Create;
    lData.Add(lGroup);
    lItem := TtstPerObjOwnedObj.Create;
    lData.Add(lItem);
    Check(lData.TopOfHierarchy = lData, 'Failed on 1');
    Check(lGroup.TopOfHierarchy = lData, 'Failed on 2');
    Check(lItem.TopOfHierarchy = lData, 'Failed on 3');
  finally
    lData.Free;
  end;
end;


procedure TTestTIObject.IsUnique;
var
  lTop : TtstPerObjList;
  lItem : TtstPerObjAbs;
begin
  lTop := TtstPerObjList.Create;
  try
    lTop.OID.AsString := '1';
    Check(lTop.IsUnique(lTop), 'Failed on test 1');
    lItem := TtstPerObjAbs.Create;
    lItem.OID.AsString := '1';
    Check(not lTop.IsUnique(lItem), 'Failed on test 2');
    lTop.Add(lItem);
    Check(not lTop.IsUnique(lItem), 'Failed on test 3');
    lItem := TtstPerObjAbs.Create;
    lItem.OID.AsString := '2';
    Check(lTop.IsUnique(lItem), 'Failed on test 4');
    lTop.Add(lItem);
    Check(lTop.IsUnique(lItem), 'Failed on test 5');
  finally
    lTop.Free;
  end;
end;


procedure TTestTIObject.CloneCompound;
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


procedure TTestTIObject.CloneList;
var
  lFrom : TtstPerObjList;
  lItem : TtstPerObjAbs;
  lTo  : TtstPerObjList;
  i: integer;
begin
  lFrom := TtstPerObjList.Create;
  try
    lFrom.OwnsObjects := true;
    SetTSTPerObjList(lFrom, 1);
    lItem := TtstPerObjAbs.Create;
    SetTSTPerObjAbs(lItem, 2);
    lFrom.Add(lItem);
    lTo  := lFrom.Clone;
    try
      CheckTSTPerObjList(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
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

  lFrom := TtstPerObjList.Create;
  try
    lFrom.OwnsObjects := false;
    lFrom.AutoSetItemOwner := false;
    SetTSTPerObjList(lFrom, 1);
    lItem := TtstPerObjAbs.Create;
    SetTSTPerObjAbs(lItem, 2);
    lFrom.Add(lItem);
    lTo  := lFrom.Clone;
    try
      CheckTSTPerObjList(lTo, 1);
      Check(posEmpty = lTo.ObjectState, 'Failed on ObjectState');
      CheckEquals(true, lTo.SelfIterate,'Failed on SelfIterate');
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


procedure TTestTIObject.TstFindMethodWithParam(AObject: TtiObject;
  var AFound: boolean; AUserContext: Pointer);
begin
  AFound := false;
  if not (AObject is TtstPerObjAbs) then
    Exit; //==>
  AFound := TtstPerObjAbs(AObject).IntProp = Integer(AUserContext);
end;


procedure TTestTIObject.TstFindAll(AObject : TtiObject; var AFound : boolean);
begin
  AFound := false;
  if not (AObject is TtstPerObjAbs) then
    Exit; //==>
  AFound := TtstPerObjAbs(AObject).IntProp <= 5;
end;


procedure TTestTIObject.IsValid;
var
  lData : TtstPerObjAbs;
  lErrors : TtiObjectErrors;
begin
  lData := TtstPerObjAbs.Create;
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


procedure TTestTIObject.SetPropValue;
var
  lData : TtstPerObjAbs;
begin
  lData := TtstPerObjAbs.Create;
  try
    lData.PropValue['StrProp']:= 'test';
    CheckEquals('test', lData.StrProp, 'StrProp');

    lData.PropValue['IntProp']:= 1234;
    CheckEquals(1234, lData.IntProp, 'IntProp');

    lData.PropValue['FloatProp']:= 1234.5678;
    CheckEquals(1234.5678, lData.FloatProp, cDUnitTestFloatPrecision, 'FloatProp');

    lData.PropValue['DateProp']:= EncodeDate(2003, 01, 01);
    CheckEquals(EncodeDate(2003, 01, 01), lData.DateProp, 0.00001, 'DateProp');

    lData.PropValue['BoolProp']:= true;
    CheckEquals(true, lData.BoolProp, 'BoolProp true');
    lData.PropValue['BoolProp']:= false;
    CheckEquals(false, lData.BoolProp, 'BoolProp false');

    lData.PropValue['BoolProp']:= 'true';
    CheckEquals(true, lData.BoolProp, 'BoolProp ''true''');
    lData.PropValue['BoolProp']:= 'false';
    CheckEquals(false, lData.BoolProp, 'BoolProp ''false''');

    lData.PropValue['BoolProp']:= 'TRUE';
    CheckEquals(true, lData.BoolProp, 'BoolProp ''TRUE''');
    lData.PropValue['BoolProp']:= 'FALSE';
    CheckEquals(false, lData.BoolProp, 'BoolProp ''FALSE''');

    lData.PropValue['BoolProp']:= 'True';
    CheckEquals(true, lData.BoolProp, 'BoolProp ''True''');
    lData.PropValue['BoolProp']:= 'False';
    CheckEquals(false, lData.BoolProp, 'BoolProp ''False''');

    lData.PropValue['BoolProp']:= 'T';
    CheckEquals(true, lData.BoolProp, 'BoolProp ''T''');
    lData.PropValue['BoolProp']:= 'F';
    CheckEquals(false, lData.BoolProp, 'BoolProp ''F''');

    lData.PropValue['BoolProp']:= 1;
    CheckEquals(true, lData.BoolProp, 'BoolProp 1');
    lData.PropValue['BoolProp']:= 0;
    CheckEquals(false, lData.BoolProp, 'BoolProp 0');

    lData.PropValue['BoolProp']:= '1';
    CheckEquals(true, lData.BoolProp, 'BoolProp ''1''');
    lData.PropValue['BoolProp']:= '0';
    CheckEquals(false, lData.BoolProp, 'BoolProp ''0''');

    lData.PropValue['OrdProp']:= tstOrdProp_2;
    Check(tstOrdProp_2 = lData.OrdProp, 'OrdProp');
  finally
    lData.Free;
  end;
end;


procedure TTestTIObject.GetPropValue;
var
  lData : TtstPerObjAbs;
  lOrd : String;
  lStr : string;      
  lInt : integer;
  lFloat : extended;
begin
  lData := TtstPerObjAbs.Create;
  try
    lData.StrProp := 'test';
    lStr := lData.PropValue['StrProp'];
    CheckEquals('test', lStr, 'StrProp');

    lData.IntProp := 1234;
    lInt := lData.PropValue['IntProp'];
    CheckEquals(1234, lInt, 'IntProp');

    lData.FloatProp := 1234.5678;
    lFloat := lData.PropValue['FloatProp'];
    CheckEquals(1234.5678, lFloat, cDUnitTestFloatPrecision, 'FloatProp');

    lData.DateProp := EncodeDate(2003, 01, 01);
    CheckEquals(EncodeDate(2003, 01, 01), lData.PropValue['DateProp'], 0.00001, 'DateProp');

    lData.BoolProp := true;
    CheckEquals(true, lData.PropValue['BoolProp'], 'BoolProp');

    // This is a bit of a mess, but it was the
    // best I could come up with to get PropValue working
    // with ordinals.
    lData.OrdProp := tstOrdProp_2;
    lOrd := lData.PropValue['OrdProp'];
    CheckEquals('tstOrdProp_2',
      GetEnumName(TypeInfo(TtstOrdProp),
                   ord(tstOrdProp_2)),
      'OrdProp');
  finally
    lData.Free;
  end;
end;


procedure TTestTIObject.IsReadWriteProp;
var
  lData : TtstPerObjAbs;
begin
  lData := TtstPerObjAbs.Create;
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


procedure TTestTIObject.FieldName;
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


procedure TTestTIObject.FieldBoolean;
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


procedure TTestTIObject.FieldDateTime;
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


procedure TTestTIObject.FieldFloat;
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


procedure TTestTIObject.FieldInt64;
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


procedure TTestTIObject.FieldString;
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


procedure TTestTIObject.PropType;
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


procedure TTestTIObject.FieldBoolean_Assign;
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

procedure TTestTIObject.FieldBoolean_Equals;
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


procedure TTestTIObject.FieldDateTime_Assign;
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

procedure TTestTIObject.FieldDateTime_YearsMonthsDays;
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

procedure TTestTIObject.FieldDateTime_HoursMinutesSeconds;
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

procedure TTestTIObject.FieldDateTime_Equals;
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


procedure TTestTIObject.FieldFloat_Assign;
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

procedure TTestTIObject.FieldFloat_Equals;
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


procedure TTestTIObject.FieldInt64_Assign;
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

procedure TTestTIObject.FieldInt64_Equals;
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


procedure TTestTIObject.FieldString_Assign;
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

procedure TTestTIObject.FieldString_Equals;
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


procedure TTestTIObject.Parent_TtiObject;
var
  LObject1: TtiObject;
  LObject2: TtiObject;
begin
  LObject1:= TtiObject.Create;
  try
    LObject2:= TtiObject.Create;
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

procedure TTestTIObject.Parent_TtiObjectList;
var
  LObject1: TtiObject;
  LObject2: TtiObject;
  LList:    TtiObjectList;
begin
  LObject1:= TtiObject.Create;
  try
    LList:= TtiObjectList.Create;
    try
      LList.Owner:= LObject1;
      LObject2:= TtiObject.Create;
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

procedure TTestTIObject.PropCount;
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


procedure TTestTIObjectList.Add;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
  i : integer;
  idx: integer;
begin
  lList := TtstPerObjList.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create;
      idx := lList.Add(lData);
      CheckEquals(i, lList.List.IndexOf(lData), 'Failed on ' + IntToStr(i));
      CheckEquals(idx, i, 'Failed on ' + IntToStr(i));
      CheckEquals(idx, lList.List.IndexOf(lData), 'Failed on ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;


procedure TTestTIObjectList.AutoSetItemOwner;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lList.Add(lData1);
    CheckSame(lList, lData1.Owner, 'Failed on test 1');
    lList.AutoSetItemOwner := false;
    lData2 := TtstPerObjAbs.Create;
    lList.Add(lData2);
    CheckNull(lData2.Owner, 'Failed on test 2');
  finally
    lList.Free;
  end;
end;


procedure TTestTIObjectList.PropToStrings;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
  lData3     : TtstPerObjAbs;
  lsl        : TStringList;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData2 := TtstPerObjAbs.Create;
    lData3 := TtstPerObjAbs.Create;
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


procedure TTestTIObjectList.Clear;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData := TtstPerObjAbs.Create;
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
    lData := TtstPerObjAbs.Create;
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


procedure TTestTIObjectList.Count;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
  i : integer;
begin
  lList := TtstPerObjList.Create;
  try
    CheckEquals(0, lList.Count, 'Failed on 0');
    for i := 1 to 10 do
    begin
      lData := TtstPerObjAbs.Create;
      lList.Add(lData);
      CheckEquals(i, lList.Count, 'Failed on ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;


procedure TTestTIObjectList.CountNotDeleted;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
  i : integer;
begin
  lList := TtstPerObjList.Create;
  try
    CheckEquals(0, lList.Count, 'Failed on 0');
    for i := 1 to 10 do
    begin
      lData := TtstPerObjAbs.Create;
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


function TTestTIObjectList.CreateList: TtstPerObjList;
var
  xItem : TtstPerObjAbs;
begin
  result:=TtstPerObjList.Create;
  // 1
  xItem:=TtstPerObjAbs.Create;
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
  xItem:=TtstPerObjAbs.Create;
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
  xItem:=TtstPerObjAbs.Create;
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
  xItem:=TtstPerObjAbs.Create;
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
  xItem:=TtstPerObjAbs.Create;
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


procedure TTestTIObjectList.Delete;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
  i : integer;
begin
  lList := TtstPerObjList.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.Empty;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.Extract;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
  i : integer;
begin
  lList := TtstPerObjList.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.FindByProps_DirectValue;
var
  LList : TtstPerObjList;
  LItem : TtstPerObjAbs;
begin
  LList:=CreateList;
  try
    // by boolean
    CheckEquals(5,LList.Count,'Wrong list! - Wrong count!');
    LItem:=TtstPerObjAbs(LList.FindByProps(['BoolProp'],[true]));
    CheckNotNull(LItem,'Find By Boolean value - NO result when expected');
    CheckEquals('1',LItem.OID.AsString,'Find By Boolean value - wrong object!');
    LItem:=TtstPerObjAbs(LList.FindByProps(['BoolProp'],[false]));
    CheckNull(LItem,'Find By Boolean value - result when not expected');
    // by int
    LItem:=TtstPerObjAbs(LList.FindByProps(['IntProp'],[-4942345]));
    CheckNotNull(LItem,'Find By Integer value - NO result when expected -4942345');
    CheckEquals('2',LItem.OID.AsString,'Find By Integer value - wrong object! -4942345');
    LItem:=TtstPerObjAbs(LList.FindByProps(['IntProp'],[7878787]));
    CheckNull(LItem,'Find By Integer value - result when not expected 7878787');
    // by string
    LItem:=TtstPerObjAbs(LList.FindByProps(['StrProp'],['TEST VALUE']));
    CheckNotNull(LItem,'Find By String value (case sensitive) - NO result when expected <TEST VALUE>');
    CheckEquals('3',LItem.OID.AsString,'Find By String value (case sensitive) - wrong object! <TEST VALUE>');
    LItem:=TtstPerObjAbs(LList.FindByProps(['StrProp'],['Test Value'],false));
    CheckNotNull(LItem,'Find By String value (not case sensitive) - NO result when expected <Test AValue>');
    CheckEquals('3',LItem.OID.AsString,'Find By String value (not case sensitive) - wrong object! <Test AValue>');
    LItem:=TtstPerObjAbs(LList.FindByProps(['StrProp'],['Test Value'],true));
    CheckNull(LItem,'Find By String value (case sensitive) - result when not expected <Test AValue>');
    LItem:=TtstPerObjAbs(LList.FindByProps(['StrProp'],['_Test Value_'],false));
    CheckNull(LItem,'Find By String value (not case sensitive) - result when not expected <_Test Value_>');
    // float
    LItem:=TtstPerObjAbs(LList.FindByProps(['FloatProp'],[235465.34321]));
    CheckNotNull(LItem,'Find By Float value - NO result when expected 235465.34321');
    CheckEquals('4',LItem.OID.AsString,'Find By Float value - wrong object! 235465.34321');
    LItem:=TtstPerObjAbs(LList.FindByProps(['FloatProp'],[235465.3432]));
    CheckNull(LItem,'Find By Float value - result when not expected 235465.3432');
    // by Date
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[EncodeDate(2004,05,12)]));
    CheckNotNull(LItem,'Find By Date value 2004-05-12 - NO result when expected');
    CheckEquals('5',LItem.OID.AsString,'Find By Date value - wrong object!');
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[0]));
    CheckNotNull(LItem,'Find By Date value 0 - NO result when expected');
    CheckEquals('4',LItem.OID.AsString,'Find By Date value - wrong object!');
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[EncodeDate(2004,05,13)]));
    CheckNull(LItem,'Find By Date value 2004-05-13 - result when not expected');
    // by DateTime
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[EncodeDate(2000,02,03)+EncodeTime(4,5,6,7)]));
    CheckNotNull(LItem,'Find By DateTime value 2000-02-03, 4:05:06.007 - NO result when expected');
    CheckEquals('2',LItem.OID.AsString,'Find By Integer value 2000-02-03, 4:05:06.007 - wrong object!');
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[EncodeDate(1999,02,06)+EncodeTime(20,0,0,1)]));
    CheckNull(LItem,'Find By DateTime value 1999-02-06, 20:00:00.001 - result when not expected');
  finally
    LList.Free;
  end;
end;

procedure TTestTIObjectList.FindByProps_TypedValue;
var
  LList : TtstPerObjList;
  LItem : TtstPerObjAbs;
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
    LItem:=TtstPerObjAbs(LList.FindByProps(['BoolProp'],[LBool]));
    CheckNotNull(LItem,'Find By Boolean value - NO result when expected');
    CheckEquals('1',LItem.OID.AsString,'Find By Boolean value - wrong object!');
    LBool:=false;
    LItem:=TtstPerObjAbs(LList.FindByProps(['BoolProp'],[LBool]));
    CheckNull(LItem,'Find By Boolean value - result when not expected');
    // by int
    LInt:=-4942345;
    LItem:=TtstPerObjAbs(LList.FindByProps(['IntProp'],[LInt]));
    CheckNotNull(LItem,'Find By Integer value - NO result when expected -4942345');
    CheckEquals('2',LItem.OID.AsString,'Find By Integer value - wrong object! -4942345');
    LInt:=7878787;
    LItem:=TtstPerObjAbs(LList.FindByProps(['IntProp'],[LInt]));
    CheckNull(LItem,'Find By Integer value - result when not expected 7878787');
    // by string
    LStr:='TEST VALUE';
    LItem:=TtstPerObjAbs(LList.FindByProps(['StrProp'],[LStr]));
    CheckNotNull(LItem,'Find By String value (case sensitive) - NO result when expected <TEST VALUE>');
    CheckEquals('3',LItem.OID.AsString,'Find By String value (case sensitive) - wrong object! <TEST VALUE>');

    LStr:='Test Value';
    LItem:=TtstPerObjAbs(LList.FindByProps(['StrProp'],[LStr],false));
    CheckNotNull(LItem,'Find By String value (not case sensitive) - NO result when expected <Test AValue>');
    CheckEquals('3',LItem.OID.AsString,'Find By String value (not case sensitive) - wrong object! <Test AValue>');
    LItem:=TtstPerObjAbs(LList.FindByProps(['StrProp'],[LStr],true));
    CheckNull(LItem,'Find By String value (case sensitive) - result when not expected <Test AValue>');

    LStr:='_Test Value_';
    LItem:=TtstPerObjAbs(LList.FindByProps(['StrProp'],[LStr],false));
    CheckNull(LItem,'Find By String value (not case sensitive) - result when not expected <_Test Value_>');

    // float
    LFloat:=235465.34321;
    LItem:=TtstPerObjAbs(LList.FindByProps(['FloatProp'],[LFloat]));
    CheckNotNull(LItem,'Find By Float value - NO result when expected 235465.34321');
    CheckEquals('4',LItem.OID.AsString,'Find By Float value - wrong object! 235465.34321');
    LFloat:=235465.3432;
    LItem:=TtstPerObjAbs(LList.FindByProps(['FloatProp'],[LFloat]));
    CheckNull(LItem,'Find By Float value - result when not expected 235465.3432');

    // by Date
    LDate:=EncodeDate(2004,05,12);
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[LDate]));
    CheckNotNull(LItem,'Find By Date value 2004-05-12 - NO result when expected');
    CheckEquals('5',LItem.OID.AsString,'Find By Date value - wrong object!');

    LDate:=0;
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[LDate]));
    CheckNotNull(LItem,'Find By Date value 0 - NO result when expected');
    CheckEquals('4',LItem.OID.AsString,'Find By Date value - wrong object!');

    LDate:=EncodeDate(2004,05,13);
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[LDate]));
    CheckNull(LItem,'Find By Date value 2004-05-13 - result when not expected');

    // by DateTime
    LDate:=EncodeDate(2000,02,03)+EncodeTime(4,5,6,7);
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[LDate]));
    CheckNotNull(LItem,'Find By DateTime value 2000-02-03, 4:05:06.007 - NO result when expected');
    CheckEquals('2',LItem.OID.AsString,'Find By Integer value 2000-02-03, 4:05:06.007 - wrong object!');

    LDate:=EncodeDate(1999,02,06)+EncodeTime(20,0,0,1);
    LItem:=TtstPerObjAbs(LList.FindByProps(['DateProp'],[LDate]));
    CheckNull(LItem,'Find By DateTime value 1999-02-06, 20:00:00.001 - result when not expected');
  finally
    LList.Free;
  end;
end;

procedure TTestTIObjectList.First;
var
  lList : TtstPerObjList;
  lData0 : TtstPerObjAbs;
  lData1 : TtstPerObjAbs;
  lData2 : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData0 := TtstPerObjAbs.Create;
    lList.Add(lData0);
    lData1 := TtstPerObjAbs.Create;
    lList.Add(lData1);
    lData2 := TtstPerObjAbs.Create;
    lList.Add(lData2);
    CheckSame(lData0, lList.First, 'Failed on First');
    lData0.Deleted := true;
    // This test will fail. Should it?
    // CheckSame(lData1, lList.First, 'Failed on First when the first was deleted');
  finally
    lList.Free;
  end;
end;

procedure TTestTIObjectList.DoForEachMethod(AData : TtiObject);
begin
  Assert(AData is TtstPerObjAbs, 'AData not a TtstPerObjAbs');
  TtstPerObjAbs(AData).StrProp := 'tested';
end;

procedure TTestTIObjectList.ForEachMethod;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
  lData3     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData2 := TtstPerObjAbs.Create;
    lData3 := TtstPerObjAbs.Create;
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

procedure DoForEachMethodRegular(AData : TtiObject);
begin
  Assert(AData is TtstPerObjAbs, 'AData not a TtstPerObjAbs');
  TtstPerObjAbs(AData).StrProp := 'tested';
end;

procedure TTestTIObjectList.ForEachMethodRegular;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
  lData3     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData2 := TtstPerObjAbs.Create;
    lData3 := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.FreeDeleted;
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

procedure TTestTIObjectList.tiListToStreamDefault;
var
  lStream : TStringStream;
  lList  : TTestListOfPersistents;
begin
  lStream := TStringStream.Create('');
  try
    lList := TTestListOfPersistents.Create;
    try
      tiObject.tiListToStream(lStream, lList);
      CheckEquals(Length(lList.AsString), lStream.Size, 'Failing on 1');
      CheckEquals(lList.AsString, lStream.DataString, 'Failing on 2');
    finally
      lList.Free;
    end;
  finally
    lStream.Free;
  end;
end;

procedure TTestTIObjectList.tiListToStreamDelims;
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
        CheckEquals(Length(lList.AsString(#9, '|', lFields)), lStream.Size);
        CheckEquals(lList.AsString(#9, '|', lFields), lStream.DataString);
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

procedure TTestTIObjectList.tiListToStreamFields;
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
        CheckEquals(Length(lList.AsString(',', #13#10, lFields)), lStream.Size);
        CheckEquals(lList.AsString(',', #13#10, lFields), lStream.DataString);
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

procedure TTestTIObjectList.tiListToCSVDefault;
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

  CheckEquals(Length(lString1), Length(lString2), 'Failed on 1');
  CheckEquals(lString1, lString2, 'Failed on 2');
end;

procedure TTestTIObjectList.tiListToCSVFields;
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

  CheckEquals(Length(lString1), Length(lString2), 'Length');
  CheckEquals(lString1, lString2, 'String');
end;

procedure TTestTIObjectList.IndexOf;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lList.Add(lData1);
    lData2 := TtstPerObjAbs.Create;
    lList.Add(lData2);
    CheckEquals(0, lList.IndexOf(lData1), 'Failed on test 1');
    CheckEquals(1, lList.IndexOf(lData2), 'Failed on test 2');
  finally
    lList.Free;
  end;
end;

procedure TTestTIObjectList.InsertByObject;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
  lData3     : TtstPerObjAbs;
  lData4     : TtstPerObjAbs;
  lData5     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData1.ObjectState := posClean;
    lList.Add(lData1);
    lData2 := TtstPerObjAbs.Create;
    lData2.ObjectState := posClean;
    lList.Add(lData2);
    lData3 := TtstPerObjAbs.Create;
    lData3.ObjectState := posClean;
    lData4 := TtstPerObjAbs.Create;
    lData4.ObjectState := posClean;
    lData5 := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.InsertByIndex;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
  lData3     : TtstPerObjAbs;
  lData4     : TtstPerObjAbs;
  lData5     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData1.ObjectState := posClean;
    lList.Add(lData1);
    lData2 := TtstPerObjAbs.Create;
    lData2.ObjectState := posClean;
    lList.Add(lData2);
    lData3 := TtstPerObjAbs.Create;
    lData3.ObjectState := posClean;
    lData4 := TtstPerObjAbs.Create;
    lData4.ObjectState := posClean;
    lData5 := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.ItemOwner;
var
  lList      : TtstPerObjList;
  lItemOwner1 : TtstPerObjAbs;
  lData      : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    CheckSame(lList, lList.ItemOwner, 'Failed on test 1');
    lData := TtstPerObjAbs.Create;
    lList.Add(lData);
    CheckSame(lList, lData.Owner, 'Failed on test 2');
    lItemOwner1 := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.Items;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
  i : integer;
begin
  lList := TtstPerObjList.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create;
      lList.Add(lData);
      CheckSame(lData, lList.Items[i], 'Failed on ' + IntToStr(i));
    end;
  finally
    lList.Free;
  end;
end;

procedure TTestTIObjectList.Last;
var
  lList : TtstPerObjList;
  lData0 : TtstPerObjAbs;
  lData1 : TtstPerObjAbs;
  lData2 : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData0 := TtstPerObjAbs.Create;
    lList.Add(lData0);
    lData1 := TtstPerObjAbs.Create;
    lList.Add(lData1);
    lData2 := TtstPerObjAbs.Create;
    lList.Add(lData2);
    CheckSame(lData2, lList.Last, 'Failed on Last');
    lData0.Deleted := true;
    Check(lData1 <> lList.First, 'Failed on First when the first was deleted');
  finally
    lList.Free;
  end;
end;

procedure TTestTIObjectList.LastExcludeDeleted;
var
  lList : TtstPerObjList;
  lData0 : TtstPerObjAbs;
  lData1 : TtstPerObjAbs;
  lData2 : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData0 := TtstPerObjAbs.Create;
    lList.Add(lData0);
    lData1 := TtstPerObjAbs.Create;
    lList.Add(lData1);
    lData2 := TtstPerObjAbs.Create;
    lList.Add(lData2);
    CheckSame(lData2, lList.LastExcludeDeleted, 'Failed on LastExcludeDeleted');
    lData2.Deleted := true;
    CheckSame(lData1, lList.LastExcludeDeleted, 'Failed on LastExcludeDeleted');
  finally
    lList.Free;
  end;
end;

procedure TTestTIObjectList.MarkListItemsDirty;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData2 := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.MarkListItemsForDeletion;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData2 := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.OwnsObjects;
var
  lList : TtstPerObjList;
begin
  // A trivial test because OwnsObjects is delegated to the
  // owned instance of TObjectList
  lList := TtstPerObjList.Create;
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

procedure TTestTIObjectList.Remove;
var
  lList : TtstPerObjList;
  lData : TtstPerObjAbs;
  i : integer;
begin
  lList := TtstPerObjList.Create;
  try
    for i := 0 to 9 do
    begin
      lData := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.SortByOID;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
  lData3     : TtstPerObjAbs;
  lData4     : TtstPerObjAbs;
  lData5     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData1.OID.AsString := '1';
    lData2 := TtstPerObjAbs.Create;
    lData2.OID.AsString := '2';
    lData3 := TtstPerObjAbs.Create;
    lData3.OID.AsString := '3';
    lData4 := TtstPerObjAbs.Create;
    lData4.OID.AsString := '4';
    lData5 := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.SortByProps;
var
  lList      : TtstPerObjList;
  lData1     : TtstPerObjAbs;
  lData2     : TtstPerObjAbs;
  lData3     : TtstPerObjAbs;
  lData4     : TtstPerObjAbs;
  lData5     : TtstPerObjAbs;
begin
  lList := TtstPerObjList.Create;
  try
    lData1 := TtstPerObjAbs.Create;
    lData1.StrProp := '1';
    lData2 := TtstPerObjAbs.Create;
    lData2.StrProp := '2';
    lData3 := TtstPerObjAbs.Create;
    lData3.StrProp := '3';
    lData4 := TtstPerObjAbs.Create;
    lData4.StrProp := '4';
    lData5 := TtstPerObjAbs.Create;
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

procedure TTestTIObjectList.Find;
var
  lData : TtstPerObjList;
  lTarget: TtstPerObjAbs;
  lOID: TOID;
  i : integer;
begin
  lData := CreateTestData;
  try
    for i := 0 to lData.Count - 1 do
    begin
      lTarget := lData.Items[i];
      lOID := lTarget.OID;
      CheckSame(lTarget, lData.Find(lOID), '#' + lOID.AsString);

      lTarget := (lData.Items[i] as TtstPerObjOwnedObj).ObjProp;
      lOID := lTarget.OID;
      CheckNull(lData.Find(lOID), '#' + lOID.AsString);
    end;

    lData.SortByOID;
    for i := 0 to lData.Count - 1 do
    begin
      lTarget := lData.Items[i];
      lOID := lTarget.OID;
      CheckSame(lTarget, lData.Find(lOID), '#' + lOID.AsString);

      lTarget := (lData.Items[i] as TtstPerObjOwnedObj).ObjProp;
      lOID := lTarget.OID;
      CheckNull(lData.Find(lOID), '#' + lOID.AsString);
    end;
  finally
    lData.Free;
  end;
end;

procedure TTestTIObjectList.FindInHierarchy;
var
  lData : TtstPerObjList;
  lTarget: TtstPerObjAbs;
  lOID: TOID;
  i : integer;
begin
  lData := CreateTestData;
  try
    for i := 0 to lData.Count - 1 do
    begin
      lTarget := lData.Items[i];
      lOID := lTarget.OID;
      CheckSame(lTarget, lData.FindInHierarchy(lOID), '#' + lOID.AsString);

      lTarget := (lData.Items[i] as TtstPerObjOwnedObj).ObjProp;
      lOID := lTarget.OID;
      CheckSame(lTarget, lData.FindInHierarchy(lOID), '#' + lOID.AsString);
    end;
  finally
    lData.Free;
  end;
end;

{ TtstPerObjList }

function TtstPerObjList.Add(AObject: TtstPerObjAbs; ADefDispOrdr: boolean): integer;
begin
  result := inherited Add(AObject, ADefDispOrdr);
end;

function TtstPerObjList.Clone: TtstPerObjList;
begin
  result := TtstPerObjList(inherited Clone);
end;

function TtstPerObjList.GetItems(i: integer): TtstPerObjAbs;
begin
  result := TtstPerObjAbs(inherited GetItems(i));
end;

procedure TtstPerObjList.SetItems(i: integer; const AValue: TtstPerObjAbs);
begin
  inherited SetItems(i, AValue);
end;

{ TtstPerObjAbs }

function TtstPerObjAbs.Clone: TtstPerObjAbs;
begin
  result := TtstPerObjAbs(inherited Clone);
end;

constructor TtstPerObjAbs.Create;
begin
  inherited;
  Populate;
end;

constructor TtstPerObjAbs.CreateNew(const AOwner : TtiObject; const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
begin
  Create;
  Owner := AOwner as TtstPerObjList;
  OID.AsString := IntToStr(Integer(Self));
  ObjectState := posCreate;
end;

function TtstPerObjAbs.Equals(const AData: TtiObject): boolean;
var
  LData: TtstPerObjAbs;
begin
  Assert(AData.TestValid(TtstPerObjAbs), cErrorTIPerObjAbsTestValid);
  LData:= AData as TtstPerObjAbs;
  result :=
    (Self.StrProp   = LData.StrProp)   and
    (Self.IntProp   = LData.IntProp)   and
    (Self.FloatProp = LData.FloatProp) and
    (Self.DateProp  = LData.DateProp)  and
    (Self.BoolProp  = LData.BoolProp)  and
    (Self.OrdProp   = LData.OrdProp);
end;

function TtstPerObjAbs.GetOwner: TtstPerObjList;
begin
  result := TtstPerObjList(inherited GetOwner);
end;

function TtstPerObjAbs.IsValid(const AErrors: TtiObjectErrors): boolean;
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

procedure TtstPerObjAbs.Populate;
begin
  BoolProp := false;
  IntProp  := 1;
  FloatProp := 1.1111111;
  StrProp  := 'testing';
  DateProp := EncodeDate(2002,01,01);
  OrdProp  := tstOrdProp_1;
end;

procedure TtstPerObjAbs.SetOwner(const AValue: TtstPerObjList);
begin
  inherited SetOwner(AValue);
end;

{ TtstPerObjOwnedObj }

constructor TtstPerObjOwnedObj.Create;
begin
  inherited;
  FObjProp := TtstPerObjAbs.Create;
end;

destructor TtstPerObjOwnedObj.Destroy;
begin
  FObjProp.Free;
  inherited;
end;

function TtstPerObjOwnedObj.Equals(const AData: TtiObject): boolean;
var
  LData: TtstPerObjOwnedObj;
begin
  Assert(AData.TestValid(TtstPerObjOwnedObj), cErrorTIPerObjAbsTestValid);
  LData:= AData as TtstPerObjOwnedObj;
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

procedure TTestTIObjectList.FirstExcludeDeleted;
var
  LList : TtstPerObjList;
  LData0 : TtstPerObjAbs;
  LData1 : TtstPerObjAbs;
  LData2 : TtstPerObjAbs;
begin
  LList := TtstPerObjList.Create;
  try
    LData0 := TtstPerObjAbs.Create;
    LList.Add(LData0);
    LData1 := TtstPerObjAbs.Create;
    LList.Add(LData1);
    LData2 := TtstPerObjAbs.Create;
    LList.Add(LData2);
    CheckSame(LData0, lList.FirstExcludeDeleted, 'Failed on FirstExcludeDeleted');
    LData0.Deleted := true;
    CheckSame(LData1, lList.FirstExcludeDeleted, 'Failed on FirstExcludeDeleted');
  finally
    lList.Free;
  end;
end;

procedure TTestTIObject.AttachDetachObserver;
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

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtstSubject
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

procedure TtstSubject.SetName(const AValue: string);
begin
  BeginUpdate;
  FName := AValue;
  EndUpdate;
end;

// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
// *
// * TtstObserver
// *
// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

procedure TtstObserver.Update(ASubject: TtiObject);
begin
  inherited;
  FName := TtstSubject(ASubject).Name;
end;

procedure TTestTIObjectList.CompareWithEvent;
var
  LList1: TtiObjectList;
  LList2: TtiObjectList;
  LItemInBothSame1: TtstPerObjAbs;
  LItemInBothSame2: TtstPerObjAbs;
  LItemInBothNotSame1: TtstPerObjAbs;
  LItemInBothNotSame2: TtstPerObjAbs;
  LItemIn1Only: TtstPerObjAbs;
  LItemIn2Only: TtstPerObjAbs;
begin
  LList1:= TtiObjectList.Create;
  try
    LList2:= TtiObjectList.Create;
    try
      LItemInBothSame1  := TtstPerObjAbs.Create;
      LItemInBothSame2  := TtstPerObjAbs.Create;
      LItemInBothNotSame1:= TtstPerObjAbs.Create;
      LItemInBothNotSame2:= TtstPerObjAbs.Create;
      LItemIn1Only      := TtstPerObjAbs.Create;
      LItemIn2Only      := TtstPerObjAbs.Create;

      LItemInBothSame1.OID.AsString   := '1';
      LItemInBothSame2.OID.AsString   := '1';
      LItemInBothNotSame1.OID.AsString := '2';
      LItemInBothNotSame2.OID.AsString := '2';

      LItemIn1Only.OID.AsString       := '3';
      LItemIn2Only.OID.AsString       := '4';

      LItemInBothSame1.StrProp        := 'A';
      LItemInBothSame2.StrProp        := 'A';
      LItemInBothNotSame1.StrProp     := 'B';
      LItemInBothNotSame2.StrProp     := 'C';

      LItemIn1Only.StrProp            := 'D';
      LItemIn2Only.StrProp            := 'E';

      LList1.Add(LItemInBothSame1);
      LList2.Add(LItemInBothSame2);
      LList1.Add(LItemInBothNotSame1);
      LList2.Add(LItemInBothNotSame2);
      LList1.Add(LItemIn1Only);
      LList2.Add(LItemIn2Only);

      LList1.CompareWith(LList2, InBothAndEqualsEvent, InBothAndNotEqualsEvent, In1OnlyEvent, In2OnlyEvent);

      CheckEquals(1, FInBothAndEquals.Count,    'FInBothAndEquals.Count');
      CheckEquals(1, FInBothAndNotEquals.Count, 'FInBothAndNotEquals.Count');
      CheckEquals(1, FIn1Only.Count,            'FIn1Only.Count');
      CheckEquals(1, FIn2Only.Count,            'FIn2Only.Count');

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

constructor TTestTIObjectList.Create{$IFNDEF FPC}(AMethodName: string){$ENDIF};
begin
  inherited;
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
  
end;

destructor TTestTIObjectList.Destroy;
begin
  FInBothAndEquals.Free;
  FInBothAndNotEquals.Free;
  FIn1Only.Free;
  FIn2Only.Free;
  inherited;
end;

procedure TTestTIObjectList.In1OnlyEvent(AItem1, AItem2: TtiObject);
begin
  Assert(AItem1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AItem2=nil, 'AItem2 assigned');
  Assert(FIn1Only.TestValid, cErrorTIPerObjAbsTestValid);
  FIn1Only.Add(AItem1);
end;

procedure TTestTIObjectList.In2OnlyEvent(AItem1, AItem2: TtiObject);
begin
  Assert(AItem1=nil, 'AItem1 assigned');
  Assert(AItem2.TestValid, cErrorTIPerObjAbsTestValid);
  FIn2Only.Add(AItem2);
end;

procedure TTestTIObjectList.InBothAndEqualsEvent(AItem1, AItem2: TtiObject);
begin
  Assert(AItem1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AItem2.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(FInBothAndEquals.TestValid, cErrorTIPerObjAbsTestValid);
  FInBothAndEquals.Add(AItem1);
end;

procedure TTestTIObjectList.InBothAndNotEqualsEvent(AItem1, AItem2: TtiObject);
begin
  Assert(AItem1.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(AItem2.TestValid, cErrorTIPerObjAbsTestValid);
  Assert(FInBothAndNotEquals.TestValid, cErrorTIPerObjAbsTestValid);
  FInBothAndNotEquals.Add(AItem1);
end;

constructor TtstPerObjList.CreateNew(const ADatabaseName : string = ''; const APersistenceLayerName : string = '');
begin
  Create;
  OID.AsString := IntToStr(Integer(Self));
  ObjectState := posCreate;
end;

end.



