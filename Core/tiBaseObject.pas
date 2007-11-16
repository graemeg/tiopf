{: Unit containing base class used by all tiOPF classes - implements live
   object tracking to help detect memory leaks Cloned from IndySoap's
   abstract base class.}
unit tiBaseObject;

{$I tiDefines.inc}

interface

uses
 { The contents of this uses clause are controlled. Do not add any units
    to this list without first discussing with grahame@kestral.com.au

    The reason for this is that this unit is rather for finding leaks,
    and the less units it depends on, the more useful it is. Currently,
    this unit depends on classes.pas. It may be rewritten in the future to
    not depend on classes.pas. This allows for you to search for leaks
    involving classes defined there (such as TLists etc, which often
    leak (no, the TList etc classes themselves doesn't leak, but it is often
    forgotten when cleaning up....))
  }
  Classes;

type

  {:Abstract base class for all tiOPF objects. Implements live object tracking
    TestValid() can be used to confirm the object points to valid data.
    TtiBaseObject(s) can be created with or without reference counting.

    @example Paste the code below to into a test application to see how live
    object tracking works.<br><br>NOTE: You will require a conditional define
    object_tracking in your application.<br>
    <code>
    procedure TestObjectTracking;
    var
      LO: TtiBaseObject;
    begin
      LO:= TtiBaseObject.Create;
      Assert(LO.TestValid);
      LO.Free;
      Assert(not LO.TestValid);
    end;
    </code>

    }

  TtiBaseObject = class(TObject, IInterface)
  private
    {$IFDEF OBJECT_TRACKING}
    FSerialNo: integer;
    {$ENDIF}
    {$IFDEF REFERENCE_COUNTING}
    FRefCounting: Boolean;
    FRefCount: Integer;
    {$ENDIF}
  protected
    function    QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function    _AddRef: Integer; stdcall;
    function    _Release: Integer; stdcall;
  public
    constructor Create;
    destructor  Destroy; override;
    {$IFDEF REFERENCE_COUNTING}
    constructor CreateWithRefCounting;
    procedure   AfterConstruction; override;
    procedure   BeforeDestruction; override;
    class function NewInstance: TObject; override;
    {$ENDIF}

    {: Checks that the object is valid and in the list of current
       objects. If not, checks that self <> nil (which is not always very
       useful but still worth checking.)<br><br>
       Available if object_tracking is defined.<br><br>
       NOTE: Use TestValid in your code where ever an object is passed as a
             parameter. IT WILL SAVE YOU HOURS OF TRACKING BUGS WHERE THERE IS
             A POINTER TO AN OBJECT THAT HAS BEEN DESTROYED.
       @param AClassType The class this instance of the object should be.
       @param AAllowNil Can this instance of the object be nil?
       @example The code below shows some typical use of TestValid():<br/>
       <code>
         procedure TCustomer.AddStreetAddress(pAddress: TAddress);
         begin
           // Tests that:
           // a) pAddress is an instance of TStreetAddress; and
           // b) pAddress is not nil
           Assert(pAddress.TestValid(TStreetAddress,false), 'Invalid object');
           Add(pAddress);
         end;

         // Try this code:
         FMyData := TtiBaseObject;
         Assert(FMyData.TestValid(TtiBaseObject), 'Invalid object');
         FMyData.Free;
         Assert(FMyData.TestValid(TtiBaseObject, true), 'Invalid object'); // This assert will pass
         Assert(FMyData.TestValid(TtiBaseObject, false), 'Invalid object'); // This assert will fail
       </code>}
    function TestValid(AClassType: TClass = NIL; AAllowNil: boolean = False): boolean; overload;
    function TestValid(AAllowNil: boolean): boolean; overload;

    {$IFDEF OBJECT_TRACKING}
      {: If AskForBreakPointOnFree is called, then when the object is freed, a debugger
         breakpoint will be called. This will be raised even if the object is
         erroneously freed as another object (but not if it is erroneously freed
         using freemem or similiar - though even this can be caught using
         FREEMEM_CHECKING}
    procedure AskForBreakPointOnFree;
    class function GetLiveObjectCount: cardinal;
    property SerialNumber: integer read FSerialNo;
    {$ENDIF}
  end;

  TtiBaseObjectClass = class of TtiBaseObject;

{$IFDEF OBJECT_TRACKING}
function tiGetTotalObjectCount: integer;
function tiDescribeLiveObjects: string;
{$ENDIF}

implementation

uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows    // needed for InterlockedDecrement()
  {$ENDIF}
  ,SyncObjs   // This unit must always appear after the Windows unit!
  ;

const
  ASSERT_UNIT = 'IdSoapDebug';

{$IFNDEF DELPHI5ORABOVE}
procedure FreeAndNil(var AObj);
var
  Temp: TObject;
begin
  Temp          := TObject(AObj);
  Pointer(AObj) := NIL;
  Temp.Free;
end;
{$ENDIF}

//break point into the debugger if there is one;
procedure IdBreakpoint;
begin
  try
    asm
      int $03
    end;
  except
    // on some poorly configured Windows systems int $03 can cause unhandled
    // exceptions with improperly installed Dr Watsons etc....
  end;
end;

{$IFDEF OBJECT_TRACKING}
{==============================================================================
 Object/Class Tracking - List Library
 ==============================================================================}
const
  HASH_MASK = $03FF;
  HASH_SIZE = 1024;
  GROW_SIZE = 128;

type
  TIdDebugObjectList = class;


  TObjArray = array [0..0] of integer;
  pObjArray = ^TObjArray;


  TIdDebugObjectSubList = class(TObject)
  private
    FVal: cardinal;
    FCount, FAllocated: cardinal;
    FItems: pObjArray;
    procedure AddItem(AObj: integer);
    procedure DeleteItem(AObj: integer);
    function FindItem(AObj: integer; var VIndex: cardinal): boolean;
    procedure Grow;
  public
    constructor Create;
    destructor Destroy; override;
  end;


  TListArray = array [0..HASH_SIZE - 1] of TIdDebugObjectSubList;
  pListArray = ^TListArray;


  TIdDebugObjectList = class(TObject)
  private
    FHashTable: pListArray;
    function GetExists(AObj: TObject): boolean;
    procedure SetExists(AObj: TObject; AValue: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    property Exists[AObj: TObject]: boolean read GetExists write SetExists;
  end;


{ TIdDebugObjectSubList }

constructor TIdDebugObjectSubList.Create;
begin
  inherited Create;
  FCount     := 0;
  FAllocated := GROW_SIZE;
  GetMem(FItems, FAllocated * sizeof(cardinal));
end;


destructor TIdDebugObjectSubList.Destroy;
begin
  Assert(self <> NIL, 'Self is nil');
  FreeMem(FItems);
  inherited Destroy;
end;


procedure TIdDebugObjectSubList.AddItem(AObj: integer);
var
  i: cardinal;
begin
  Assert(self <> NIL, 'Self is nil');
  // no check on AObj
  if FindItem(AObj, i) then
    Assert(False, 'Attempt to re-register an object')
  else
  begin
    if FCount = FAllocated then
      Grow;
    if I < FCount then
      System.Move(FItems^[i], FItems^[i + 1], (FCount - I) * SizeOf(cardinal));
    FItems^[i] := AObj;
    Inc(FCount);
  end;
end;


function TIdDebugObjectSubList.FindItem(AObj: integer; var VIndex: cardinal): boolean;
var
  L, H, I, C: integer;
begin
  Assert(self <> NIL, 'Self is nil');
  // no check on AObj
  Result := False;
  L      := 0;
  H      := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := FItems^[I] - AObj;
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L      := I;
      end;
    end;
  end;
  VIndex := L;
end;


procedure TIdDebugObjectSubList.Grow;
begin
  Assert(self <> NIL, 'Self is nil');
  Inc(FAllocated, GROW_SIZE);
  ReallocMem(FItems, FAllocated * SizeOf(cardinal));
  Assert(FItems <> NIL, 'Grow failed to reallocate it''s memory');
end;


procedure TIdDebugObjectSubList.DeleteItem(AObj: integer);
var
  i: cardinal;
begin
  Assert(self <> NIL, 'Self is nil');
  // no check on AObj
  if FindItem(AObj, i) then
  begin
    Dec(FCount);
    if I < FCount then
      System.Move(FItems^[I + 1], FItems^[I], (FCount - I) * SizeOf(cardinal));
  end
  else
    Assert(False, 'Attempt to de-register an object that doesn''t exist ("' +
      TObject(AObj).ClassName + '")');
end;


{ TIdDebugObjectList }

constructor TIdDebugObjectList.Create;
var
  i: longint;
begin
  inherited Create;
  GetMem(FHashTable, HASH_SIZE * sizeof(pointer));
  Assert(assigned(FHashTable), 'Failed to assign memory for Hash Table');
  for i := 0 to HASH_SIZE - 1 do
  begin
    FHashtable^[i]      := TIdDebugObjectSubList.Create;
    FHashtable^[i].FVal := i;
  end;
end;


destructor TIdDebugObjectList.Destroy;
var
  i: longint;
begin
  Assert(self <> NIL, 'Self is nil');
  for i := 0 to HASH_SIZE - 1 do
    FreeAndNil(FHashtable^[i]);
  FreeMem(FHashTable, HASH_SIZE * sizeof(pointer));
  inherited Destroy;
end;


{ shr 3 cause objects are generally 4 or 8 aligned. }
function TIdDebugObjectList.GetExists(AObj: TObject): boolean;
var
  LDummy: cardinal;
begin
  Assert(self <> NIL, 'Self is nil');
  // no check on AObj
  Result := FHashTable^[(cardinal(AObj) shr 3) and HASH_MASK].FindItem(integer(AObj), LDummy);
end;


procedure TIdDebugObjectList.SetExists(AObj: TObject; AValue: boolean);
begin
  Assert(self <> NIL, 'Self is nil');
  // no check on AObj or AValue
  if AValue then
    FHashTable^[(cardinal(AObj) shr 3) and HASH_MASK].AddItem(integer(AObj))
  else
    FHashTable^[(cardinal(AObj) shr 3) and HASH_MASK].DeleteItem(integer(AObj));
end;

{==============================================================================
 Object/Class Tracking - infrastructural support
 ==============================================================================}

var
  GObjectTrackingLock: TCriticalSection;
  GIdObjectsCount:     TStringList = NIL;
  GTotalObjectCount:   integer;
  {$IFDEF OBJECT_TRACKING}
  GLastObjectID:       cardinal;
  GFullObjectList:     TIdDebugObjectList;
  GBreakPointList:     TIdDebugObjectList;

  {$ENDIF}

function tiGetTotalObjectCount: integer;
begin
  GObjectTrackingLock.Enter;
  try
    Result := GTotalObjectCount;
  finally
    GObjectTrackingLock.Leave;
  end;
end;


procedure InitObjectTracking;
begin
  Assert(GIDObjectsCount = NIL, 'Attempt to reinitialize Object Tracking after it has already been initialised');
  gObjectTrackingLock := TCriticalSection.Create;

  GIDObjectsCount        := TStringList.Create;
  GIDObjectsCount.Sorted := True;
  GTotalObjectCount      := 0;
  {$IFDEF OBJECT_TRACKING}
  gFullObjectList        := TIdDebugObjectList.Create;
  gBreakPointList        := TIdDebugObjectList.Create;
  {$ENDIF}
end;

function tiDescribeLiveObjects: string;
var
  i: integer;
begin
  gObjectTrackingLock.Enter;
  try
    Result := '';
    for I := 0 to GIDObjectsCount.Count - 1 do    // Iterate
      Result := Result + GIDObjectsCount[i] + '=' +
        IntToStr(integer(GIDObjectsCount.Objects[i])) + #13#10;
  finally
    gObjectTrackingLock.Leave;
  end;
end;


procedure CloseObjectTracking;

  function _IsLibrary: boolean;
  begin
    {$ifdef FPC}
    Result := IsLibrary;
    {$ELSE}
    Result := ModuleIsLib;
    {$ENDIF}
  end;

begin
  Assert(GIDObjectsCount <> NIL, 'Attempt to finalize Object Tracking before it has been initialised');
  {$IFDEF OBJECT_TRACKING}
  FreeAndNil(gFullObjectList);
  FreeAndNil(gBreakPointList);
  {$ENDIF}
  FreeAndNil(GIDObjectsCount);
  FreeAndNil(gObjectTrackingLock);
end;

function IdObjRegister(AObject: TObject): cardinal;
var
  i: integer;
begin
  Assert(GIDObjectsCount <> NIL, 'Attempt to use Object tracking before it is initialised');
  gObjectTrackingLock.Enter;
  try
    Inc(GTotalObjectCount);
    if not GIDObjectsCount.find(AObject.ClassName, i) then
      i := GIDObjectsCount.AddObject(AObject.ClassName, NIL);
    GIDObjectsCount.objects[i] := TObject(integer(GIDObjectsCount.objects[i]) + 1);
    {$IFDEF OBJECT_TRACKING}
    Inc(gLastObjectID);
    Result := gLastObjectID;
    Assert(cardinal(Result) <> 0, 'Error getting Unique number for Object');
    gFullObjectList.Exists[AObject] := True;
    {$ELSE}
    Result := 0;
    {$ENDIF}
  finally
    gObjectTrackingLock.Leave;
  end;
end;


function IdObjTestValid(AObject: TObject; AAllowNil: boolean = False): boolean;
begin
  Assert(GIDObjectsCount <> NIL, 'Attempt to use Object tracking before it is initialised');
{$IFDEF OBJECT_TRACKING}
  gObjectTrackingLock.Enter;
  try
    if AObject = NIL then
      Result := AAllowNil
    else
      Result := gFullObjectList.exists[AObject];
  finally
    gObjectTrackingLock.Leave;
  end;
{$ELSE}
  Result := True;
{$ENDIF}
end;


procedure IdObjDeregister(AObject: TObject);
var
  i: integer;
begin
  Assert(GIDObjectsCount <> NIL, 'Attempt to use Object tracking before it is initialised');
  gObjectTrackingLock.Enter;
  try
    if GTotalObjectCount = 0 then
      IdBreakpoint
    else
      Dec(GTotalObjectCount);  { if..else }

    if not GIDObjectsCount.find(AObject.ClassName, i) then
      IdBreakpoint// likely cause for being here : a missing inherited create in some descendent class

    else
    begin
      {$IFDEF OBJECT_TRACKING}
      if not IdObjTestValid(AObject) then
        IdBreakpoint;
      if gBreakPointList.Exists[AObject] then
      begin
        IdBreakpoint;
        gBreakPointList.Exists[AObject] := False;
      end;
      gFullObjectList.Exists[AObject] := False;
      {$ENDIF}
      GIDObjectsCount.objects[i] := TObject(integer(GIDObjectsCount.objects[i]) - 1);
      if integer(GIDObjectsCount.objects[i]) = 0 then
        GIDObjectsCount.Delete(i);
    end;  { if..else }
  finally
    gObjectTrackingLock.Leave;
  end;
end;


procedure IdObjBreakPointOnFree(AObject: TObject);
begin
  Assert(GIDObjectsCount <> NIL, 'Attempt to use Object tracking before it is initialised');
  Assert(IdObjTestValid(AObject), 'Attempt to watch an invalid object');
  {$IFDEF OBJECT_TRACKING}
  gObjectTrackingLock.Enter;
  try
    gBreakPointList.Exists[AObject] := True;
  finally
    gObjectTrackingLock.Leave;
  end;
  {$ENDIF}
end;

{$ENDIF}

function IdObjectRegister(AObject: TObject): cardinal;
begin
{$IFDEF OBJECT_TRACKING}
  Assert(AObject <> NIL, 'Object is Nil registering in Object Tracking System');
  Result := IdObjRegister(AObject);
{$ELSE}
  Result := 0;
{$ENDIF}
end;

function IdObjectTestValid(AObject: TObject; AClassType: TClass = NIL): boolean;
begin
{$IFDEF OBJECT_TRACKING}
  Result := IdObjTestValid(AObject);
{$ELSE}
  Result := AObject <> NIL;
{$ENDIF}
  if Result and assigned(AClassType) then
    Result := AObject is AClassType;
end;

procedure IdObjectBreakPointOnFree(AObject: TObject);
begin
{$IFDEF OBJECT_TRACKING}
  IdObjBreakPointOnFree(AObject);
{$ENDIF}
end;

procedure IdObjectDeregister(AObject: TObject);
begin
{$IFDEF OBJECT_TRACKING}
  IdObjDeregister(AObject);
{$ENDIF}
end;

{ TtiBaseObject }

{$IFDEF OBJECT_TRACKING}
class function TtiBaseObject.GetLiveObjectCount: cardinal;
var
  i: integer;
begin
  Assert(GIDObjectsCount <> NIL, 'Attempt to use Object tracking before it is initialised');
  gObjectTrackingLock.Enter;
  try
    if not GIDObjectsCount.find(ClassName, i) then
      Result := 0
    else
      Result := integer(GIDObjectsCount.objects[i]);
  finally
    gObjectTrackingLock.Leave;
  end;
end;

procedure TtiBaseObject.AskForBreakPointOnFree;
begin
  IdObjBreakPointOnFree(self);
end;

{$ENDIF}

function TtiBaseObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TtiBaseObject._AddRef: Integer;
begin
  {$IFDEF REFERENCE_COUNTING}
  Result:= InterlockedIncrement(FRefCount);
  {$else}
  Result:= 0;
  {$ENDIF}
end;

function TtiBaseObject._Release: Integer;
begin
  {$IFDEF REFERENCE_COUNTING}
  Result := InterlockedDecrement(FRefCount);
  if FRefCounting then
    if Result = 0 then
      Destroy;
  {$else}
  Result:= 0;
  {$ENDIF}
end;

constructor TtiBaseObject.Create;
begin
  inherited;
  {$IFDEF OBJECT_TRACKING}FSerialNo := IdObjRegister(self);{$ENDIF}
end;

{ Set an implicit refcount so that refcounting during construction won't
  destroy the object. }
{$IFDEF REFERENCE_COUNTING}
constructor TtiBaseObject.CreateWithRefCounting;
begin
  Create;
  FRefCounting := True;
end;
{$ENDIF}

destructor TtiBaseObject.Destroy;
begin
  {$IFDEF OBJECT_TRACKING}IdObjDeregister(self);{$ENDIF}
  inherited;
end;

{$IFDEF REFERENCE_COUNTING}
procedure TtiBaseObject.AfterConstruction;
begin
  inherited AfterConstruction;
  // Release the constructor's implicit refcount
  if FRefCounting then
    InterlockedDecrement(FRefCount);
end;
{$ENDIF}

{$IFDEF REFERENCE_COUNTING}
procedure TtiBaseObject.BeforeDestruction;
begin
  if FRefCounting then
    if FRefCount <> 0 then
      System.Error(reInvalidPtr);
  inherited BeforeDestruction;
end;

class function TtiBaseObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TtiBaseObject(Result).FRefCount := 1;
end;
{$ENDIF}

function TtiBaseObject.TestValid(AClassType: TClass = NIL; AAllowNil: boolean = False): boolean;
begin
  {$IFDEF OBJECT_TRACKING}
  Result := IdObjTestValid(self, AAllowNil);
  {$ELSE}
  Result := AAllowNil or Assigned(self);
  {$ENDIF}
  if Result and Assigned(self) and Assigned(AClassType) then
    Result := Self is AClassType;
end;

function TtiBaseObject.TestValid(AAllowNil: boolean): boolean;
begin
  result:= TestValid(TtiBaseObject, AAllowNil);
end;

initialization
  {$IFDEF OBJECT_TRACKING}
  InitObjectTracking;
  {$ENDIF}

finalization
  {$IFDEF OBJECT_TRACKING}
  CloseObjectTracking;
  {$ENDIF}

end.






