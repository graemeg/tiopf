
{
IndySOAP: This unit tracks objects to prevent object leaks
Version History:
  21-Mar 2003   Grahame Grieve                  Finish adding TIdSoapHashTable support
  18-Mar 2003   Grahame Grieve                  Add Hash function for Java bigots;-)
  25-Feb 2003   Grahame Grieve                  Fix typo
  17-Sep 2002   Grahame Grieve                  Fix Compile problems
  13-Aug 2002   Grahame Grieve                  Suppress Leak reporting
  09-Jul 2002   Grahame Grieve                  Put ObjectCount in interface
  26-Apr 2002   Andrew Cumming                  Move includes to allow D6 compile
  11-Apr 2002   Grahame Grieve                  Use ASSERT_LOCATION, ResourceStrings were appropriate
   7-Mar 2002   Grahame Grieve                  Review assertions
   3-Mar 2002   Grahame Grieve                  Move IdComponent & IdGlobal out
   7-Feb 2002   Grahame Grieve                  Fix problems when CLASS_TRACKING not defined
   3-Feb 2002   Andrew Cumming                  Added D4 support
  25-Jan 2002   Grahame Grieve/Andrew Cumming   First release of IndySOAP
   4-Jun 2003   Peter Hinrichsen                Cloned from IndySoap to tiOPF
   2-July 2007  Peter Hinrichsen                Refactored to remove dependency on Windows.pas & GUI code
}

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

    @example Paste the code below to into a test application to see how live
    object tracking works.<br><br>NOTE: You will require a conditional define
    CLASS_TRACKING in your application.<br>
    <code>
    (*$DEFINE OBJECT_TRACKING*)
    TForm1.Button1Click(Sender: TObject);
    var
      LO: TtiBaseObject
    begin
      LO:= TtiBaseObject.Create;
      Assert(LO.TestValid);
      LO.Free;
      Assert(not LO.TestValid);
    end;
    </code>
    }

  TtiBaseObject = class(TObject)
  private
    FSerialNo: integer;
  public
    constructor Create;
    destructor Destroy; override;
    {: Checks that the object is valid and in the list of current
       objects. If not, checks that self <> nil (which is not always very
       useful but still worth checking.)<br><br>
       Available if OBJECT_TRACKING is defined.<br><br>
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
    {$ENDIF}
    property SerialNumber: integer read FSerialNo;

  end;

  TtiBaseObjectClass = class of TtiBaseObject;

{$IFDEF OBJECT_TRACKING}
function tiGetTotalObjectCount: integer;
function tiDescribeLiveObjects: string;
{$ENDIF OBJECT_TRACKING}

implementation

uses
  SysUtils
  ,
  SyncObjs;

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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectSubList.Create';
begin
  inherited Create;
  FCount     := 0;
  FAllocated := GROW_SIZE;
  GetMem(FItems, FAllocated * sizeof(cardinal));
end;


destructor TIdDebugObjectSubList.Destroy;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectSubList.Destroy';
begin
  Assert(self <> NIL, ASSERT_LOCATION + ': Self is nil');
  FreeMem(FItems);
  inherited Destroy;
end;


procedure TIdDebugObjectSubList.AddItem(AObj: integer);
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectSubList.AddItem';
var
  i: cardinal;
begin
  Assert(self <> NIL, ASSERT_LOCATION + ': Self is nil');
  // no check on AObj
  if FindItem(AObj, i) then
    Assert(False, ASSERT_LOCATION + ': Attempt to re-register an object')
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectSubList.FindItem';
var
  L, H, I, C: integer;
begin
  Assert(self <> NIL, ASSERT_LOCATION + ': Self is nil');
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectSubList.Grow';
begin
  Assert(self <> NIL, ASSERT_LOCATION + ': Self is nil');
  Inc(FAllocated, GROW_SIZE);
  ReallocMem(FItems, FAllocated * SizeOf(cardinal));
  Assert(FItems <> NIL, ASSERT_LOCATION + ': Grow failed to reallocate it''s memory');
end;


procedure TIdDebugObjectSubList.DeleteItem(AObj: integer);
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectSubList.DeleteItem';
var
  i: cardinal;
begin
  Assert(self <> NIL, ASSERT_LOCATION + ': Self is nil');
  // no check on AObj
  if FindItem(AObj, i) then
  begin
    Dec(FCount);
    if I < FCount then
      System.Move(FItems^[I + 1], FItems^[I], (FCount - I) * SizeOf(cardinal));
  end
  else
    Assert(False, ASSERT_LOCATION + ': Attempt to de-register an object that doesn''t exist ("' +
      TObject(AObj).ClassName + '")');
end;


{ TIdDebugObjectList }

constructor TIdDebugObjectList.Create;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectList.Create';
var
  i: longint;
begin
  inherited Create;
  GetMem(FHashTable, HASH_SIZE * sizeof(pointer));
  Assert(assigned(FHashTable), ASSERT_LOCATION + ': Failed to assign memory for Hash Table');
  for i := 0 to HASH_SIZE - 1 do
  begin
    FHashtable^[i]      := TIdDebugObjectSubList.Create;
    FHashtable^[i].FVal := i;
  end;
end;


destructor TIdDebugObjectList.Destroy;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectList.Destroy';
var
  i: longint;
begin
  Assert(self <> NIL, ASSERT_LOCATION + ': Self is nil');
  for i := 0 to HASH_SIZE - 1 do
    FreeAndNil(FHashtable^[i]);
  FreeMem(FHashTable, HASH_SIZE * sizeof(pointer));
  inherited Destroy;
end;


{ shr 3 cause objects are generally 4 or 8 aligned. }
function TIdDebugObjectList.GetExists(AObj: TObject): boolean;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectList.GetExists';
var
  LDummy: cardinal;
begin
  Assert(self <> NIL, ASSERT_LOCATION + ': Self is nil');
  // no check on AObj
  Result := FHashTable^[(cardinal(AObj) shr 3) and HASH_MASK].FindItem(integer(AObj), LDummy);
end;


procedure TIdDebugObjectList.SetExists(AObj: TObject; AValue: boolean);
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TIdDebugObjectList.SetExists';
begin
  Assert(self <> NIL, ASSERT_LOCATION + ': Self is nil');
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdGetTotalObjectCount';
begin
  GObjectTrackingLock.Enter;
  try
    Result := GTotalObjectCount;
  finally
    GObjectTrackingLock.Leave;
  end;
end;


procedure InitObjectTracking;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.InitObjectTracking';
begin
  Assert(GIDObjectsCount = NIL, ASSERT_LOCATION +
    ': Attempt to reinitialize Object Tracking after it has already been initialised');
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.DescribeLiveObjects';
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
    {$IFDEF FPC}
    Result := IsLibrary;
    {$ELSE}
    Result := ModuleIsLib;
    {$ENDIF}
  end;

const
  ASSERT_LOCATION = ASSERT_UNIT + '.CloseObjectTracking';
begin
  Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION +
    ': Attempt to finalize Object Tracking before it has been initialised');
  {$IFDEF OBJECT_TRACKING}
  FreeAndNil(gFullObjectList);
  FreeAndNil(gBreakPointList);
  {$ENDIF}
  FreeAndNil(GIDObjectsCount);
  FreeAndNil(gObjectTrackingLock);
end;

function IdObjRegister(AObject: TObject): cardinal;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdObjRegister';
var
  i: integer;
begin
  Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION +
    ': Attempt to use Object tracking before it is initialised');
  gObjectTrackingLock.Enter;
  try
    Inc(GTotalObjectCount);
    if not GIDObjectsCount.find(AObject.ClassName, i) then
      i := GIDObjectsCount.AddObject(AObject.ClassName, NIL);
    GIDObjectsCount.objects[i] := TObject(integer(GIDObjectsCount.objects[i]) + 1);
    {$IFDEF OBJECT_TRACKING}
    Inc(gLastObjectID);
    Result := gLastObjectID;
    Assert(cardinal(Result) <> 0, ASSERT_LOCATION + ': Error getting Unique number for Object');
    gFullObjectList.Exists[AObject] := True;
    {$ELSE}
    Result := 0;
    {$ENDIF}
  finally
    gObjectTrackingLock.Leave;
  end;
end;


function IdObjTestValid(AObject: TObject; AAllowNil: boolean = False): boolean;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdObjTestValid';
begin
  Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION +
    ': Attempt to use Object tracking before it is initialised');
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdObjDeregister';
var
  i: integer;
begin
  Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION +
    ': Attempt to use Object tracking before it is initialised');
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdObjBreakPointOnFree';
begin
  Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION +
    ': Attempt to use Object tracking before it is initialised');
  Assert(IdObjTestValid(AObject), ASSERT_LOCATION + ': Attempt to watch an invalid object');
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdObjectRegister';
begin
{$IFDEF OBJECT_TRACKING}
  Assert(AObject <> NIL, ASSERT_LOCATION +
    ': Object is Nil registering in Object Tracking System');
  Result := IdObjRegister(AObject);
{$ELSE}
  Result := 0;
{$ENDIF}
end;


function IdObjectTestValid(AObject: TObject; AClassType: TClass = NIL): boolean;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdObjectTestValid';
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdObjectBreakPointOnFree';
begin
{$IFDEF OBJECT_TRACKING}
  IdObjBreakPointOnFree(AObject);
{$ENDIF}
end;


procedure IdObjectDeregister(AObject: TObject);
const
  ASSERT_LOCATION = ASSERT_UNIT + '.IdObjectDeregister';
begin
{$IFDEF OBJECT_TRACKING}
  IdObjDeregister(AObject);
{$ENDIF}
end;

{ TtiBaseObject }

{$IFDEF OBJECT_TRACKING}
constructor TtiBaseObject.Create;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TtiBaseObject.Create';
begin
  inherited;
  FSerialNo := IdObjRegister(self);
end;


destructor TtiBaseObject.Destroy;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TtiBaseObject.Destroy';
begin
  IdObjDeregister(self);
  inherited;
end;


class function TtiBaseObject.GetLiveObjectCount: cardinal;
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TtiBaseObject.GetLiveObjectCount';
var
  i: integer;
begin
  Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION +
    ': Attempt to use Object tracking before it is initialised');
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
const
  ASSERT_LOCATION = ASSERT_UNIT + '.TtiBaseObject.AskForBreakPointOnFree';
begin
  IdObjBreakPointOnFree(self);
end;

{$ELSE}


constructor TtiBaseObject.Create;
begin
  inherited;
end;


destructor TtiBaseObject.Destroy;
begin
  inherited;
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
