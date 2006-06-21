{
IndySOAP: This unit tracks objects to prevent object leaks
}
{
Version History:
  21-Mar 2003   Grahame Grieve                  Finish adding TIdSoapHashTable support
  18-Mar 2003   Grahame Grieve                  Add Hash function for Java bigots ;-)
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
}                         

{: Unit containing base class used by all tiOPF classes - implements live
   object tracking to help detect memory leaks Cloned from IndySoap's
   abstract base class.}
unit tiObjAbs;

{$I tiDefines.inc}

interface

uses
  { the contents of this uses clause are controlled. Do not add any units
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

  {$IFDEF CLASS_TRACKING}
  {:Abstract base class for all tiOPF objects. Implements live object tracking (
    a count of leaking objects will be shown in a dialog when the application
    closes) and TestValid() to confirm the object points to valid data.

    @example Paste the code below to into a test application to see how live
    object tracking works.<br><br>NOTE: You will require a conditional define
    CLASS_TRACKING in your application.<br>
    <code>
    (*$DEFINE CLASS_TRACKING*)
    TForm1.Button1Click(Sender: TObject);
    begin
      TtiObjAbs.Create;
      Close; // When the application closes, you will see a popup dialog saying:
             // Leaking objects:
             // 1 TtiObjAbs
    end;
    </code>
    }

  TtiObjAbs = class(TPersistent)
  Private
    FSerialNo: Integer;
  Public
    constructor Create;
    destructor Destroy; Override;

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
         FMyData := TtiObjAbs;
         Assert(FMyData.TestValid(TtiObjAbs), 'Invalid object');
         FMyData.Free;
         Assert(FMyData.TestValid(TtiObjAbs, true), 'Invalid object'); // This assert will pass
         Assert(FMyData.TestValid(TtiObjAbs, false), 'Invalid object'); // This assert will fail
       </code>}
    function TestValid(AClassType: TClass = NIL; AAllowNil : boolean = false): Boolean;

    {: If AskForBreakPointOnFree is called, then when the object is freed, a debugger
       breakpoint will be called. This will be raised even if the object is
       erroneously freed as another object (but not if it is erroneously freed
       using freemem or similiar - though even this can be caught using
       FREEMEM_CHECKING}
    procedure AskForBreakPointOnFree;

    {$IFNDEF EXCLUDE_FROM_DOC}
    class function GetLiveObjectCount: Cardinal;
    property SerialNumber: Integer Read FSerialNo;
    procedure TestInvariants(const AParamName: string; Const AClass : TClass; const ALoc : string);
    {$ENDIF}
  end;
  {$ENDIF}

  {$IFNDEF CLASS_TRACKING}
  // nothing - just redefine TtiObjAbs as a TObject
  TtiObjAbs = class (TPersistent)
  private
  public
    constructor Create;
    destructor Destroy; override;
    function TestValid(AClassType: TClass = NIL; AAllowNil : boolean = false): Boolean;
    procedure TestInvariants(const AParamName: string; Const AClass : TClass; const ALoc : string);
  end;
  {$ENDIF}

{$IFNDEF EXCLUDE_FROM_DOC}
function  IdObjectRegister(AObject: TObject): Cardinal;
function  IdObjectTestValid(AObject: TObject; AClassType: TClass = NIL): Boolean;
procedure IdObjectBreakPointOnFree(AObject: TObject);
procedure IdObjectDeregister(AObject: TObject);
{$IFNDEF DOTNET}
procedure IdBreakpoint;
{$ENDIF}
{$ENDIF}

{$IFNDEF EXCLUDE_FROM_DOC}

{$IFDEF CLASS_TRACKING}
function IdGetThreadObjectCount: Integer;
function IdGetTotalObjectCount: Integer;
procedure IdSoapListObjectCounts(AList : TStringList); // put names in list, with count as object
{$ENDIF}

var
  GIdSoapSuppressLeakDialog : boolean = false;
  GIdSoapTestInvariantFormat : string = '%s (%s): %3:s [%2:s]';

{$ENDIF}

implementation

uses
  SysUtils
  {$IFDEF MSWINDOWS}
  ,Windows
  ,Forms
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  ,QForms
  {$ENDIF LINUX}
  {$IFDEF DELPHI6ORABOVE}
  ,SyncObjs
  {$ENDIF}
  ;
                      
const
  ASSERT_UNIT = 'IdSoapDebug';
  RS_ERR_DEBUG_LEAKING_OBJECTS = 'Leaking Objects';
  RS_OP_DEBUG_OBJECT_TRACKING = 'Object Tracking';
  RS_MSG_DEBUG_OBJECT_TRACKING_NOT_FOUND = 'No Live Objects found';

procedure SystemMessage(ATitle, AContent: String);
begin
  {$IFDEF LINUX}
  Application.MessageBox(AContent, ATitle,[smbOK]);
  {$ENDIF LINUX}
  {$IFDEF MSWINDOWS}
  MessageBox(0, PChar(AContent), PChar(ATitle), mb_ok);
  {$ENDIF MSWINDOWS}
end;

{$IFDEF OBJECT_TRACKING}
procedure LogMessage( const pMessage : string ) ;
var
  lsl : TStringList;
  lFileName : string ;
begin
  lFileName :=
    ChangeFileExt( ParamStr(0), '' ) + '_Leaks.txt';
  lsl := TStringList.Create ;
  try
    lsl.Text := pMessage ;
    lsl.SaveToFile( lFileName ) ;
  finally
    lsl.Free ;
  end;
end;
{$ENDIF}

procedure _Assert( pCondition : boolean ; const pMessage : string ) ;
begin
  if not pCondition then
  begin
    SystemMessage( 'Assertion failed', pMessage ) ;
    Abort ;
  end;
end ;

{$IFNDEF DELPHI5ORABOVE}
procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;
{$ENDIF}


//break point into the debugger if there is one;
{$IFNDEF DOTNET}
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
{$ENDIF}

{$IFDEF CLASS_TRACKING}
{==============================================================================
 Object/Class Tracking - List Library
 ==============================================================================}
const
  HASH_MASK = $03FF;
  HASH_SIZE = 1024;
  GROW_SIZE = 128;

type
  TIdDebugObjectList = class;

  TObjArray = array [0..0] of Integer;
  pObjArray = ^TObjArray;

  TIdDebugObjectSubList = class(TObject)
  Private
    FVal: Cardinal;
    FCount, FAllocated: Cardinal;
    FItems: pObjArray;
    procedure AddItem(AObj: Integer);
    procedure DeleteItem(AObj: Integer);
    function FindItem(AObj: Integer; var VIndex: Cardinal): Boolean;
    procedure Grow;
  Public
    constructor Create;
    destructor Destroy; Override;
  end;

  TListArray = array [0..HASH_SIZE - 1] of TIdDebugObjectSubList;
  pListArray = ^TListArray;

  TIdDebugObjectList = class(TObject)
  Private
    FHashTable: pListArray;
    function GetExists(AObj: TObject): Boolean;
    procedure SetExists(AObj: TObject; AValue: Boolean);
  Public
    constructor Create;
    destructor Destroy; Override;
    property Exists[AObj: TObject]: Boolean Read GetExists Write SetExists;
  end;

  { TIdDebugObjectSubList }

constructor TIdDebugObjectSubList.Create;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.Create';
begin
  inherited Create;
  FCount := 0;
  FAllocated := GROW_SIZE;
  GetMem(FItems, FAllocated * sizeof(Cardinal));
end;

destructor TIdDebugObjectSubList.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.Destroy';
begin
  _Assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  FreeMem(FItems);
  inherited Destroy;
end;

procedure TIdDebugObjectSubList.AddItem(AObj: Integer);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.AddItem';
var
  i: Cardinal;
begin
  _Assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj
  if FindItem(AObj, i) then
    begin
    _Assert(False, ASSERT_LOCATION+': Attempt to re-register an object')
    end
  else
    begin
    if FCount = FAllocated then
      Grow;
    if I < FCount then
      System.Move(FItems^[i], FItems^[i + 1], (FCount - I) * SizeOf(Cardinal));
    FItems^[i] := AObj;
    Inc(FCount);
    end;
end;

function TIdDebugObjectSubList.FindItem(AObj: Integer; var VIndex: Cardinal): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.FindItem';
var
  L, H, I, C: Integer;
begin
  _Assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
    begin
    I := (L + H) shr 1;
    C := FItems^[I] - AObj;
    if C < 0 then
      begin
      L := I + 1
      end
    else
      begin
      H := I - 1;
      if C = 0 then
        begin
        Result := True;
        L := I;
        end;
      end;
    end;
  VIndex := L;
end;

procedure TIdDebugObjectSubList.Grow;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.Grow';
begin
  _Assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  inc(FAllocated, GROW_SIZE);
  ReallocMem(FItems, FAllocated * SizeOf(Cardinal));
  _Assert(FItems <> NIL, ASSERT_LOCATION+': Grow failed to reallocate it''s memory');
end;

procedure TIdDebugObjectSubList.DeleteItem(AObj: Integer);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectSubList.DeleteItem';
var
  i: Cardinal;
begin
  _Assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj
  if FindItem(AObj, i) then
    begin
    Dec(FCount);
    if I < FCount then
      System.Move(FItems^[I + 1], FItems^[I], (FCount - I) * SizeOf(Cardinal));
    end
  else
    begin
    _Assert(False, ASSERT_LOCATION+': Attempt to de-register an object that doesn''t exist ("'+TObject(AObj).ClassName+'")');
    end;
end;

{ TIdDebugObjectList }

constructor TIdDebugObjectList.Create;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectList.Create';
var
  i: Longint;
begin
  inherited Create;
  GetMem(FHashTable, HASH_SIZE * sizeof(pointer));
  _Assert(assigned(FHashTable), ASSERT_LOCATION+': Failed to assign memory for Hash Table');
  for i := 0 to HASH_SIZE - 1 do
    begin
    FHashtable^[i] := TIdDebugObjectSubList.Create;
    FHashtable^[i].FVal := i;
    end;
end;

destructor TIdDebugObjectList.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectList.Destroy';
var
  i: Longint;
begin
  _Assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  for i := 0 to HASH_SIZE - 1 do
    begin
    FreeAndNil(FHashtable^[i]);
    end;
  FreeMem(FHashTable, HASH_SIZE * sizeof(pointer));
  inherited Destroy;
end;

// shr 3 cause objects are generally 4 or 8 aligned.
function TIdDebugObjectList.GetExists(AObj: TObject): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectList.GetExists';
var
  LDummy: Cardinal;
begin
  _Assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj
  Result := FHashTable^[(Cardinal(AObj) shr 3) and HASH_MASK].FindItem(Integer(AObj), LDummy);
end;

procedure TIdDebugObjectList.SetExists(AObj: TObject; AValue: Boolean);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TIdDebugObjectList.SetExists';
begin
  _Assert(self <> NIL, ASSERT_LOCATION+': Self is nil');
  // no check on AObj or AValue
  if AValue then
    FHashTable^[(Cardinal(AObj) shr 3) and HASH_MASK].AddItem(Integer(AObj))
  else
    FHashTable^[(Cardinal(AObj) shr 3) and HASH_MASK].DeleteItem(Integer(AObj));
end;

{==============================================================================
 Object/Class Tracking - infrastructural support
 ==============================================================================}

var
  GObjectTrackingLock: TCriticalSection;
  GIdObjectsCount: TStringList = NIL;
  GTotalObjectCount : integer;
  {$IFDEF OBJECT_TRACKING}
  GLastObjectID: Cardinal;
  GFullObjectList: TIdDebugObjectList;
  GBreakPointList: TIdDebugObjectList;
  {$ENDIF}

threadvar
  GThreadObjectCount: Integer;
  // can be very large -ve if one thread is dropping objects created by another thread

function IdGetThreadObjectCount: Integer;
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdGetThreadObjectCount';
begin
  Result := GThreadObjectCount;
end;

function IdGetTotalObjectCount: Integer;
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdGetTotalObjectCount';
begin
  GObjectTrackingLock.Enter;
  try
    result := GTotalObjectCount;
  finally
    GObjectTrackingLock.Leave;
    end;
end;

procedure InitObjectTracking;
Const ASSERT_LOCATION = ASSERT_UNIT+'.InitObjectTracking';
begin
  _Assert(GIDObjectsCount = NIL, ASSERT_LOCATION+': Attempt to reinitialize Object Tracking after it has already been initialised');
  gObjectTrackingLock := TCriticalSection.Create;

  GIDObjectsCount := TStringList.Create;
  GIDObjectsCount.Sorted := True;
  GTotalObjectCount := 0;
  {$IFDEF OBJECT_TRACKING}
  gFullObjectList := TIdDebugObjectList.Create;
  gBreakPointList := TIdDebugObjectList.Create;
  {$ENDIF}
end;

procedure IdSoapListObjectCounts(AList : TStringList); // put names in list, with count as object
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdSoapListObjectCounts';
var
  i: Integer;
begin
  gObjectTrackingLock.Enter;
  try
    for I := 0 to GIDObjectsCount.Count - 1 do    // Iterate
      begin
      AList.AddObject(GIDObjectsCount[i], GIDObjectsCount.Objects[i]);
      end;
  finally
    gObjectTrackingLock.Leave;
    end;
end;

function DescribeLiveObjects: String;
Const ASSERT_LOCATION = ASSERT_UNIT+'.DescribeLiveObjects';
var
  i: Integer;
begin
  gObjectTrackingLock.Enter;
  try
    Result := '';
    for I := 0 to GIDObjectsCount.Count - 1 do    // Iterate
      begin
      Result := Result + GIDObjectsCount[i] + ' ' + IntToStr(Integer(GIDObjectsCount.Objects[i])) + #13#10;
      end;
  finally
    gObjectTrackingLock.Leave;
    end;
end;

procedure CloseObjectTracking;
Const ASSERT_LOCATION = ASSERT_UNIT+'.CloseObjectTracking';
var
  ls : string ;
begin
  _Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION+': Attempt to finalize Object Tracking before it has been initialised');
  if not ModuleIsLib and not GIdSoapSuppressLeakDialog and (GTotalObjectCount > 0) then
    begin
    _Assert(GIDObjectsCount.count > 0, ASSERT_LOCATION+': Total Indy Object count shows that objects exist, but Indy Object Class List is empty');
    // Some Indy Objects are still live.....
    // they can be described using DescribeLiveObjects
    ls := DescribeLiveObjects ;
    LogMessage(ls);
    SystemMessage(RS_ERR_DEBUG_LEAKING_OBJECTS, ls );
    end;
  {$IFDEF OBJECT_TRACKING}
  FreeAndNil(gFullObjectList);
  FreeAndNil(gBreakPointList);
  {$ENDIF}
  FreeAndNil(GIDObjectsCount);
  FreeAndNil(gObjectTrackingLock);
end;

function IdObjRegister(AObject: TObject): Cardinal;
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjRegister';
var
  i: Integer;
begin
  _Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION+': Attempt to use Object tracking before it is initialised');
  inc(GThreadObjectCount);
  gObjectTrackingLock.Enter;
  try
    inc(GTotalObjectCount);
    if not GIDObjectsCount.find(AObject.ClassName, i) then
      begin
      i := GIDObjectsCount.AddObject(AObject.ClassName, NIL);
      end;
    GIDObjectsCount.objects[i] := TObject(Integer(GIDObjectsCount.objects[i]) + 1);
    {$IFDEF OBJECT_TRACKING}
    inc(gLastObjectID);
    Result := gLastObjectID;
    _Assert(Cardinal(Result) <> 0, ASSERT_LOCATION+': Error getting Unique number for Object');
    gFullObjectList.Exists[AObject] := True;
    {$ELSE}
    Result := 0;
    {$ENDIF}
  finally
    gObjectTrackingLock.Leave;
    end;
end;

function IdObjTestValid(AObject: TObject; AAllowNil : boolean = false): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjTestValid';
begin
  _Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION+': Attempt to use Object tracking before it is initialised');
{$IFDEF OBJECT_TRACKING}
  gObjectTrackingLock.Enter;
  try
    if AObject = nil then
      begin
      Result := AAllowNil;
      end
    else
      begin
      Result := gFullObjectList.exists[AObject];
      end;
  finally
    gObjectTrackingLock.Leave;
    end;
{$ELSE}
  Result := True;
{$ENDIF}
end;

procedure IdObjDeregister(AObject: TObject);
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjDeregister';
var
  i: Integer;
begin
  _Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION+': Attempt to use Object tracking before it is initialised');
  dec(GThreadObjectCount);
  gObjectTrackingLock.Enter;
  try
    If GTotalObjectCount = 0 then
      begin
      IdBreakpoint;
      end
    else
      begin
      dec(GTotalObjectCount);
      end;
    if not GIDObjectsCount.find(AObject.ClassName, i) then
      begin
      // likely cause for being here : a missing inherited create in some descendent class
      IdBreakpoint;
      end
    else
      begin
      {$IFDEF OBJECT_TRACKING}
      if not IdObjTestValid(AObject) then
        begin
        IdBreakpoint;
        end;
      if gBreakPointList.Exists[AObject] then
        begin
        IdBreakpoint;
        gBreakPointList.Exists[AObject] := False;
        end;
      gFullObjectList.Exists[AObject] := False;
      {$ENDIF}
      GIDObjectsCount.objects[i] := TObject(Integer(GIDObjectsCount.objects[i]) - 1);
      if Integer(GIDObjectsCount.objects[i]) = 0 then
        begin
        GIDObjectsCount.Delete(i);
        end;
      end;
  finally
    gObjectTrackingLock.Leave;
    end;
end;

procedure IdObjBreakPointOnFree(AObject: TObject);
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjBreakPointOnFree';
begin
  _Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION+': Attempt to use Object tracking before it is initialised');
  _Assert(IdObjTestValid(AObject), ASSERT_LOCATION+': Attempt to watch an invalid object');
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

function IdObjectRegister(AObject: TObject): Cardinal;
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjectRegister';
begin
{$IFDEF CLASS_TRACKING}
  _Assert(AObject <> NIL, ASSERT_LOCATION+': Object is Nil registering in Object Tracking System');
  Result := IdObjRegister(AObject);
{$ELSE}
  Result := 0
{$ENDIF}
end;

function IdObjectTestValid(AObject: TObject; AClassType: TClass = NIL): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjectTestValid';
begin
{$IFDEF CLASS_TRACKING}
  Result := IdObjTestValid(AObject);
{$ELSE}
  result := AObject <> nil;
{$ENDIF}
  if Result and assigned(AClassType) then
    begin
    Result := AObject is AClassType;
    end;
end;

procedure IdObjectBreakPointOnFree(AObject: TObject);
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjectBreakPointOnFree';
begin
{$IFDEF CLASS_TRACKING}
  IdObjBreakPointOnFree(AObject);
{$ENDIF}
end;

procedure IdObjectDeregister(AObject: TObject);
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdObjectDeregister';
begin
{$IFDEF CLASS_TRACKING}
  IdObjDeregister(AObject);
{$ENDIF}
end;

procedure IdClassCountDlg(AClassName: String);
Const ASSERT_LOCATION = ASSERT_UNIT+'.IdClassCountDlg';
{$IFDEF CLASS_TRACKING}
var
  i: Integer;
begin
  _Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION+': Attempt to use Object tracking before it is initialised');
  gObjectTrackingLock.Enter;
  try
    if not GIDObjectsCount.find(AClassName, i) then
      begin
      SystemMessage(RS_OP_DEBUG_OBJECT_TRACKING, AClassName+': '+RS_MSG_DEBUG_OBJECT_TRACKING_NOT_FOUND);
      end
    else
      begin
      SystemMessage(RS_OP_DEBUG_OBJECT_TRACKING, AClassName + ': ' + IntToStr(Integer(GIDObjectsCount.objects[i])));
      end;
  finally
    gObjectTrackingLock.Leave;
    end;
{$ELSE}
begin
{$ENDIF}
end;

{ TtiObjAbs }

{$IFDEF CLASS_TRACKING}
constructor TtiObjAbs.Create;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TtiObjAbs.Create';
begin
  inherited;
  FSerialNo := IdObjRegister(self);
end;


destructor TtiObjAbs.Destroy;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TtiObjAbs.Destroy';
begin
  IdObjDeregister(self);
  inherited;
end;

class function TtiObjAbs.GetLiveObjectCount: Cardinal;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TtiObjAbs.GetLiveObjectCount';
var
  i: Integer;
begin
  _Assert(GIDObjectsCount <> NIL, ASSERT_LOCATION+': Attempt to use Object tracking before it is initialised');
  gObjectTrackingLock.Enter;
  try
    if not GIDObjectsCount.find(ClassName, i) then
      begin
      Result := 0;
      end
    else
      begin
      Result := Integer(GIDObjectsCount.objects[i]);
      end;
  finally
    gObjectTrackingLock.Leave;
    end;
end;

procedure TtiObjAbs.AskForBreakPointOnFree;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TtiObjAbs.AskForBreakPointOnFree';
begin
  IdObjBreakPointOnFree(self);
end;
{$ELSE}

constructor TtiObjAbs.Create;
begin
  inherited;
end;

destructor TtiObjAbs.Destroy;
begin
  inherited;
end;

{$ENDIF}

function TtiObjAbs.TestValid(AClassType: TClass = NIL; AAllowNil : boolean = false): Boolean;
Const ASSERT_LOCATION = ASSERT_UNIT+'.TtiObjAbs.TestValid';
begin
  {$IFDEF CLASS_TRACKING}
  {$IFDEF OBJECT_TRACKING}
  Result := IdObjTestValid(self, AAllowNil);
  {$ELSE}
  Result := AAllowNil or Assigned(self);
  {$ENDIF}
  {$ELSE}
  Result := AAllowNil or Assigned(self);
  {$ENDIF}
  if Result and Assigned(self) and assigned(AClassType) then
    begin
    Result := Self is AClassType;
    end;
end;

procedure TtiObjAbs.TestInvariants(const AParamName: string; Const AClass : TClass; const ALoc : string);
Const ASSERT_LOCATION = ASSERT_UNIT+'.TBaseObject.TestInvariants';
begin
  _Assert(ALoc <> '', ASSERT_LOCATION+': ALoc = ''''');
  _Assert(self.TestValid(AClass), Format(GIdSoapTestInvariantFormat, [ASSERT_LOCATION, ALoc, AParamName, 'self is not valid']));
end;


initialization
  {$IFDEF CLASS_TRACKING}
  InitObjectTracking;
  {$ENDIF}

finalization
  {$IFDEF CLASS_TRACKING}
  CloseObjectTracking;
  {$ENDIF}
end.
