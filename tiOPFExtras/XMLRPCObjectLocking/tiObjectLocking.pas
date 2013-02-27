{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Yi Chen, Darren Camp and
  Tom Tuddenham.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiObjectLocking;

interface

uses
  Classes, SysUtils, SyncObjs, ActiveX, Windows, ComObj,

  tiPtnVisPerObj, tiPtnVis;

type
  EObjectLockingServerException = class(Exception);
  EObjectLockingClientException = class(Exception);

  ItiObjectLocking = interface
    ['{EE592C74-6DD5-4DC4-99D7-DF39E5D6130A}']
    function AcquireLock(ObjectIdentity: string): string; stdcall;
    function IsLocked(ObjectIdentity: string): Boolean; stdcall;
    procedure ReleaseLock(ObjectLock: string); stdcall;
    procedure ReleaseAllLocks; stdcall;
  end;

  TtiObjectLocking = class(TInterfacedObject, ItiObjectLocking)
  private
    FObjectIdentityList: TStringList;
    FObjectIdentityGUIDList: TStringList;
    FCriticalSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function AcquireLock(ObjectIdentity: string): string; stdcall;
    function IsLocked(ObjectIdentity: string): Boolean; stdcall;
    procedure ReleaseLock(ObjectLock: string); stdcall;
    procedure ReleaseAllLocks; stdcall;
  end;

  TLockablePerObj = class(TPerObjAbs)
  private
    FLocked: Boolean;
    FObjectLock: string;
  public
    property Locked: Boolean read FLocked write FLocked;
    property ObjectLock: string read FObjectLock write FObjectLock;
  end;

  TLockablePerObjList = class(TPerObjList)
  private
    FLocked: Boolean;
    FObjectLock: string;
  public
    property Locked: Boolean read FLocked write FLocked;
    property ObjectLock: string read FObjectLock write FObjectLock;
  end;

  TVisLockablePerObjAbs = class(TVisitorAbs)
  private
    function GetLockableObject: TLockablePerObj;
  protected
    property LockableObject: TLockablePerObj read GetLockableObject;
  end;

  TVisLockablePerObjAcquireLock = class(TVisLockablePerObjAbs)
  protected
    function AcceptVisitor: Boolean; override;
  public
    procedure Execute(const pVisited: TVisitedAbs); override;
  end;

  TVisLockablePerObjReleaseLock = class(TVisLockablePerObjAbs)
  protected
    function AcceptVisitor: Boolean; override;
  public
    procedure Execute(const pVisited: TVisitedAbs); override;
  end;

  procedure RegisterObjectLockingInterface(ObjectLockingInterface: ItiObjectLocking);
  function GetObjectLockingInterface: ItiObjectLocking;

implementation

var
  gObjectLockingInterface: ItiObjectLocking;

procedure RegisterObjectLockingInterface(ObjectLockingInterface: ItiObjectLocking);
begin
  gObjectLockingInterface := ObjectLockingInterface;
end;

function GetObjectLockingInterface: ItiObjectLocking;
begin
  Result := gObjectLockingInterface;
end;

{ TtiObjectLocking }

constructor TtiObjectLocking.Create;
begin
  FObjectIdentityList := TStringList.Create;
  FObjectIdentityGUIDList := TStringList.Create;
  FCriticalSection := TCriticalSection.Create;

  FObjectIdentityList.Duplicates := dupError;
  FObjectIdentityGUIDList.Duplicates := dupError;
end;

destructor TtiObjectLocking.Destroy;
begin
  FreeAndNil(FObjectIdentityList);
  FreeAndNil(FObjectIdentityGUIDList);
  FreeAndNil(FCriticalSection);
  inherited;
end;

function TtiObjectLocking.AcquireLock(ObjectIdentity: string): string;
var
  LockUID: TGUID;
begin
  if ObjectIdentity = '' then
    raise EObjectLockingServerException.Create('Cannot acquire lock when ObjectIdentity is blank');

  FCriticalSection.Enter;
  try
    if FObjectIdentityList.IndexOf(ObjectIdentity) <> -1 then
      raise EObjectLockingServerException.Create(Format('ObjectIdentity ''%s'' is already locked', [ObjectIdentity]));

    if CoCreateGUID(LockUID) <> S_OK then
      raise EObjectLockingServerException.Create('Unable to create GUID for object lock');

    FObjectIdentityList.Add(ObjectIdentity);
    FObjectIdentityGUIDList.Add(GUIDToString(LockUID));

    Result := GUIDToString(LockUID);

    Assert(FObjectIdentityList.Count = FObjectIdentityGUIDList.Count, 'Identity and IdentityGUID lists no longer match');
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TtiObjectLocking.ReleaseAllLocks;
begin
  FObjectIdentityList.Clear;
  FObjectIdentityGUIDList.Clear;
end;

function TtiObjectLocking.IsLocked(ObjectIdentity: string): Boolean;
begin
  if ObjectIdentity = '' then
    raise EObjectLockingServerException.Create('Cannot test lock when ObjectIdentity is blank');

  FCriticalSection.Enter;
  try
    Result := FObjectIdentityList.IndexOf(ObjectIdentity) <> -1;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TtiObjectLocking.ReleaseLock(ObjectLock: string);
var
  LockIndex: Integer;
begin
  if ObjectLock = '' then
    raise EObjectLockingServerException.Create('Cannot release lock when ObjectLock is blank');

  FCriticalSection.Enter;
  try
    LockIndex := FObjectIdentityGUIDList.IndexOf(ObjectLock);
    if LockIndex = -1 then
      raise EObjectLockingServerException.Create(Format('ObjectLock ''%s'' is not a valid lock id', [ObjectLock]));

    FObjectIdentityList.Delete(LockIndex);
    FObjectIdentityGUIDList.Delete(LockIndex);

    Assert(FObjectIdentityList.Count = FObjectIdentityGUIDList.Count, 'Identity and IdentityGUID lists no longer match');
  finally
    FCriticalSection.Leave;
  end;
end;

{ TVisLockablePerObjAbs }

function TVisLockablePerObjAbs.GetLockableObject: TLockablePerObj;
begin
  Result := Visited as TLockablePerObj;
end;

{ TVisLockablePerObjAcquireLock }

function TVisLockablePerObjAcquireLock.AcceptVisitor: Boolean;
begin
  Result := (Visited is TLockablePerObj) and
            (Visited as TLockablePerObj).Locked = False;
end;

procedure TVisLockablePerObjAcquireLock.Execute(
  const pVisited: TVisitedAbs);
var
  ObjectIdentity: string;
  ObjectLock: string;
begin
  inherited;
  if DoAcceptVisitor then
  begin
    ObjectIdentity := IntToStr(LockableObject.OID);
    if not GetObjectLockingInterface.IsLocked(ObjectIdentity) then
    begin
      ObjectLock := GetObjectLockingInterface.AcquireLock(ObjectIdentity);
      LockableObject.ObjectLock := ObjectLock;
      LockableObject.Locked := True;
    end
    else
    begin
      raise EObjectLockingClientException.Create('Object is already locked');
    end;
  end;
end;

{ TVisLockablePerObjReleaseLock }

function TVisLockablePerObjReleaseLock.AcceptVisitor: Boolean;
begin
  Result := (Visited is TLockablePerObj) and
            (Visited as TLockablePerObj).Locked = True;
end;

procedure TVisLockablePerObjReleaseLock.Execute(
  const pVisited: TVisitedAbs);
var
  ObjectIdentity: string;
begin
  inherited;
  if DoAcceptVisitor then
  begin
    ObjectIdentity := IntToStr(LockableObject.OID);
    if GetObjectLockingInterface.IsLocked(ObjectIdentity) then
    begin
      GetObjectLockingInterface.ReleaseLock(LockableObject.ObjectLock);
      LockableObject.ObjectLock := '';
      LockableObject.Locked := False;
      Assert(GetObjectLockingInterface.IsLocked(ObjectIdentity) = False);
    end
    else
    begin
      raise EObjectLockingClientException.Create('Object is not locked');
    end;
  end;
end;

end.
