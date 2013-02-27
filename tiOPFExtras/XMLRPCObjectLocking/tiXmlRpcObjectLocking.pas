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
unit tiXmlRpcObjectLocking;

interface

uses
  Classes,

  XmlRpcServer, XmlRpcTypes, XmlRpcClient,

  tiObjectLocking;

type
  TtiXmlRpcObjectLockingServer = class
  private
    FObjectLocking: TtiObjectLocking;
    FServer: TServer;
    FAcquireLockMethodHandler: TMethodHandler;
    FReleaseLockMethodHandler: TMethodHandler;
    FIsLockedMethodHandler: TMethodHandler;
    FReleaseAllLocksMethodHandler: TMethodHandler;
    FListenPort: Integer;
    procedure AcquireLock(const MethodName: string; const plist: TList; const return: TReturn);
    procedure IsLocked(const MethodName: string; const plist: TList; const return: TReturn);
    procedure ReleaseLock(const MethodName: string; const plist: TList; const return: TReturn);
    procedure ReleaseAllLocks(const MethodName: string; const plist: TList; const return: TReturn);
    function GetStarted: Boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Start;
    procedure Stop;
    property ListenPort: Integer read FListenPort write FListenPort;
    property Started: Boolean read GetStarted;
  end;

  TtiXmlRpcObjectLockingClient = class(TInterfacedObject, ItiObjectLocking)
  private
    FHostPort: Integer;
    FHostName: string;
    function CreateCaller: TCaller;
  public
    procedure AfterConstruction; override;
    function AcquireLock(ObjectIdentity: string): string; stdcall;
    function IsLocked(ObjectIdentity: string): Boolean; stdcall;
    procedure ReleaseLock(ObjectLock: string); stdcall;
    procedure ReleaseAllLocks; stdcall;
    property HostName: string read FHostName write FHostName;
    property HostPort: Integer read FHostPort write FHostPort;
  end;


const
  DefaultHost = '127.0.0.1';
  DefaultPort = 801;


implementation

{ TtiXmlRpcObjectLockingServer }

procedure TtiXmlRpcObjectLockingServer.AfterConstruction;
begin
  inherited;
  FObjectLocking := TtiObjectLocking.Create;
  FListenPort := DefaultPort;
end;

procedure TtiXmlRpcObjectLockingServer.BeforeDestruction;
begin
  if Started then Stop;
  FObjectLocking.Free;
  inherited;
end;

procedure TtiXmlRpcObjectLockingServer.AcquireLock(const MethodName: string;
  const plist: TList; const return: TReturn);
var
  ObjectIdentity: string;
  ObjectLock: string;
begin
  ObjectIdentity := TParameter(plist[0]).GetString;
  ObjectLock := FObjectLocking.AcquireLock(ObjectIdentity);
  Return.AddParam(ObjectLock);
end;

function TtiXmlRpcObjectLockingServer.GetStarted: Boolean;
begin
  Result := Assigned(FServer) and FServer.Active;
end;

procedure TtiXmlRpcObjectLockingServer.IsLocked(const MethodName: string;
  const plist: TList; const return: TReturn);
var
  ObjectIdentity: string;
  ObjectIsLocked: Boolean;
begin
  ObjectIdentity := TParameter(plist[0]).GetString;
  ObjectIsLocked := FObjectLocking.IsLocked(ObjectIdentity);
  Return.AddParam(ObjectIsLocked);
end;

procedure TtiXmlRpcObjectLockingServer.ReleaseAllLocks(const MethodName: string;
  const plist: TList; const return: TReturn);
begin
  FObjectLocking.ReleaseAllLocks;
end;

procedure TtiXmlRpcObjectLockingServer.ReleaseLock(const MethodName: string;
  const plist: TList; const return: TReturn);
var
  ObjectLock: string;
begin
  ObjectLock := TParameter(plist[0]).GetString;
  FObjectLocking.ReleaseLock(ObjectLock);
end;

procedure TtiXmlRpcObjectLockingServer.Start;
begin
  if Started then
    raise EObjectLockingServerException.Create('XmlRpcObjectLockingServer already started');
  FServer := TServer.Create;
  FServer.ListenPort := FListenPort;
  FServer.EnableIntrospect := True;

  FAcquireLockMethodHandler           := TMethodHandler.Create;
  FAcquireLockMethodHandler.Name      := 'TechInsite.AcquireLock';
  FAcquireLockMethodHandler.Method    := AcquireLock;
  FAcquireLockMethodHandler.Signature := 'string (string ObjectIdentity)';
  FAcquireLockMethodHandler.Help      := 'Request a lock for an object identity';
  FServer.RegisterMethodHandler(FAcquireLockMethodHandler);

  FReleaseLockMethodHandler           := TMethodHandler.Create;
  FReleaseLockMethodHandler.Name      := 'TechInsite.ReleaseLock';
  FReleaseLockMethodHandler.Method    := ReleaseLock;
  FReleaseLockMethodHandler.Signature := 'void (string ObjectLock)';
  FReleaseLockMethodHandler.Help      := 'Release a lock held for an object identity';
  FServer.RegisterMethodHandler(FReleaseLockMethodHandler);

  FIsLockedMethodHandler           := TMethodHandler.Create;
  FIsLockedMethodHandler.Name      := 'TechInsite.IsLocked';
  FIsLockedMethodHandler.Method    := IsLocked;
  FIsLockedMethodHandler.Signature := 'boolean (string ObjectIdentity)';
  FIsLockedMethodHandler.Help      := 'Test if an object identity is locked';
  FServer.RegisterMethodHandler(FIsLockedMethodHandler);

  FReleaseAllLocksMethodHandler           := TMethodHandler.Create;
  FReleaseAllLocksMethodHandler.Name      := 'TechInsite.ReleaseAllLocks';
  FReleaseAllLocksMethodHandler.Method    := ReleaseAllLocks;
  FReleaseAllLocksMethodHandler.Signature := 'void';
  FReleaseAllLocksMethodHandler.Help      := 'Request all locks for all object identities';
  FServer.RegisterMethodHandler(FReleaseAllLocksMethodHandler);

  FServer.Active := True;
end;

procedure TtiXmlRpcObjectLockingServer.Stop;
begin
  if not Started then
    raise EObjectLockingServerException.Create('XmlRpcObjectLockingServer not started');
  FObjectLocking.ReleaseAllLocks;;
  FServer.Active := False;
  FAcquireLockMethodHandler.Free;
  FReleaseLockMethodHandler.Free;
  FIsLockedMethodHandler.Free;
  FReleaseAllLocksMethodHandler.Free;
  FServer.Free;
end;

{ TtiXmlRpcObjectLockingClient }

function TtiXmlRpcObjectLockingClient.AcquireLock(
  ObjectIdentity: string): string;
var
  Caller: TCaller;
  Func: TFunction;
  FuncResult: TResult;
begin
  Caller := CreateCaller;
  Func := TFunction.Create;
  try
    Func.ObjectMethod := 'TechInsite.AcquireLock';
    Func.AddParam(ObjectIdentity);
    FuncResult := Caller.Execute(Func);
    try
      if FuncResult.IsError then
        raise EObjectLockingClientException.Create('TechInsite.AcquireLock failed');

      Result := FuncResult.GetString;
    finally
      FuncResult.Free;
    end;
  finally
    Func.Free;
    Caller.Free;
  end;
end;

procedure TtiXmlRpcObjectLockingClient.AfterConstruction;
begin
  inherited;
  FHostName := DefaultHost;
  FHostPort := DefaultPort;
end;

function TtiXmlRpcObjectLockingClient.CreateCaller: TCaller;
begin
  Result := TCaller.Create;
  Result.HostName := FHostName;
  Result.HostPort := FHostPort;
  Result.EndPoint := '/';
end;

function TtiXmlRpcObjectLockingClient.IsLocked(
  ObjectIdentity: string): Boolean;
var
  Caller: TCaller;
  Func: TFunction;
  FuncResult: TResult;
begin
  Func := TFunction.Create;
  Caller := CreateCaller;
  Result := True;
  try
    Func.ObjectMethod := 'TechInsite.IsLocked';
    Func.AddParam(ObjectIdentity);
    FuncResult := Caller.Execute(Func);
    try
      if FuncResult.IsError then
        raise EObjectLockingClientException.Create('TechInsite.IsLocked failed');

      Result := FuncResult.GetBoolean;
    finally
      FuncResult.Free;
    end;
  finally
    Func.Free;
    Caller.Free;
  end;
end;

procedure TtiXmlRpcObjectLockingClient.ReleaseAllLocks;
var
  Caller: TCaller;
  Func: TFunction;
  FuncResult: TResult;
begin
  Func := TFunction.Create;
  Caller := CreateCaller;
  try
    Func.ObjectMethod := 'TechInsite.ReleaseAllLocks';
    FuncResult := Caller.Execute(Func);
    try
      if FuncResult.IsError then
        raise EObjectLockingClientException.Create('TechInsite.ReleaseAllLocks failed');
    finally
      FuncResult.Free;
      Caller.Free;
    end;
  finally
    Func.Free;
  end;
end;

procedure TtiXmlRpcObjectLockingClient.ReleaseLock(
  ObjectLock: string);
var
  Caller: TCaller;
  Func: TFunction;
  FuncResult: TResult;
begin
  Func := TFunction.Create;
  Caller := CreateCaller;
  try
    Func.ObjectMethod := 'TechInsite.ReleaseLock';
    Func.AddParam(ObjectLock);
    FuncResult := Caller.Execute(Func);
    try
      if FuncResult.IsError then
        raise EObjectLockingClientException.Create('TechInsite.ReleaseLock failed');
    finally
      FuncResult.Free;
      Caller.Free;
    end;
  finally
    Func.Free;
  end;
end;

end.
