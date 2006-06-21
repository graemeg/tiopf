{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  The contents of this file are subject to the Mozilla Public
  License Version 1.1 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of
  the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS
  IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
  implied. See the License for the specific language governing
  rights and limitations under the License.

  Originally developed and released by Yi Chen, Darren Camp and Tom Tuddenham.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit tiXmlRpcObjectLockingServer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,

  xmlrpcserver, xmlrpctypes,

  tiObjectLockingService, StdCtrls;

type
  TXMLRPCServerForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FObjectLockingService: TtiObjectLockingService;
    FServer: TServer;
    FAcquireLockMethodHandler: TMethodHandler;
    FReleaseLockMethodHandler: TMethodHandler;
    FIsLockedMethodHandler: TMethodHandler;
    FReleaseAllLocksMethodHandler: TMethodHandler;
    procedure StartServer;
    procedure StopServer;
    function AcquireLock(const MethodName: string; const plist: TList; const return: TReturn);
    function IsLocked(const MethodName: string; const plist: TList; const return: TReturn);
    procedure ReleaseLock(const MethodName: string; const plist: TList; const return: TReturn);
    procedure ReleaseAllLocks(const MethodName: string; const plist: TList; const return: TReturn);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure RequestLock(const MethodName: string; const plist: TList; const return: TReturn);
  end;

var
  XMLRPCServerForm: TXMLRPCServerForm;

implementation

{$R *.DFM}

{ TForm1 }

procedure TXMLRPCServerForm.AfterConstruction;
begin
  inherited;
  FObjectLockingService := TtiObjectLockingService.Create;
end;

procedure TXMLRPCServerForm.BeforeDestruction;
begin
  FObjectLockingService.Free;
  inherited;
end;

procedure TXMLRPCServerForm.StartServer;
begin
  FServer := TServer.Create;
  FServer.ListenPort := 8002;
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
  FReleaseLockMethodHandler.Signature := 'string (string ObjectLock)';
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
  FReleaseAllLocksMethodHandler.Signature := '';
  FReleaseAllLocksMethodHandler.Help      := 'Request all locks for all object identities';
  FServer.RegisterMethodHandler(FReleaseAllLocksMethodHandler);

  FServer.Active := True;
end;

procedure TXMLRPCServerForm.StopServer;
begin
  FServer.Active := False;
  FAcquireLockMethodHandler.Free;
  FReleaseLockMethodHandler.Free;
  FIsLockedMethodHandler.Free;
  FReleaseAllLocksMethodHandler.Free;
  FServer.Free;
end;

{ TXMLRPCServerForm - events }

procedure TXMLRPCServerForm.Button1Click(Sender: TObject);
begin
  StartServer;
end;

procedure TXMLRPCServerForm.Button2Click(Sender: TObject);
begin
  StopServer;
end;

end.
 
