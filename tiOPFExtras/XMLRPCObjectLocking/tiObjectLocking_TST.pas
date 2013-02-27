unit tiObjectLocking_TST;

interface

uses
  Dialogs, sysutils,

  TestFramework,

  tiObjectLocking, tiXmlRpcObjectLocking, tiPersist;

type
  TtiObjectLocking_Test = class(TTestCase)
  protected
    procedure Setup; override;
    procedure Teardown; override;
  published
    procedure TestAcquireAndReleaseLockForClashingObjects; virtual;
    procedure TestAcquireAndReleaseLockForCompoundObject; virtual;
    procedure TestAcquireAndReleaseLockForSimpleObject; virtual;
    procedure TestAcquireAndReleaseLockForMultipleObjects; virtual;
  end;

  TtiXmlRpcObjectLocking_Test = class(TtiObjectLocking_Test)
  protected
    procedure Setup; override;
    procedure Teardown; override;
  end;

implementation

type
  TLockableCompoundObject = class(TLockablePerObj)
  private
    FContainedObject: TLockablePerObj;
  published
    property ContainedObject: TLockablePerObj read FContainedObject write FContainedObject;
  end;

{ TtiObjectLocking_Test }

procedure TtiObjectLocking_Test.Setup;
begin
  RegisterObjectLockingInterface(TtiObjectLocking.Create);
end;

procedure TtiObjectLocking_Test.Teardown;
begin
  RegisterObjectLockingInterface(nil);
end;

procedure TtiObjectLocking_Test.TestAcquireAndReleaseLockForClashingObjects;
var
  LockableObject1: TLockablePerObj;
  LockableObject2: TLockablePerObj;
begin
  LockableObject1 := TLockablePerObj.Create;
  LockableObject1.OID := 1000;
  LockableObject2 := TLockablePerObj.Create;
  LockableObject2.OID := 1000;
  try
    Check(LockableObject1.Locked = False, 'Object should not be locked yet');
    Check(LockableObject2.Locked = False, 'Object should not be locked yet');

    gTiPerMgr.VisMgr.Execute('AcquireLock', LockableObject1);
    Check(LockableObject1.Locked = True, 'Object should be locked');
    Check(LockableObject2.Locked = False, 'Object should not be locked');

    gTiPerMgr.VisMgr.Execute('AcquireLock', LockableObject2);
{ TODO -oTT -cNotes : 
The TechInsite framework should throw up a series of error messages
at this point. I'm not sure how to handle them as the exceptions don't 
appear to be accessible at this level of the code }

    Check(LockableObject1.Locked = True, 'Object should be locked');
    Check(LockableObject2.Locked = False, 'Object should be locked');

    gTiPerMgr.VisMgr.Execute('ReleaseLock', LockableObject1);
    Check(LockableObject1.Locked = False, 'Object should not be locked');
    Check(LockableObject2.Locked = False, 'Object should be locked');
  finally
    LockableObject1.Free;
    LockableObject2.Free;
  end;
end;

procedure TtiObjectLocking_Test.TestAcquireAndReleaseLockForCompoundObject;
var
  LockableCompoundObject: TLockableCompoundObject;
  LockableObject: TLockablePerObj;
begin
  LockableCompoundObject := TLockableCompoundObject.Create;
  LockableObject := TLockablePerObj.Create;

  LockableCompoundObject.OID := 1000;
  LockableObject.OID := 1001;

  LockableCompoundObject.ContainedObject := LockableObject;
  try
    Check(LockableCompoundObject.Locked = False, 'Object should not be locked yet');
    Check(LockableObject.Locked = False, 'Object should not be locked yet');

    gTiPerMgr.VisMgr.Execute('AcquireLock', LockableCompoundObject);

    Check(LockableCompoundObject.Locked = True, 'Object should now be locked');
    Check(LockableCompoundObject.ObjectLock <> '', 'ObjectLock should now be set');

    Check(LockableObject.Locked = True, 'Object should now be locked');
    Check(LockableObject.ObjectLock <> '', 'ObjectLock should now be set');

    gTiPerMgr.VisMgr.Execute('ReleaseLock', LockableCompoundObject);

    Check(LockableCompoundObject.Locked = False, 'Object should now be unlocked');
    Check(LockableObject.Locked = False, 'Object should now be unlocked');
  finally
    LockableObject.Free;
    LockableCompoundObject.Free;
  end;
end;

procedure TtiObjectLocking_Test.TestAcquireAndReleaseLockForSimpleObject;
var
  LockableObject: TLockablePerObj;
begin
  LockableObject := TLockablePerObj.Create;
  LockableObject.OID := 1000;
  try
    Check(LockableObject.Locked = False, 'Object should not be locked yet');
    gTiPerMgr.VisMgr.Execute('AcquireLock', LockableObject);
    Check(LockableObject.Locked = True, 'Object should now be locked');
    Check(LockableObject.ObjectLock <> '', 'ObjectLock should now be set');
    gTiPerMgr.VisMgr.Execute('ReleaseLock', LockableObject);
    Check(LockableObject.Locked = False, 'Object should now be unlocked');
  finally
    LockableObject.Free;
  end;
end;

procedure TtiObjectLocking_Test.TestAcquireAndReleaseLockForMultipleObjects;
var
  LockableObject1: TLockablePerObj;
  LockableObject2: TLockablePerObj;
begin
  LockableObject1 := TLockablePerObj.Create;
  LockableObject1.OID := 1000;
  LockableObject2 := TLockablePerObj.Create;
  LockableObject2.OID := 2000;
  try
    Check(LockableObject1.Locked = False, 'Object should not be locked yet');
    Check(LockableObject2.Locked = False, 'Object should not be locked yet');

    gTiPerMgr.VisMgr.Execute('AcquireLock', LockableObject1);
    Check(LockableObject1.Locked = True, 'Object should be locked');
    Check(LockableObject2.Locked = False, 'Object should not be locked');

    gTiPerMgr.VisMgr.Execute('AcquireLock', LockableObject2);
    Check(LockableObject1.Locked = True, 'Object should be locked');
    Check(LockableObject2.Locked = True, 'Object should be locked');

    Check(LockableObject1.ObjectLock <> LockableObject2.ObjectLock ,
      'ObjectLocks are the same');

    gTiPerMgr.VisMgr.Execute('ReleaseLock', LockableObject1);
    Check(LockableObject1.Locked = False, 'Object should not be locked');
    Check(LockableObject2.Locked = True, 'Object should be locked');

    gTiPerMgr.VisMgr.Execute('ReleaseLock', LockableObject2);
    Check(LockableObject1.Locked = False, 'Object should not be locked');
    Check(LockableObject2.Locked = False, 'Object should not be locked');

  finally
    LockableObject1.Free;
    LockableObject2.Free;
  end;
end;

{ TtiXmlRpcObjectLocking_Test }

procedure TtiXmlRpcObjectLocking_Test.Setup;
begin
  RegisterObjectLockingInterface(TtiXmlRpcObjectLockingClient.Create);
end;

procedure TtiXmlRpcObjectLocking_Test.Teardown;
begin
  RegisterObjectLockingInterface(nil);
end;
//
//procedure TtiXmlRpcObjectLocking_Test.TestAcquireAndReleaseLockForClashingObjects;
//begin
//  inherited;
//end;
//
//procedure TtiXmlRpcObjectLocking_Test.TestAcquireAndReleaseLockForCompoundObject;
//begin
//  inherited;
//end;
//
//procedure TtiXmlRpcObjectLocking_Test.TestAcquireAndReleaseLockForMultipleObjects;
//begin
//  inherited;
//end;
//
//procedure TtiXmlRpcObjectLocking_Test.TestAcquireAndReleaseLockForSimpleObject;
//begin
//  inherited;
//end;

initialization
  RegisterTest('TechInsite.ObjectLocking', TtiObjectLocking_Test);
  RegisterTest('TechInsite.ObjectLocking', TtiXmlRpcObjectLocking_Test);
  gTIPerMgr.VisMgr.DefaultDBConnectionName := 'dummy';
  gTIPerMgr.VisMgr.RegisterVisitor('AcquireLock', TVisLockablePerObjAcquireLock);
  gTIPerMgr.VisMgr.RegisterVisitor('ReleaseLock', TVisLockablePerObjReleaseLock);
end.
