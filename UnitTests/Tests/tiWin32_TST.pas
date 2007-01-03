unit tiWin32_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry
  {$ELSE}
  TestFramework
  {$ENDIF}
  ,tiTestFramework
  ,Classes
 ;


type
  TTestTIWin32 = class(TtiTestCase)
  published
    procedure CoInitialize_CoUnInitialize_MainThread;
    procedure CoInitialize_CoUnInitialize_MultiThread;
    procedure CoInitialize_ForceCoUnInitialize;
  end;


  TThreadCoInitializeTest = class(TThread)
  private
    FIterations: Integer;
  public
    constructor Create(AIterations: Integer);
    procedure Execute; override;
  end;
  

procedure RegisterTests;


implementation
uses
   tiWin32
  ,tiDUnitDependencies
  ,tstPerFramework_BOM
  ,tiDUnitUtils
  ,tiDUnitINI
  ,Contnrs
  ,Windows
 ;


procedure RegisterTests;
begin
  RegisterNonPersistentTest(TTestTIWin32);
end;


{ TTestTIWin32 }

procedure TTestTIWin32.CoInitialize_CoUnInitialize_MainThread;
begin
  tiWin32CoInitialize;
  Check(tiWin32HasCoInitializeBeenCalled);
  tiWin32CoUnInitialize;
  Check(not tiWin32HasCoInitializeBeenCalled);
end;


procedure TTestTIWin32.CoInitialize_CoUnInitialize_MultiThread;
var
  LList: TObjectList;
  i: Integer;
const
  cIterations= 10;
  cThreadCount= 100;
begin
  LList:= TObjectList.Create(True);
  try
    for i:= 1 to cThreadCount do
      LList.Add(TThreadCoInitializeTest.Create(cIterations));
    for i:= 0 to LList.Count-1 do
      TThread(LList.Items[i]).WaitFor;
  finally
    LList.Free;
  end;
end;


procedure TTestTIWin32.CoInitialize_ForceCoUnInitialize;
begin
  tiWin32ForceCoInitialize;
  Check(tiWin32HasCoInitializeBeenCalled);
  tiWin32CoUnInitialize;
  Check(not tiWin32HasCoInitializeBeenCalled);
end;

{ TThreadCoInitializeTest }

constructor TThreadCoInitializeTest.Create(AIterations: Integer);
begin
  inherited Create(True);
  FIterations:= AIterations;
  FreeOnTerminate:= False;
  Resume;
end;


procedure TThreadCoInitializeTest.Execute;
var
  i: Integer;
begin
  for i:= 1 to FIterations do
  begin
    tiWin32CoInitialize;
    Assert(tiWin32HasCoInitializeBeenCalled);
    tiWin32CoUnInitialize;
    Assert(not tiWin32HasCoInitializeBeenCalled);
    Sleep(Trunc(Random*100));
  end;
end;


end.

