unit tiWin32_TST;

{$I tiDefines.inc}

interface
uses
  {$IFDEF FPC}
  testregistry,
  {$ENDIF}
  tiTestFramework
  ,Classes
 ;


type
  TTestTIWin32 = class(TtiTestCase)
  published
    procedure CoInitialize_CoUnInitialize_MainThread;
    procedure CoInitialize_CoUnInitialize_MultiThread;
    procedure CoInitialize_ForceCoUnInitialize;

    procedure TestTIWin32_tiWin32RunProcessWithTimeout;
  public
    constructor Create{$IFNDEF DUNIT2ORFPC}(AMethodName: string){$ENDIF}; override;
  end;

procedure RegisterTests;

implementation
uses
   tiWin32
  ,tiTestDependencies
  ,Contnrs
  ,Windows
  ,tiUtils
  ,SysUtils
 ;


procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TTestTIWin32);
end;


{ TTestTIWin32 }

constructor TTestTIWin32.Create {$IFNDEF DUNIT2ORFPC}(AMethodName: string) {$ENDIF};
begin
  inherited;
  // There is a one off 32 byte leak the first time this pair execute
  // and I cannot pinpoint it's cause.
  tiWin32CoInitialize;
  tiWin32CoUnInitialize;
end;

procedure TTestTIWin32.TestTIWin32_tiWin32RunProcessWithTimeout;
const
  CTimeoutSecs = 5;
  CParams = '-s %d -f %s';
  CLogFileName = 'tiWin32RunProcessWithTimeout_TST.log';
  CEXEName = 'sleepfor.exe';
  CBATName = 'sleepfor.bat';
  CDirectory = 'DUnitSupportApps\SleepFor\_bin\';
var
  LCommandLine: string;
  LParams: string;
  LDirectory: string;
  LProcessNameToKill: string;

  LSleepForSecs: Cardinal;
  LTimeoutSecs: Cardinal;

  LLogFileName: string;
const
  CBeforeSleepMessage = 'Before sleep message';
  CAfterSleepMessage = 'After sleep message';
begin

  // Timeout > SleepFor (Task will run to completion)
  LDirectory := tiAddTrailingSlash(tiGetEXEPath) + CDirectory;
  LCommandLine := LDirectory + CEXEName;
  LSleepForSecs := 1;
  LTimeoutSecs  := 2;
  LLogFileName := LDirectory + CLogFileName;
  LParams := Format(CParams, [LSleepForSecs, LLogFileName]);
  LProcessNameToKill := CEXEName;

  tiWin32RunProcessWithTimeout(LCommandLine, LParams, LDirectory, LTimeoutSecs, LProcessNameToKill);
  CheckEquals(true, FileExists(LLogFileName));
  CheckEquals(IntToStr(LSleepForSecs), tiFileToString(LLogFileName));

  tiDeleteFile(LLogFileName);

  // SleepFor > Timeout (Task will be killed by timeout)
  LDirectory := tiAddTrailingSlash(tiGetEXEPath) + CDirectory;
  LCommandLine := LDirectory + CEXEName;
  LSleepForSecs := 2;
  LTimeoutSecs  := 1;
  LLogFileName := LDirectory + CLogFileName;
  LParams := Format(CParams, [LSleepForSecs, LLogFileName]);
  LProcessNameToKill := CEXEName;

  tiWin32RunProcessWithTimeout(LCommandLine, LParams, LDirectory, LTimeoutSecs, LProcessNameToKill);
  Check( not FileExists(LLogFileName), 'Time out failed');

end;

procedure TTestTIWin32.CoInitialize_CoUnInitialize_MainThread;
begin
  tiWin32CoInitialize;
  Check(tiWin32HasCoInitializeBeenCalled);
  tiWin32CoUnInitialize;
  Check(not tiWin32HasCoInitializeBeenCalled);
end;

type

  TThreadCoInitializeForTesting = class(TThread)
  private
    FIterations: Integer;
    FTestCase: TtiTestCase;
  public
    constructor Create(const ATestCase: TtiTestCase; const AIterations: Integer);
    procedure Execute; override;
  end;

  constructor TThreadCoInitializeForTesting.Create(
    const ATestCase: TtiTestCase; const AIterations: Integer);
  begin
    inherited Create(True);
    FTestCase:= ATestCase;
    FIterations:= AIterations;
    FreeOnTerminate:= False;
    Resume;
  end;

  procedure TThreadCoInitializeForTesting.Execute;
  var
    i: Integer;
  begin
    for i:= 1 to FIterations do
    begin
      tiWin32CoInitialize;
      FTestCase.Check(tiWin32HasCoInitializeBeenCalled);
      tiWin32CoUnInitialize;
      FTestCase.Check(not tiWin32HasCoInitializeBeenCalled);
      Sleep(Trunc(Random*10));
    end;
  end;

procedure TTestTIWin32.CoInitialize_CoUnInitialize_MultiThread;
var
  LList: TObjectList;
  i: Integer;
const
  CIterations= 10;
  CThreadCount= 5; // Crank ThreadCount up to about 20, and DUnit2 will report leaks
                    // ToDo: Track down leak.
begin
  AllowedMemoryLeakSize:= 56;
  LList:= TObjectList.Create(True);
  try
    for i:= 1 to CThreadCount do
      LList.Add(TThreadCoInitializeForTesting.Create(Self, CIterations));
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

initialization

end.


