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
  public
    constructor Create(AMethodName: string); reintroduce; virtual;
  published
    procedure CoInitialize_CoUnInitialize_MainThread;
    procedure CoInitialize_CoUnInitialize_MultiThread;
    procedure CoInitialize_ForceCoUnInitialize;

    procedure TestTIWin32_tiWin32RunEXEAndWait;
    procedure TestTIWin32_tiWin32RunEXEAndWaitWithParams;
    procedure TestTIWin32_tiWin32RunProcessWithTimeout;
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
  inherited Create;
  // There is a one off 32 byte leak the first time this pair execute
  // and I cannot pinpoint it's cause.
  tiWin32CoInitialize;
  tiWin32CoUnInitialize;
end;

procedure TTestTIWin32.TestTIWin32_tiWin32RunEXEAndWait;
//const
//  CTimeoutSecs = 5;
//  CParams = '-s %d -f %s';
//  CLogFileName = 'tiWin32RunProcessWithTimeout_TST.log';
//  CEXEName = 'sleepfor.exe';
//  CBATName = 'sleepfor.bat';
//  CDirectory = 'DUnitSupportApps\SleepFor\_bin\';
//var
//  LCommandLine: string;
//  LParams: string;
//  LDirectory: string;
//  LSleepForSecs: Cardinal;
//  LTimeoutSecs: Cardinal;
//  LLogFileName: string;
//  LResult: DWord;
//  LStartTime: cardinal;
//  LEndTime: cardinal;
//  LSecsTaken: cardinal;
//const
//  CBeforeSleepMessage = 'Before sleep message';
//  CAfterSleepMessage = 'After sleep message';
begin
// Commented out to get broken build working. Was failing because signature of tiWin32RunProcessWithTimeout has been changed.
//  // Timeout > SleepFor (Task will run to completion)
//  LDirectory := tiAddTrailingSlash(tiGetEXEPath) + CDirectory;
//  LCommandLine := LDirectory + CEXEName;
//  LSleepForSecs := 1;
//  LTimeoutSecs  := 60;
//  LLogFileName := LDirectory + CLogFileName;
//  LParams := Format(CParams, [LSleepForSecs, LLogFileName]);
//
//  LStartTime := GetTickCount;
//  LResult:= tiWin32RunProcessWithTimeout(LCommandLine, LParams, LDirectory, LTimeoutSecs);
//  LEndTime := GetTickCount;
//
//  LSecsTaken := (LEndTime - LStartTime) div 1000;
//  Check(LSecsTaken < LSleepForSecs);
//
//  CheckEquals(WAIT_OBJECT_0, LResult, 'tiWin32RunProcessWithTimeout result');
//  CheckEquals(true, FileExists(LLogFileName));
//  CheckEquals(IntToStr(LSleepForSecs), tiFileToString(LLogFileName));
//
//  tiDeleteFile(LLogFileName);
//
//  // SleepFor > Timeout (Task will be killed by timeout)
//  LDirectory := tiAddTrailingSlash(tiGetEXEPath) + CDirectory;
//  LCommandLine := LDirectory + CEXEName;
//  LSleepForSecs := 60;
//  LTimeoutSecs  := 1;
//  LLogFileName := LDirectory + CLogFileName;
//  LParams := Format(CParams, [LSleepForSecs, LLogFileName]);
//
//  LStartTime := GetTickCount;
//  LResult:= tiWin32RunProcessWithTimeout(LCommandLine, LParams, LDirectory, LTimeoutSecs);
//  LEndTime := GetTickCount;
//
//  LSecsTaken := (LEndTime - LStartTime) div 1000;
//  Check(LSecsTaken < LSleepForSecs);
//
//  CheckEquals(WAIT_TIMEOUT, LResult, 'tiWin32RunProcessWithTimeout result');
//  Check( not FileExists(LLogFileName), 'Time out failed');

end;

procedure TTestTIWin32.TestTIWin32_tiWin32RunEXEAndWaitWithParams;
begin

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
  LCreateProcessParams: TtiCreateProcessParams;
  LCreateProcessResults: TtiCreateProcessResult;
  LSleepForSecs: Cardinal;
  LLogFileName: string;
  LResult: Boolean;
  LStartTime: cardinal;
  LEndTime: cardinal;
  LSecsTaken: cardinal;
const
  CBeforeSleepMessage = 'Before sleep message';
  CAfterSleepMessage = 'After sleep message';
begin

  // Timeout > SleepFor (Task will run to completion)
  LCreateProcessParams.WorkingDirectory := tiAddTrailingSlash(tiGetEXEPath) + CDirectory;
  LCreateProcessParams.CommandLine := LCreateProcessParams.WorkingDirectory + CEXEName;
  LSleepForSecs := 1;
  LCreateProcessParams.TimeoutAfterSecs  := 60;
  LLogFileName := LCreateProcessParams.WorkingDirectory + CLogFileName;
  LCreateProcessParams.CommandLineParams := Format(CParams, [LSleepForSecs, LLogFileName]);

  LStartTime := GetTickCount;
  LResult:= tiWin32RunProcessWithTimeout(LCreateProcessParams, LCreateProcessResults);
  LEndTime := GetTickCount;

  LSecsTaken := (LEndTime - LStartTime) div 1000;
  Check(LSecsTaken < LCreateProcessParams.TimeoutAfterSecs);

  CheckTrue(LResult, 'tiWin32RunProcessWithTimeout result');
  CheckEquals(0, LCreateProcessResults.ErrorCode);
  CheckEquals(0, LCreateProcessResults.WaitResult);
  CheckEquals(true, FileExists(LLogFileName));
  CheckEquals(IntToStr(LSleepForSecs), tiFileToString(LLogFileName));

  tiDeleteFile(LLogFileName);

  // SleepFor > Timeout (Task will be killed by timeout)
  LCreateProcessParams.WorkingDirectory := tiAddTrailingSlash(tiGetEXEPath) + CDirectory;
  LCreateProcessParams.CommandLine := LCreateProcessParams.WorkingDirectory + CEXEName;
  LSleepForSecs := 60;
  LCreateProcessParams.TimeoutAfterSecs  := 1;
  LLogFileName := LCreateProcessParams.WorkingDirectory + CLogFileName;
  LCreateProcessParams.CommandLineParams := Format(CParams, [LSleepForSecs, LLogFileName]);

  LStartTime := GetTickCount;
  LResult:= tiWin32RunProcessWithTimeout(LCreateProcessParams, LCreateProcessResults);
  LEndTime := GetTickCount;

  LSecsTaken := (LEndTime - LStartTime) div 1000;
  Check(LSecsTaken < LSleepForSecs);

  CheckFalse(LResult, 'tiWin32RunProcessWithTimeout result');
  CheckEquals(0, LCreateProcessResults.ErrorCode);
  CheckEquals(WAIT_TIMEOUT, LCreateProcessResults.WaitResult);
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
    inherited Create(False);
    FTestCase:= ATestCase;
    FIterations:= AIterations;
    FreeOnTerminate:= False;
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


