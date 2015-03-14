unit tiLogToFile_TST;

{$I tiDefines.inc}

interface

uses
  tiTestFramework
  ,TestExtensions
  ,SysUtils
  ,Classes
  ;


type

  TtiLogTestCase = class(TtiTestCase)
  published
    procedure LogLevel;
  end;

  TtiLogToFileTestCase = class(TtiTestCase)
  private
    function LogFileLineCount(const AFileName: string): integer;
  published
    procedure tiLog_TestLogFileContention;
    procedure tiLog_Log;
    procedure tiLog_Purge;
    procedure tiLog_Free;
  end;

procedure RegisterTests;

implementation

uses
   tiTestDependencies
  {$IFDEF MSWINDOWS}
  ,Windows
  {$ENDIF}
  ,tiConstants
  ,tiLog
  ,tiLogToFile
  ,tiUtils
  ,SyncObjs
  ,tiThread
  ,tiExcept
  ;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TtiLogTestCase);
  tiRegisterNonPersistentTest(TtiLogToFileTestCase);
end;

type
  TtiLogToFileClass = class of TtiLogToFile;

  TtiLogToFile_0 = class (TtiLogToFile);
  TtiLogToFile_1 = class (TtiLogToFile);
  TtiLogToFile_2 = class (TtiLogToFile);
  TtiLogToFile_3 = class (TtiLogToFile);
  TtiLogToFile_4 = class (TtiLogToFile);

const
  CLoggers: array[0..4] of TtiLogToFileClass = (
    TtiLogToFile_0,
    TtiLogToFile_1,
    TtiLogToFile_2,
    TtiLogToFile_3,
    TtiLogToFile_4
  );

procedure TtiLogToFileTestCase.tiLog_Log;
var
  f: string;
begin
  CheckEquals(False,
    (GLog.FindByLogClass(TtiLogToFile) as TtiLogToFile).EnableCaching,
      'EnableCaching must be off for unit testing');
  GLog.Log('test');
  f := (GLog.FindByLogClass(TtiLogToFile) as TtiLogToFile).Filename;
  CheckEquals(True, FileExists(f), 'Failed on 1');
  // now clean up
  if not SysUtils.DeleteFile(f) then
    raise EtiOPFFileSystemException.CreateFmt(cErrorCanNotDeleteFile, [f]);
  // ToDo: Should check output in file
end;

function TtiLogToFileTestCase.LogFileLineCount(
  const AFileName: string): integer;
var
  LSL: TStringList;
begin
  LSL:= TStringList.Create;
  try
    LSL.LoadFromFile(AFileName);
    result:= LSL.Count;
  finally
    LSL.Free;
  end;
end;

type

  TThreadWriteToLog = class(TtiSleepThread)
  private
    FLog: TtiLog;
  public
    constructor Create(const ALog: TtiLog); reintroduce;
    procedure Execute; override;
  end;

  constructor TThreadWriteToLog.Create(const ALog: TtiLog);
  begin
    inherited Create(true);
    FreeOnTerminate:= False;
    FLog:= ALog;
    SleepResponse := 10;
  end;

  procedure TThreadWriteToLog.Execute;
  var
    i: integer;
  begin
    SleepAndCheckTerminated(Random(500));
    for i := 1 to 20000 do
      FLog.Log(IntToStr(i), lsNormal);
    FLog.Purge;
  end;

procedure TtiLogToFileTestCase.tiLog_Purge;
var
  LLog: TtiLog;
  LThreadList: TtiThreadList;
  i: Integer;
const
  CLogFileCount = 5;

  function _FileName(const AIndex: Integer): string;
  begin
    result := tiAddTrailingSlash(TempDirectory) +
        Format('LoggingTest%d.log', [AIndex]);
  end;

begin
  Randomize;
  LLog:= TtiLog.Create;
  try
    for i := 0 to CLogFileCount - 1 do
      LLog.RegisterLog(TtiLogToFile.CreateWithFileName(TempDirectory,
          ExtractFileName(_FileName(i)), true));

    LThreadList:= TtiThreadList.Create;
    try
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.ResumeAll;
      LThreadList.WaitForAll;
    finally
      LThreadList.Free;
    end;

    for i := 0 to CLogFileCount - 1 do
      { 200,000 is the 20,000 for-loop in each thread, times 10 threads }
      CheckEquals(200000, LogFileLineCount(_FileName(i)), 'Failed on logfile #'+IntToStr(i));
  finally
    LLog.Free;
  end;
end;

procedure TtiLogToFileTestCase.tiLog_TestLogFileContention;
var
  LLoggers: array of TtiLogToFile;
  i,j: integer;
  LThreadID: string;
  LMessage : string;
  LTimestamp: string;
  LLogFileName: string;

const
  CIterations = 5000;   //  Original was 100000. Why not make this a INI file parameter if needed.
  COverwriteOldFolders = true;

begin
  LLogFilename := tiGetTempFile('log'); // workable with limited user rights

  SetLength(LLoggers, Length(CLoggers));
  FmtStr(LThreadID, '%.4d', [PtrUInt(GetCurrentThreadID)]);

  for i := Low(LLoggers) to High(LLoggers) do
  begin
    LLoggers[i] := CLoggers[i].CreateWithFileName(
      ExtractFilePath(LLogFileName), ExtractFileName(LLogFileName),
      COverwriteOldFolders);
  end;

  try
    for i := 0 to CIterations - 1 do
    begin
      FmtStr(LMessage, 'Message (%.5d)', [i]);
      LTimestamp := FormatDateTime(cIntlDateTimeDisp, Now);

      for j := Low(LLoggers) to High(LLoggers) do
      begin
        LLoggers[j].Log(LTimestamp, LThreadID, LMessage, lsNormal);
      end;
    end;
  finally
    for i := Low(LLoggers) to High(LLoggers) do
      LLoggers[i].Free;
  end;

  CheckEquals(True, FileExists(LLogFilename), 'Failed on 1');
  // now clean up
  if not SysUtils.DeleteFile(LLogFilename) then
    raise EtiOPFFileSystemException.CreateFmt(cErrorCanNotDeleteFile, [LLogFilename]);
end;

procedure TtiLogToFileTestCase.tiLog_Free;
var
  LLog: TtiLog;
  LThreadList: TtiThreadList;
begin
  LLog:= TtiLog.Create;
  try
    LLog.RegisterLog(TtiLogToFile.CreateWithFileName(TempDirectory, 'LoggingTest1.log', true));
    LLog.RegisterLog(TtiLogToFile.CreateWithFileName(TempDirectory, 'LoggingTest2.log', true));
    LLog.RegisterLog(TtiLogToFile.CreateWithFileName(TempDirectory, 'LoggingTest3.log', true));
    LLog.RegisterLog(TtiLogToFile.CreateWithFileName(TempDirectory, 'LoggingTest4.log', true));
    LLog.Log('test', lsNormal);
    LThreadList:= TtiThreadList.Create;
    try
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.ResumeAll;
      LThreadList.WaitForAll;
    finally
      LThreadList.Free;
    end;
  finally
    LLog.Free;
  end;
  Check(True);  // Not sure what to check
end;

{ TtiLogTestCase }

procedure TtiLogTestCase.LogLevel;
begin
 Check(True);
 // ToDo: Implement
end;

end.


