unit tiLogToFile_TST;

{$I tiDefines.inc}

interface

uses
  tiTestFramework
  {$IFDEF FPC}
  ,testregistry
  ,testdecorator
  {$ELSE}
  ,TestExtensions
  {$ENDIF}
  ,SysUtils
  ,Classes
  ,TypInfo
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
    procedure tiLog_Skip;
    procedure tiLog_Free;
  end;

procedure RegisterTests;

implementation

uses
  Contnrs  // used to remove Delphi's H2443 inline function hint
  ,tiTestDependencies
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

const
  CLogEventCount = 20000;

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
    for i := 1 to CLogEventCount do
      FLog.Log(IntToStr(i), lsNormal);
    FLog.Purge;
  end;

procedure TtiLogToFileTestCase.tiLog_Purge;
var
  LLog: TtiLog;
  LLogTo: TtiLogToFile;
  LThreadList: TtiThreadList;
  i: Integer;
const
  CLogFileCount = 5;
  CThreadWriterCount = 10;

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
    begin
      LLogTo := TtiLogToFile.CreateWithFileName(TempDirectory,
          ExtractFileName(_FileName(i)), true);
      LLogTo.MaxListSize := CLogEventCount * CThreadWriterCount; // No skipping
      LLog.RegisterLog(LLogTo);
    end;

    LThreadList:= TtiThreadList.Create;
    try
      for i := 1 to CThreadWriterCount do
        LThreadList.Add(TThreadWriteToLog.Create(LLog));
      LThreadList.StartAll;
      LThreadList.WaitForAll;
    finally
      LThreadList.Free;
    end;

    for i := 0 to CLogFileCount - 1 do
      CheckEquals(CLogEventCount * CThreadWriterCount, LogFileLineCount(_FileName(i)));
  finally
    LLog.Free;
  end;
end;

procedure TtiLogToFileTestCase.tiLog_Skip;
var
  LLog: TtiLog;
  LLogTo: TtiLogToList;
  i: Integer;
const
  CMaxListSize = 5;
  CEventsToSkip = 3;
begin
  LLog := TtiLog.Create;
  try
    LLogTo := TtiLogToList.Create;
    LLogTo.MaxListSize := CMaxListSize;
    LLog.RegisterLog(LLogTo);

    // Log events to produce the following:
    // Norm 1
    // Norm 2
    // Norm ...
    // Norm n
    // Warn Limit reached
    // Warn Skipped m
    // Norm n+m+1

    // Log n, skip m
    for i := 1 to CMaxListSize + CEventsToSkip do
      LLog.Log(IntToStr(i), lsNormal);
    // Increase size so we can resume logging
    LLogTo.MaxListSize := CMaxListSize + 2 + 1; // Prev max + 2xWarn + 1 new
    // Log another item
    LLog.Log(IntToStr(CMaxListSize + CEventsToSkip + 1), lsNormal);

    // Now check the list contains the expected items
    CheckEquals(CMaxListSize + 2 + 1, LLogTo.Count);
    // Before skip
    for i := 0 to CMaxListSize - 1 do
      CheckEquals(IntToStr(i+1), LLogTo.Items[i].LogMessage);
    // Skip starts
    CheckEquals(Format('Log limit of %d reached, skipping events...', [CMaxListSize]),
        LLogTo.Items[CMaxListSize].LogMessage);
    // Skip ends
    CheckEquals(Format('%d log events were skipped', [CEventsToSkip]),
        LLogTo.Items[CMaxListSize + 1].LogMessage);
    // After skip
    CheckEquals(IntToStr(CMaxListSize + CEventsToSkip + 1),
        LLogTo.Items[CMaxListSize + 2].LogMessage);
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
      LThreadList.StartAll;
      LThreadList.WaitForAll;

    finally
      LThreadList.Free;
    end;
  finally
    LLog.Free;
  end;
  //Just ensure there is no exception
  Check(True);
end;

{ TtiLogTestCase }

procedure TtiLogTestCase.LogLevel;
var
   LLog: TtiLog;
   LMinLevel,LCurLevel : TtiLogSeverity;
   LWrittenString : string;
   LFileName : TFileName;
   LFileLog  : TtiLogToFile;
begin
   LLog:= TtiLog.Create;
  try
    LFileLog := TtiLogToFile.CreateWithFileName(TempDirectory, 'LoggingTest1.log', true);
    LFileName := LFileLog.FileName;
    LLog.RegisterLog(LFileLog);

    for LMinLevel := Low(TtiLogSeverity) to High(TtiLogSeverity) do
    begin
      //only log if within minLevel to maxLevel
      LLog.SevToLog := [LMinLevel .. High(TtiLogSeverity)];
      for LCurLevel := Low(TtiLogSeverity) to High(TtiLogSeverity) do
      begin
        LWrittenString := 'Level set as ' + GetEnumName(TypeInfo(TtiLogSeverity),integer(LMinLevel)) +
                            ' written as ' + GetEnumName(TypeInfo(TtiLogSeverity),integer(LCurLevel));

        LLog.Log(LWrittenString, LCurLevel);
        LLog.Purge;
        if LCurLevel >= LMinLevel then
          Check(tiStrPos(PChar(tiUtils.tiFileToString(LFileName)), PChar(LWrittenString)) <> nil,
                  'No matching line found when it should have been logged '+ GetEnumName(TypeInfo(TtiLogSeverity),integer(LMinLevel)) + ' ' +  GetEnumName(TypeInfo(TtiLogSeverity),integer(LCurLevel)))
        else
          Check(tiStrPos(PChar(tiUtils.tiFileToString(LFileName)), PChar(LWrittenString)) = nil,
                  'Matching line found when it shouldnt have been ' + GetEnumName(TypeInfo(TtiLogSeverity),integer(LMinLevel)) + ' ' +  GetEnumName(TypeInfo(TtiLogSeverity),integer(LCurLevel)));
      end;

    end;
  finally
    LLog.Free;
  end;

end;

end.


