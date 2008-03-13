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
  ;


type

  TestTtiLogToFile = class(TtiTestCase)
  published
    procedure TestLogFileContention;
    procedure Log;
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
  ,tiLogToFile;

procedure RegisterTests;
begin
  tiRegisterNonPersistentTest(TestTtiLogToFile);
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

procedure TestTtiLogToFile.Log;
begin
  CheckEquals(False,
    (GLog.FindByLogClass(TtiLogToFile) as TtiLogToFile).EnableCaching,
      'EnableCaching must be off for unit testing');
  GLog.Log('test');
  Check(True);
  // ToDo: Should check output in file
end;

procedure TestTtiLogToFile.TestLogFileContention;
var
  LLoggers: array of TtiLogToFile;
  i,j: integer;
  LThreadID: string;
  LMessage : string;
  LTimestamp: string;
  LLogFileName: string;

const
  CIterations = 100000;
  COverwriteOldFolders = true;

begin

  SetLength(LLoggers, Length(CLoggers));
  FmtStr(LThreadID, '%.4d', [GetCurrentThreadID]);

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

  Check(True); // To suppress DUnit2 warning

end;

end.


