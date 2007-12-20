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

  // test decorator class which provides SetUp and TearDown
  // to straddle execution of TestTtiLogToFile methods

  tiLogToFileTestSetup = class(TTestSetup)
  protected
    procedure {$IFDEF FPC}OneTime{$ENDIF}SetUp; override;
    procedure {$IFDEF FPC}OneTime{$ENDIF}TearDown; override;
  end;

  // Test methods for class TtiLogToFile

  TestTtiLogToFile = class(TtiTestCase)
  private
    FLogFileName: string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLogFileContention;
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
  , TestFramework;

const
  CTestLogFileName = '..' + PathDelim + 'Data' + PathDelim;

var
  uTempDirectory: string;

procedure RegisterTests;
begin
  RegisterNonPersistentTest(tiLogToFileTestSetup, TestTtiLogToFile);
end;

{ tiLogToFileTestSetup }

procedure tiLogToFileTestSetup.{$IFDEF FPC}OneTime{$ENDIF}SetUp;
begin
  ReleaseLog;
  uTempDirectory := TtiTestCase.TempDirectory;
end;

procedure tiLogToFileTestSetup.{$IFDEF FPC}OneTime{$ENDIF}TearDown;
begin
  uTempDirectory := '';
  ReleaseLog;
end;

procedure TestTtiLogToFile.SetUp;
begin
  FmtStr(FLogFileName, '%s' + PathDelim + '%s', [uTempDirectory, 'TestTtiLogToFile.log']);
end;

procedure TestTtiLogToFile.TearDown;
begin
  FLogFileName := '';
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

procedure TestTtiLogToFile.TestLogFileContention;
var
  LLoggers: array of TtiLogToFile;
  i,j: integer;
  LThreadID: string;
  LMessage : string;
  LTimestamp: string;

const
  CIterations = 100000;
  COverwriteOldFolders = true;

begin
  FailsOnNoChecksExecuted := False;  // Prevent NoCheck failure but mark where this is applied.
  SetLength(LLoggers, Length(CLoggers));
  FmtStr(LThreadID, '%.4d', [GetCurrentThreadID]);

  for i := Low(LLoggers) to High(LLoggers) do
  begin
    LLoggers[i] := CLoggers[i].CreateWithFileName(
      ExtractFilePath(FLogFileName), ExtractFileName(FLogFileName),
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

end;

end.


