program SleepFor;

//Console application that sleeps for the number of seconds provided in the -s command line argument
//then writes the calue of -s to a file, the name of which is provided in the -f parameter.

//Used by unit test for tiWin32.tiWin32RunProcessWithTimeout
{$APPTYPE CONSOLE}

uses
  SysUtils,
  tiUtils,
  tiCommandLineParams;

var
  LSecondsToSleep: Cardinal;
  LFileName: string;

const
  CUsage_Error = 'Usage: SleepFor -s (no. of secs to sleep) -f (log file name) ' + #10#13 +
                 'eg. SleepFor -s 10 -f SleepFor.log';
{$R *.res}

begin

  Assert(gCommandLineParams.IsParam('s'), CUsage_Error);
  Assert(gCommandLineParams.IsParam('f'), CUsage_Error);

  LSecondsToSleep := tiStrToInt(gCommandLineParams.GetParam('s'));
  LFileName       := gCommandLineParams.GetParam('f');
  WriteLn('Before sleep message');
  Sleep(LSecondsToSleep * 1000);
  WriteLn('After sleep message');
  tiStringToFile(gCommandLineParams.GetParam('s'), LFileName);

end.
