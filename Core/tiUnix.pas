{ Base Unix (Linux, MacOS, *BSD) specific functions used by tiUtils. }
unit tiUnix;

{$I tiDefines.inc}

interface
uses
  SysUtils
  ;

  procedure tiUnixRunEXEAndWait(pStrEXE: string);
  function  tiUnixGetTickCount: Cardinal;
  function  tiUnixGetUserName: string;
  function  tiUnixGetComputerName: string;
  function  IsCharAlpha(AChar: Char): Boolean;


implementation
uses
  Process
  ,unix
  ;


procedure tiUnixRunEXEAndWait(pStrEXE: string);
var
  lProcess: TProcess;
begin
  lProcess := TProcess.Create(nil);
  lProcess.CommandLine := pStrEXE;
  lProcess.Options := [poNoConsole, poWaitOnExit];
  lProcess.Execute;
end;

function tiUnixGetTickCount: Cardinal;
begin
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
end;

function tiUnixGetUserName: string;
begin
  // the first two are used when run from a normal login shell
  Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('USER');
  // Used it program is run from cron jobs
  if Result = '' then
    Result := GetEnvironmentVariable('LOGNAME');
end;

function tiUnixGetComputerName: string;
begin
  Result := GetHostName;
end;

function IsCharAlpha(AChar: Char): Boolean;
begin
  // This is very primitive and doesn't take special chars or unicode in
  // consideration.
  if AChar in ['a'..'z', 'A'..'Z'] then
    Result := True
  else
    Result := False;
end;

end.
