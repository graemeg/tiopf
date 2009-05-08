{ Linux specific functions used by tiUtils. }
unit tiLinux;

{$I tiDefines.inc}

interface
uses
  SysUtils
 ;

  { Wraps the Linux call in a method with the equavalent Windows name }
  function  GetCurrentThreadID: Cardinal;
  procedure tiLinuxRunEXEAndWait(pStrEXE: string);
  function  tiLinuxGetTickCount: Cardinal;
  function  tiLinuxGetUserName: string;
  function  tiLinuxGetComputerName: string;
  function  IsCharAlpha(AChar: Char): Boolean;


implementation
uses
  Process
  ,unix
 ;


function GetCurrentThreadID: Cardinal;
begin
  Result:=system.GetCurrentThreadID;
//  Result := pthread_self;
end;


procedure tiLinuxRunEXEAndWait(pStrEXE: string);
var
  lProcess: TProcess;
begin
  lProcess := TProcess.Create(nil);
  lProcess.CommandLine := pStrEXE;
  lProcess.Options := [poNoConsole, poWaitOnExit];
  lProcess.Execute;
end;


function tiLinuxGetTickCount: Cardinal;
begin
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
end;


function tiLinuxGetUserName: string;
begin
  // the first two are used when run from a normal login shell
  Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('USER');
  // Used it program is run from cron jobs
  if Result = '' then
    Result := GetEnvironmentVariable('LOGNAME');
end;


function tiLinuxGetComputerName: string;
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
