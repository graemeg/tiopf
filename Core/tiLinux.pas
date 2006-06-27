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


implementation
uses
  Libc
  ,Process
  ,LCLIntf      // Delphi compatabilty unit for Linux
  ,unix
  ;


function GetCurrentThreadID: Cardinal;
begin
  Result := pthread_self;
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
  Result := GetTickCount;
end;


function tiLinuxGetUserName: string;
begin
  Result := GetEnvironmentVariable('USERNAME');
  if Result = '' then
    Result := GetEnvironmentVariable('USER');
end;


function tiLinuxGetComputerName: string;
begin
  Result := GetHostName;
end;

end.
