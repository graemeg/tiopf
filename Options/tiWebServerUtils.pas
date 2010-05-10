unit tiWebServerUtils;

{$I tiDefines.inc}

interface

procedure tiWebServerExecuteCGIApp(const AParams: string; var AResponse: string; const ACGIApp: string; var Result: Cardinal);

implementation
uses
  tiUtils,
  tiConstants,
  tiConsoleApp, SysUtils;

procedure tiWebServerExecuteCGIApp(const AParams: string; var AResponse: string; const ACGIApp: string; var Result: Cardinal);
var
  LTempFileName: string;
begin
  if Length(ACGIApp) + Length(AParams) <= CMaximumCommandLineLength then
   Result := tiExecConsoleApp(ACGIApp, AParams, AResponse, nil, false)
  else
  begin
    LTempFileName := tiGetTempFile('tmp');
    tiStringToFile(AParams, LTempFileName);
    try
      Result := tiExecConsoleApp(ACGIApp, CCGIExtensionLargeParamFlag + '=' + LTempFileName, AResponse, nil, false);
    finally
      tiDeleteFile(LTempFileName);
    end;
  end;
end;


end.
