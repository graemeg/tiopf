unit tiWebServerUtils;

{$I tiDefines.inc}

interface

procedure tiWebServerExecuteCGIApp(const AParams: string; var AResponse: string; const ACGIApp: string; var Result: Cardinal);

implementation
uses
  tiUtils,
  tiConstants,
  {$IFNDEF FPC}
  tiConsoleApp,
  {$ELSE}
  process,
  {$ENDIF}
  SysUtils;

{$IFNDEF FPC}
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

{$ELSE}
procedure tiWebServerExecuteCGIApp(const AParams: string; var AResponse: string; const ACGIApp: string; var Result: Cardinal);
const
  BufSize = 1024; //4096;
var
  p: TProcess;
  Buf: string;
  Count: integer;
  i: integer;
  LineStart: integer;
begin
  p := TProcess.Create(nil);
  p.Options := [poUsePipes, poStdErrToOutPut];
  p.ShowWindow := swoShowNormal;
  p.CommandLine := ACGIApp + ' ' + AParams;
  try
    p.Execute;
    { Now process the output }
    AResponse := '';
    SetLength(Buf,BufSize);
    repeat
      if (p.Output<>nil) then
      begin
        Count := p.Output.Read(Buf[1],Length(Buf));
      end
      else
        Count:=0;
      LineStart:=1;
      i:=1;
      while i<=Count do
      begin
        if Buf[i] in [#10,#13] then
        begin
          AResponse := AResponse + Copy(Buf,LineStart,i-LineStart);
          if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1]) then
            inc(i);
          LineStart:=i+1;
        end;
        inc(i);
      end;
      AResponse := AResponse + Copy(Buf,LineStart,Count-LineStart+1);
    until Count=0;
  finally
    Result := p.ExitStatus;
    p.Free;
  end;
end;
{$ENDIF}

end.