program tiWebServerCGIForTesting;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  tiConstants,
  tiUtils;
var
  LParam: string;
  LFlagLength: Word;
  LFileName: string;
  LResponse: string;
begin
  LFlagLength:= Length(CCGIExtensionLargeParamFlag);
  LParam:= Trim(ParamStr(1));
  if Copy(LParam, 1, LFlagLength) = CCGIExtensionLargeParamFlag then
  begin
    LFileName:= Copy(LParam, LFlagLength + 2, Length(LParam) - LFlagLength - 1);
    LResponse:= tiFileToString(LFileName);
  end else
    LResponse:= LParam;
  try
    Write(LResponse);
  except
    on E:Exception do
      Write(E.Classname, ': ', E.Message);
  end;
end.
