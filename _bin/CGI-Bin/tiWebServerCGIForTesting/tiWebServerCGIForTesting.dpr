program tiWebServerCGIForTesting;

{$APPTYPE CONSOLE}

uses
  SysUtils;

begin
  try
    Write(Trim(ParamStr(1)));
  except
    on E:Exception do
      Write(E.Classname, ': ', E.Message);
  end;
end.
