program AdrsBookUIConsole;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  tiUtils,
  tiCommandLineParams,
  AdrsBookUIConsole_BOM in 'AdrsBookUIConsole_BOM.pas';

var
  LO: TAdrsBookUIConsole;
begin
  try
    LO:= TAdrsBookUIConsole.Create;
    try
      LO.Execute;
    finally
      LO.Free;
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  if GCommandLineParams.IsParam('pause') then
    tiConsoleAppPause;
end.
