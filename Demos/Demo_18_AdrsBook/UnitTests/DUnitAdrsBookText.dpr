program DUnitAdrsBookText;

{$APPTYPE CONSOLE}

uses
  FastMM4,
  SysUtils,
  TextTestRunner,
  Adrs_SrvAutoMap,
  tiUtils,
  DUnitAdrsBookDependencies in 'DUnitAdrsBookDependencies.pas ';

begin
  try
    // ToDo: Ask which persistence mechanism
    DUnitAdrsBookDependencies.RegisterTests;
    DUnitAdrsBookDependencies.ConnectToDatabase;
    TextTestRunner.RunRegisteredTests;
  except
    on E:Exception do
    begin
      Writeln(E.Classname, ': ', E.Message);
      tiConsoleAppPause;
    end;
  end;
end.
