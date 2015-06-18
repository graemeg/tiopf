program Demo_CreateDatabase;

{$I tiDefines.inc}

{$IFDEF MSWINDOWS}
  {$APPTYPE CONSOLE}
{$ENDIF}

{
  Adding a tiQueryXXX unit to your project will force that
  persistence layer to be loaded.

  Note to Lazarus users:
    When compiling with Lazarus 1.2 or later, we removed
    the tiOPF.lpk package dependency. That way this demo
    will work as indended, by not using the LINK_xxx defines
    from tiOPF.lpk, but rather only look at the tiQueryXXX
    unit listed in the uses clause.

    For this demo to now compile on your system, you need
    to tell Lazarus where find the tiOPF source code.
    Please edit the "Project Options" -> "Compiler Options" ->
    "Additions and Overrides" screen, and change the path
    show in the 'tiopf' macro.
}
uses
  SysUtils
  ,tiOPFManager
  ,tiConstants
  ,tiCommandLineParams
  ,tiUtils
  {$IFDEF FPC}
  ,tiQuerySqldbIB
  {$ELSE}
  ,tiQueryIBX
  {$ENDIF}
  ,tiQueryXMLLight
  ,tiQueryCSV
  ,tiQueryTAB
  ,Demo_UIConsole_BOM
  ,Command_BOM
  ;

var
  LO: TDemoUIConsole;
begin
  try
    LO:= TDemoUIConsole.Create;
    try
      RegisterCustomCommands(LO);
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
