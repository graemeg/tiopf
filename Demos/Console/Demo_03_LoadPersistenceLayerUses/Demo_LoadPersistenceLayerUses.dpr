program Demo_LoadPersistenceLayerUses;

{$I tiDefines.inc}

{$IFNDEF FPC}
  {$APPTYPE CONSOLE}
{$ENDIF}

{
  Adding a tiQueryXXX unit to your project will force that
  persistence layer to be loaded.
  When you run this demo, a dialog will show listing
  the loaded persistence layers. (Note, the dialog will
  say the database is not connected - which is correct.)

  ps:
    When compiling with Lazarus 1.2 or later, we removed
    the tiOPF.lpk package dependency. That way this demo
    will work as indended, by not using the LINK_xxx defines
    from tiOPF.lpk, but rather only look at the tiQueryXXX
    unit listed in the uses clause.

    For this demo to now compile on your system, you need
    to tell Lazarus where find the tiOPF source code.
    Please edit the "Project Options" -> "Compiler Options" ->
    "Additions and Overrides" screen, and change the path
    shown in the 'tiopf' macro.
}
uses
  SysUtils
  ,tiOPFManager
  ,tiConstants
  {$IFDEF FPC}
  ,tiQuerySqldbIB
  {$ELSE}
  ,tiQueryIBX
  {$ENDIF}
  ,tiQueryXMLLight
  ,tiQueryCSV
  {$IFDEF MSWINDOWS}
  ,tiQueryXML
  ,tiQueryADOAccess
  {$ENDIF}
  ;

procedure ShowConnectedDatabases;
var
  i: integer;
  LS: string;
begin
  LS := '';
  for i := 0 to GTIOPFManager.PersistenceLayers.Count - 1 do
  begin
    if LS <> '' then
      LS := LS + cLineEnding;
    if Trim(GTIOPFManager.PersistenceLayers.Items[i].DBConnectionPools.DetailsAsString) = '' then
      LS := LS + 'Persistence layer: "'+ GTIOPFManager.PersistenceLayers.Items[i].PersistenceLayerName +
            '" loaded, but not connected to a database.'
    else
      LS := LS + GTIOPFManager.PersistenceLayers.Items[i].DBConnectionPools.DetailsAsString
  end;

  if LS <> '' then
    WriteLn(LS)
  else
    WriteLn('No persistence layers loaded');
end;

begin
  ShowConnectedDatabases;
end.

