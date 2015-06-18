program Demo_LoadPersistenceLayerIfDef;

(*
  Add a {$DEFINE LINK_???} to your project, either in the DPR
  or the Project | Options - Conditional Defines dialog,
  the specified persistence layers will be linked and loaded.
  If you use the Lazarus tiOPF.lpk package, the conditional
  defines are set in there.

  When you run this demo it will show a listing of
  the loaded persistence layers. (Note, it will
  say the database is not connected - wich is correct.)

  Take a look at tiOPFManager about line 243 and you will see
  how these defines are implemented.
*)

{$I tiDefines.inc}

{$IFNDEF FPC}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils
  ,tiOPFManager
  ,tiConstants
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
