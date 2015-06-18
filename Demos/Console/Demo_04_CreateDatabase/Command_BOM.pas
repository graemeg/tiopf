unit Command_BOM;

{$I tiDefines.inc}

interface

uses
  Classes
  ,SysUtils
  ,Demo_UIConsole_BOM
  ,tiObject
  ,tiBaseObject
  ;

type

  TUIConsoleCommandListPL = class(TUIConsoleCommand)
  public
    function    CanExecute(const ACommand: string): boolean; override;
    procedure   Execute(const AAppObject: TtiObject; const AParams: string); override;
  end;


procedure RegisterCustomCommands(const AConsoleApp: TtiBaseObject);

implementation

uses
  tiOPFManager
  ,tiConstants
  ;

procedure RegisterCustomCommands(const AConsoleApp: TtiBaseObject);
begin
  TDemoUIConsole(AConsoleApp).RegisterCommand(TUIConsoleCommandListPL.Create);
end;

{ TUIConsoleCommandListPL }

function TUIConsoleCommandListPL.CanExecute(const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'l');
end;

procedure TUIConsoleCommandListPL.Execute(const AAppObject: TtiObject; const AParams: string);
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
    WriteLn('No persistence layers loaded');end;

end.

