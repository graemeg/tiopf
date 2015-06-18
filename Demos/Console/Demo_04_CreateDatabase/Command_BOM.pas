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
    procedure   Execute(const AAppObject: TtiBaseObject; const AParams: string); override;
  end;


  TUIConsoleCommandSelectPL = class(TUIConsoleCommand)
  public
    function    CanExecute(const ACommand: string): boolean; override;
    procedure   Execute(const AAppObject: TtiBaseObject; const AParams: string); override;
  end;

procedure RegisterCustomCommands(const AConsoleApp: TtiBaseObject);

implementation

uses
  tiOPFManager
  ,tiConstants
  ;

{ TUIConsoleCommandListPL }

function TUIConsoleCommandListPL.CanExecute(const ACommand: string): boolean;
begin
  result:= SameText(ACommand, 'l');
end;

procedure TUIConsoleCommandListPL.Execute(const AAppObject: TtiBaseObject; const AParams: string);
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

{ TUIConsoleCommandSelectPL }

function TUIConsoleCommandSelectPL.CanExecute(const ACommand: string): boolean;
begin
  Result := SameText(ACommand, 's');
end;

procedure TUIConsoleCommandSelectPL.Execute(const AAppObject: TtiBaseObject; const AParams: string);
begin
  Assert(AAppObject<>nil, '"AAppObject" may not be unassigned');
  TDemoUIConsole(AAppObject).PersistenceLayerName := AParams;
end;

procedure RegisterCustomCommands(const AConsoleApp: TtiBaseObject);
begin
  TDemoUIConsole(AConsoleApp).RegisterCommand(TUIConsoleCommandListPL.Create);
end;

end.

